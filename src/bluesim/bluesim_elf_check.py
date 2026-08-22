#!/usr/bin/env python3
"""Verify that a generated Bluesim model shared object is freestanding.

A Bluesim model .so performs all of its I/O through the host
operations table passed to bk_sync_init() and provides its C string
routines locally (src/vendor/musl/), so a correctly built model has
NO dependency on the host environment beyond the dynamic loader
mapping it in.  This checker makes that property enforceable:

  1. no DT_NEEDED entries: the object names no shared libraries;

  2. no unresolved dynamic symbols, weak ones included: every symbol
     the object uses is defined inside it (weak-undefined symbols
     would silently resolve to NULL or to whatever the host process
     happens to export -- both are bindings to the environment);

  3. no relocations that need runtime symbol resolution: only
     R_*_RELATIVE base adjustments are accepted.  They are applied by
     the loader before any symbol binding (they reference no symbol,
     only the module's own load base) and, since the object is linked
     -z relro -z now, the data they patch is sealed read-only before
     any model code runs.  Everything else -- R_*_JUMP_SLOT and
     R_*_GLOB_DAT (PLT/GOT bindings to named symbols), R_*_IRELATIVE
     (runs resolver code during relocation), TLS and COPY relocations
     -- is rejected.  The object must also be marked BIND_NOW, so
     nothing is left for lazy resolution;

  4. no system-call instructions (x86_64: syscall/sysenter/int $0x80;
     aarch64: svc) in any executable section: the model asks the
     kernel for nothing directly.  This also rules out vDSO use: the
     vDSO is reached either through an imported symbol (check 2) or
     by locating it via getauxval/auxv (an import) or a raw system
     call (this check).

BDPI models are the one documented exception to full freestanding
operation: their argument-marshaling fallback calls malloc/free (see
prim_ops.cxx), and the imported C functions themselves are compiled
outside this scheme.  Under --bdpi the checker admits exactly the
documented exclusion list -- DT_NEEDED on libc, undefined malloc and
free, and the eagerly-bound (BIND_NOW) PLT/GOT relocations naming
those two symbols -- and nothing else.  The system-call check still
applies to the code linked into the object.

Usage: bluesim_elf_check.py [--bdpi] [--quiet] model.so [more.so ...]

Exit status: 0 when every object passes, 1 when any check fails,
2 on usage or environment errors.  Violations are listed on stderr;
stdout carries a stable one-line-per-check summary (used verbatim by
the testsuite).
"""

import argparse
import struct
import subprocess
import sys

# ---------------------------------------------------------------------------
# Minimal ELF64 reader (little-endian; the only Bluesim targets)

ELF_MAGIC = b"\x7fELF"
ET_DYN = 3
SHT_RELA = 4
SHT_DYNAMIC = 6
SHT_REL = 9
SHT_DYNSYM = 11
SHF_ALLOC = 0x2
SHF_EXECINSTR = 0x4
PT_GNU_STACK = 0x6474E551
PF_X = 0x1
DT_NEEDED = 1
DT_BIND_NOW = 24
DT_FLAGS = 30
DT_FLAGS_1 = 0x6FFFFFFB
DF_BIND_NOW = 0x8
DF_1_NOW = 0x1

EM_X86_64 = 62
EM_AARCH64 = 183

# per-machine relocation classification
RELOC_NAMES = {
    EM_X86_64: {
        1: "R_X86_64_64", 6: "R_X86_64_GLOB_DAT", 7: "R_X86_64_JUMP_SLOT",
        8: "R_X86_64_RELATIVE", 37: "R_X86_64_IRELATIVE",
    },
    EM_AARCH64: {
        257: "R_AARCH64_ABS64", 1025: "R_AARCH64_GLOB_DAT",
        1026: "R_AARCH64_JUMP_SLOT", 1027: "R_AARCH64_RELATIVE",
        1032: "R_AARCH64_IRELATIVE",
    },
}
RELATIVE_RELOCS = {EM_X86_64: {8}, EM_AARCH64: {1027}}
SLOT_RELOCS = {EM_X86_64: {6, 7}, EM_AARCH64: {1025, 1026}}

# per-machine system-call mnemonics, matched against objdump output
SYSCALL_MNEMONICS = {
    EM_X86_64: ("syscall", "sysenter"),
    EM_AARCH64: ("svc",),
}


class Elf:
    def __init__(self, path):
        with open(path, "rb") as fh:
            self.data = fh.read()
        d = self.data
        if d[:4] != ELF_MAGIC:
            raise ValueError("not an ELF file")
        if d[4] != 2 or d[5] != 1:
            raise ValueError("not a little-endian ELF64 file")
        (self.e_type, self.e_machine) = struct.unpack_from("<HH", d, 16)
        (self.e_shoff,) = struct.unpack_from("<Q", d, 0x28)
        (self.e_phoff,) = struct.unpack_from("<Q", d, 0x20)
        (self.e_phentsize, self.e_phnum,
         self.e_shentsize, self.e_shnum,
         self.e_shstrndx) = struct.unpack_from("<HHHHH", d, 0x36)
        self.sections = []
        for i in range(self.e_shnum):
            off = self.e_shoff + i * self.e_shentsize
            (sh_name, sh_type, sh_flags, sh_addr, sh_offset, sh_size,
             sh_link, sh_info, sh_align, sh_entsize) = \
                struct.unpack_from("<IIQQQQIIQQ", d, off)
            self.sections.append(dict(
                name_off=sh_name, type=sh_type, flags=sh_flags,
                addr=sh_addr, offset=sh_offset, size=sh_size,
                link=sh_link, entsize=sh_entsize))
        shstr = self.sections[self.e_shstrndx]
        for s in self.sections:
            s["name"] = self._cstr(shstr["offset"] + s["name_off"])
        self.phdrs = []
        for i in range(self.e_phnum):
            off = self.e_phoff + i * self.e_phentsize
            (p_type, p_flags, p_off, p_vaddr, p_paddr, p_filesz,
             p_memsz, p_align) = struct.unpack_from("<IIQQQQQQ", d, off)
            self.phdrs.append(dict(type=p_type, flags=p_flags))

    def _cstr(self, off):
        end = self.data.index(b"\0", off)
        return self.data[off:end].decode("utf-8", "replace")

    def dynamic_entries(self):
        for s in self.sections:
            if s["type"] == SHT_DYNAMIC:
                strtab = self.sections[s["link"]]
                n = s["size"] // 16
                for i in range(n):
                    (tag, val) = struct.unpack_from(
                        "<qQ", self.data, s["offset"] + 16 * i)
                    if tag == 0:
                        break
                    yield (tag, val, strtab)

    def dyn_symbols(self):
        """(name, bind, shndx) for every .dynsym entry past index 0."""
        for s in self.sections:
            if s["type"] == SHT_DYNSYM:
                strtab = self.sections[s["link"]]
                count = s["size"] // 24
                syms = []
                for i in range(1, count):
                    off = s["offset"] + 24 * i
                    (st_name, st_info, st_other, st_shndx) = \
                        struct.unpack_from("<IBBH", self.data, off)
                    name = self._cstr(strtab["offset"] + st_name)
                    syms.append((i, name, st_info >> 4, st_shndx))
                return syms
        return []

    def relocations(self):
        """(reloc_type, symbol_index) for every alloc'd RELA/REL entry."""
        for s in self.sections:
            if s["type"] not in (SHT_RELA, SHT_REL):
                continue
            if not (s["flags"] & SHF_ALLOC):
                continue
            entsize = 24 if s["type"] == SHT_RELA else 16
            for i in range(s["size"] // entsize):
                (r_offset, r_info) = struct.unpack_from(
                    "<QQ", self.data, s["offset"] + entsize * i)
                yield (r_info & 0xFFFFFFFF, r_info >> 32)


# ---------------------------------------------------------------------------

def check_object(path, bdpi, quiet):
    """Returns a list of violation strings (empty = pass) and prints
    the stable summary lines for this object to stdout."""
    problems = []
    try:
        elf = Elf(path)
    except (OSError, ValueError, IndexError) as e:
        return ["%s: cannot parse: %s" % (path, e)]

    if elf.e_type != ET_DYN:
        problems.append("not a shared object (e_type != ET_DYN)")
    if elf.e_machine not in RELOC_NAMES:
        problems.append("unrecognized machine %d (checker knows x86_64 "
                        "and aarch64)" % elf.e_machine)
        return problems

    # -- 1: DT_NEEDED ------------------------------------------------
    needed = []
    bind_now = False
    for (tag, val, strtab) in elf.dynamic_entries():
        if tag == DT_NEEDED:
            needed.append(elf._cstr(strtab["offset"] + val))
        elif tag == DT_BIND_NOW:
            bind_now = True
        elif tag == DT_FLAGS and (val & DF_BIND_NOW):
            bind_now = True
        elif tag == DT_FLAGS_1 and (val & DF_1_NOW):
            bind_now = True

    def is_libc(name):
        base = name.split(".so")[0]
        return base == "libc"

    if bdpi:
        stray = [n for n in needed if not is_libc(n)]
        for n in stray:
            problems.append("DT_NEEDED beyond the documented BDPI "
                            "exclusion (libc): %s" % n)
        needed_summary = ("libc only (documented BDPI exclusion)"
                          if needed and not stray
                          else ("none" if not needed else "VIOLATION"))
    else:
        for n in needed:
            problems.append("DT_NEEDED entry: %s" % n)
        needed_summary = "none" if not needed else "VIOLATION"

    # -- 2: unresolved dynamic symbols -------------------------------
    allowed_undef = {"malloc", "free"} if bdpi else set()
    allowed_undef_indices = set()
    undef_bad = []
    saw_allowed = False
    for (idx, name, bind, shndx) in elf.dyn_symbols():
        if shndx != 0 or name == "":
            continue
        if name in allowed_undef:
            allowed_undef_indices.add(idx)
            saw_allowed = True
            continue
        undef_bad.append(name)
        problems.append("unresolved dynamic symbol%s: %s" %
                        (" (weak)" if bind == 2 else "", name))
    if undef_bad:
        undef_summary = "VIOLATION"
    elif bdpi and saw_allowed:
        undef_summary = "malloc/free only (documented BDPI exclusion)"
    else:
        undef_summary = "none"

    # -- 3: relocations ----------------------------------------------
    rel_names = RELOC_NAMES[elf.e_machine]
    relative = RELATIVE_RELOCS[elf.e_machine]
    slots = SLOT_RELOCS[elf.e_machine]
    symnames = {idx: name for (idx, name, _b, _s) in elf.dyn_symbols()}
    slot_ok = False
    rel_bad = []
    for (rtype, symidx) in elf.relocations():
        if rtype in relative:
            continue
        if bdpi and rtype in slots and symidx in allowed_undef_indices:
            slot_ok = True  # eager (BIND_NOW) binding of malloc/free
            continue
        rel_bad.append("%s -> %s" %
                       (rel_names.get(rtype, "reloc type %d" % rtype),
                        symnames.get(symidx, "(no symbol)")))
    for r in rel_bad:
        problems.append("relocation needing runtime resolution: %s" % r)
    if (needed or saw_allowed or slot_ok or
            any(True for _ in elf.relocations())):
        # an object with any dynamic work must be marked BIND_NOW so
        # nothing resolves lazily behind the model's back
        if not bind_now:
            problems.append("object is not marked BIND_NOW "
                            "(link with -z now)")
    if rel_bad:
        rel_summary = "VIOLATION"
    elif slot_ok:
        rel_summary = ("relative + eager bindings of the documented "
                       "BDPI exclusion")
    else:
        rel_summary = "relative-only"

    # -- extra hygiene: no executable stack request -------------------
    for p in elf.phdrs:
        if p["type"] == PT_GNU_STACK and (p["flags"] & PF_X):
            problems.append("PT_GNU_STACK requests an executable stack")

    # -- 4: system-call instructions ----------------------------------
    mnemonics = SYSCALL_MNEMONICS[elf.e_machine]
    try:
        dis = subprocess.run(["objdump", "-d", path],
                             capture_output=True, text=True, check=True)
    except (OSError, subprocess.CalledProcessError) as e:
        return problems + ["cannot disassemble %s: %s" % (path, e)]
    syscalls = []
    for line in dis.stdout.splitlines():
        # instruction lines look like "  1a2b:\t0f 05      \tsyscall"
        parts = line.split("\t")
        if len(parts) < 3 or not parts[0].strip().endswith(":"):
            continue
        insn = parts[2].strip()
        mnemonic = insn.split()[0] if insn else ""
        if mnemonic in mnemonics:
            syscalls.append(line.strip())
        elif (elf.e_machine == EM_X86_64 and mnemonic == "int"
              and "$0x80" in insn):
            syscalls.append(line.strip())
    for s in syscalls:
        problems.append("system-call instruction: %s" % s)
    sys_summary = "none" if not syscalls else "VIOLATION"

    if not quiet:
        print("bluesim_elf_check: %s%s" %
              (path, " (BDPI exclusions admitted)" if bdpi else ""))
        print("  DT_NEEDED entries: %s" % needed_summary)
        print("  unresolved dynamic symbols: %s" % undef_summary)
        print("  dynamic relocations: %s" % rel_summary)
        print("  system-call instructions: %s" % sys_summary)
        print("  %s" % ("PASS" if not problems else "FAIL"))
    return problems


def main(argv):
    ap = argparse.ArgumentParser(
        description="verify that a Bluesim model shared object is "
                    "freestanding")
    ap.add_argument("--bdpi", action="store_true",
                    help="admit the documented BDPI exclusions "
                         "(DT_NEEDED libc; malloc/free and their "
                         "eager bindings)")
    ap.add_argument("--quiet", action="store_true",
                    help="no summary on stdout, violations only")
    ap.add_argument("objects", nargs="+", metavar="model.so")
    args = ap.parse_args(argv)

    failed = False
    for path in args.objects:
        problems = check_object(path, args.bdpi, args.quiet)
        for p in problems:
            print("%s: %s" % (path, p), file=sys.stderr)
        if problems:
            failed = True
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
