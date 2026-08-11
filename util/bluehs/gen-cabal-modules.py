#!/usr/bin/env python3
"""Regenerate the exposed-modules list in bsc.cabal.

Enumerates every .hs/.lhs module under the source roots that feed the bsc
executables (the same pool src/comp/Makefile compiles), excluding:
  * the program entry files (module names Main_* / BlueTcl don't match their
    file names, and they belong to executables, not the library), and
  * dead vendored-Parsec modules that no tool's import closure reaches and
    that no longer compile with modern GHC.

Verifies each file's declared module name matches its path before emitting.
Rewrites the exposed-modules block of bsc.cabal in place.
"""
import os
import re
import sys

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

# (root, subdirs-to-skip-because-they-are-their-own-roots)
ROOTS = [
    ("src/comp", ["Libs", "GHC"]),
    ("src/comp/Libs", []),
    ("src/comp/GHC/posix", []),
    ("src/Parsec", []),
    ("src/vendor/stp/HaskellIfc", []),
    ("src/vendor/yices/v2.6/HaskellIfc", []),
    ("src/vendor/htcl", []),
]

# Program entry files (compiled with -main-is by the Makefile; scripts in
# util/bluehs replace the utility ones).
EXCLUDE_FILES = {
    "bsc.hs", "bsc2bsv.hs", "bsv2bsc.hs", "dumpbo.hs", "dumpba.hs",
    "showrules.hs", "vcdcheck.hs", "bluetcl.hs",
}

# Dead code in the vendored Parsec fork: unreachable from any tool and
# uncompilable with modern base (Haskell98 'Char'/'List' imports, stale API).
# FSTRead (when present in the tree) foreign-imports the vendored libfst C
# reader (and -lz), which the cabal library does not compile in; only the
# fstcheck tool links those objects, so exposing it would ship dangling
# symbols.
EXCLUDE_MODULES = {"ParsecToken", "ParsecLanguage", "ParsecPerm", "FSTRead"}

MOD_RE = re.compile(r"^(?:> )?module\s+([A-Za-z0-9_.']+)", re.M)


def declared_module(path):
    with open(path, encoding="utf-8", errors="replace") as f:
        m = MOD_RE.search(f.read())
    return m.group(1) if m else None


def enumerate_modules():
    modules = []
    for root, skips in ROOTS:
        absroot = os.path.join(REPO, root)
        for dirpath, dirnames, filenames in os.walk(absroot):
            rel = os.path.relpath(dirpath, absroot)
            if rel == ".":
                dirnames[:] = [d for d in dirnames if d not in skips]
            for fn in sorted(filenames):
                if not fn.endswith((".hs", ".lhs")):
                    continue
                if root == "src/comp" and rel == "." and fn in EXCLUDE_FILES:
                    continue
                path = os.path.join(dirpath, fn)
                expect = os.path.splitext(os.path.relpath(path, absroot))[0]
                expect = expect.replace(os.sep, ".")
                if expect in EXCLUDE_MODULES:
                    continue
                decl = declared_module(path)
                if decl != expect:
                    sys.exit(f"module/file mismatch: {path}: "
                             f"declared {decl}, expected {expect}")
                modules.append(expect)
    dups = {m for m in modules if modules.count(m) > 1}
    if dups:
        sys.exit(f"duplicate module names across roots: {sorted(dups)}")
    return sorted(modules)


def rewrite_cabal(modules):
    cabal_path = os.path.join(REPO, "bsc.cabal")
    with open(cabal_path) as f:
        text = f.read()
    block = "\n".join(f"        {m}" for m in modules)
    new_text, n = re.subn(
        r"(    exposed-modules:\n)(?:        \S+\n)+",
        r"\1" + block + "\n",
        text,
        count=1,
    )
    if n != 1:
        sys.exit("could not locate exposed-modules block in bsc.cabal")
    with open(cabal_path, "w") as f:
        f.write(new_text)
    print(f"bsc.cabal: exposed-modules updated ({len(modules)} modules)")


if __name__ == "__main__":
    rewrite_cabal(enumerate_modules())
