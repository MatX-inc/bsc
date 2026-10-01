#!/usr/bin/env python3
"""Generate and verify the bsc component packages.

The specification is doc/recabalization-brief.md (Revision 2), sections 3.1
to 3.5 and Appendix A; util/recabal/manifest.json holds the data: the ten
components in dependency order, their module lists, the component DAG and
the shared library settings.  This script is the only writer of

    components/<short>/<pkg>.cabal          one Hooks package per component
    components/<short>/SetupHooks.hs        three lines, importing bsc-setup
    components/<short>/<Module/Path>.{hs,lhs}  the symlink farm into src/ (3.3)
    components/<short>/.recabal-generated   the paths written there
    cabal.project                           the twelve packages, build settings
    bsc.cabal                               the facade package: header and
                                            executables kept, libraries rewritten

Usage (paths are resolved from this file's location, so any cwd works):

    python3 util/recabal/gen.py generate
    python3 util/recabal/gen.py verify [--strict]

generate is idempotent: it rewrites only what differs and removes links and
directories it created earlier that the manifest no longer calls for.
verify never writes and exits nonzero when a check fails.  Until the source
edits of brief 3.4 and 3.5 have landed, the DAG check (d) tolerates the
edges listed under known_dag_edges and the root check (e) only warns;
--strict turns both into failures.
"""

import argparse
import json
import os
import re
import sys
from collections import defaultdict
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
MANIFEST = HERE / "manifest.json"
MARKER = ".recabal-generated"
STANZA_KINDS = ("common", "library", "executable", "test-suite", "benchmark",
                "flag", "source-repository", "custom-setup", "foreign-library")

# `import` at the start of a code line: optionally {-# SOURCE #-}, safe,
# qualified, and a package-qualified name.  In a literate file only code
# counts: bird-track lines, or the \begin{code} blocks of the LaTeX style
# (GHCPretty.lhs), never the prose between them.
_IMPORT = (r'import\s+(?:\{-#\s*SOURCE\s*#-\}\s+)?(?:safe\s+)?(?:qualified\s+)?'
           r'(?:"[^"]*"\s+)?([A-Z][A-Za-z0-9_.\']*)')
IMPORT_HS = re.compile(r"^" + _IMPORT, re.M)
IMPORT_LHS = re.compile(r"^>\s*" + _IMPORT, re.M)
CODE_BLOCK = re.compile(r"^\\begin\{code\}[^\n]*\n(.*?)^\\end\{code\}", re.M | re.S)
FIELD_RE = re.compile(r"^(\s*)([A-Za-z][A-Za-z0-9-]*)\s*:(.*)$")


# ----------------------------------------------------------------------------
# manifest

def load_manifest(path=MANIFEST):
    with open(path, encoding="utf-8") as fh:
        m = json.load(fh)
    names = [c["name"] for c in m["components"]]
    if len(set(names)) != len(names):
        die("manifest: duplicate component names")
    dirs = [c["dir"] for c in m["components"]]
    if len(set(dirs)) != len(dirs):
        die("manifest: duplicate component directories")
    seen = set()
    owner = {}
    for c in m["components"]:
        for d in c["depends"]:
            if d not in seen:
                die(f"manifest: {c['name']} depends on {d}, which is not listed before it")
        seen.add(c["name"])
        if c["modules"] != sorted(c["modules"]):
            die(f"manifest: modules of {c['name']} are not sorted")
        for mod in c["modules"]:
            if mod in owner:
                die(f"manifest: module {mod} is in both {owner[mod]} and {c['name']}")
            owner[mod] = c["name"]
    m["_owner"] = owner
    return m


def die(msg):
    print("error: " + msg, file=sys.stderr)
    sys.exit(2)


def short(comp):
    return Path(comp["dir"]).name


def generated(m):
    return set(m["generated_modules"])


# ----------------------------------------------------------------------------
# source lookup

def module_relpath(mod):
    return mod.replace(".", "/")


def locate(m, mod):
    """All source files for a module under the roots, in root order (the
    first is canonical).  Repository-relative paths."""
    hits = []
    for root in m["source_roots"]:
        for ext in m["source_extensions"]:
            p = Path(root) / (module_relpath(mod) + ext)
            if (REPO / p).is_file():
                hits.append(p)
    return hits


def scan_roots(m):
    """module name -> [repository-relative paths] for every Haskell source
    under the roots.  A root's walk does not descend into another root (nor
    a prefix of one, such as src/comp/GHC) or into source_skip_dirs."""
    roots = [Path(r) for r in m["source_roots"]]
    skip = {Path(s) for s in m["source_skip_dirs"]}
    exts = tuple(m["source_extensions"])
    found = defaultdict(list)
    for root in roots:
        base = REPO / root
        if not base.is_dir():
            continue
        for dirpath, dirnames, filenames in os.walk(base):
            rel = Path(dirpath).relative_to(REPO)
            keep = []
            for d in sorted(dirnames):
                sub = rel / d
                if sub in skip or sub in roots or any(r != root and str(r).startswith(str(sub) + "/") for r in roots):
                    continue
                keep.append(d)
            dirnames[:] = keep
            for f in sorted(filenames):
                for ext in exts:
                    if f.endswith(ext):
                        mod = ".".join((rel.relative_to(root) / f[: -len(ext)]).parts)
                        found[mod].append(rel / f)
    return found


def read_text(path):
    with open(path, encoding="utf-8", errors="replace") as fh:
        return fh.read()


def imports_of(path):
    text = read_text(path)
    rx = IMPORT_HS
    if str(path).endswith(".lhs"):
        if "\\begin{code}" in text:
            text = "\n".join(g.group(1) for g in CODE_BLOCK.finditer(text))
        else:
            rx = IMPORT_LHS
    return [g.group(1) for g in rx.finditer(text)]


# ----------------------------------------------------------------------------
# rendering: component packages

def render_setup_hooks(m):
    s = m["setup"]
    return "\n".join([
        "module SetupHooks (setupHooks) where",
        f"import {s['hooks_module']} ({s['hooks_value']})",
        "import Distribution.Simple.SetupHooks (SetupHooks)",
        "setupHooks :: SetupHooks",
        f"setupHooks = {s['hooks_value']}",
        "",
    ])


def _list_field(name, items, indent=2):
    pad = " " * indent
    out = [f"{pad}{name}:"]
    out += [f"{pad}  {it}," for it in items]
    return out


def render_component_cabal(m, comp):
    d = m["library_defaults"]
    gen = generated(m)
    is_core = not comp["depends"]
    L = []
    L.append(f"cabal-version: {m['cabal_version']}")
    L.append("")
    L.append("-- Generated by util/recabal/gen.py from util/recabal/manifest.json; do not")
    L.append("-- edit by hand (CI regenerates and diffs).  The module sources next to this")
    L.append("-- file are symlinks into src/ (doc/recabalization-brief.md, 3.3).")
    L.append(f"name: {comp['name']}")
    L.append(f"version: {m['package_version']}")
    L.append(f"synopsis: {comp['synopsis']}")
    L.append(f"license: {m['license']}")
    L.append("build-type: Hooks")
    L.append(f"tested-with: {m['tested_with']}")
    L.append("")
    L.append("custom-setup")
    L += _list_field("setup-depends", m["setup_depends"])
    L.append("")
    L.append("library")
    L.append("  hs-source-dirs: .")
    L.append("  build-depends:")
    L.append("    -- The hook generates Warmup from the exposed modules of every package")
    L.append("    -- named here (brief 3.4), so hidden boot packages such as ghc-bignum")
    L.append("    -- and ghc-internal have to be named too.")
    L += [f"    {dep}," for dep in d["build-depends"]]
    if comp["depends"]:
        L.append("    -- Component dependencies: the DAG of the brief's Appendix A.")
        L += [f"    {dep}," for dep in comp["depends"]]
    for cond in d["conditional-build-depends"]:
        L.append(f"  if {cond['if']}")
        L.append("    build-depends: " + ", ".join(cond["build-depends"]))
    L.append(f"  default-language: {d['default-language']}")
    L += _list_field("default-extensions", d["default-extensions"])
    L.append(f"  ghc-options: {d['ghc-options']}")
    ex = comp.get("extras")
    if ex:
        if ex.get("extra-libraries"):
            L.append("  extra-libraries: " + ", ".join(ex["extra-libraries"]))
        if ex.get("extra-libraries-osx") or ex.get("extra-libraries-other"):
            L.append("  if os(osx)")
            L.append("    extra-libraries: " + ", ".join(ex["extra-libraries-osx"]))
            L.append("  else")
            L.append("    extra-libraries: " + ", ".join(ex["extra-libraries-other"]))
        for key in ("c-sources", "include-dirs"):
            if ex.get(key):
                L += _list_field(key, [rel_from(comp["dir"], p) for p in ex[key]])
    exposed = list(comp["modules"])
    hidden = []
    if not is_core:
        hidden = [g for g in gen if g == "Warmup"]
    L.append("  exposed-modules:")
    L += [f"    {mod}" for mod in exposed]
    if hidden:
        L.append("  other-modules:")
        L += [f"    {mod}" for mod in hidden]
    L.append("")
    return "\n".join(L)


def rel_from(dir_rel, target_rel):
    """Lexical relative path from directory dir_rel to target_rel (both
    repository-relative)."""
    return os.path.relpath(str(target_rel), start=str(dir_rel))


def farm_links(m, comp):
    """link path (relative to the component dir) -> symlink target text."""
    gen = generated(m)
    links = {}
    missing = []
    for mod in comp["modules"]:
        if mod in gen:
            continue
        hits = locate(m, mod)
        if not hits:
            missing.append(mod)
            continue
        src = hits[0]
        link = module_relpath(mod) + src.suffix
        link_dir = Path(comp["dir"]) / Path(link).parent
        links[link] = rel_from(link_dir, src)
    return links, missing


def component_files(m, comp):
    """Everything generate writes into the component directory:
    path (relative to the directory) -> ('file', text) | ('link', target)."""
    files = {
        f"{comp['name']}.cabal": ("file", render_component_cabal(m, comp)),
        "SetupHooks.hs": ("file", render_setup_hooks(m)),
    }
    links, missing = farm_links(m, comp)
    for link, target in links.items():
        files[link] = ("link", target)
    return files, missing


def render_marker(files):
    L = ["# Written by util/recabal/gen.py.  Every path below, relative to this",
         "# directory, was generated and is removed when the manifest drops it."]
    L += sorted(files)
    L.append("")
    return "\n".join(L)


def read_marker(dirpath):
    p = dirpath / MARKER
    if not p.is_file():
        return []
    return [l for l in read_text(p).splitlines() if l and not l.startswith("#")]


# ----------------------------------------------------------------------------
# rendering: cabal.project

def render_cabal_project(m):
    pkgs = [".", m["setup"]["dir"]] + [c["dir"] for c in m["components"]]
    L = [
        "-- Generated by util/recabal/gen.py from util/recabal/manifest.json; do not",
        "-- edit by hand (CI regenerates and diffs).  Machine-local settings go in",
        "-- cabal.project.local, which is ignored by git.",
        "packages:",
    ]
    L += [f"  {p}" for p in pkgs]
    L += [
        "",
        "-- Parity with the make build (src/comp/Makefile GHCOPTLEVEL = -O2), for every",
        "-- package of the project.",
        "optimization: 2",
        "",
        "-- One GHC job server shared by every package's ghc --make -j",
        "-- (doc/recabalization-brief.md, 3.1; R8 is the fallback: jobs: 1 and -j per",
        "-- package).",
        "jobs: $ncpus",
        "semaphore: True",
        "",
        "-- Lets bare ghci/runghc/ghc resolve the compiled bsc libraries from any cwd",
        "-- inside the repo (GHC searches upward for .ghc.environment.*).  Harmless to",
        "-- the make build: src/comp/Makefile compiles with -hide-all-packages plus",
        "-- explicit -package flags, which override the environment file.",
        "write-ghc-environment-files: always",
        "",
        f"-- The hooks library is the one exception to optimization: 2 (brief 3.1; F6",
        "-- and R2: an optimized build of it failed to link, and it is not",
        "-- performance-sensitive).",
        f"package {m['setup']['name']}",
        "  optimization: 0",
        "",
        "-- -fobject-determinism: parity with src/comp/Makefile (GHCOBJDET), which",
        "-- adds it whenever GHC offers it (9.14+; every package here requires 9.14).",
        "-- Without it two clean builds differ in object code while agreeing on every",
        "-- interface: nondeterministic uniques, which also perturb inlining and",
        "-- strictness decisions. program-options applies to every component of every",
        "-- local package, executables and test suites included.",
        "program-options",
        "  ghc-options: -j -fobject-determinism",
        "",
    ]
    return "\n".join(L)


# ----------------------------------------------------------------------------
# rendering: the facade bsc.cabal (a transformation of the file on disk)

class Stanza:
    def __init__(self, kind, name, head, lead, body):
        self.kind, self.name, self.head, self.lead, self.body = kind, name, head, lead, body

    def lines(self):
        return self.lead + [self.head] + self.body


def split_stanzas(text):
    """-> (header lines, [Stanza], trailing lines).  Blank and comment lines
    before a stanza belong to it (its lead)."""
    header, stanzas, pending, cur = [], [], [], None
    for line in text.split("\n"):
        s = line.strip()
        if line and not line[0].isspace():
            if s.startswith("--"):
                pending.append(line)
                continue
            word = s.split()[0]
            if word in STANZA_KINDS:
                cur = Stanza(word, s[len(word):].strip() or None, line, pending, [])
                pending = []
                stanzas.append(cur)
                continue
            if cur is not None:
                die(f"bsc.cabal: top-level field after a stanza: {line!r}")
            header += pending
            pending = []
            header.append(line)
        elif not s:
            pending.append(line)
        elif cur is None:
            header += pending
            pending = []
            header.append(line)
        else:
            cur.body += pending
            pending = []
            cur.body.append(line)
    return header, stanzas, pending


def join_stanzas(header, stanzas, trailing):
    out = list(header)
    for st in stanzas:
        out += st.lines()
    out += trailing
    return "\n".join(out)


class Item:
    """One field (or conditional block) of a stanza body."""

    def __init__(self, lead, lines, name, inline):
        self.lead, self.lines, self.name, self.inline = lead, lines, name, inline

    def values(self):
        parts = [self.inline] + [l for l in self.lines[1:] if not l.strip().startswith("--")]
        text = " ".join(parts)
        return [v for v in re.split(r"[,\s]+", text) if v]


def split_items(body):
    code = [l for l in body if l.strip() and not l.strip().startswith("--")]
    base = min((len(l) - len(l.lstrip()) for l in code), default=2)
    items, pending, cur = [], [], None
    for line in body:
        s = line.strip()
        if not s or s.startswith("--"):
            pending.append(line)
            continue
        indent = len(line) - len(line.lstrip())
        if indent <= base or cur is None:
            f = FIELD_RE.match(line)
            cur = Item(pending, [line], f.group(2).lower() if f else None, f.group(3).strip() if f else "")
            pending = []
            items.append(cur)
        else:
            cur.lines += pending
            pending = []
            cur.lines.append(line)
    return base, items, pending


def join_items(items, tail):
    out = []
    for it in items:
        out += it.lead + it.lines
    return out + tail


def field_values(stanza, name):
    _, items, _ = split_items(stanza.body)
    vals = []
    for it in items:
        if it.name == name:
            vals += it.values()
    return vals


def with_warmup(stanza):
    """The executable stanza with Warmup in other-modules."""
    base, items, tail = split_items(stanza.body)
    pad = " " * base
    existing = [it for it in items if it.name == "other-modules"]
    vals = []
    for it in existing:
        vals += it.values()
    if "Warmup" in vals:
        return stanza
    vals.append("Warmup")
    if len(vals) == 1:
        lines = [f"{pad}other-modules: {vals[0]}"]
    else:
        lines = [f"{pad}other-modules:"] + [f"{pad}  {v}" for v in vals]
    new = Item(existing[0].lead if existing else [], lines, "other-modules", "")
    items = [it for it in items if it.name != "other-modules"]
    pos = len(items)
    for i, it in enumerate(items):
        if it.name == "main-is":
            pos = i + 1
    items.insert(pos, new)
    return Stanza(stanza.kind, stanza.name, stanza.head, stanza.lead, join_items(items, tail))


def facade_header(header, m):
    out = []
    has_tested = any(FIELD_RE.match(l) and FIELD_RE.match(l).group(2).lower() == "tested-with" for l in header)
    for line in header:
        f = FIELD_RE.match(line)
        if f and f.group(2).lower() == "build-type":
            out.append("build-type: Hooks")
            if not has_tested:
                out.append(f"tested-with: {m['tested_with']}")
            continue
        out.append(line)
    return out


def facade_custom_setup(m):
    return Stanza("custom-setup", None, "custom-setup", [""],
                  _list_field("setup-depends", m["setup_depends"]))


def facade_library(m):
    mods = sorted(m["_owner"])
    body = ["  build-depends:", "    base >=4.18.3.0,"]
    body += [f"    {c['name']}," for c in m["components"]]
    body += [f"  default-language: {m['library_defaults']['default-language']}",
             "  reexported-modules:"]
    body += [f"    {mod}," for mod in mods[:-1]] + [f"    {mods[-1]}"]
    lead = ["",
            "-- The facade: the original module surface, re-exported from the component",
            "-- packages under components/ (doc/recabalization-brief.md, 3.1 and 3.3), so",
            "-- the executables and external consumers see one `bsc` library as before.",
            "-- Generated by util/recabal/gen.py from util/recabal/manifest.json: the",
            "-- header above and the stanzas below are kept, this stanza and custom-setup",
            "-- are rewritten, and the executables get Warmup in other-modules (3.4)."]
    return Stanza("library", None, "library", lead, body)


def render_facade(m, text):
    header, stanzas, trailing = split_stanzas(text)
    header = facade_header(header, m)
    out = []
    saw_setup = saw_lib = False
    for st in stanzas:
        if st.kind == "custom-setup":
            out.append(facade_custom_setup(m))
            saw_setup = True
        elif st.kind == "common" and st.name == "lib-defaults":
            continue
        elif st.kind == "library" and st.name is not None:
            continue
        elif st.kind == "library":
            out.append(facade_library(m))
            saw_lib = True
        elif st.kind == "executable":
            out.append(with_warmup(st))
        else:
            out.append(st)
    if not saw_setup:
        out.insert(0, facade_custom_setup(m))
    if not saw_lib:
        out.insert(1, facade_library(m))
    return join_stanzas(header, out, trailing)


# ----------------------------------------------------------------------------
# generate

def write_if_changed(path, text, log):
    path = Path(path)
    if path.is_file() and not path.is_symlink() and read_text(path) == text:
        return False
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(text)
    log.append(f"wrote   {path.relative_to(REPO)}")
    return True


def remove_path(path, log):
    if path.is_symlink() or path.is_file():
        path.unlink()
        log.append(f"removed {path.relative_to(REPO)}")


def prune_empty_dirs(top):
    for dirpath, dirnames, filenames in os.walk(top, topdown=False):
        p = Path(dirpath)
        if p != top and not dirnames and not filenames:
            try:
                p.rmdir()
            except OSError:
                pass
        # os.walk computed dirnames before children were removed; re-check
        if p != top and p.is_dir() and not any(p.iterdir()):
            p.rmdir()


def generate_component(m, comp, log):
    cdir = REPO / comp["dir"]
    cdir.mkdir(parents=True, exist_ok=True)
    files, missing = component_files(m, comp)
    if missing:
        die(f"{comp['name']}: no source under the roots for: {' '.join(missing)}")
    old = read_marker(cdir)
    for rel in old:
        if rel not in files:
            remove_path(cdir / rel, log)
    for rel, (kind, payload) in sorted(files.items()):
        p = cdir / rel
        if kind == "link":
            if p.is_symlink():
                if os.readlink(p) == payload:
                    continue
                p.unlink()
            elif p.exists():
                if rel not in old:
                    die(f"{comp['name']}: {p.relative_to(REPO)} exists and is not a generated file; refusing to replace it")
                p.unlink()
            p.parent.mkdir(parents=True, exist_ok=True)
            os.symlink(payload, p)
            log.append(f"linked  {p.relative_to(REPO)} -> {payload}")
        else:
            if p.is_symlink():
                p.unlink()
            elif p.exists() and rel not in old and read_text(p) != payload:
                die(f"{comp['name']}: {p.relative_to(REPO)} exists and is not a generated file; refusing to overwrite it")
            write_if_changed(p, payload, log)
    prune_empty_dirs(cdir)
    write_if_changed(cdir / MARKER, render_marker(files), log)


def generate_orphans(m, log):
    """Directories under components/ with a marker but no manifest entry."""
    comps_root = REPO / "components"
    if not comps_root.is_dir():
        return
    live = {REPO / c["dir"] for c in m["components"]}
    for d in sorted(comps_root.iterdir()):
        if d in live or not d.is_dir() or not (d / MARKER).is_file():
            continue
        for rel in read_marker(d):
            remove_path(d / rel, log)
        remove_path(d / MARKER, log)
        prune_empty_dirs(d)
        if not any(d.iterdir()):
            d.rmdir()
            log.append(f"removed {d.relative_to(REPO)}/")


def cmd_generate(m):
    log = []
    for comp in m["components"]:
        generate_component(m, comp, log)
    generate_orphans(m, log)
    write_if_changed(REPO / "cabal.project", render_cabal_project(m), log)
    facade = REPO / m["facade"]["cabal_file"]
    if not facade.is_file():
        die(f"{facade.relative_to(REPO)} is missing; the facade is a transformation of it")
    write_if_changed(facade, render_facade(m, read_text(facade)), log)
    for line in log:
        print(line)
    n = len(m["components"])
    links = sum(1 for c in m["components"] for k, (t, _) in component_files(m, c)[0].items() if t == "link")
    print(f"generate: {n} components, {links} farm links, {len(log)} changes")
    return 0


# ----------------------------------------------------------------------------
# verify

class Report:
    def __init__(self, strict):
        self.strict = strict
        self.failures = 0
        self.warnings = 0

    def section(self, title):
        print(f"\n== {title}")

    def ok(self, msg):
        print(f"   ok    {msg}")

    def info(self, msg):
        print(f"   note  {msg}")

    def fail(self, msg):
        self.failures += 1
        print(f"   FAIL  {msg}")

    def warn(self, msg, strict_fails=True):
        if strict_fails and self.strict:
            self.fail(msg)
        else:
            self.warnings += 1
            print(f"   warn  {msg}")


def facade_stanzas(m):
    text = read_text(REPO / m["facade"]["cabal_file"])
    _, stanzas, _ = split_stanzas(text)
    return text, stanzas


def verify_a(m, rep):
    rep.section("(a) facade re-exports versus the manifest")
    owner = m["_owner"]
    want = m["expected_module_count"]
    if len(owner) != want:
        rep.fail(f"manifest lists {len(owner)} modules, expected {want}")
    else:
        rep.ok(f"manifest: {want} modules, each in exactly one component")
    _, stanzas = facade_stanzas(m)
    libs = [s for s in stanzas if s.kind == "library" and s.name is None]
    if len(libs) != 1:
        rep.fail(f"bsc.cabal has {len(libs)} unnamed library stanzas")
        return
    names = field_values(libs[0], "reexported-modules")
    if len(names) != want:
        rep.fail(f"bsc.cabal re-exports {len(names)} names, expected {want}")
    if len(set(names)) != len(names):
        rep.fail("bsc.cabal re-exports a name twice")
    extra = sorted(set(names) - set(owner))
    missing = sorted(set(owner) - set(names))
    if extra:
        rep.fail(f"re-exported but in no component: {' '.join(extra)}")
    if missing:
        rep.fail(f"in a component but not re-exported: {' '.join(missing)}")
    if not extra and not missing and len(names) == want:
        rep.ok(f"bsc.cabal re-exports exactly the {want} component modules")
    deps = field_values(libs[0], "build-depends")
    pkgs = [c["name"] for c in m["components"]]
    lacking = [p for p in pkgs if p not in deps]
    if lacking:
        rep.fail(f"facade library does not depend on: {' '.join(lacking)}")
    for s in stanzas:
        if s.kind == "library" and s.name is not None:
            rep.fail(f"bsc.cabal still has a sublibrary stanza: {s.head}")
        if s.kind == "common" and s.name == "lib-defaults":
            rep.fail("bsc.cabal still has the common lib-defaults stanza")
        if s.kind == "executable" and "Warmup" not in field_values(s, "other-modules"):
            rep.fail(f"{s.head}: other-modules lacks Warmup")


def verify_b(m, rep):
    rep.section("(b) farm symlinks exist and resolve")
    total = bad = 0
    for comp in m["components"]:
        links, missing = farm_links(m, comp)
        for mod in missing:
            rep.fail(f"{comp['name']}: no source under the roots for {mod}")
        for rel, target in sorted(links.items()):
            total += 1
            p = REPO / comp["dir"] / rel
            if not p.is_symlink():
                bad += 1
                rep.fail(f"{p.relative_to(REPO)}: missing or not a symlink")
            elif not p.exists():
                bad += 1
                rep.fail(f"{p.relative_to(REPO)} -> {os.readlink(p)}: dangling")
    if not bad:
        rep.ok(f"{total} links present and resolving")


def verify_c(m, rep):
    rep.section("(c) every Haskell source under the roots is accounted for")
    found = scan_roots(m)
    owner = m["_owner"]
    excluded = m["excluded_sources"]
    for path, why in sorted(excluded.items()):
        if not (REPO / path).is_file():
            rep.warn(f"excluded_sources names a file that does not exist: {path}", strict_fails=False)
    rep.info("deliberately excluded (manifest excluded_sources):")
    for path, why in sorted(excluded.items()):
        rep.info(f"    {path}: {why}")
    unaccounted = []
    shadowed = []
    for mod, paths in sorted(found.items()):
        if len(paths) > 1:
            shadowed.append(f"{mod}: {' '.join(map(str, paths))}")
        for p in paths:
            if mod in owner or str(p) in excluded:
                continue
            unaccounted.append(str(p))
    for s in shadowed:
        rep.warn(f"module found under more than one root (first wins): {s}", strict_fails=False)
    for p in unaccounted:
        rep.fail(f"unaccounted source: {p}")
    gen = generated(m)
    for mod in sorted(owner):
        if mod not in gen and mod not in found:
            rep.fail(f"manifest module without a source under the roots: {mod}")
    if not unaccounted:
        rep.ok(f"{sum(len(v) for v in found.values())} sources scanned; none unaccounted")
    return found


def component_imports(m):
    """component name -> {module -> [imported module names]} (manifest
    modules only, generated ones skipped)."""
    gen = generated(m)
    out = {}
    for comp in m["components"]:
        mods = {}
        for mod in comp["modules"]:
            if mod in gen:
                continue
            hits = locate(m, mod)
            mods[mod] = imports_of(REPO / hits[0]) if hits else []
        out[comp["name"]] = mods
    return out


def verify_d(m, rep, imports, found):
    rep.section("(d) import DAG: each module imports only its own component and declared dependencies")
    owner = m["_owner"]
    gen = generated(m)
    known = {tuple(e) for e in m["known_dag_edges"]["edges"]}
    allowed = {c["name"]: set(c["depends"]) | {c["name"]} for c in m["components"]}
    edges = 0
    bad_known = []
    bad_new = []
    stray = []
    for cname, mods in imports.items():
        for mod, imps in sorted(mods.items()):
            for imp in imps:
                if imp in gen:
                    continue
                if imp not in owner:
                    if imp in found:
                        stray.append(f"{mod} ({cname}) imports {imp}, a source under the roots that is in no component")
                    continue
                edges += 1
                tgt = owner[imp]
                if tgt in allowed[cname]:
                    continue
                line = f"{mod} ({cname}) -> {imp} ({tgt})"
                (bad_known if (mod, imp) in known else bad_new).append(line)
    for s in stray:
        rep.fail(s)
    for line in bad_new:
        rep.fail(f"edge outside the DAG: {line}")
    for line in bad_known:
        rep.warn(f"edge outside the DAG, known until the brief 3.5 moves land: {line}")
    if bad_known:
        rep.info(f"{len(bad_known)} known edge(s) still present; manifest known_dag_edges lists {len(known)}")
    if not bad_new and not bad_known and not stray:
        rep.ok(f"{edges} intra-manifest import edges, all inside the DAG")
    elif not bad_new and not stray:
        rep.ok(f"{edges} intra-manifest import edges; none outside the DAG except the known ones above")


def verify_e(m, rep, imports):
    rep.section("(e) Warmup roots: a module importing nothing from its own component imports Warmup")
    # Imports of the generated modules do not count as own-component imports:
    # BuildVersion and BuildSystem import nothing, so a module above only them
    # (Version) is still scheduled independently of Warmup (brief F7).
    gen = generated(m)
    total_roots = total_missing = 0
    for comp in m["components"]:
        mods = imports[comp["name"]]
        own = set(mods)
        roots = sorted(mod for mod, imps in mods.items() if not (set(imps) & own - {mod}))
        missing = [r for r in roots if "Warmup" not in mods[r]]
        total_roots += len(roots)
        total_missing += len(missing)
        line = f"{comp['name']}: {len(roots)} roots, {len(roots) - len(missing)} import Warmup"
        if missing:
            rep.warn(f"{line}, missing: {' '.join(missing)}")
        else:
            rep.ok(line)
    rep.info(f"{total_roots} component roots, {total_missing} without the Warmup import"
             " (brief Appendix A: 66 before the source edits of 3.4 land)")
    # (e') the facade's executables are roots of their own package (3.4.4)
    rep.section("(e') facade executables: every root module of each executable imports Warmup")
    _, stanzas = facade_stanzas(m)
    commons = {s.name: s for s in stanzas if s.kind == "common"}
    app_missing = 0
    for s in stanzas:
        if s.kind != "executable":
            continue
        srcdirs = field_values(s, "hs-source-dirs")
        for imp in field_values(s, "import"):
            if imp in commons:
                srcdirs += field_values(commons[imp], "hs-source-dirs")
        srcdirs = srcdirs or ["."]
        files = {}
        main = field_values(s, "main-is")
        others = [o for o in field_values(s, "other-modules") if o not in gen]
        for name in main:
            files[name] = name
        for mod in others:
            files[mod] = module_relpath(mod) + ".hs"
        located = {}
        for key, rel in files.items():
            for d in srcdirs:
                p = REPO / d / rel
                if p.is_file():
                    located[key] = p
                    break
            else:
                rep.fail(f"{s.head}: cannot find {rel} under {' '.join(srcdirs)}")
        own = set(others)
        missing = []
        for key, p in sorted(located.items()):
            imps = imports_of(p)
            if set(imps) & own:
                continue
            if "Warmup" not in imps:
                missing.append(str(p.relative_to(REPO)))
        if missing:
            app_missing += len(missing)
            rep.warn(f"{s.head}: roots without the Warmup import: {' '.join(missing)}")
        else:
            rep.ok(f"{s.head}: {len(located)} module(s), every root imports Warmup")
    rep.info(f"{app_missing} facade executable roots without the Warmup import")


def verify_f(m, rep):
    rep.section("(f) generated files are byte-identical to what generate would write")
    diffs = 0
    for comp in m["components"]:
        cdir = REPO / comp["dir"]
        files, _ = component_files(m, comp)
        if not cdir.is_dir():
            rep.fail(f"{comp['dir']} does not exist")
            diffs += 1
            continue
        for rel, (kind, payload) in sorted(files.items()):
            p = cdir / rel
            if kind == "link":
                if not p.is_symlink() or os.readlink(p) != payload:
                    diffs += 1
                    rep.fail(f"{p.relative_to(REPO)}: expected symlink to {payload}")
            else:
                if p.is_symlink() or not p.is_file() or read_text(p) != payload:
                    diffs += 1
                    rep.fail(f"{p.relative_to(REPO)}: content differs from generate's output")
        marker = cdir / MARKER
        if not marker.is_file() or read_text(marker) != render_marker(files):
            diffs += 1
            rep.fail(f"{marker.relative_to(REPO)}: differs from generate's output")
        # strays: anything else in the directory that GHC could pick up
        for dirpath, dirnames, filenames in os.walk(cdir):
            dirnames[:] = [d for d in dirnames if d != "dist-newstyle"]
            for f in filenames:
                p = Path(dirpath) / f
                rel = str(p.relative_to(cdir))
                if rel in files or rel == MARKER:
                    continue
                if p.is_symlink() or f.endswith((".hs", ".lhs", ".hs-boot", ".cabal")):
                    diffs += 1
                    rep.fail(f"{p.relative_to(REPO)}: not generated by the manifest (would be in hs-source-dirs: .)")
                else:
                    rep.info(f"{p.relative_to(REPO)}: stray file, not generated (left alone)")
    for name, text in (("cabal.project", render_cabal_project(m)),):
        p = REPO / name
        if not p.is_file() or read_text(p) != text:
            diffs += 1
            rep.fail(f"{name}: differs from generate's output")
    facade = REPO / m["facade"]["cabal_file"]
    if not facade.is_file():
        diffs += 1
        rep.fail(f"{facade.name}: missing")
    else:
        cur = read_text(facade)
        if render_facade(m, cur) != cur:
            diffs += 1
            rep.fail(f"{facade.name}: not a fixed point of generate (run generate)")
    if not diffs:
        rep.ok("component packages, cabal.project and bsc.cabal match generate")


def verify_environment(m, rep):
    rep.section("environment notes (not gates of this script)")
    setup_dir = REPO / m["setup"]["dir"]
    if setup_dir.is_dir() and list(setup_dir.glob("*.cabal")):
        rep.info(f"{m['setup']['dir']} is present")
    else:
        rep.info(f"{m['setup']['dir']} ({m['setup']['name']}) is not present yet; cabal.project names it")
    root_hooks = REPO / "SetupHooks.hs"
    if not root_hooks.is_file():
        rep.warn("SetupHooks.hs at the repository root is missing; the facade is build-type Hooks", strict_fails=False)
    elif m["setup"]["hooks_module"] not in read_text(root_hooks):
        rep.warn(f"SetupHooks.hs at the repository root does not import {m['setup']['hooks_module']};"
                 f" the facade's setup-depends names {m['setup']['name']}", strict_fails=False)
    else:
        rep.info(f"SetupHooks.hs at the repository root imports {m['setup']['hooks_module']}")


def cmd_verify(m, strict):
    rep = Report(strict)
    verify_a(m, rep)
    verify_b(m, rep)
    found = verify_c(m, rep)
    imports = component_imports(m)
    verify_d(m, rep, imports, found)
    verify_e(m, rep, imports)
    verify_f(m, rep)
    verify_environment(m, rep)
    print()
    status = "FAILED" if rep.failures else "passed"
    print(f"verify {status}: {rep.failures} failure(s), {rep.warnings} warning(s)"
          + ("" if strict else "; --strict turns the (d) and (e) warnings into failures"))
    return 1 if rep.failures else 0


# ----------------------------------------------------------------------------

def main(argv):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--manifest", default=str(MANIFEST), metavar="PATH",
                    help="an alternative manifest (default: util/recabal/manifest.json), for what-if verify runs")
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("generate", help="write the component packages, cabal.project and bsc.cabal")
    v = sub.add_parser("verify", help="check the tree against the manifest; exit 1 on failure")
    v.add_argument("--strict", action="store_true", help="fail on the known DAG edges and on missing Warmup roots")
    args = ap.parse_args(argv)
    m = load_manifest(args.manifest)
    if args.cmd == "generate":
        return cmd_generate(m)
    return cmd_verify(m, args.strict)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
