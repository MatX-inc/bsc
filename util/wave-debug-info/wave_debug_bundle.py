#!/usr/bin/env python3
"""Bundle a design's waveform debug information: the fragments of the
modules its top module reaches through the fragments' "children", copied
into a directory with an index.json, for a viewer to read beside the
design's dump as <dump>.debug/.

    wave_debug_bundle.py [--top MODULE] [--backend bluesim|verilog]
        [--out DIR] [--tar FILE] FRAGMENT_OR_DIRECTORY...

Fragments are wavedebuginfo's JSON files; a directory stands for every
*.wave-debug.json in it.  Any superset of the design's fragments may be
offered (every module of its library closure, say): only those the top
module instantiates, directly or through other modules, are bundled.
The top module defaults to the one fragment no other reaches, and the
backend to the one the fragments come from, when either is unambiguous.
The directory defaults to <top>.debug; a tar of the same files is
written when --tar names one.

index.json names the format ("bsc-wave-debug-bundle", version 1), the
top module, the backend and the bundled fragments' file names.  A
reader starts at the top module's fragment and follows "children".
"""

import argparse
import glob
import json
import os
import shutil
import sys
import tarfile


def fragment_files(paths):
    for path in paths:
        if os.path.isdir(path):
            yield from sorted(glob.glob(os.path.join(path, "*.wave-debug.json")))
        else:
            yield path


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--top", help="the top module (default: the one no fragment reaches)")
    parser.add_argument("--backend", choices=["bluesim", "verilog"],
                        help="the elaboration the fragments describe (default: the fragments' own)")
    parser.add_argument("--out", help="the bundle directory (default: <top>.debug)")
    parser.add_argument("--tar", help="also write the bundle as this tar file")
    parser.add_argument("fragments", nargs="+", metavar="FRAGMENT_OR_DIRECTORY")
    args = parser.parse_args()

    fragments = {}  # (backend, module) -> (path, contents)
    for path in fragment_files(args.fragments):
        with open(path) as f:
            fragment = json.load(f)
        if fragment.get("format") != "bsc-wave-debug-info" or fragment.get("version") != 2:
            sys.exit(f"{path}: not a wavedebuginfo fragment (format version 2)")
        key = (fragment["backend"], fragment["module"])
        if key in fragments:
            sys.exit(f"{path}: another fragment of {key[1]} ({key[0]}) was given: {fragments[key][0]}")
        fragments[key] = (path, fragment)
    if not fragments:
        sys.exit("no fragments given")

    backends = sorted({backend for backend, _ in fragments})
    backend = args.backend
    if backend is None:
        if len(backends) > 1:
            sys.exit(f"fragments of both backends were given ({', '.join(backends)}): choose one with --backend")
        backend = backends[0]
    offered = {module: fragments[(b, module)] for b, module in fragments if b == backend}
    if not offered:
        sys.exit(f"no {backend} fragments were given")

    top = args.top
    if top is None:
        children = {child["module"] for _, fragment in offered.values() for child in fragment["children"]}
        roots = sorted(set(offered) - children)
        if len(roots) != 1:
            sys.exit(f"the top module is ambiguous ({', '.join(roots) or 'none'}): name it with --top")
        top = roots[0]
    elif top not in offered:
        sys.exit(f"no {backend} fragment of the top module {top} was given")

    reached = []
    pending = [top]
    while pending:
        module = pending.pop()
        if module in reached:
            continue
        if module not in offered:
            sys.exit(f"no {backend} fragment of module {module}, instantiated under {top}")
        reached.append(module)
        pending.extend(child["module"] for child in offered[module][1]["children"])

    out = args.out or top + ".debug"
    os.makedirs(out, exist_ok=True)
    names = []
    for module in sorted(reached):
        path = offered[module][0]
        names.append(os.path.basename(path))
        shutil.copyfile(path, os.path.join(out, names[-1]))
    index = {
        "format": "bsc-wave-debug-bundle",
        "version": 1,
        "top": top,
        "backend": backend,
        "fragments": names,
    }
    with open(os.path.join(out, "index.json"), "w") as f:
        json.dump(index, f, indent=2, sort_keys=True)
        f.write("\n")
    if args.tar:
        with tarfile.open(args.tar, "w") as tar:
            tar.add(os.path.join(out, "index.json"), arcname="index.json")
            for name in names:
                tar.add(os.path.join(out, name), arcname=name)


if __name__ == "__main__":
    main()
