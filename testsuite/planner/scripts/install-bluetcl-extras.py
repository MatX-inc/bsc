#!/usr/bin/env python3
"""Install optional Bluetcl test dependencies after the full root build.

The normal install omits these scripts. Preserve its package index: the
source extras' pkgIndex.tcl is not a replacement for the installed index.
"""

from pathlib import Path
import shutil


ROOT = Path(__file__).resolve().parents[3]
SOURCE = ROOT / "util/bluetcl-scripts"
DESTINATION = ROOT / "inst/lib/tcllib/bluespec"
FILES = ("InstSynth.tcl", "expandPorts.tcl", "portUtil.tcl", "types.tcl")
PACKAGES = ("InstSynth", "types")


def main():
    index = DESTINATION / "pkgIndex.tcl"
    if not index.is_file() or not (ROOT / "inst/bin/bluetcl").is_file():
        raise SystemExit("first run from repo root: make -j32 GHCJOBS=16 install-src")
    original = index.read_text()
    additions = []
    for package in PACKAGES:
        prefix = f"package ifneeded {package} "
        registration = (
            f"{prefix}1.0 [list source [file join $dir {package}.tcl]]"
        )
        existing = [line.strip() for line in original.splitlines()
                    if line.strip().startswith(prefix)]
        if existing and existing != [registration]:
            raise SystemExit(f"conflicting installed registration for {package}")
        if not existing:
            additions.append(registration)
    for name in FILES:
        if not (SOURCE / name).is_file():
            raise SystemExit(f"missing source script: {SOURCE / name}")
    for name in FILES:
        shutil.copy2(SOURCE / name, DESTINATION / name)
    if additions:
        index.write_text(original.rstrip("\n") + "\n" +
                         "\n".join(additions) + "\n")
    print(f"Installed optional Bluetcl scripts in {DESTINATION}")


if __name__ == "__main__":
    main()
