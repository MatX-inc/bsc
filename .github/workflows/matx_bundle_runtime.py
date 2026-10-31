"""Give an install its own runtime, so it runs on hosts with an older glibc,
such as the Rocky 9 RBE workers, and on hosts with no Tcl at all.

The ELF tools move to bin/core/, the build host's loader and runtime libraries
are copied to lib/rt/, its Tcl script library is copied to lib/tcl8.6/ with the
Tcl modules beside it in lib/tcl8/, and bin/<tool> becomes a launcher that runs
the tool on that loader (upstream's old wrapper.sh layout, plus a loader). The
launcher's library path names only the install, and it sets TCL_LIBRARY to the
copied script library: init.tcl demands the exact patchlevel of the libtcl it
shipped with, and Tcl would otherwise try the host's library first. A host
extension that looks for its own scripts beside Tcl's library, as Itcl 3 and
Itk 3 do, has to be told where they are (ITCL_LIBRARY, ITK_LIBRARY); one that
asks pkgconfig or searches auto_path, as Tk does, needs nothing. The launcher
uses builtins only, since one action passes no PATH, and sets BLUESPECDIR,
which bsc would otherwise derive from the loader's path. It does not resolve
symlinks. Bundling again is harmless.

This file lives in two repositories, which cannot reach each other:
tools/bsc-release/releaseprep/runtime.py in MatX-inc/matx, which bsc-rp imports
when it builds a release branch, and .github/workflows/matx_bundle_runtime.py
in MatX-inc/bsc, which the release workflow runs on its builder. Keep the two
identical. TODO: once every release branch bsc-rp builds carries the workflow's
copy, have build.py run it from the worktree and delete the monorepo's copy and
its runtime_test.py.

    python3 <this file> <install>     # bundle, then run bluetcl on the result
"""

from __future__ import annotations

import os
import platform
import shutil
import subprocess
import sys
import tempfile
from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path

# Copied into lib/rt. The loader is recognised as ldd's line without `=>`.
# libtcl brings libz. Both were once left to the running host, which then had
# to carry a Tcl of the same series, and a host without one could not run
# bluetcl at all.
RUNTIME = (
    "libc.so.6",
    "libm.so.6",
    "libgcc_s.so.1",
    "libstdc++.so.6",
    "libgmp.so.10",
    "libtcl8.6.so",
    "libz.so.1",
)

# The Tcl script library the bundled libtcl needs (init.tcl, the encodings),
# copied to lib/<this>. The name is Tcl's own for the directory, and its suffix
# names the tclsh asked where the host keeps it: the host's tclsh and libtcl are
# one installation.
TCL_LIBRARY_DIR = "tcl8.6"

# The Tcl modules (msgcat, which `clock` needs, http, platform, tcltest), copied
# to lib/<this>/<minor version>/. Tcl looks for them beside the script library,
# one directory per minor version. Debian instead keeps them flat inside the
# library and reaches them through a compiled-in path, which a bundled libtcl
# still consults and which a host without Tcl does not have.
TCL_MODULES_DIR = "tcl8"

RT = Path("lib") / "rt"
CORE = Path("bin") / "core"

_ELF = b"\x7fELF"

_LAUNCHER = """\
#!/bin/bash
# Written by the runtime bundler: runs bin/core/<tool> on lib/rt. Builtins only.
inst=$(cd "${0%/*}/.." && pwd) || exit
export BLUESPECDIR="${BLUESPECDIR:-$inst/lib}"
@TCL_LIBRARY@
exec "$inst/lib/rt/@LOADER@" \\
  --library-path "$inst/lib/rt:$inst/lib/SAT${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \\
  "$inst/bin/core/${0##*/}" "$@"
"""


class BundleError(Exception):
    """The install cannot be given a runtime from this host."""


@dataclass(frozen=True)
class Bundled:
    loader: str  # file name in lib/rt
    libraries: tuple[str, ...]  # sonames in lib/rt
    tools: tuple[str, ...]  # launchers in bin/
    tcl_library: str  # directory under lib/, or "" when no tool links Tcl


def _ldd(binary: Path, solvers: Path) -> str:
    """ldd's listing. The tools' rpath misses lib/SAT from bin/core/, so it is
    put on the library path."""
    env = {**os.environ, "LD_LIBRARY_PATH": str(solvers)}
    result = subprocess.run(
        ["ldd", str(binary)], capture_output=True, text=True, env=env
    )
    if result.returncode != 0:
        raise BundleError(
            f"`ldd {binary}` failed:\n  {(result.stderr or result.stdout).strip()}"
        )
    return result.stdout


def _tclsh(script: str) -> str:
    """What the host's tclsh of the bundled series prints for `script`."""
    series = TCL_LIBRARY_DIR[len("tcl") :]
    exe = shutil.which(f"tclsh{series}") or shutil.which("tclsh")
    if exe is None:
        raise BundleError(
            "no tclsh on PATH, so the host's Tcl script library cannot be found"
        )
    result = subprocess.run([exe], input=script, capture_output=True, text=True)
    if result.returncode != 0:
        raise BundleError(
            f"`{exe}` failed:\n  {(result.stderr or result.stdout).strip()}"
        )
    return result.stdout


def _is_elf(path: Path) -> bool:
    if not path.is_file() or path.is_symlink():
        return False
    with path.open("rb") as stream:
        return stream.read(4) == _ELF


def _dependencies(
    binary: Path, inst: Path, ldd: Callable[[Path, Path], str]
) -> tuple[Path | None, dict[str, Path]]:
    """The loader and the RUNTIME libraries `binary` loads; any other is refused."""
    loader: Path | None = None
    runtime: dict[str, Path] = {}
    root = inst.resolve()
    for line in ldd(binary, inst / "lib" / "SAT").splitlines():
        words = line.split()
        if not words:
            continue
        if "=>" in words:
            soname, path = words[0], words[words.index("=>") + 1]
            if path == "not" or not path.startswith("/"):
                raise BundleError(
                    f"{binary.relative_to(inst)} needs {soname}, which this host "
                    "does not have; the install does not run here either"
                )
            resolved = Path(path).resolve()
            if root in resolved.parents:
                continue  # the install's own, such as the solvers in lib/SAT
            if soname in RUNTIME:
                runtime[soname] = Path(path)
            else:
                raise BundleError(
                    f"{binary.relative_to(inst)} needs {soname} (from {path}), which "
                    "the runtime bundler does not know. Add it to RUNTIME if it "
                    "should travel with the install."
                )
        elif words[0].startswith("/"):
            loader = Path(words[0])
        # Anything else: the vDSO, or `statically linked`.
    return loader, runtime


def _tcl_library(tclsh: Callable[[str], str]) -> Path:
    """The host's Tcl script library, for the series the tools link."""
    series = TCL_LIBRARY_DIR[len("tcl") :]
    lines = [
        line.strip()
        for line in tclsh("puts [info library]; puts [info patchlevel]").splitlines()
        if line.strip()
    ]
    if len(lines) != 2:
        raise BundleError(
            f"tclsh did not report its library and patchlevel; it printed {lines}"
        )
    library, patchlevel = Path(lines[0]), lines[1]
    if not patchlevel.startswith(series + "."):
        raise BundleError(
            f"the host's tclsh is Tcl {patchlevel}, and the tools link libtcl{series}; "
            "the script library has to come from the Tcl they link"
        )
    if not (library / "init.tcl").is_file():
        raise BundleError(
            f"tclsh names {library} as its library, and there is no init.tcl there"
        )
    return library


def _copy(src: Path, dst: Path) -> None:
    """As a regular executable file, never a symlink."""
    dst.unlink(missing_ok=True)
    shutil.copyfile(src.resolve(), dst)
    dst.chmod(0o755)


def _copy_tree(src: Path, dst: Path) -> None:
    """A fresh copy, so a file the host no longer has does not linger."""
    if dst.exists():
        shutil.rmtree(dst)
    shutil.copytree(src, dst)


def bundle(
    inst: Path,
    ldd: Callable[[Path, Path], str] = _ldd,
    tclsh: Callable[[str], str] = _tclsh,
    system: str = platform.system(),
) -> Bundled | None:
    """Make the install at `inst` carry its runtime; None on macOS."""
    if system != "Linux":
        return None
    # ldd reports relative paths for a relative install.
    inst = inst.resolve()
    bin_dir, core, rt = inst / "bin", inst / CORE, inst / RT
    if not bin_dir.is_dir():
        raise BundleError(f"there is no bin/ under {inst}")

    # 1. ELF tools into bin/core/; scripts in bin/ are launchers, rewritten below.
    core.mkdir(exist_ok=True)
    for path in sorted(bin_dir.iterdir()):
        if _is_elf(path):
            path.replace(core / path.name)
    tools = sorted(p.name for p in core.iterdir() if _is_elf(p))
    if not tools:
        raise BundleError(f"no ELF tools under {bin_dir}; is this a Linux install?")

    # 2. What the tools and solvers load.
    loaders: set[Path] = set()
    runtime: dict[str, Path] = {}
    solvers = (
        sorted((inst / "lib" / "SAT").glob("*.so*"))
        if (inst / "lib" / "SAT").is_dir()
        else []
    )
    for binary in [core / name for name in tools] + [s for s in solvers if _is_elf(s)]:
        found_loader, found_runtime = _dependencies(binary, inst, ldd)
        if found_loader is not None:
            loaders.add(found_loader)
        runtime.update(found_runtime)
    if len(loaders) != 1:
        raise BundleError(
            f"the tools name {len(loaders)} loaders ({', '.join(map(str, sorted(loaders)))}); "
            "one host builds them all, so this install is not one build"
        )
    loader = loaders.pop()

    # 3. lib/rt.
    rt.mkdir(parents=True, exist_ok=True)
    wanted = {loader.name: loader, **runtime}
    for stale in rt.iterdir():
        if stale.name not in wanted:
            stale.unlink()
    for name, src in wanted.items():
        _copy(src, rt / name)

    # 4. The Tcl script library and modules, when a tool links libtcl.
    tcl_library = ""
    if f"lib{TCL_LIBRARY_DIR}.so" in runtime:
        tcl_library = TCL_LIBRARY_DIR
        series = tcl_library[len("tcl") :]
        library = _tcl_library(tclsh)
        _copy_tree(library, inst / "lib" / tcl_library)
        root = inst / "lib" / TCL_MODULES_DIR
        if root.exists():
            shutil.rmtree(root)
        for modules in (library.parent / TCL_MODULES_DIR, library / TCL_MODULES_DIR):
            if modules.is_dir():
                # A flat directory of modules is one version's leaf, not the root.
                flat = any(modules.glob("*.tm"))
                _copy_tree(modules, root / series if flat else root)
                break

    # 5. Launchers.
    launcher = _LAUNCHER.replace("@LOADER@", loader.name).replace(
        "@TCL_LIBRARY@\n",
        f'export TCL_LIBRARY="$inst/lib/{tcl_library}"\n' if tcl_library else "",
    )
    for name in tools:
        target = bin_dir / name
        target.write_text(launcher)
        target.chmod(0o755)

    return Bundled(
        loader=loader.name,
        libraries=tuple(sorted(runtime)),
        tools=tuple(tools),
        tcl_library=tcl_library,
    )


def verify(inst: Path) -> str:
    """Run the install's bluetcl with an empty environment and return the Tcl
    patchlevel it reports. Refuses a bluetcl that does not run, or that runs on
    a script library other than the install's own."""
    inst = inst.resolve()
    launcher = inst / "bin" / "bluetcl"
    library = inst / "lib" / TCL_LIBRARY_DIR
    if not launcher.is_file():
        raise BundleError(f"there is no bluetcl under {inst / 'bin'}")
    with tempfile.NamedTemporaryFile("w", suffix=".tcl", delete=False) as probe:
        probe.write("puts [info library]\nputs [info patchlevel]\n")
    try:
        result = subprocess.run(
            [str(launcher), probe.name], env={}, capture_output=True, text=True
        )
    finally:
        os.unlink(probe.name)
    if result.returncode != 0:
        raise BundleError(
            f"{launcher} does not run with an empty environment:\n"
            f"  {(result.stderr or result.stdout).strip()}"
        )
    lines = result.stdout.split()
    if len(lines) != 2 or Path(lines[0]).resolve() != library:
        raise BundleError(
            f"{launcher} runs on the Tcl library {lines[0] if lines else '(none)'}, "
            f"not the install's own {library}"
        )
    return lines[1]


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        print(f"usage: {argv[0]} <install>", file=sys.stderr)
        return 2
    inst = Path(argv[1])
    try:
        done = bundle(inst)
        if done is None:
            print(f"{inst}: not Linux, nothing to bundle")
            return 0
        print(
            f"{inst}: {', '.join(done.tools)} run on lib/rt/{done.loader} "
            f"with {', '.join(done.libraries)}"
        )
        if "bluetcl" in done.tools:
            print(
                f"{inst}: bluetcl runs Tcl {verify(inst)} from lib/{done.tcl_library}"
            )
    except BundleError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
