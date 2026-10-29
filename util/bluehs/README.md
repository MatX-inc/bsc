# Haskell scripts against the bsc library

The programs `bsc.cabal` builds as executables can also be run as scripts,
by `runghc` against the **compiled** bsc library rather than compiled to
binaries of their own. `ghci` works the same way, giving an interactive
session with the whole compiler importable.

There is no second copy of any program here. `runghc` executes a file's
`main` whatever its module is called, so `src/comp/app/dumpbo.hs` --- the
same file the `dumpbo` executable is built from, module `Main_dumpbo` ---
is also the script. The launcher in this directory resolves a tool name to
that file and runs it.

## Setup

    util/bluehs/setup.sh

`setup.sh` installs the vendored solvers under `inst/lib/SAT` (`make
install-solvers`) and runs the cabal build with that directory named in
`LIBRARY_PATH`, since the library links the solvers by name and is told no
directory (doc/cabal.md section 2); the hooks generate `BuildVersion.hs`.
The first build compiles every module of the `bsc` package; later builds
are incremental.

**Re-run `setup.sh` whenever you rebuild bsc at a new commit.** This is a
correctness requirement, not hygiene: `.bo`/`.ba` files embed the build
version string (git hash), and the library rejects `.ba` files whose stamp
differs from its own (`EBinFileVerMismatch`). The library and the `bsc` that
produced your files must share a `BuildVersion.hs`.

## Running scripts

Three equivalent ways, most convenient first:

    # from anywhere --- the launcher sets GHC_ENVIRONMENT and BLUESPECDIR:
    util/bluehs/bin/dumpbo foo.bo
    util/bluehs/bin/showrules -o out.vcd mkTop dump.vcd

    # from inside the repo --- GHC finds .ghc.environment.* by upward search:
    runghc src/comp/app/dumpba.hs foo.ba

    # explicit project context:
    cabal exec -v0 -- runghc src/comp/app/bsc2bsv.hs Foo.bs

`bin/` carries a symlink per tool, named after it, pointing at the
`bluehs` launcher, which dispatches on the name it was invoked as. The
launcher also takes a path to any `.hs` file, so a script of your own runs
the same way, as does a program of `bsc.cabal` that has no symlink here:

    util/bluehs/bluehs my-analysis.hs design.ba
    util/bluehs/bluehs src/comp/app/bsc.hs -verilog Foo.bsv

A script split across several modules names the directories that hold them
with `-i`, before the script, as `runghc` would take them:

    util/bluehs/bluehs -imy-tool my-tool/Main.hs design.ba

Startup cost is ~0.3-0.5s: the library is compiled, and only the entry
file is interpreted.

## Interactive use

    ghci        # anywhere inside the repo (but see the .ghci caveat below)
    ghci> :m + GenBin ISyntax PPrint Error
    ghci> import qualified Data.ByteString as BS
    ghci> errh <- initErrorHandle
    ghci> bs <- BS.readFile "inst/lib/Libraries/Arbiter.bo"
    ghci> (bi, bo, ipkg, hash) <- readBinFile errh "Arbiter.bo" bs
    ghci> putStr (ppReadable bi)

Every module the library exposes is importable --- parsers, type checker,
`ISyntax`/`ASyntax`, `.bo`/`.ba` (de)serialization, VCD handling,
scheduling. (`cabal repl bsc` also works but interprets the library from
source, which is much slower to load.)

## Caveats and invariants

* **Do not start `ghci` with cwd `src/comp`.** The `src/comp/.ghci` there
  (for the interpret-from-source workflow) is merged into the session and
  fights the package environment. Use the repository root, or
  `ghci -ignore-dot-ghci`.
* **The solver libraries are loaded, not linked in.** `bsc.cabal` names
  `libstp` and `libyices` in `extra-libraries`, so every script and every
  `ghci` session opens them at load, and the library's shared object records
  them as dependencies. They come from the installed tree, `inst/lib/SAT`:
  the launcher names it in `LD_LIBRARY_PATH` for GHC's loader, and the
  shared object carries the rpath that resolves them (`cabal.project`), the
  build tree's `lib/SAT` beside it pointing at the installed copies. The
  distribution's registration names its own `SAT` directory, so plain `ghci`
  works there without the launcher.
* **`BLUESPECDIR` must name a real `inst/lib`** for the tools that read it
  (`showrules` and `vcdcheck` use it for `%` expansion and default `.ba`
  search paths). The launcher defaults it to `<repo>/inst/lib`, which is
  where `make install-src` puts it.

## Distribution

`make install-bluehs`, after `make install-src`, installs a self-contained
copy of all this at `inst/bluehs`: a pruned GHC runtime, a package store
holding the compiled bsc libraries (every package of `cabal.project`) and
the dependencies of the build plan, the SAT solver libraries, the tool
entry scripts (every program of `bsc.cabal` but `bluetcl`), and this
launcher as `bin/bluehs`. It runs scripts with **no Haskell toolchain
installed**, and the tree can be moved with the rest of `inst`:

    inst/bluehs/bin/bluehs dumpbo foo.bo
    inst/bluehs/bin/bluehs -imy-tool my-tool/Main.hs design.ba

It needs from the host only the C and C++ runtimes, libgmp, zlib and Tcl,
and a C compiler for scripts that use CPP; other scripts run with no C
compiler reachable. `util/bluehs/mk-dist.sh` does the work; it needs `ghc`
and `cabal` on `PATH` (the GHC it ships), `python3`, `cpio`, and `patchelf`
on Linux. It builds the project a second time from another directory, so
the checkout's own build and environment file are untouched, resolves
dependencies against the project's `index-state` into the cabal store
(the files a dependency builds record the store's path, so a store under
the checkout would make the distribution depend on where the checkout is;
the ladder checks that it does not), ships the plan's libraries and no
build tool, strips the solver libraries, and before finishing it
runs a copy of the tree from another directory under an empty environment
with a probe script that imports the compiler's modules, and checks that
the library's build version matches `inst/bin/bsc`'s and that the probe is
the only module interpreted, so the compiler's modules come from the
compiled library. The package set and the program list are read off
`cabal.project` and `bsc.cabal`, so a later split of the library into
packages, or a later program, needs no change here.

Build it in the same tree as the `bsc` beside it: the library rejects `.ba`
files whose build version stamp differs (see Caveats). The second build
takes the checkout's `cabal.project.local` when there is one, so a
developer's settings reach the shipped libraries as they reach the main
build.

The determinism ladder (`doc/cabal.md` section 5) installs the distribution
on every leg and hashes it with the compiler: `inst/bluehs` must be the same
bytes whatever the parallelism and the checkout directory, which the
relocation above makes possible and the ladder checks.

Everything redistributed is covered in `inst/bluehs/LICENSES/`:
`LICENSE.ghc` (compiler/runtime), a generated `LICENSE.ghc_pkgs` enumerating
every shipped Haskell package with license and copyright (via
`src/comp/make-ghc-pkg-info.sh` over the exact shipped package closure, from
the `bsc` package), and the STP/Yices texts for the bundled solver libraries.

## Not converted

* `bsc` itself: works as a script mechanically, but the ~1.5s/invocation
  interpretation cost of the 2300-line driver is wrong for a tool invoked
  once per compilation unit. The right eventual shape is moving `hmain` into
  a library module (`Driver`), making `bsc` a 3-line compiled `Main`, after
  which custom driver *scripts* are trivial for those who want them.
* `bluetcl`: structurally not a script: its Haskell `main` hands control to
  Tcl through a C shim (`bluetcl_shim.c`) compiled into the executable, which
  an interpreted run cannot supply.
