# The cabal build of bsc

One cabal package at the repository root builds the compiler; `make
install-src` builds the solvers, runs that build and installs the result.
INSTALL.md covers prerequisites, the toolchain and the build variants.

## 1. Shape

`bsc.cabal` is a `build-type: Hooks` package: one library, the compiler, and
one `executable` stanza per program. The library's `hs-source-dirs` are
`src/comp`, `src/comp/Libs`, `src/comp/GHC/posix`, `src/Parsec` and the
vendored Tcl and solver bindings under `src/vendor`; `exposed-modules` lists
every module, the generated `Warmup` and `BuildVersion` included, and an
imported module that is not listed fails the build, naming it
(`-Werror=missing-home-modules`). The generated modules are `autogen-modules`,
which is what lets `cabal sdist` leave them out. Each program is an entry
module under `src/comp/app` linked against the library's archive. It is one
package, not sublibraries: cabal-install builds a Hooks package as one unit and
rejects sublibraries in it.

## 2. Building

    make install-solvers
    LIBRARY_PATH=$PWD/inst/lib/SAT cabal build all

The library links `stp` and `yices` by name: `make install-solvers` installs
the vendored copies under `inst/lib/SAT`, which `LIBRARY_PATH` names to the
linker; with it unset, the linker takes the system's copies. Cabal is told
no directory, so none reaches the registration, an rpath or the bytes.
Without the solvers on the link path, configuring stops with cabal's own
message (INSTALL.md).

- GHC 9.4.7 through 9.14: 9.4.7 is Ubuntu 24.04's GHC, and the lower bounds
  are its boot libraries; the upper bounds are the next major versions after
  9.14.1's, the newest CI builds with, so a newer GHC needs them raised.
- cabal-install 3.16.1.0, not 3.18: the hooks need Cabal 3.16, and 3.16
  speaks the job semaphore protocol v1, GHC's since 9.8; 3.18 speaks only
  v2, which GHC adds in 9.10.4, 9.12.5, 9.14.2 and 10.0.1, and with a v1 GHC
  falls back to plain `-j` per package.
- `index-state` in `cabal.project` pins the Hackage index, so the
  dependencies follow the commit; an older index stops the build
  (INSTALL.md).

From GHC 9.12, the first with `-fobject-determinism`, the flag is on
(`bsc.cabal`, and `cabal.project` for every package of the build) and
`semaphore: True` puts cabal-install and every GHC of the build on one job
server, so `jobs` (cabal's `-j`, make's `GHCJOBS`) bounds the modules compiling
at once; below 9.12 parallel GHC is not deterministic, so each package compiles
serially and `jobs` bounds only the packages building at once.
`write-ghc-environment-files: always` serves bare `ghci` and `runghc`.

The programs and the library's dynamic object (what `ghci` and `runghc`
load) resolve the solvers beside themselves, `lib/SAT`, by the rpath
`$ORIGIN/../lib/SAT` (`@loader_path` on Darwin), from `bsc.cabal`'s
`ld-options`; Cabal 3.16 applies those to the programs' links only, so
`cabal.project` repeats it for the dynamic object.

`make install-src` installs the solvers (`src/Makefile`) and then, in
`src/comp/Makefile`'s `install`, runs `cabal build -j$(GHCJOBS) all` with
`LIBRARY_PATH` set and copies bsc and bluetcl into `inst/bin`; `make -C
src/comp install-extra` copies the other programs over the same build.
`BuildVersion` names the commit, but its rule runs only when cabal-install
builds the package (a changed declared source or a reconfigure), so `install`
first writes HEAD and the version name (`update-build-version.sh --name`) into
`build/cabal-head`, a declared source: a new commit, or a tag at the built one,
is then a changed source. The hooks write the same stamp from the first build,
since cabal-install watches only the sources it finds when it records a build.

`Setup.hs` is for a tool that runs a Setup script rather than cabal-install
(the hooks' external program): cabal-install generates the same two lines for
itself and never reads the file, and `cabal check` requires one of every
build type but Simple. Such a build runs the same hooks, with the solvers on
the link path as above, and sees nothing of `cabal.project`: it passes
`--enable-optimization=2` to `configure` for the project's -O2, takes its
dependencies from the package database it is given rather than the pinned
index, and compiles serially, which the bytes do not depend on.

## 3. The hooks

`SetupHooks.hs`, run at the repository root:

- `Warmup`, for the library and each executable: a generated import of every
  exposed module of every direct dependency, so that the external interfaces
  enter GHC's environment in one order before any other module compiles;
  otherwise `ghc --make -jN` output varies with scheduling. Every root module
  of the library and each program's entry module imports it.
- `BuildVersion`, by `src/comp/update-build-version.sh` run in the autogen
  directory; the script reads the repository it lives in, whatever the working
  directory or the caller's git environment, and a tree without one (an
  unpacked sdist) builds as no-git; the rule monitors git `HEAD` and its reflog
  for a cabal-install that runs the rules itself (Cabal 3.18). `BuildSystem` is
  checked in: a CPP test on the host OS picks the object format. A copy of a
  generated module under `src/comp`, left by an older checkout's make build,
  would be compiled instead: `make install-src` and the per-program targets
  remove the two before building, and a bare `cabal build` refuses while one is
  there (`make -C src/comp no-stale-modules` removes them).
- Tcl, at configure: the include, link and `-D` flags, from `platform.sh`.
- Solvers: after each build, `lib/SAT` beside the artifact is linked to the
  `LIBRARY_PATH` directory holding them, so the build tree runs in place
  (copies in a system directory need no link).

cabal-install builds without configuring when only sources changed, and a
changed `SetupHooks.hs` rebuilds the Setup without reconfiguring, so a change
to what a configure hook produces takes effect at the next configure: edit
`bsc.cabal`, or build clean.

## 4. Adding a module, adding a program

A module is its file under a `src/comp` root and its name in
`exposed-modules`; GHC fails the build naming an imported module that is
missing there, and the edit to `bsc.cabal` is what makes cabal-install
reconfigure. A module importing nothing else from the library is a root and
carries `import Warmup ()` (section 3).

A program is one `executable` stanza: `import: common_exes`, `hs-source-dirs:
src/comp/app`, `main-is: <name>.hs`, `ghc-options: -main-is Main_<name>`,
`other-modules: Warmup`, and no packages of its own, since `common_exes` brings
the library's (`common_deps`); `src/comp/app` is no source directory of the
library, so the entry module is the program's alone, and it carries `import
Warmup ()`. (bluetcl differs: no `-main-is`, a helper module and a C shim.)

## 5. The determinism ladder

The claim: one commit built from clean serially, oversubscribed or in another
checkout directory, or rebuilt after an edit and its revert, gives
bit-identical objects, archives, shared objects and programs, and interfaces
identical as `ghc --show-iface` text.

What holds it: every program links against the library's archive, compiled
once; `Warmup` fixes the interface-load order; `-fobject-determinism`, from GHC
9.12, makes an object independent of the session's uniques; and the solvers are
linked by name.

`util/cabal-ladder.sh`:

- `hash OUT [DIR...]` hashes the build under `dist-newstyle` and every file of
  each installed DIR; interfaces as `ghc --show-iface` text minus the interface
  hash and flags fingerprint, since their bytes carry the checkout directory.
- `verdict REF OTHER...` compares each list with the reference; an absent list
  is MISSING.
- `run` is the ladder on one machine (two clean builds, `-j1`, `-j64`, then a
  marker edit to `src/comp/Libs/IOUtil.hs`, a rebuild, its revert and a
  rebuild), into `determinism-results/`.

`.github/workflows/cabal.yml` runs on the project's own pull requests that
touch the build and on a dispatch, which picks the GHC (default 9.14.1). Six
legs on ubuntu-24.04 and macos-15, each a checkout directory and a jobs count
(`jobs: N` and, from 9.12, `ghc-options: -jN` in a `cabal.project.local`):
`c-j1`, the reference, `a-j1`, `c-j16`, `c-j64`, `a-j64` and `d-j64`. Each
builds, installs (`make install-src`, `make -C src/comp install-extra`, `make
install-bluehs`) and hashes the build with `inst/bluehs`. A verdict job per OS
compares every planned leg against `c-j1`; a failed one is MISSING. A
difference is a design question before it is a fix: the first suspects are a
root module without the Warmup import, interface content the direct-dependency
rule does not cover, and a program's entry module.
