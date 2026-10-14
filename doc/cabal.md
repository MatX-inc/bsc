# The cabal build of bsc

One cabal package at the repository root builds the compiler, and `make
install-src` builds the solvers, runs it and installs what it built (section
2). The make build's own `ghc --make` targets remain beside it and keep
generating their own `Warmup.hs` and `BuildVersion.hs`. The comments in
`bsc.cabal`, `cabal.project` and `SetupHooks.hs` cite this document.

## 1. Shape

`bsc.cabal` is a `build-type: Hooks` package with one library, the compiler,
and one `executable` stanza per program. The library's `hs-source-dirs` are
the three compiler roots (`src/comp`, `src/comp/Libs`, `src/comp/GHC/posix`),
`src/Parsec`, `src/vendor/htcl` and the solver bindings under
`src/vendor/stp/HaskellIfc` and `src/vendor/yices/v2.6/HaskellIfc`. Its
`exposed-modules` lists every module, the generated `Warmup`, `BuildSystem`
and `BuildVersion` included (the hooks declare those autogen). The hooks
hold the list to the tree when the package configures: under each source
directory of the library below `src/comp`, walked recursively, a `.hs` or
`.lhs` file is a module when its first `module X` header (plain or
bird-track `> module X`) names the module its path relative to that root
spells, and every module so found must be listed, or configuring fails
naming the missing ones. A file whose header disagrees is not a module of
the library: a program main (`src/comp/app/bsc.hs` is `Main_bsc`), or
`src/comp/Libs/IOUtil.hs` seen from `src/comp` (it is `IOUtil`, a module of
the `Libs` root). The generated three are skipped if the make build has left
them in the tree.

Each program is one entry module under `src/comp/app` over the library
(`bluetcl`'s entry module `bluetcl_Main.hs` hands control to Tcl through the
C shim `bluetcl_shim.c`, with the helper module `Bluetcl` beside it); linking
the programs against the library archive is what removes the link-order
nondeterminism of the make build's `ghc --make` link. It is one package, not
sublibraries, because cabal-install builds a Hooks package as one unit and
rejects sublibraries in it.

## 2. Building

    make install-solvers
    LIBRARY_PATH=$PWD/inst/lib/SAT cabal build all
    $(cabal list-bin bsc:exe:bsc) -v

The library links the solvers by name, so the linker has to find `libstp`
and `libyices` first. The cabal build does not build them: `make
install-solvers` builds the vendored copies with their own Makefiles and
installs them under `inst/lib/SAT`, and `LIBRARY_PATH` names that directory
for every link, the library's shared object included; `make install-src`
does both. A build may instead link copies the system provides, with no
variable set. Cabal is told no directory either way, so nothing of where the
solvers are reaches the registration or an rpath (section 3); a configure
hook says this again when the linker finds nothing.

Build with cabal-install 3.16.1.0. The lower bound is the hooks:
`setup-depends` names `Cabal ^>=3.16` and `Cabal-hooks ^>=3.16`, and
cabal-install caps the setup-scope Cabal below its own major version plus
two. The upper bound is the GHC job server: cabal-install 3.16 speaks
semaphore protocol v1, which GHC 9.8 through 9.14.1 implement; 3.18 speaks
only v2, which GHC reports from 9.14.2 and 10.2, and with an older GHC it
warns and falls back to `-j` per package (correct, but oversubscribed). The
pin moves when the GHC in use moves.

GHC 9.14 or later gives `-fobject-determinism`; an older compiler builds
without it, and the ladder of section 5 means nothing then. The native
dependencies are the make build's: Tcl headers and library (`platform.sh`
finds them), a C and C++ compiler, and the tools the vendored solvers'
Makefiles run (autoconf, bison, flex, gperf), as installed by
`.github/workflows/install_dependencies_*.sh`.

`cabal.project` sets `optimization: 2` (parity with the make build), `jobs:
$ncpus` with `semaphore: True` (one job server for cabal-install and every
`ghc -j`), `-j` inside the package, `write-ghc-environment-files: always`
(bare `ghci` and `runghc` find the library from any directory of the
checkout), and `-fobject-determinism` for the Hackage dependencies under
`package *`, since `program-options` reaches local packages only; `bsc.cabal`
carries the flag for the package under `if impl(ghc >= 9.14)`. The same
file gives the library's shared object its rpath, `$ORIGIN/../lib/SAT`
(`@loader_path` on Darwin), under `program-options`, which reaches every GHC
invocation: Cabal 3.16 passes a library's `ld-options` to the programs'
links but not to the library's own (section 3). Machine
settings go in `cabal.project.local`, which git ignores. The build writes
nothing outside `dist-newstyle`, the autogen directories and the
`.ghc.environment.*` file at the root that `write-ghc-environment-files` asks
for; nothing under `src/comp` or `src/vendor` is written.

### Installing

    make install-src

installs the cabal build: `src/comp/Makefile`'s `install` runs `cabal build
all` and copies every executable of `bsc.cabal` into `inst/bin`, then the
libraries are built with that compiler as before. `install-extra` is the same
install, kept for callers that run both. The make build's own `ghc --make`
targets remain for a build without cabal-install (`make -C src/comp bsc`) and
are not what `install` installs. `GHCJOBS` is passed as cabal's `-j`, which
with the project's semaphore is the number of GHC slots in all; the heap is
not capped, and the full build's high-water mark on four cores was under 6 GB.

`BuildVersion` names the commit, and its rule runs when cabal-install builds
the package, which it does when a declared source changed. A commit that
touches no source of the package, such as a testsuite or workflow change,
would leave the previous commit's version in the programs, so `install`
writes HEAD into `build/cabal-head`, which `bsc.cabal` declares in
`extra-source-files`: a new commit is a changed source, and the version
follows. A plain `cabal build` after such a commit keeps the previous version
until a source changes, as section 3 describes; `make install-src` does not.

With `BSC_BUILD_PKGINFO` set, as the release workflows set it, the install
also writes `LICENSES/LICENSE.ghc_pkgs` from the packages the programs were
linked against: the in-place library, cabal-install's store and GHC's own,
walked from the library by `src/comp/make-ghc-pkg-info.sh`.

## 3. The hooks

`SetupHooks.hs` at the root is the package's hooks module (`module SetupHooks
(setupHooks)`), run with the repository root as the working directory:

- The module-list check, the rule of section 1, as a configure hook on the
  library that fails when a module under `src/comp` is not listed.
- `Warmup`, one per library and per executable, declared autogen and
  generated before the component builds. `ghc --make -jN` produces objects
  that vary with scheduling unless the whole external interface set enters
  GHC's environment in one order before any module compiles (GHC.Core.Rules,
  "Note [Overall plumbing for rules]"); `Warmup` is that barrier: `import M
  ()` for every exposed module of every direct dependency package of the
  component, sorted, read from the package database at build time. Direct
  dependencies only, since the transitive closure would name modules of
  packages GHC was not given. The barrier holds only where every root of the
  compiled unit imports it: a library module that imports nothing else from
  the library carries `import Warmup ()`, and so does each program's entry
  module (for `bluetcl`, `Bluetcl`, which `bluetcl_Main.hs` imports); in the
  make build the same line resolves to make's `Warmup.hs`. A `Warmup.hs` the
  make build has generated under `src/comp` would be compiled in place of the
  generated module, since GHC takes `hs-source-dirs` first, so configuring
  refuses while one, or a `BuildSystem.hs`, is there.
- `BuildSystem` and `BuildVersion` on the library, also autogen: the first
  from the host platform, the second by running
  `src/comp/update-build-version.sh` in the autogen directory with `GIT_DIR`
  and `GIT_WORK_TREE` naming the repository, so a build directory outside the
  checkout works. The rule runs whenever Setup builds the package, which
  cabal-install decides from the declared sources, not the repository: an
  incremental build after a commit that touched none of them keeps the
  previous version string until one does. `make install-src` closes that
  gap by writing HEAD into `build/cabal-head`, a declared source (section
  2); a plain `cabal build` does not. A clean build is always right; the
  make build runs the script every time and has no such gap. The rule also
  monitors `HEAD` and the reflog, for a build system that runs the rules
  itself (cabal-install with a Cabal 3.18 setup).
- Tcl, on the library: the include, link and `-D` flags from `platform.sh`,
  injected at configure time because a path of the build machine cannot be
  written in `bsc.cabal`; they reach `bluetcl` through the package database.
- The solvers, on the library: the cabal build does not build them. The
  library links `stp` and `yices` by name (`extra-libraries` in `bsc.cabal`),
  against whichever copies the linker finds, as any package links a C
  library; the make build builds and installs the vendored copies first and
  names their directory in `LIBRARY_PATH` for the cabal build (section 2).
  Cabal is given no directory, so none reaches the registration or an rpath,
  and the linked bytes do not depend on where the tree or the solvers are.
  The solvers are shared libraries (the dynamic link is yices's GPL
  boundary), so the library's dynamic object, what `ghci` and `runghc` load,
  records them as dependencies; everything resolves them beside itself,
  `lib/SAT`, the make build's rpath (`SAT_RPATH_FLAGS` in
  `src/comp/Makefile`), `$ORIGIN/../lib/SAT` (`@loader_path` on Darwin): the
  programs from the library's `ld-options` in `bsc.cabal`, inherited through
  the package database, and the library's shared object from `cabal.project`
  (section 2). Two hooks remain: a
  configure hook that checks the linker finds the solvers and says what to do
  when it does not, and a post-build hook that links `lib/SAT` beside each
  artifact in the build tree to the `LIBRARY_PATH` directory holding them,
  so the programs run where they are built.

## 4. Adding a module, adding a program

A module is its file under `src/comp`, `src/comp/Libs` or
`src/comp/GHC/posix` and its name in `exposed-modules`; the check of section
1 names a forgotten one at the next configure, and the edit to `bsc.cabal` is
what makes cabal-install reconfigure. A module importing nothing else from
the library is a root and carries `import Warmup ()` (section 3).

A program is one `executable` stanza: `import: common_exes`, `hs-source-dirs:
src/comp/app`, `main-is: <name>.hs`, `ghc-options: -main-is Main_<name>`,
`other-modules: Warmup`, and its direct dependencies beyond `bsc` and `base`.
The `Main_<name>` header keeps the file out of the library under the rule of
section 1, and the entry module carries `import Warmup ()`.

## 5. The determinism ladder

The claim: one commit, built from a clean `dist-newstyle` at the project's
settings, serially, oversubscribed and in other checkout directories, gives
bit-identical objects, interfaces, archives and programs, and a rebuild after
an edit and its revert lands on the same bytes. `util/cabal-ladder.sh`:

- `hash OUT` hashes one build: every object, archive and program (from
  `cabal list-bin bsc:exe:<name>` for each `executable` stanza) as bytes,
  with paths relative to the repository so directories compare. An interface
  file is hashed as the text of `ghc --show-iface` minus its interface hash
  and flags fingerprint, with the package environment off: GHC fingerprints
  the flags it was given, and cabal passes the autogen include directory by
  absolute path, so the bytes of a `.hi` depend on the checkout directory
  while its content does not. A list with no `.hi` lines or holding the
  digest of empty text is refused, a failing or silent `--show-iface` fails
  the run, and a partial list is removed, so nothing compares IDENTICAL by
  producing nothing.
- `verdict REF OTHER...` compares each list against the first, reporting
  differences by category (program, object, interface, archive); a missing
  list is MISSING, and the verdict fails rather than passing on nothing.
- `run` is the ladder on one machine: legs A and B at the project's settings
  from clean (repeatability), C at `-j1` (serial against parallel), D at
  `-j64` (oversubscription), each compared with A; then a marker edit to one
  module with a few dozen importers (`src/comp/Libs/IOUtil.hs`), a rebuild,
  the revert and a rebuild that must match A. It needs GHC 9.14 or later, a
  clean working tree and no `cabal.project.local`; results go to
  `determinism-results/<timestamp>`.

`.github/workflows/cabal.yml` runs the same two halves across machines and
directories, with cabal-install pinned to 3.16.1.0 (section 2): a pull
request builds `-j1` against `-j64` in one directory on ubuntu-22.04 and
macos-15; a dispatch of the full ladder adds `-j16` there and `-j64` in two
more directories, which checks that the build does not depend on its path.
Every planned leg is named to the verdict, so a failed leg is MISSING, not
skipped. `BuildVersion` embeds the commit, so a ladder compares one commit. A
failure is a design stop, not a mechanical fix: the first suspects are a root
without the Warmup import, interface content the direct-dependency rule does
not cover, and the programs' entry modules.

## 6. Decomposing into packages later

A second library is a second `.cabal` with hooks of its own: a copy of
`SetupHooks.hs`, or a shared hooks package again (a `Simple` library named in
each package's `setup-depends`, re-exported by each `SetupHooks.hs`).
Sublibraries are not an option (section 1). What a split has to keep: no two
packages share a source directory, since GHC's finder resolves a home-unit
module from the source path before it consults packages and each would
compile a private copy of a module it imports from the other; every root of
every package imports `Warmup`, and in-place packages count as direct
dependencies, so a dependent's `Warmup` covers the carved library; the Tcl
hook follows `HTcl.hs` to the package that holds it, and the solvers'
`extra-libraries` and `ld-options` follow `STP.hs` and `Yices.hs`, while
`LIBRARY_PATH` reaches every package's links as it reaches one. The
semaphore keeps the effective parallelism at the configured slots as the
build splits, where `-j` per package would multiply it, and the ladder is
unchanged (`hash` finds everything under
`dist-newstyle/build`; the incremental round takes one leaf per package). An
import-DAG check, that every import edge lies inside the package DAG and
every root carries the import, can return as a script when there is a DAG to
check.
