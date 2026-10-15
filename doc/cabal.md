# The cabal build of bsc

One cabal package at the repository root builds the compiler, and `make
install-src` builds the solvers, runs it and installs what it built (section
2); there is no other build of the compiler. The comments in `bsc.cabal`,
`cabal.project` and `SetupHooks.hs` cite this document.

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
the `Libs` root). The generated three are exempt (a copy of one under
`src/comp` is refused, section 3).

Each program is one entry module under `src/comp/app` over the library
(`bluetcl`'s entry module `bluetcl_Main.hs` hands control to Tcl through the
C shim `bluetcl_shim.c`, with the helper module `Bluetcl` beside it); linking
the programs against the library archive is what removes the link-order
nondeterminism of a `ghc --make` link of each program. It is one package, not
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
cabal-install caps the setup-scope Cabal below its own major version plus two.
The upper bound is the GHC job server: cabal-install 3.16 speaks semaphore
protocol v1, which GHC 9.8 through 9.14.1 implement; 3.18 speaks only v2, which
GHC reports from 9.14.2 and 10.2, and with a GHC that speaks only v1 (9.8
through 9.14.1; the project turns the semaphore on from 9.12) it warns and
falls back to `-j` per package (correct, but oversubscribed). The pin moves
when the GHC in use moves.

GHC 9.4.7 is the floor by choice, not by what the sources need: it is the GHC
that Ubuntu 24.04 packages (26.04 packages 9.10.3, 22.04 8.8.4), so
`bsc.cabal`'s lower bounds are 9.4.7's boot libraries, the language is GHC2021,
and CI builds with 9.4.7. The sources themselves build from GHC 9.2, the first
with GHC2021; 22.04's 8.8.4 has not built them since c1348cb6d (bytestring
0.11's `indexMaybe`), and its runner image GitHub retires in April 2027.
`-fobject-determinism` is on from GHC 9.12.1, where it was introduced, and GHC
compiles in parallel under the same condition; an older compiler compiles each
package serially, so GHC's own parallelism has nothing to vary below 9.12. The
claim of section 5 is measured on 9.12.2, 9.12.4 and 9.14.1 with GHC parallel,
and on 9.4.7 with GHC serial; its rebuild after an edit and its revert on
9.14.1, where it holds, and on 9.4.7, where it does not. The native
dependencies are those of `make install-src`: Tcl headers and library
(`platform.sh` finds them), a C and C++ compiler, and the tools the vendored
solvers' Makefiles run (autoconf, bison, flex, gperf), as installed by
`.github/workflows/install_dependencies_*.sh`.

`cabal.project` pins `index-state`, so the versions of the Hackage dependencies
follow the commit rather than the build date. Moving it is a change of its own,
since the dependency plan and the bytes of the build move with it: set it to
any instant the local index covers (`cabal update` reports the index's latest)
and build. An index fetched before the pin stops the build with cabal's own
message, and `cabal update` is the remedy. The file also sets
`optimization: 2`, `jobs: $ncpus`, `write-ghc-environment-files: always` (bare
`ghci` and `runghc` find the library from any directory of the checkout), and
from GHC 9.12 `semaphore: True` (one job server for cabal-install and every GHC
compile of the build) and `-fobject-determinism` for the Hackage dependencies
under `package *`, since `program-options` reaches local packages only;
`bsc.cabal` carries the flag for the package under the same
`if impl(ghc >= 9.12)`. The same file gives the library's shared object its
rpath, `$ORIGIN/../lib/SAT` (`@loader_path` on Darwin), under
`program-options`, which reaches every GHC invocation: Cabal 3.16 passes a
library's `ld-options` to the programs' links but not to the library's own
(section 3). Machine settings go in `cabal.project.local`, which git ignores.
The build writes nothing outside `dist-newstyle`, the autogen directories, the
`.ghc.environment.*` file at the root that `write-ghc-environment-files` asks
for, and the install stamp `build/cabal-head` (section 3); nothing under
`src/comp` or `src/vendor` is written.

### Installing

    make install-src

installs the cabal build: `src/comp/Makefile`'s `install` runs `cabal build
all` and copies every executable of `bsc.cabal` into `inst/bin`, then the
libraries are built with that compiler as before. `install-extra` is the same
install, kept for callers that run both. The build's flags are `bsc.cabal`'s
and `cabal.project`'s; a variant (unoptimized, debug, profiling) is a
`cabal.project.local`, as INSTALL.md describes. `GHCJOBS` is passed as cabal's
`-j`, which from GHC 9.12, with the project's semaphore, is the number of GHC
slots in all, and below it the number of packages building at once.
`GHCRTSFLAGS`, GHC's RTS options as CI sets them to cap the heap on its
runners, is passed to cabal as GHC options, which reach every GHC of the
build; unset, the heap is not capped, and the full build's high-water mark on
four cores was under 6 GB.

`BuildVersion` names the commit, and its rule runs when cabal-install builds
the package, which it does when a declared source changed. A commit that
touches no source of the package, such as a testsuite or workflow change,
would leave the previous commit's version in the programs, so `install`
writes HEAD into `build/cabal-head`, which `bsc.cabal` declares in
`extra-source-files`: a new commit is a changed source, and the version
follows. The hooks' version rule writes the same stamp, so it exists from
the first build: cabal-install watches the declared sources it finds when it
records a build, and a stamp first written by an install after a build
would go unwatched. A plain `cabal build` after such a commit keeps the
previous version until a source changes, as section 3 describes;
`make install-src` does not.

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
  module (for `bluetcl`, `Bluetcl`, which `bluetcl_Main.hs` imports). A
  `Warmup.hs`, `BuildSystem.hs` or `BuildVersion.hs` under `src/comp` would be
  compiled in place of the generated module, since GHC takes `hs-source-dirs`
  first, so configuring refuses while one is there, and so does each build.
- `BuildSystem` and `BuildVersion` on the library, also autogen: the first
  from the host platform, the second by running
  `src/comp/update-build-version.sh` in the autogen directory with `GIT_DIR`
  and `GIT_WORK_TREE` naming the repository, so a build directory outside the
  checkout works; the same script, with `--name`, gives the Bluesim kernel
  its version header, so the two agree. The rule runs whenever Setup builds
  the package, which cabal-install decides from the declared sources, not
  the repository: an incremental build after a commit that touched none of
  them keeps the previous version string until one does. `make install-src`
  closes that gap by writing HEAD into `build/cabal-head`, a declared source
  (section 2); a plain `cabal build` does not. A clean build is always
  right. The rule also monitors `HEAD` and the reflog, for a build system
  that runs the rules itself (cabal-install with a Cabal 3.18 setup).
- Tcl, on the library: the include, link and `-D` flags from `platform.sh`,
  injected at configure time because a path of the build machine cannot be
  written in `bsc.cabal`; they reach `bluetcl` through the package database.
- The solvers, on the library: the cabal build does not build them. The
  library links `stp` and `yices` by name (`extra-libraries` in `bsc.cabal`),
  against whichever copies the linker finds, as any package links a C
  library; `make install-src` builds and installs the vendored copies first
  and names their directory in `LIBRARY_PATH` (section 2).
  Cabal is given no directory, so none reaches the registration or an rpath,
  and the linked bytes do not depend on where the tree or the solvers are.
  The solvers are shared libraries (the dynamic link is yices's GPL
  boundary), so the library's dynamic object, what `ghci` and `runghc` load,
  records them as dependencies; everything resolves them beside itself,
  `lib/SAT`, through the rpath `$ORIGIN/../lib/SAT` (`@loader_path` on
  Darwin): the programs from the library's `ld-options` in `bsc.cabal`,
  inherited through the package database, and the library's shared object
  from `cabal.project` (section 2). Two hooks remain: a
  configure hook that checks the linker finds the solvers and says what to do
  when it does not, and a post-build hook that links `lib/SAT` beside each
  artifact in the build tree to the `LIBRARY_PATH` directory holding them,
  so the programs run where they are built.

A change to what the configure hooks produce (the Tcl and solver settings
above) needs a reconfigure: cabal-install rebuilds the Setup when
`SetupHooks.hs` changes, but configures the package again only when the
package's own inputs change, so touch `bsc.cabal` or build clean.

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
bit-identical objects, interfaces, archives, programs and shared objects, and,
from GHC 9.12, a rebuild after an edit and its revert lands on the same bytes;
and the installed tree, `make install-src` and `make install-bluehs` over that
build, is the same bytes too. The programs link against the library archive in
one order, the Warmup imports give every module the external interface set in
one order (section 3), `-fobject-determinism` holds the object code (GHC 9.12,
section 2), and no directory of the checkout is written into any of them, since
the solvers are linked by name (section 3). In the installed tree the compiler
stores file names relative to the build directory (upstream 1135), and the
Bluesim and VPI libraries map the checkout out of their debug information
(`-ffile-prefix-map` in their Makefiles). The programs carry no local symbols
(`-Wl,-x` in `bsc.cabal`): GHC compiles its link stubs from temporary files
named by a per-process counter, and the C compiler records each name as a FILE
symbol, which once differed between two builds of one commit; nothing reads a
program's local symbols. `util/cabal-ladder.sh`:

- `hash OUT [DIR...]` hashes one build: every object, archive, shared
  object and program (the programs from `cabal list-bin bsc:exe:<name>` for
  each `executable` stanza) as
  bytes, with paths relative to the repository so directories compare, then
  every file of each installed directory named after the list (CI names
  `inst`, the installed tree, with the script runtime of `util/bluehs` under
  it), as bytes except the interfaces under a `store`, which are hashed as
  below. An interface
  file is hashed as the text of `ghc --show-iface` minus its interface hash
  and flags fingerprint, with the package environment off: GHC fingerprints
  the flags it was given, and cabal passes the autogen include directory by
  absolute path, so the bytes of a `.hi` depend on the checkout directory
  while its content does not. A list with no `.hi` lines or holding the
  digest of empty text is refused, a failing or silent `--show-iface` fails
  the run, and a partial list is removed, so nothing compares IDENTICAL by
  producing nothing. Beside the list it writes a record of what built it,
  `c-j1.toolchain` beside `c-j1.sha256`: the runner image when GitHub's runner
  names one, the system, the first line the C compiler, the linker and `ar`
  print for their version, ghc, cabal, patchelf where present, and the tool
  and dependency versions of the package's `cabal_macros.h`, the header Cabal
  includes into every module that runs the C preprocessor. That header names
  every program Cabal configured and found a version for (`ld`, `ar` and `tar`
  carry none), whether the package uses it or not, so a program installed at
  another version between two legs' dependency steps is in the interface of
  every such module, in the package and in the store alike, with nothing in
  the build to blame; the record names it.
- `verdict REF OTHER...` compares each list against the first, reporting
  differences by category (program, shared object, object, interface,
  archive; installed for an installed directory's files, and distribution
  for a bluehs runtime's under it); a missing list is MISSING, and the
  verdict fails rather than passing on nothing. Under a DIFFER it says
  whether the two legs' toolchain records agree and prints their difference
  when they do not, and identical lists whose records differ are noted too;
  the records do not decide the result, which is the bytes'.
- `rebuild NAME [DIR...]` is that round alone, for CI: the rebuilds, each
  hashed as `hash` does into `NAME-rebuild`, `NAME-edit` and `NAME-revert`,
  with `BUILD_CMD` as the build (`cabal build all` unless given; CI gives the
  make targets, so the installed directory named to it is rebuilt and hashed
  too) and `REBUILD_STEPS` as the steps where the caller decides them (CI
  passes its plan's, so the lists made are the lists its verdict expects).
  Below GHC 9.12 the round is the rebuild with nothing changed alone: there a
  module's object carries the session's uniques, so a module recompiled alone
  differs from the one compiled in the full build (at 9.4.7, `IOUtil.o` in the
  name of one local label and the relocation naming it, with the same code and
  data) while clean builds agree among themselves; from 9.12
  `-fobject-determinism`, which the project turns on there, holds it: at 9.14.1
  a module recompiled alone under a perturbed unique supply is identical with
  the flag and differs without it (section 2).
- `run` is the ladder on one machine: legs A and B at the project's settings
  from clean (repeatability), then the in-place round on B: a rebuild with
  nothing changed, which must match A, and from GHC 9.12 a marker edit, a
  comment, to one module (`src/comp/Libs/IOUtil.hs`), a rebuild, which
  recompiles that module alone, the revert and a rebuild, which must match A as
  well; then C at `-j1` (serial against parallel) and D at `-j64`
  (oversubscription), each compared with A. It installs the solvers first
  (`make install-solvers`, section 2) and names them to the linker for every
  leg. It runs with any GHC the package builds with (GHC's own parallelism
  varies only from 9.12, section 2), and needs a clean working tree and no
  `cabal.project.local`; the lists, the toolchain records and the logs go to
  `determinism-results/<timestamp>`.
- `toolchain` prints the record `hash` writes beside a list, for the machine
  it runs on and the build in `dist-newstyle`.

`.github/workflows/cabal.yml` runs the same two halves across machines and
directories, with cabal-install pinned to 3.16.1.0 (section 2): on ubuntu-24.04
and macos-15 it compares the serial build against a second serial build in
another directory, `-j16`, and `-j64` three times in three directories, which
checks that the build depends neither on its parallelism nor on its path. Each
leg then installs the tree (`make install-src`) and the bluehs runtime over it
(`make install-bluehs`, whose second build of the libraries takes the leg's
`cabal.project.local`), and hashes the whole of `inst` with the build: the
programs, the solvers, the Bluespec libraries, the Bluesim and VPI libraries,
the Verilog and Tcl libraries and the runtime are all held to the same claim,
the same bytes whatever the parallelism and the directory. The two serial legs
then rebuild in place, with nothing changed and, from GHC 9.12, after a marker
edit and its revert, and those rebuilds are held to the reference as well, in
the reference directory and in another. It runs on the project's own pull
requests and on a dispatch; a fork's pull requests do not run it, since a
fork's branches are a developer's loop, and a fork dispatches it when it
qualifies a release. Every planned leg and rebuild is named to the verdict, so
a failed leg is MISSING, not skipped. Each leg uploads its toolchain records
with its lists, and the verdict's log prints the reference's, so a difference
is read against what built the legs: once, a macOS leg whose `brew update`
failed kept the image's pkgconf while the other legs upgraded theirs, and the
interface of every module that runs the C preprocessor differed through
`cabal_macros.h` alone. `BuildVersion` embeds the commit, so a ladder compares
one commit. A failure is a design stop, not a mechanical fix: the first
suspects are a root without the Warmup import, interface content the
direct-dependency rule does not cover, and the programs' entry modules.

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
