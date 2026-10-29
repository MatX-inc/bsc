# The cabal build of bsc

One cabal package at the repository root builds the compiler beside the make
build, which is untouched and keeps generating its own `Warmup.hs` and
`BuildVersion.hs`. INSTALL.md covers prerequisites and the toolchain.

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
linker; with it unset, the linker takes the system's copies. The vendored
copies are the versions bsc is tested with; a distribution that packages the
solvers can build against its own and ship no `lib/SAT`, since the rpath only
adds that directory to the loader's search. Cabal is told no directory, so
none reaches the registration, an rpath or the bytes.
Without the solvers on the link path, configuring stops with cabal's own
message (INSTALL.md).

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
server, so `jobs` (cabal's `-j`) bounds the modules compiling at once; below
9.12 parallel GHC is not deterministic, so each package compiles serially and
`jobs` bounds only the packages building at once. `write-ghc-environment-files:
always` serves bare `ghci` and `runghc`.

The programs and the library's dynamic object (what `ghci` and `runghc`
load) resolve the solvers beside themselves, `lib/SAT`, by the rpath
`$ORIGIN/../lib/SAT` (`@loader_path` on Darwin), from `bsc.cabal`'s
`ld-options`; Cabal 3.16 applies those to the programs' links only, so
`cabal.project` repeats it for the dynamic object.

`BuildVersion` names the commit, but its rule runs only when cabal-install
builds the package (a changed declared source or a reconfigure), so the hooks
write HEAD and the version name into `build/cabal-head`, a declared source
(`update-build-version.sh --stamp`): a new commit, or a tag at the built one,
is then a changed source. The stamp is written from the first build, since
cabal-install watches only the sources it finds when it records a build.

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
  unpacked sdist) builds as no-git. `BuildSystem` is checked in: a CPP test on
  the host OS picks the object format. A `Warmup.hs` or `BuildVersion.hs` the
  make build has generated under `src/comp` would be compiled instead (`make -C
  src/comp full_clean` removes them).
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

## 5. The determinism claim

The claim: one commit built from clean serially, oversubscribed or in another
checkout directory gives the same objects, interfaces, archives, programs and
shared object, and a rebuild after an edit and its revert lands on the same
bytes.

What holds it: every program links against the library's archive, compiled
once; `Warmup` fixes the interface-load order; `-fobject-determinism`, from GHC
9.12, makes an object independent of the session's uniques; and the solvers are
linked by name.

The check of the claim, a ladder of builds hashed and compared on one machine
and in CI, is a separate series.
