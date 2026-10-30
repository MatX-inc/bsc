# Deterministic BSC builds with Cabal

## 1. Determinism requirements

For the same commit, toolchain, dependencies, and build options, serial,
parallel and relocated builds must produce identical binaries, the objects,
archives, executables and shared libraries, so that performance does not
depend on how the compiler was built. An incremental build after an edit and
its revert must return to those same bytes.

This goal drives the build's requirements:

- Compile the compiler library once; link every program against it (section 2).
- Load external interfaces in a stable order through `Warmup` (section 4).
- Enable `-fobject-determinism` from GHC 9.12; compile each package serially
  on older GHCs (section 5).
- Link solvers by name with relative runtime paths, keeping checkout paths
  out of the linked artifacts (section 3).
- Track version metadata so incremental builds use the current commit and
  version name (section 4).

## 2. Package layout

[`bsc.cabal`](../bsc.cabal) uses `build-type: Hooks`: one library containing
the compiler, vendored Parsec, and Tcl and solver bindings, plus eight
executables whose entry modules live separately under `src/comp/app`.

The library lists its modules in `exposed-modules`; `-Werror=missing-home-modules`
rejects an imported module missing from the list. The generated `Warmup` and
`BuildVersion` are `autogen-modules`. cabal-install builds a Hooks package as
one unit and rejects sublibraries in it.

## 3. Building

After installing the [prerequisites](../INSTALL.md), the toolchain and the
Hackage index among them, run at the repository root:

```bash
make install-src
```

This builds the vendored solvers, then every program with cabal, and installs
`bsc` and `bluetcl` with the libraries under `inst`; `make -C src/comp
install-extra` installs the six other programs over the same build, and
`make -C src/comp bsc` builds one program and links it beside the Makefile.
Repeat `PREFIX`, `GHC` and `BSC_BUILD` across these.

### Solvers

`make install-solvers` installs the vendored `stp` and `yices` under
`inst/lib/SAT`; `make install-src` does so first and names that directory to
the linker. The library links both by name and records no directory, so no
solver path reaches the registration or the binaries, and a build without the
vendored copies, a distribution's against its system packages, links the
system's. At run time a program finds them through a relative rpath, `lib/SAT`
beside its `bin`, or in the system directories. If configuring stops with
`Missing (or bad) C libraries: stp, yices`, run `make install-solvers`; do not
take cabal's `--extra-lib-dirs` suggestion, which writes the directory into
the package registration. `STP_STUB=1` or `YICES_STUB=1` builds a stub in
place of one solver ([INSTALL.md](../INSTALL.md)).

## 4. Build hooks and generated files

[`SetupHooks.hs`](../SetupHooks.hs) runs from the repository root:

- **Configure**, for the library: query `platform.sh` for Tcl include/link
  flags and defines.
- **Pre-build:** generate each component's `Warmup`, importing every exposed
  module of its direct dependencies except `Warmup` itself, in sorted order.
  For the library, also generate `BuildVersion` and reject stale sources.
- **Post-build:** link `lib/SAT` beside the build tree's artifacts to the
  solvers, so they run in place.

Every root module (one importing no other module from its component) must
import `Warmup ()` so external interfaces load before other modules compile.

`src/comp/update-build-version.sh` generates `BuildVersion` from its own
repository, whatever the caller's directory or Git environment.

`make install-src` refreshes `build/cabal-head` (HEAD and the version name), a
declared source, so a new commit or tag rebuilds `BuildVersion`; the hook
writes it too, from the first build.

Old `src/comp/Warmup.hs` or `BuildVersion.hs` files would override autogen
copies, so the hook refuses them. Remove them with
`make -C src/comp no-stale-modules`; Make's install and per-program targets
do this automatically. They remain visible in `git status`.

## 5. Project settings and variants

[`cabal.project`](../cabal.project) permits parallel compilation only where
it preserves determinism: from GHC 9.12, `-fobject-determinism` applies to
every package and `semaphore: True` shares one job limit across cabal-install
and its GHC processes. Older GHCs compile each package serially; independent
packages may still build in parallel.

`GHCJOBS` (default: the processor count, capped at 16) is that limit, cabal's
`-j`; make's own `-j` controls its targets separately. The project also
selects `-O2` and writes a GHC environment file for bare `ghci` and `runghc`
(`src/comp/.ghci`).

cabal-install is 3.16.1.0, not 3.18 ([INSTALL.md](../INSTALL.md)).

`index-state` pins the Hackage snapshot the dependencies resolve against; an
older local index stops the build with cabal's own message (INSTALL.md). The
dependencies build once into cabal's shared store, not one under the checkout:
the store's path reaches the library's shared object (its rpath names the
store's package directories), so a store per checkout would make relocated
builds differ, besides fetching the index and building every dependency again
for each checkout.

Each variant imports `cabal.project`:

| Make option | Project file | Change |
| --- | --- | --- |
| `BSC_BUILD=NOOPT` | `cabal.project.noopt` | Disable optimization. |
| `BSC_BUILD=DEBUG` | `cabal.project.debug` | Add `-dcore-lint -debug` to BSC. |
| `BSC_BUILD=PROF` | `cabal.project.prof` | Profile all functions; `-with-rtsopts=-H256m -K256m -i1`. |

For example, `make install-src BSC_BUILD=PROF`. Local settings go in
`cabal.project.local`, or `cabal.project.<variant>.local` for a variant. Make
selects GHC with `GHC=` and passes `GHCRTSFLAGS` when compiling BSC; see
[INSTALL.md](../INSTALL.md).

`Setup.hs` is the entry point `cabal check` requires of a Hooks package;
cabal-install builds the hooks into a program of its own and never runs it.

## 6. Adding modules and programs

Add library modules under a library `hs-source-dirs` directory and list them
in `exposed-modules`. Root modules need `import Warmup ()`.

Put program entry modules under `src/comp/app` and add:

```cabal
executable example
  import: common_exes
  ghc-options: -main-is Main_example
  main-is: example.hs
```

Declare `module Main_example where` and import `Warmup ()`. `common_exes`
supplies dependencies, source directory, and generated `Warmup` declarations.
Bluetcl instead uses a C shim and helper module, without `-main-is`.
