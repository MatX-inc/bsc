# Evidence toys for doc/recabalization-brief.md

Two self-contained cabal projects that reproduce facts F3, F5 and F6 of the
brief. Run each with `cabal build all` from its directory (GHC 9.14.1,
cabal-install 3.16.1.0 were used on 2026-09-30).

- `shadowtest/`: as checked in, both sublibraries of `pkgb` list the shared
  directory and each compiles its own copy of `M1`; `cabal build all` fails in
  `exe:use` with "Couldn't match expected type 'pkgb-0.1:b-y:M1.T' with actual
  type 'T' ... defined in package 'pkga-0.1'" (F3). Give `b-x` and `b-y`
  directories holding only their own module and the project builds, the
  executable prints `3` and `T 1`, and `readelf -d` on it shows
  `RUNPATH [/opt/fake-solver-dir]` inherited from `pkga`'s ld-options (F5).
- `pkgtoy/`: two Hooks packages sharing a local setup library through
  `setup-depends`, each receiving a generated hidden `Warmup` listing every
  exposed module of its direct dependencies; `toy-sem`'s Warmup contains
  `import CoreM ()` from the in-place `toy-core`; the facade's executable
  prints `2` (F6). The setup library is compiled at -O0 (see its .cabal).
