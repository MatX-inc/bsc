# P0 rebuild benchmark harness

Implements delivery-plan P0 ("Pin and measure", doc/engine-first-plan.md):
a reproducible benchmark command over the scenarios the plan names, with
wall time, CPU time, peak memory, GHC recompilation counts and package
compilation counts recorded per stage, plus a toolchain/environment
inventory so another developer can reproduce the numbers.

This harness only ever *appends and removes marker comment lines* to make
edits; it never changes program text, and it restores every file it
touched (verified against git) before exiting.

## Usage

    util/rebuild-bench/bench.sh [scenario ...]

Run from anywhere inside the repository; results land in
`bench-results/<timestamp>/`:

  - `inventory.txt`  - toolchain, environment, git revision (plan P0 item 4)
  - `results.tsv`    - one row per (scenario, stage) with the counters
  - `log/`           - full stdout/stderr of every stage command

With no arguments, all scenarios run in the order below. The make build
is untouched and remains the comparison oracle; this harness drives the
cabal build (`cabal build`) and the library build (`make -C src/Libraries`)
with the same commands a developer types.

## Scenarios (plan P0 item 3)

  cold            - full build from a clean tree (cabal clean + library clean)
  null            - unchanged incremental rebuild
  clean-outputs   - library outputs deleted, compiler unchanged
                    (the P2 restore target measures against this)
  leaf-edit       - marker edit to src/comp/app/vcdcheck.hs
  frontend-edit   - marker edit to src/comp/TCheck.hs
  evaluator-edit  - marker edit to src/comp/IExpand.hs
  scheduler-edit  - marker edit to src/comp/ASchedule.hs
  verilog-edit    - marker edit to src/comp/AVerilog.hs
  bluesim-edit    - marker edit to src/comp/SimCCBlock.hs
  library-edit    - marker edit to src/Libraries/Base1/ListN.bs

## Stages measured per scenario

  compiler   - cabal build (all components)
  bsc-only   - cabal build bsc:exe:bsc   (the iteration-loop candidate)
  libraries  - make -C src/Libraries build
  install    - make -C src/Libraries install

## Counters

  wall_s      - wall clock (date +%s.%N)
  cpu_s       - user+sys from /usr/bin/time
  max_rss_kb  - peak resident set from /usr/bin/time
  ghc_mods    - GHC "[ N of M] Compiling" lines observed
  bsc_pkgs    - library packages compiled (checking/compiled lines + .bo mtimes)

Numbers from this harness are MEASURED; keep them labeled as such and
separate from the modeled testsuite economics (plan, Fixed decisions).
