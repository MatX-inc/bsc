# TRS — handoff (rewritten 2026-07-09, start here)

Branch `claude/trs`, all work committed and pushed through
303e1182.  ALWAYS `git push personal` — NEVER bare `git push origin`
(origin is B-Lang-org; a stray push once created a public branch).
Standing OK to commit/push small commits on this branch.

Read alongside: `DESIGN.md` (architecture), `BIR.md` (export format),
`docs/TCL-CAPI.md` (the bluetcl/debug-mode contract — current),
`docs/VCD-CONTRACT.md`, `docs/PERF-BASELINE.md` (measured numbers).

## What this is

trs replaces Bluesim: bsc exports BIR (CBOR) via `-bir`; the Rust
side is an interpreter (the byte-exact ORACLE), a hybrid JIT, and an
AOT linker (`trs link` -> wrapper + .bir + design .so).  Two
products, like VCS:
- FAST artifact (default `trs link`): edge-SSA whole-edge fusion,
  export elision, O3.  No debug contract by design.
- DEBUG/interactive (`trs link --interactive`): a model .so that
  stock bluetcl `sim load`s as a drop-in Bluesim (bk_* C API), engine-
  multiplexed (interp / hybrid-jit / aot, one or several = oracle).

## Current state (all sealed)

- CORPUS: 975 PASS / 0 DIFF / 0 anything-ours (1037 designs; the 62
  non-PASS are all upstream: bsc COMPILE_FAIL 25, NO_SOURCE 20,
  NOT_SUPPORTED/BVI 14, bsc-side LINK_FAIL 3).  Sealed 8x on frozen
  binaries 2026-07-09 (latest: b8691ab4, compiled-path $finish).  ~825 of the 975 run COMPILED; ~120 interp
  fallback (MCD/Sync, module input ports, VCD tests by design,
  exotic prims).
- PERF: sudoku ~1.35x ahead (0.27s); LongCnt ~5x; corpus link ratio
  median 0.03.  Grid v3 (bench/grid, program tiles): link ahead of
  ref build at EVERY N after the replication-aware outline dial
  (N=32: 27.4s vs 150.1s); run ahead except N=32 where the deficit
  is O(instances) STARTUP (type-keyed analysis is the queued fix).
  bsc's own frontend is everyone's wall (511s at N=32).
- INTERACTIVE: battery 25/25 BYTE-IDENTICAL vs reference Bluesim
  (tests/interactive/run.sh mirrors testsuite/bsc.bluesim/
  interactive + local FinishPeek, bdpi, vcdtcl witnesses).  Async
  runs on the jit engine (capability tiers: peek tests pin
  engines=interp).  Model .so is 49MB after gc-sections/strip.
- PACKAGING (task #10): `make install` in src/trs builds+
  installs libtrs_capi.a next to the binary (jit iff
  LLVM_SYS_181_PREFIX set, so the top-level install-src stays lean-
  buildable).  BDPI companions travel with interactive models:
  link --interactive copies <model>.bdpi.so; bk_init dladdr-locates
  its own .so and loads the companion into every engine.
- COMPILE-WORKER JOIN (review backlog closed): short jit-engine
  interactive sessions segfaulted 5/5 at teardown — detached body-
  compile workers still executing model-.so code when bluetcl
  dlcloses it.  JitPlans now owns the JoinHandles; Drop sets
  lazy.stop (checked per batch) and joins.  8/8 clean after.
- VCD-UNDER-TCL (task #10 rung 1): bk_set_VCD_file/enable/disable
  wired to the interp writer (non-interp primaries degrade with a
  stderr note); bk_shutdown mirrors kernel vcd_reset via
  Interp::finish().  Two parity bugs found+fixed by measurement:
  value-method result ports declared width 1 in VCD (Expr::Def
  carries no width — resolve via def table; TbGCD result is 51-bit),
  and the yield-boundary flush (final stanzas of a stepped session
  never landed).  `sim vcd`, step/off/on, and -V run-to-$finish all
  byte-identical incl. VCD bytes (vcdtcl.cmd witness).
- $FINISH SEMANTICS (all engines): completes the in-flight edge
  schedule (kernel contract), suppresses the whole dollar_display.cxx
  output family post-finish, and the yield preempts the PG_FINAL
  after-edge pass.  COMPILED paths fixed to the same contract: the
  foreign callback no longer signals stop_bb for $finish/$stop
  (reserved for genuine aborts), the JIT dispatch walk and central
  player run the edge to completion, loops stop at the slice
  boundary.  Witnesses: FinishEdge (regress: suppression; vcd:
  boundary), FinishPeek (battery, jit engine: post-finish state
  writes peeked from the arena — discriminated the pre-fix binary,
  mark=0 vs 1000042).  MEASURED: the reference DROPS the finish
  instant's buffered VCD changes at shutdown (vcd.cxx flush_changes
  early-return at t==now), so post-finish writes never appear in any
  VCD — peeks are the only state witness.
- ULTRACODE REVIEW (7 finders, 72/72 verdicts upheld): 10 findings
  fixed+sealed (4e5df577), 9 queued below.

## Build / gate discipline (non-negotiable)

- bsc: ONLY `make -j32 GHCJOBS=16 install-src` from the repo root.
- Rust: `LLVM_SYS_181_PREFIX=/usr/lib/llvm-18 CARGO_TARGET_DIR=$M/cargo-fix
  cargo build --release -p trs --features jit` from src/trs.
  capi: `-p trs-capi` (default features = jit; --no-default-features
  = lean).  $M = the session scratchpad.
- SWEEP GATE for any compiler/interp-touching change: freeze the
  binary (`mkdir $M/frozen-<sha> && cp .../trs $M/frozen-<sha>/trs`
  — the file MUST be named trs), then
  `python3 tools/diffsweep.py --aot --trs $M/frozen-<sha>/trs
  --out $M/<name>.json` from src/trs with inst/bin on PATH.
  Expect 975/0.  LPT scheduling reads tools/sweep-costs.json.
  Perf fence flags = treat like DIFFs (ratios vs tools/perf-fence.json;
  rebaseline only on accepted equilibria).  NO other builds or heavy
  jobs during a sweep (timing noise -> false flags).
- Local ladders: tests/regress/run.sh (6), tests/vcd/run.sh (10),
  tests/interactive/run.sh (25; needs TRS_CAPI_LIB=<libtrs_capi.a>),
  plus sudoku + sysMips byte-parity from kept .bir (copy designs to a
  STABLE dir — sweeps rm -rf their work dirs).
- Traps: TRS_JIT env is is_none()-tested — ANY value (even 0)
  enables; unset to disable.  Wrapper scripts bake the absolute
  linker path but honor $TRS.  Bash tool cwd persists — cd to the
  crate root before heredoc patches and check the ok-sentinel.
  Another checkout (claude3/prim-fixes) sometimes runs testsuites on
  this box — check ps before trusting timing.

## NEXT UP (in order)

1. Finish task #10 (capi): ORACLE mode (secondary-engine quiet
   flag, lockstep compare at stops, AOT engine construction from the
   artifact pair, trs_* control entry points).  VCD-under-Tcl and
   packaging (capi lib install, BDPI companions; the battery is a
   standing ladder) DONE — see current state.
2. Review backlog (all confirmed, file:line in the 4e5df577 commit
   message): $stop-vs-$finish resume; multi-clock EN latch clearing;
   exporter round 2 = SimCOpt-surviving methodPorts set (replaces the
   const-ready RDY interim; same pattern as the def `sym` flag in
   SimExportIR.hs); symOrd char-wise compare; Fifo/RWire arena-
   attached peek staleness; central-loop negedge overcount; link
   feature-probe (nm the staticlib for LLVM refs); fence mode-
   awareness; add module.verify() in debug codegen builds.
   (prime()'s detached compile workers vs dlclose: FIXED — see
   COMPILE-WORKER JOIN above.)
3. Hygiene: sysCRCTest1's link fence flag now REPRODUCES on an idle
   box (0.40-0.45 vs baseline 0.22, b3_link ~1.3-1.5s) on BOTH the
   b8691ab4 and pre-fix binaries — binary-independent drift, so the
   0.22 baseline is stale; rebaseline it at the next accepted
   equilibrium (memq flagged once under sweep load, idle-clean;
   sysTrafficBRAM did not flag).  Full `make -j128 -C testsuite
   fullparallel` to re-certify zero-fail after the exporter changes
   (TEST_SYSTEMC_* env per global CLAUDE.md).
4. Scale arc: loop-rolled spine (planner run-detection over
   comp_nodes + affine base/token strides + counted-loop emission
   around the EXISTING outlined-body call ABI; bail unless provably
   affine; exec sites first, sched sections after) -> type-keyed
   analysis (startup) -> pools -> lanes.  Long-run grid measurement
   (1M cycles) still untaken.
5. PR #1027 rebase (task #7): onto personal/bluesim-fst (superset of
   -dump-formats).  merge-base 534241d5; conflicts concentrated in 9
   files/18 commits (flag tables, bsc.hs Verilog link path).  Needs
   quiet tree, full bsc rebuild, testsuite, fence re-baseline.
   -dump-formats is the link-time waveform contract the compile-mode
   split wants; libfst enables FST in the debug mode.

## Key implementation landmarks

- crates/trs-interp: lib.rs (Interp; advance_until/StopCond —
  edge_limits/at_times/abort/progress; symbol accessors; method-port
  peeks; $finish edge-completion; timescale), prim.rs (Prim trait +
  sym_children/sym_read tables mirroring bs_prim_mod_*.h), jit.rs
  (planner: cones/poison/outline dial with replication amortization/
  helper specs; AOT emit/load).
- crates/trs-codegen/lower.rs: edge-SSA driver, effectful-eval
  thunks + eager-first latch (full-range RegFile exempt, judged by
  ADDRESS width), AvAction module-child inline (AV widths from the
  RESULT — synthetic temps are in no def table), BDPI direct calls.
- crates/trs-capi: the bk_* surface (SimState/engines/Runner/Sym
  tree).  crates/trs/main.rs: link (+--interactive: incbin shim +
  -u keep-list + shared libLLVM + gc-sections/strip + bluesim.tcl
  wrapper).
- bsc side: SimExportIR.hs (encDef `sym` flag from post-SimCOpt
  publicDefs via bsc.hs symMap), SimCCBlock.hs exports isOkId.
- tools/diffsweep.py: --aot, --filter, --costs (LPT), fence, 420s
  ref-build ceiling (BRAM0Test/FloatTest measure 166-256s).

## The one paragraph that explains everything

Every decision routes through byte parity with reference Bluesim:
`cmp` across 1037 designs + 22 interactive sessions is the oracle,
the product promise, and the tie-breaker (evaluation counts, warning
order, $finish semantics, symbol tables — all "measured from the
reference source, mirrored exactly, gated forever").  When speed
fights fidelity, split the product (fast vs debug link modes), never
fuzz the oracle.
