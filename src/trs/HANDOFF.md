# TRS — handoff (rewritten 2026-07-09, start here)

Branch `claude/trs`, all work committed and pushed through
c01db5cd.  ALWAYS `git push personal` — NEVER bare `git push origin`
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
  NOT_SUPPORTED/BVI 14, bsc-side LINK_FAIL 3).  Sealed 14x on frozen
  binaries 2026-07-09 (latest: c01db5cd, fleet round-2 fixes;
  the only fence flag each time is the dispositioned sysCRCTest1
  stale baseline, 0.39-0.45 band).  ~825 of the 975 run COMPILED;
  ~120 interp fallback (MCD/Sync, module input ports, VCD tests by
  design, exotic prims).
- PERF: sudoku ~1.35x ahead (0.27s); LongCnt ~5x; corpus link ratio
  median 0.03.  Grid v3 (bench/grid, program tiles): link ahead of
  ref build at EVERY N after the replication-aware outline dial
  (N=32: 27.4s vs 150.1s); run ahead except N=32 where the deficit
  is O(instances) STARTUP (type-keyed analysis is the queued fix).
  bsc's own frontend is everyone's wall (511s at N=32).
- INTERACTIVE: battery 29/29 BYTE-IDENTICAL vs reference Bluesim
  (tests/interactive/run.sh mirrors testsuite/bsc.bluesim/
  interactive + local FinishPeek, bdpi, oracle, oracleaot,
  finishpeekaot, oracleprims, vcdtcl witnesses).
  Async runs on the jit engine (capability tiers: peek tests pin
  engines=interp).  Model .so is 49MB after gc-sections/strip.
- PACKAGING (task #10): `make install` in src/trs builds+
  installs libtrs_capi.a next to the binary (jit iff
  LLVM_SYS_181_PREFIX set, so the top-level install-src stays lean-
  buildable).  BDPI companions travel with interactive models:
  link --interactive copies <model>.bdpi.so; bk_init dladdr-locates
  its own .so and loads the companion into every engine.
- ORACLE MODE (task #10 rung 2): secondary engines run QUIET —
  console/file/VCD sinks suppressed ($fopen(w) allocates an FSlot::
  Sink so design-visible fd keys match; read-mode opens stay real;
  $fatal still latches state), lockstep-compared against the primary
  at every stop (time, per-clock edge counts, finished) — divergence
  reports on stderr and flips the fatal flag.  bk_abort_now is now
  SLICE-ALIGNED (the code allowed mid-instant stops; the contract
  and the oracle catch-up need whole slices), and async secondaries
  catch up to the primary's stop via at_times=[primary.now] (edge-
  count targets are wrong: the slice-end check fires on ANY reached
  limit).  Probes: gcd/vcdtcl/bdpi dual-engine byte-identical;
  async stop+resume 5/5 divergence-free.  Witness: oracle.cmd
  (battery, engines=interp,jit).  STILL QUEUED from the oracle list:
  a deterministic ASYNC battery witness needs a tunable-wall design.
- TRS_* NAMESPACE (task #10, last rung): trs_engine_count /
  trs_engine_kind / trs_oracle_check (on-demand lockstep+state
  checkpoint) live beside the frozen bk_* surface (the export map
  already whitelisted the prefix).  Witness: capi_witness.c — the
  battery's first DIRECT C-API test (dlopens the model, no bluetcl;
  line-buffered C stdio so lines interleave with Rust's design
  output deterministically).  trs_advance-with-rich-StopCond is
  deferred until a consumer exists (speculative ABI otherwise).
- STATE COMPARE (oracle, final rung): at every stop the compare now
  also walks ARCHITECTURAL STATE — every prim sub-symbol, scalars
  and range entries, read per engine via prim_sym_read[_range]
  (engines share inst indexing; no Sym tree needed, so the async
  worker can run it).  First probe caught BOTH halves of the queued
  "Fifo/RWire arena-attached peek staleness" item: (1) FIFO sym
  reads ignored the arena mirror — compiled engines answered the
  0xAA init pattern forever; sym_read_range now refresh()es from
  the arena first (FIXED, closes the fifo half).  (2) RWire stop-
  time values are CLEAR-PLACEMENT artifacts (compiled clears at
  edge end, reference at next-edge top) — wires are edge-transient,
  not architectural state: Prim::sym_transient() excludes them from
  the compare.  Residue: pure jit/aot-tier wire PEEKS still answer
  the cleared value where the reference shows the held one — the
  doctrine answer is NoValue degradation, queued.  Witness:
  oracleprims.cmd (fifo state through the mirror, interp,aot).
  TRAP for future probes: `grep -cE` exits 1 on zero matches — a
  `build | grep -c error && link && run` chain SKIPS the relink,
  and you measure a stale model (two false observations today).
- AOT ENGINE (task #10 rung 3): `trs link --interactive` now ALSO
  emits the fast-artifact design .so as <base>.aot.so beside the
  model (ineligible designs: note + interp/jit, like plain link);
  bk_init's Aot kind dladdr-locates it and aot_request_code's it
  (bir_hash + layout checks at prime; stale/missing -> stderr note
  + in-process fallback).  The FLAGSHIP config engines=interp,aot
  works: full introspection on the interp primary, quiet aot
  secondary lockstep-checked.  Probes byte-identical: finishpeek on
  pure aot (register peeks from the warm arena + $finish edge
  completion on the aot tier) and gcd.cmd on interp,aot.  Witnesses:
  oracleaot.cmd, finishpeekaot.cmd.
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
- ULTRACODE REVIEW round 1 (7 finders, 72/72 verdicts upheld): 10
  findings fixed+sealed (4e5df577).  ROUND 2 (2026-07-09 night, 109
  agents over the day's 6 increments): 29 confirmed / 5 rejected;
  10 mechanical fixes landed same-night (ConfigReg sym_read refresh;
  lean-build link hard-fail -> Ineligible stub; central-loop $finish
  negedge credit — closes the old backlog item; per-engine oracle
  shape gate; worker batch cap 8 = bounded teardown join;
  mark_fatal latches finished; missing-.bdpi.so fails bk_init loudly
  + shared-globals note for multi-engine BDPI; interruptible async
  catch-up (Runner.catch_abort, compare skipped w/ note if
  incomplete); sync-path secondaries bounded by the primary's stop
  (diverged-secondary hang); JIT_SYNC-pinned finishpeek witness +
  stripped installed staticlib).  STRUCTURAL findings queued below.

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
  tests/interactive/run.sh (29; needs TRS_CAPI_LIB=<libtrs_capi.a>),
  plus sudoku + sysMips byte-parity from kept .bir (copy designs to a
  STABLE dir — sweeps rm -rf their work dirs).
- Traps: TRS_JIT env is is_none()-tested — ANY value (even 0)
  enables; unset to disable.  Wrapper scripts bake the absolute
  linker path but honor $TRS.  Bash tool cwd persists — cd to the
  crate root before heredoc patches and check the ok-sentinel.
  Another checkout (claude3/prim-fixes) sometimes runs testsuites on
  this box — check ps before trusting timing.

## NEXT UP (in order)

1. Finish task #10 (capi): architectural-state lockstep compare
   (per-engine symbol peeks); trs_* control entry points.  AOT
   engine construction, quiet flag + time/edge/finish lockstep,
   VCD-under-Tcl, and packaging DONE — see current state.
2. Review backlog (all confirmed, file:line in the 4e5df577 commit
   message): $stop-vs-$finish resume; multi-clock EN latch clearing;
   exporter round 2 = SimCOpt-surviving methodPorts set (replaces the
   const-ready RDY interim; same pattern as the def `sym` flag in
   SimExportIR.hs); symOrd char-wise compare; link feature-probe
   (nm the staticlib for LLVM refs); fence mode-awareness; add
   module.verify() in debug codegen builds; compiled-tier wire
   PEEKS answer the cleared value (NoValue degradation per doctrine).
   FROM FLEET ROUND 2 (failure scenarios in docs/review-round2.json): oracle engines share process-global BDPI/libc-RNG state
   (real fix = per-engine RNG reproducing glibc random(), dlmopen
   namespaces for user BDPI; the bk_init note is the interim);
   $finish may skip same-instant OTHER-CLOCK PG_LOGIC edges the
   reference still runs (needs an MCD witness measured vs
   reference); quiet flag misses prim-level println! (fifo guard
   warnings), $fgetc double-consumes shared stdin, and format
   'Output error:' lines leak from quiet engines (thread-local
   quiet, like FROM_COMPILED, is the likely shape); state compare
   blind to CReg/BRAM/DualPortRam/synchronizers (no sym_children);
   VCD-under-Tcl dead on the shipped default jit engine and bluetcl
   can't see bk_enable's 0 (auto-downgrade at -V or a loader note);
   quiet $fopen(w) Sink cannot mirror a primary open FAILURE (fd
   key skew — document or probe-open); async catch-up on a slow
   secondary still blocks bk_sync for its serial replay (lockstep
   slice advancing is the real fix).
   (prime()'s compile workers vs dlclose: FIXED; Fifo/RWire peek
   staleness: FIXED; central-loop negedge overcount: FIXED.)
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
