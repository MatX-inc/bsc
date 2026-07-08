# TRS — session handoff

Branch: `claude/trs` (all work committed and pushed through
`eaae283b` — ALWAYS `git push personal`, never bare `git push origin`:
origin is the B-lang-org repo; a bare push once created a stray public
branch there, since deleted with Ravi's approval).  Read `DESIGN.md`
(goals/architecture), `BIR.md` (export format), `docs/VCD-CONTRACT.md`
(byte-level VCD semantics), and `docs/PERF-BASELINE.md` (measured
numbers) alongside this.

## Current state

- **Resumable stepper landed** (981ad24a): `run()` = `prime()` (one-time
  setup, idempotent) + `advance(max_cycles)` (event loop, resumable —
  the cycle-limit edge is pushed back on the heap) + `finish()` (final
  VCD flush, kept out of advance so bounded stepping can't corrupt VCD).
  `RComp`/`Stepper`/`REntry` at module scope; state is
  `Option<Stepper>` on Interp.  The -c driver's `sim step N` is true
  multi-step (cbee70c1), byte-identical vs the reference driver incl.
  VCD-across-steps and post-$finish errors ("cannot step"/"cannot run
  anymore", exit 1).  The JIT harness (run-to-cycle, compare, continue)
  sits directly on `advance()`.
- **Eager schedule-position defs** (8bc49016): each REntry carries the
  CF/WF cone defs (getExprIds-True closure, first-Sched-node-attached)
  and the edge pass evaluates them into the latch before the entry's
  nodes — mirroring the C++ schedule_posedge def-statement lists.  This
  fixed the last known real bug (sysMips missing 10 RegFile warnings)
  and made the interp ~16% FASTER (shared cone defs compute once per
  edge, not per rule).  CF/WF defs (rule or method, via DefProps) are
  traversed but never latched — method WILL_FIREs follow the call-time
  EN protocol (pre-latching them made sub-module rules fire during
  their parent's method call: mkVCDTest1_Sub regression, fixed).
- Segment pre-resolution: entries hold resolved node slices + tick port
  names; the per-edge domain search / nodes.clone() tax is gone.
- RegFile/BRAM bounds warnings render addresses like dump_val
  (0x-prefix, width-padded; 3509045e).
- **Verification**: diffsweep 556 PASS / 0 DIFF (up from 554: the
  $sformat fix was never in the old release binary — see gotchas — and
  sysStringFormat2 + one more now pass).  VCD battery 9/9.  Final full
  suite with all fixes: 9533 PASS / 53 FAIL / 68 XFAIL / 0 XPASS, where
  the 53 = 44 bsc.bluesim/interactive (Tcl surface, task #20) + 6
  sudoku/MPEG4 (killed mid-run; those tests are long-test-gated now and
  won't run at all — see below) + 3 bsc.bsv_examples/wallace
  (testCombServer: DejaGnu's per-test timeout under -j128 load; 24/0
  when the dir runs solo).  So the real residue is interactive-only.
- **Slow tests are long-test-gated** (44c2a1e9): sudoku's
  mkGenerateTest3 moved behind the bsc.long_tests enable mechanism
  (sudoku.exp.golden, linked by `make -C bsc.long_tests sudoku`), and
  the stale enabler links for MPEG4/conflict_free_large/log2_loop were
  removed — an earlier `enablelongtests` had left them on, which is why
  every trs suite run paid two 15-minute kills.  fullparallel's
  enablelongtests still enables everything, sudoku included.
- The "6-minute watchdog" in earlier handoffs was actually DejaGnu's
  own per-test timeout: slow interpreter sims (~30s solo) exceed it
  under -j128 contention.  wallace/testCombServer is the borderline
  case and can flake in full parallel runs until the JIT lands; it
  passes solo.
- Testsuite guards batch 3 (61b0b3d6): vlink_regen, gen_mode, options,
  splitports, derived_bits, inout, tasks, bh_pragmas, higherrank,
  instances + BRAM's bug-1731 xfail scoped to cxx_codegen_tests (under
  -trs the WidthTest link must and does succeed).  All 11 dirs verified
  0 FAIL under both -sim (VTEST=1) and -trs (VTEST=0) from clean dirs.
- diffsweep.py has `--trs <binary>` (54f7e089) — sweep a scratch
  build without touching target/release.  The override travels via env
  because Python 3.14 pool workers re-import the module.

## Open items

- Tcl surface: 44 bsc.bluesim/interactive tests need the bk_* compat
  .so (DESIGN.md §7; task #20 approved by Ravi).  The stepper refactor
  it needed is DONE.  Plan: trs-capi cdylib exporting the 46 bk_* +
  new_MODEL_<top> symbols BluesimLoader.hs dlsyms -> driver thread for
  async run -> symbol table incrementally.  trsLink should emit a tiny
  per-design shim .so linking the shipped runtime; spike
  dlsym-through-dependency early.
- Latent VCD parity classes (battery green, matter for byte-parity
  goals): (a) clock-alias vars (dut.CLK vs dut.CLK_c1 map to one kernel
  clock id where reference keeps them distinct); (b) never-computed
  defs at the initial dump — C++ dumps the zeroed member, trs dumps
  the write_undet pattern (sysMips VCD has 48 such lines vs reference);
  (c) SyncFIFO + RegAligned VCD hooks still TODO (silent-default class).
- wallace/testCombServer can flake under full -j128 suite load
  (DejaGnu per-test timeout on a ~30s-solo interpreter run); goes away
  with the JIT.

## Next phase: performance (task #19)

LLVM toolchain is READY: llvm-18-dev + libzstd-dev + libpolly-18-dev
installed; `LLVM_SYS_181_PREFIX=/usr/lib/llvm-18 cargo build -p
trs-codegen --features llvm` builds and its JIT smoke test passes.
START: LLVM lowering per DESIGN.md behind the `llvm` feature, rule
bodies and CF/WF cones as native code, prims as calls into trs-rt;
the interpreter is the differential oracle (sweep + battery as the
net); the harness = `prime()` / `advance(to_cycle)` / compare /
continue on both engines.  Note the eager per-entry def lists in
REntry are exactly the def-statement lists the JIT should compile per
schedule position.  Baseline (quiet machine): 5M-cycle counter
reference 0.27s vs interp ~35s post-refactor (~130x); sudoku-class
cones >1600x — the JIT is the fix.

## Cardinal rules / gotchas

- Never rebuild `inst/bin/bsc` or `target/release/trs` while a sweep,
  battery, or testsuite run is using them.  Develop against
  `CARGO_TARGET_DIR=<scratch>/cargo-alt cargo build`; sweep scratch
  builds with `diffsweep.py --trs`.
- **Stale-binary trap (bit us this session)**: target/release/trs was
  built at 04:25 but the $sformat fix landed at 04:36 — the whole
  first suite run and sweep #1 tested a stale binary (one phantom DIFF,
  one phantom suite FAIL).  After committing interp changes, rebuild
  target/release BEFORE judging any run.  Check `ls -la` mtime vs
  `git log` when results look off.
- **Watchdogs must match argv[0] exactly** (`awk '$3 == "<abs path>"'`):
  a substring match on the command line kills the suite launcher itself
  (its env assignment contains the binary path).  Known-slow tests
  legitimately exceed 15 min under load.
- Suite runs: `env CONFIG_SHELL=/bin/sh SIM_BACKEND_FLAG=-trs
  TRS=<repo>/src/trs/target/release/trs VTEST=0 SYSTEMCTEST=0
  PATH=<repo>/inst/bin:$PATH make -j128 INIT=bsc checkparallel` from
  testsuite/ (parallel equivalent of the old serial `check`; ~16 min +
  slow-test tail).  Aggregate per-dir testrun.sum for progress; the
  per-dir counts only settle once make clean has swept old sums.
  Verifying single dirs: `make -C <dir> clean` first or stale .bo/.ba
  suppress warnings the .exp expects (phantom FAILs).
- Reference executables are scripts needing `bluetcl` on PATH; beware
  the OTHER bsc checkout (~/bluespec/bsc) — always prepend this repo's
  inst/bin, and remember exported PATH does NOT persist across shell
  invocations.
- BIR files are re-exported by the *installed* bsc; stale .bir/.ba need
  regeneration after exporter changes.
- Commit policy: small commits, push to `personal` (nanavati/bsc)
  freely (Ravi's standing OK); trailers per session convention.
