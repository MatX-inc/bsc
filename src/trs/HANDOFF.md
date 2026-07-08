# TRS — session handoff

Branch: `claude/trs` (all work committed and pushed through
`fad21d8d`).  Read `DESIGN.md` (goals/architecture), `BIR.md` (export
format), `docs/VCD-CONTRACT.md` (byte-level VCD semantics), and
`docs/PERF-BASELINE.md` (measured numbers) alongside this.

## Current state

- **Full testsuite** (`env CONFIG_SHELL=/bin/sh SIM_BACKEND_FLAG=-trs
  TRS=<repo>/src/trs/target/release/trs make VTEST=0
  SYSTEMCTEST=0 check` from testsuite/): last complete run was
  6971 PASS / 128 FAIL / 23 XFAIL, and every FAIL was triaged.  Since
  that run, fixes landed for all real bugs but ONE (below); the other
  FAILs were: 47 Tcl-surface tests (44 bsc.bluesim/interactive + 3
  scattered `-c` tests — now partially covered by the driver's -c/-f
  subset), ~25 spurious test-script bugs (unguarded VTEST=0 compares —
  all guarded now), and 6 slow-interpreter watchdog artifacts (sudoku
  generator, MPEG4: legitimate ~7-minute interpreter runs killed at 6
  min; the P2 JIT is the real fix).  A fresh full run should be near
  zero unexpected failures modulo those categories.
- **Differential sweep** (`python3 tools/diffsweep.py`, ~40 min):
  554 PASS / 0 DIFF / 0 panics.  VCD battery (`tests/vcd/run.sh`): 9/9.
- The install now packages trs: `make install-src` builds the release
  runtime via `src/trs/Makefile` into `inst/bin/trs`.

## Fixed this session (each verified byte-exact vs reference)

- RegAligned primitive; phantom prim-domain clocks resolve to Never
  (SpecialSyncReg/SpecialSyncFIFO).
- GatedClock: cross-domain setGateCond while the input clock is low
  propagates through the transparent latch immediately, via a new
  `Prim::clock_level` pre-rules hook (mcd_Rand).
- Native library BDPI rand32/srand via glibc random/srandom
  (bsc.lib/Divide, SquareRoot); library names excluded from .bdpi.so
  eager resolution.
- Driver `-c`/`-f` scripting subset: one `sim run`/`sim step N` plus
  `sim time` / `sim clock` (getClockInfo tuple) / `puts [...]`
  (2x2-switch, sysEmptyModule, bsc.if b*).
- **Per-node schedule segments**: compositions now reproduce bsc's flat
  merged order exactly (getput sysTestUGFIFOF CF-order divergence; also
  kills the SRAMFile cyclic-segment class).  touchingRules/meRules
  analysis deleted.
- always_enabled check_rdy: BIR Method carries the pragma; the interp
  gates the body on the sibling RDY_<m> method (rdy_en_pragmas x3,
  sysTestBypassWire).
- $swrite/$sformat use the full format engine (every string arg is a
  format; sysFormat4/5).
- Divide-by-zero raises SIGFPE like native division (bsc.misc/divmod).
- GatedClockDiv = ClockDivider with a gated input clock (ClockDividers).
- ifc_clock_gates: exported per interface clock; Expr::Gate on a user
  child chases the child's gate expr recursively (Bug-1677 family).
- Testsuite: many VTEST=0 guards (arrays, b381, b1490, Cntrs,
  CompletionBuffer, NullCrossing, SShow, primtcons, prims/name,
  NoClock, log2_loop.golden, misc ccomp compares).

## THE ONE REMAINING REAL BUG (finish this first)

`bsc.mcd/Gating`: 6 designs (GatedClock_TwoModTwoSyn, SubMethod,
SubRule, MethodTb, RuleTb, MethodTb2) share an identical residual
diff — a ONE-EDGE-LATE gate window (`18,30c18,30`, rg2 = b0 vs af over
t=180..300, re-converging after).  Repro: copy bsc.mcd/Gating/*.bsv to
scratch, build each with `bsc -u -trs -g sys<T> <T>.bsv` (+ `-g
mkGatedClock_TwoModTwoSyn_Sub` for TwoModTwoSyn), diff against
sysGatedClock_OneMod.out.expected.  Evidence so far (SubMethod, via
keep-fires VCD diff of ref -sim vs -trs): g1.new_gate matches
everywhere; the divergence is `s.sg.ssg.new_gate` — reference holds it
0 for the whole g1-off window (165..295), trs shows one extra toggle
pair at 165/175 and resumes late (315 vs 295).  Key C++ semantics not
yet replicated: MOD_GatedClock's latch output is
`PORT_CLK_GATE_OUT = clk_in_gate & reg` — the INPUT clock's gate
participates, so when the outer gate (g1) is off, the inner gate (ssg,
whose clk_in is the g1-gated clock) must go 0 regardless of its own
cond register, and rules gated by ssg must stop.  Check (a) what gate
expr the ssg prim's clk_in tick receives in the BIR composition (it
must be g1's gate, not constant true) on BOTH edges, (b) whether the
rule toggling ssg's cond (top rule r1, itself g1-gated through TWO
boundaries) fires one edge too long — i.e. whether the Expr::Gate
conjunct is evaluated at Sched (latch) time vs the C++'s port read.
The vcdcmp.py comparator (name-keyed VCD diff) from this session is a
10-minute rewrite if needed: parse $scope/$var, replay changes keyed
by hierarchical name, report first divergence excluding `____d\d+$`
defs and CLK* aliases.

## Also open (not blocking the perf shift)

- Tcl surface: 44 bsc.bluesim/interactive tests need the bk_* compat
  .so (DESIGN.md §7; task #20 approved by Ravi).  Plan: resumable
  stepper refactor (shared with JIT) -> trs-capi cdylib exporting the
  46 bk_* + new_MODEL_<top> symbols BluesimLoader.hs dlsyms -> driver
  thread for async run -> symbol table incrementally.  trsLink should
  emit a tiny per-design shim .so (it already builds .bdpi.so) linking
  the shipped runtime; spike dlsym-through-dependency early.
- VCD clock-alias parity: trs maps a module's input-clock alias vars
  (dut.CLK vs dut.CLK_c1) to one kernel-clock id where reference keeps
  them distinct; latent (battery green), matters for byte-parity goals.
- SyncFIFO + RegAligned VCD hooks still TODO (silent-default class).
- Watchdogged slow tests (sudoku mkGenerateTest3, MPEG4): expected
  until the JIT lands.
- Stray tracked artifacts to clean up (ask Ravi): testsuite
  a.out.bir files (gcd, fifo, rwire, mcd/Misc), root dump.txt and
  sysGatedClock_OneMod.bir — run artifacts accidentally in git.

## Next phase: performance (task #19)

Correctness ledger is clean modulo the one Gating bug.  Start with the
resumable-stepper refactor (event heap + resolved comps as Interp
fields; the trs-kernel crate's Yield/Quit scaffolding anticipates
it), then LLVM lowering per DESIGN.md behind the `llvm` feature
(needs llvm-18-dev), interpreter as differential oracle, sweep +
battery as the net.  Baseline: ~335x slower than compiled Bluesim on
tight loops, >1600x on sudoku; link already 11-20x faster than -sim.
Per-node segments made compositions per-rule-node — the entries loop
clones each 1-node segment per edge; fold that into the stepper
refactor (pre-resolve entries to node slices once).

## Cardinal rules / gotchas (unchanged)

- Never rebuild `inst/bin/bsc` or `target/release/trs` while a
  sweep, battery, or testsuite run is using them.  Develop against
  `CARGO_TARGET_DIR=<scratch>/cargo-alt cargo build` (debug).
- Watchdog-kill wedged `trs run` processes during suite runs — but
  known-slow tests (sudoku, MPEG4) legitimately exceed 6 minutes.
- Reference executables are scripts needing `bluetcl` on PATH
  (`PATH=<repo>/inst/bin:$PATH`); beware the OTHER bsc checkout on
  PATH (~/bluespec/bsc) — always prepend this repo's inst/bin.
- BIR files are re-exported by the *installed* bsc; stale .bir/.ba
  need regeneration after exporter changes.
- Commit policy: small commits, push to `personal claude/trs`
  freely (Ravi's standing OK); trailers per session convention.
