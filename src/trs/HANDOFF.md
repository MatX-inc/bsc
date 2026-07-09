# TRS — session handoff

Branch: `claude/trs` (all work committed and pushed through
`7fd231db` — ALWAYS `git push personal`, never bare `git push origin`:
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

## Performance (task #19): hybrid JIT v1 LANDED

The P2 slice is in (2b4d2d1f/f105f91d/b1b26e8d): eligible rules run as
LLVM-compiled Sched/Exec functions inside the interpreter's event loop
over a shared u64 arena (plain ≤64-bit sync regs, reset levels, CF/WF,
eager defs).  $display-family statements call back into the
interpreter with their condition FORCED TRUE (the compiled branch
already decided it — re-evaluating after a same-body register store
skipped $finish; 16 sweep DIFFs taught us that).  v1 is all-or-nothing
per design; anything ineligible (wide values, non-reg prims, dynamic
extract, AV, Quot/Rem, async-reset regs, method schedule nodes,
crossing rules, VCD) falls back to the interpreter.

Build + run: `LLVM_SYS_181_PREFIX=/usr/lib/llvm-18 cargo build
--release -p trs --features jit`, then `TRS_JIT=1 trs run ...`
(TRS_JIT_TRACE=1 says on/off and why; TRS_JIT_DUMP=1 prints IR).
The default build has no LLVM dependency; the installed trs is
interp-only unless built with the feature.

Verified on the EXTENDED sweep corpus (1037 designs — diffsweep now
covers mk* tops and .bs sources; the old sys*/.bsv-only sweep was why
the suite kept catching bugs the sweep couldn't see): interp baseline
968 PASS / 0 DIFF; with TRS_JIT=1 967 PASS / 0 DIFF / 1 TIMEOUT —
the timeout IS the acceptance marker: sudoku mkGenerateTest3 under the
5s long-test leash (enable-gated dirs get max(5s, 5x reference wall);
normal tests keep the flat 60s).  It falls back to the interpreter
until method calls + RWire compile; when they land, that line flips
to PASS.  Battery 9/9 both modes.  Exact-width iN values landed
(24fa8727): no 64-bit cap, wide regs are multi-slot, Quot/Rem SIGFPE.
sysLongCnt 5M cycles: 0.50s vs 35.7s interp (~70x), vs 0.27s compiled
C++ (~1.9x).  Compile cost ~10ms/small design.

SUDOKU UNLOCKED (22299140): cross-module method inlining (per-instance
InstEnvs, fresh child frames, EN-slot enable protocol), the generic
prim TRAMPOLINE (any prim method — FIFO/ConfigReg/RegFile/BRAM —
compiles as a tabled callback into the interpreter's boxed prim),
native $display-arg marshaling (no more callback re-evaluation rules),
$time-class tasks, RWire arena backing, dynamic extracts (runtime-hi
masking — the sweep caught the first cut ignoring hi), and lazy
If/Case control flow (select would fire untaken-arm prim side
effects).  mkGenerateTest3: byte-identical, 15s wall (~13s JIT
compile, ~2s sim vs 0.36s reference) — the interpreter never finished
it.  Extended sweep with TRS_JIT=1: 965 PASS / 0 DIFF; the 2
TIMEOUTs (sudoku, conflict_free_large) are COMPILE TIME vs the 5s
long-test leash, which is the next target.

COMPILE TIME 5x FASTER (71a07aa4): parallel rule-batch compilation
(per-thread contexts, Once-guarded target init — the per-call init
races), -O0 default (TRS_JIT_OPT raises), owner-ordered eager-slot
cone sharing (sched fns load slots stored by earlier entries; inlined
callee frames must RECOMPUTE — the first cut let them load/store
callee slots whose owners hadn't run, corrupting sudoku).  Sudoku:
compile 17.7s -> 3.55s, full run ~3.9s byte-identical.  Sweep: 966
PASS / 0 DIFF; conflict_free_large TIMEOUT->PASS; sudoku fits the 5s
leash solo but not under 8-way sweep contention.

SCHED-EAGER / BODY-LAZY (Ravi's split, current): scheduling functions
compile eagerly and blocking (they run on EVERY edge, and cone sharing
makes them tiny — 4% of sudoku's IR, 108ms), while rule BODIES stream
in on background workers filling per-rule OnceLock cells; an Exec node
whose cell is still cold reads its WF straight from the arena slot the
native sched just wrote and interprets the body (exec_rule_forced).
Key pieces: trial_lower() decides eligibility synchronously by
lowering into a throwaway context (no engine work); token = ordinal<<17
| TOKEN_KIND_EXEC | local so callbacks resolve call-site specs through
the shared LazyJit; the interpreter's Def evaluation falls through to
arena slots (jit_eager_slots) so interpreted bodies see the fire
signals/eager defs the scheds computed — do NOT bridge slot values
into the latch instead: slots evolve during the firing as later scheds
run, and the resulting staleness is compile-race-dependent (same
binary alternated pass/fail); interpreted method calls write EN slots
through (jit_en_slots); the driver _exit()s after flushing so atexit
doesn't stall on in-flight LLVM workers.  TRS_JIT_SYNC=1 restores
blocking body compiles for measurement.  Sudoku: -m 1 startup 0.48s
(trial-lower dominated), -m 4000 1.80s, full run 5.8s byte-identical
across repeated runs; LongCnt 0.51s.  Battery 9/9.  Sweep: 967 PASS
/ 0 DIFF / 0 TIMEOUT — sudoku's long-test-leash marker flipped to
PASS (the -m 4000 probe runs in 1.8s), so the whole extended corpus
is green under TRS_JIT=1.

AOT PERSISTENT ARTIFACT (Ravi's call: match how Verilator/VCS/Bluesim
amortize compiles — and make our compile directly comparable to
theirs).  `trs link <top>.bir [-o <out>]` compiles everything at
build time and emits <out> (wrapper script, same CLI as reference
.cexe — needs trs on PATH like reference needs bluetcl), <out>.bir,
and <out>.so (PIC objects, cc -shared).  `trs run --code <so>`
resolves every rule's sched_/exec_ function from the artifact instead
of compiling; the wrapper passes it automatically.  Key mechanics:
callbacks are POINTER-GLOBALS (trs_cb_foreign/sigfpe/prim) defined
once in a meta object and filled by the loader after dlopen — chunk
objects only declare them (defining in every chunk = duplicate-symbol
ld failure), and the JIT path bakes the addresses as constants
instead (no more add_global_mapping).  Slot allocation had to become
DETERMINISTIC across processes (link bakes slot numbers, load
re-derives them): two HashMap-iteration loops (children reg/wire
slots, EN ports) now iterate sorted — proven by hashing full IR dumps
from separate processes (identical after masking ASLR'd baked
callback addresses).  The load path re-runs the plan walk +
trial_lower (~0.35s on sudoku) to rebuild call-site spec tables; the
artifact carries trs_bir_hash (FNV-1a of the .bir) and
trs_layout_rev globals, checked at load — any mismatch warns and
falls back to in-process compilation (verified: swapped .so runs
correctly via fallback).  INELIGIBLE designs still link: the artifact
just omits --code and runs interpreted (reference Bluesim always
yields an executable; only infra failures — LLVM/cc/IO — fail the
link).  BDPI: link copies the <in>.bdpi.so sibling to
<out>.bdpi.so — the artifact renames the .bir, which silently broke
the sibling-lookup convention (7 sweep panics taught us).  diffsweep
--aot sweeps the whole corpus through link+artifact.  Sudoku numbers:
link 4.7s TOTAL vs reference C++ link 13.94s (~3x, at -O0, before
body splitting); artifact runs 1.9-2.2s byte-identical (was 5.8s
streaming JIT), -m 4000 probe 0.49s.  Coverage: 697/966 designs
compile; the first AOT sweep's ineligibility histogram is the JIT
coverage roadmap — "def inside conditional arm" 146 designs,
"expression kind not compilable" 56, zero-width 20, avaction 8.
Follow-ups: serialize FnProtos into the artifact to skip trial_lower
at load (goal <0.1s startup); artifacts bake host CPU features (like
-march=native) — a generic-arch knob if artifacts should move
between machines.

PRIM FAST PATHS TIER 1 (07c13ed2): TRS_PROF=1 profiling
(dispatch/ticks/trampoline split + per-method histogram) showed 67%
of sudoku's 3.97M trampoline calls were ConfigReg reads.  ConfigReg
reads and FIFO value methods (notFull/notEmpty/first/i_notFull/
i_notEmpty) now compile inline over mirrored arena state; a global
now-slot (stamped per edge) reproduces the interpreter's
begin-of-instant rules exactly (written_at/saved_elems selection —
do NOT commit on tick: same-instant cross-clock reads would see the
new value).  Actions (write/enq/deq/clear) stay on the trampoline
and mirror.  AOT_LAYOUT_REV=2.  Sudoku artifact 1.93s -> 1.46s;
trampoline 0.59s -> 0.08s.  Remaining gap to reference (0.36s) is
-O0 jitted code: dispatch 0.89s, startup (trial-lower) 0.32s.
Measured on the IR dump: exec_i69_155 == exec_i92_208 byte-identical
after constant normalization (module-type dedup = exact 2x), sibling
tactics 155 vs 156 overlap 96% (stable-def sharing needs the
intervening-write analysis — the sched cone-sharing corruption is
the cautionary tale).  Tier B2 (inline enq/deq/write fast paths with
trampoline warning slow-path) parked as cold-path polish.

PER-MODULE-TYPE EXEC DEDUP (981a6be2): DFS subtree-contiguous arena
regions with type-canonical layouts (eager slots allocate as a
per-instance sorted UNION — schedule attachment splits them
differently between twins); exec fns take (arena, env, region base,
token base) with in-region slots base-relative and runtime tokens;
rules group by recursive subtree signature and compile ONCE per
class (per-ordinal call-site tables come from trial protos).  Twin
IR proven raw-identical before grouping.  Sudoku: 276 bodies -> 221
classes, .so 2.94 -> 1.59MB, link 4.70 -> 3.26s; N-replicated
designs dedup N-fold.  AOT_LAYOUT_REV=3.  The signature MUST cover
every input the exec lowering reads — extend it when the lowering
grows new inputs (the sweep + twin-hash check referee).

BODY SPLITTING v1+v2 LANDED, opt-in TRS_JIT_SPLIT=<thresh>
(671d73fb, 7b8cd9ac): def pieces outline as helper fns (arena, env,
base[, port args]) -> iN — base-relative (dedups across twins),
arg-parameterized for method-arg cones (widths from module inputs,
cap 8, inline fallback in unbound frames), per-instant memo for
STABLE arg-free pieces ([stamp,value] region slots vs the now slot,
stamps init u64::MAX, dedup sig extended).  JIT bakes helper
addresses (compiled before execs); artifacts carry helpers as .so
symbols and BAKE the split threshold (trs_split_thresh global +
--split pinned in the wrapper — the threshold changes the arena
layout; mismatch warns and falls back, verified).  Helpers must be
callback-free (hard error otherwise).  Sudoku: 112 pieces (38
memoized), byte-identical everywhere, total IR 508k -> 291k (-43%);
runtime parity.  THE MONSTERS ARE IMMUNE: their 52-55k bodies are
giant inline DECISION TREES (14k branches/4.8k phis from If action
arms; def refs short-circuit to eager slots; zero helper calls at
any threshold) — reducing them needs ARM OUTLINING (outline/dedup
If-Case ACTION arms as parameterized action-helpers; needs action
lowering + token plumbing; the dedup-within-a-rule analog).  Split
stays opt-in until that lands.  Split-forced JIT sweep: 966/0.

CODEGEN QUALITY (7fd231db): TRS_JIT_OPT only ever set the BACKEND
level — the middle-end pipeline (GVN/instcombine/SimplifyCFG) NEVER
RAN.  run_ir_passes() now runs default<O{1,2,3}> before engine/
object creation: +8-10% sim at O2, link unchanged.  Case lowers as
one llvm switch (jump tables) instead of icmp ladders —
runtime-neutral on sudoku (branch mass is If trees), kept as better
IR.  O3: one-env-var experiment, low expectation (branchy scalar).
Startup decomposition: 1.26s artifact run = 0.32s trial/plan
(FnProtos serialization kills it) + 0.94s sim vs 0.36s reference =
~2.4-2.6x true codegen gap.

BODY SPLITTING analysis trail (task #14, recon commits — recon landed 2ae22835 +
a9fc0204, lowering NOT yet built): select_outlined() picks def
pieces bottom-up over the module def DAG (DAG-accurate sizing;
TRS_JIT_SPLIT threshold, default 1000, 200 looks right;
TRS_JIT_SPLIT_STATS=1 prints recon).  KEY FINDINGS: (1) sudoku's
def DAGs are small (max 2.6k nodes) — the 52k-insn bodies are
If/Case ARM-SCOPED SSA re-expansion (~20x), so splitting RESTORES
sharing the lowering loses (reference C++ has the same pathology);
(2) STABILITY IS FREE for solid prims (Ravi): the scheduler confines
every legal read — Reg read SB write, ConfigReg contract, FIFO i_*
snapshots — so every read site in an instant sees one value; recon:
38/38 outlined pieces memo-eligible.  DOCTRINE (d97b7e4a): stability
may rely only on VALUE-LEVEL prim contracts (ConfigReg written_at,
FIFO i_* saved_elems) or schedule confinement of prims with NO
unsafe wrapper (plain Reg) — mkUnsafeRWire reuses the RWire runtime
prim with relaxed annotations, so wires are not name-certifiable;
loopy FIFOs already excluded via the FifoType::Simple arena gate.
THIRD PROOF ROUTE (Ravi): certify wire INSTANCES from the emitted
schedule itself — collect transitive readers/writers per wire via
the cross-module use-walk (same machinery the classifier recursion
needs), certify iff every reader position follows every writer
position in each composition.  Unsafe wires fail the check
automatically; the same per-window argument later admits mkCReg
ports.  Also
unstable: FIFO immediate views, mkCReg ports (window-stable per
port, future), eager-set defs (their discipline is the eager-slot
mechanism).
REMAINING WORK: (a) classifier must recurse through user-child
method cones (mir=3, the biggest type, outlines 0 — tainted by
inlined submodule calls); (b) helper lowering: Expr::Def hook emits
call to hlp fn (arena, env, base) -> iN, base-relative (dedup
composes; helpers keyed (inst_sig, def)); per-type helper LLVM
module compiled before execs in the worker queue, baked addresses
for JIT / symbols for AOT; (c) per-instant memo prologue for stable
pieces: [stamp, value] slots in the instance region (EXTEND THE
DEDUP SIG with the memo map!), stamps initialized to u64::MAX at
arena init, compare against the now slot; (d) gates: twin-IR hash,
sudoku x3 both modes, battery, dual sweeps, and O2 trial
(TRS_JIT_OPT=2 becomes affordable on split functions).

NEXT (in rough order of value):
- Cone/body splitting into helper fns — two sudoku rule bodies carry
  ~130k-insn cones and set the body-compile floor (~3.5s wall in the
  background); splitting also unlocks raising TRS_JIT_OPT.  With
  AOT this matters doubly: link time AND affordable -O2 artifacts.
- Heuristic escape hatch for sched compile if a cone-heavy design
  makes the blocking phase noticeable (trial_lower already measures
  sched IR size per design for free; aggregate TRS_JIT_TIME across
  the sweep corpus to pick the threshold).  Not needed for any current
  corpus design.
- Per-MODULE-TYPE code sharing (slot-offset tables instead of baked
  constants) — one codegen for N instances (DESIGN.md §5.2); matters
  for replicated-instance designs, not sudoku.
- Prim arena fast paths for FIFO2/ConfigReg (hot trampoline calls) and
  the latch/tick machinery bypass — sudoku sim is ~6x off reference;
  these close most of it.
- Per-composition fallback granularity (the arena fall-through in
  eval Def now provides the cross-composition read path naturally).
- Ship it: enable the jit feature in the Makefile release build once
  llvm-18-dev is a build prerequisite Ravi accepts; then point bsc's
  trsLink at `trs link` so every -trs build produces the
  persistent artifact (build-vs-build and run-vs-run then compare
  column-for-column against reference Bluesim).

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
