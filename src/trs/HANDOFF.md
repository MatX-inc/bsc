# TRS — session handoff

Branch: `claude/trs` (all work committed and pushed through
99f167a9, replication-aware outline dial, 975/0 SEALED — new high — ALWAYS `git push personal`, never bare `git push origin`:
origin is the B-lang-org repo; a bare push once created a stray public
branch there, since deleted with Ravi's approval).  Read `DESIGN.md`
(goals/architecture), `BIR.md` (export format), `docs/VCD-CONTRACT.md`
(byte-level VCD semantics), and `docs/PERF-BASELINE.md` (measured
numbers) alongside this.

## SESSION 2026-07-09 EVENING: battery 22/22, \$finish semantics,
## ultracode review — pushed through 4e5df577 (sweep sealing)

- INTERACTIVE BATTERY 22/22 byte-identical (from 14 at first
  contact).  Final fixes: async driver thread (StopCond abort/
  progress; bk_now answers LIVE from the worker's published slice
  time); JIT ENGINE wired (arm_jit bypasses the env gate; default
  engine = jit when the lib carries it; battery pins engines=interp
  for peek tests, jit for async — the capability tiers in action);
  \$FINISH COMPLETES THE IN-FLIGHT EDGE (unanimous 4-agent hunt:
  kernel bk_finish_now only marks + yields; the edge schedule runs
  to completion; Ravi's VCD-coherence framing — instants must be
  fixed points).  Suppression = the WHOLE dollar_display.cxx output
  family (29 gates: console+file+severity), NOT just console — and
  the yield preempts the PG_FINAL after-edge pass (sysFWrite3
  caught the overshoot).  Compiled paths still mid-edge-abort on
  finish: NEXT ITEM (callback stops signaling abort; edge fn runs
  to completion; add the trailing-state-rule regress witness).
- ULTRACODE REVIEW (7 finders, 72/72 verdicts upheld): 10 findings
  FIXED in 4e5df577 (def_thunk ssa dominance leak; effectful
  ForeignCall; 3 async-window process aborts; bk_shutdown joins the
  worker before dlclose; bk_sync raw time + panicked-worker
  survival; mid-sim timescale reject; tier-honest NoValue; AV
  result recording; ConfigReg raw member; const-ready RDY interim).
  REVIEW BACKLOG (confirmed, queued): \$stop-vs-\$finish resume
  semantics (bk_finished must not latch on \$stop; interp needs
  resume-past-\$stop); multi-clock EN latch clearing (latched.clear
  per ANY edge); central-loop negedge overcount-by-one; exporter
  round 2 — SimCOpt-surviving methodPorts set (kills the RDY
  interim); symOrd char-wise compare; Fifo/RWire arena-attached
  peek staleness (jit-engine tier); link_interactive should PROBE
  the staticlib for LLVM refs instead of trusting its own cfg;
  fence mode-awareness (AOT baseline vs non-AOT sweeps); prime()'s
  detached compile workers vs dlclose.
- Interactive .so link: -u keep-list (48 syms) + shared libLLVM +
  ffi/tinfo/zstd + --no-undefined; 166MB (gc-sections/strip TODO).

## IN FLIGHT: trs-capi (task #10) — the bluetcl surface

Landed (7a0cd508..38293521): crate scaffold (staticlib, jit feature
DEFAULT ON per Ravi — 'sim run' at hybrid speed; --no-default-
features keeps a lean LLVM-free .so and the no-jit interp build is
repaired), multi-engine SimState (interp/jit/aot, one or SEVERAL =
interactive oracle; primary owns stdout), docs/TCL-CAPI.md carries
the FULL contract: measured dlsym set + load protocol, namespaces
(bk_* FROZEN bit-for-bit; trs_* for everything ours INCLUDING
fancier variants of bk functionality — bk names never grow options),
capability tiers (fast/AOT engines have NO debug introspection by
design — architectural state peeks only, absence rendered in the
reference API's own vocabulary: NULL peek / NoValue), degradation
contract (downgrade-to-interp with stderr notes; stdout is
byte-parity territory), oracle divergence stops at the divergent
instant.  Flagship debug config: --engines=interp,aot.

PROGRESS (this session, all pushed):
- f6a2c00e StopCond/advance_until SEALED 975/0 (4 fence flags are
  suspect — capi builds ran during the timing window, violating the
  quiet-sweep rule; re-verify sysTrafficBRAM/sysRegSelect/
  sysConflictFreeNotOKLarge/sysCRCTest1 quietly before accepting).
- 60e7b733 dlsym-complete surface: STOCK BLUETCL LOADS US.  Probe
  (scratchpad capi-probe/: embedded-BIR shim.c + --whole-archive
  libtrs_capi.a + stock export map) runs sim load/time/clock/
  step/run BYTE-IDENTICAL vs the reference model .so on mkTest.
- 6ee6d08b clock surface + run control (limit slots absolute,
  disarmed at-or-below count = bluetcl's restore idiom).
- 31d31edc symbol tree: ls/lookup/cd/pwd/get/describe live;
  get/describe/cd/pwd BYTE-IDENTICAL.  'sim ls' def subset still
  approximate: the reference registers only defs SURVIVING AS C++
  MEMBERS = CF/WF cone closure (SimMakeCBlocks.hs:264-269
  getExprIds) minus SimCOpt-deleted inlinees (SimCOpt.hs:266).
  EXACT FIX (next): bsc-side — SimExportIR emits a per-def `sym`
  flag taken from the post-SimCOpt sb_publicDefs/sb_privateDefs
  survivor set (bsc.hs has both the blocks and the exporter call;
  encDef currently discards props), BIR field with serde(default),
  full bsc rebuild + testsuite, then drop the interim ___d<N>
  filter in Interp::def_symbols.

INTERACTIVE BATTERY: 20/22 (f3e221d2) — every fix pinned against
the reference sources (timescale=bk_now scaling; static Wave.delay
first_edge; SYM_PORT method ports with EN latch/arg recording/
result-member/CAN_FIRE-alias RDY; per-prim sub-symbol tables from
bs_prim_mod_*.h incl. the level->&size and raw-ring-fetch
contracts; RegFile raw backing fetch at address width; catch_unwind
peek belt).  REMAINING: mkLong async (driver thread) and mkTbGCD
debug (2-line one-swap-behind transient at one stop, re-converges
— step-boundary alignment, fresh eyes).  Batch gates green
(regress 5/5, sudoku/sysMips identical; \$time gained a *1 multiply
— identity at timescale=1; next sweep rides the next increment).

FIRST CONTACT was (frozen 5baca7b9 + capi):
14/22 PASS (mkTest all 6, mkTop all 3, aperiodic both, TbGCD
debug3/4/5).  The 8 failures, root causes PINNED:
1. mkMCDTest clock.cmd — derived clock (clk2$CLK_OUT) tuple: our
   first_edge=0 vs ref 2.  Ref knows waveforms of ClockGen-derived
   clocks STATICALLY (prim params); our VcdClock only observes.
   Fix: populate first_edge/durations from ClockGen prim params at
   prime().
2. mkLong async.cmd — bk_advance ignores the async flag (sim run
   async blocks; sim stop starves; 9-minute hang, killed).  Fix:
   the driver thread (bk_is_running/bk_sync/bk_abort_now real).
3. mkPrims prims.cmd — 'No match for isValid' + a stray '{} signal'
   in our ls (empty-key child leaking where ref hides it).  RWire-
   class prims expose named sub-signals; inspect prims.bsv shapes.
4. mkTbGCD gcd/debug/debug2 — 'No match for EN_start': METHOD PORT
   symbols (SYM_PORT: EN_/arg/ret ports of module instances) not in
   our tree.  Ports are in the BIR (module inputs/method args); the
   peek side needs last-driven port value recording (like the def
   recording).
5. mkTimescaleTest both — %t values unscaled: bk_set_timescale is
   stored but must plumb into the interp's %t/$time display scaling
   (ref prints time x timescale factor).
ALSO: the machine hosted ANOTHER session's full testsuite during
today's sweeps (claude3/prim-fixes worktree) — the repeat fence
flags (sysRegSelect run, sysCRCTest1 link, SpecialSyncReg) are
suspect-environmental; re-verify with --filter on a TRULY idle box
before touching code.  diffsweep now LPT-schedules from
tools/sweep-costs.json (d2ad4ccd) — the straggler tail collapses.

NEXT BLOCK (original plan, stop conditions DONE):
- VcdClock is already the tClockInfo mirror (bk_clock_* fields
  labeled in comments); it lacks a NEG_COUNT (bk_clock_edge_count
  needs both directions) — add + increment beside pos_count.
- StopCond { edge_limits: Vec<(clock_idx, posedge, count)>,
  at_times: Vec<u64>, max_cycles } + advance_until(StopCond);
  advance(max_cycles) delegates.  Semantics: bk_quit_after_edge =
  stop AFTER edge #count of (clock, dir) completes (bluetcl
  computes count = bk_clock_edge_count + N); bk_quit_at = stop at
  END of time slice t (pop of an event with time > t stops, edge
  pushed back like the max_cycles path at lib.rs ~3619).
- CENTRAL LOOP engages only for a pure default-clock-cycles cond
  (anything else bails — its niche is batch runs).
- Gates: the stepper is shared by ALL paths — full battery +
  sudoku/sysMips parity + corpus sweep before sealing.
Then: bk clock surface (mostly reading VcdClock/ClockInfo), symbol
tree, 'trs link --interactive' shim (embed BIR + new_MODEL_<top>,
cc-link with libtrs_capi.a, export map), rung 1 under real
bluetcl (sim load/time/ls on testsuite/bsc.bluesim/interactive
mkTest).

## SEALED (2026-07-09 latest): replication-aware outline dial

99f167a9: OUTLINE_FLOOR / k (k = module-type replication in the
composition; k=1 keeps every prior decision).  Grid v3 link 202 ->
27s at N=32 (ahead of ref build at every N again), run 0.320 ->
0.079s at N=16 (the unroll was an I-cache run cost too).  Sweep 975
PASS / 0 DIFF (420s ref-build ceiling recovered 2 misfiled
LINK_FAILs); fence rebaselined (925).  All 10 flags benign (new
coverage + dial redistribution, all still ahead of ref).  NEXT ON
THIS ARC: loop-rolled spine (planner run-detection + affine base/
token strides + loop emission around the existing outlined-body
call ABI; groups bail unless provably affine; roll exec call sites
first, sched sections later), then type-keyed analysis for the
O(instances) startup (N=32 run 0.262 vs ref 0.157 is ALL startup).

## SEALED (2026-07-09 later): ActionValue methods on module children

c9c60f5f + 19110cdc: AvAction MethCall on INLINED MODULE CHILDREN
lowers (EN protocol, body stmts in a child frame under the call
cond, result expr, phi out of the taken arm; skip = undet zeros).
Two lessons, both regress-tested:
- synthetic AV result defs are in NO def table: binding width must
  come from the evaluated result (an intermediate def_width version
  built an i1 phi — grid v3's checksum caught the truncation;
  tests/regress/AvMethInline.bsv keeps it caught), and expr_width
  consults Frame.av_widths.
- cone(): a def-table miss now POISONS the cone (poison|=1,
  context-bound like an arg port).  Edge-SSA had hoisted slices of
  another rule's AV result into a section with no binding (PAClib
  RadixSort rev4: hard 'unknown def' AFTER screening passed).
  RadixSort now COMPILES, byte-identical, b3_run 0.060 vs ref 0.111.
- 'unknown def' diagnostics now name the def + lowering context +
  expansion chain.
- diffsweep ref-build timeout 180 -> 420s: sysBRAM0Test/sysFloatTest
  ref builds measure 166-256s under load and flapped at the 180s
  ceiling as LINK_FAIL "unknown"; both re-verified PASS.
- bench/grid v3 (default tile): program tiles (case-ROM, PC, RegFile,
  opcode dispatch) + ActionValue oTake drains in the link-rule arms.
- docs/TCL-CAPI.md: the measured bluetcl contract (47 dlsym'd bk_*
  fns, load protocol, symbol-tree semantics, interactive-test usage
  profile) + the trs-capi design for the DEBUG compile mode.

## SEALED (2026-07-09): conditional-arm class + direct BDPI actions

Ravi's "fix actionvalue and conditional arm" is DONE end to end:
- 21bacd87: Frame.dead_defs discipline (arm defs allowed; only a
  post-arm USE of a dead def is ineligible, error names the def),
  same-port-pure-cond Cond-run merging, IDEMPOTENT-task join
  re-materialization ($time/$stime from the now slot;
  $test$plusargs/$value$plusargs re-called — run-constant,
  side-effect-free), and direct BDPI Action::Foreign +
  AvAction{Foreign} (integer-slot ABI, stdio flush pair, ActionValue
  phi-binding out of the taken arm).  mkTestValues: interpreter
  fallback -> FULLY COMPILED, byte-identical.
- 09cb6a86: the sweep's one DIFF (sysMips 116 RegFile bounds warnings
  vs 66) exposed EFFECTFUL-EVAL fidelity: evaluation of a cone that
  can WARN (partial-range RegFile.sub) or TRAP (Quot/Rem) is
  observable, so compiled code must evaluate exactly as often — and
  where — the interpreter does.  Count fix: effectful defs expand
  through an entry-alloca THUNK (value + valid flag; first dynamic
  reference evaluates, later ones reuse — ssa memos die at Cond/mux
  joins for dominance, and re-expansion re-fired warnings).  Order
  fix: sched sections latch EFFECTFUL eager defs first in
  REntry::eager list order (interp latch position); pure defs stay
  lazy.  Full-range RegFiles exempt, judged against the ADDRESS
  WIDTH (mkRegFileFull can't warn; thunking sudoku's LUTs cost 2.4x;
  power-of-two-SIZED partial ranges like sysMips ram_arr still
  warn).  TRS_WARN_DEBUG=1 tags warnings [now= src=I|C:kindN:local]
  via a thread-local trampoline token — the instrument that pinned
  both mechanisms.  tests/regress/RegFileWarnCone.bsv is the proven
  witness (pre-fix binary: 4 warnings vs ref 2).
- SEALING SWEEP on frozen 09cb6a86: 973 PASS / 0 DIFF (ties the
  all-time high, now WITH the arm-def class compiled).  All 8 fence
  flags were link-time on former early-bail designs now doing real
  LLVM work (0.01 -> 0.07-0.10s) — new-coverage cost, accepted;
  fence REBASELINED to 947 designs (09cb6a86).
- Constraint cost A/B (loose 21bacd87 vs fidelity 09cb6a86):
  sysMips 0.04s both; synthetic hot partial-range-RegFile guard+body
  bench 0.40 vs 0.39s (equal, ~2.8x ahead of Bluesim 1.14s).  The
  fidelity is free where it now applies; the 2.4x/1.3x costs were
  the over-broad first cuts, scoped away.

## RESOLVED: the g2 regression was ONE bug — always-fire (task #23)

Bisect finished as predicted: 7694c351 (fusion-JIT) walk-CLEAN,
447c1a0d (one-module) walk-CLEAN, 668303f1 (always-fire) fails both
repros on BOTH paths (JIT-sync walk and AOT artifact).  The "two
bugs" theory is dead: the g2-2/g2-4 sweep binaries were built from
the WORKING TREE that already carried the uncommitted always-fire
code (committed 23:16, mid-gate) — the reverse stale-binary trap:
record the binary's commit+dirty state in every gate log.

ROOT CAUSE: detection accepted const-true CAN_FIRE + empty
inhibit_slots as "provably always fires".  False theorem: bsc bakes
preemption/urgency into the WILL_FIRE def EXPRESSION
(WF_a = CF_a && !WF_b) and NEVER into me_inhibits/cross_inhibits
(both empty for every victim).  Misclassified rules compiled with no
WF gate and fired unconditionally.  ALL EIGHT g2-4 AOT DIFFs were
this one bug (Esposito preemption, RegFileVector off-by-one-edge,
memq DQueueTb, IfNested, 3x interra SRAM truncations, sudoku
emitting nothing) — every one verified PASS vs reference post-fix.

FIXES LANDED (all pushed to personal):
- 2f01bd68 always-fire detection resolves the WF def itself through
  Def-alias chains to a constant; CAN_FIRE arm removed.  WF=Def(CF),
  CF=const still qualifies: Esposito's RL_b/RL_set_done verified
  gate-free in the IR dump, victims verified gated.
- 982afcd8 sysInit65536Bit AOT link timeout: LLVM known-bits is
  quadratic in width; i65536 wedged default<O1> >90s (O0: 1.9s).
  run_ir_passes skips the DEFAULT pipeline when any instruction type
  exceeds 4096 bits (module_max_int_width walk); explicit
  TRS_JIT_OPT still forces it.  Links 2.2s, byte-identical.
- decc231e central loop now engages in AOT artifacts: fused edges
  exist at t=0, so the first slice-boundary probe ran during the
  initial reset pulse and bail #4 permanently burned the attempt —
  streaming JIT only reached the 0.09s floor because fusion compiles
  after reset by accident.  Transient reset (rst_asserted/
  rst_pending) now bails WITHOUT burning (#15); static disqualifiers
  (VCD, driver clocks, rstgen_out) still burn.  LongCnt artifact
  0.51s -> 0.11s (loaded machine), central engaged.

GATE: ALL GREEN on decc231e (definitive 3-leg, 1037 designs).
AOT 966 PASS / 0 DIFF / 0 TIMEOUT (best AOT leg ever: always-fire
DIFFs gone, width-cap link timeout gone, central-loop-at-scale
clean); JIT-sync 966 / 0 DIFF; JIT-lazy 966 / 0 DIFF.  Battery 9/9.
Quiet re-measure: sudoku link 8.1s / run 0.48s byte-identical (the
fixes cost nothing); LongCnt ARTIFACT 0.05-0.06s — beats the 0.09s
streaming floor (no compile workers at startup), ~5x ahead of
reference 0.27s.  Sudoku does NOT central-loop: bail #9, its
ConfigReg/FIFO prims need per-edge ticks — folding prim ticks into
the fused edge is the future rung if tick-bearing designs should
qualify.

LEASH FAIRNESS (7fa5f46e, Ravi's call): diffsweep now times the
reference build and floors every trs run limit at the reference's
own build+run wall — sync-JIT compiles inside the timed window while
the reference compiled off the clock (conflict_free_large: leash was
max(5s, 5 x 0.09s ref run) against a reference whose C++ build alone
takes minutes).  Both conflict_free_large designs TIMEOUT->PASS
under JIT-sync; the leash stays tight for the interpreter-blowup
class.

EDGE-SSA (#24) LANDED, opt-in TRS_EDGE_SSA=1 (d00f8954 emitter,
28686b02 symbol elision, 1124fe23 M1 analysis; fc3251b2 tick skip
rode along).  Whole-edge inline emission: one edge_c<k> per comp,
every sched/exec section lowered INLINE sharing an EdgeCtx cache —
LATCHED values (CF/WF/eager at compute position, never evicted =
slot semantics) + HOISTED pure shared defs (first-consumer section
start, spine dominance, evicted on write-set intersection; self-
killing consumers never hoist: tsort body-position semantics).
Insertions ONLY from driver/section-top (dominance by construction).
Purity: arena-inline prim reads only, no foreign/ports (ports:
method args are call-site-specific, EN mutates mid-edge).  Elision:
covered rules emit no standalone sched_/exec_ symbols (loader panic-
stubs them; token tables always built from protos).

MEASURED (sudoku, quiet): run 0.48 -> 0.41s (gap 1.55x -> 1.32x vs
ref 0.31s), insns -7%, L1d loads -8%, .so -40%, link 8.1 -> 17.1s
(residual = O1+regalloc on the mega-fn).  BYTE-IDENTICAL everywhere;
battery 9/9 both modes; TWO perfect full-corpus AOT sweeps (966/0/0,
pre-elision + elision builds).  KEY FINDINGS: (1) exec bodies make
ZERO eager-slot reloads — slot promotion was ~2% of load mass; the
win is unslotted cross-rule sharing; (2) the sound sharing ceiling
IS ~v1's 539 hoists: all 1501 remaining shared defs are MethodArg-
dependent through the RESULT cone, and per Ravi single-caller-per-
(method,port) makes their census sharing ME-moot at runtime; the M1
"73.7% shareable" overcounted by treating (inst,def) as one value;
(3) call-boundary variant (bodies as fns + shared args) is dead for
sudoku-class designs (p90=285 args/body) — whole-design link-time
codegen is the price of sharing, per-type calls stay the dial for
replicated designs.

TICKS: the surviving per-edge tick entries on reg/creg/fifo designs
are RESET ticks (real ticks are noop-filtered) — fc3251b2 skips them
in steady state via an asserted-reset counter (sudoku PROF ticks
0.137 -> 0.092s).  RESIDUAL = wire valid-bit clears + __me_check
R0001 checkers (both arena-friendly): compiling THOSE into the edge
fn is the next increment — it also clears central-loop bail #9, so
sudoku-class designs enter the central loop (LongCnt is 0.05s vs ref
0.27s there).

TASK #24 COMPLETE — EIGHT perfect sweep legs (966/0 each), and the
O-ladder verdict: O3 on the SSA+outlined IR lands AT REFERENCE RUN
PARITY (0.32-0.34s vs 0.29-0.32s) while linking 10.7s vs their
13.9s.  The outline COST MODEL (1b323b3d: outline iff body_mass >
max(800, 2 x consumed-sharable-mass), self-calibrating; d92ef566
carried the dial + the sched/exec coverage-split bugfix that was
silently dropping every exec symbol) made O3 affordable; monsters
leaving the mega-fn is runtime-POSITIVE (L1-misses 6.5x down).
Full numbers in docs/PERF-BASELINE.md post-edge-SSA section.

THE CROSSING (0cdd785a): wire ticks compiled into the edge fn
(coverage derived deterministically both sides via the
trs_edge_wire_ticks meta flag; interp skips covered entries only
when the fused fn ran; central #9 ignores covered ticks, #10 admits
rule-less negedge comps with only covered clears).  Sudoku enters
the CENTRAL LOOP and runs 0.25-0.28s vs reference 0.29-0.32s —
trs is FASTER THAN REFERENCE BLUESIM, byte-identical, from 0.48s
(1.55x behind) the same morning.  Link 10.7s vs 13.9s.  O3 verdict
(Ravi asked): pre-edge-SSA it bought ~0.05s (per-body scraps);
post-edge-SSA it buys ~22% run for +1s link — the transformation
made the middle-end's scope real.  __me_check entries are RULES
(already compiled), not ticks; remaining interp ticks are MCD prims
in central-ineligible designs.

SESSION COMPLETE — ELEVEN SWEEP LEGS, ALL GREEN (966/0).  Final
state (fb0f1888 defaults + 1b3e147a review fixes + 695b042a tests):
- bare `trs link` = edge-SSA + outline cost model + O3 + wire
  ticks + export elision (TRS_EDGE_SSA=0 restores classic).
  Ravi's ruling: the artifact is a SPECIALIZED compile — speed and
  scale first; the slot-level debug contract is NOT its surface.
- Sudoku: run 0.22-0.26s vs reference 0.29-0.32s; link ~10s vs
  13.9s.  Corpus timing table (diffsweep now records ref_build/
  ref_run/b3_link/b3_run per PASS): link ratio median 0.03 / p90
  0.06 / max 0.85 — trs links faster than the reference build on
  EVERY corpus design; run ratio median 0.25 (startup-dominated for
  small tests).  Export elision was worth ~10% (stores are
  optimization barriers — Ravi called it, my store-count arithmetic
  undercounted).
- ULTRACODE adversarial review (6 lenses x 2 skeptics over the
  session's commits): 4 confirmed findings; fixed same-day
  (1b3e147a): CRITICAL pre-evict at self-killing consumers (later
  shown SHIELDED by bsc's tsort positioned-body-defs contract — fix
  kept as defense-in-depth; see tests/regress/EdgeSelfKill.bsv),
  MAJOR x2 hoisted Quot/Rem SIGFPE (PROVEN by
  tests/regress/HoistDivTrap.bsv: pre-fix exit 136 vs 0), MINOR
  cost-model shared-mass credit from outlined partners (logged, not
  fixed — model v2).
- tests/regress/ battery landed (run.sh, VCD-battery-style).

NEXT SESSION QUEUE:
(1) #22 DIRECT BDPI — the keystone: deletes the foreign round-trip
    (MatX priority), makes call sites memory-annotatable, unlocks
    alias metadata + cross-call value residency, both gated
    per-design on trampoline-freedom (the linker knows from protos);
(2) RegFile inline fast path (B2 shape: in-bounds arena array +
    cold-path warning callback) — biggest remaining trampoline;
(3) parameterized N x N grid benchmark — link/run/memory/analysis
    curves vs Bluesim/Verilator (+VCS reasoning recorded in session
    transcript): measures the spine-growth wall; spine SUBTREE
    SEGMENTATION is the designed answer (regions give the cut
    points), type-keyed analysis + LOOP-ROLLED spine over
    stride-regular replicas (arena regions are DFS-contiguous with
    type-canonical layouts — the invariants already exist) is the
    replicated-design win and the #20 pools/lanes substrate;
(4) JIT-sync/lazy belt-and-braces legs; timing-threshold fence in
    the sweep summary (perf regressions have no automated guard);
(5) fullparallel testsuite AOT comparison — parity is arguably MADE;
(6) backlog: #15 arm outlining, Tcl surface, VCD parity classes,
    MCD tick classes, quiescence gating (the VCS-sparsity answer),
    content-hash incremental link.

## DIRECTION SET BY RAVI (2026-07-09 late): compile modes + finish the product surfaces

COMPILE MODES, LIKE VCS: stop making one artifact serve everything.
The FAST compile is what we built (edge-SSA, export elision, O3 —
the debug contract is explicitly not its surface).  A DEBUG/
INTERACTIVE compile mode gets slot exports, interp-visible state,
bk_*-linkability, stepping — the mode the Tcl surface and the 44
interactive tests target.  This dissolves the export-elision-vs-bk
tension permanently: bk never needed the fast artifact.

PRODUCT SURFACES TO *DONE* (before the scale arc):
1. BDPI: VALUE IMPORTS DONE (d2d6e12e + bceb3f30) — direct C calls,
   all widths byte-identical at O1/O2/O3; the newly-corpus-visible
   mkBDPIBitN caught a 2-limb Poly miscompile (in-process
   default<O2+> deleted correct out-buffer readback loads around the
   opaque call; SYSTEM opt on identical IR does NOT reproduce —
   open LLVM invocation-divergence item, repro in the session
   scratchpad bitn/ dir).  Fix: VOLATILE buffer traffic (honest for
   externally-touched memory) + entry-block allocas + debug envs
   TRS_JIT_PIPELINE / TRS_JIT_DUMP_PRE/POST / TRS_JIT_NOVEC.
   Sealing sweep: 973 PASS / 0 DIFF — highest ever (+7: foreign
   battery now permanently in the corpus after the diffsweep .h
   fix).  Fence rebaselined (939 designs, 0d71922b).  REMAINING:
   direct-call the ACTION/ActionValue side (currently correct via
   the interp callback), then per-design memory attributes.
2. def-inside-conditional-arm eligibility (146 designs — largest
   coverage bucket).
3. Tcl bk_* surface: trs-capi cdylib (46 bk_* + new_MODEL_<top>,
   BluesimLoader dlsym, driver thread) on the resumable stepper;
   builds against the DEBUG mode by definition.
4. VCD parity residue (clock-alias vars, never-computed initial
   dumps, SyncFIFO/RegAligned hooks) — debug-mode territory.
5. MCD/Sync execution story (remaining tick classes, multi-clock).

THEN the scale arc (type-keyed analysis -> loop-rolled spine ->
pools -> LANES/multicore), for which the grid v2 (cde57a4c) is the
instrument; Ravi capped the grid at N=32 and restructured it (rich
synthesized tiles, K large link rules — bsc scheduling is O(rules^2),
the 71s frontend wall was probably benchmark shape).

REBASE PLAN (Ravi, 2026-07-09): rebase claude/trs onto upstream
PR #1027 "Add FST support to Bluesim" (head personal/bluesim-fst —
Ravi's fork branch; superset of PR #1000 "-dump-formats") when there
is a chance (tree quiet, no sweep in flight).  Measured: merge-base
534241d5, theirs 25 / ours 214 commits; we never touch src/bluesim
(their main surface); conflict surface = 9 files across 18 of our
commits — flag tables (Flags.hs / FlagsDecode.hs / bsc.hs: our
-bir/-trs/-c vs their -dump-formats), bsc.help.out.expected,
testsuite/config/unix.exp, CI ymls, user_guide.tex; the one
real-thought spot is their "rework the Verilog dump harness" vs our
Verilog link-path commits in bsc.hs.  Mechanics: `git rebase --onto
personal/bluesim-fst 534241d5 claude/trs`, `git submodule
update --init` (libfst), full rebuild + full local gates, force-push
personal, then REGEN THE PERF FENCE BASELINE (ref Bluesim changed).
Payoff: -dump-formats IS the link-time waveform contract the compile
-mode split wants (fast = none, debug = vcd/fst); libfst in-tree
enables FST output in the future debug mode; testsuite gains FST
cross-checks wherever VCD is checked — plan an FST twin of the VCD
battery.  Tracked as task #7.

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

SELECT LOWERING (dfb486ed) — the big one, Ravi's insight: bsc LIFTS
shared updates into mux dataflow (If EXPRESSIONS); lazy_mux was
re-branching them into diamond forests LLVM's capped speculation
could not undo.  Pure small arms (pure_size probe, cap 64) now lower
as ONE select.  Monster bodies HALVED (54.9k->25.8k insns; 17k
selects); quiet-machine link O0 5.9->2.2s, O1 7.6->4.1s; run 1.01s
at O1 (ref 0.36).  v2 helper fixes rode along: method-arg ports
parameterize (Method.args, not Module.inputs — the unknown-port arm
must TAINT), phantom 112 pieces -> 63 real; AOT helpers best-effort
like JIT; lower_helper defines into existing DECLARATIONS (forward
call sites declare; add_function silently renames to sym.1 leaving a
bodyless decl -> FunctionLookupError / undefined .so symbol).

ARTIFACT DEFAULTS + STARTUP (beaeb3c4, 6fc102de): artifacts default
to O1 via thread-local AotModeGuard (measured ladder: O1 captures
the whole win; O2/O3 add only link time; reference ships -O3), JIT
stays O0; TRS_JIT_OPT=0..3 overrides.  Artifacts bake
trs_protos/_len (encode/decode_protos LE-u32 wire format):
loading DECODES call-site tables instead of trial-lowering — trial
is now lazy (link/plain-JIT/fallback only).  AOT_LAYOUT_REV=4.
Startup -m 1: 0.64s -> 0.08s interleaved-under-load; byte-identical
x3; rev-3 artifacts refuse + fall back cleanly.

STRUCTURAL PARITY ACHIEVED, THEN PASSED (evening arc, ~12 commits):
- TIER B2 COMPLETE (b95b2742/1f64fb07/c73bb09c): ConfigReg writes +
  FIFO enq/deq inline (arena-authoritative refresh()/mirror; inline
  UNDER the action-condition branch — branchless everywhere was a
  measured 19% regression, write sites are arm-multiplied); prim
  trampoline census on sudoku: 0 calls.
- DISPATCH FUSION (7694c351/c14fcfdb): one compiled edge fn per
  composition (JitPlans::try_fuse when warm / edge_c<k> symbols in
  artifacts, AOT_LAYOUT_REV=5) — the schedule promoted from data to
  code; killed ~77M per-node walk visits on sudoku.
- CENTRAL LOOP (b8429625): steady-state player for single-Wave-clock
  designs (t += period; fused_edge; repeat) with heap kept for
  aperiodic events; three over-strict preconditions found by STATIC
  bail counters (env-var probes measure themselves at 10M slices!):
  retry-after-fusion-exists, skip one-shot foreign-clock comps,
  accept all-rst tick lists.  sysLongCnt 5M cycles: 0.56s -> 0.09s —
  3x FASTER THAN REFERENCE (0.27s).  Plan-player generalization (the
  EdgePlan hyperperiod design for gated/MCD) recorded in task #21
  history.
- WHOLE-EDGE INLINING (447c1a0d): one-module AOT emission (helpers +
  scheds + exec reps + fused edges), pipeline flattens 306/552 edge
  call sites; ALSO fixed run_ir_passes ignoring the AOT O1 default
  ("O1 artifacts" had never run the inliner unless the env var was
  typed).  Sudoku 0.625 -> 0.472s; link 8.0s single-module (
  per-composition module parallelism = obvious follow-up).
- trsLink INTEGRATION (82df91c4): bsc -trs links via `trs link`
  (interp-wrapper fallback); wrappers honor $TRS.  bsc REBUILT.
- FnProtos in artifacts + O1 default recorded above (task #16).

TASK BOARD (see task list; all designs recorded in task metadata):
#15 action dedup; #19 waves (needs conflict-DAG export from bsc);
#20 pools/batching/lanes (Ravi's grid design — the tile-grid
transformation); #22 foreign marshaling fast path (MatX priority:
allocation-free callbacks, then DIRECT BDPI calls); #23 always-fire
short circuit (WILL_FIRE==const-true rules: no sched/WF, defs
always-compute-always-share — the static p=1 case, no PGO).
MEASUREMENT QUEUE (Ravi's ordering, revised): five-leg gate (leg 1
green) -> QUIET PARITY WORK: O-ladder re-run (post-inlining, O2/O3
finally have scope), fresh profile on optimized code, alias-metadata
experiment, #24 edge-SSA if the cheap rungs don't close the ~1.3x ->
COMPUTE PARITY -> testsuite AOT comparison (HELD until parity).
N-tile grid benchmark: PARKED (bigger step — needs a representative
design, bsc frontend scaling data, and #20's pools/batching to be
meaningful); it is #20's demonstration, not a near-term measurement.

SCOREBOARD (sudoku, quiet machine, evening):
build 8.0s one-module-O1 (2.2s chunked-O0) vs reference 13.94s at
-O3; run 0.472s vs 0.36s (1.3x, was 16x this morning); startup
0.05s; LongCnt floor 0.09s vs 0.27s (trs 3x AHEAD).

PREVIOUS SCOREBOARD (sudoku, quiet machine): build 2.2s (O0) / 4.1s (O1) vs
reference 13.94s at their -O3; run ~1.0s vs 0.36s with startup now
~0.08s — residual gap is ~2x sim-only, concentrated in the (halved)
decision trees: task #15 arm outlining/dedup + taken-path tightness.

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
