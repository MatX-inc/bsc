# TRS — handoff (rewritten 2026-07-09, start here)

NOTE 2026-07-10: the live branch is claude/trs-fst — the whole
history REBASED onto personal/bluesim-fst (PR #1027, FST support)
plus the trs FST implementation.  claude/trs fast-forwards
to it once the rebase gates finish (sweep parity DONE 2x0-DIFF,
plus 2 more idle 992/0 sweeps 2026-07-10 with HEAD's bsc — the
load-victim heavies re-verify is covered; full testsuite DONE
2026-07-10: 23473 PASS / 0 FAIL / 134 XFAIL, fullparallel+SystemC,
tree incl. the startup-snapshot change; still pending: fence
rebaseline once the new-bsc equilibrium is ACCEPTED).

Branch `claude/trs`, all work committed and pushed through
cdfd7611.  ALWAYS `git push personal` — NEVER bare `git push origin`
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
  NOT_SUPPORTED/BVI 14, bsc-side LINK_FAIL 3).  Sealed 15x on frozen
  binaries 2026-07-09 (latest: cdfd7611, quiet diagnostics + $stop resume;
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
- INTERACTIVE: battery 33/33 BYTE-IDENTICAL vs reference Bluesim
  (tests/interactive/run.sh mirrors testsuite/bsc.bluesim/
  interactive + local FinishPeek, bdpi, oracle, oracleaot,
  finishpeekaot, oracleprims, quietwarn, stopres x2,
  capi_witness, vcdtcl witnesses).
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
- $STOP RESUME (review backlog closed): $stop now PAUSES — a one-
  shot stop_request yield (cleared per advance) separate from
  finished, so bk_finished stays false and `sim run`/`sim step`
  resume; $finish stays terminal.  Batch stays byte-identical (the
  yield reaches script end and exits 0, matching the reference's
  terminal batch $stop).  Central player yields resume-correct
  (fin_break re-arms the SUCCESSOR posedge; the executed one must
  not re-pop).  MEASURED first (stopres probe): reference stops AT
  the $stop with the edge complete, resumes to $finish, refuses
  post-$finish steps.  Witnesses: stopres.cmd (interp) +
  stopresoracle.cmd (interp,jit), batch + artifact probes identical.
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
- STARTUP SNAPSHOT (2026-07-10): `<base>.birsnap` decoded-design
  sidecar written by trs link, loaded by run when EVERY header gate
  passes (magic | BIR_VERSION | SNAP_LAYOUT_REV | .bir fingerprint |
  payload fnv1a — all pre-deserialize, then structural verify()).
  Grid N=32 startup 0.138s -> ~0.083s (the CBOR decode was the real
  cost, 80.3ms; plan-walk attribution was wrong).  DISCIPLINE: bincode
  is positional — BUMP SNAP_LAYOUT_REV (trs-ir/lib.rs) with any
  serde-visible trs-ir change, the AOT_LAYOUT_REV twin rule.  link
  loads via load_file_fresh (writer reads the .bir source of truth,
  never a prior sidecar).  10-case hostile-snap battery + poison-at-
  link probe all fall back byte-identically; 48-agent review fleet's
  2 confirmed roots (layout drift, payload integrity) closed by the
  v2 header.  Chapter + dead ends (rkyv, helper thread): PERF-
  BASELINE.md.
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
  Expect 1008 PASS / 0 DIFF at the tuple-fix equilibrium (1075
  enumerated incl. the bsc.trs witness designs; non-PASS is all
  upstream-class: COMPILE_FAIL 28, NO_SOURCE 20, NOT_SUPPORTED 16,
  LINK_FAIL 3 — EXPORT_FAIL is 0 since the encExpr tuple fix).
  LPT scheduling reads tools/sweep-costs.json.
  Perf fence flags = treat like DIFFs (ratios vs tools/perf-fence.json;
  rebaseline only on accepted equilibria).  NO other builds or heavy
  jobs during a sweep (timing noise -> false flags).
- Local ladders: tests/regress/run.sh (6), tests/vcd/run.sh (10),
  tests/interactive/run.sh (33; needs TRS_CAPI_LIB=<libtrs_capi.a>),
  plus sudoku + sysMips byte-parity from kept .bir (copy designs to a
  STABLE dir — sweeps rm -rf their work dirs).
- Traps: TRS_JIT env is is_none()-tested — ANY value (even 0)
  enables; unset to disable.  Wrapper scripts bake the absolute
  linker path but honor $TRS.  Bash tool cwd persists — cd to the
  crate root before heredoc patches and check the ok-sentinel.
  Another checkout (claude3/prim-fixes) sometimes runs testsuites on
  this box — check ps before trusting timing.  A `make -C src/trs
  install` WITHOUT LLVM_SYS_181_PREFIX silently rebuilds trs LEAN (no
  JIT/AOT): sweeps still PASS byte-identical but every design runs
  interpreted (~50 false fence flags, instant trs_link) — PROBE the
  frozen binary before any sweep (`trs link <small>.bir` must
  produce a .so).  NO THREADS on the run startup path — one
  short-lived thread permanently drops glibc malloc's single-thread
  fast path and interp-fallback runs pay ~50% (startup.rs doctrine).

## NEXT UP (in order)

1. FST REBASE (task #7, IN FLIGHT 2026-07-10): claude/trs-fst
   = all 264 commits rebased onto personal/bluesim-fst + the trs
   FST feature.  DONE: GenABin 139th Flags binder (the one semantic
   rebase conflict — both parents grew the record); FST-era loader
   compat (bk_get_VCD_file_name hard-required, bk_set/get_waveform_
   format); COMMON WAVE ENGINE in vcd.rs (format-agnostic buffering/
   state machine/limits; Text sink = frozen VCD bytes, Fst sink =
   fst.rs over the vendored libfst built with the reference's exact
   config — scopes carry MODULE TYPE, prims pass NULL like the
   reference); format-dependent default dump file; +bscvcd/+bscfst
   batch plusargs; `sim fst` witness (fsttcl.cmd) + FST twin in the
   vcd ladder (fstcmp.py semantic compare — FST bytes embed
   timestamps).  Reference-vs-trs FST: SEMANTIC MATCH (batch +
   interactive).  diffsweep hardened against in-tree VPI wrapper
   residue (fullparallel leaves it; 40 phantom LINK_FAILs).
   fstscopes module-type cross-check DONE 2026-07-10 (install-extra
   built; ref exes relinked -dump-formats vcd,fst; scope dumps
   BYTE-IDENTICAL on sysVCDTest2 115-line two-level hierarchy,
   sysCDiv, sysSyncB).  -dump-formats gating: reference semantics
   RE-MEASURED 2026-07-11 (the earlier "silent no-op" note was a
   stdout-only capture — WRONG) — +bscfst on a vcd-only exe is rc=0,
   no file, but LOUD on stderr: "Error: this model was not built
   with FST support (rebuild with -dump-formats fst)" (vcd.cxx
   bk_set_waveform_format returns BK_ERROR; bluetcl's simWaveform
   swallows the status ON PURPOSE, bluetcl.hs ~3082 — sim continues
   without dumping).  -V ignores the extension and ALWAYS writes VCD
   bytes (bluesim.tcl hardwires wave_fmt vcd for -V), even on a
   vcd,fst exe.  FLOW FACT: the .bir export (bsc.hs writeBirFile,
   in genModuleC just after simPackageOpt) is UPSTREAM of where
   -dump-formats takes effect (SimBlocksToC set_wave_formats emits
   vcd_set_allowed_formats(mask) + vcd_register_fst into the
   generated create_model — that call is what links libfst+libz in).
   The flag is in scope at export time but the .bir carries no trace
   of it — it is an artifact/link policy, not design.  DECISION
   still open: MIRROR the restriction (strict parity = reproduce the
   stderr refusal; plumbing options: (a) .bir carries dumpFormats —
   BIR_VERSION + SNAP_LAYOUT_REV bumps, bakes link policy into the
   design IR; or (b) pass it through the trsLink command line +
   fallback wrapper (bsc.hs trsLink linkCmd) — .bir stays pure, and
   hand-linked artifacts default to vcd exactly like a default
   reference build) vs DIVERGE (trs artifacts carry both writers =
   capability superset; stdout/exit oracle unaffected, but trs would
   dump where the reference prints the stderr error and refuses —
   a visible stderr-parity divergence, NOT a silent-drop one) vs
   FIX THE REFERENCE (bluesim-fst is NOT upstreamed — the gating
   semantics are ours to change; e.g. default -dump-formats vcd,fst
   would make every reference exe carry both writers and trs's
   superset becomes exact parity for free.  Cost: libfst+libz link
   into every Bluesim exe — defensible locally, may not survive
   upstream review if #1027 is ever resubmitted).  The old DIVERGE
   recommendation rested on the false silent-drop reading; with the
   loud refusal measured, strict-parity doctrine favors MIRROR via
   (b), or FIX-THE-REFERENCE if the both-writers default is
   acceptable.  PENDING: that decision, and the $dumplimit FST
   estimate witness.
2. Review backlog (all confirmed, file:line in the 4e5df577 commit
   message): SimExportIR.encExpr split-port tuples FIXED 2026-07-12
   (all 14 EXPORT_FAILs converted to byte-parity PASS; three layers:
   ATuple/ATupleSel -> Concat/Extract encodings mirroring
   SimCCBlock's wide-bit lowering; argInputPorts per-PORT expansion
   at ACall/AMethCall — callers sent one value per ARG while the rc3
   adaptation flattened callee inputs per PORT, scrambling split-arg
   bindings; and trs aot_emit EmitFail routing so post-trial-lower
   ineligibility degrades to the interp artifact instead of hard-
   failing the link).  NEW QUEUED: compiled-tier MethValue support
   (no lowering exists — Extract(MethValue) shapes run interp:
   sysInstanceSplit, sysShallowSplit, sysSplitVectorPorts,
   sysFloatTest) + trial-lower coverage for cones it does not walk;
   multi-clock EN latch clearing;
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
   reference); $fgetc double-consumes shared stdin under the oracle
   (needs a primary-writes/secondary-replays tee in FSlot::Stdin)
   [the other quiet leaks — prim println! guard warnings and
   'Output error:' lines — FIXED same night: thread-local
   QUIET_ENGINE + qprintln!, quietwarn battery witness]; state compare
   blind to CReg/BRAM/DualPortRam/synchronizers (no sym_children);
   VCD-under-Tcl dead on the shipped default jit engine and bluetcl
   can't see bk_enable's 0 (auto-downgrade at -V or a loader note);
   quiet $fopen(w) Sink cannot mirror a primary open FAILURE (fd
   key skew — document or probe-open); async catch-up on a slow
   secondary still blocks bk_sync for its serial replay (lockstep
   slice advancing is the real fix).
   (prime()'s compile workers vs dlclose: FIXED; Fifo/RWire peek
   staleness: FIXED; central-loop negedge overcount: FIXED.)
3. Hygiene: sysCRCTest1 CLOSED 2026-07-11 — the fence was
   REBASELINED at the accepted i0 equilibrium (the 992/0 sweep on
   frozen-i0-v3; ACCEPTED by Ravi): tools/perf-fence.json now pins
   930 designs from that run's ratios (sysCRCTest1 link 0.51 =
   the measured binary-independent reality; memq link improved to
   0.10 with the tail gone; corpus link median 0.032 -> 0.036).
   History of the flag: reproduced on an idle box (0.40-0.45 vs the
   stale 0.22, trs_link ~1.3-1.5s) on BOTH the b8691ab4 and pre-fix
   binaries — binary-independent drift (sysTrafficBRAM did not
   flag).  traffic_light_controller_separate link flag (tuplefix
   sweep, 0.07 vs 0.01 baseline) DISPOSITIONED 2026-07-12: the old
   0.02s "links" were interp-era artifacts of VERSION-MATCHED stale
   .ba residue in the testsuite dir (left by the Jul-10 fullparallel,
   admitted via diffsweep's -p wk:testdir:+ search path); the bsc
   rebuild's version bump forced the first fresh sweep-flag
   elaboration — the design now COMPILES, byte-parity, 0.19s idle on
   both binaries, and a full SimExportIR revert reproduces it (the
   tuple/morder changes are exonerated).  Rebaseline at the next
   accepted equilibrium.  diffsweep hardening DONE 2026-07-12:
   sources are copied into the work dir and testdir is OFF the
   search path (was -p wk:testdir:+), so version-matched .bo/.ba
   residue can never substitute .exp-flag elaborations for
   sweep-flag ones; validated under fresh residue (post-fullparallel
   tree): 1008/0, class distribution identical, traffic_light
   compiles fresh.  Fence REBASELINED at the accepted hardened-sweep
   equilibrium (2026-07-12, Ravi-accepted): 987 designs pinned (was
   930 — the tuple-fix converts and witness designs enter fence
   coverage; traffic_light now 0.07, its flag CLOSED).  memq
   DISPOSITIONED
   2026-07-10: its link is BIMODAL — modal 0.39-0.55s (= the 0.12
   baseline) with a 2.4-9.9s tail at ~13% (2/15 idle runs, same
   binary/.bir); TRS_JIT_TIME isolates the tail to "trs aot: ir
   passes" (245ms modal -> 2.4-9.5s), i.e. LLVM O3 time varies with
   the RUN-TO-RUN IR shape (HashMap iteration order in lowering) —
   sim output stays byte-identical.  Not fixable by rebaseline; the
   real fix is DETERMINISTIC IR EMISSION (stable iteration order in
   lower.rs/planner), queued with the review backlog.  Full testsuite
   RE-CERTIFIED 2026-07-10 at the rebased tree + startup-snapshot
   change: 23473 PASS / 0 FAIL / 134 XFAIL (fullparallel, SystemC
   enabled; was 18865/0/129 pre-rebase — the new base's tests account
   for the growth).
4. Scale arc: loop-rolled spine (planner run-detection over
   comp_nodes + affine base/token strides + counted-loop emission
   around the EXISTING outlined-body call ABI; bail unless provably
   affine; exec sites first, sched sections after) -> type-keyed
   analysis (startup) -> pools -> lanes.  Long-run grid measurement
   (1M cycles) still untaken.
5. RELEASE REBASE (waiting on the new downstream release; playbook =
   docs/REBASE-PLAYBOOK.md, fleet-verified 2026-07-12): true
   merge-base d2f996c0, upstream +38 of which ~30 are squashes of
   OUR OWN work (resolve already-applied); conflict map, #1040-LAST
   rule, and the narrow 6be62d63/71226f07 parity-audit targets are
   in the playbook.  The old item-5 numbers (534241d5, 9 files/18
   commits) are SUPERSEDED.
6. -C-PHASE / BAZEL ARC (2026-07-11, full plan + measured dossier in
   docs/CPHASE-PLAN.md).  DOCTRINE (Ravi): BOTH flows optimal — the
   one-shot flow stays first-class; our downstream use runs entirely
   under Bazel, where caching/incrementality belong to BAZEL, not
   the tools (no tool-internal caches); extra flags exposing
   finer-grained hermetic, DETERMINISTIC steps are fine (the -c
   pattern).  MEASURED (grid8 leaf-edit loop 2.37s): bsc compile 66%,
   .bir export 0.165s (cheap), trs link 0.65s (ir-passes 52% /
   backend 24%); reference object reuse ~0% on leaf edits, ~25% best
   case — trs full relink already beats it 3.3x.  KEY FACT: parent
   exec bodies INLINE child method bodies (lower.rs:2405/3797), so
   per-type cache keys are the fragment CLOSURE — leaf edits
   invalidate ancestors; wins are sibling/top/cross-design/CI.
   STAGED (gate between 2 and 3): I0 deterministic IR emission —
   IN FLIGHT 2026-07-11, see docs/CPHASE-PLAN.md "AS BUILT": the
   plain consumers sort was a TRAP (memq 0.79s -> 17.5s: emission
   CONTENT was order-dependent — lazy_mux re-expands non-shared deps
   in both arms, 2^k-1 copies; the old ~13% bimodal tail was random
   orders losing the dep-before-user race).  Fix = en_slots sort +
   pinned corder + Kahn topo of each hoist prelude (deps first);
   memq now 6,242 IR lines vs 8.8K best-ever random draw, grid8
   size-neutral, byte-identical IR+.so everywhere.  Witness: the
   DejaGnu testsuite dir testsuite/bsc.trs/determinism (CountShare =
   the chained-fold shape + HoistDivTrap; 3-link byte-compare of
   pre-O3 IR + .so, golden stdout; UNSUPPORTED when trs is not
   installed, and VERIFIED to FAIL against the pre-fix binary —
   note it gates the INSTALLED trs, so refresh inst/bin after
   changes).  ALL GATES GREEN 2026-07-11: memq
   20-link wall UNIMODAL 0.40-0.54s (was bimodal 0.46-12.6s; median
   0.79 -> 0.43); ladders on the final binary 6/6 + 11/11 + 35/35;
   3-lens adversarial review + verify = ZERO confirmed defects;
   ISOLATED SWEEP on frozen-i0-v3: 992 PASS / 0 DIFF, sole fence
   flag = the standing dispositioned sysCRCTest1 stale baseline
   (re-confirmed binary-independent: IR byte-equal 11,036 lines and
   1.35-1.5s links on BOTH binaries; rebaseline DONE — equilibrium
   accepted, see hygiene item 3).
   AWAITING Ravi's commit sign-off.  Emitted-IR size =
   new load-immune fence metric; single-consumer 2^k cones remain a
   latent (pre-existing) class, emitter-side fix queued gate-
   triggered — CLOSES the review-backlog determinism item once the
   sweep seals; I1 step-split flags +
   hermeticity + Bazel graph at existing granularity
   (-trs-export-only, --cc, TMPDIR, env-knobs->flags, pre-lowering
   TRS_JIT_TIME lap, .ba byte-cutoff experiment); I2 noinline A/B +
   gate measurements (proceed bar: >30% of relink wall reusable on
   top/sibling edits at target size); I3 per-module .bfrag at -c
   (content_hash fills SimExportIR.hs:733 TODO; golden splice
   byte-identity vs monolithic .bir); I4 trs plan/modc/link --objs
   per-type objects (manifest-verify -> full in-link fallback = the
   certified path; TYPE_ABI_REV; no absolute slots in plan cards).
   Judge dissent: only I0-2 have GUARANTEED measured payoff; do not
   start I3 first.

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
