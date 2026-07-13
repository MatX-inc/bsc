# rc4 verilator burn-down journal (started 2026-07-13, session d7f74d0f)

Goal: green (or credibly-gateable) full testsuite under the MatX-deployed Verilator
(build-20260701_1, Verilator 5.048), per HANDOFF-release-2026.07.rc4.md. Feeds Ravi's
open decision: gate vs demote vs subset for this release.

## Environment (reused from previous session, verified working)
- MatX verilator + python3.11 shim: /tmp/claude-1000/-home-ravi-bluespec-matx-bsc/
  072ede51-1a56-404b-9a3e-53f30d4769e6/scratchpad/matx-verilator/{inst/bin,shim}
  (tmpfs — re-fetch from MatX-inc/verilator 787ac6d if the machine reboots).
- Run recipe: TEST_BSC_VERILOG_SIM=verilator, CXXFLAGS=-O0, RUN_TESTCASES_IN_ORDER_OF_TIME=1,
  SystemC vars, PATH = inst/bin : shim : matx-verilator/bin : ...; make -j24 -C testsuite
  fullparallel (NEVER -j128: per-test g++ OOM).

## Step 1 DONE: -use-dpi implied under verilator (harness, uncommitted)
- New proc vsim_implied_bsc_flags in testsuite/config/unix.exp (after get_vcomp_flags):
  returns "-use-dpi" iff verilog_compiler == verilator, "" otherwise ([info exists]-guarded).
- Injected in bsc_compile_verilog (cmd, BEFORE per-test $options — wrapper flavor is chosen
  at compile) and bsc_link_verilog (inside link_options, next to vcomp_flags, verilog.tcl:57).
- bsc -use-dpi is a Toggle flag => -no-use-dpi exists; later flag wins, so per-test override
  is possible (but VPI flavor then dies at verilator link anyway — VPI rejection).
- Validation: bsc.codegen/foreign/battery under verilator 66P/25F -> 87P/4F;
  bsc.options 67P/7F -> 67P/7F but the sysGCD link/sim/diff chain fixed (new fails are a
  different class, below). Battery under iverilog: 73P/0F (no default-sim change).

## Failure classes catalogued so far (killed-run sums + smokes)
1. VPI class — FIXED by step 1 (was: foreign 20 tests, battery 7, options sysGCD).
2. srandom DPI collision (NEW, exposed by step 1): battery mkTestActionValues imports BDPI
   fn literally named `srandom`; DPI flavor collides with SV built-in srandom => Verilator
   5.048 INTERNAL ERROR at link (V3Width.cpp:7231 "Classless rand-thing"). Clean link fail.
   => skip/xfail keyed on verilator; do NOT rename upstream test content. Consider
   reporting upstream to Verilator (ICE is their bug regardless).
3. VPI-artifact expectation tests (NEW, exposed by step 1): bsc.options tests asserting
   vpi_wrapper_my_time.{h,o} land in srcfiles//vfiles/ + `srcfiles/my_time.ba should link`
   + non-empty filtered link chatter. Under -use-dpi no VPI wrappers are generated.
   => per-test resolution: sim-keyed expectations or -no-use-dpi/skip (triage agent report
   pending in workflow wf_95482027-903).
4. inout -no-inline-inout-connect class: ALL 95 bsc.verilog/inout fails are the first
   (option-flagged) half of inout.exp; the default half passes. Suspect InoutConnect/tran
   primitive under verilator. On-disk .bsc-vcomp-out files are misleading — overwritten by
   the passing second half (multi-config .exp reuses output names).
5. INFINITELOOP class (MCD/derived-clock, genuinely spins, fatal lint = correct interlock,
   stays ON): exact list NOT reconstructible from attempt-4 log (bare ./foo.exp names, -j
   interleaving) and the killed run never reached the Sync*/Clock dirs. baseline2 provides
   ground truth via on-disk link outputs.
6. showrules (12F, "showrules terminated abnormally"), portRenaming, fwrite, MacTestBench,
   FloatingPoint, mesa/course_lab, divmod, SquareRoot, BRAM, mcd/* etc. — triage after
   baseline2 (genuine-diff class per handoff).

## Step 2 IMPLEMENTED (2026-07-13 ~02:3x, uncommitted): verinorun + auto-refusal + timeout
Per Ravi's direction ("there was supposed to be a check that looked for output clocks ... and
refused to try verilator ... another check for the inout stuff"): the mechanism is DETECTION-
FIRST (auto-refusal at link time), with manual spans only for oddballs. Verified facts feeding
the predicate:
- ClockGen: 109 baseline .v files, ZERO sim passes (forever loop; INFINITELOOP lint) → refuse.
- ClockInverter/GatedClockInverter: no ClockGen-free samples exist; derived-edge generators;
  refusing costs zero green tests today → refuse.
- InoutConnect: verilator rejects as %Error-UNSUPPORTED "complex ports (IEEE 1800 23.2.2)"
  (repro in scratchpad/inout-repro) → refuse. Covers the whole bsc.verilog/inout first half.
- ClockDiv/GatedClock/MakeClock/ClockMux/ClockSelect: measurably mostly-green (3 pure-ClockDiv
  passes incl. sysBRAMLoadTest; Gating dir 172/1) → NOT refused; runtime spinners get spans +
  the timeout backstop. sysOutputReset* are compile-only tests (never simulated) — resets stay
  diff-class, NOT refuse-class.
- RUNTIME SPINNER class exists (passes lint, spins in sim): sysClockDivOffset spun 31 min at
  91% CPU in BOTH ClockDividers dirs (Ravi caught it; run killed). Lint is not a complete
  interlock → hence the timeout backstop for unknowns + spans for knowns.
Implementation (testsuite/config/unix.exp + verilog.tcl + 3 .exp spans):
- unix.exp: verinorun/verinorun_end/verinorun_active/verinorun_why/verinorun_unsupported +
  verinorun_known_sims validation (typo'd sim = perror, never silent) + verinorun_auto_clear;
  verilator_unsupported_mods list (the 4 prims + rationale comments); vsim_design_unsupported
  (closure scan: .v files at link + toplevel.v + locally-generated submodules via one-pass
  \m(alternation)\M reference walk; lib prims matched by instantiation regex, never read);
  verinorun_auto_check (cd-safe wrapper); vsim_run_timeout_prefix ("timeout -k 10 ${VSIM_RUN_
  TIMEOUT:-300} " under verilator only, auto_execok-guarded for macOS); compare_file
  missing-file net; check_verilog_output guard; test_c_veri_worker_int whole-tail wrap (link
  proc emits its own marker; worker emits sim/compare/vcd markers; Bluesim half untouched;
  -elab .ba feed preserved); bsc_init/bsc_finish per-.exp state clear (tool=bsc via site.exp;
  verified against /usr/share/dejagnu/runtest.exp:1615/1652 — runs even after Tcl error).
- verilog.tcl: link_verilog_pass/_fail/_no_main_pass = auto_clear → auto_check → armed?
  UNSUPPORTED+return (guard before incr_stat); sim_verilog/_vcd/_status guards;
  sim_verilog_int cmd prefixed with vsim_run_timeout_prefix.
- Spans: battery.exp srandom (TestActionValues only — UnusedValue has no srandom.ba);
  ClockDividers.exp ×2 sysClockDivOffset.
- UNSUPPORTED audit: grep -rn '^verinorun ' for sites; ^UNSUPPORTED: in sums at runtime;
  incr_stat "verinorun" in the distribution summary. Suite monitors must tally UNSUPPORTED.
- Baseline2 FINAL CATALOG (killed at the ClockDividers spinners, everything else complete):
  22,748 PASS / 709 FAIL / 0 ERROR across 883 sums. Sits on disk for the triage fan-out.

## Validation round 1 (verilator) + predicate extension
- battery 87P/0F/4U (srandom span ✓, was 87/4F); Synchronizers 123P/0F/52U (ClockGen
  auto-refusal converted every FAIL ✓); ClockDividers 95P/5F/48U; inout 106P/31F/64U.
- UNSUPPORTED text verified, e.g. "UNSUPPORTED: `sysClockDiv.v' link to executable
  `sysClockDiv': not run under verilator: instantiates ClockGen: derived-clock generator
  needs --timing".
- TIMEOUT BACKSTOP PROVED LIVE: sysGatedClockDiv (2nd runtime spinner) killed at 300s ×2
  and the run moved on — baseline2 would have hung. Span added ×2 dirs.
- PREDICATE EXTENSION (round-1 lesson): bsc emits inout aliasing as module-HEADER named
  port expressions bound to one net (e.g. sysArgToIfc.v "module sysArgToIfc(CLK, RST_N,
  .i_in(i_in), .i_out(i_in));") — verilator "complex ports" — in GENERATED design files,
  not only via the InoutConnect primitive. ManyLineConnect* escaped the primitive check
  this way (their submodule sysArgToIfc carries the alias header; closure walk reached it,
  predicate didn't look). Added header-alias check to vsim_design_unsupported.
- Also surgeries landed: options.exp VPI section verinorun-guarded (1 section UNSUPPORTED
  under verilator; runs everywhere else); options.exp -q filtered-compare → veribug-XFAIL
  keyed {verilator} (the unplumbed-quiet bsc gap); depend.exp ×3 + b1758 + signal_names
  check_ats(FFunc) pinned to -no-use-dpi (today's behavior under every non-verilator sim).
- Residual known diff-class (to triage fan-out): sysResetInv ×2 (reset-inverter golden),
  sysRegEnConnect{,2} (inout z-resolution 2-state divergence — sims run, output differs).

## CI INTEGRATION IMPLEMENTED (per Ravi "figure out how to add verilator to CI in a
## sensible fashion" + workflow wf_7f8228a0-876: 6 agents, 474k tok; full synthesis in
## rc4-verilator-ci-design.md (repo root) — read it before touching this design)
Design = staged-rollout (judged 8.5 vs gate-first 7.5, coverage-econ 6.5). Investigation
ground truth: ALL 4 prior single-job hosted-runner attempts died (2h32m-3h37m); macOS queue
starvation observed at 24h; pinned verilator tag predates the 787ac6d python fix (pyshim is
LOAD-BEARING); PR-1004 continue-on-error = the hollow-green anti-pattern to never repeat.
Implemented (uncommitted):
- testsuite/scripts/filter-testdirs.pl — TESTDIRS dir-prefix filter for the test list;
  pass-through no-op when unset. (suitemake.mk seam NOT yet edited — deferred until the
  triage fan-out's makes finish; needs: insert '| perl $(CONFDIR)/scripts/filter-testdirs.pl'
  after sort-by-time.pl in BOTH run-tests-setup pipelines + 'export TESTDIRS'.)
- testsuite/scripts/ci-vsim-verdict.sh — collect|seed|judge. judge grades shard fragments
  against committed manifests: completeness (missing fragment = red), coverage (union of
  shard schedules vs in-tree .exp = red on holes), NEW fails vs fails manifest (red in
  ratchet/gate), stale entries (warn in ratchet, red in gate), UNSUPPORTED set drift (red
  both directions), flaky-list excusal, always-published ledger. FIXTURE-TESTED: all 10
  red/green directions asserted (scratchpad/verdict-fixture).
- testsuite/ci-manifests/verilator/shards.txt — 7 shards, REST computed (new dirs can't
  drop). PENDING after baseline3: timing-ci.txt snapshot + seed fails/unsupported/flaky
  manifests (ci-vsim-verdict.sh seed testsuite testsuite/ci-manifests/verilator).
- build-and-test-ubuntu.yml: input matx_verilator_gating(bool) → matx_verilator_mode
  (string, default 'off'; ci.yml verified not to pass it); the 81a91b38 single gating job
  REPLACED by test-ubuntu-verilator-matx-shard (matrix 1-7, fail-fast:false, 120min,
  pin-assert step, ccache restore/save-always split per shard, 8G swap, LPT via committed
  timing-ci.txt, VSIM_RUN_TIMEOUT=300, make exit recorded-not-gating, collect+schedule
  artifacts) + verilator-matx-verdict (if:always(), judges fragments). Informational
  verilator job got its Archive-test-logs step (the dangling steps.testsuite.outcome nit).
- build-and-test-macos.yml: matx gating job + input REMOVED (deployed sim is linux-x86_64;
  3-core/7GB OOM; queue starvation) — comment left in place.
- matx-release.yml: dispatch input verilator_mode (default ratchet; push-trigger uses
  committed default via '|| ratchet'); macos call no longer passes the flag.
- testsuite/README.md: "Simulator-specific behavior" section documenting auto-refusal,
  verinorun, sim-keyed goldens, veribug sim-lists, VSIM_RUN_TIMEOUT.
- All 4 workflow YAMLs parse (python yaml.safe_load).
Rollout per judge: Stage 0 commit skip layer + baseline3 seeds manifests; Stage 1 push +
observe-mode shakedown dispatch (~1.5h, first-ever hosted-runner completion proof);
reconcile manifests; Stage 2 release dispatch at ratchet → matx-prerelease. Promotion
ratchet→gate = 1-line default bump when fails manifest empties. Fallback = rc3
iverilog-green standard + observe ledger, never continue-on-error.

## TRIAGE FAN-OUT COMPLETE (wf_66d3d40c-235: 36 agents, 3.76M tok, 2060 tool calls, 82 min)
22/23 groups returned, ALL at goal state: verilator FAIL=0 (everything PASS/UNSUPPORTED/XFAIL),
iverilog byte-parity green re-proven per dir. 61 sim-keyed goldens (61 BENIGN_CONFIRMED by
adversarial re-diff) + 25 spans/veribugs. lib-BRAM agent died without returning (its dirs show
verilator-last sums 03:09 — re-validating both sims now). Full result: task output
wd75mh2dq / workflow journal. KEY FINDINGS:
- **REAL VERILATOR 5.048 BUG (adversarially CONFIRMED, minimal repro in scratchpad/triage/
  f1-signed/)**: $signed(part-select) of a runtime-written reg under $display %d prints the
  WHOLE reg unmasked (16'h4D58: $signed(z[7:0]) → 19800, not 88). Upstream regression from
  the #7280 display-arg rework, NOT MatX-specific; version-matrix'd 5.032-good/5.048-bad.
  Affects splitports (3 tests XFAIL-keyed) + divmod (veribug'd). **DEPLOYMENT INTEL: the
  MatX-deployed verilator prints wrong $signed part-select values — report upstream +
  consider for the next MatX verilator build.**
- **inout residual class root-caused (NOT z-resolution)**: bsc sequences each module's
  system tasks via 'always@(negedge CLK) begin #0; ...; $finish; end'; under --no-timing
  the #0 is inert (STMTDLY lint_off in shipped .vlt), so hand-written negedge blocks
  statically order AFTER the finish block; the harness's clean_verilator_output cut-at-
  $finish then truncates the last (correct) line. Goldens = expected minus post-marker
  line; sysTbBoth's 36 identical lines prove verilator resolves the 3-driver z-bus
  CORRECTLY. b898 same family (its golden reviewer refuted the agent's 'terminates first'
  claim — it's the harness cut policy, output values all correct).
- **bsc follow-up PR candidates catalogued (post-rc4, all need rebuild)**: (1) -q not
  plumbed vSimLink→build scripts; (2) sim_main.cpp missing Verilated::traceEverOn(true)
  (design-initiated $dump* aborts; bsc.bluesim/vcd + sysDumpOnOff class); (3) sim_main.cpp
  dumps BEFORE eval (VCD posedge values lag; showrules class); (4) VCD.hs fmtUnit/
  readTimeUnit timescale table mis-keyed (no 15→fs; ps-VCD crashes showrules).
- **Stale-partial-install hazard found by showrules group**: inst/bin mixed 607f5b00-era
  utils with 81c36a7a-era bsc (the 1027 VCD.hs changes staled showrules → its crashes were
  ENVIRONMENT, not verilator). Agent rebuilt showrules; dumpba/dumpbo/bsc2bsv staleness
  falsified as behavior-affecting. Lesson: full install-src before final suites.
- **clean-eats-goldens hazard (reviewer-confirmed URGENT) FIXED GLOBALLY**: cleanonly.mk
  filter-out now protects %.verilator (agents' 30 per-dir KEEPFILES workarounds all
  REVERTED; dry-run verified). Goldens are UNTRACKED until commit — do not clean dirs
  outside make (the protection only covers the harness clean).
- Critic: baseline2 was killed pre-tail, so dirs queued behind the ClockDividers hang were
  never catalogued (bsc.syntax/bh, PAClib, b1424, shifter, CReg, mcd/NoClock, relax-schedule,
  conflict_free, interra/operators/Arith, pack-unpack, verilog.exp...). baseline1's 317-fail
  log bounds the class (~227 fails, pre--use-dpi so many are already-fixed classes).
  baseline3 disposes of these. Critic's other flags checked: getput/vector_modargs ARE
  covered (misread); Synchronizers "4 fails" = stale 07-12 other-session artifact.
- MPEG4: its 1 baseline fail = the 300s timeout killing a LEGIT long sim; resolved via
  per-dir VSIM_RUN_TIMEOUT in its Makefile (long_tests are outside CI shards anyway).
- 64 .expected.verilator on disk = 61 goldens + 2 bug-expected (veribug-XFAIL variants,
  tasks dirs) + 1 MPEG4 .expected.sorted.verilator.

## BASELINE3 (2026-07-13 ~04:5x): **VERILATOR SUITE FULLY GREEN**
- Full suite, -j24, all mechanisms live: **22,919 PASS / 2 FAIL / 634 UNSUPPORTED /
  138 XFAIL / 0 ERROR** (883 dirs, log rc4-verilator-baseline3.log). The critic's feared
  uncatalogued-tail fails (syntax/bh, PAClib, b1424, shifter, CReg...) ALL absorbed by the
  fix stack — zero fails in the entire tail.
- The 2 fails: bsc.options/verilog-e sim-echo tests (goldens record the echoed link
  command line; implied -use-dpi adds `-dpi` to the echo). Fixed with per-test -no-use-dpi
  (echo isn't a real sim; pin is safe under every sim) → re-validated 15P/0F/0U under BOTH
  sims. Final verilator state: **22,921 PASS / 0 FAIL / 634 UNSUPPORTED**.
- CI manifests SEEDED from this state: fails-linux-x86_64.txt **EMPTY** (locally
  gate-eligible content!), unsupported-linux-x86_64.txt = 634 reasoned lines (the skip
  contract), flaky empty, timing-ci.txt = fresh 881-line LPT snapshot.
- iverilog full parity suite launched (rc4-iverilog-parity.log) — must be 0 FAIL /
  0 UNSUPPORTED to prove the whole burn-down invisible under the default sim.

## In flight
- baseline2: full suite, -j24, harness fix in => log rc4-verilator-baseline2.log,
  monitor bkgnxsb1q. Started ~01:31. (Baseline1 partial for comparison: 16,365P/317F
  at kill, and that was WITHOUT the -use-dpi fix.)
- Workflow wf_95482027-903: 3-design judge panel for the sim-keyed skip-not-run
  mechanism (requirements in scratchpad/skip-design-brief.md) + adversarial review of the
  -use-dpi diff + bsc.options triage. Skip mechanism lands AFTER baseline2 (never edit
  harness mid-run).

## Workflow wf_95482027-903 results (6 agents, 475k tok — designs+reviews COMPLETE)
- **Skip mechanism decided: `verinorun SIMS REASON` ... `verinorun_end`** (ambient bracket,
  veribug-style sim keying, DejaGnu UNSUPPORTED per suppressed check, house style).
  Winner minimal-extension 8.5 > sim-capability 8 > declarative-guard 7.5; synthesis grafts:
  sim-name validation (typo'd sim = perror, never a silently-inert guard), compare_file
  missing-file net (raw sites need zero conversion), bsc_init/bsc_finish per-.exp auto-clear
  (DejaGnu tool hooks, unused today), CI UNSUPPORTED-count audit. Full synthesis with exact
  Tcl + hook lines: rc4-verilator-verinorun-design.md (repo root). Hooks: unix.exp (core procs above
  check_verilog_output; guard in check_verilog_output; net in compare_file; whole-tail wrap
  of test_c_veri_worker_int's verilog arm AFTER compile — -elab .ba feed preserved),
  verilog.tcl (link_verilog_pass/fail, link_verilog_no_main_pass, sim_verilog{,_vcd,_status}).
  Capability-table design REJECTED for now (premature taxonomy; can migrate later).
- **-use-dpi diff review: 4 HIGH (all = VPI-expectation tests that now fail under verilator,
  fix at the .exp/test level, mechanism itself sound):**
  H1 bsc.driver/depend/depend.exp asserts "VPI wrapper files created:" chatter (×2);
  H2 bsc.names/signal_names FFunc check_ats golden contains that line;
  H3 b1758 find_regexp requires $imported_my_time VPI call syntax in the .v;
  H4 bsc.options VPI section (:128-192) artifact checks.
  Compile-only ones (H1-H3, options :154/:171) → per-test -no-use-dpi (Toggle verified;
  later-wins; a no-op under iverilog since useDPI defaults False = pins today's behavior).
  Link-stage ones (options :183/:184/:191/:192) → VPI simply doesn't exist under verilator
  → section-level skip via verinorun_active guard (one audited UNSUPPORTED for the section).
  MED: timestamp-only rebuild logic reuses opposite-flavor .v when switching sims in an
  uncleaned tree — reinforces always-clean-per-dir; multi-vsim plan must special-case BDPI.
  LOW ×2 (uncovered BDPI-irrelevant procs — latent, no current tests; string match arg
  order = house style).
- **bsc.options triage (rc4-verilator-bsc-options-triage.md (repo root)): the 7 fails fully explained.**
  Monomorphic DPI imports produce NO wrapper artifacts (decl inline in .v); only polymorphic
  imports get dpi_wrapper_<fn>.c (same vdir placement rule). VPI-section links (:183/:191)
  only ever linked because iverilog's shared-object VPI tolerates the undefined my_time
  symbol until runtime; verilator's static DPI link correctly requires the C impl. **GENUINE
  BSC GAP: -q/-quiet never plumbed vSimLink → bsc_build_vsim_verilator (only -verbose is)**
  → options.exp:363 empty-log check can't pass under verilator. Decision: verilator-keyed
  XFAIL (compare_file_bug) w/ comment, NOT a sed filter (behavior is genuinely deficient);
  upstream bsc fix = follow-up PR candidate, NOT rc4 (keeps burn-down harness-only, no
  rebuild, iverilog suite proof stays valid).

## PROPOSED COMMIT STRUCTURE (nothing committed — Ravi's approval required)
Inventory: 43 modified + ~69 new files (git status; rc3-*/rc4-*.log/md journal artifacts and
untracked-bsc.* quarantines excluded as before; bsc.options/remap-path/dir{A,B,C} are
pre-existing 1040-test run artifacts, not part of this batch).
1. "testsuite: sim-keyed run/skip machinery + verilator support" — config/unix.exp,
   config/verilog.tcl, cleanonly.mk, suitemake.mk, scripts/filter-testdirs.pl,
   README.md. (The core: implied -use-dpi, verinorun, auto-refusal, sim-keyed goldens,
   VSIM_RUN_TIMEOUT, TESTDIRS, golden clean-protection.)
2. "testsuite: verilator triage — spans, veribug keys, -no-use-dpi pins, sim-keyed
   goldens" — the 38 .exp edits + 64 .expected*.verilator files.
3. "CI: sharded MatX-verilator leg with manifest-graded verdict" — 3 workflow ymls,
   scripts/ci-vsim-verdict.sh, ci-manifests/verilator/*.
4. "rc4: burn-down journal + handoff update" — rc4-verilator-burndown.md,
   HANDOFF-release-2026.07.rc4.md.
Upstream-PR potential: commits 1-2 are clean upstream candidates (B-Lang-org) after rc4;
commit 3 is MatX-specific except filter-testdirs/TESTDIRS (upstreamable separately).

## Gotchas honored
- One full suite at a time; per-dir localcheck (clean first) for revalidation.
- .vlt suppressions forbidden (reverted before; fatal lint is the enforcement).
- grep -r --include misbehaves in this env; use find|xargs grep.
- awk: `exp` is a reserved builtin — don't use it as a variable name.

## SHIPPED (2026-07-13 ~10:0x local)
- iverilog parity suite GREEN: **23,600 PASS / 0 FAIL / 0 UNSUPPORTED, make exit 0**
  (+11 net additive checks vs the 23,589 pre-burn-down baseline; zero regressions).
  NOTE: two earlier parity attempts died externally (Claude session restart killed the
  first at 11,336/0/0; machine REBOOT killed the second at ~11,917/0/0 and wiped the
  tmpfs verilator install — re-fetch from MatX-inc/verilator when next needed).
- Golden legality audit (Ravi's ordering question): 64 goldens mechanically classified —
  36 additions-only, 6 tail-truncations (cut-at-$finish; values verified by direct vexe
  runs), 20 content-changes (X-init/$random-self-checking/display formatting/time-0
  gating), 1 interior-line (Gating time-0 sample), 1 pure PERMUTATION (interfacecalls
  bug-expected XFAIL variant — legal intra-timestep $display interleave). No unexplained
  value difference goldened.
- COMMITTED as 86c1a713 (machinery) / 4baa7b94 (triage) / fdd965e6 (CI) / 60a046f4 (docs);
  branch at 227 commits; PUSHED to MatX-inc/bsc release-2026.07.rc4 (first push).
- matx-release DISPATCHED on the branch ref with verilator_mode=observe (the shakedown):
  run 29269261906. Next: reconcile CI-vs-local manifest deltas from its ledger, commit,
  then the release dispatch at ratchet → matx-prerelease with the green run id.

## DOWNSTREAM BRANCHES (shakedown finding, 2026-07-13 ~10:5x)
Ravi flagged bsc-contrib/bdw failures in the observe run; investigation showed the branch
match WORKED ("Checking out user's branch" at the recorded hashes d5b6a2c/1cff4e4) — the
failures were CONTENT: goldens on the downstream release branches predate rc4's .ba-chatter
changes (988 -g/-c/-e rework). Two regold classes, reproduced + fixed locally against the
rc4 inst and pushed:
- bsc-contrib @ f4dcea0: TestSched_VectorFIFOF sched-out golden minus the "Elaborated
  module file created" line (full testing tree revalidated: 232 PASS / 0 FAIL).
- bdw @ 3859991: link/simulate_via_verilog goldens minus the S0099 "No elaboration file"
  4-line warning block ×2 (positive_tests revalidated green; the macos leg passed because
  those tests don't run there).
NOTE: these jobs GATE the run conclusion (no continue-on-error), so the observe run will
conclude failure from the pre-fix jobs — expected; the release dispatch picks up the fixed
branches. GOTCHA (contrib/bdw leaf Makefiles): leaf `make check` runs runtest twice; the
second bare invocation overwrites testrun.sum with an empty one when the first was all
green — judge leaf reruns by EXIT CODE, not the sum.
