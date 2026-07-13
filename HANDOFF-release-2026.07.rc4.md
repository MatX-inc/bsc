# HANDOFF: release-2026.07.rc4 (2026-07-13, pre-context-clear)

Companion to rc4-state.md (the full assembly journal). This is the "resume here" document.

## Where the release stands

- **Branch**: release-2026.07.rc4, LOCAL ONLY (never pushed), **223 commits** @ 81a91b38
  on upstream/main 578123c4 (which is still current — upstream unmoved).
- **Content**: 28 PRs (967 DROPPED per Ravi — release only, PR discussion continues; 1027 FST
  included; 925 re-picked from the fingerprint head). Tags bsc-bo-20260713-1 / bsc-ba-20260713-1,
  Flags = 138 fields. All fixes from the two adversarial review programs are committed as 14
  PR-aligned commits (cherry-picks where bases matched: remap ×2, scheduling ×3; same-subject
  mirrors otherwise) + the CI gating commit 81a91b38.
- **Validation**: full suite under iverilog GREEN — 23,589 pass / 0 unexpected / 0 errors
  (binary 607f5b00-era; all regolds diff-verified). Affected bluetcl dirs re-validated green via
  `make localcheck` (the per-dir mechanism; multi-target .group invocations ESCAPE SCOPE — never
  use them).
- **Working tree**: clean except rc4-state.md (journal appends, commit with this handoff),
  rc3-* journal artifacts (rc3-branch history, leave), rc4-gate-all.log + verilator attempt logs,
  two quarantined trees (untracked-bsc.{bdw,contrib}-moved-from-testsuite), and killed-run
  test-copy strays (pong/*.bsv, BoolTest_sat-stp.bsv — regenerated per run, ignorable).

## Release sequence (when resumed)

1. Ravi's remaining decisions (below), then push release-2026.07.rc4 to origin (MatX-inc/bsc).
2. Dispatch matx-release **on the branch ref** (dispatch now branch-matches downstream repos —
   fixed in 81a91b38; push-trigger only fires on main).
3. matx-prerelease with the green run id (rc3 playbook).
4. Downstream release branches ALREADY LIVE: MatX-inc/bsc-contrib and MatX-inc/bdw @
   release-2026.07.rc4 (from nanavati vlink-regen; goldens check bsc COMMAND OUTPUT, not .ba
   bytes, so they're current despite later format changes). Toooba falls back to bluespec/Toooba
   default. bdw + Toooba coverage = upstream CI's ungated jobs in the shared workflows.

## THE OPEN DECISION: verilator gating vs this release

Commit 81a91b38 added GATING MatX-verilator jobs (ubuntu+macos) to matx-release
(matx_verilator_gating input; pinned release build-20260701_1; python3.11 shim included).
**A release run's gate would currently be RED** — the burn-down below is not done. Options:
(a) hold release for burn-down; (b) demote the new job to informational for this release
(one line), gate next; (c) gate on a curated subset. RAVI TO DECIDE.

## Verilator burn-down — **COMPLETE 2026-07-13, see rc4-verilator-burndown.md**

The section below is the pre-burn-down diagnosis, kept for history.  The burn-down
finished with the full suite GREEN under the MatX verilator (22,921 PASS / 0 FAIL /
634 reasoned UNSUPPORTED), a rewritten sharded+manifest-gated CI leg, and an empty
fails manifest (gate-eligible content).  All changes uncommitted, awaiting Ravi.
The gating decision (a)/(b)/(c) below is superseded by the ratchet-mode design —
see the journal's CI section and scratchpad/ci-synthesis.md.

## Verilator burn-down (original diagnosis; superseded)

Baseline attempts 1-6 chronicle (logs rc4-verilator-baseline.attempt*.log at repo root):
OOM at -j128 (use -j24; /tmp worktrees were eating tmpfs — removed) → python3.11 missing
(the MatX verilator release bakes the build host's python; FIXED AT SOURCE: MatX-inc/verilator
787ac6d builds with explicit python3.11 — src/astgen needs >=3.8, the walrus operator — and
normalizes the packaged verilated.mk to generic python3; CI jobs also shim) → INFINITELOOP
fatal lint: **a CORRECT interlock, not noise** (Ravi's insight): the flagged designs (mostly MCD
derived-clock generators needing --timing, which the harness rightly disables) genuinely SPIN
under verilator. Suppression was tried and REVERTED (verilator_config.vlt pristine; .vlt files
accept NO comments — parse-verified changes only). Final partial catalog: 16,365 pass / 317 fail
at kill time.

Remaining work to a green gate:
1. **Imply -use-dpi under verilator** in the harness (vcomp_flags reaches LINK only —
   verilog.tcl:57; the WRAPPER FLAVOR is chosen at COMPILE, so bsc_compile_verilog needs it too;
   BDPI is the one class where sim choice reaches the bsc compile — multi-vsim plan must
   special-case it). This de-fails the whole bsc.codegen/foreign class. SAFE ONLY BECAUSE the
   batch fixed the VMDPI crash (bsc -verilog -use-dpi used to die).
2. **Skip-not-run interlocks for the MCD/timing class**: NO such mechanism exists today
   (verified: zero unsupported/skip machinery in the harness; veribug = XFAIL-after-running).
   Ravi half-remembered one — it was the 990-995 enablement work (merged, in base: b925 XFAIL,
   $display, BRAM0Test, MPEG4 fixes) + the veribug sim-lists, which are ~all IVERILOG-keyed.
   The verilator quirk list was never written (informational CI legs made it nobody's problem —
   PR 1004's own run: 22.04/4.038 leg hollow-green via step continue-on-error, all 5.x legs RED
   and ignored). The ~60 INFINITELOOP sites in rc4-verilator-baseline.attempt4-infiniteloop.log
   enumerate the skip list; leave the fatal lint ON as enforcement.
3. **Triage genuine output diffs** (SquareRoot, FP/FloatingPoint, BRAM loads, Gating cycle
   counts, fwrite, MacTestBench, mesa, showrules, portRenaming...): benign 2-state/X divergence
   → veribug or sim-specific golden; suspicious → fix (the 990s prove verilator finds real bsc
   bugs). Starred anomalies: bsc.options + bsc.bluesim/vcd failed (sim-independent dirs —
   likely load flakes); fst_correlation (our own recent work); parameters/string (997 territory —
   fix is per-test -system-verilog-output, NEVER implied globally: golden avalanche).
4. Rerun baseline at -j24 with the shim → iterate to green → flip/keep gate.

## Other pending decisions (unchanged)

- Optional pre-push residue: doc build (make install-doc), perf smoke vs rc3 (ATF cache +
  fingerprint content deserve it; suite provably can't see perf).
- Oracle-checked history reassembly (optional cosmetics, post-suite method documented in journal).
- 988 BDPI header option already implemented upstream (inline decls); 965 cherry-pick offer open.
- bsc.bdw/bsc.contrib quarantined trees: final disposition.

## Upstream/downstream state (all synced)

PRs updated this cycle: 955 @a81d445e (+ issue #1042), 956 @50578062, 988 @4975c60b,
1040 @8fa87d4a, 1023 @c28babff, 919 @b6893d68 (rebased by parallel session, delete-wins taken,
attribution re-applied), PR #1043 OPENED (scheduling+guards @bd04de4c). 965: fix branch
nanavati:965-via-target-shape @bd91fcf1 + comment offer. 998/999: VMDPI cross-PR comments.
Attribution sweep: 10 branches force-pushed Ravi-as-author (patch-id verified); Greg's 988
commit + Lucas's 1028 commits deliberately untouched. MatX-inc/verilator @787ac6d (python fix).
MatX-inc/{bsc-contrib,bdw} @release-2026.07.rc4.

## Session gotchas (verified the hard way)

- Background "exit 0" notifications lie for compound commands — grep the LOG for the real exit.
- pkill -f self-matches your own compound (use runtime-constructed patterns or TaskStop).
- .vlt config files: no comments; parse-verify (verilator --lint-only cfg t.v) before a run.
- make multi-target .group escapes scope; `make localcheck` per dir is correct.
- Hand-typed lease hashes: never (ls-remote only). git add -A in dirty test dirs: never
  (explicit file lists). Worktrees in /tmp are tmpfs: remove builds after pushing.
- -j for verilator suites: 24 (per-test g++ memory).
