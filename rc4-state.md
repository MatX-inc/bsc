# release-2026.07.rc4 assembly state

- Checkout: /home/ravi/bluespec/matx/bsc (MAIN checkout), branch release-2026.07.rc4
- Base: upstream/main 578123c4 (d2f996c0 + 38 commits — the 2026-07-10/11 MERGE SPREE)
- Scope per Ravi 2026-07-11: the 29 surviving release PRs + #1026 (tree-sitter Classic). #1027 (FST Bluesim) HELD OUT
  (design-soundness investigation running separately).
- PR heads in refs/rcpr/<N>; merged-dep heads in refs/rcpr/dep<N> (923 916 1032 1007 1006 995 849 959 983 987 1001).

## Merge-spree context (what changed vs rc3)
22 of rc3's 51 PRs merged upstream 2026-07-10/11 (mostly SQUASH merges → git cherry canNOT dedup stacked
copies): 849 916 923 926 929 933 936 954 959 983 986 987 990 991 993 994 995 1001 1006 1007 1019 1032.
Upstream also absorbed two former rc integration fixes: TupleSize proviso (578123c4), tuple-mux distribute
(366d2186). Upstream b1490 caps unified at 288M (08beeb08) — combined tree still needs 320M (rc3 CI lesson).
987 and three 1008-branch commits (9f56c78e/a3a0f600/05777ea9) rebase-merged patch-identically (cherry DOES dedup those).

## Curated pick plan (~176 commits; git cherry vs HEAD unless noted)
A: 967 = full range 578123c4..75dd4b5b (5 commits — krame505 rebased onto 578123c4 on 2026-07-11/12;
   head was CHANGING during setup: 75dd4b5b→96d31f08→75dd4b5b; PINNED 75dd4b5b, re-verified via ls-remote.
   WATCH: mesa -no-aggressive-conditions pragma drop is in 7d0a902f "Make PrimPair a struct" — rc3 deviation
   e5a9005f says RESTORE pragma on mkMesaTxLpm0).
B: 919 (13, head ac00cca8 MOVED since rc3 — updated 2026-07-11), 925-own = 38a539e4..559ed999 (20; prefix
   through 38a539e4 is 923-content, upstream squash 5c5ebb12; gate-925 will flag any squash omissions).
C: 955 (7), 957 (18), 1028-own = faaf1bc4..eb08b723 (3: 95cce711 Unify expandSynATF, 4560905b MakeSymTab,
   eb08b723 review; prefix 9 commits = old-916, upstream squash 6be62d63).
D: 944 (1), 945 (2), 965 (5).
E: 997 (1), 998 (7), 999 (2), 1023-range (3, contains 1022's 2), 1020 (2).
F: 988 (cherry +13 of 18; 987's 5 dedup automatically), 996 (1), 1000 (5),
   1008 = cherry +22 MINUS ba219c5d (MPEG4 fix = upstream 6f871dc7, patch-id drift; 0f26e611 off-by-one and
   e07b1daa testsuite-update are NOT upstream → keep) = 21 commits, head b7a68dfa (bugfix stack + pack-unpack
   test move — commits b950127d/3d029676/d3512166/aa2010d8 + tests + b7a68dfa),
   1030 (4, unchanged head 0d7e209e).
G: 956 (16, head e649f058 = rc3 value), 969 (3, head 91656526 = rc3 value).
I: batch-6 = d38778e0..b8f8aea5 (24: skips first 4 = 1032-copies, upstream squash 71226f07; covers
   1033(4)+1035(7)+1036(2)+1037(5)+1038-own(6); ancestor chain 1035⊆1036⊆1037⊆1038 verified), then 1034 (1).
J: 1004 = ONLY 07e2d763, 1f1bf3a0, a528c457 (CI-unique; b234195b/29652dbc/8a2b0383/8f47b241/52b8fbce are
   stale 1001-queue copies — 1001's squash 8458d046 verified to contain ALL follow-ups incl. filter/renamefire.pl).
K: 1026 (1 commit 6c28669f, purely additive util/tree-sitter-bluespec/, 219k lines, zero conflict risk).

## Standing decisions carried from rc3 (see rc3-state.md for full history)
- GenBin/GenABin tag conflicts: always OURS mid-assembly; final canonical bump at end → bsc-bo-20260711-1 / bsc-ba-20260711-1.
- Error tags: check collisions across ALL PRs at end (rc3: 1023's T0159/T0160 → T0162/T0163 vs batch-6;
  upstream may have consumed more since — RE-DERIVE against 578123c4's Error.hs; 1020 G0129/G0130, 998 trio
  G0131-G0133 + svkeywords.exp).
- mesa mkMesaTxLpm0 keeps (* options="-no-aggressive-conditions" *).
- b1490 cap: 320M for the combined tree (rc3 CI ground truth; upstream 288M insufficient).
- Bin Flags: after ANY Flags record change (988 -c mode adds fields), run rc4-genflags.py (copy of rc3-genflags.py)
  and verify all four sites = N. NEVER let indexMaybe-style Maybe byte-readers inline into the Flags reader.
- Conflict hygiene: check ALL THREE marker forms (<<<<<<< ======= >>>>>>>) before EVERY git add; never
  regex-resolve multi-arm case hunks (splice from owning head); verify Edit landed before staging.
- rerere: review every fill (rc3: stale fills lied).
- derived_bits.exp: 1008-final state wins.

## Build/test recipe
- Closure typecheck first: ghc -fno-code, -iGHC/posix, stub BuildVersion.hs/BuildSystem.hs then DELETE;
  typecheck BOTH bsc.hs and bluetcl.hs closures.
- Build: make -j32 GHCJOBS=16 GHCRTSFLAGS="+RTS -M16G -A128m -RTS" install-src (repo root). Watch GenABin memory.
- Testsuite: make -j128 -C testsuite fullparallel, inst/bin PATH, SystemC env vars, 2-min testrun.sum monitor.
  ZERO unexpected FAIL. rc3 drift classes for triage: CSE tie-order naming, bluetcl void entries, richer
  batch-6 diagnostics, splitports w→w_1/w_2.

## Progress
- [x] branch + 30 heads fetched + containment matrix (2026-07-11)
- [x] picks A..K — ALL 30 PRs, 181 commits (Groups G-K: 956/969/1034/1004×3/1026 clean; batch-6 range 24
      with rerere fills verified at each step — SolvedBinds carried all four 923/925 fields + extractClosures;
      TCheck pool weave = warnTI×2/pool_sbs×4; Unify kept asATFAp guard; bsc.hs import = union incl.
      mkSymTabWithWarnings + mergeCATFCaches)
- [x] full-tree three-form marker sweep → caught ONE stray '>>>>>>>' terminator in SolvedBinds.hs
      (rc3 build-1 class) → fixed pre-build in 4804c375
- [x] integration fixes (186 commits total):
      * T0159/T0160 collision (1023 vs batch-6) → EForeignWidthNotBare=T0162, EForeignCtxNotNumeric=T0163,
        noinline.exp updated (systematic Error.hs dup scan vs base: these were the ONLY new collisions)
      * Bluesim -dump-formats guard: simLink rejects fst/fsdb loudly (EGeneric/S0015) — closes the PR-1000
        silent-garbage trap found by the 1027 investigation
      * genflags verified: Flags record 136 = GenABin arity 136
      * b1490: MyBool at 320M via 925's own rts_flags form ✓
      * MatX workflows copied from rc3 branch (matx-release/matx-prerelease/build-and-test×2/ci.yml)
      * canonical tags → bsc-bo-20260711-1 / bsc-ba-20260711-1
- [ ] gate all 30 (rc4-gate.sh, running)
- [ ] closure typecheck (running) + build
- [ ] full testsuite
- [ ] report; push ONLY with Ravi's explicit go
NOTE for build: SolvedBinds fill had renameBinds ABSENT (rc3 build-6 added it for record-construction
completeness) — if the build fails there, extend renameBinds/extractClosures per rc3's fix.

## Closure-typecheck iterations (2026-07-11/12) — clean at round 3, BEFORE any full build
- Round 1: (a) AVerilogUtil ips'/es' out of scope (fill from superseded 1023 intermediate → iports++oports,
  rc3 build-1(c) twin); (b) TCMisc parse error = DUPLICATED incoherent Reduction arm (correct pool-weave copy
  at 14-sp indent + stale orphan at 12-sp; rc3 build-2 twin). Repair: excise stale copy, graft its 923/925
  payload (DirectIncoherence + WIncoherentMatch twarn) into the surviving arm's success case, re-anchor
  bad_match. PYTHON GOTCHA: str.index on an indentation-prefixed marker substring-matches inside DEEPER-
  indented lines — anchor markers with a leading \n. (First attempt wiped both arms; restored from HEAD.)
- Round 2: bluetcl clean; bsc 12 errors, all rc3's arity-drift class: VDPI 3→5-field ×3 sites (999),
  VAParameter 3→4 (997), vi_inst_params Either→plain-named-list ×4 sites (1023 final), IRefT 3→4 ×2
  (getBuriedPreds mkRef, pushBNot' key — rc3 build-3 twin).
- Round 3: bsc clean. Committed as the weave-repair commit (187 total).
- Gate: PR 1026 line-gate is O(n²) on its 219k-line generated parser — SKIPPED in gate, verified instead by
  byte-equality of all 15 merge-base-diff files vs refs/rcpr/1026 (OK=15). SHELL GOTCHA: pkill -f <script>
  matches your own compound command's cmdline — run it standalone.
- Full build launched (rc4-build.log); gate remainder (1028 1030 969 1033-1038) running.

## Build iteration log (rc4 — compare rc3's 7)
- Build 1: compiler + bluetcl + GenABin all compiled clean; FAILED only at bo2bloogle (969's tool, its own
  main = outside both typechecked closures): [Word8] vs 936's ByteString readBinFile (rc3 build-5 twin).
  One-line fix, commit 188.
- Build 2: everything installed; FAILED at first Prelude compile (RUNTIME): SolvedBinds extractClosures
  record construction missing incoherentIds (rc3 build-6 twin — predicted by the journal note). Fix:
  renameBinds maps all four 923/925 fields' keys through the rename; extractClosures restricts
  bindClasses/bindTypes to collected ids, incoherence fields empty by construction. Commit 189.
  LESSON (rc4 addition): grep -Wmissing-fields warnings out of the build log BEFORE the stdlib phase;
  better, add closure typecheck with -Wmissing-fields -Werror=missing-fields for these weave files.
- Build 3: launched.

## Gate results (line gate + byte check) — COMPLETE
9/29 GATE OK (967 944 945 965 956 957 996 969 1034), 1026 byte-verified (15/15 files identical),
20 GATE FAIL with 517 miss lines → adversarial workflow wf_42c404a8-01f (20 agents, 939k tokens):
**19/20 all_superseded with per-line citations; 1 REAL LOSS (PR 1008)**: the 925-pick THEIRS b1490.exp kept
925's stale "VsortOriginal regressed" negative test, but the PrimBNot memoization (upstream 136f3386) fixed
that regression — restored upstream's positive block (kept 925's rts_flags + MyBool 320M). Runtime-validated:
VsortOriginal PASSES in the full suite.

## SUITE FINAL (2026-07-12): 21,160 PASS / 134 XFAIL / 0 XPASS — ALL 15 FAILs triaged & fixed, re-run 636/0
- The 15 tracked FAILs were EXACTLY rc3's four drift classes (all re-verified by diff before regolding):
  class 1 naming (lc-sysTb, CTX_sysTb, b302 — b302 def-count parity verified 17/3=17/3);
  class 2 bluetcl zero-width void/PrimUnit entries (wiretypemap/splitports/clocks ×bsv,bh);
  class 3 richer diagnostics (noinline Arg/ResNotInBits T0043 3→2 + T0032 chain; primtcons VectorIfc T0043 →2);
  class 4 splitports SplitArgSlice direct assigns (adopted rc3's validated exp patterns verbatim).
- ALL 23 ERRORs + zero tracked content: foreign UNTRACKED testsuite/bsc.bdw (leftover in this checkout;
  moved aside to ./untracked-bsc.bdw-moved-from-testsuite — restore or delete at Ravi's pleasure).
- Re-run of all 7 affected dirs after per-dir clean: 636 PASS / 0 FAIL (no-clean rerun artifact noted:
  cached .bo suppresses compile chatter → false golden diffs; ALWAYS per-dir clean before re-runs).
- Final: 195 commits (194 + this journal), branch release-2026.07.rc4, binary d47246ef-era (rebuilt each fix).
- STATUS: ASSEMBLY COMPLETE, SUITE CLEAN. NOT PUSHED — awaiting Ravi's explicit go
  (push to origin → matx-release.yml → matx-prerelease.yml, per rc3's sequence).

## LATE ADDITIONS (2026-07-11/12, per Ravi) — rc4 now 206 commits
1. **Testsuite speedup** (commit 196 = branch nanavati:testsuite-time-ordered-parallel @253bf12a, NO PR yet):
   LPT time-ordered scheduling fixed (tool?=bsc, timing.txt refresh+merge, sort-by-time.pl graceful fallback,
   README recipe). CXXFLAGS=-O0 half already active in the MatX workflows. Work recovered from the
   UNCOMMITTED tree of /home/ravi/bluespec/upstream/bsc (memory: -home-ravi-bluespec-upstream-bsc/
   testsuite-speedup-levers.md).
2. **-remap-path-prefix determinism fix** (commit 197 = 42b968cf; upstream **PR #1040** from
   nanavati:remap-path-prefix @772993f6): audit found .bo nondeterminism = paths ONLY (positions carry pwd
   even for relative invocations; cascades transitively via ipkg_depends hashes; .ba adds abmi_path/src_name
   + Flags paths; NO timestamps/other machine bytes — Ravi's "confirm no others" satisfied, documented in the
   PR). Design: serialization-time remap at BinData's share-P chokepoint + .ba raw strings + stored-Flags
   normalization INCLUDING clearing remapPathPrefix itself (its FROM values are machine-specific!).
   Verified: byte-identical .bo/.ba across dirs, residual-free, repeatable, zero-flag byte-compat with the
   old compiler. Test: testsuite/bsc.options/remap-path. Flags 136→137, .ba tag → bsc-ba-20260712-1.
   GOTCHA: dev bsc binary needs LD_LIBRARY_PATH=inst/lib/SAT; worktree typecheck needs the MAIN checkout's
   generated vendor include_hs + real BuildVersion/BuildSystem.hs (stubs lack getBinFmtType).
3. **PR 1027 FST included + cleaned** (commits 198-206): picks 7a121215/911355b8/9ffd4e43/c0822bb7/6da4ca9d/
   ac7512ae + REVERT of the interim d922a07a guard (superseded by 1027's real fst support + fsdb rejection);
   skipped its bundled 1000-copies, 956-rework (rc4's 956 is a newer superset), d750379b (rc4 fixed same line).
   Conflicts as scoped: Makefile UTILEXES union, SimFileUtils codeGenOptionDescr union (top + no-wave-dump).
   Cleanup commits (also pushed to nanavati:bluesim-fst @259af1c2 + PR description written via REST API —
   gh pr edit hits a projects-classic GraphQL bug): dead previous_files path deleted; sim vcd/fst queries
   format-aware via bk_get_waveform_format (VCD-only fallback for old models).
   libfst submodule initialized locally; build now needs `make -C src/comp install-extra` (fstscopes/fstcheck)
   — CI covered by ac7512ae's edits to the shared build-and-test workflows (submodules:true already).
4. Final rebuild + FULL SUITE RERUN pending (dogfooding RUN_TESTCASES_IN_ORDER_OF_TIME=1 CXXFLAGS=-O0).

## UNREVIEWED-PR REVIEW PROGRAM (2026-07-12, 6 agents over 18 PRs; full reports in session transcripts)
Review-status ground truth: 967/919/965/998/956/957/988/1004/955/1028 have upstream review activity; the rest had ZERO.
Wrong-results-class findings (NONE are rc4 regressions — all shipped identically in published rc3):
- #925 dropDict UNSOUND under legal cross-package instance overlap (counterexample: general instance in base
  pkg, specific added downstream → downstream's evidence deleted, silently redirected to general; worse with
  IdPDict on instance defs). Fix direction: never drop instance defs, dedup on evidence identity, deterministic
  winner. ALSO: .bo tag not bumped in-PR (IdPCAF tag 38); cacheDef rewrite smuggled in; zero directed tests —
  NOTE: untracked testsuite/bsc.misc/sal/*.bsv in THIS checkout look like the uncommitted directed tests.
- #1035 capture-judgment keyed on WHOLE-pred unification but clause selection is INPUT-keyed → sibling-order-
  dependent accept/reject (counterexample vs its own FdOrder contract). Design note referenced from code/tests
  (typechecker-coherent-instance-commitment.md) DOES NOT EXIST in tree. #1033/#1037 ready; #1036 needs b675 pin;
  #1038 tip (modal pool guard) unpinned.
- #1034 REPRODUCED silent wrong hardware: negative Integer param at width>32 zero-extends (-3 → 4294967293
  via 32-bit canonicalization). Fix: error on k>32 or document.
- #1023 proviso-only tyvars become PHANTOM Verilog parameters (loud Verilog error, not silent).
- #1020 emsg8 duplicate-attr check defeated by applyDefaultArgAttrs re-keying; legacy default_reset repurpose
  has a silent-meaning-change corner.
Hygiene pattern (3×!): #925/#997/#999 changed .bo/.ba formats WITHOUT tag bumps (same slip I made on the 1040
branch — fixed b93ab61d). Suggest an upstream CI guard: Bin-instance-files-changed ⇒ tag-changed.
Other: #1000 wired into only 3/11 sims ("none" not honest; fst→VCD bytes on iverilog; Bluesim trap — rc4 has
1027's real fix, upstream 1000 alone does not); #1008 READY TO UNDRAFT after hygiene (consumer hunt clean;
one LOW hole: Bits at monadic types ICEs); #1030 "hygiene" framing contradicted for residual-dynamic-array-
selection designs (needs re-frame + test; tension with the earlier byte-identical A/B probe — different design
class); #1026 licensing contradiction + 6MB generated artifacts + zero tests; #944/#945 mergeable;
#996 ready-with-nits; #969 needs README (what is bloogle?).
1040/1027-cleanup findings ALL FIXED same-day (rc4 fbe9893e; branches b93ab61d / 2e6b93bf).

## REVIEW CONTINUATION (2026-07-12, workflow wf_41453822-4a8: 37 agents, 16 units, refute panels;
## full archive ~/.claude/projects/-home-ravi-bluespec-matx-bsc/review-reports-2026-07-12b.md)
Covered: the 10 upstream-reviewed-but-not-by-us PRs (967 919 955 957 1028 965 998 988 956 1004), rc4's
own integration commits (weave/regolds/tags/workflows/late additions), release sweep. 97 findings,
4 CONFIRMED HIGH:
- **955 ATF cache DEAD in rc4** (also shipped dead in rc3): the 1038-pool weave (fb009a28) dropped the
  coherent-arm recordATFs in TCMisc sat; only the incoherent arm recorded (the one that SHOULDN'T persist).
  Masked from git -S by the duplicated arm 040614fc excised; invisible to the suite (only miss tests exist).
  Empirically proven dead on the installed binary (-trace-atf-cache: zero hits, even Prelude.Rep PrimUnit).
  **FIXED (Ravi-approved)**: recordATFs restored in coherent arm, REMOVED from incoherent arm (pool
  never-freeze parity), + hit tests ATFCacheHit.bs (same-pkg) / ATFCacheHitDef+Use.bs (.bo-delivered) with
  -trace-atf-cache assertions in typeclasses.exp. Rebuild green; hit trace verified:
  (ATFCacheHit.EncSize, [PrimPair (UInt 5)(UInt 5)]) HIT + Prelude entries now hit.
- **VMDPI crash**: vModuleDeclVIds (998's decl-scan gate) has no VMDPI case → `-verilog -use-dpi` on ANY
  BDPI design = non-exhaustive pattern crash (AVerilog.hs:2203-2213). rc4-only weave interaction: 999's
  module-scope VMDPI (ff244a07) x 998's gate. Ord VMItem also lacks VMDPI (latent). Fix: go(VMDPI...) case.
- **Time-ordered scheduling can silently run ZERO tests** (38b84196): empty/malformed timing.txt →
  sort-by-time.pl dies inside an unguarded pipeline → all_tests.mk empty → green no-op run; macOS
  deterministic (time_cmd writes no time.out; generate-stats installs EMPTY timing.txt). ALSO F2: tool?=bsc
  breaks subdirectory checkparallel (grep '^\./bsc\.' matches nothing from subdirs). MUST FIX before the
  dogfooded rerun. Fix: fail on empty list + warn-fallback the dies + test -s in generate-stats.
CONFIRMED MED: 919 resurrected pre-849 walkNF IRefT arm (rc3 deviation propagated; upstream 849 replaced
it — drop or sentinel); 965 DerivingVia multi-constructor via-target = silent unspecified-value hardware
(critic: treat as HIGH; fix = shape check in doVia); 998 string-keyed isExternal suppresses safe rename
(same-scope reg/DPI-fn collision + misleading G0133); 988 imported_BDPI_functions.h closure-wide
last-writer-wins under -c fan-out + codeGenOptionDescr misses -unspecified-to.
Notable MEDs: b1490.exp caps regressed vs rc3 CI ground truth (5/6 tests at 256-275M < 320M rule —
restore uniform 320M); remapPathMaybe trailing-'/' on marker-free paths corrupts .ba abmi_src_name
(1040 branch needs same fix); untracked bsc.contrib (96 files) sweeps into rerun (inert w/o BSCCONTRIBDIR
— decide track/move); binary provenance corrected: installed = aa12f09d (206), NOT b0a6d7d8 — only
65896922/fbe9893e/5e593850 missing, so rerun REQUIRES the rebuild (goldens ahead of old binary);
957 PROF -K inverted; 1028 T0156 double-report (dedup no-op); 965 `via` fully reserved in .bs lexer.
Critic: every 209 commits owned; residue = ec1a1682 (926 if-then-undef, verify prior-1008-archive coverage),
doc build unexercised, perf smoke recommended (green suite provably can't see perf deaths), delta review
for post-HEAD fix commits, 998-crash attribution = 999's DPI commits (1004 unit reviewed CI trio only).

## FIX BATCH (2026-07-12, Ravi: "make all the fixes, fix upstream PRs when possible, flag for human review")
All in the WORKING TREE (uncommitted, per commit-approval rule). Each empirically validated on a fresh build:
1. 955 ATF cache: recordATFs restored (coherent arm), REMOVED (incoherent arm), dead third sub-case excised
   (TCMisc). Hit tests ATFCacheHit/Def/Use + typeclasses.exp -trace-atf-cache assertions. 3 hits verified.
2. VMDPI: go(VMDPI...) in vModuleDeclVIds (AVerilog) + total Ord VMItem arms (Verilog). -use-dpi compiles.
3. remapPathMaybe: marker-decode only paths containing "///" (FileNameUtil + isInfixOf import);
   run_remap.sh extended w/ absolute-path leg (NO-TRAILING-SLASH / CLEAN / IDENTICAL-ABS all pass).
4. DerivingVia: viaTargetShapeOK guard (single-ctor/single-arg/named-like-type) → positioned T0000;
   DerivingViaMultiCon.bs + golden; ShallowSplit still accepted.
5. b1490.exp: uniform 320M + upstream rationale comment restored (both prior CI burns cited).
6. Scheduling: sort-by-time.pl die→warn+fallback ×2, numeric median, subdir filter fallback;
   suitemake.mk empty-list HARD FAIL + timing.txt.new test -s guard. All paths exercised.
7. Hygiene: '&1' + dupctx.c deleted; bsc.contrib → ./untracked-bsc.contrib-moved-from-testsuite.
8. 919 walkNF resurrected arm DELETED (per Ravi, after posting the delete-wins resolution on PR 919):
   the ICSel region now byte-matches upstream 578123c4; 919's only content there was the unused poss
   pattern widening. Delta-review additions also in: qualifier-aware viaTargetShapeOK, fallback
   harness-file exclusion, grep anchor. Deriving.hs+IExpand.hs changes still need the ONE rebuild.
Withdrawn per Ravi: bespoke -use-dpi tests (coverage = 1004's Verilator legs, which force -use-dpi via
bsc_build_vsim_verilator's VPI rejection; note legs are informational until green).

## RESTRUCTURE 2026-07-12 (per Ravi): rc4 now 214 commits @ HEAD
- Net-zero pair (d922a07a+503b077c) DROPPED via two-step --onto rebase; final tree verified
  BYTE-IDENTICAL pre/post; branch moved with reset --soft under the uncommitted batch. 209→207.
- Regolds kept separate (Ravi), 4804c375 NOT squashed (only the pair was named).
- **925 re-picked from PR head c54eade7** (fingerprint stack, 7 commits → 214). PR 925 review activity:
  still ZERO. Conflicts: GenBin/GenABin tags → THEIRS **bsc-bo-20260713-1 / bsc-ba-20260713-1** (one
  never-released tag covers BOTH format changes: IdPEvidenceFP IdProp serialization + liftDicts Flags
  field); FixupDefs signature → union (fingerprint map type + 955's 5-field IPackage); Bin Flags
  REGENERATED via rc4-genflags.py → **138 fields / 10 chunks**; log2_loop golden → THEIRS (rerere fill
  AUDITED == branch head; restored margins are the safe combined-tree choice); TCMisc/TypeCheck →
  keep batch-6 Reduction constructor + adopt debris deletions (fst3 import NOT taken — no tuple shape
  in rc4). GOTCHA: a mid-sequence 'cherry-pick failed' abort silently ADVANCED the sequencer past
  d41012ce — caught by counting landed commits vs the range; picked explicitly after. Gate: LiftDicts
  IDENTICAL to branch; FixupDefs delta = own_atf_cache weave only; Id.hs delta = 957 idQuality only.
- **Scheduling PR OPENED: upstream #1043** (nanavati:testsuite-time-ordered-parallel @cd4e6ba1).
- TODO (Ravi): bsc-contrib + bsc-bdw CI branches matching the release branch's .ba changes (tags
  20260713-1, Flags 138) — prior branches exist from 988/verilator work; a release-branch-matched one
  is wanted. matx-release.yml currently has NO contrib/bdw jobs and NO verilator legs (ci.yml enables
  verilator_testsuite:true upstream only).
UPSTREAM: PR 1040 pushed 7b3078db (remap fix + test); testsuite-time-ordered-parallel pushed f49e82d7
(scheduling fixes); PR 955 fix COMMITTED LOCALLY on fix/atf-incoherent-record but push to
nanavati:atf-cache-pr DENIED by permission classifier — needs Ravi's explicit go; VMDPI cross-PR comments
posted on #998+#999 (neither branch can host the fix); #965 comment posted w/ confirmed repro + fix +
offer to push (jkopanski's fork — not pushed unasked). b1490/TCMisc-weave fixes = rc4-only, no upstream.
LEDGER: rc4-review-ledger.md at repo root — 28/209 commits at-or-before a verified human review point
(965 full; pre-review parts of 956/998/967/1028); 181 flagged + this uncommitted batch; recommended
human-review order included. Validation: 4 affected suite dirs running; delta-review agent on the batch.

## Pick log
(append: PR, commits picked, conflicts, resolution)

### Group A (done, 5 commits): 967 @ 75dd4b5b — ALL CLEAN, no conflicts.
Mesa: new series REVERTS mkTheCocoon/mkMesaTxLpm0 to interface results (cce1b31e message) — pragma question
mooted vs rc3; verify pragma state at gate time (file: bsc.bsv_examples/mesa/spiless-tx-bsv-cocoon/MesaTxLpm.bsv).

### Group B (done, 33 commits): 919 (13) + 925-own (20). Running total 38.
- 919 eb044afa: IExpand.hs conflict, rerere fill REVIEWED = mechanical IRefT 3→4-field adaptation (eqPtrs,
  findNF S.empty, PrimModuleFix position singleton, isCanon, HeapToDef, walkNF) — accepted; zero anchored markers;
  all IRefT sites verified 4-field incl. upstream's PrimBNot/getBuriedPreds territory.
- 919 b571965f (-cross-info): GenABin tag hunk → OURS (bsc-ba-20260705-1, upstream's current); Bin Flags instance
  REGENERATED via rc4-genflags.py → 134 fields / 9 chunks (upstream had 936's chunked 135; -cross-info removes one).
  IExpandUtils export hunk = UNION (drop getCross, keep getBNotCache/updBNotCache from merged 1007).
- 919 45ea9065 log2_loop golden → THEIRS (mid-branch state; later commit lands final tightened-caps+steps form;
  end-state verified == 919 head).
- 925 e65d6dfc goldens (lc-sysTb, CTX_sysTb.sal) → THEIRS explicitly (did NOT trust rerere fills).
- 925 5512e81f b1490.exp → THEIRS = structured rts_flags form; 925 head has Bug1490MyBool at 320M — the rc3 CI
  320M rule is satisfied BY THE PR; upstream's flat 288M replaced.
- End-state checks: b1490.exp/lc-sysTb/CTX_sysTb == 925 head; IExpand.hs delta vs 919 head = upstream part-2/3
  rewrites only.
- GOTCHA (shell): `grep -c` exits 1 on zero matches — don't && it before git add/continue.

### Group C part 1 (done, 7 commits): 955 @ a2d2d151. Running total 45.
- ea26f0a0 (original May design): 6-file weave; rerere filled FixupDefs/ISyntaxCheck/TypeCheck/bsc.hs (reviewed;
  fills FRONT-RUN later picks: TypeCheck satisfyEq site already says tiResult(...) = batch-6 form — at batch-6
  pick expect no-op/conflict there, resolve keep-ours). GenBin tag → OURS (bsc-bo-20260705-1); IExpandUtils
  export = union (955's mergeATFCache + our getBNotCache/updBNotCache, getCross stays dead).
- 280e7e17: FlagsDecode fill = 3 trace-flag names, no Flags record change.
- a2d2d151 (July fix): FixupDefs fill = final own-entries-only form w/ explanatory comment; bsc.hs fill =
  elabATFCache foldl' over binmods (the validated design); GenBin tag → OURS.
- END-STATE VERIFIED vs refs/rcpr/955: FixupDefs delta = exactly 925-hoist (mkCoherentDictMap/fixUp cm/dropDict,
  IPackage output carries own_atf_cache); TypeCheck delta = 923 warnTransitiveIncoherent weave; ISyntaxCheck = 919 IRefT.

### Group C part 2 + D + E (done): 957 (18: b302 golden→theirs, on regold list), 1028-own (3 CLEAN),
944/945/965 (8 CLEAN). 997 (1 clean). 998 (7: skipped mainline-sync merge commit a4269d04 — cherry-pick
refuses merges, use `--quit` + resume AFTER the merge; AVerilog weaves = 998 evolution + our renameInoutPorts
(upstream-1001) + systemVerilogOutput (997); end-state matches head incl. Generic.listify). 999 (2:
AVerilogUtil fill = vco_ffmap/def_widths ✓). 1023-range (3 ⊇1022: VMInst named params; vCommentTaskName
DELETED by -v95 removal — 3 dangling fill-reinstated calls INLINED immediately (rc3 build-1 lesson);
genflags 134→133 fields, v95 flag field removed; Error.hs fill = complete union G0129/130 (1020) +
G0131-133 (998, renumbered by its own b26a61d7)). 1020 (2: trivial headerBS keep-ours hunks). Total 89.

### Group F (done, 44 commits → 133 total): 988 (13 via cherry '+' list, 987's 5 auto-dedup'd; genflags
after -sim-codegen-only 133→134 and after -c mode → 135), 996 (1), 1000 (5; genflags → 136 fields/10 chunks
= rc3's final count), 1008 (21 curated: a85856fd near-noop (926 upstream, whitespace-only conflict);
e07b1daa log2_loop → OURS (919's tightened caps supersede 1008's stale old-main copy — watch suite);
MPEG4 files == 1008 head after theirs (ba219c5d exclusion validated); goldens lc-sysTb/CTX_sysTb/mkTop_glob/
DynArrSel → theirs + regold list; 8e63fefc bitsSelInfo fill ✓ + getBNotCache union; bugfix stack + pack-unpack
move (57 files) landed clean), 1030 (4: eqPtrs = 1030 rewrite + 4-field IRefT + KEPT 1008's hardened
heapOf diagnostic; ArrayCell canonicalization in; comment-evolution commit woven same way).

### PR 1027 investigation (agent, 2026-07-11) — affects rc4 integration fixes
Bluesim + `-dump-formats fsdb|fst` under PR 1000 ALONE (rc4 ships 1000): links silently, runtime opens real VCD,
kernel appends clock changes with NO $var defs (defs live in the stubbed dump_VCD_defs which prints the WRONG
"-dump-formats none" error). Perpetually growing garbage. → ADD rc4 integration commit: link-time rejection of
fst/fsdb for Bluesim in simLink (model on 1027's 9ffd4e43 bsc.hs hunk, S0015-style, cover BOTH formats).
1027 itself: FST design sound (forward-buffering architecture moot's rollback concern; licensing/link-gating clean);
held out for process/risk (102-file unreviewed 3-layer stack). Full assessment in the conversation of 2026-07-11.

## 2026-07-12 LATE SESSION: 967 DROP + fixes + attribution sweep
- **967 DROPPED from the release** (per Ravi; upstream PR discussion continues): first-5-commit rebase,
  ZERO conflicts, 209 commits. AExpr2STP/Yices clean vs base; TypeAnalysis delta = 955's (fine);
  splitports.exp REVERTED to base in the batch (d4000566's SplitArgSlice hunk asserted 967 behavior).
  EXPECT suite arbitration: CSE-naming goldens (b302/lc/CTX/mkTop_glob) were captured on a 967 tree —
  PrimPair struct removal may shift __h numbers; regold with diff verification if so. Scope now 29 PRs.
- **956 EN/RDY fix** (batch): WireAnalysis submodEnRdyCandidates — EN from vf_enable minus VPinhigh
  (always_enabled ⇒ inhigh ⇒ no netlist wire), RDY from isRdyId Method entries in vFields (always_ready
  ⇒ entry absent) ⇒ no phantom candidates by construction. vcd_correlation baselines may shift (hits
  count) — regold at suite time. Push to bluetcl-wiretypemap AFTER local validation.
- **988 -unspecified-to descriptor fix** (batch): codeGenOptionDescr records "unspec-<v>"; DEVELOP.md
  two-things → three-things. imported_BDPI_functions.h issue still OPEN (decision pending).
- **ATTRIBUTION SWEEP** (per Ravi: he must be author/co-author on all session commits): scanned all
  open PRs; 30 bare-"Claude" commits across 10 nanavati branches rewritten (author → Ravi, Claude
  co-author trailer), patch-ids verified identical, force-pushed. NEW HEADS: 955=d0059875,
  956=29681fc3, 1020=f523f16c, 957=85ec10a5, 1005=bd5374d2, 925=d98dc3f4, 1008=ceb370af,
  919=99310ddd, 998=a3e7250f, 1004=9035771d. SKIPPED (not ours to rewrite): 988's 472d1df68
  (Greg Steuck) and 1028's nine Lucas Kramer commits. refs/rcpr/* pins now stale hash-wise but
  patch-id-safe. STANDING CONVENTION: repo git config makes Ravi author; keep Claude trailer.
- macOS Verilator is a GOAL (Ravi): matx-release should gain macOS Verilator legs (advisory),
  macOS CI needs gtkwave (fst2vcd checks silently skip), 1004 ccache save fix worthwhile.
- PR-movement check (post-sweep): upstream/main UNMOVED (578123c4). 31/36 heads as expected.
  Real movers: 969 → 29f99e25 (amy's merge+ByteString fix — the wanted update); 1027 → 2e6b93bf (our
  earlier cleanup push). **919 REBASED onto 578123c4 by another session (13→12 commits, committer-dated
  07-13): took the posted delete-wins walkNF resolution — old arm GONE, matching rc4's batch deletion —
  but clobbered the attribution rewrite; re-applied on the rebased head → plug-leaks @ b6893d68**
  (12 commits, patch-ids verified). GOTCHA: never hand-type lease hashes — take from ls-remote.
- 919-rebase vs rc4 verdict (Ravi asked; answer: DON'T re-pick): range-diff shows all commits
  patch-identical except (a) the IRefT commit minus the deleted walkNF arm hunk (matches rc4's batch
  deletion) and (b) old resource commits 7+8 squashed into one. SOLE content divergence = log2_loop
  golden caps: PR@-H128M -M256M -K20M vs rc4@-H150M -M300M -K30M (925-fix restore, last-pick-wins).
  rc4's generous caps are DELIBERATE (combined tree carries more; b1490 CI lessons) — known deviation,
  do not "fix" toward the PR; gates should expect this line to differ.
- SUITE FINAL (2026-07-12 evening): **23,589 PASS / 11 triaged FAIL / 0 ERROR / 132 XFAIL** on the
  full rebuild (binary 607f5b00, tags 20260713-1, Flags 138, 967 dropped, 925 fingerprint in).
  967-drop naming goldens (b302/lc/CTX/mkTop_glob) ALL HELD — PrimPair revert shifted nothing.
  The 11: 8 bluetcl EN/RDY golden growth (verified purely additive; hits 117→169; 12 RDY entries),
  1 DerivingViaMultiCon golden missing the -u preamble (regenerated), 2 splitports patterns.
  CORRECTION: my splitports revert-to-base was WRONG — the d4000566 regold patterns match the
  no-967 output too (drift is combined-tree naming, NOT 967); exp restored from HEAD.
- 956 upstream push prep: annotated fixture (mkAnnotStash: always_enabled tick / always_ready count /
  plain push) compile-verified — mkAnnotStash.v has ONLY EN_push/RDY_push, no removed ports; phantom
  check + dumpmap.tcl added; fixture mirrored into rc4 vcd+fst correlation dirs (fst compiles from
  shared SRC; MOD_AT_LIST patched both). Branch build running in worktree; on green: regen branch
  goldens → run bluetcl dirs → push to bluetcl-wiretypemap.
- MatX CI verilator strategy (Ravi): pull **MatX-inc/verilator release (build-20260701_1)** into the
  CI legs — exactly matches deployment, pins the version, avoids ALL 4.x veribugging (matx test matrix
  = ubuntu-22.04 + macos-15; apt would give verilator 4.038). MatX-inc/iverilog exists too — same trick
  available. Multi-vsim harness mode: PLANNING ONLY (wf_15a9fddd), nothing lands in rc4.
- GOTCHA (harness): `make -j8 A.group B.group C.group` at the testsuite root ESCAPED SCOPE and ran
  far beyond the named groups (observed in bsv_examples ~40min in; killed). Single .group targets,
  sequentially, are the known-good pattern. (The earlier 4-target run Ravi killed likely did the same.)
- FLAKE CANDIDATE: sysLife.c-vcd.out vs c.out trace-invariance diff appeared ONLY in the rogue run
  (under ~full-machine load), NOT in the clean full suite — rerun bsc.games/life solo before reading
  anything into it.
- 956 branch: EN/RDY fix adapted to pre-spree base (vf_output SINGULAR — deliberate cross-base
  deviation vs rc4's vf_outputs); phantom check corrected to query the PARENT map's annot$ entries
  (the module's OWN map deliberately over-emits ifc-side candidates — pre-existing design); branch
  probe: annot$EN_push/RDY_push present, EN_tick/RDY_tick/RDY_count absent ✓. Branch goldens all
  purely additive (wiretypemap +108, splitports +12, clocks +26 — gating-primitive EN-class ports).
- **rc4 VALIDATION COMPLETE (2026-07-12 late)**: full suite 23,589/0-err + all regolds settled +
  final localcheck of the three fixture-affected bluetcl dirs GREEN (1/1/72, zero fails). The
  clocks.tcl pair (missed in the first regold round) verified additive (+26) and regolded. Phantom
  check green in rc4's own output. sort-by-time filter fixed (dir-form tools; default-tool-only
  fallback; perl interpolation gotcha: $ARGV[0][/.] in a string = nested index + unterminated regex)
  → pushed to 1043 @ bd04de4c. localcheck = the per-dir mechanism (the .group path is top-level-only).
  **The working tree is commit-ready, awaiting Ravi's word.** 988 branch: header-elimination +
  fan-out test green first try (52 pass); descriptor fix added; rebuild+retest cycle running.
- **988 PUSHED @ 4975c60b** (2 commits): a67c230b inline per-module BDPI decls + -c header elimination
  + -e deprecation (#warning/banner) + shared-simdir fan-out test (52/0 + sweep 50/0 green — the
  descriptor change composes with byte-identity); 4975c60b unspec-<v> reuse descriptor + DEVELOP.md.
  ALL confirmed review findings with our-venue fixes are now LANDED upstream. Remaining upstream
  items sit on others' PRs (965 cherry-pick offer pending; 998/999 VMDPI comments posted).
- ec1a1682 PROVENANCE CLOSED + DROPPED (per Ravi): the if-then-undef rule is upstream BASE content
  (926 merged); rc4's commit carried ONLY the unused UndefKind import (the 1008-archive LOW), deduped
  rule but kept import in the pick's whitespace resolution. Rebase-dropped (119 replayed clean, tree
  delta = exactly the 1-line import). 223→222. Zero behavioral delta (unused import).
- CORRECTED downstream-CI picture: the shared build-and-test workflows run THREE ungated
  downstream-repo jobs — Toooba (bluespec/Toooba), bsc-contrib (B-Lang-org/bsc-contrib), bdw
  (B-Lang-org/bdw) — all via checkout_possible_branch.sh NAME-MATCHED branches. matx-release runs all
  three. Ravi's release-matched branches = push 'release-2026.07.rc4' branches to those repos (with
  .ba@20260713-1/Flags-138-affected expectations updated); picked up automatically by the release run.
- Downstream release branches PUSHED: MatX-inc/bsc-contrib and MatX-inc/bdw @ release-2026.07.rc4
  (from nanavati vlink-regen d5b6a2c / 1cff4e4). Per Ravi these goldens check bsc COMMAND OUTPUT
  (the .ba-written chatter), NOT .ba bytes — so the post-vlink-regen format changes (remap/
  fingerprint/liftDicts) don't touch them; branches current as-is. checkout_possible_branch matches
  by name on PUSH events only — trigger matx-release by pushing the branch, never workflow_dispatch
  (dispatch silently falls back to B-Lang-org defaults). Toooba unmatched → bluespec/Toooba default.
- VERILATOR BASELINE (attempt 6, in flight): harness gap found — NOTHING implies -use-dpi under
  verilator (vcomp_flags empty; only iverilog gets derived flags), so all BDPI tests take the VPI
  path and verilator's link hard-rejects → the whole bsc.codegen/foreign class is harness, not
  divergence. Fix = derive -use-dpi for verilator at BOTH compile and link (wrapper flavor is chosen
  at COMPILE — BDPI is the one class where sim choice reaches the bsc compile; multi-vsim plan must
  special-case it: per-sim compile or always-DPI). -system-verilog-output must NOT be implied
  (golden avalanche); per-test only. Verilator-keyed "don't try" interlocks barely exist (b925 +
  990s) — the iverilog quirk list has no verilator counterpart; this baseline writes it.
  NOTE the interlock: implying -use-dpi is only safe because the batch fixed the VMDPI crash.
- INFINITELOOP CORRECTION (Ravi's call, confirmed): the fatal lint was a CORRECT interlock, not
  noise — the flagged designs (mostly MCD: derived-clock generators needing --timing, which the
  harness rightly disables via --no-timing) genuinely SPIN under verilator. My suppression converted
  clean link-failures into hung sims (cores spinning; run killed). Suppression REVERTED (both .vlt
  copies pristine). The ~60 INFINITELOOP sites from the attempt-4 logs enumerate the DON'T-RUN
  interlock list. Verilator burn-down now well-defined: (a) imply -use-dpi under verilator
  (compile+link harness derivation); (b) sim-keyed SKIP interlocks for the MCD/timing class
  (fatal lint stays as enforcement); (c) triage genuine diffs (SquareRoot/FP/BRAM/fwrite/params);
  (d) per-test -system-verilog-output where SV output is the point. GATING SEQUENCE DECISION
  PENDING: the committed gating job would be RED on a release run until the burn-down lands.
