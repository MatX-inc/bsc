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
