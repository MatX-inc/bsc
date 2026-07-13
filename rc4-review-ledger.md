# rc4 human-review ledger (2026-07-12)

Definition (per Ravi): a commit is FLAGGED if it is not covered by a **verified human review
point on GitHub** — i.e. it postdates the last review event by a human other than the author
on its PR, or its PR has no human review at all, or it exists only on the release branch.
Review events = formal GitHub reviews (APPROVED / CHANGES_REQUESTED / COMMENTED) by non-bot,
non-author users. Commit dates are committedDate, so a rebase after review conservatively
re-flags the whole PR (the reviewed content cannot be verified against the re-stamped commits
without patch-level re-diffing — which is exactly what "verified" should mean).

Raw sweep data: scratchpad review-ledger/ledger-raw.txt (gh, 32 PRs, 2026-07-12).

## Summary

Of the 209 commits on release-2026.07.rc4 (578123c4..5e593850):
- **28 commits (~13%) sit at or before a verified human review point** — PR 965 in full,
  and the pre-review portions of 956, 998, 967, 1028.
- **181 commits (~87%) are FLAGGED**, plus the entire uncommitted fix batch now in the
  working tree.

Mitigation (not a substitute for the human bar): every rc4 PR and every rc4-only integration
commit has been through the two adversarial agent-review programs
(review-reports-2026-07-12.md and -12b.md, ~90 agents total, refute-panel-verified findings),
and all CONFIRMED HIGH findings are fixed in the working tree.

## Per-PR status (reviewers; last review; flagged commits)

**Fully covered at head (0 flagged):**
- #965 DerivingVia (jkopanski) — reviewed by krame505, matx-amy, quark17 through 2026-07-05;
  no commits after. NOTE: our via-shape fix (uncommitted, and to be offered upstream) is new
  unreviewed content on top.

**Mostly covered (flagged commits listed):**
- #956 wiretypemap — krame505+quark17 through 2026-07-06; flagged 1/16:
  f26149a7 (run_correlation.sh PATH fallback; rc4 copy of upstream e649f058).
- #998 SV identifiers — quark17 2026-07-05; flagged 5/7 in rc4: 3451a758 (keyword table),
  b937d2ad (drop --no-std), 880be88f (foreign-name/decl-scan gate — the VMDPI-adjacent one),
  1f684f68 (G0131-3 renumber), c8729f0f (VMInst record patterns).
  Covered: 0c6c8e88, 52d1d442.
- #967 tuple splitting — nanavati+quark17 through 2026-07-11; flagged 1/5:
  4cebadfa (SplitVector test update after rebase; upstream 75dd4b5b).
- #1028 T0029/ATF-synonym — quark17 2026-07-08; flagged 1/3 own:
  3120ec2b ("Address PR #1028 review"; upstream eb08b723 — the review-response itself
  was never re-reviewed, and our program found its dedup is a no-op).

**Reviewed but fully re-flagged (rebased/reworked after the last review):**
- #919 evaluator leaks — quark17 reviewed 2026-07-05, branch re-pushed later that day;
  all 13 rc4 commits flagged. (Includes the resurrected walkNF arm finding.)
- #955 ATF cache — krame505+quark17 reviewed 2026-06-15, branch REWORKED 2026-07-05/06
  (the .bo snowball fix); all 7 rc4 commits flagged.
- #957 quadratic fixes — quark17 reviewed 2026-06-27, rebased 2026-07-04 + tie-order fix
  2026-07-06; all 18 rc4 commits flagged (content largely predates the review, but the
  rebase makes that unverifiable from GitHub).
- #988 block codegen — quark17 reviewed 2026-07-02 BEFORE the -c-mode rework landed the
  same week; all 13 rc4 commits flagged.
- #1004 Verilator CI — quark17 reviewed 2026-07-04; the queue was re-pushed 2026-07-06;
  the 3 CI commits in rc4 flagged.

**Zero human review (all commits flagged):**
- #925 lift dictionaries (20 rc4 commits) — also carries the dropDict-unsoundness finding;
  the evidence-fingerprint fix exists on nanavati:lift-dictionaries@c54eade7 but is NOT
  in rc4 (re-pick decision open).
- #944 unary negation (1), #945 unbased literals (2) — matx-chris.
- #969 bo2bloogle (3) — matx-amy.
- #996 TOP_CXXFLAGS (1), #997 SV strings (1), #999 DPI (2), #1000 dump-formats (5),
  #1008 pack/unpack (21), #1020 default_clock/reset attrs (2), #1022+#1023 named
  params/foreign provisos (3), #1030 eqPtrs (4) — nanavati.
- #1026 tree-sitter (1) — mieszko.
- #1027 FST stack (8 in rc4 incl. the two bluetcl/Bluesim cleanups) — nanavati.
- #1033–#1038 typeclass-coherence line (25) — nanavati.
- #1040 remap-path-prefix (in rc4 as 42b968cf + 65896922 + fbe9893e) — nanavati.

## Release-branch-only commits (no PR, all flagged) — 22

Integration/weave: 4804c375, 040614fc, ca23726e, d47246ef.
Error-tag/format/workflows: 9fa1e7f6, 26c3a29c, 6a083546, d922a07a + 503b077c (net-zero pair).
Regolds: 04288918, 720f4994, 995fc0f4, 3282b8ed, d4000566.
Late additions: 38b84196 (scheduling; branch exists, NO PR per Ravi), 5e593850 (-v95 message).
Journal: b0a6d7d8.
(42b968cf/65896922/fbe9893e are counted under PR 1040 above; ccc7b991-era FST commits under
#1027.)

## Uncommitted working tree (this fix batch, all new)

955 recordATFs restore + incoherent-arm removal + dead-arm excision (TCMisc.hs);
ATF hit tests ×3 + typeclasses.exp; VMDPI clauses (AVerilog.hs, Verilog.hs);
remapPathMaybe marker guard (FileNameUtil.hs) + remap-path test extension;
via-shape check (Deriving.hs) + DerivingViaMultiCon test + golden;
b1490.exp uniform 320M; sort-by-time.pl fallbacks + suitemake.mk guards;
journal updates. Validated: VMDPI compile, via rejection + ShallowSplit pass, remap
IDENTICAL-ABS, ATF hits ×3, scheduling fallbacks ×3, run-tests-setup guard.

## Trim analysis for the 22 release-branch-only commits (2026-07-12)

Reclassified — already PR-homed (an upstream review venue exists; dedups at the next re-pick):
- 5e593850 (-v95 message) is PATCH-IDENTICAL to 3098e479 on PR 1022's branch.
- ca23726e (bo2bloogle ByteString): PR 969's head (amy/bloogle) already carries the fix.
- 9fa1e7f6 (T0162/T0163): pushed to PR 1023 @ c28babff today. rc4's copy has a different
  patch-id (context drift) — EXCLUDE 9fa1e7f6 from rc5 resume lists (deliberate-deviation rule).

History-rewrite candidates (branch unpushed, so allowed — needs Ravi's go):
- Drop d922a07a + 503b077c (net-zero guard/revert pair; clean by construction).
- Squash 4804c375 into 040614fc (both are weave repair).
- Optionally squash the 5 regolds into one drift-classes commit (loses per-class messages).

Candidate for a new PR: 38b84196 + f49e82d7 (time-ordered scheduling + the zero-test guards)
— the guards fix silent-coverage-loss bugs upstream would want regardless of the LPT feature.

Irreducible combined-tree core (~8): 040614fc, d47246ef (cross-PR interaction repairs — cannot
be hosted on any single upstream branch), 26c3a29c (MatX workflows), 6a083546 (canonical tag
pair), the regold commit(s), b0a6d7d8 (journal, docs-only), plus the release-only parts of the
current fix batch (955 weave restore, VMDPI, b1490).

## Recommended review order for a human pass

1. The uncommitted fix batch (smallest, newest, gates everything else).
2. The 22 release-branch-only commits (no upstream venue will ever review these).
3. #925 (wrong-results-class finding, 20 commits, zero review, perf-critical).
4. #1008 (21 commits incl. wrong-hardware fixes; upstream review requested via undraft).
5. #1033–#1038 (typechecker semantics, 25 commits).
6. The re-flagged-by-rebase set (#957, #919, #988, #955) — diff against the reviewed-era
   heads first; much may reduce to verified-by-content.
7. The long tail of small zero-review PRs.
