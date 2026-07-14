# release-2026.07.rc6 assembly state

## What rc6 is

rc5's content rebased onto the updated upstream/main @ 941eecfe (2026-07-14),
which absorbed three of the woven PRs as rebase-merges that morning:

- #919 (evaluator memory leaks)      merged 08:01Z
- #957 (quadratic blowups / stacks)  merged 08:37Z
- #955 (ATF cache)                   merged 09:29Z

Per Ravi: 967 and 1038 stay dropped (both were already out — 967 since rc4,
1038's pool since rc5's surgery), and rc6 adds the committed-residual fix that
post-dates the woven 1035-1037 copies (see below).

## Assembly (2026-07-14, worktree /home/ravi/bluespec/matx/bsc-rc6)

- `git rebase -i --empty=drop --onto upstream/main 578123c4 release-2026.07.rc6`
  with a sequence-editor drop list (.rc6-drops) of the 33 upstream-merged
  commits: 24 patch-identical + 9 restructured-before-merge.  NOTE: the
  automatic cherry-pick drop does NOT apply here — git compares against the
  <upstream> argument (578123c4), not the --onto target — hence the explicit
  list.
- 8 stops, all small: two emptied margin/steps tweaks whose intent is in
  upstream's merged line (log2_loop), two import/comment collisions with
  upstream-955's ATF plumbing (bsc.hs, TypeCheck.hs — rerere replayed most of
  the rc5-surgery resolutions), the FixupDefs evidence-dedup vs own_atf_cache
  field (kept both: 5-field IPackage + dropDict/ds''), the GenABin Flags-arity
  seam (133 at that point in the replay; final record is 138 = arity 138), and
  two stale tag bumps (kept the newer values; rc5's final 20260714-2 bump wins).
- Drop accounting vs rc5: 205 -> 188 carried commits.  17 truly dropped =
  9 planned + 8 emptied whose content is verified present via the upstream
  base (ATF cache cleanups, cCtxReduceIO doc comment, walkNF arm, mcd golden,
  TestATFCacheMiss golden, log2_loop x2, rc4 tag bump).  Everything else
  applied (patch-ids drift from context adaptation only).
- Tree diff rc6 vs rc5: 3 files, all cosmetic (TCMisc comment wording from
  upstream's final 955, GenABin do-layout whitespace, log2_loop flag order).
  Serialization identical -> tags stay bsc-bo/ba-20260714-2 (rc6 .bo/.ba are
  byte-compatible with rc5).

## Post-rebase additions

- Cherry-picked from personal/claude/bound-variable-discipline (the rebased
  1035-1037 stack, 2026-07-13/14):
  * "Never report a committed residual unsatisfiable without a final retry"
    (1035 fix: tiExpl endgame satisfy-probe on ground uds + ATF normalization
    in matchTopIsReducible's check_fds; + DeferCommittedResidual{,TwoInstances}
    tests).  The woven 1035-1037 predate this fix — first rc6 build failed the
    MinSelf reproducers with T0032; post-pick they pass.
  * The endgame ORDERING INVARIANT comment at the settlement point.
  * TCMisc conflict resolved as in the stack rebase: modal `mgu []` (1037)
    composed with monadic normT check_fds (the fix).

## Downstream

- MatX-inc/bsc-contrib release-2026.07.rc6 @ f4dcea09 (= rc4/rc5 head)
- MatX-inc/bdw         release-2026.07.rc6 @ 38599919 (= rc4/rc5 head)

## Validation

- Full build clean (make -j32 GHCJOBS=8 GHCRTSFLAGS='+RTS -M16G -A128m -RTS'
  install-src; zero errors), version 2026.01-345-g54c9dcca.
- MinSelf.bs / MinSelf2.bs: PASS (were T0032 pre-fix).
- Typechecker suites: commit 19/0, instances 191/0, typeclasses 123/0,
  settle 4/0, bound-type-vars 14/0 (dictpool gone with the pool).
- Full iverilog suite (2026-07-14, -j128 fullparallel, SystemC on):
  **23,508 PASS / 0 FAIL / 132 XFAIL / 0 XPASS / 2 UNSUPPORTED** — clean.
  (rc5 was 23,505; delta = the two DeferCommittedResidual tests + upstream's
  new perf-creg additions net of restructuring.)
- Verilator leg: not run this cycle (rc5's was in flight at handoff; the
  ratchet-mode gating design from rc4-verilator-burndown.md still applies).
- NOT pushed to origin: per the rc discipline, push only with Ravi's
  explicit go.

## Lifting swap (2026-07-14, later): fingerprint 925 -> structural stack

- Backed out the old-925 line via drop-rebase onto 941eecfe (26 dropped:
  the original lifting pass + ISimpDicts/FixupDefs/IdPCAF base, the
  evidence-fingerprint stack, rc5's digest+verify and its tag bump and
  regolds; 968cf1d8 emptied — its content was purely lifting-era golden
  refresh).  d700f5ca re-applies the comment cleanup the itIsDictType
  commit carried.
- Cherry-picked the validated structural stack (nanavati/bsc
  lift-dicts-idefs @ 94d70453 on ftv-metadata e06d39cc): SYB removal,
  tconcheck x3, position restamp, interning x2, ftv metadata x3,
  lifting x3 + tests (341deccc..1c84e1b1).  Key splices: IExpand eqPtrs
  (stack's real-S.empty poss + rc line's ICLazyArray canonicalization),
  tconcheck/bloogle/libfst Makefile union, GenABin Bin Flags regenerated
  at 138 fields / 10 chunks via rc4-genflags.py.
- Tags: bsc-bo-20260714-3 / bsc-ba-20260714-3 (f4c55204; -2 was
  three-way ambiguous).  IdPCAF back at IdProp code 38; IdPEvidenceFP
  gone.  No Error.hs tag changes.
- Suite (iverilog, -j128 fullparallel, SystemC on, showrules installed):
  **23,576 PASS / 0 FAIL / 132 XFAIL / 0 XPASS / 0 UNSUPPORTED**.
  Movement: bsc.misc/liftdicts structural-evidence tests (34, replacing
  the fingerprint set), five dump goldens settled in 95356a60 (three
  identical to pre-swap; sysStructs pair = position renumbering +
  name-form mix, no structural change).
- Soundness repro: topTag=2 midTag=1 PASS.  BlowupN d=16: 9.5s.
- tconcheck: 94 entries OK.  Verilator leg NOT run; timing-ci.txt:110
  still lists bsc.misc/liftdicts (dir persists with same-name tests) —
  re-shakedown when the verilator leg next runs.
- NOT pushed; publication is the coordinator's call.
