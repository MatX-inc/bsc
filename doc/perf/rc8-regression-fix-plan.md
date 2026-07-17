# Fix every bsc rc8 performance regression (PR #18772) without giving up features

## Context

matx PR #18772 bumped bsc from `build-20260525-2` (= `1391edbd`, OLD) to
`build-20260715-1` (= `88e88222`, NEW, tip of the `release-2026.07.rc8`
lineage; 1307 commits). The 2131-action A/B benchmark showed wall +15.6%, CPU
+3.4%, **peak residency +34.7%**, with severe per-phase regressions
(schedule +55%, verilog +198%, binary +71%, liftdicts +132%, internal +102%,
fixup +80%, wrapper_typecheck +36%, imports +16%, sympost* +6–41%) alongside
real wins (expanded −27%, writeBin −51%). Follow-up issues: matx#19126
(verilog), upstream bsc#1056 (schedule).

**Goal (user directive): end up significantly BETTER than the OLD release in
wall clock time and memory usage — not merely back to parity — while keeping
all new features**: associated type functions (SizeOf-as-ATF), coherent-commit
instance resolution + PredTrie, port splitting, SV identifier legalization,
sched-conditions scheduling precision, cross-package dictionary dedup, interned
ITypes. That means two tracks: (a) fix every regression, (b) a
beyond-parity optimization workstream (WS7) attacking the biggest global
costs — ~22% of all CPU is GC, every phase boundary deepseqs its whole result,
and the top CPU sinks (expanded 20.6%, imports 13.3%, typecheck 10.9%) have
identified headroom. Wall clock additionally rides on the long-pole actions
(TAControllerScalarUnit 569s, ComparePostSrbInstr 490s) — both are directly
shortened by WS1–WS4.

Work happens in MatX-inc/bsc on branch `claude/bsc-18772-perf-regressions-9vi622`.
**The branch is currently based on `main` (`13c85fff`), which does not contain
the rc8 code** — step one is `git checkout -B <branch> 88e88222` +
force-with-lease push (branch has no unique commits).

## Root causes (verified against `git show`/`git diff` of the two tags; key sites spot-checked by hand)

| # | Regression | Root cause | Key commits |
|---|---|---|---|
| 1 | schedule +54.7% (worst +3865%), sched GC +79% at flat alloc | Default flip `schedConds`/`aggImpConds` False→True (feature pre-existed): `mkConflictMap` (ASchedule.hs:3911-3965) runs ≤2 Yices queries per conflicting method pair × O(rules²) × 3 conflict graphs; `checkDisjointExprWithCtx` (DisjointTest.hs:97-107) rebuilds `aAnd pred cond` per pair (XXX comments already flag it); `proofMap :: M.Map (AExpr,AExpr) Y.Status` (AExpr2Yices.hs:322) keyed on those big composites, retained all phase | `0f50f1ab` |
| 2a | verilog +198% universal floor; verilogDollar +144% (zero code change — first to force the tree); residency 2× on worst target | `renameInoutPorts` (AVerilog.hs:881-923) runs `Generic.everywhere` SYB rebuild **unconditionally on every module**, even with zero inout ports — destroys sharing | `8458d046` |
| 2b | verilog worst case (ComparePostSrbInstr +596% time, +1972% alloc) | SV ident legalization: gate is whole-program; one colliding identifier (e.g. `process`) → `renameSVStdIdents` runs 3–4 full SYB listify passes + `everywhere` rebuild over ALL modules incl. collision-free ones (AVerilog.hs:2057-2160, 2237) + whole-program `listify isReservedVId` (L127) | `5dd1a1ca`, `6339f242` |
| 2c | writeVerilog +26% | per-identifier reserved-word Set lookup in `PPrint VId` (Verilog.hs:783-793) + rendering the now-unshared tree | `5dd1a1ca` |
| 3 | typecheck +9.8%/alloc +24%; wrapper_typecheck +35.6% | SizeOf as ATF: `expTFun` (TCMisc.hs:339) does 2× `findCls` + fresh tyvars for every class param + new VPred per occurrence (`mkATFClassPred`), also invoked from context reduction (CtxRed.hs:398); `CATFCache` caches normalization, not constraint synthesis; `925b29be` retains more predicates + eager `PredAncestor` chains (Pred.hs) | `add2e991`, `925b29be` |
| 4 | liftdicts +132% CPU, alloc 0.27→27.5 GB on TileSS | LiftDicts rewrite: `handleDict` runs `convDict` (full iConvT+iConvExpr of evidence) on **every** dict occurrence before dedup, then linear structural scan of type bucket; OLD had O(1) `Map CType Id` coherent fast path with one deferred conversion | `967a6b8f`, `7e367a69` |
| 5a | binary +70.7% (SimR2 +2051%) | phase `dump` deepseqs ALL imported IPackages; payload grew (new `ipkg_atf_cache` .bo section, `DefP_DictRendering/Kids/Types`, lifted-dict IDefs, FTV-cached ITypes) and `instance NFData IType` (IType.hs:507) **walks interned DAGs as trees** — shared subtrees re-forced everywhere. writeBin −51% is real (DList writer `f1026a46`, intern-keyed sharing `4ed037c8`) | `add2e991`, `7e367a69`, `f6989da8` |
| 5b | imports +16% | same enlarged .bo payload; `hasPoisonPill` walk + hash merge over larger def sets | same |
| 5c | internal +102% | `iConvPackage` now converts the whole ATF cache + appends lifted-dict IDefs (IConv.hs:52-66), then deepseq (same DAG-as-tree rnf) | `967a6b8f`, `add2e991` |
| 5d | fixup +79.5% | evidence-identity dict dedup (`evmap`/`mkRedirects`/`verifyPair`) recomputed over local+ALL imported defs **per `updDef` call**, + 2 `redirectDictProps` full rewrite passes | `7e367a69`, `980d5afd` |
| 5e | sympost* +6–41% | per-class PredTrie + O(n²) eager `cmpMemo` spine + `overlapErrors` rebuilt on **each of the 5+ `mkSymTab` rebuilds** per package | `05865c6f`, `add2e991` |
| 6 | peak residency +34.7% | 2a sharing loss (worst target); strict `!VarSet` FTV cache on immortal interned ITypes (`f6989da8`; escape hatch `-hack-no-itype-ftv-cache`); WS1 proofMap composites; eager PredAncestor | `f6989da8`, `925b29be` |

All fixes below are **behavior-preserving** (pure memoization/laziness/gating);
no feature is dropped. Diagnostic escape hatches already in rc8 for A/B
attribution: `-no-sched-conditions`, `-no-aggressive-conditions`,
`-legacy-inst-index`, `-legacy-defer-instances`, `-hack-no-itype-ftv-cache`,
`-no-lift-dicts`.

## Step 0 — branch setup + attribution experiments (before coding)

1. Reset the bsc branch onto rc8: `git checkout -B claude/bsc-18772-perf-regressions-9vi622 88e88222`, push `--force-with-lease`.
2. Build bsc from source once (per repo INSTALL; GHC + `make install-src`) to enable micro-benchmarks with `-v` phase timestamps and `+RTS -s`.
3. Attribution experiments that pick tiers (each ~minutes with the stock rc8 binary on extracted repro cases):
   - **schedConds vs aggImpConds split**: worst schedule targets with `-no-sched-conditions` / `-no-aggressive-conditions` / both. WS1 assumes schedConds dominates.
   - **DAG-as-tree rnf hypothesis**: patch `rnf t = t `seq` ()` for interned ITypes locally, measure SimR2 binary/internal/fixup. If confirmed, WS3.1 alone recovers most of bucket 5.
   - **FTV-cache residency share**: ScalarUnit with `-hack-no-itype-ftv-cache` + `+RTS -s`.
   - **verilog cliff repro**: matx#19126 `genr.py 2000 trip|notrip` for WS2 before/after.

## Workstreams (Tier 1 = low-risk, behavior-identical; Tier 2 = only if numbers demand)

### WS1 — schedule (files: ASchedule.hs, DisjointTest.hs, AExpr2Yices.hs, AExpr2STP.hs)
Tier 1:
- **Interned proof-cache keys**: intern condition/context AExprs to Ints per `DisjointTestState` session; `proofMap :: M.Map (Int,Int,Int,Int) Status`. Kills deep `Ord` on huge composite keys and their retention (fixes GC/residency at flat alloc). Build `aAnd` only for the actual solver conversion on a miss.
- **`aTrue` shortcut**: unconditional method uses (very common) reduce the ctx-augmented test to rule disjointness already computed by `genDisjointSet` — answer without SAT.
Tier 2:
- **Batch rule-predicate assertion**: new `checkDisjointExprListWithCtx` pushes ctx once per rule pair (Yices push/assert, then per-pair assert/check/pop) instead of `aAnd` per method pair — exactly what the DisjointTest.hs:87-96 XXX proposes. Keep the two-stage strategy (ctx-free test first; those results cache globally).
- Lazy refinement (only test pairs whose conflict edge affects a warning/urgency outcome) — punt unless needed.
Verify: `testsuite/bsc.scheduler/sched-conditions/` + full bsc.scheduler + the `0f50f1ab`-regolded tests; schedules and warnings must be bit-identical (memoization only; `Nothing`/unknown SAT results stay `Nothing`).

### WS2 — verilog (files: AVerilog.hs, Verilog.hs)
Tier 1:
- **2a fix**: in `renameInoutPorts`, compute `renames` from ports first (cheap); when empty (no `VAInout` needing rewrite — the overwhelmingly common case) return `vm` with only the ports-local `plain` fixup; skip `id_count` listify and the `everywhere` rebuild entirely.
- **2b fix**: per-module gating in `renameSVStdIdentsInVModule` — leading cheap scan via existing `vModuleDeclVIds` (spine-only, sound per its documented invariant); return module untouched if no std-package hit; skip `mapRenameableVIds` rebuild when `ren_map` is empty; collect G0131 reserved-word warnings from per-module decl scans instead of the whole-program `Generic.listify isReservedVId` (watch warning-position parity in goldens; fall back to per-tripped-module listify if positions drift).
Tier 2:
- Fold the 2–3 SYB listify scans into one hand-written traversal reusing `collectRenameableVIds`; move G0132 renaming to emit time via `PPrint VId` (like reserved-word escaping already is).
- 2c: fast pre-filter before the `isVReservedWord` Set lookup in `PPrint VId` (only if WS0 profiling shows it matters).
Verify: `testsuite/bsc.verilog/svkeywords/` (G0131/G0132/G0133 inventory identical), inout tests from `8458d046`, `.v` goldens byte-identical, #19126 repro cliff gone (trip ≈ notrip).

### WS3 — dict/serialization cluster (files: IType.hs, LiftDicts.hs, FixupDefs.hs, bsc.hs, GenBin.hs/BinData.hs)
Tier 1:
- **3.1 O(1) `rnf` for interned ITypes** (do first — re-baselines everything): guarantee deep NF at intern time (constructors already force keys; children interned ⇒ NF by induction), then `instance NFData IType: rnf t = t `seq` ()` for interned nodes. Audit IKind similarly.
- **3.2 LiftDicts pre-conversion fast path**: add `dictPoolC :: M.Map CType [(CExpr, Id)]`; structural CSyntax hit returns existing Id with **zero conversion**; miss → `convDict` once, fall through to existing interned pool (still required: distinct CExprs can convert equal). Keep incoherent dicts off the fast path (module note at L58-66 explains why type-only keying is unsound — the evidence check stays).
- **3.3 fixup redirects hoisted**: compute `evmap`+`mkRedirects` once per compile next to `mkDictBuckets` in bsc.hs; thread the redirect map into `fixupDefs`/`updDef` instead of recomputing per synthesized module; skip both `redirectDictProps` passes when the map is empty.
Tier 2 (only if binary/imports regressions survive 3.1):
- Shallow-force at the `DFbinary` dump when not actually dumping; lazy/offset decode of `ipkg_atf_cache` + `DefP_Dict*` .bo sections; intern types at read time (mirror of writer sharing `4ed037c8`); gate `hasPoisonPill` walk when poison pills disabled.
Verify: cross-package dict-dedup tests from `7e367a69`/`967a6b8f`, `-trace-lift-dicts`/`-trace-drop-dicts` parity (identical lifted defs and dedup decisions), full testsuite, `+RTS -s` on SimR2/TileSS repros.

### WS4 — typecheck/ATF (files: TCMisc.hs, TIMonad.hs, Pred.hs, Subst.hs, CtxRed.hs)
Tier 1:
- Hoist duplicate `findCls` (pass the `Class` into `mkATFClassPred`; 3 call sites).
- Mint fresh tyvars only for uncovered class params (`[0..n-1] \\ (tIdx:pIdxs)`) instead of all params.
- Lazy `PredAncestor` chains (consumed only by `ContextErrors` on the error path; already inert for solving per Pred.hs:92-96).
- **Free-variable cache on VPred** (user-requested): `data VPred = VPred Id PredWithPositions` (TIMonad.hs:462) gains a cached free-tyvar set (as a strict Set field or memoized inside PredWithPositions; use a pattern synonym to keep the ~all existing `VPred i p` match sites compiling). Consumers: `split_rs.affected_pred` (TCMisc.hs:194-200) currently recomputes `tv r` via nub/union list math per predicate per solver round — becomes an O(min) set-vs-substitution-domain intersection; `apSub` on a VPred first checks `domain(s) ∩ fv = ∅` and returns the **original** predicate untouched. Mirrors the interned-IType FTV cache (`f6989da8`) at the predicate level, but on transient values (no immortal-table residency concern).
- **Sharing-preserving "changed" apSub** (user-requested): implement the `Types` class (Subst.hs:199-227) via an internal worker returning `Maybe t` (`Nothing` = untouched); unchanged nodes/lists keep pointer identity, and `normTAp`/`expandSyn` (Subst.hs:218, Pred.hs:170 — both currently re-run on every substitution over every node) execute only on genuinely substituted subtrees. Applies to every `apSub` call in the satisfy loop (`apSub s0 es`/`apSub s0 ps` per `satisfyXStream` entry, TCMisc.hs:185; `apSub s es` in `batchSolveNumericPreds`, `apSub s sbs`; 49 call sites in TCMisc alone) and to substitution composition (`Map.adjust (seqCType . apSub this_sub)`, Subst.hs:107). Kills the dominant rebuild churn behind typecheck alloc +24% / wrapper_typecheck +35.6% beyond the ATF-specific fixes, and preserves sharing (residency).
Tier 2:
- Per-satisfy-session ATF constraint-synthesis memo `M.Map (Id,[Type]) (VPred,Type)` alongside the existing `tsATFCache` — inspect substitution/backtracking interaction (`tiRecoveringFromError`) first; heap-profile the `925b29be` retention claim before any deeper restructuring.
Verify: ATF tests from `add2e991`, coherence tests from `925b29be` (T0158 text/positions exact), parity runs under `-legacy-defer-instances`/`-legacy-inst-index`; for the fv-cache/changed-apSub, full `bsc.typechecker/` — results must be bit-identical (pure sharing/skipping; the fv set must be maintained through every constructor and substitution or lookups go stale — audit `mkVPred`/`mkVPredNoNewPos`/`mkVPredFromPred` and all VPred producers).

### WS5 — symbol table (files: MakeSymTab.hs, bsc.hs)
Tier 1:
- **Leaf-scoped `cmpMemo`**: build the instance-comparison memo only over pairs sharing a PredTrie leaf instead of the eager O(n²) spine over all instances of a class — collapses the common non-overlapping case (hundreds of `Bits` instances) to ~O(n). 
Tier 2:
- Reuse unchanged per-class tries/overlap results across the 5 `mkSymTab` rebuilds (key on the class's `QInsts` identity); or skip `overlapErrors` on rebuilds where the instance set provably didn't change (e.g. sympostctxreduce). Must not miss genuinely new overlaps (EBadInstanceOverlap/EDuplicateInstance).
Verify: overlap/duplicate-instance error tests from `05865c6f`, instance-resolution order goldens, `-legacy-inst-index` parity.

### WS7 — beyond-parity optimizations (make it significantly FASTER/LEANER than OLD)
Ranked by measured-cost × risk (citations verified at 88e8822):
1. **GHC RTS tuning (trivial, ~10-25% wall)**: GC is ~22% of all CPU (5038s/23233s). bsc is built `-O2 -rtsopts` with baked `-with-rtsopts=-H256m -K10m -i1` (src/comp/Makefile:195-197,228) — the allocation area `-A` is at GHC's small default, forcing very frequent minor GCs. Pass `+RTS -A64m -n4m -I0 -RTS` per action from the **matx side** (rules.bzl bsc invocation — the designated matx branch carries this) and bump the baked default in the bsc Makefile as fallback. Tune `-A` (64m vs 128m) on the biggest targets; per-action memory cost (~64MB) is negligible vs the caps.
2. **Evaluator: stop computing `aVars` per beta-reduction (low risk, expanded = 20.6% of CPU)**: every `eSubstBatch` entry seeds capture-avoidance `allIds = fvx ∪ aVars e` (ISyntaxSubst.hs:440,449,...) — a full body traversal + Set build per application — but in the evaluator the substituted args come from `toHeap` (`ICon`/`IRefT`, both `fVars = ∅`, ISyntax.hs:999-1000), so the alpha-conversion branch never fires. Make `allIds` a lazy thunk forced only on actual capture. Validate on `origin/gnezdo/grid-elab-slow-repro`'s GridTest repro + MXUTest (expanded 224s).
3. **Text/ByteString Builder Verilog output (~5-15% wall + residency)**: `vstring = comment ++ pp80 vprog` builds the whole file as `String`, then `writeVFileCatch` does a second full `lines/unlines` pass, then `hPutStr` pushes it **char-by-char** through the UTF-8 encoder (bsc.hs:1259-1309, FileIOUtil.hs:188-191). Add a Builder-based render (fold `dropTrailingBlanks` in), write via `hPutBuilder`. Incremental first step: keep `pp80`, kill the double pass + char-wise write.
4. **Builder for .bo/.ba writes (~3-8%)**: `encodeWith` accumulates output as a lazy `[Word8]` then `concat`+`B.pack` (BinData.hs:1545-1557, FileIOUtil.hs:245-250). Replace the byte-list accumulation with `ByteString.Builder`; keep the DList/sharing/compress layers. Must be byte-identical (format tag `bsc-bo-20260715-6`).
5. **Gate phase-boundary deepseq (~2-8% CPU)**: `dump`/`ddump`/`vdump`/`sdump` (TopUtils.hs:76-96) deepseq their argument **unconditionally** across ~50-70 boundaries per verilog compile, before any flag test. Gate on `verbose flags || isJust (dumpInfo flags d)`. Caveats: error-attribution moves downstream (acceptable, document); peak residency can *rise* where forcing collapsed thunk chains — A/B the 7.7GB targets before landing; synergizes with WS3.1 (the remaining forced walks become O(shared) cheap).
6. **Cached free-var sets on IExpr + substitution pruning (medium risk, high value)**: `tSubstWith` already prunes via interned-type FTV caches (`ctxAvoids`, ISyntaxSubst.hs:255-257) but `eSubstWith` has no equivalent — expression substitution is unconditionally O(body). Mirror the mechanism for `IExpr` (lazy FV annotation or via interning). Subsumes item 2 when done.
7. **Cherry-pick ready-made elaboration work** from unmerged branches (verify each with `--cherry-mark`; the PredTrie/typecheck-perf branches are **already merged** into 88e8822 — confirmed, don't re-pick): LiftDicts CSE stack (`47556d95`+follow-ups on origin/atf-cache; medium risk, golden churn), `66a2a7e1` PrimStringSplit direct result type, grid-elab cleanups (`d4d44d22`, `405a32f0`, `c08d74de`).
8. **IExpr interning (strategic, stage last)**: `eqE`/`==`/`Ord` on IExpr are deep structural (ISyntax.hs:463-531) and ITransform's simplification does deep `eqE` on nested `PrimIf` branches through `ICValue` (ITransform.hs:313-425, 1400-1402) — the plausible driver of the 389s transform outlier (VpuBkwdBeltTest). Interning gives O(1) equality + a home for item 6's FV sets. High effort; profile the outlier first (land `origin/s.alejandro/bsc-prof` profiling infra or `BSC_BUILD=PROF` as the enabler).
9. Deprioritized (checked, not worth it): Id/FString comparisons are already O(1) interned ints (SpeedyString.hs:22-23); `-threaded` parallel-GC rebuild is a separate high-risk experiment.

### WS8 — critical-path pipelining: split elaboration from Verilog codegen (user-requested; stretch −50% critical path)
Today a BscV bazel action serializes elaborate→schedule→AState→verilog-emit,
and dependents wait on the whole thing. But downstream compilation only needs
the .bo/.ba (elaboration products) — .v emission is a pure leaf. Splitting
them turns the bazel critical path into the elaboration chain only, with all
codegen fanned out in parallel (on ComparePostSrbInstr alone, verilog 66s +
writeVerilog 113s of 490s leaves the critical path).
- bsc side: rc8 already has `-elab`/`genABin` (FlagsDecode.hs:1232). The
  unmerged branch `origin/verilog-codegen-stage` (22 commits, 2026-07-02)
  implements the full feature: ".ba written by default for the Verilog
  backend" (`1eae209b`), `-c` codegen mode replacing `-block-codegen`
  (`2e368e71`, root-only `fcd78a59`), stale-.ba handling (`d8ecc22a`), and
  **byte-identity sweep tests** proving split output == monolithic output
  (`57d0689f`, `175e264f`, `601e6892`). Plan: rebase/merge this branch onto
  the fix branch (it's 22 commits on a recent base), re-run its byte-identity
  suite, and fix conflicts with WS2/WS7 changes.
- matx side: split the `_bsc` BscV action in `rtl/bluespec/rules.bzl` into an
  elab action (produces .bo/.ba; dependents depend on this) and a codegen
  action (.ba→.v via `-c`), with scheduler memory estimates split accordingly.
  Gate behind a build flag initially (`--//rtl/bluespec:split_codegen`) for
  A/B, then default on.
- Acceptance for this WS: byte-identical .v vs monolithic on all 2314 BscV
  actions; bazel critical path (from build-event log / `--profile`) reduced;
  no scheduling regressions from doubled action count (tune local-scheduler
  memory reservations).

### WS6 — residency residuals (data-driven, last)
Mostly falls out of WS2a (sharing), WS1 (proofMap), WS3.1 (rnf), WS4 (PredAncestor). Remaining: FTV-cache policy per WS0 measurement — bound/lazify the `!VarSet`, or accept + document (it buys tSubst pruning), with `-no-itype-ftv-cache` as a supported per-target lever. Intern-table immortality is accepted design (per-action compiles bound its lifetime).

## Sequencing

```
Step 0 (branch reset + experiments)
→ PR1 WS2a inout guard            (independent; biggest single win, lowest risk)
→ PR2 WS2b/2c SV legalization gating (independent)
→ PR3 WS3.1 O(1) IType rnf        (independent; land BEFORE re-measuring bucket 5)
→ PR4 WS1 scheduler caching       (independent)
→ PR5 WS3.2 LiftDicts fast path   → PR6 WS3.3 fixup hoist (same-file stack, in order)
→ PR7 WS4 ATF tier 1 + fv-cache/changed-apSub  (independent)
→ PR8 WS5 leaf-scoped cmpMemo     (independent)
→ PR9 WS7.1 RTS tuning            (matx branch: rules.bzl +RTS flags; bsc branch: Makefile baked default)
→ PR10 WS7.2 lazy aVars           (independent, validate on GridTest repro)
→ PR11 WS7.3/7.4 Builder output   (verilog first, then .bo/.ba; byte-identical outputs)
→ PR12 WS7.5 gated deepseq        (after PR3; A/B peak residency on worst targets)
→ PR13 WS8 elab/codegen split     (bsc: merge origin/verilog-codegen-stage +
                                   byte-identity suite; matx: split _bsc rule
                                   behind --//rtl/bluespec:split_codegen)
→ re-measure full A/B → if not ≥10% better than OLD on wall/CPU/residency/
  alloc/critical-path, continue down the WS7 backlog (7.6 IExpr FV pruning,
  7.7 cherry-picks, 7.8 IExpr interning) until the targets are met
```
One logical fix per commit; message cites the phase and micro-bench delta.
Upstream candidates (B-Lang-org/bsc): WS1 (attach to upstream #1056), WS2a/2b,
the DisjointTest XXX fix; keep upstreamable commits separated from
matx-lineage-only ones.

## Verification / acceptance

- Per-PR: relevant testsuite dirs + full testsuite before merge; warning
  inventories identical (G0131/G0132/G0133, T0158, scheduler warnings); `.v`
  goldens unchanged; micro-bench target recovered (goal ≤1.1× OLD per phase:
  schedule worst-case, #19126 repro, SimR2 binary, TileSS liftdicts,
  wrapper-heavy typecheck, ScalarUnit residency).
- Final: publish a prerelease via the existing `matx-prerelease` workflow;
  run `matx/tools/bench_bs_master.sh` (5-round median) OLD vs fixed-NEW.
  Accept when **significantly better than OLD**: bazel wall ≤ −10% vs OLD;
  total CPU ≤ −10%; peak max-residency ≤ −10%; **sum heap alloc ≤ −10%**;
  every previously-regressed phase at or below OLD (no worst-case action
  > +10% of its OLD time); **no regression in any critical-path (long-pole)
  target** — every action in the report's top-10-by-OLD-CPU list
  (TAControllerScalarUnit BscV+BscBa, ComparePostSrbInstr, VpuBkwdBeltTest,
  TileStubGrid, MXUTest, TileGrid, ControllerSimdUnitTest,
  TAInstrAssemblerCosimTest, REControllerScalarUnit) must be at or below its
  OLD CPU and residency; **bazel critical path ≥10% shorter than OLD**
  (measured via build-event log/`--profile`), with WS8's elab/codegen split
  targeting a stretch reduction toward −50%; NEW's wins preserved (expanded
  −27%, writeBin −51%); zero behavioral diffs in generated .v/.bo (for WS8,
  split-mode .v must be byte-identical to monolithic). Headroom math: NEW is +15.6% wall /
  +3.4% CPU / +34.7% residency over OLD; regression fixes recover that, and
  WS7 (GC/RTS tuning at ~22% GC share, phase-boundary deepseq removal,
  elaboration/imports headroom) provides the additional ≥10-15% below OLD.
  If after WS7 the −10% targets are not met, iterate on the ranked WS7
  backlog (largest remaining phase first) rather than declaring done. Then
  revert matx's `ComparePostSrbInstr` `memory_multiplier` 8→4 in a follow-up
  matx PR as the memory proof-point.

## Deliverables on the designated branches (full execution: Tier 1 + Tier 2)

- `MatX-inc/bsc` branch `claude/bsc-18772-perf-regressions-9vi622` (reset onto
  `88e88222`): this plan document committed as
  `doc/perf/rc8-regression-fix-plan.md`, then ALL Tier-1 fix commits per the
  sequencing above, then Tier-2 commits (ctx-batched SAT assertion, single-pass
  SV scan/emit-time renaming, ATF constraint-synthesis memo, lazy .bo section
  decode, symtab trie reuse) — each Tier-2 item implemented conservatively and
  individually revertable; where a Tier-2 item's soundness precondition fails
  during implementation (e.g. ATF cache vs substitution), document why it was
  skipped rather than shipping a risky version.
- Verification in-session: build bsc from source in this container, run the
  targeted testsuite dirs per workstream plus micro-bench repros (#19126
  genr.py; extracted schedule/liftdicts cases). Full 2131-action matx A/B via
  bench_bs_master.sh is a follow-up on real build hosts (documented in the
  plan doc).
- `MatX-inc/matx` branch of the same name: the WS7.1 RTS tuning flags in
  `rtl/bluespec/rules.bzl` (`+RTS -A64m -n4m -I0 -RTS` on bsc invocations,
  coordinated with the existing per-action heap caps) and the WS8 split of the
  `_bsc` BscV action into elab + codegen actions behind
  `--//rtl/bluespec:split_codegen`. The bsc pin bump + `memory_multiplier`
  8→4 revert happen after a fixed bsc build is published; noted in the plan
  doc as follow-up.

## Best measurement targets per change

Per-action phase data from the PR #18772 benchmark (matx targets; measure with
`-show-timestamps -v` phase times and `+RTS -s`, or the bazel
`--//rtl/bluespec:profile` attribute). "Best" = largest, cleanest signal for
that specific change.

| Change | Best targets (primary first) | Signal to watch |
|---|---|---|
| WS1 scheduler | `rtl.tile.vpu.VpuRecvBB` (schedule 0.77→30.53s, most extreme ratio); `rtl.controller.dma.TAFrontierDMAEngine` (18.9→129.4s); `rtl.controller.dma.REFrontierDMAEngine`; `rtl.rate_limiter.re_io_sch.ReIOSchedulerRateLimiter` (14.7→94.4s); median case `rtl.controller.TAControllerScalarUnit` (schedule 101→133s) | schedule phase CPU; sched-phase GC time |
| WS2a inout guard | `rtl.bs_sv_validation.ComparePostSrbInstr` (verilog 9.5→66.1s, resid 3.5→7.7GB); `rtl.bs_sv_validation.BroadcastTileInstrSelfChecker` (verilog 3.1→32.1s); synthetic: matx#19126 `genr.py 2000 notrip` | verilog+verilogDollar phases; peak residency |
| WS2b SV legalization gate | matx#19126 `genr.py 2000 trip` vs `notrip` (cliff must vanish); `rtl.bs_sv_validation.ComparePostSrbInstr`; `rtl.controller.test.BroadcastTileInstrCheck` (verilog alloc 0.86→14.4GB) | verilog phase time+alloc with a `process` identifier present |
| WS2c PPrint VId | `rtl.bs_sv_validation.ComparePostSrbInstr` (writeVerilog 89.6→112.8s) | writeVerilog phase |
| WS3.1 O(1) IType rnf | `rtl.SimR2` BscBa (binary 1.43→30.76s, +2051%); `rtl.controller.TAControllerScalarUnit` (internal 1.09→19.39s, +1679%) | binary + internal + fixup phases |
| WS3.2 LiftDicts fast path | `rtl.tile.TileSS` / `rtl.tile.TileSSColumn` / `rtl.tile.stub.TileStubSS` / `rtl.tile.TileSS3x3` (liftdicts alloc 0.27→27.5GB each) | liftdicts phase alloc |
| WS3.3 fixup redirects hoist | `rtl.controller.TAControllerScalarUnit`; `rtl.SimR2` (widest import closure) | fixup phase |
| WS4 ATF/typecheck + fv-cache/changed-apSub | `rtl.controller.TAControllerScalarUnit` (typecheck 153→213s); TileSS family (typecheck alloc +68%); wrapper_typecheck: `rtl.tile.stub.TileStubSS` (49→118s), `rtl.tile.TileSS` (75→118s), `rtl.tile.mxu.MXUSaCellGrid` (+70%) | typecheck + wrapper_typecheck CPU and alloc |
| WS5 symtab | `rtl.SimR2` (largest package); any import-heavy controller test (`rtl.controller.test.TAControllerDMAC2CTestScriptGen`) | syminitial + sympost* phases |
| WS6 residency | `rtl.bs_sv_validation.ComparePostSrbInstr` (7.7GB); `rtl.controller.TAControllerScalarUnit` (7.2GB); `rtl.controller.REControllerScalarUnit`; `rtl.tile.TileSS` | `+RTS -s` max residency |
| WS7.1 RTS -A tuning | global; spot-check `rtl.controller.TAControllerScalarUnit`, `rtl.bs_sv_validation.ComparePostSrbInstr`, `rtl.tile.belts.vpu.test.VpuBkwdBeltTest` (GC-heavy) | GC time share (%GC in +RTS -s); wall |
| WS7.2 lazy aVars (expanded) | `rtl.tile.mxu.test.MXUTest` (expanded 224s); `rtl.controller.test.TAControllerDMAC2CTestScriptGen` (expanded 86→109s); GridTest repro on `origin/gnezdo/grid-elab-slow-repro` | expanded phase |
| WS7.3 Builder Verilog output | `rtl.bs_sv_validation.ComparePostSrbInstr` (writeVerilog 113s); `rtl.tile.stub.TileStubGrid` | writeVerilog phase; residency |
| WS7.4 Builder .bo/.ba | `rtl.SimR2` (writeBin largest); `rtl.controller.TAControllerScalarUnit` | writeBin/writeABin phases |
| WS7.5 gated deepseq | `rtl.controller.TAControllerScalarUnit` (crosses most phases); guard: residency must not rise on `ComparePostSrbInstr` | total CPU; peak residency A/B |
| WS7.8 IExpr interning/eqE | `rtl.tile.belts.vpu.test.VpuBkwdBeltTest` (transform 389s); `rtl.controller.test.TAInstrAssemblerCosimTest` (transform 151s) | transform phase |
| WS8 elab/codegen split | full `//rtl/...` build critical path (bazel --profile); per-action: `rtl.bs_sv_validation.ComparePostSrbInstr` (verilog+writeVerilog ≈179s leaves critical path); grid chain `rtl.tile.TileGrid` → `emulation.gate_count.tile.TileGrid_16x32` | bazel critical path length; byte-identity of .v |

## Implementation status (2026-07-16, branch claude/bsc-18772-perf-regressions-9vi622)

Landed (bsc, this branch — one commit per item):
- WS2a/2b verilog SYB gating; WS3.1 O(1) interned-IType rnf; WS1 scheduler
  (ctx-tuple memo + unconditional-use shortcut, Yices+STP); WS3.2 LiftDicts
  pre-conversion pool; WS3.3 fixup redirect hoist; WS4 (findCls hoist, fewer
  ATF tyvars, VPred fv cache for split_rs, sharing-preserving Type apSubM,
  lazy PredAncestor forcing); WS5 leaf-scoped instance comparisons; WS7.1 RTS
  defaults (-A64m -n4m -I0); WS8 verilog-codegen-stage merge + new
  -elab-only flag.  (WS7.5 gated deepseq was landed and then REVERTED:
  gating the force on -v made evaluation order -- hence string-intern
  order, Map-over-Id iteration order, and emitted output order -- depend
  on verbosity.  Identical flags must give identical bytes.  Revisit only
  with intern-order-free Id ordering; the O(1) IType rnf already removed
  most of the forcing cost.)

Landed (matx, same branch name): +RTS -n4m -I0 on bsc actions;
--//rtl/bluespec:split_codegen (default off) splitting BscV into
BscVElab/BscVCodegen.

Measured on the #19126 2000-register repro vs stock build-20260715-1:
verilog phase notrip 0.37s -> 0.14s, trip 1.03s -> 0.57s (G0132 renames
intact); typecheck at parity (45s); split flow .v byte-identical modulo
timestamp comment.  Scheduler micro-bench (150 rules, shared register,
distinct predicates): schedule 0.54s -> 0.32s.

Learned the hard way (do not re-attempt without new evidence):
- WS7.2 (lazy aVars) was skipped on inspection: allIds is already passed
  lazily and forced only in the alpha-conversion branch, which the
  evaluator's heap-ref arguments never trigger.
- The Types [a] instance must stay LAZY: eager whole-list changed-detection
  doubled typecheck on long assumption lists.
- substDomainDisjoint must probe per-variable; Map.keysSet per call is
  O(subst size) and also doubled typecheck.
- apSub on a Pred doubles as an expandSyn normalization pass: skipping the
  rebuild when the substitution changed nothing broke instance resolution
  (T0031 in bsc.typechecker/PrintfATS.bs).  Whole-pred sharing therefore
  requires first making construction sites expandSyn-normalized — future
  work; per-element type sharing is kept.

Remaining backlog (in priority order): WS7.3/7.4 Builder-based .v/.bo
output; WS4 Tier-2 ATF constraint-synthesis memo; WS5 Tier-2 symtab
trie/overlap reuse across rebuilds; WS7.6/7.8 IExpr fv-cache/interning;
full 2131-action A/B via tools/bench_bs_master.sh on real build hosts
(publish a prerelease with .github/workflows/matx-prerelease.yml), then the
matx pin bump, memory_multiplier 8->4 revert, and split_codegen default-on.

---

# Per-change benchmark matrix (rc8 fix stack)

Environment: 4-core shared container, GHC 9.6.7, one full `make install-src`
tree per commit (`88e88222` rc8 base built in the same environment for
uniformity). Method: per row, 3 interleaved PRE/POST pairs, medians of all
four axes; phase CPU from `bsc -v`, total/alloc/max-residency from `+RTS -s`.
Output gate: `.v` compared modulo the version-header comment; `.bo` compared
with both binaries pinned to the PRE install's library (imported-`.bo`
hashes differ per install tree — verified provenance-only). Box noise ±10%
between batches; comparisons are only valid within a row, never across rows.
Workload generators live in the session scratchpad and are inlined in the
handoff doc; shapes are lifted from the matx targets named in the perf report.

## Measured rows (adjacent commits; PRE → POST medians)

| Row / commit | Workload | Target phase | Total CPU | Alloc | Max res | Output gate |
|---|---|---|---|---|---|---|
| verilog `ba2d13e4` | 2000-reg + `$`-ident cliff | verilog 1.27→0.74s (**0.58×**) | 47.4→40.7s (−14%) | −1% | flat | clean |
| itype `cafa5243` | synonym-DAG D=16, 4 importers | internal/binary 15.3→14.3s (0.94×) | 120.3→116.3s (−3%) | identical | flat | clean |
| sched `f8b55444` | 800 rules, distinct preds, const writes | schedule 32.0→28.1s (**0.88×**) | 44.6→41.1s (−8%) | −3% | +3% | `.v` = version header only |
| ws4 `de4e7868` | proviso-heavy (1200 regs, depth-5 Vector) | typecheck 71.4→44.0s (**0.62×**) | 72.8→45.2s (−38%) | **−25%** | flat | clean |
| symtab `fd0b22f1` | 4000 same-leaf instances | syminitial 34.9→41.3s (1.18×) | 168.8→214.4s (+27%) | **+175%** | **−95%** (822M→43M) | clean |
| rts `33dcf06c` (defaults) | proviso-heavy | typecheck 36.8→43.9s (1.19×) | +19% | flat | +4% | `.bo` header-only |
| rts2 `33dcf06c` (defaults) | 2000-reg register chain | typecheck 79.6→61.3s (**0.77×**) | −23% | flat | **−22%** (2.09→1.63G) | n/a |
| wsa `6a3441c7` | proviso-heavy | typecheck 45.8→47.3s (1.03×) | +3% | flat | flat | `.bo` differs (expanded preds — intended) |
| wsb `be4fd993` | proviso-heavy | typecheck 47.6→49.6s (1.04×) | flat | −0.4% | +1% | clean |
| **bracket** `88e88222→be4fd993` | proviso-heavy | typecheck 66.8→55.5s (**0.83×**) | −16% | **−25%** | +2% | — |
| split `03005593` | 800-rule sched shape | see split section below | | | | `.v` byte-identical |

## Split row (`-elab-only` + `-verilog -c`, binary `03005593`)

Same binary, same workload, medians of 3: monolithic `-verilog -g` wall
**110.8s**; `-elab-only` wall **103.4s (0.93×)**; `-c` codegen wall
**30.2s**. Split `.v` is byte-identical to monolithic `.v` (modulo the
version header; timestamps suppressed). Reading for the matx build graph:
downstream module elaborations depend only on elab outputs, so the
per-module critical path shortens ~7% on this shape while the codegen wall
(27% of the monolithic run) moves off the inter-module chain into
parallelizable actions; total CPU rises ~20% (the `.ba` round-trip), the
intended trade. Codegen-heavier real targets shift the ratio further toward
the split; the ≥10%-critical-path goal is expected to be met on long matx
chains and must be confirmed by the bazel replay / branch CI.

Aggregate reference (measured earlier, same box): stock rc8 vs branch tip on
the combined register repro: wall 55.3→49.5s (−10%), max residency
2.01→1.51G (−25%); verilog cliff phase 0.37→0.11s; and on Rn.bs (2000 regs)
tip-vs-WS-B interleaved: parity on all axes.

## Honest notes per row

- **verilog**: the 2× phase target is met (0.58× phase); the −14% total
  shows the commit's gating helps several output phases. Phase absolute is
  small (~1s) because larger modules make non-target phases dominate the run.
- **itype**: no honest 2× workload found in-container. The structural-rnf
  walk never dominates a feasible synthetic shape: linear types are too
  small, synonym-DAG shapes are swamped by a *separate pre-existing*
  CType-side exponential that hits both binaries equally (see Findings).
  Primary evidence remains the aggregate residency (−25%) and the report's
  binary/SimR2 rows (+70%/+2051%) — to be confirmed by bazel replay.
- **sched**: 0.88× on the constant-write shape (weaker than the 0.59×
  measured on the dev repro where predicates carry arithmetic); the SAT
  short-circuit depends on predicate structure. Both numbers reported.
- **symtab**: mixed on the adversarial shape (4000 instances in one trie
  leaf): +18% phase CPU / +175% alloc, but −95% max residency — the dropped
  eager memo was retaining 800MB. On the report's actual targets the sympost
  regressions this commit fixes dominate. PR should present both and
  consider a bounded memo if a real target ever shows the adversarial shape.
- **rts**: workload-dependent, both directions (−23% churn-heavy, +19%
  residency-heavy). matx bazel rules pass explicit per-action RTS flags, so
  the baked default only governs bare invocations. Recommend keeping
  `-A64m -n4m` and treating `-I0`/nursery size as tunable in the PR.
- **wsa/wsb**: +3%/+4% on the proviso shape. WS-A's value is soundness
  (construction-time normalization invariant, checker-enforced) and
  enabling WS-B; WS-B's is architectural (universal null-subst shortcut,
  one shared Changed machinery, precise dirty-bit forcing that fixed the
  +52% residency regression measured mid-development). Neither shows a
  standalone CPU win on available workloads; the typecheck stack as a whole
  is the unit that wins (bracket row: −17% CPU / −25% alloc vs rc8).
- **liftdicts `ab75cae4` / fixup `98282c71`**: no row — the liftdicts phase
  stays <2s at every feasible synthetic scale (single- and cross-package
  shapes tried); the fixed cost is per imported dictionary occurrence at
  matx package scale. Evidence: in-session directed measurement during
  development and the report's liftdicts/fixup rows; validate via bazel
  replay.
- **codegen-stage merge `9f049adb`**: does not build standalone (GenABin
  ByteString mismatch; fixed in `03005593`) — row pairs `33dcf06c→03005593`
  and conflates the merge with `-elab-only` (additive flag). Fold the fix
  into the merge commit when preparing the upstream PR.

## Findings surfaced by the matrix (not regressions in this stack)

1. **CType/TItype symtab exponential** (pre-existing; OLD = rc8 = tip):
   synonym `TyCon`s embed their RHS (`TItype`), so doubling-synonym chains
   are DAGs walked as 2^D trees by MakeSymTab kind-checking (`chkTAp'`,
   ~45% of profile), structural CType rnf (~15%), and
   `setTypePosition`/`splitTAp` rebuilds. Repro: `gen-itype.py` D=18
   (~15 min compile, 4.2TB alloc, both releases). Candidate follow-on fix:
   the IType playbook (strictness → sharing/caches) applied to CType, or
   one-level synonym kind-checking.
2. **`-K10m` baked stack limit**: a 1400-rule module dies of stack overflow
   in `adropdefs` (pre-existing; stock rc8 identical). Benchmarks pass
   `-K256m`; real matx targets may want the same in rules.bzl.
3. **`.bo` embeds imported-library hashes**: byte-comparing `.bo` across
   binaries requires pinning one library install; same-binary compiles are
   fully deterministic.
