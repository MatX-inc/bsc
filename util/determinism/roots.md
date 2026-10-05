# Root ids

The classifier's roots table for the release-devel-B0 line, from the
2026-10-05 baseline: bsc 3be09aa3 (det/gate-parallel = release-devel-B0
4b086bb5 + the AState/InlineCReg port-spelling fold + the ported
-reverse-intern-order switch + the stableOrdNub fix of
instPortMapCollisions), the full testsuite run normally and with
-reverse-intern-order in `TEST_BSC_OPTIONS` (both passes in temporary
worktrees with `-remap-path-prefix`) and every generated file compared
byte for byte with `compare.py`.  The suite's goldens see 172 test
results that fail only under the flag (0 fail only without it; 55 more
tests fail in both passes because `-remap-path-prefix` changes how
positions read back from a .bo/.ba print, see README.md); the bytes see
13403 allow-list entries.

One row per `root_id` used in `allowlist.txt`.  The root ids and the `site`
column are the upstream line's (2026-10-03 baseline, bsc 3fe6aaa3), carried
over by path: an entry that differed there with a root keeps that root here,
and the `site` cites are that line's `src/comp` lines, not this one's.  The
counts are this baseline's: live differing files of each kind the root
accounts for (v = .v/.sv; cxx/h = .cxx/.h/.c; ba = .ba and its dumpba text;
bo = .bo/.bi and its dumpbo text; other = transcripts, harness diffs, dumps,
bluetcl output).  `UNCLASSIFIED` (7960 entries) are files that differ on
this line and not on the upstream baseline: release-devel-B0 lacks every
determinism fix det/base-b0 and det/all-stars carry (scheduler use order,
tsort ties, backend def order, BinData Map/Set encoding, the typechecker
orders, ...), so these are mostly the roots those fixes closed, plus files
new to this line's testsuite (bsc.verilog/instports among them); they are to
be classified as the IdMap and fingerprint streams take them.  Roots marked
`(noise)` are not compiler ordering and are covered by `ignore.txt` instead.
The counts go stale as the list shrinks; the ids and sites do not.  Against
the 2026-10-04 table (bsc 71e1b19b, 13517 entries) the 'other' column lost
the 124 `*.filtered` copies a harness bug left behind in serial runs and
four noise transcripts, and UNCLASSIFIED gained the 14 instports files.

| root | v | cxx/h | ba | bo | other | total | site |
|---|---|---|---|---|---|---|---|
| UNCLASSIFIED | 150 | 70 | 2051 | 5439 | 250 | 7960 | differs on this line only; see above |
| new-simcopt-sched-local-decl-order | 0 | 1082 | 0 | 0 | 0 | 1082 | SimCOpt.hs:181-189 (move_map = M.fromListWith (++) [... \| ((sbid,aid), |
| new-ba-dump-ruleuses-map-order | 0 | 0 | 962 | 0 | 0 | 962 | AUses.hs:205-233 (toListMethodExprUses/toListMethodActionUses/toListFF |
| pending-ba | 0 | 0 | 860 | 0 | 0 | 860 |  |
| me-pairs-ordpair | 132 | 383 | 268 | 0 | 10 | 793 | ASchedule.hs:2531 (nub (map ordPair (concatMap mkAllPairs sps)) in ext |
| genwrap-ppmap-order | 0 | 0 | 0 | 607 | 109 | 716 | GenWrap.hs:310 (moduledefs <- concatMapM (getDef generating ds) (M.toL |
| iexpand-pconj-set-order | 63 | 20 | 164 | 0 | 5 | 252 | IExpandUtils.hs:265 (predToIExpr folds S.toList es) |
| rule-use-map-id-order | 0 | 0 | 27 | 0 | 132 | 159 | AUses.hs:460-469 (rumToMethodUseMap: M.Map Id (M.Map Id [UniqueUse]) b |
| linked-hash-transitive (noise) | 0 | 0 | 0 | 94 | 0 | 94 | GenBin.hs:37 (header ++ encode (bi_sig, bo_sig, ipkg)) and BinData.hs: |
| only-one-side | 1 | 0 | 0 | 0 | 74 | 75 |  |
| rule-before-methods-map | 0 | 0 | 39 | 27 | 3 | 69 | ASchedule.hs:3335-3339 (before = map_insertManyWith (++) ... M.empty;  |
| ifc-port-name-clash-sort | 0 | 0 | 0 | 0 | 50 | 50 | IExpandUtils.hs:2103 (ifc_names = sort $ ifc_port_names ++ ... : sorts |
| use-cond-set-order | 0 | 6 | 36 | 0 | 0 | 42 | AUses.hs:622-626 (UseCond: S.Set AExpr / M.Map AExpr) |
| bluesim-package-map-order | 0 | 0 | 0 | 0 | 40 | 40 | ABinUtil.hs:91,164 (m_abmis_used prepend) |
| itransform-runt-map-order | 0 | 24 | 10 | 0 | 1 | 35 | ITransform.hs:1441-1453 (runT emits M.toList cse_map ++ M.toList def_m |
| iexpand-eqptrs-dsm-elems | 0 | 0 | 0 | 0 | 32 | 32 | IExpand.hs:639 (eqPtrs returns (M.elems dsm, ptrm): dsm is M.Map HExpr |
| lc-updateapkgtypes-elems | 0 | 0 | 0 | 0 | 22 | 22 | LambdaCalcUtil.hs:788-800 (updateAPackageTypes: apkg_local_defs = M.el |
| static-sched-rule-pair-order | 0 | 0 | 3 | 0 | 17 | 20 | ASchedule.hs:4641 (separate $ checkOneRule (M.toList ruleMethodUseMap) |
| new-itransform-cse-rename-id-tiebreak | 0 | 0 | 18 | 0 | 0 | 18 | ITransform.hs:1704-1715 (cse_ids_map :: M.Map Id (S.Set (Int, Id)) bui |
| depend-map-order | 0 | 0 | 0 | 0 | 17 | 17 | Depend.hs:93-94 (chkDeps: tsort over DM.elems piMap) |
| new-cf-cond-wires-set-order | 0 | 0 | 16 | 0 | 0 | 16 | AAddSchedAssumps.hs:183 (newWires = concatMap (buildWireInsts ...) (S. |
| new-systemc-wrapper-port-map | 0 | 14 | 0 | 0 | 0 | 14 | SystemCWrapper.hs:140-145 (port_map, merged_port_map = M.fromList / M. |
| rule-use-map-instance-order | 0 | 0 | 13 | 0 | 0 | 13 | ASchedule.hs:1944-1960 (has_shadowing / WActionShadowing built from th |
| urgency-cycle-vertex-order | 0 | 0 | 2 | 0 | 9 | 11 | ASchedule.hs:4070 (vs = G.vertices umap = M.keys) |
| insttree-unsuffixed-id-order | 0 | 0 | 0 | 0 | 8 | 8 | Id.hs:265-274 (cmpSuffixedIdByName: suffix count 0 falls back to Ord I |
| new-heap-def-renumbering | 8 | 0 | 0 | 0 | 0 | 8 | src/comp/IExpand.hs:232-233, 302-312 (pDef names heap defs iExpandPref |
| new-fixupdefs-binder-position | 6 | 0 | 0 | 0 | 0 | 6 | src/comp/IExpandUtils.hs:2469-2472 (improveCellName: keeps the binder  |
| inlinereg-partition-by-clock | 4 | 0 | 0 | 0 | 0 | 4 | InlineReg.hs:38,44,114 (M.toList (partitionByClock / partitionByClockA |
| new-cf-assump-methcond-map-order | 0 | 4 | 0 | 0 | 0 | 4 | AAddSchedAssumps.hs:124 (mkCFAssump = concat $ M.elems overlapMap) |
| always-ready-exprmap-order | 0 | 0 | 0 | 0 | 3 | 3 | AAddScheduleDefs.hs:193 (handleAlwaysReady: map doRdy (M.toList pre_rd |
| insttree-vector-position | 0 | 0 | 0 | 0 | 3 | 3 | InstNodes.hs:345-347 (comparein: comparing getPosition first) |
| weak-context-tyvar-sort | 0 | 0 | 0 | 0 | 3 | 3 | ContextErrors.hs:547 (tvars = nub $ sort $ ... with Ord TyVar = (tv_nu |
| inst-comments-map | 2 | 0 | 0 | 0 | 0 | 2 |  |
| modarg-port-clash-sort | 0 | 0 | 0 | 0 | 2 | 2 | PragmaCheck.hs:571 (arg_names = sort [ (getIdBaseString n, getAPIArgNa |
| numeq-ordpair-orientation | 0 | 0 | 0 | 0 | 2 | 2 | StdPrel.hs:1058 ((tA, tB) = ordPair (t, t') in genNumEqInsts; `tA shou |
| submodule-method-uses-map | 0 | 0 | 0 | 0 | 2 | 2 | bluetcl.hs:1454-1459 (M.toList mumap grouped per instance) |
| insttree-map-dump-order | 0 | 0 | 0 | 0 | 1 | 1 | bsc.hs:815-817 (when -trace-inst-tree: putStr (ppReadable (apkg_inst_t |
| new-aopt-boolopt-var-sort | 0 | 1 | 0 | 0 | 0 | 1 | BoolOpt.hs:69-71 (optBoolExprQM: vs = sort (getVars e)) |
| pending-bluesim | 0 | 1 | 0 | 0 | 0 | 1 |  |
| position-file-id | 0 | 0 | 0 | 0 | 1 | 1 | Error.hs:1265-1266 (prEMsgList sortBy cmpEMsg) |
