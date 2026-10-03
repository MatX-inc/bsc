# Root ids

The classifier's roots table, from the 2026-10-03 baseline (bsc at
3fe6aaa3, the full testsuite run normally and with
`-reverse-intern-order`).  One row per `root_id` used in `allowlist.txt`:
the counts are the differing files of each kind the root accounted for on
that date, and `site` is where the order is taken in `src/comp`.  Roots
marked `(noise)` are not compiler ordering and are covered by `ignore.txt`
instead.  Roots marked `(closed, #N)` were fixed by that pull request, with a
directed probe under `testsuite/bsc.batching`; an entry still carrying a
closed id differs through another root.  The counts go stale as the list
shrinks; the ids and sites do not.

| root | v | cxx/h | ba | bo | msgs | total | site |
|---|---|---|---|---|---|---|---|
| new-simcopt-sched-local-decl-order | 0 | 997 | 0 | 0 | 0 | 997 | SimCOpt.hs:181-189 (move_map = M.fromListWith (++) [... \| ((sbid,aid), |
| new-ba-dump-ruleuses-map-order | 0 | 0 | 884 | 0 | 0 | 884 | AUses.hs:205-233 (toListMethodExprUses/toListMethodActionUses/toListFF |
| me-pairs-ordpair (closed, #206) | 130 | 385 | 122 | 0 | 10 | 647 | ASchedule.hs:2531 (nub (map ordPair (concatMap mkAllPairs sps)) in ext |
| iexpand-pconj-set-order (closed, #207) | 194 | 200 | 91 | 0 | 6 | 491 | IExpandUtils.hs:265 (predToIExpr folds S.toList es) |
| genwrap-ppmap-order | 0 | 0 | 0 | 232 | 88 | 320 | GenWrap.hs:310 (moduledefs <- concatMapM (getDef generating ds) (M.toL |
| inlinereg-partition-by-clock (closed, #208) | 154 | 0 | 0 | 0 | 13 | 167 | InlineReg.hs:38,44,114 (M.toList (partitionByClock / partitionByClockA |
| rule-use-map-id-order | 0 | 0 | 16 | 0 | 142 | 158 | AUses.hs:460-469 (rumToMethodUseMap: M.Map Id (M.Map Id [UniqueUse]) b |
| new-sim-sched-def-set-order | 0 | 140 | 0 | 0 | 0 | 140 | SimMakeCBlocks.hs:1126-1127 (mkRuleSchedStmts: ids = S.toList $ getExp |
| new-sim-sched-stmt-map-order | 0 | 109 | 0 | 0 | 0 | 109 | SimMakeCBlocks.hs:184 (schedules = mapMaybe mkOneSchedule (M.toList st |
| new-heap-def-renumbering | 72 | 0 | 0 | 0 | 0 | 72 | src/comp/IExpand.hs:232-233, 302-312 (pDef names heap defs iExpandPref |
| use-cond-set-order | 5 | 8 | 42 | 0 | 5 | 60 | AUses.hs:622-626 (UseCond: S.Set AExpr / M.Map AExpr) |
| ifc-port-name-clash-sort | 0 | 0 | 0 | 0 | 50 | 50 | IExpandUtils.hs:2103 (ifc_names = sort $ ifc_port_names ++ ... : sorts |
| depend-map-order | 0 | 0 | 0 | 0 | 48 | 48 | Depend.hs:93-94 (chkDeps: tsort over DM.elems piMap) |
| linked-hash-transitive (noise) | 0 | 0 | 0 | 45 | 0 | 45 | GenBin.hs:37 (header ++ encode (bi_sig, bo_sig, ipkg)) and BinData.hs: |
| lc-updateapkgtypes-elems | 0 | 0 | 0 | 0 | 42 | 42 | LambdaCalcUtil.hs:788-800 (updateAPackageTypes: apkg_local_defs = M.el |
| new-run-noise-timestamps (noise) | 0 | 0 | 0 | 0 | 40 | 40 |  |
| itransform-runt-map-order | 0 | 35 | 0 | 0 | 2 | 37 | ITransform.hs:1441-1453 (runT emits M.toList cse_map ++ M.toList def_m |
| new-itransform-cse-rename-id-tiebreak | 0 | 0 | 36 | 0 | 0 | 36 | ITransform.hs:1704-1715 (cse_ids_map :: M.Map Id (S.Set (Int, Id)) bui |
| new-heap-pointer-noise (noise) | 0 | 0 | 0 | 0 | 36 | 36 |  |
| iexpand-eqptrs-dsm-elems | 0 | 0 | 0 | 0 | 36 | 36 | IExpand.hs:639 (eqPtrs returns (M.elems dsm, ptrm): dsm is M.Map HExpr |
| bluesim-package-map-order | 0 | 0 | 0 | 0 | 36 | 36 | ABinUtil.hs:91,164 (m_abmis_used prepend) |
| insttree-vector-position | 0 | 0 | 0 | 0 | 34 | 34 | InstNodes.hs:345-347 (comparein: comparing getPosition first) |
| rule-before-methods-map | 0 | 0 | 16 | 9 | 6 | 31 | ASchedule.hs:3335-3339 (before = map_insertManyWith (++) ... M.empty;  |
| new-iverilog-vvp-heap-addresses (noise) | 28 | 0 | 0 | 0 | 0 | 28 |  |
| imodule-port-types-map-dump | 0 | 0 | 0 | 0 | 26 | 26 | ISyntax.hs:182 (type PortTypeMap = M.Map (Maybe Id) (M.Map VName IType |
| static-sched-rule-pair-order | 0 | 0 | 0 | 0 | 17 | 17 | ASchedule.hs:4641 (separate $ checkOneRule (M.toList ruleMethodUseMap) |
| insttree-unsuffixed-id-order | 0 | 0 | 0 | 0 | 16 | 16 | Id.hs:265-274 (cmpSuffixedIdByName: suffix count 0 falls back to Ord I |
| new-flag-echo (noise) | 0 | 0 | 0 | 0 | 15 | 15 | Flags.hs/FlagsDecode.hs (-print-flags echoes the effective command lin |
| new-systemc-wrapper-port-map | 0 | 14 | 0 | 0 | 0 | 14 | SystemCWrapper.hs:140-145 (port_map, merged_port_map = M.fromList / M. |
| numeq-ordpair-orientation | 0 | 0 | 0 | 3 | 9 | 12 | StdPrel.hs:1058 ((tA, tB) = ordPair (t, t') in genNumEqInsts; `tA shou |
| new-sim-gate-info-inst-map | 0 | 12 | 0 | 0 | 0 | 12 | SimMakeCBlocks.hs:928 (mapMaybe mkGateInfo (M.toList inst_map)) |
| new-fixupdefs-binder-position | 11 | 0 | 0 | 0 | 0 | 11 | src/comp/IExpandUtils.hs:2469-2472 (improveCellName: keeps the binder  |
| new-cf-cond-wires-set-order | 0 | 0 | 11 | 0 | 0 | 11 | AAddSchedAssumps.hs:183 (newWires = concatMap (buildWireInsts ...) (S. |
| urgency-cycle-vertex-order | 0 | 0 | 0 | 0 | 9 | 9 | ASchedule.hs:4070 (vs = G.vertices umap = M.keys) |
| new-random-seed-test-generator (noise) | 0 | 0 | 0 | 0 | 9 | 9 |  |
| new-cf-assump-methcond-map-order | 0 | 9 | 0 | 0 | 0 | 9 | AAddSchedAssumps.hs:124 (mkCFAssump = concat $ M.elems overlapMap) |
| rule-use-map-instance-order | 0 | 0 | 8 | 0 | 0 | 8 | ASchedule.hs:1944-1960 (has_shadowing / WActionShadowing built from th |
| weak-context-tyvar-sort | 0 | 0 | 0 | 0 | 7 | 7 | ContextErrors.hs:547 (tvars = nub $ sort $ ... with Ord TyVar = (tv_nu |
| emsg-sort-noposition | 0 | 0 | 0 | 0 | 7 | 7 | Error.hs:1265-1266 (prEMsgList sortBy cmpEMsg on Position) |
| none-test-generated-vectors (noise) | 0 | 6 | 0 | 0 | 0 | 6 |  |
| new-sim-domain-map-clock-order (closed, #205) | 0 | 6 | 0 | 0 | 0 | 6 | SimExpand.hs:825 (domains = M.toList dmap) and :868 (map extractCSI do |
| new-binary-systemc-exe (noise) | 0 | 0 | 0 | 0 | 6 | 6 |  |
| none-build-timestamp (noise) | 0 | 5 | 0 | 0 | 0 | 5 |  |
| new-testsuite-random-seed (noise) | 5 | 0 | 0 | 0 | 0 | 5 | testsuite/bsc.interra/operators/Arith/generate/gen.pl:3 ($seed = srand |
| new-ghc-rts-stats-noise (noise) | 0 | 0 | 0 | 0 | 5 | 5 |  |
| submodule-method-uses-map | 0 | 0 | 0 | 0 | 4 | 4 | bluetcl.hs:1454-1459 (M.toList mumap grouped per instance) |
| insttree-map-dump-order | 0 | 0 | 0 | 0 | 4 | 4 | bsc.hs:815-817 (when -trace-inst-tree: putStr (ppReadable (apkg_inst_t |
| new-aopt-mux-arm-sort | 3 | 0 | 0 | 0 | 0 | 3 | src/comp/AOpt.hs:1199 (gpd = groupBy testf (sortBy cmpSnd xps) in sort |
| always-ready-exprmap-order | 0 | 0 | 0 | 0 | 3 | 3 | AAddScheduleDefs.hs:193 (handleAlwaysReady: map doRdy (M.toList pre_rd |
| new-test-random-vectors (noise) | 0 | 0 | 2 | 0 | 0 | 2 | testsuite/bsc.interra/operators/Arith/arith.exp (gen_arith.v random se |
| new-rule-between-witness-path | 0 | 0 | 2 | 0 | 0 | 2 | ASchedule.hs:3263-3288 (hasRuleBetween: reachable_methods = M.fromList |
| new-bluesim-reuse-flip (noise) | 0 | 0 | 0 | 0 | 2 | 2 | SimFileUtils.hs:87-104 (analyzeBluesimDependencies: reuse decided by h |
| modarg-port-clash-sort | 0 | 0 | 0 | 0 | 2 | 2 | PragmaCheck.hs:571 (arg_names = sort [ (getIdBaseString n, getAPIArgNa |
| position-file-id | 0 | 0 | 0 | 0 | 1 | 1 | Error.hs:1265-1266 (prEMsgList sortBy cmpEMsg) |
| new-test-env-date (noise) | 0 | 0 | 1 | 0 | 0 | 1 | testsuite/bsc.interra/libraries/Environment (Env4 testbench, compile-t |
| new-sim-rule-defs-set-order | 0 | 1 | 0 | 0 | 0 | 1 | SimMakeCBlocks.hs:494 (defs = map (findDef def_map) (S.toList ids)) |
| new-intern-order-self-test (noise) | 0 | 0 | 0 | 0 | 1 | 1 | FStringCompat.hs/SpeedyString.hs (intern table dump) |
| new-aopt-boolopt-var-sort | 0 | 1 | 0 | 0 | 0 | 1 | BoolOpt.hs:69-71 (optBoolExprQM: vs = sort (getVars e)) |
