module ASchedMaterialize
    ( aMaterializeSchedule
    , aAddSchedAssumps
    , aAddSchedAssumpsWith
    , aAddCFConditionWiresWith
    , needsCFConditionWires
    ) where

import ANoInline(aNoInline)
import ARemoveAssumps(aRemoveAssumps)
import ADropUndet(aDropUndet)
import ASyntax
import ASyntaxUtil
import AUses(MethodId(..), MethodUsers, UniqueUse(..), UseCond(..), MethodUsesList, ucTrue,
             mergeUseMapData, extractCondition, ruleMethodUsesToUUs)
import AScheduleInfo(AScheduleInfo(..))
import RSchedule(RAT)
import VModInfo(VMethodConflictInfo, vSched)
import SchedInfo(SchedInfo(..), MethodConflictInfo(..))
import PreIds
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List(genericLength, nub, sortOn)
import PPrint
import Pragma(ASchedulePragma, SchedulePragma(..))
import Error(internalError, ErrMsg(..), showErrorList, ErrorHandle)
import Flags(Flags, remapPathPrefix)
import Id
import Position(Position, getPosition, remapPositionFile)
import IntLit(IntLit(..))
import Prim(PrimOp(..))
import Util(unzipWith, ordPair, ordPairBy, mapSnd)

-- | Reconstruct the backend representation from the checked scheduled body.
-- The template is captured during elaboration; reading artifacts does not
-- require a symbol table or elaborate the RWire library again.
aMaterializeSchedule :: ErrorHandle -> Flags -> Maybe AVInst ->
                        APackage -> AScheduleInfo ->
                        (APackage, AScheduleInfo)
aMaterializeSchedule errh flags wireTemplate apkg schedinfo =
    let noinline = aNoInline flags apkg
        (wires, wireSched) =
            aAddCFConditionWiresWith wireTemplate noinline schedinfo
        (assumps, finalSched) =
            aAddSchedAssumpsWith (remapPositionFile (remapPathPrefix flags))
                                wires (asi_schedule schedinfo) wireSched
        noAssumps = aRemoveAssumps assumps
        finalBody = aDropUndet errh flags noAssumps
    in (finalBody, finalSched)

-- | Whether materializing this module requires a captured RWire template.
needsCFConditionWires :: APackage -> Bool
needsCFConditionWires = not . null . extractCFPairsSP . apkg_schedule_pragmas

-- These small helpers intentionally have no dependency on ASchedule: applying
-- a saved schedule must not link in or rerun the scheduler.
extractCFPairsSP :: [ASchedulePragma] -> [(ARuleId, ARuleId)]
extractCFPairsSP sps =
    nub (map ordPair (concatMap pairs sps))
  where pairs (SPConflictFree idss) = mkPairs idss
        pairs _ = []
        mkPairs [] = []
        mkPairs (ids:idss) =
            [(x,y) | x <- ids, y <- concat idss] ++ mkPairs idss

errAction :: String -> AAction
errAction msg =
    AFCall idErrorTask "$error" False [aTrue, ASStr defaultAId ty msg] True
  where ty = ATString (Just (genericLength msg))

-- | Method name mapped to condition of usage
type MethodCondMap = M.Map AMethodId AExpr

-- | State elements to the map of method condition usage
type OMCondMap = M.Map AId (MethodCondMap)

-- | Rules to the objects whose methods they use (with conditions)
type RuleMethodMap = M.Map ARuleId (OMCondMap)

-- | We only need the method conflict info
type OSchedMap = M.Map AId VMethodConflictInfo

buildOMCondMap :: MethodUsesList -> OMCondMap
buildOMCondMap uses = M.fromListWith (M.unionWith aOr) omuses'
  where uses'   = mapSnd buildUseConditions uses
        omuses  = [(o, (m, c)) | (MethodId o m, c) <- uses' ]
        omuses' :: [(AId, MethodCondMap)]
        omuses' = mapSnd (uncurry M.singleton) omuses

buildUseConditions :: [UniqueUse] -> AExpr
buildUseConditions = aOrs . sortOn exprKey . map materializeUseCondition

-- UseCond's maps and sets compare expressions by interned identifier order.
-- That order changes when a saved schedule is loaded in a fresh process.
-- Construct the delayed checks in a stable structural order instead; keep
-- AUses' scheduler-side expression construction unchanged.
materializeUseCondition :: UniqueUse -> AExpr
materializeUseCondition use@(UUAction _) = extractCondition use
materializeUseCondition (UUExpr _ condition) =
    foldl aAnd aTrue terms
  where
    equal expr value = APrim defaultAId aTBool PrimEQ
        [expr, ASInt defaultAId (aType expr) value]
    unequal expr value = APrim defaultAId aTBool PrimBNot [equal expr value]
    terms = sortOn exprKey (S.toList (true_exprs condition)) ++
        map aNot (sortOn exprKey (S.toList (false_exprs condition))) ++
        [ equal expr value | (expr, value) <- sortOn (exprKey . fst)
                                              (M.toList (eq_map condition)) ] ++
        [ unequal expr value | (expr, values) <- sortOn (exprKey . fst)
                                                (M.toList (neq_map condition))
                             , value <- S.toList values ]

-- Include constructors, types, names, and literal values. Positions and
-- identifier allocation numbers do not affect this key. Pretty-printing
-- alone would lose distinctions between differently typed expressions.
data ExprKey = ExprKey String [String] [ExprKey] deriving (Eq, Ord)

exprKey :: AExpr -> ExprKey
exprKey expr = case expr of
    APrim i t op args -> node "APrim" t [name i, show op] args
    AMethCall t i m args -> node "AMethCall" t [name i, name m] args
    AMethValue t i m -> node "AMethValue" t [name i, name m] []
    ATuple t args -> node "ATuple" t [] args
    ATupleSel t arg index -> node "ATupleSel" t [show index] [arg]
    ANoInlineFunCall t i fun args -> node "ANoInlineFunCall" t [name i, show fun] args
    AFunCall t i fun isC args -> node "AFunCall" t [name i, fun, show isC] args
    ATaskValue t i fun isC cookie -> node "ATaskValue" t [name i, fun, show isC, show cookie] []
    ASPort t i -> node "ASPort" t [name i] []
    ASParam t i -> node "ASParam" t [name i] []
    ASDef t i -> node "ASDef" t [name i] []
    ASInt i t value -> node "ASInt" t
        [name i, show (ilWidth value), show (ilBase value), show (ilValue value)] []
    ASReal i t value -> node "ASReal" t [name i, show value] []
    ASStr i t value -> node "ASStr" t [name i, value] []
    ASAny t value -> node "ASAny" t [] (maybe [] (:[]) value)
    ASClock t (AClock osc gate) -> node "ASClock" t [] [osc, gate]
    ASReset t (AReset wire) -> node "ASReset" t [] [wire]
    ASInout t (AInout wire) -> node "ASInout" t [] [wire]
    AMGate t i clock -> node "AMGate" t [name i, name clock] []
  where
    name = getIdString
    node tag t fields args = ExprKey tag fields (typeKey t : map exprKey args)

typeKey :: AType -> ExprKey
typeKey t = case t of
    ATBit size -> ExprKey "ATBit" [show size] []
    ATString size -> ExprKey "ATString" [show size] []
    ATReal -> ExprKey "ATReal" [] []
    ATArray size element -> ExprKey "ATArray" [show size] [typeKey element]
    ATTuple elements -> ExprKey "ATTuple" [] (map typeKey elements)
    ATAbstract i sizes -> ExprKey "ATAbstract" [getIdString i, show sizes] []

uqWGet, uqWSet :: Id
uqWGet = unQualId idWGet
uqWSet = unQualId idWSet

-- We check for overlaps when updating the RAT in case we made a mistake while injecting the new wires.
newRatErr :: RAT -> RAT -> MethodId -> M.Map UniqueUse Integer -> M.Map UniqueUse Integer -> M.Map UniqueUse Integer
newRatErr oldRat newRatEntries m _ _ =
  internalError $ "Conflict between original RAT and newRatEntries on methodId " ++ ppReadable m ++ "\n" ++
                  "Should be impossible because newRatEntries is all about newly instantiated wires.\n" ++
                  "RAT:           " ++ ppReadable oldRat ++ "\n" ++
                  "newRatEntries: " ++ ppReadable newRatEntries

aAddSchedAssumps :: APackage -> ASchedule -> AScheduleInfo -> (APackage, AScheduleInfo)
aAddSchedAssumps = aAddSchedAssumpsWith id

aAddSchedAssumpsWith :: (Position -> Position) -> APackage -> ASchedule ->
                       AScheduleInfo -> (APackage, AScheduleInfo)
aAddSchedAssumpsWith remapPosition apkg schedule schedinfo = (apkg'', schedinfo')
  where ruleMap :: M.Map ARuleId Integer
        ruleMap = M.fromList (zip (asch_rev_exec_order schedule) [0..])
        err i = internalError ("AAddAssumps - unknown rule: " ++ ppReadable i)
        get i = M.findWithDefault (err i) i ruleMap
        -- use reverse order because we want the last rule to come first
        -- since later rules will do the checking
        cmpRule r1 r2 = compare (get r1) (get r2)
        pragmas = apkg_schedule_pragmas apkg
        ruleMethodMap :: RuleMethodMap
        ruleMethodMap = M.map (buildOMCondMap .
                               ruleMethodUsesToUUs . snd)
                              (asi_rule_uses_map schedinfo)
        instSchedMap :: OSchedMap
        instSchedMap = M.fromList
                         [(n, methodConflictInfo (vSched vmi))
                             | AVInst { avi_vname = n, avi_vmi = vmi }  <- insts ]
        insts = apkg_state_instances apkg
        apkg' = apkg
        doCFAssumps = addCFAssumps remapPosition pragmas ruleMethodMap instSchedMap cmpRule
        (rs', newUseInfos) = unzip (map doCFAssumps (apkg_rules apkg'))
        newUseInfo = concat newUseInfos
        apkg'' = apkg' { apkg_rules = rs' }
        newRatEntries = M.fromList $ map useInfoToRatEntry newUseInfo
        newUseMapEntries = map useInfoToUseMapEntry newUseInfo
        oldRat = asi_resource_alloc_table schedinfo
        newRat = M.unionWithKey (newRatErr oldRat newRatEntries) oldRat newRatEntries
        newUseMap = M.unionWith (mergeUseMapData)
                      (asi_method_uses_map schedinfo)
                      (M.fromListWith (flip mergeUseMapData) newUseMapEntries)
        schedinfo' = schedinfo { asi_method_uses_map = newUseMap,
                                 asi_resource_alloc_table = newRat }


addCFAssumps :: (Position -> Position) -> [ASchedulePragma] -> RuleMethodMap -> OSchedMap
             -> (ARuleId -> ARuleId -> Ordering) -> ARule
             -> (ARule, [(ARuleId, MethodId, UniqueUse)])
addCFAssumps remapPosition pragmas ruleMethodMap instSchedMap cmpRule = proc_rule
  where cf_pairs = extractCFPairsSP pragmas
        sorted_cf_pairs = map (ordPairBy cmpRule) cf_pairs
        check_pairs = [ (a, [b]) | (a, b) <- sorted_cf_pairs ]
        check_map = M.fromListWith (++) check_pairs
        proc_rule r@(ARule { arule_id = rid }) =
          case (M.lookup rid check_map) of
            Nothing -> (r, [])
            Just rids ->
             let (new_assumps, useinfos) = unzip (mkCFAssumps remapPosition ruleMethodMap instSchedMap rid rids)
             in (r { arule_assumps = arule_assumps r ++ new_assumps }, useinfos)

mkCFAssumps :: (Position -> Position) -> RuleMethodMap -> OSchedMap -> ARuleId -> [ARuleId]
            -> [(AAssumption, (ARuleId, MethodId, UniqueUse))]
mkCFAssumps remapPosition ruleMethodMap instSchedMap rid rids =
    concatMap (mkCFAssump remapPosition ruleMethodMap instSchedMap rid) rids

mkCFAssump :: (Position -> Position) -> RuleMethodMap -> OSchedMap -> ARuleId -> ARuleId
           -> [(AAssumption, (ARuleId, MethodId, UniqueUse))]
mkCFAssump remapPosition ruleMethodMap instSchedMap r1 r2 =
    concatMap snd (sortOn (getIdString . fst) (M.toList overlapMap))
  where
    omcm_r1 = getOMCond r1
    omcm_r2 = getOMCond r2
    r1_s = getIdString r1
    r2_s = getIdString r2
    r2_WF = aBoolVar (mkIdWillFire r2)
    getOMCond r = case (M.lookup r ruleMethodMap) of
                    Nothing -> err r
                    Just m -> m
    err r = internalError ("AAddSchedAssumps: no OMCondMap: " ++ ppReadable r)
    overlapMap = M.intersectionWithKey checkMethodCalls omcm_r1 omcm_r2
    -- methods are ok if they appear in the CF list in either order
    -- or if they appear in the SB or SBR list in the correct execution order
    -- remember r2 executes before r1!
    -- r1 is the "second" rule, to which we attach scheduling logic
    isOKPair sched p@(m1, m2) = p `elem` sCF sched || p2 `elem` sCF sched ||
                                p2 `elem` sSB sched ||
                                p2 `elem` sSBR sched
       where p2 = (m2, m1)
    checkMethodCalls o methCondMap1 methCondMap2 = newAssumps
      where
        o_s = getIdString o
        sched = case (M.lookup o instSchedMap) of
                  Nothing -> internalError ("AddSchedAssumps: no VSchedInfo: " ++ ppReadable o)
                  Just sched -> sched
        -- convert to lists to do a cross-product
        newAssumps = [ (assump, useinfo) | (m1, c1) <- sortOn (getIdString . fst) (M.toList methCondMap1),
                                           (m2, _) <- sortOn (getIdString . fst) (M.toList methCondMap2),
                                           not (isOKPair sched (m1, m2)),
                                           let obj = mkCFCondWireInstId r2 o m2,
                                           -- extracts m2's condition from the wire
                                           -- include the WILL_FIRE because we don't check the tag bit
                                           let c2 = AMethCall aTBool obj uqWGet [],
                                           let c = aAnds [r2_WF, c1, c2],
                                           let m1_s = getIdString m1,
                                           let m2_s = getIdString m2,
                                           -- This position becomes a string, so binary position
                                           -- remapping cannot fix it later.  Remap here both to
                                           -- avoid leaking source paths and to match replay from
                                           -- an already-remapped .bmod (whose mapping is empty).
                                           let cferr = (remapPosition (getPosition r1),
                                                        EConflictFreeRulesFail (r1_s, r2_s) o_s (m1_s, m2_s)),
                                           let str = showErrorList [cferr],
                                           let a = [errAction str],
                                           let assump = AAssumption c a,
                                           -- XXX ucTrue is conservative, but we don't use the UseCond anyway
                                           let useinfo = (r1, MethodId obj uqWGet, UUExpr c2 ucTrue)]

-- | Rule to the methods it uses (with conditions)
type RuleMethCondMap = M.Map ARuleId [(MethodId, AExpr)]

-- | Add the wires using only the instance template saved with the module.
aAddCFConditionWiresWith :: Maybe AVInst -> APackage -> AScheduleInfo ->
                           (APackage, AScheduleInfo)
aAddCFConditionWiresWith wireTemplate apkg schedinfo =
   if (null cfPairs) then
    (apkg, schedinfo)
   else
    let template = case wireTemplate of
            Just inst -> inst
            Nothing -> internalError
                "aAddCFConditionWiresWith: missing RWire instance template"
        mkWireInst r (MethodId obj meth) =
            template { avi_vname = mkCFCondWireInstId r obj meth }
        newWires = concatMap (buildWireInsts ruleMethodMap mkWireInst)
                            (sortOn getIdString (S.toList cfRules))
    in (apkg { apkg_rules = rules',
               apkg_state_instances = oldState ++ newWires },
        schedinfo { asi_resource_alloc_table = newRat,
                    asi_method_uses_map = newUseMap })

  where pragmas = apkg_schedule_pragmas apkg
        cfPairs = extractCFPairsSP pragmas
        cfRules = S.fromList ((map fst cfPairs) ++ (map snd cfPairs))
        ruleMethodMap :: RuleMethCondMap
        ruleMethodMap = M.map (buildMethCondList .
                               ruleMethodUsesToUUs . snd)
                              (asi_rule_uses_map schedinfo)
        oldState = apkg_state_instances apkg
        (rules', newUseInfos) = unzipWith (addCFCondWires cfRules ruleMethodMap) (apkg_rules apkg)
        newUseInfo = concat newUseInfos
        newRatEntries = M.fromList $ map useInfoToRatEntry newUseInfo
        newUseMapEntries = map useInfoToUseMapEntry newUseInfo
        oldRat = asi_resource_alloc_table schedinfo
        newRat = M.unionWithKey (newRatErr oldRat newRatEntries) oldRat newRatEntries
        newUseMap = M.unionWith (mergeUseMapData)
                      (asi_method_uses_map schedinfo)
                      (M.fromListWith (flip mergeUseMapData) newUseMapEntries)

useInfoToRatEntry :: (ARuleId, MethodId, UniqueUse)
                  -> (MethodId, M.Map UniqueUse Integer)
useInfoToRatEntry (_,mid,u) = (mid, M.singleton u 1)

useInfoToUseMapEntry :: (ARuleId, MethodId, UniqueUse)
                     -> (MethodId, [(UniqueUse, MethodUsers)])
useInfoToUseMapEntry (rid, mid, u) = (mid, [(u, ([],[rid],[]))])

buildMethCondList :: MethodUsesList -> [(MethodId, AExpr)]
buildMethCondList uses = sortOn methodKey (M.toList (M.fromListWith aOr uses'))
  where uses'   = mapSnd buildUseConditions uses
        methodKey (MethodId object method, _) = (getIdString object, getIdString method)

buildWireInsts :: RuleMethCondMap -> (ARuleId -> MethodId -> AVInst) -> ARuleId -> [AVInst]
buildWireInsts ruleMethCondMap mkWireInst r = map (mkWireInst r) methIds
  where methIds = case (M.lookup r ruleMethCondMap) of
                    Just ms -> map fst ms
                    Nothing -> internalError ("AAddSchedAssumps.buildWireInsts missing rule: " ++
                                             ppReadable r ++ ppReadable ruleMethCondMap)

-- | Add wire-setting actions and return new RAT entries
addCFCondWires :: S.Set ARuleId -> RuleMethCondMap -> ARule -> (ARule, [(ARuleId, MethodId, UniqueUse)])
addCFCondWires cfRules ruleMethCondMap r | rid `S.member` cfRules =
  case (M.lookup rid ruleMethCondMap) of
    Just ms ->
      let (newActions, newUseInfo) = unzip $ [(a, (rid, mid, UUAction a)) | (MethodId o m, c) <- ms,
                                                                            let obj = mkCFCondWireInstId rid o m,
                                                                            let mid = MethodId obj uqWSet,
                                                                            let a   = ACall obj uqWSet [aTrue,c]]
      in (r { arule_actions = arule_actions r ++ newActions }, newUseInfo)
    Nothing -> internalError ("AAddAchedAssumps.addCFCondWires missing rule: " ++
                              ppReadable r ++ ppReadable ruleMethCondMap)
  where rid = arule_id r
addCFCondWires cfRules ruleMethCondMap r = (r, [])
