{-# LANGUAGE BangPatterns #-}
module ASignalNaming (
    AProvLookup,
    mkSignalNamingEnvFromAPackage,
    mkSignalNamingEnvFromASPackage,
    signalAType,
    signalAExpr,
    signalAExprWithStems,
    signalAExprWithProv,
    signalAExprStem,
    signalNameFromAExpr,
    signalNameFromAExprDefault,
    exprNameFromAExpr,
    collectAPackageExistingIds,
    collectASPackageExistingIds,
    nameAPackageDefs,
    applyAPackageTempNames
) where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import ASourceTypes (provenanceLookup, sourceTypeKey)
import ASyntax (
    AAbstractInput(..),
    AClock(..),
    ADef(..),
    AExpr(..),
    AIFace(..),
    AInout(..),
    ANoInlineFun(..),
    APackage(..),
    ARule(..),
    AReset(..),
    ASPSignalInfo(..),
    ASPackage(..),
    AType(..),
    AVInst(..),
    getAPackageParamsPortsAndInouts,
    getSpecialOutputs
    )
import ASyntaxUtil (aVars, mapAExprs)
import ErrorUtil (internalError)
import Eval (rnf)
import Flags (Flags)
import Id (
    Id,
    IdProp(..),
    getIdBaseString,
    getIdString,
    hasIdProp,
    mkIdCanFire,
    mkIdWillFire,
    mk_homeless_id,
    setBadId,
    setIdBaseString
    )
import IntLit (IntLit(..))
import PPrint (ppReadable)
import PreIds (idPrimUnit)
import Prim (PrimOp(..))
import SCC (tsort)
import SignalNaming (
    AnchorCollision(..),
    ExprName,
    ModuleNameItem(..),
    SignalExpr(..),
    SignalNamingEnv(..),
    SignalType(..),
    assignModuleNames,
    cleanGenBase,
    emptySignalNamingEnv,
    exprName,
    ifcBoundaryNames,
    isClockOrResetPortName,
    isDefAnchorId,
    isDefTempId,
    lookupArgNamedLit,
    lowerResolvedMethodRead,
    mkDefOrPort,
    mkSignalConcat,
    mkSignalExtract,
    mkSignalInt,
    mkSignalNamingEnv,
    mkSignalNat,
    mkSignalPort,
    mkSignalTuple,
    mkSignalTupleSel,
    nliMatchesBitLit,
    resolveOutputGate,
    selectProvenanceSignalExpr,
    signalExprStem,
    signalExprType,
    wireInfoSpecialNames
    )
import SourceProvenance (NamedLitInfo, ProvFact(..))

isIfWrapperPrim :: PrimOp -> Bool
isIfWrapperPrim PrimExpIf = True
isIfWrapperPrim PrimNoExpIf = True
isIfWrapperPrim PrimNosplitDeep = True
isIfWrapperPrim PrimSplitDeep = True
isIfWrapperPrim _ = False

-- ============================================================
-- APackage & ASPackage environment construction
-- ============================================================

mkSignalNamingEnvFromAPackage :: Flags -> APackage -> SignalNamingEnv
mkSignalNamingEnvFromAPackage flags apkg =
    let instPairs = [ (avi_vname avi, avi_vmi avi) | avi <- apkg_state_instances apkg ]
        baseEnv = mkSignalNamingEnv flags instPairs [] []
        (paramIns, portIns, inoutIns) = getAPackageParamsPortsAndInouts apkg
        (wiParams, wiClkRst) = wireInfoSpecialNames (apkg_external_wires apkg)
        sizeParams = map getIdBaseString (apkg_size_params apkg)
        clkRstArgs =
            concatMap
                (\inp -> case inp of
                    AAI_Clock osc mGate ->
                        getIdBaseString osc : maybe [] (\g -> [getIdBaseString g]) mGate
                    AAI_Reset r -> [getIdBaseString r]
                    AAI_Inout io _ -> [getIdBaseString io]
                    AAI_Port _ -> [])
                (apkg_inputs apkg)
        (ifcPorts, ifcInouts, ifcRuleIds) =
            L.foldl'
                (\(ps, cs, rids) aif ->
                    let (p, c) = ifcBoundaryNames (aif_fieldinfo aif)
                        rs =
                            case aif of
                                AIAction { aif_body = brs } -> map arule_id brs
                                AIActionValue { aif_body = brs } -> map arule_id brs
                                _ -> []
                    in (p ++ ps, c ++ cs, rs ++ rids))
                ([], [], [])
                (apkg_interface apkg)
        allRuleIds = map arule_id (apkg_rules apkg) ++ ifcRuleIds
        ruleFireNames =
            concatMap
                (\rid ->
                    [ getIdBaseString (mkIdWillFire rid)
                    , getIdBaseString (mkIdCanFire rid)
                    ])
                allRuleIds
        specialSubmod =
            [ getIdBaseString pid
            | avi <- apkg_state_instances apkg
            , (pid, _, _) <- getSpecialOutputs avi
            ]
        explicitKeep =
            [ getIdBaseString (adef_objid d)
            | d <- apkg_local_defs apkg
            , hasIdProp (adef_objid d) IdPProbe ||
              hasIdProp (adef_objid d) IdP_keepEvenUnused ||
              hasIdProp (adef_objid d) IdPMethodPredicate
            ]
        clkRstSet =
            S.fromList
                (wiClkRst ++ clkRstArgs ++ map (getIdBaseString . fst) inoutIns ++ ifcInouts)
        portNames =
            [ s
            | (i, _) <- portIns
            , let s = getIdBaseString i
            , not (s `S.member` clkRstSet)
            ]
    in baseEnv
           { sne_state_ports = L.foldl' (flip S.insert) (sne_state_ports baseEnv) specialSubmod
           , sne_top_ports = S.fromList (portNames ++ ifcPorts)
           , sne_params = S.fromList (sizeParams ++ wiParams ++ map (getIdBaseString . fst) paramIns)
           , sne_clk_rst_inouts = clkRstSet
           , sne_explicit_anchors = S.fromList (explicitKeep ++ ruleFireNames)
           }

mkSignalNamingEnvFromASPackage :: Flags -> ASPackage -> SignalNamingEnv
mkSignalNamingEnvFromASPackage flags aspkg =
    let instPairs = [ (avi_vname avi, avi_vmi avi) | avi <- aspkg_state_instances aspkg ]
        si = aspkg_signal_info aspkg
        siInlined = [ pid | (_, _, pids) <- aspsi_inlined_ports si, pid <- pids ]
        inlinedIds = aspkg_inlined_ports aspkg ++ siInlined
        baseEnv = mkSignalNamingEnv flags instPairs inlinedIds []
        stateOutNames = [ getIdBaseString i | (i, _) <- aspkg_state_outputs aspkg ]
        specialSubmod =
            [ getIdBaseString pid
            | avi <- aspkg_state_instances aspkg
            , (pid, _, _) <- getSpecialOutputs avi
            ]
        outClks =
            [ getIdBaseString i
            | (osc, gates) <- aspsi_output_clks si
            , i <- osc : gates
            ]
        outRsts = map getIdBaseString (aspsi_output_rsts si)
        ifcIots = map getIdBaseString (aspsi_ifc_iots si)
        inouts = map (getIdBaseString . fst) (aspkg_inouts aspkg)
        clkRstInoutList = outClks ++ outRsts ++ ifcIots ++ inouts
        clkRstSet =
            S.fromList
                ( clkRstInoutList ++
                  [ s
                  | (i, _) <- aspkg_inputs aspkg
                  , let s = getIdBaseString i
                  , isClockOrResetPortName s
                  ]
                )
        topInNames =
            [ s
            | (i, _) <- aspkg_inputs aspkg
            , let s = getIdBaseString i
            , not (s `S.member` clkRstSet)
            ]
        topOutNames =
            [ s
            | (i, _) <- aspkg_outputs aspkg
            , let s = getIdBaseString i
            , not (s `S.member` clkRstSet)
            ]
        paramNames = [ getIdBaseString i | (i, _) <- aspkg_parameters aspkg ]
        explicitKeep =
            [ getIdBaseString (adef_objid d)
            | d <- aspkg_values aspkg ++ aspkg_inout_values aspkg
            , hasIdProp (adef_objid d) IdPProbe ||
              hasIdProp (adef_objid d) IdP_keepEvenUnused ||
              hasIdProp (adef_objid d) IdPMethodPredicate
            ]
    in baseEnv
           { sne_state_ports =
                 L.foldl'
                     (flip S.insert)
                     (sne_state_ports baseEnv)
                     (stateOutNames ++ specialSubmod)
           , sne_top_ports = S.fromList (topInNames ++ topOutNames)
           , sne_params = S.fromList paramNames
           , sne_clk_rst_inouts = clkRstSet
           , sne_explicit_anchors = S.fromList explicitKeep
           }

collectAPackageExistingIds :: APackage -> [Id]
collectAPackageExistingIds apkg =
    let (paramIns, portIns, inoutIns) = getAPackageParamsPortsAndInouts apkg
        clkRstIds =
            concatMap
                (\inp -> case inp of
                    AAI_Clock osc mGate -> osc : maybe [] (: []) mGate
                    AAI_Reset r -> [r]
                    AAI_Inout io _ -> [io]
                    AAI_Port _ -> [])
                (apkg_inputs apkg)
        instIds = map avi_vname (apkg_state_instances apkg)
        defIds = map adef_objid (apkg_local_defs apkg)
    in apkg_size_params apkg ++
       map fst paramIns ++
       map fst portIns ++
       map fst inoutIns ++
       clkRstIds ++
       instIds ++
       defIds

collectASPackageExistingIds :: ASPackage -> [Id]
collectASPackageExistingIds aspkg =
    map fst (aspkg_parameters aspkg) ++
    map fst (aspkg_inputs aspkg) ++
    map fst (aspkg_outputs aspkg) ++
    map fst (aspkg_inouts aspkg) ++
    map fst (aspkg_state_outputs aspkg) ++
    aspkg_inlined_ports aspkg ++
    map avi_vname (aspkg_state_instances aspkg) ++
    map adef_objid (aspkg_values aspkg) ++
    map adef_objid (aspkg_inout_values aspkg)

-- ============================================================
-- ASyntax type & expression lowering
-- ============================================================

signalAType :: AType -> SignalType
signalAType (ATBit n) = SignalBit n
signalAType (ATString mn) = SignalString mn
signalAType ATReal = SignalReal
signalAType (ATArray len elemTy) = SignalArray len (signalAType elemTy)
signalAType (ATTuple tys) = SignalTuple (map signalAType tys)
signalAType (ATAbstract aid szs)
    | aid == idPrimUnit = SignalBit 0
    | otherwise = SignalAbstract (getIdString aid) szs

type AProvLookup = AExpr -> [ProvFact AExpr]

noAProv :: AProvLookup
noAProv _ = []

isAUnitArg :: AExpr -> Bool
isAUnitArg (ASInt i (ATBit 0) _) = i == idPrimUnit
isAUnitArg _ = False

aLitMatches :: AExpr -> NamedLitInfo -> Bool
aLitMatches (ASInt _ (ATBit w) lit) nli = nliMatchesBitLit w (ilValue lit) nli
aLitMatches _ _ = False

isSelfAExpr :: Maybe Id -> AExpr -> AExpr -> Bool
isSelfAExpr mSelfId cur candidate
    | candidate == cur = True
    | Just sid <- mSelfId
    , ASDef _ did <- candidate = did == sid
    | otherwise = False

skipsAProv :: AExpr -> Bool
skipsAProv (ASDef {}) = True
skipsAProv (ASPort {}) = True
skipsAProv (ASParam {}) = True
skipsAProv (ASClock {}) = True
skipsAProv (ASReset {}) = True
skipsAProv (ASInout {}) = True
skipsAProv (AMGate {}) = True
skipsAProv (ATaskValue {}) = True
skipsAProv _ = False

signalAExpr :: SignalNamingEnv -> AExpr -> SignalExpr
signalAExpr env expr = signalAExprWithStems env M.empty expr

signalAExprStem :: SignalNamingEnv -> AExpr -> String
signalAExprStem env expr = signalExprStem env (signalAExpr env expr)

signalNameFromAExpr :: SignalNamingEnv -> AExpr -> String
signalNameFromAExpr = signalAExprStem

signalNameFromAExprDefault :: AExpr -> String
signalNameFromAExprDefault = signalAExprStem emptySignalNamingEnv

exprNameFromAExpr :: SignalNamingEnv -> AExpr -> ExprName
exprNameFromAExpr env expr = exprName env (signalAExpr env expr)

signalAExprWithStems :: SignalNamingEnv -> M.Map Id String -> AExpr -> SignalExpr
signalAExprWithStems env stemMap expr =
    signalAExprWithProv env noAProv stemMap expr

signalAExprWithProv ::
    SignalNamingEnv ->
    AProvLookup ->
    M.Map Id String ->
    AExpr ->
    SignalExpr
signalAExprWithProv env lookupProv stemMap expr =
    signalAExprWithProvSelf env lookupProv stemMap Nothing expr

signalAExprWithProvSelf ::
    SignalNamingEnv ->
    AProvLookup ->
    M.Map Id String ->
    Maybe Id ->
    AExpr ->
    SignalExpr
signalAExprWithProvSelf env lookupProv stemMap mSelfId expr =
    signalAExprGo env lookupProv stemMap mSelfId 0 expr

signalAExprGo ::
    SignalNamingEnv ->
    AProvLookup ->
    M.Map Id String ->
    Maybe Id ->
    Int ->
    AExpr ->
    SignalExpr
signalAExprGo env lookupProv stemMap mSelfId depth (APrim _ _ PrimRange [_, _, arg]) =
    signalAExprGo env lookupProv stemMap mSelfId depth arg
signalAExprGo env lookupProv stemMap mSelfId depth (APrim _ _ p [arg])
    | isIfWrapperPrim p =
        signalAExprGo env lookupProv stemMap mSelfId depth arg
signalAExprGo env lookupProv stemMap mSelfId depth expr
    | depth < 8 && not (skipsAProv expr) =
        let facts = lookupProv expr
        in case if null facts
                then Nothing
                else selectProvenanceSignalExpr
                         env
                         (signalAType (ae_type expr))
                         (isSelfAExpr mSelfId expr)
                         (signalAExprGo env lookupProv stemMap Nothing (depth + 1))
                         (aLitMatches expr)
                         facts of
               Just se -> se
               Nothing ->
                   signalAExprStructural env lookupProv stemMap (depth + 1) facts expr
    | otherwise =
        signalAExprStructural env lookupProv stemMap (depth + 1) [] expr

lowerAArgWithFacts ::
    SignalNamingEnv ->
    AProvLookup ->
    M.Map Id String ->
    Int ->
    [ProvFact AExpr] ->
    (Int, AExpr) ->
    SignalExpr
lowerAArgWithFacts env lookupProv stemMap nextDepth facts (argIdx, arg)
    | not (null facts)
    , Just nli <- lookupArgNamedLit argIdx facts
    , aLitMatches arg nli =
        SignalNamedLit (signalAType (ae_type arg)) nli
    | otherwise =
        signalAExprGo env lookupProv stemMap Nothing nextDepth arg

signalAExprStructural ::
    SignalNamingEnv ->
    AProvLookup ->
    M.Map Id String ->
    Int ->
    [ProvFact AExpr] ->
    AExpr ->
    SignalExpr
signalAExprStructural env lookupProv stemMap nextDepth _ (APrim _ (ATBit outW) PrimSelect [arg]) =
    let se = signalAExprGo env lookupProv stemMap Nothing nextDepth arg
        inW =
            case signalExprType se of
                SignalBit w -> w
                _ -> outW
    in mkSignalExtract outW inW se (mkSignalNat (outW - 1)) (mkSignalNat 0)
signalAExprStructural env lookupProv stemMap nextDepth _ (APrim _ (ATBit outW) PrimExtract [arg, hi, lo]) =
    let se = signalAExprGo env lookupProv stemMap Nothing nextDepth arg
        shi = signalAExprGo env lookupProv stemMap Nothing nextDepth hi
        slo = signalAExprGo env lookupProv stemMap Nothing nextDepth lo
        inW =
            case signalExprType se of
                SignalBit w -> w
                _ -> outW
    in mkSignalExtract outW inW se shi slo
signalAExprStructural env lookupProv stemMap nextDepth _ (APrim _ _ PrimRange [_, _, arg]) =
    signalAExprGo env lookupProv stemMap Nothing nextDepth arg
signalAExprStructural env lookupProv stemMap nextDepth _ (APrim _ _ PrimIntegerToBit [APrim _ _ PrimStringToInteger [s]]) =
    signalAExprGo env lookupProv stemMap Nothing nextDepth s
signalAExprStructural env lookupProv stemMap nextDepth _ (APrim _ _ p [arg])
    | isIfWrapperPrim p = signalAExprGo env lookupProv stemMap Nothing nextDepth arg
signalAExprStructural env lookupProv stemMap nextDepth _ (APrim _ (ATBit outW) PrimZeroExt [arg]) =
    let se = signalAExprGo env lookupProv stemMap Nothing nextDepth arg
    in case signalExprType se of
           SignalBit inW ->
               let padW = outW - inW
               in if padW <= 0
                  then se
                  else mkSignalConcat [mkSignalInt (SignalBit padW) 0, se]
           _ -> SignalPrim (SignalBit outW) PrimZeroExt [se]
signalAExprStructural env lookupProv stemMap nextDepth _ (APrim _ (ATBit outW) PrimTrunc [arg]) =
    let se = signalAExprGo env lookupProv stemMap Nothing nextDepth arg
    in case signalExprType se of
           SignalBit inW ->
               mkSignalExtract outW inW se (mkSignalNat (outW - 1)) (mkSignalNat 0)
           _ -> SignalPrim (SignalBit outW) PrimTrunc [se]
signalAExprStructural env lookupProv stemMap nextDepth _ (APrim _ _ PrimConcat es) =
    mkSignalConcat (map (signalAExprGo env lookupProv stemMap Nothing nextDepth) es)
signalAExprStructural env lookupProv stemMap nextDepth facts (APrim _ ty p es) =
    let ses =
            map
                (lowerAArgWithFacts env lookupProv stemMap nextDepth facts)
                (zip [0 :: Int ..] es)
    in SignalPrim (signalAType ty) p ses
signalAExprStructural env _ _ _ _ (ATupleSel ty (AMethValue _ instId methId) idx1) =
    lowerResolvedMethodRead env Nothing (signalAType ty) instId methId (Just idx1) []
signalAExprStructural env lookupProv stemMap nextDepth facts (ATupleSel ty mc@(AMethCall _ instId methId rawArgs) idx1) =
    let mFacts = if null facts then lookupProv mc else facts
        indexedArgs = [ (k, a) | (k, a) <- zip [1 :: Int ..] rawArgs, not (isAUnitArg a) ]
        sArgs = map (lowerAArgWithFacts env lookupProv stemMap nextDepth mFacts) indexedArgs
    in lowerResolvedMethodRead env Nothing (signalAType ty) instId methId (Just idx1) sArgs
signalAExprStructural env lookupProv stemMap nextDepth _ (ATupleSel ty inner idx1) =
    mkSignalTupleSel (signalAType ty) (signalAExprGo env lookupProv stemMap Nothing nextDepth inner) idx1
signalAExprStructural env _ _ _ _ (AMethValue ty instId methId) =
    lowerResolvedMethodRead env Nothing (signalAType ty) instId methId Nothing []
signalAExprStructural env lookupProv stemMap nextDepth facts (AMethCall ty instId methId rawArgs) =
    let indexedArgs = [ (k, a) | (k, a) <- zip [1 :: Int ..] rawArgs, not (isAUnitArg a) ]
        sArgs = map (lowerAArgWithFacts env lookupProv stemMap nextDepth facts) indexedArgs
    in lowerResolvedMethodRead env Nothing (signalAType ty) instId methId Nothing sArgs
signalAExprStructural env lookupProv stemMap nextDepth _ (ATuple ty elems) =
    mkSignalTuple (signalAType ty) (map (signalAExprGo env lookupProv stemMap Nothing nextDepth) elems)
signalAExprStructural env lookupProv stemMap nextDepth facts (ANoInlineFunCall ty objId (ANoInlineFun fn params ports _) rawArgs) =
    SignalNoInlineCall
        (signalAType ty)
        (cleanGenBase objId)
        fn
        params
        ports
        (map (lowerAArgWithFacts env lookupProv stemMap nextDepth facts) (zip [0 :: Int ..] rawArgs))
signalAExprStructural env lookupProv stemMap nextDepth facts (AFunCall ty _ fn isCFlag rawArgs) =
    SignalFunCall
        (signalAType ty)
        fn
        isCFlag
        (map (lowerAArgWithFacts env lookupProv stemMap nextDepth facts) (zip [0 :: Int ..] rawArgs))
signalAExprStructural _ _ _ _ _ (ATaskValue ty _ fn isCFlag cookie) =
    SignalTaskValue (signalAType ty) fn isCFlag cookie
signalAExprStructural env _ _ _ _ (ASPort ty pid) =
    mkSignalPort env (signalAType ty) (getIdBaseString pid)
signalAExprStructural _ _ _ _ _ (ASParam ty pid) =
    SignalParam (signalAType ty) (getIdBaseString pid)
signalAExprStructural env _ stemMap _ _ (ASDef ty did) =
    let nm = M.findWithDefault (getIdBaseString did) did stemMap
    in mkDefOrPort env (signalAType ty) nm
signalAExprStructural _ _ _ _ _ (ASInt _ ty lit) =
    mkSignalInt (signalAType ty) (ilValue lit)
signalAExprStructural _ _ _ _ _ (ASReal _ ty r) =
    SignalRealLit (signalAType ty) r
signalAExprStructural _ _ _ _ _ (ASStr _ ty s) =
    SignalStr (signalAType ty) s
signalAExprStructural env lookupProv stemMap nextDepth _ (ASAny ty mVal) =
    SignalAny (signalAType ty) (fmap (signalAExprGo env lookupProv stemMap Nothing nextDepth) mVal)
signalAExprStructural env lookupProv stemMap nextDepth _ (ASClock _ (AClock osc gate)) =
    SignalClock
        (signalAExprGo env lookupProv stemMap Nothing nextDepth osc)
        (signalAExprGo env lookupProv stemMap Nothing nextDepth gate)
signalAExprStructural env lookupProv stemMap nextDepth _ (ASReset _ (AReset wire)) =
    SignalReset (signalAExprGo env lookupProv stemMap Nothing nextDepth wire)
signalAExprStructural env lookupProv stemMap nextDepth _ (ASInout ty (AInout wire)) =
    SignalInout (signalAType ty) (signalAExprGo env lookupProv stemMap Nothing nextDepth wire)
signalAExprStructural env _ _ _ _ (AMGate _ instId clkId) =
    resolveOutputGate env Nothing instId clkId

-- ============================================================
-- Whole-APackage unnamed temporary naming (for AConv)
-- ============================================================

dedupInts :: [Int] -> [Int]
dedupInts = go IS.empty
  where
    go _ [] = []
    go !seen (x : xs)
        | IS.member x seen = go seen xs
        | otherwise = x : go (IS.insert x seen) xs

nameAPackageDefs :: Flags -> APackage -> M.Map Id Id
nameAPackageDefs flags apkg =
    let env = mkSignalNamingEnvFromAPackage flags apkg
        ds = apkg_local_defs apkg
        rawProv = apkg_provenance apkg
        lookupProv0 = provenanceLookup ds rawProv
        defProvMap =
            if null rawProv
            then M.empty
            else M.fromList
                     [ (sourceTypeKey e, fsDef)
                     | ADef i t e _ <- ds
                     , let fsDef = lookupProv0 (ASDef t i)
                     , not (null fsDef)
                     ]
        lookupProv expr
            | null rawProv = []
            | otherwise =
                let fs0 = lookupProv0 expr
                in if not (null fs0)
                   then fs0
                   else M.findWithDefault [] (sourceTypeKey expr) defProvMap
        nds = zip [0 :: Int ..] ds
        posMap = M.fromList [ (i, n) | (n, ADef i _ _ _) <- nds ]
        synDeps e = mapMaybe (\v -> M.lookup v posMap) (aVars e)
        provDeps selfIdx e
            | null rawProv = []
            | otherwise =
                filter (/= selfIdx) $
                concatMap
                    (\f ->
                        case f of
                            PFactAccess base _ -> synDeps base
                            PFactConcat parts -> concatMap synDeps parts
                            _ -> [])
                    (lookupProv e)
        synGraph = [ (n, dedupInts (synDeps e)) | (n, ADef _ _ e _) <- nds ]
        combGraph =
            [ (n, dedupInts (synDeps e ++ provDeps n e))
            | (n, ADef _ _ e _) <- nds
            ]
        defByIdx = IM.fromList nds
        topoIdxs =
            if null rawProv
            then case tsort synGraph of
                     Right ns -> ns
                     Left sccs ->
                         internalError ("ASignalNaming.nameAPackageDefs: cycle: " ++ ppReadable sccs)
            else case tsort combGraph of
                     Right ns -> ns
                     Left cycSccs ->
                         let cycSet = IS.fromList (concat cycSccs)
                             prunedGraph =
                                 [ if IS.member n cycSet
                                   then (n, dedupInts (synDeps e))
                                   else (n, dedupInts (synDeps e ++ provDeps n e))
                                 | (n, ADef _ _ e _) <- nds
                                 ]
                         in case tsort prunedGraph of
                                Right ns -> ns
                                Left _ ->
                                    case tsort synGraph of
                                        Right ns -> ns
                                        Left sccs ->
                                            internalError
                                                ("ASignalNaming.nameAPackageDefs: cycle: " ++ ppReadable sccs)
        dummyTempId = setBadId (mk_homeless_id "_")
        (_, !exprByIdx) =
            L.foldl'
                (\(!stemAcc, !seAcc) n ->
                    case IM.lookup n defByIdx of
                        Nothing -> (stemAcc, seAcc)
                        Just (ADef i _ e _)
                            | isDefTempId env i ->
                                let se = signalAExprWithProvSelf env lookupProv stemAcc (Just i) e
                                    st = signalExprStem env se
                                    !stemAcc' = rnf st `seq` M.insert i st stemAcc
                                    !seAcc' = rnf se `seq` IM.insert n se seAcc
                                in (stemAcc', seAcc')
                            | otherwise ->
                                (stemAcc, seAcc))
                (M.empty, IM.empty)
                topoIdxs
        defItems = [ mkDefItem n d | (n, d) <- nds ]
        mkDefItem n (ADef i _ e _)
            | isDefAnchorId env i = ItemAnchor i i
            | isDefTempId env i =
                let se =
                        IM.findWithDefault
                            (signalAExprWithProvSelf env lookupProv M.empty (Just i) e)
                            n
                            exprByIdx
                in ItemTemp i dummyTempId se
            | otherwise = ItemUserBinder i i Nothing
    in case assignModuleNames env defItems of
           Left (AnchorCollision nm k1 k2) ->
               internalError
                   ("ASignalNaming.nameAPackageDefs: anchor collision on " ++
                    nm ++ " (" ++ show k1 ++ " vs " ++ show k2 ++ ")")
           Right pairs ->
               L.foldl'
                   (\ !acc (origId, finalStr) ->
                       if not (isDefAnchorId env origId) &&
                          getIdBaseString origId /= finalStr
                       then let !newId = setIdBaseString origId finalStr
                            in M.insert origId newId acc
                       else acc)
                   M.empty
                   pairs

applyAPackageTempNames :: Flags -> APackage -> APackage
applyAPackageTempNames flags apkg =
    let renMap = nameAPackageDefs flags apkg
    in if M.null renMap
       then apkg
       else let renId i = M.findWithDefault i i renMap
                renClk (AClock osc gate) = AClock (renExpr osc) (renExpr gate)
                renRst (AReset w) = AReset (renExpr w)
                renIot (AInout w) = AInout (renExpr w)
                renExpr (ASDef ty i) = ASDef ty (renId i)
                renExpr (APrim aid ty op es) = APrim aid ty op (map renExpr es)
                renExpr (AMethCall ty i meth es) = AMethCall ty i meth (map renExpr es)
                renExpr (ATuple ty es) = ATuple ty (map renExpr es)
                renExpr (ATupleSel ty e idx) = ATupleSel ty (renExpr e) idx
                renExpr (ANoInlineFunCall ty i f es) = ANoInlineFunCall ty i f (map renExpr es)
                renExpr (AFunCall ty i f isC es) = AFunCall ty i f isC (map renExpr es)
                renExpr (ASAny ty me) = ASAny ty (fmap renExpr me)
                renExpr (ASClock i c) = ASClock i (renClk c)
                renExpr (ASReset i r) = ASReset i (renRst r)
                renExpr (ASInout ty io) = ASInout ty (renIot io)
                renExpr e = e
                renIfc (AIClock i c fi) = AIClock i (renClk c) fi
                renIfc (AIReset i r fi) = AIReset i (renRst r) fi
                renIfc (AIInout i io fi) = AIInout i (renIot io) fi
                renIfc aif = mapAExprs renExpr aif
                apkg1 = mapAExprs renExpr apkg
            in apkg1
                   { apkg_local_defs =
                         [ ADef (renId i) ty (renExpr e) props
                         | ADef i ty e props <- apkg_local_defs apkg
                         ]
                   , apkg_interface = map renIfc (apkg_interface apkg)
                   , apkg_clock_domains =
                         [ (d, map renClk acs)
                         | (d, acs) <- apkg_clock_domains apkg
                         ]
                   , apkg_reset_list =
                         [ (rid, renRst r)
                         | (rid, r) <- apkg_reset_list apkg
                         ]
                   }
