{-# LANGUAGE BangPatterns #-}
module ASignalNaming (
    mkSignalNamingEnvFromAPackage,
    mkSignalNamingEnvFromASPackage,
    signalAType,
    signalAExpr,
    signalAExprWithStems,
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
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

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
    resolveOutputGate,
    signalExprStem,
    signalExprType,
    wireInfoSpecialNames
    )

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

dropAPrimUnitArgs :: [AExpr] -> [AExpr]
dropAPrimUnitArgs = filter (not . isAUnitArg)
  where
    isAUnitArg :: AExpr -> Bool
    isAUnitArg (ASInt i (ATBit 0) _) = i == idPrimUnit
    isAUnitArg _ = False

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
signalAExprWithStems env stemMap (APrim _ (ATBit outW) PrimSelect [arg]) =
    let se = signalAExprWithStems env stemMap arg
        inW =
            case signalExprType se of
                SignalBit w -> w
                _ -> outW
    in mkSignalExtract outW inW se (mkSignalNat (outW - 1)) (mkSignalNat 0)
signalAExprWithStems env stemMap (APrim _ (ATBit outW) PrimExtract [arg, hi, lo]) =
    let se = signalAExprWithStems env stemMap arg
        shi = signalAExprWithStems env stemMap hi
        slo = signalAExprWithStems env stemMap lo
        inW =
            case signalExprType se of
                SignalBit w -> w
                _ -> outW
    in mkSignalExtract outW inW se shi slo
signalAExprWithStems env stemMap (APrim _ _ PrimRange [_, _, arg]) =
    signalAExprWithStems env stemMap arg
signalAExprWithStems env stemMap (APrim _ _ PrimIntegerToBit [APrim _ _ PrimStringToInteger [s]]) =
    signalAExprWithStems env stemMap s
signalAExprWithStems env stemMap (APrim _ _ p [arg])
    | isIfWrapperPrim p = signalAExprWithStems env stemMap arg
signalAExprWithStems env stemMap (APrim _ (ATBit outW) PrimZeroExt [arg]) =
    let se = signalAExprWithStems env stemMap arg
    in case signalExprType se of
           SignalBit inW ->
               let padW = outW - inW
               in if padW <= 0
                  then se
                  else mkSignalConcat [mkSignalInt (SignalBit padW) 0, se]
           _ -> SignalPrim (SignalBit outW) PrimZeroExt [se]
signalAExprWithStems env stemMap (APrim _ (ATBit outW) PrimTrunc [arg]) =
    let se = signalAExprWithStems env stemMap arg
    in case signalExprType se of
           SignalBit inW ->
               mkSignalExtract outW inW se (mkSignalNat (outW - 1)) (mkSignalNat 0)
           _ -> SignalPrim (SignalBit outW) PrimTrunc [se]
signalAExprWithStems env stemMap (APrim _ _ PrimConcat es) =
    mkSignalConcat (map (signalAExprWithStems env stemMap) es)
signalAExprWithStems env stemMap (APrim _ ty p es) =
    SignalPrim (signalAType ty) p (map (signalAExprWithStems env stemMap) es)
signalAExprWithStems env _ (ATupleSel ty (AMethValue _ instId methId) idx1) =
    lowerResolvedMethodRead env Nothing (signalAType ty) instId methId (Just idx1) []
signalAExprWithStems env stemMap (ATupleSel ty (AMethCall _ instId methId rawArgs) idx1) =
    let sArgs = map (signalAExprWithStems env stemMap) (dropAPrimUnitArgs rawArgs)
    in lowerResolvedMethodRead env Nothing (signalAType ty) instId methId (Just idx1) sArgs
signalAExprWithStems env stemMap (ATupleSel ty inner idx1) =
    mkSignalTupleSel (signalAType ty) (signalAExprWithStems env stemMap inner) idx1
signalAExprWithStems env _ (AMethValue ty instId methId) =
    lowerResolvedMethodRead env Nothing (signalAType ty) instId methId Nothing []
signalAExprWithStems env stemMap (AMethCall ty instId methId rawArgs) =
    let sArgs = map (signalAExprWithStems env stemMap) (dropAPrimUnitArgs rawArgs)
    in lowerResolvedMethodRead env Nothing (signalAType ty) instId methId Nothing sArgs
signalAExprWithStems env stemMap (ATuple ty elems) =
    mkSignalTuple (signalAType ty) (map (signalAExprWithStems env stemMap) elems)
signalAExprWithStems env stemMap (ANoInlineFunCall ty objId (ANoInlineFun fn params ports _) rawArgs) =
    SignalNoInlineCall
        (signalAType ty)
        (cleanGenBase objId)
        fn
        params
        ports
        (map (signalAExprWithStems env stemMap) rawArgs)
signalAExprWithStems env stemMap (AFunCall ty _ fn isCFlag rawArgs) =
    SignalFunCall (signalAType ty) fn isCFlag (map (signalAExprWithStems env stemMap) rawArgs)
signalAExprWithStems _ _ (ATaskValue ty _ fn isCFlag cookie) =
    SignalTaskValue (signalAType ty) fn isCFlag cookie
signalAExprWithStems env _ (ASPort ty pid) =
    mkSignalPort env (signalAType ty) (getIdBaseString pid)
signalAExprWithStems _ _ (ASParam ty pid) =
    SignalParam (signalAType ty) (getIdBaseString pid)
signalAExprWithStems env stemMap (ASDef ty did) =
    let nm = M.findWithDefault (getIdBaseString did) did stemMap
    in mkDefOrPort env (signalAType ty) nm
signalAExprWithStems _ _ (ASInt _ ty lit) =
    mkSignalInt (signalAType ty) (ilValue lit)
signalAExprWithStems _ _ (ASReal _ ty r) =
    SignalRealLit (signalAType ty) r
signalAExprWithStems _ _ (ASStr _ ty s) =
    SignalStr (signalAType ty) s
signalAExprWithStems env stemMap (ASAny ty mVal) =
    SignalAny (signalAType ty) (fmap (signalAExprWithStems env stemMap) mVal)
signalAExprWithStems env stemMap (ASClock _ (AClock osc gate)) =
    SignalClock
        (signalAExprWithStems env stemMap osc)
        (signalAExprWithStems env stemMap gate)
signalAExprWithStems env stemMap (ASReset _ (AReset wire)) =
    SignalReset (signalAExprWithStems env stemMap wire)
signalAExprWithStems env stemMap (ASInout ty (AInout wire)) =
    SignalInout (signalAType ty) (signalAExprWithStems env stemMap wire)
signalAExprWithStems env _ (AMGate _ instId clkId) =
    resolveOutputGate env Nothing instId clkId

-- ============================================================
-- Whole-APackage unnamed temporary naming (for AConv)
-- ============================================================

nameAPackageDefs :: Flags -> APackage -> M.Map Id Id
nameAPackageDefs flags apkg =
    let env = mkSignalNamingEnvFromAPackage flags apkg
        ds = apkg_local_defs apkg
        nds = zip [0 :: Int ..] ds
        posMap = M.fromList [ (i, n) | (n, ADef i _ _ _) <- nds ]
        deps e = mapMaybe (\v -> M.lookup v posMap) (aVars e)
        graph = [ (n, deps e) | (n, ADef _ _ e _) <- nds ]
        defByIdx = IM.fromList nds
        topoIdxs =
            case tsort graph of
                Right ns -> ns
                Left sccs ->
                    internalError ("ASignalNaming.nameAPackageDefs: cycle: " ++ ppReadable sccs)
        dummyTempId = setBadId (mk_homeless_id "_")
        (_, !exprByIdx) =
            L.foldl'
                (\(!stemAcc, !seAcc) n ->
                    case IM.lookup n defByIdx of
                        Nothing -> (stemAcc, seAcc)
                        Just (ADef i _ e _)
                            | isDefTempId env i ->
                                let se = signalAExprWithStems env stemAcc e
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
                let se = IM.findWithDefault (signalAExpr env e) n exprByIdx
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
