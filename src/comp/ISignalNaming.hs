{-# LANGUAGE BangPatterns #-}
module ISignalNaming (
    IProvLookup,
    iModuleProvenanceLookup,
    mkSignalNamingEnvFromIModule,
    mkSignalNamingEnvFromParts,
    signalIType,
    signalIExpr,
    signalIExprWithStems,
    signalIExprWithProv,
    signalIExprStem,
    exprNameFromIExpr,
    nameIModuleDefs
) where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

import ErrorUtil (internalError)
import Eval (rnf)
import Flags (Flags)
import ISyntax (
    ConTagInfo(..),
    IAbstractInput(..),
    IClock,
    IConInfo(..),
    IDef(..),
    IEFace(..),
    IExpr(..),
    IInout,
    IKind,
    IModule(..),
    IRule(..),
    IRules(..),
    IReset,
    IStateVar,
    IType(..),
    getClockWires,
    getInoutWire,
    getResetWire,
    getVModInfo,
    itArrow
    )
import ISyntaxSubst (tSubst)
import ISyntaxUtil (getInout_Size, isIfWrapper, isitInout_, itBit1, itPrimUnit, itReal, itString)
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
import PreIds (
    idAVValue_,
    idActionValue_,
    idBit,
    idClockGate,
    idClockOsc,
    idInout_,
    idNoReset,
    idPrimArray,
    idPrimFst,
    idPrimPair,
    idPrimSnd,
    idPrimUnit
    )
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
    exprName,
    ifcBoundaryNames,
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
    resolveOutputOsc,
    selectProvenanceSignalExpr,
    signalExprStem,
    signalExprType,
    wireInfoSpecialNames
    )
import SourceProvenance (NamedLitInfo, ProvFact(..), dedupFacts)
import VModInfo (
    VArgInfo(..),
    VName(..),
    VWireInfo(..),
    getVNameString,
    lookupIfcInoutWire,
    lookupOutputResetWire
    )

-- ============================================================
-- IModule environment construction
-- ============================================================

mkSignalNamingEnvFromParts ::
    Flags ->
    [(Id, IKind)] ->
    [IAbstractInput] ->
    VWireInfo ->
    [(Id, IStateVar a)] ->
    [IEFace a] ->
    IRules a ->
    [IDef a] ->
    SignalNamingEnv
mkSignalNamingEnvFromParts flags typeArgs wireArgs extWires stateInsts ifc rules defs =
    let instPairs = [ (instId, getVModInfo isv) | (instId, isv) <- stateInsts ]
        baseEnv = mkSignalNamingEnv flags instPairs [] []
        (wiParams, wiClkRst) = wireInfoSpecialNames extWires
        typeParamNames = [ getIdBaseString i | (i, _) <- typeArgs ]
        (argPorts, argParams, argClkRst) =
            L.foldl'
                collectIAbstract
                ([], [], [])
                (zip wireArgs (wArgs extWires ++ repeat (Port (VName "", []) Nothing Nothing)))
        (ifcPorts, ifcInouts, ifcRuleIds) =
            L.foldl'
                (\(ps, cs, rids) (IEFace _ its _ mrs _ fi) ->
                    let (p, c) = ifcBoundaryNames fi
                        argPs = [ getIdBaseString i | grp <- its, (i, _) <- grp ]
                        mRids = maybe [] (\(IRules _ rs) -> map irule_name rs) mrs
                    in (argPs ++ p ++ ps, c ++ cs, mRids ++ rids))
                ([], [], [])
                ifc
        IRules _ modRules = rules
        allRuleIds = map irule_name modRules ++ ifcRuleIds
        ruleFireNames =
            concatMap
                (\rid ->
                    [ getIdBaseString (mkIdWillFire rid)
                    , getIdBaseString (mkIdCanFire rid)
                    ])
                allRuleIds
        explicitKeep =
            [ getIdBaseString i
            | IDef i _ _ _ <- defs
            , hasIdProp i IdPProbe ||
              hasIdProp i IdP_keepEvenUnused ||
              hasIdProp i IdPMethodPredicate
            ]
    in baseEnv
           { sne_top_ports = S.fromList (argPorts ++ ifcPorts)
           , sne_params = S.fromList (typeParamNames ++ wiParams ++ argParams)
           , sne_clk_rst_inouts = S.fromList (wiClkRst ++ argClkRst ++ ifcInouts)
           , sne_explicit_anchors = S.fromList (explicitKeep ++ ruleFireNames)
           }
  where
    collectIAbstract ::
        ([String], [String], [String]) ->
        (IAbstractInput, VArgInfo) ->
        ([String], [String], [String])
    collectIAbstract (ps, pms, crs) (inp, varg) =
        case (inp, varg) of
            (IAI_Port (i, _), Param _) -> (ps, getIdBaseString i : pms, crs)
            (IAI_Port (i, _), _) -> (getIdBaseString i : ps, pms, crs)
            (IAI_Clock osc mGate, _) ->
                let g = maybe [] (\gi -> [getIdBaseString gi]) mGate
                in (ps, pms, getIdBaseString osc : (g ++ crs))
            (IAI_Reset r, _) -> (ps, pms, getIdBaseString r : crs)
            (IAI_Inout io _, _) -> (ps, pms, getIdBaseString io : crs)

mkSignalNamingEnvFromIModule :: Flags -> IModule a -> SignalNamingEnv
mkSignalNamingEnvFromIModule flags imod =
    mkSignalNamingEnvFromParts
        flags
        (imod_type_args imod)
        (imod_wire_args imod)
        (imod_external_wires imod)
        (imod_state_insts imod)
        (imod_interface imod)
        (imod_rules imod)
        (imod_local_defs imod)

-- ============================================================
-- ISyntax type lowering
-- ============================================================

signalIType :: IType -> SignalType
signalIType (ITAp (ITCon b _ _) (ITNum n))
    | b == idBit = SignalBit n
    | b == idInout_ = SignalAbstract (getIdString idInout_) [n]
signalIType (ITAp (ITCon i _ _) inner)
    | i == idActionValue_ = signalIType inner
signalIType t@(ITAp (ITAp (ITCon p _ _) _) _)
    | p == idPrimPair = SignalTuple (signalITupleTypes t)
signalIType t
    | t == itReal = SignalReal
    | t == itString = SignalString Nothing
signalIType (ITCon i _ _)
    | i == idPrimUnit = SignalBit 0
signalIType (ITAp (ITCon r _ _) elemTy)
    | r == idPrimArray = SignalArray 0 (signalIType elemTy)
signalIType t = extractAbstract t
  where
    extractAbstract :: IType -> SignalType
    extractAbstract (ITCon i _ _) = SignalAbstract (getIdString i) []
    extractAbstract (ITAp fn _) = extractAbstract fn
    extractAbstract _ = SignalUnknownType

signalITupleTypes :: IType -> [SignalType]
signalITupleTypes t
    | t == itPrimUnit = []
signalITupleTypes (ITAp (ITAp (ITCon p _ _) t1) t2)
    | p == idPrimPair = signalIType t1 : signalITupleTypes t2
signalITupleTypes t = [signalIType t]

signalITypeExpr :: IExpr a -> IType -> SignalType
signalITypeExpr (ICon _ (ICString _ s)) t
    | t == itString = SignalString (Just (toInteger (length s)))
signalITypeExpr _ t = signalIType t

instAndApplyIType :: IType -> [IType] -> Int -> IType
instAndApplyIType ty0 tyArgs nValArgs =
    dropArrows nValArgs (instForAlls ty0 tyArgs)
  where
    instForAlls :: IType -> [IType] -> IType
    instForAlls (ITForAll v _ body) (ta : tas) =
        instForAlls (tSubst v ta body) tas
    instForAlls ty _ = ty

    dropArrows :: Int -> IType -> IType
    dropArrows k (ITAp (ITAp arr _) res)
        | k > 0 && arr == itArrow = dropArrows (k - 1) res
    dropArrows _ ty = ty

-- ============================================================
-- ISyntax provenance lookup & expression lowering
-- ============================================================

type IProvLookup a = Maybe Id -> IExpr a -> [ProvFact (IExpr a)]

noIProv :: IProvLookup a
noIProv _ _ = []

iDefIdOf :: IExpr a -> Maybe Id
iDefIdOf (ICon i (ICValue {})) = Just i
iDefIdOf (ICon i (ICDef {})) = Just i
iDefIdOf (IVar i) = Just i
iDefIdOf _ = Nothing

containsIUndet :: IExpr a -> Bool
containsIUndet (ICon _ (ICUndet {})) = True
containsIUndet (IAps f _ es) = containsIUndet f || any containsIUndet es
containsIUndet (ILam _ _ b) = containsIUndet b
containsIUndet (ILAM _ _ b) = containsIUndet b
containsIUndet _ = False

iModuleProvenanceLookup ::
    [IDef a] ->
    [(IExpr a, [ProvFact (IExpr a)])] ->
    IProvLookup a
iModuleProvenanceLookup _ [] = noIProv
iModuleProvenanceLookup defs prov = lookupFn
  where
    combine new old = dedupFacts (old ++ new)
    byDefId0 =
        M.fromListWith
            combine
            [ (i, dedupFacts fs)
            | (k, fs) <- prov
            , not (null fs)
            , Just i <- [iDefIdOf k]
            ]
    byExpr0 =
        M.fromListWith
            combine
            [ (k, dedupFacts fs)
            | (k, fs) <- prov
            , not (null fs)
            , Nothing <- [iDefIdOf k]
            ]
    byExpr =
        L.foldl'
            (\acc (IDef i _ rhs _) ->
                if containsIUndet rhs
                then acc
                else case M.lookup i byDefId0 of
                         Just fs -> M.insertWith combine rhs fs acc
                         Nothing -> acc)
            byExpr0
            defs
    byDefId =
        L.foldl'
            (\acc (IDef i _ rhs _) ->
                if containsIUndet rhs
                then acc
                else case M.lookup rhs byExpr0 of
                         Just fs -> M.insertWith combine i fs acc
                         Nothing -> acc)
            byDefId0
            defs
    lookupFn (Just sid) expr =
        let fs1 = M.findWithDefault [] sid byDefId
            fs2 =
                if containsIUndet expr
                then []
                else M.findWithDefault [] expr byExpr
        in if null fs2
           then fs1
           else if null fs1
                then fs2
                else dedupFacts (fs1 ++ fs2)
    lookupFn Nothing expr =
        M.findWithDefault [] expr byExpr

iLitMatches :: IExpr a -> NamedLitInfo -> Bool
iLitMatches (ICon _ (ICInt { iConType = t, iVal = lit })) nli
    | SignalBit w <- signalIType t = nliMatchesBitLit w (ilValue lit) nli
iLitMatches _ _ = False

isSelfIExpr :: Maybe Id -> IExpr a -> IExpr a -> Bool
isSelfIExpr mSelfId cur candidate
    | candidate == cur = True
    | Just sid <- mSelfId
    , Just cid <- iDefIdOf candidate = cid == sid
    | otherwise = False

skipsIProv :: IExpr a -> Bool
skipsIProv (IVar {}) = True
skipsIProv (IRefT {}) = True
skipsIProv (ILam {}) = True
skipsIProv (ICon _ (ICValue {})) = True
skipsIProv (ICon _ (ICDef {})) = True
skipsIProv (ICon _ (ICModPort {})) = True
skipsIProv (ICon _ (ICMethArg {})) = True
skipsIProv (ICon _ (ICModParam {})) = True
skipsIProv (ICon _ (ICClock {})) = True
skipsIProv (ICon _ (ICReset {})) = True
skipsIProv (ICon _ (ICInout {})) = True
skipsIProv _ = False

fastSignalIType :: IExpr a -> Maybe SignalType
fastSignalIType expr@(ICon _ ic) =
    Just (signalITypeExpr expr (iConType ic))
fastSignalIType (IAps (ICon _ (ICPrim _ PrimSelect)) [ITNum w, _, _] _) =
    Just (SignalBit w)
fastSignalIType (IAps (ICon _ (ICPrim _ PrimExtract)) [_, _, ITNum outW] _) =
    Just (SignalBit outW)
fastSignalIType (IAps (ICon _ (ICPrim clTy PrimTrunc)) ts _) =
    Just (signalIType (instAndApplyIType clTy ts 1))
fastSignalIType (IAps (ICon _ (ICPrim clTy PrimZeroExt)) ts _) =
    Just (signalIType (instAndApplyIType clTy ts 1))
fastSignalIType (IAps (ICon _ (ICPrim _ PrimSignExt)) [_, _, ITNum outW] _) =
    Just (SignalBit outW)
fastSignalIType (IAps (ICon _ (ICPrim _ p)) _ _)
    | isBoolOrCmpPrim p = Just (SignalBit 1)
fastSignalIType _ = Nothing

signalIExpr :: SignalNamingEnv -> IExpr a -> SignalExpr
signalIExpr env expr = signalIExprWithStems env M.empty expr

signalIExprWithStems :: SignalNamingEnv -> M.Map Id String -> IExpr a -> SignalExpr
signalIExprWithStems env stemMap expr =
    signalIExprWithProv env noIProv stemMap Nothing Nothing expr

signalIExprWithProv ::
    SignalNamingEnv ->
    IProvLookup a ->
    M.Map Id String ->
    Maybe Id ->
    Maybe SignalType ->
    IExpr a ->
    SignalExpr
signalIExprWithProv env lookupProv stemMap mSelfId mKnownTy expr =
    signalIExprGo env lookupProv stemMap M.empty mSelfId mKnownTy 0 expr

signalIExprStem :: SignalNamingEnv -> IExpr a -> String
signalIExprStem env expr = signalExprStem env (signalIExpr env expr)

exprNameFromIExpr :: SignalNamingEnv -> IExpr a -> ExprName
exprNameFromIExpr env expr = exprName env (signalIExpr env expr)

signalIExprGo ::
    SignalNamingEnv ->
    IProvLookup a ->
    M.Map Id String ->
    M.Map String SignalType ->
    Maybe Id ->
    Maybe SignalType ->
    Int ->
    IExpr a ->
    SignalExpr
signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth (IAps (IAps f ts1 es1) ts2 es2) =
    signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth (IAps f (ts1 ++ ts2) (es1 ++ es2))
signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth (IAps f [] []) =
    signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth f
signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth (ILAM _ _ body) =
    signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth body
signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth (IAps (ICon _ (ICPrim _ PrimRange)) _ [_, _, arg]) =
    signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth arg
signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth (IAps (ICon _ (ICPrim _ p)) _ [arg])
    | isIfWrapper p =
        signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth arg
signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth (ICon _ (ICLazyPack { lzApplied = app })) =
    signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth app
signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth (ICon _ (ICLazyUnpack { lzApplied = app })) =
    signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth app
signalIExprGo env lookupProv stemMap tyCtx mSelfId mKnownTy depth expr
    | depth < 8 && not (skipsIProv expr) =
        let facts = lookupProv mSelfId expr
            structural = signalIExprStructural env lookupProv stemMap tyCtx (depth + 1) facts expr
            resTy =
                case mKnownTy of
                    Just kt -> kt
                    Nothing ->
                        case fastSignalIType expr of
                            Just ft -> ft
                            Nothing -> signalExprType structural
        in case if null facts
                then Nothing
                else selectProvenanceSignalExpr
                         env
                         resTy
                         (isSelfIExpr mSelfId expr)
                         (signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing (depth + 1))
                         (iLitMatches expr)
                         facts of
               Just se -> se
               Nothing -> structural
    | otherwise =
        signalIExprStructural env lookupProv stemMap tyCtx (depth + 1) [] expr

lowerIArgWithFacts ::
    SignalNamingEnv ->
    IProvLookup a ->
    M.Map Id String ->
    M.Map String SignalType ->
    Int ->
    [ProvFact (IExpr a)] ->
    (Int, IExpr a) ->
    SignalExpr
lowerIArgWithFacts env lookupProv stemMap tyCtx nextDepth facts (argIdx, arg)
    | not (null facts)
    , Just nli <- lookupArgNamedLit argIdx facts
    , iLitMatches arg nli =
        let argTy =
                case fastSignalIType arg of
                    Just ft -> ft
                    Nothing ->
                        signalExprType
                            (signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth arg)
        in SignalNamedLit argTy nli
    | otherwise =
        signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth arg

signalIExprStructural ::
    SignalNamingEnv ->
    IProvLookup a ->
    M.Map Id String ->
    M.Map String SignalType ->
    Int ->
    [ProvFact (IExpr a)] ->
    IExpr a ->
    SignalExpr
signalIExprStructural env lookupProv stemMap tyCtx nextDepth _ (ILam i vt body) =
    let vStr = getIdBaseString i
        svt = signalIType vt
    in SignalLam
           vStr
           svt
           (signalIExprGo env lookupProv stemMap (M.insert vStr svt tyCtx) Nothing Nothing nextDepth body)
signalIExprStructural env lookupProv stemMap tyCtx nextDepth _ (ILAM _ _ body) =
    signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth body
signalIExprStructural _ _ _ tyCtx _ _ (IVar i) =
    let s = getIdBaseString i
        ty = M.findWithDefault SignalUnknownType s tyCtx
    in SignalDef ty s
signalIExprStructural env _ _ _ _ _ (IRefT t ptr _ _) =
    let ty = signalIType t
    in case M.lookup ptr (sne_heap_names env) of
           Just nm -> mkDefOrPort env ty nm
           Nothing -> SignalHeapRef ty ptr
signalIExprStructural env lookupProv stemMap tyCtx nextDepth _ e@(ICon i ic) =
    signalICon env lookupProv stemMap tyCtx nextDepth e i ic
signalIExprStructural env lookupProv stemMap tyCtx nextDepth facts e@(IAps f ts es) =
    signalIAps env lookupProv stemMap tyCtx nextDepth facts e f ts es

signalICon ::
    SignalNamingEnv ->
    IProvLookup a ->
    M.Map Id String ->
    M.Map String SignalType ->
    Int ->
    IExpr a ->
    Id ->
    IConInfo a ->
    SignalExpr
signalICon env lookupProv stemMap tyCtx nextDepth fullExpr i ic
    | i == idPrimUnit = mkSignalInt (SignalBit 0) 0
    | otherwise =
        case ic of
            ICModPort { iConType = t } ->
                mkSignalPort env (signalITypeExpr fullExpr t) (getIdBaseString i)
            ICMethArg { iConType = t } ->
                mkSignalPort env (signalITypeExpr fullExpr t) (getIdBaseString i)
            ICModParam { iConType = t } ->
                SignalParam (signalITypeExpr fullExpr t) (getIdBaseString i)
            ICValue { iConType = t } ->
                let nm = M.findWithDefault (getIdBaseString i) i stemMap
                in mkDefOrPort env (signalITypeExpr fullExpr t) nm
            ICDef { iConType = t } ->
                let nm = M.findWithDefault (getIdBaseString i) i stemMap
                in mkDefOrPort env (signalITypeExpr fullExpr t) nm
            ICInt { iConType = t, iVal = lit } ->
                mkSignalInt (signalITypeExpr fullExpr t) (ilValue lit)
            ICReal { iConType = t, iReal = r } ->
                SignalRealLit (signalITypeExpr fullExpr t) r
            ICString { iConType = t, iStr = s } ->
                SignalStr (signalITypeExpr fullExpr t) s
            ICChar { iChar = c } ->
                SignalStr (SignalString (Just 1)) [c]
            ICUndet { iConType = t, imVal = mv } ->
                SignalAny
                    (signalITypeExpr fullExpr t)
                    (fmap (signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth) mv)
            ICPrim { iConType = t, primOp = p } ->
                SignalPrim (signalITypeExpr fullExpr t) p []
            ICCon { iConType = ITAp _ t, conTagInfo = cti }
                | t == itBit1 ->
                    mkSignalInt (SignalBit 1) (if conNo cti /= 0 then 1 else 0)
            ICForeign { iConType = t, fName = fn, isC = isCFlag, foports = Nothing } ->
                SignalFunCall (signalITypeExpr fullExpr t) fn isCFlag []
            ICForeign { iConType = t, fName = fn, isC = False, foports = Just ops } ->
                SignalNoInlineCall (signalITypeExpr fullExpr t) (cleanGenBase i) fn [] ops []
            ICClock { iClock = clk } ->
                signalIClock env lookupProv stemMap tyCtx nextDepth clk
            ICReset { iReset = rst } ->
                signalIReset env lookupProv stemMap tyCtx nextDepth rst
            ICInout { iConType = it, iInout = iot } ->
                signalIInout env lookupProv stemMap tyCtx nextDepth it iot
            ICLazyPack { lzApplied = app } ->
                signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth app
            ICLazyUnpack { lzApplied = app } ->
                signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth app
            _ ->
                SignalOther (signalIType (iConType ic)) (getIdBaseString i) []

signalIAps ::
    SignalNamingEnv ->
    IProvLookup a ->
    M.Map Id String ->
    M.Map String SignalType ->
    Int ->
    [ProvFact (IExpr a)] ->
    IExpr a ->
    IExpr a ->
    [IType] ->
    [IExpr a] ->
    SignalExpr
signalIAps env lookupProv stemMap tyCtx nextDepth _ _ (ICon _ (ICPrim _ PrimSelect)) [ITNum w, ITNum lo, ITNum inW] [arg] =
    let se = signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth arg
    in mkSignalExtract w inW se (mkSignalNat (w + lo - 1)) (mkSignalNat lo)
signalIAps env lookupProv stemMap tyCtx nextDepth _ _ (ICon _ (ICPrim _ PrimExtract)) [ITNum inW, _, ITNum outW] [arg, hi, lo] =
    let se = signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth arg
        shi = signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth hi
        slo = signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth slo_e
        slo_e = lo
    in mkSignalExtract outW inW se shi slo
signalIAps env lookupProv stemMap tyCtx nextDepth _ _ (ICon _ (ICPrim _ PrimRange)) _ [_, _, arg] =
    signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth arg
signalIAps
    env
    lookupProv
    stemMap
    tyCtx
    nextDepth
    _
    _
    (ICon _ (ICPrim _ PrimIntegerToBit))
    _
    [IAps (ICon _ (ICPrim _ PrimStringToInteger)) _ [s]] =
    signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth s
signalIAps env lookupProv stemMap tyCtx nextDepth _ _ (ICon _ (ICPrim _ p)) _ [arg]
    | isIfWrapper p = signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth arg
signalIAps env lookupProv stemMap tyCtx nextDepth _ _ (ICon _ (ICPrim clTy PrimZeroExt)) ts [arg] =
    let se = signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth arg
        resTy = signalIType (instAndApplyIType clTy ts 1)
    in case (resTy, signalExprType se) of
           (SignalBit outW, SignalBit inW) ->
               let padW = outW - inW
               in if padW <= 0
                  then se
                  else mkSignalConcat [mkSignalInt (SignalBit padW) 0, se]
           _ -> SignalPrim resTy PrimZeroExt [se]
signalIAps env lookupProv stemMap tyCtx nextDepth _ _ (ICon _ (ICPrim clTy PrimTrunc)) ts [arg] =
    let se = signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth arg
        resTy = signalIType (instAndApplyIType clTy ts 1)
    in case (resTy, signalExprType se) of
           (SignalBit outW, SignalBit inW) ->
               mkSignalExtract outW inW se (mkSignalNat (outW - 1)) (mkSignalNat 0)
           _ -> SignalPrim resTy PrimTrunc [se]
signalIAps env lookupProv stemMap tyCtx nextDepth _ _ (ICon _ (ICPrim _ PrimSignExt)) [_, _, ITNum outW] es =
    SignalPrim
        (SignalBit outW)
        PrimSignExt
        (map (signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth) es)
signalIAps env lookupProv stemMap tyCtx nextDepth _ _ (ICon _ (ICPrim _ PrimConcat)) _ es =
    mkSignalConcat (map (signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth) es)
signalIAps env lookupProv stemMap tyCtx nextDepth facts _ (ICon _ (ICPrim clTy p)) ts es =
    let ses =
            map
                (lowerIArgWithFacts env lookupProv stemMap tyCtx nextDepth facts)
                (zip [0 :: Int ..] es)
        fallbackTy = signalIType (instAndApplyIType clTy ts (length es))
        resTy = inferSignalPrimType p ts ses fallbackTy
    in SignalPrim resTy p ses
signalIAps env lookupProv stemMap tyCtx nextDepth facts fullExpr (ICon _ (ICSel {})) _ _ =
    let (sels, baseExprs, mInnerExpr) = unfoldISelsWithInner fullExpr
        innerFacts =
            case mInnerExpr of
                Just ie | null facts -> lookupProv Nothing ie
                _ -> facts
    in signalISelChain env lookupProv stemMap tyCtx nextDepth innerFacts sels baseExprs
signalIAps env lookupProv stemMap tyCtx nextDepth _ _ f@(ICon i _) ts es
    | i == idPrimPair =
        let elems = flattenIPairChain env lookupProv stemMap tyCtx nextDepth (IAps f ts es)
            ty = SignalTuple (map signalExprType elems)
        in mkSignalTuple ty elems
signalIAps _ _ _ _ _ _ _ (ICon _ (ICCon { iConType = ITAp _ t, conTagInfo = cti })) _ _
    | t == itBit1 =
        mkSignalInt (SignalBit 1) (if conNo cti /= 0 then 1 else 0)
signalIAps
    env
    lookupProv
    stemMap
    tyCtx
    nextDepth
    facts
    _
    (ICon _ (ICForeign { iConType = clTy, fName = fn, isC = isCFlag, foports = Nothing }))
    ts
    es =
    let resTy = signalIType (instAndApplyIType clTy ts (length es))
        ses =
            map
                (lowerIArgWithFacts env lookupProv stemMap tyCtx nextDepth facts)
                (zip [0 :: Int ..] es)
    in SignalFunCall resTy fn isCFlag ses
signalIAps
    env
    lookupProv
    stemMap
    tyCtx
    nextDepth
    facts
    _
    (ICon
        i
        (ICForeign
            { iConType = clTy
            , fName = fn
            , isC = False
            , foports = Just ops
            , fTyVarNames = tvns
            }))
    ts
    es =
    let resTy = signalIType (instAndApplyIType clTy ts (length es))
        nvals = [ n | ITNum n <- ts ]
        params = zip tvns nvals
        ses =
            map
                (lowerIArgWithFacts env lookupProv stemMap tyCtx nextDepth facts)
                (zip [0 :: Int ..] es)
    in SignalNoInlineCall resTy (cleanGenBase i) fn params ops ses
signalIAps env lookupProv stemMap tyCtx nextDepth facts _ f ts es =
    let sf = signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth f
        ses =
            map
                (lowerIArgWithFacts env lookupProv stemMap tyCtx nextDepth facts)
                (zip [0 :: Int ..] es)
        resTy =
            case f of
                ICon _ ic -> signalIType (instAndApplyIType (iConType ic) ts (length es))
                _ -> signalExprType sf
        tag =
            case f of
                ICon i _ -> M.findWithDefault (getIdBaseString i) i stemMap
                IVar i -> getIdBaseString i
                _ -> "ap"
    in SignalOther resTy tag ses

unfoldISelsWithInner :: IExpr a -> ([(Id, SignalType)], [IExpr a], Maybe (IExpr a))
unfoldISelsWithInner (IAps (IAps f ts1 es1) ts2 es2) =
    unfoldISelsWithInner (IAps f (ts1 ++ ts2) (es1 ++ es2))
unfoldISelsWithInner (IAps (ICon i (ICSel { iConType = clTy })) ts [inner]) =
    let selTy = signalIType (instAndApplyIType clTy ts 1)
        (restSels, base, mInner) = unfoldISelsWithInner inner
    in ((i, selTy) : restSels, base, mInner)
unfoldISelsWithInner full@(IAps (ICon i (ICSel { iConType = clTy })) ts args@(_ : _)) =
    let selTy = signalIType (instAndApplyIType clTy ts (length args))
    in ([(i, selTy)], args, Just full)
unfoldISelsWithInner other = ([], [other], Nothing)

isTupleSelId :: Id -> Bool
isTupleSelId s = s == idPrimFst || s == idPrimSnd

isIUnitArg :: IExpr a -> Bool
isIUnitArg (ICon i _) = i == idPrimUnit
isIUnitArg _ = False

signalISelChain ::
    SignalNamingEnv ->
    IProvLookup a ->
    M.Map Id String ->
    M.Map String SignalType ->
    Int ->
    [ProvFact (IExpr a)] ->
    [(Id, SignalType)] ->
    [IExpr a] ->
    SignalExpr
signalISelChain
    _
    _
    _
    _
    _
    _
    [(m, ty)]
    [ICon _ (ICForeign { fName = fn, isC = isCFlag, foports = Nothing, fcallNo = Just cookie })]
    | m == idAVValue_ =
        SignalTaskValue ty fn isCFlag cookie
signalISelChain
    _
    _
    _
    _
    _
    _
    [(m, ty)]
    [IAps (ICon _ (ICForeign { fName = fn, isC = isCFlag, foports = Nothing, fcallNo = Just cookie })) _ _]
    | m == idAVValue_ =
        SignalTaskValue ty fn isCFlag cookie
signalISelChain env lookupProv stemMap tyCtx nextDepth facts sels (ICon instId (ICStateVar { iVar = sv }) : rawArgs)
    | (pfx@((_, elemTy) : _), [(methId, _)]) <- span (isTupleSelId . fst) sels =
        let indexedArgs = [ (k, a) | (k, a) <- zip [1 :: Int ..] rawArgs, not (isIUnitArg a) ]
            sArgs = map (lowerIArgWithFacts env lookupProv stemMap tyCtx nextDepth facts) indexedArgs
            idx1 = toInteger (length (filter ((== idPrimSnd) . fst) pfx)) + 1
        in lowerResolvedMethodRead env (Just (getVModInfo sv)) elemTy instId methId (Just idx1) sArgs
signalISelChain env _ _ _ _ _ sels (ICon instId (ICStateVar { iVar = sv }) : _)
    | (pfx, [(iav, avTupTy), (methId, _)]) <- span (isTupleSelId . fst) sels
    , iav == idAVValue_ =
        case pfx of
            [] ->
                lowerResolvedMethodRead env (Just (getVModInfo sv)) avTupTy instId methId Nothing []
            ((_, elemTy) : _) ->
                let idx1 = toInteger (length (filter ((== idPrimSnd) . fst) pfx)) + 1
                in lowerResolvedMethodRead env (Just (getVModInfo sv)) elemTy instId methId (Just idx1) []
signalISelChain env lookupProv stemMap tyCtx nextDepth facts [(methId, resTy)] (ICon instId (ICStateVar { iVar = sv }) : rawArgs) =
    let indexedArgs = [ (k, a) | (k, a) <- zip [1 :: Int ..] rawArgs, not (isIUnitArg a) ]
        sArgs = map (lowerIArgWithFacts env lookupProv stemMap tyCtx nextDepth facts) indexedArgs
    in lowerResolvedMethodRead env (Just (getVModInfo sv)) resTy instId methId Nothing sArgs
signalISelChain env lookupProv stemMap tyCtx nextDepth _ [(m, _)] [ICon _ (ICClock { iClock = clk })]
    | m == idClockGate =
        case signalIClock env lookupProv stemMap tyCtx nextDepth clk of
            SignalClock _ gate -> gate
            other -> other
    | m == idClockOsc =
        case signalIClock env lookupProv stemMap tyCtx nextDepth clk of
            SignalClock osc _ -> osc
            other -> other
signalISelChain env lookupProv stemMap tyCtx nextDepth _ sels@((_, elemTy) : _) [base]
    | all (isTupleSelId . fst) sels =
        let idx1 = toInteger (length (filter ((== idPrimSnd) . fst) sels)) + 1
        in mkSignalTupleSel elemTy (signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth base) idx1
signalISelChain env lookupProv stemMap tyCtx nextDepth _ sels baseExprs =
    let resTy =
            case sels of
                ((_, t) : _) -> t
                [] -> SignalUnknownType
        selTag = L.intercalate "." (map (getIdBaseString . fst) sels)
    in SignalOther
           resTy
           selTag
           (map (signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth) baseExprs)

signalIClock ::
    SignalNamingEnv ->
    IProvLookup a ->
    M.Map Id String ->
    M.Map String SignalType ->
    Int ->
    IClock a ->
    SignalExpr
signalIClock env lookupProv stemMap tyCtx nextDepth clk =
    case getClockWires clk of
        IAps (ICon _ (ICTuple { fieldIds = [fOsc, fGate] })) _ [eOsc, eGate]
            | fOsc == idClockOsc && fGate == idClockGate ->
                SignalClock
                    (signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth eOsc)
                    (signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth eGate)
        IAps (ICon clkId (ICSel {})) _ [ICon instId (ICStateVar { iVar = sv })] ->
            let vmi = getVModInfo sv
            in SignalClock
                   (resolveOutputOsc env (Just vmi) instId clkId)
                   (resolveOutputGate env (Just vmi) instId clkId)
        other ->
            let cw = signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth other
            in SignalClock cw (mkSignalInt (SignalBit 1) 1)

signalIReset ::
    SignalNamingEnv ->
    IProvLookup a ->
    M.Map Id String ->
    M.Map String SignalType ->
    Int ->
    IReset a ->
    SignalExpr
signalIReset env lookupProv stemMap tyCtx nextDepth rst =
    case getResetWire rst of
        IAps (ICon rstId (ICSel {})) _ [ICon instId (ICStateVar { iVar = sv })] ->
            let rstVn = lookupOutputResetWire rstId (getVModInfo sv)
            in SignalReset
                   (SignalPort (SignalBit 1) (getIdBaseString instId ++ "$" ++ getVNameString rstVn))
        ICon i (ICPrim _ PrimResetUnassertedVal)
            | i == idNoReset ->
                SignalReset (SignalPrim (SignalBit 1) PrimResetUnassertedVal [])
        other ->
            SignalReset (signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth other)

signalIInout ::
    SignalNamingEnv ->
    IProvLookup a ->
    M.Map Id String ->
    M.Map String SignalType ->
    Int ->
    IType ->
    IInout a ->
    SignalExpr
signalIInout env lookupProv stemMap tyCtx nextDepth it iot =
    let ty =
            if isitInout_ it
            then SignalAbstract (getIdString idInout_) [getInout_Size it]
            else signalIType it
    in case getInoutWire iot of
           IAps (ICon ioId (ICSel {})) _ [ICon instId (ICStateVar { iVar = sv })] ->
               let ioVn = lookupIfcInoutWire ioId (getVModInfo sv)
               in SignalInout
                      ty
                      (SignalPort ty (getIdBaseString instId ++ "$" ++ getVNameString ioVn))
           other ->
               SignalInout ty (signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth other)

flattenIPairChain ::
    SignalNamingEnv ->
    IProvLookup a ->
    M.Map Id String ->
    M.Map String SignalType ->
    Int ->
    IExpr a ->
    [SignalExpr]
flattenIPairChain env lookupProv stemMap tyCtx nextDepth (IAps (ICon i _) _ [e1, e2])
    | i == idPrimPair =
        signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth e1 :
        flattenIPairChain env lookupProv stemMap tyCtx nextDepth e2
flattenIPairChain _ _ _ _ _ (ICon i _)
    | i == idPrimUnit = []
flattenIPairChain env lookupProv stemMap tyCtx nextDepth other =
    [signalIExprGo env lookupProv stemMap tyCtx Nothing Nothing nextDepth other]

inferSignalPrimType :: PrimOp -> [IType] -> [SignalExpr] -> SignalType -> SignalType
inferSignalPrimType PrimIf _ [_, eThen, eElse] _ =
    unifySignalStringTypes [signalExprType eThen, signalExprType eElse]
inferSignalPrimType PrimCase _ (_ : dflt : rest) _ =
    let armTys = collectCaseArmTypes rest
    in unifySignalStringTypes (signalExprType dflt : armTys)
  where
    collectCaseArmTypes :: [SignalExpr] -> [SignalType]
    collectCaseArmTypes (_ : armVal : more) =
        signalExprType armVal : collectCaseArmTypes more
    collectCaseArmTypes _ = []
inferSignalPrimType PrimConcat _ es _ =
    let w = L.foldl' (\acc e -> acc + bitWidth (signalExprType e)) 0 es
    in SignalBit w
  where
    bitWidth :: SignalType -> Integer
    bitWidth (SignalBit n) = n
    bitWidth _ = 0
inferSignalPrimType PrimMul [_, _, ITNum outW] _ _ = SignalBit outW
inferSignalPrimType PrimQuot (ITNum outW : _) _ _ = SignalBit outW
inferSignalPrimType PrimRem [_, ITNum outW] _ _ = SignalBit outW
inferSignalPrimType p _ _ _
    | isBoolOrCmpPrim p = SignalBit 1
inferSignalPrimType p _ (e1 : _) _
    | isArithSameTypePrim p = signalExprType e1
inferSignalPrimType PrimStringConcat _ es@(_ : _) _ =
    SignalString (sumSignalStrSizes es)
inferSignalPrimType PrimBuildArray _ es@(e0 : _) _ =
    let sz = toInteger (length es)
        elemTy = unifySignalStringTypes (map signalExprType es)
        resolvedTy =
            case elemTy of
                SignalUnknownType -> signalExprType e0
                t -> t
    in SignalArray sz resolvedTy
inferSignalPrimType PrimArrayDynSelect _ (arr : _) fallback =
    case signalExprType arr of
        SignalArray _ elemTy -> elemTy
        _ -> fallback
inferSignalPrimType _ _ _ fallback = fallback

isBoolOrCmpPrim :: PrimOp -> Bool
isBoolOrCmpPrim p =
    case p of
        PrimBAnd -> True
        PrimBOr -> True
        PrimBNot -> True
        PrimEQ -> True
        PrimEQ3 -> True
        PrimULE -> True
        PrimULT -> True
        PrimSLE -> True
        PrimSLT -> True
        _ -> False

isArithSameTypePrim :: PrimOp -> Bool
isArithSameTypePrim p =
    case p of
        PrimAdd -> True
        PrimSub -> True
        PrimAnd -> True
        PrimOr -> True
        PrimXor -> True
        PrimSL -> True
        PrimSRL -> True
        PrimSRA -> True
        PrimInv -> True
        PrimNeg -> True
        _ -> False

unifySignalStringTypes :: [SignalType] -> SignalType
unifySignalStringTypes [] = SignalUnknownType
unifySignalStringTypes (t0 : ts) =
    case t0 of
        SignalString Nothing -> SignalString Nothing
        SignalString (Just _) ->
            if all (== t0) ts then t0 else SignalString Nothing
        _ -> t0

sumSignalStrSizes :: [SignalExpr] -> Maybe Integer
sumSignalStrSizes = L.foldl' step (Just 0)
  where
    step :: Maybe Integer -> SignalExpr -> Maybe Integer
    step (Just acc) e =
        case signalExprType e of
            SignalString (Just n) -> Just (acc + n)
            _ -> Nothing
    step Nothing _ = Nothing

-- ============================================================
-- Whole-module IDef temporary naming (for ITransform)
-- ============================================================

-- Compute canonical SignalExprs for unnamed local defs in topological order so
-- nested temporary references resolve to their expression stems, then assign
-- module-wide collision-resolved names while leaving anchors and user binders
-- untouched.
dedupInts :: [Int] -> [Int]
dedupInts = go IS.empty
  where
    go _ [] = []
    go !seen (x : xs)
        | IS.member x seen = go seen xs
        | otherwise = x : go (IS.insert x seen) xs

nameIModuleDefs :: Flags -> IModule a -> M.Map Id Id
nameIModuleDefs flags imod =
    let env = mkSignalNamingEnvFromIModule flags imod
        ds = imod_local_defs imod
        rawProv = imod_provenance imod
        lookupProv = iModuleProvenanceLookup ds rawProv
        nds = zip [0 :: Int ..] ds
        posMap = M.fromList [ (i, n) | (n, IDef i _ _ _) <- nds ]
        synDeps e = mapMaybe (\v -> M.lookup v posMap) (immediateDefRefs e)
        provDeps selfIdx i e
            | null rawProv = []
            | otherwise =
                filter (/= selfIdx) $
                concatMap
                    (\f ->
                        case f of
                            PFactAccess base _ -> synDeps base
                            PFactConcat parts -> concatMap synDeps parts
                            _ -> [])
                    (lookupProv (Just i) e)
        synGraph = [ (n, dedupInts (synDeps e)) | (n, IDef _ _ e _) <- nds ]
        combGraph =
            [ (n, dedupInts (synDeps e ++ provDeps n i e))
            | (n, IDef i _ e _) <- nds
            ]
        defByIdx = IM.fromList nds
        topoIdxs =
            if null rawProv
            then case tsort synGraph of
                     Right ns -> ns
                     Left sccs ->
                         internalError ("ISignalNaming.nameIModuleDefs: cycle: " ++ ppReadable sccs)
            else case tsort combGraph of
                     Right ns -> ns
                     Left cycSccs ->
                         let cycSet = IS.fromList (concat cycSccs)
                             prunedGraph =
                                 [ if IS.member n cycSet
                                   then (n, dedupInts (synDeps e))
                                   else (n, dedupInts (synDeps e ++ provDeps n i e))
                                 | (n, IDef i _ e _) <- nds
                                 ]
                         in case tsort prunedGraph of
                                Right ns -> ns
                                Left _ ->
                                    case tsort synGraph of
                                        Right ns -> ns
                                        Left sccs ->
                                            internalError
                                                ("ISignalNaming.nameIModuleDefs: cycle: " ++ ppReadable sccs)
        dummyTempId = setBadId (mk_homeless_id "_")
        (_, !exprByIdx) =
            L.foldl'
                (\(!stemAcc, !seAcc) n ->
                    case IM.lookup n defByIdx of
                        Nothing -> (stemAcc, seAcc)
                        Just (IDef i defTy e _)
                            | isDefTempId env i ->
                                let se =
                                        signalIExprWithProv
                                            env
                                            lookupProv
                                            stemAcc
                                            (Just i)
                                            (Just (signalIType defTy))
                                            e
                                    st = signalExprStem env se
                                    !stemAcc' = rnf st `seq` M.insert i st stemAcc
                                    !seAcc' = rnf se `seq` IM.insert n se seAcc
                                in (stemAcc', seAcc')
                            | otherwise ->
                                (stemAcc, seAcc))
                (M.empty, IM.empty)
                topoIdxs
        defItems =
            [ mkDefItem n d
            | (n, d) <- nds
            ]
        mkDefItem n (IDef i defTy e _)
            | isDefAnchorId env i = ItemAnchor (Just i) i
            | isDefTempId env i =
                let se =
                        IM.findWithDefault
                            (signalIExprWithProv
                                env
                                lookupProv
                                M.empty
                                (Just i)
                                (Just (signalIType defTy))
                                e)
                            n
                            exprByIdx
                in ItemTemp (Just i) dummyTempId se
            | otherwise = ItemUserBinder (Just i) i Nothing
    in case assignModuleNames env defItems of
           Left (AnchorCollision nm k1 k2) ->
               internalError
                   ("ISignalNaming.nameIModuleDefs: anchor collision on " ++
                    nm ++ " (" ++ show k1 ++ " vs " ++ show k2 ++ ")")
           Right pairs ->
               L.foldl'
                   (\ !acc (mOrig, finalStr) ->
                       case mOrig of
                           Just origId
                               | not (isDefAnchorId env origId) &&
                                 getIdBaseString origId /= finalStr ->
                                   let !renamed = setIdBaseString origId finalStr
                                       !newId = if isDefTempId env origId then setBadId renamed else renamed
                                   in M.insert origId newId acc
                           _ -> acc)
                   M.empty
                   pairs

-- Immediate def/variable references in left-to-right expression sighting order,
-- without traversing into ICValue/ICDef bodies or sorting by Ord Id.
immediateDefRefs :: IExpr a -> [Id]
immediateDefRefs expr0 = reverse (snd (go S.empty [] expr0))
  where
    note !seen !acc i
        | S.member i seen = (seen, acc)
        | otherwise = (S.insert i seen, i : acc)
    go !seen !acc (ILam _ _ e) = go seen acc e
    go !seen !acc (ILAM _ _ e) = go seen acc e
    go !seen !acc (IVar i) = note seen acc i
    go !seen !acc (IAps f _ es) =
        let (!seen1, !acc1) = go seen acc f
        in L.foldl' (\(!s, !a) arg -> go s a arg) (seen1, acc1) es
    go !seen !acc (ICon i (ICDef {})) = note seen acc i
    go !seen !acc (ICon i (ICValue {})) = note seen acc i
    go !seen !acc (ICon _ (ICUndet { imVal = Just e })) = go seen acc e
    go !seen !acc (ICon _ _) = (seen, acc)
    go !seen !acc (IRefT {}) = (seen, acc)
