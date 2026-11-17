{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Resolves in-memory TypeLinks evaluator events (Y, Z, W, K, C) into
-- SourceProvenance facts after elaboration and eqPtrs CSE complete.
--
-- Intermediate struct, tuple, vector, array, and tagged-union values are
-- eliminated during IExpand and leave no surviving cells of their own in
-- iheap.  Walking the recorded coerce/copy/projection/array graph propagates
-- hardware Bit-carrier origins across arbitrary nesting depth (including
-- hand-written Bits instances and pushed sel*/out*/is* operations) onto the
-- surviving canonical cells and inline expressions.
module TypeLinksProvenance(
        RawTarget(..),
        harvestTypeLinksProvenance,
        rawExprToIExpr,
        resolveRawTarget,
        resolveProvFact
    ) where

import Control.Exception(SomeException, evaluate, catch)
import Control.Monad(foldM, forM_, when)
import qualified Data.Array as Array
import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe(fromMaybe)
import qualified Data.Set as S
import System.Mem.StableName(makeStableName, hashStableName)

import CType(TISort(..), StructSubType(..))
import Id(Id, getIdBaseString, getIdQualString)
import IntLit(IntLit(..))
import ISyntax
import ISyntaxSubst(tSubst)
import ISyntaxUtil(iGetType, itBit1, itString)
import Position(noPosition)
import PreIds(idAVValue_, idActionValue_, idArrow, idBit, idBool, idClock,
              idClockGate, idClockOsc, idInout_, idPrimFst, idPrimPair,
              idPrimSnd, idPrimUnit, idReal, idReset, idString)
import Prim(PrimOp(..))
import SourceProvenance
import TypeLinks(TLog, tlOn, RawExpr(..), RawArray(..), SemRec(..),
                 AnySN(..), lookupSN, tlReadSem, tlReadSemLits)

-- Pre-resolution carrier or target expression: either a post-eqPtrs canonical
-- heap cell pointer or an inline expression assigned a sequential StableName id.
data RawTarget
  = RTCell !Int
  | RTExpr !Int RawExpr

rawTargetKey :: RawTarget -> Int
rawTargetKey (RTCell c) = c
rawTargetKey (RTExpr eid _) = -(eid + 1)

-- Convert a raw evaluator expression (with arbitrary heap-ref payload type)
-- into a synthesizable hardware IExpr b using a lookup table on heap pointers.
-- Peels evaluator-only representation wrappers (PrimChr, PrimRange) and returns
-- Nothing if any surviving IRefT is absent from the map, a held coercion
-- escaped, or the resulting expression contains evaluator-only constructs that
-- do not survive into hardware IR (ITransform / AConv).
rawExprToIExpr :: (Int -> Maybe (IExpr b)) -> RawExpr -> Maybe (IExpr b)
rawExprToIExpr lookupPtr (RawExpr e0) = do
    e' <- goExpr e0
    let e'' = peelHardwareWrappers e'
    if isHardwareIExpr e'' then Just e'' else Nothing
  where
    goExpr (IAps (ICon _ (ICPrim _ PrimChr)) _ [x]) = goExpr x
    goExpr (IAps (ICon _ (ICPrim _ PrimRange)) _ [_, _, x]) = goExpr x
    goExpr (ILam i t e) = ILam i t <$> goExpr e
    goExpr (IAps f ts es) = IAps <$> goExpr f <*> pure ts <*> mapM goExpr es
    goExpr (IVar i) = Just (IVar i)
    goExpr (ILAM i k e) = ILAM i k <$> goExpr e
    goExpr (IRefT _ p _ _) = lookupPtr p
    goExpr (ICon _ (ICLazyPack {})) = Nothing
    goExpr (ICon _ (ICLazyUnpack {})) = Nothing
    goExpr (ICon i ic) = ICon i <$> goCon ic

    goCon (ICDef t d) = ICDef t <$> goExpr d
    goCon (ICPrim t op) = Just (ICPrim t op)
    goCon (ICForeign t nm isC ports callNo tvns) = Just (ICForeign t nm isC ports callNo tvns)
    goCon (ICCon t cti) = Just (ICCon t cti)
    goCon (ICIs t cti) = Just (ICIs t cti)
    goCon (ICOut t cti) = Just (ICOut t cti)
    goCon (ICSel t sel n) = Just (ICSel t sel n)
    goCon (ICTuple t fids) = Just (ICTuple t fids)
    goCon (ICVerilog t u vi mc) = Just (ICVerilog t u vi mc)
    goCon (ICUndet t u mv) = ICUndet t u <$> traverse goExpr mv
    goCon (ICInt t v) = Just (ICInt t v)
    goCon (ICReal t v) = Just (ICReal t v)
    goCon (ICString t s) = Just (ICString t s)
    goCon (ICChar t c) = Just (ICChar t c)
    goCon (ICHandle t h) = Just (ICHandle t h)
    goCon (ICRuleAssert t as) = Just (ICRuleAssert t as)
    goCon (ICSchedPragmas t sps) = Just (ICSchedPragmas t sps)
    goCon (ICName t nm) = Just (ICName t nm)
    goCon (ICAttrib t atts) = Just (ICAttrib t atts)
    goCon (ICPosition t pos) = Just (ICPosition t pos)
    goCon (ICType t it) = Just (ICType t it)
    goCon (ICPred t pr) = ICPred t <$> goPred pr
    goCon (ICValue t d) = ICValue t <$> goExpr d
    goCon (ICMethArg t) = Just (ICMethArg t)
    goCon (ICModPort t) = Just (ICModPort t)
    goCon (ICModParam t) = Just (ICModParam t)
    goCon (ICStateVar t sv) = ICStateVar t <$> goSV sv
    goCon (ICMethod t ins outs m) = ICMethod t ins outs <$> goExpr m
    goCon (ICClock t c) = Just (ICClock t (goClockLazy c))
    goCon (ICReset t r) = Just (ICReset t (goResetLazy r))
    goCon (ICInout t io) = Just (ICInout t (goInoutLazy io))
    goCon (ICLazyArray {}) = Nothing
    goCon (ICLazyPack {}) = Nothing
    goCon (ICLazyUnpack {}) = Nothing

    -- Clock, reset, and inout wires can be cyclic (submodule output clocks),
    -- so rebuild their wire expressions lazily without strict Maybe bind.
    goExprLazy e = fromMaybe (IVar idPrimUnit) (goExpr e)
    goClockLazy (IClock cid dom w) = IClock cid dom (goExprLazy w)
    goResetLazy (IReset rid clk w) = IReset rid (goClockLazy clk) (goExprLazy w)
    goInoutLazy (IInout clk rst w) = IInout (goClockLazy clk) (goResetLazy rst) (goExprLazy w)

    goSV sv = do
        es' <- mapM goExpr (isv_iargs sv)
        let clks' = [ (i, goClockLazy c) | (i, c) <- isv_clocks sv ]
            rsts' = [ (i, goResetLazy r) | (i, r) <- isv_resets sv ]
        return sv { isv_iargs = es', isv_clocks = clks', isv_resets = rsts' }
    goPred (PConj ps) = PConj . psFromList <$> mapM goPTerm (psToList ps)
    goPTerm (PAtom e) = PAtom <$> goExpr e
    goPTerm (PIf c t e) = PIf <$> goExpr c <*> goPred t <*> goPred e
    goPTerm (PSel idx sz ps) = PSel <$> goExpr idx <*> pure sz <*> mapM goPred ps

peelHardwareWrappers :: IExpr b -> IExpr b
peelHardwareWrappers (IAps (ICon _ (ICPrim _ PrimChr)) _ [x]) =
    peelHardwareWrappers x
peelHardwareWrappers (IAps (ICon _ (ICPrim _ PrimRange)) _ [_, _, x]) =
    peelHardwareWrappers x
peelHardwareWrappers e = e

isHardwareValueType :: IType -> Bool
isHardwareValueType (ITAp (ITCon b _ _) (ITNum n)) | b == idBit = n >= 0
isHardwareValueType (ITAp (ITCon i _ _) (ITNum n)) | i == idInout_ = n >= 0
isHardwareValueType (ITAp (ITAp (ITCon p _ _) t1) t2) | p == idPrimPair =
    isHardwareValueType t1 && isHardwareValueType t2
isHardwareValueType (ITAp (ITCon i _ _) t) | i == idActionValue_ =
    isHardwareValueType t
isHardwareValueType (ITCon i _ _) =
    i == idPrimUnit || i == idReal || i == idString || i == idClock || i == idReset
isHardwareValueType _ = False

isHardwareBitType :: IType -> Bool
isHardwareBitType (ITAp (ITCon b _ _) (ITNum n)) | b == idBit = n >= 0
isHardwareBitType _ = False

isHardwareIExpr :: IExpr b -> Bool
isHardwareIExpr (ICon _ (ICValue {})) = True
isHardwareIExpr (ICon _ (ICModPort { iConType = t })) = isHardwareValueType t
isHardwareIExpr (ICon _ (ICModParam { iConType = t })) = isHardwareValueType t
isHardwareIExpr (ICon _ (ICMethArg { iConType = t })) = isHardwareValueType t
isHardwareIExpr (ICon _ (ICInt { iConType = t })) = isHardwareValueType t
isHardwareIExpr (ICon _ (ICReal {})) = True
isHardwareIExpr (ICon _ (ICString {})) = True
isHardwareIExpr (ICon _ (ICUndet { iConType = t, imVal = mv })) =
    t /= itString && isHardwareValueType t && maybe True isHardwareIExpr mv
isHardwareIExpr (ICon _ (ICForeign { iConType = t, foports = Nothing })) =
    isHardwareValueType t
isHardwareIExpr (ICon _ (ICForeign { iConType = t, isC = False, foports = Just _ })) =
    isHardwareValueType t
isHardwareIExpr (ICon _ (ICClock {})) = True
isHardwareIExpr (ICon _ (ICReset {})) = True
isHardwareIExpr (ICon _ (ICInout { iConType = t })) = isHardwareValueType t
isHardwareIExpr (ICon i _) | i == idPrimUnit = True
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimSelect)) [ITNum i1, ITNum i2, ITNum i3] [e]) =
    i1 >= 0 && i2 >= 0 && i2 < i3 && i3 - i2 >= i1 && isHardwareIExpr e
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimExtract)) [ITNum i1, _, ITNum i2] [e, h, l]) =
    i1 > 0 && i2 >= 0 && isHardwareIExpr e && isHardwareIExpr h && isHardwareIExpr l
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimRange)) _ [_, _, e]) =
    isHardwareIExpr e
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimIntegerToBit)) _
                      [IAps (ICon _ (ICPrim _ PrimStringToInteger)) _ [s]]) =
    isHardwareIExpr s
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimSignExt)) [_, _, ITNum n] [e]) =
    n >= 0 && isHardwareIExpr e
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimZeroExt)) [_, _, ITNum n] [e]) =
    n >= 0 && isHardwareIExpr e
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimTrunc)) [_, _, ITNum n] [e]) =
    n >= 0 && isHardwareIExpr e
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimConcat)) _ es@(_ : _)) =
    all (\a -> isHardwareIExpr a && isHardwareBitType (iGetType a)) es
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimIf)) [t] [c, thn, els]) =
    isHardwareValueType t && isHardwareIExpr c && isHardwareIExpr thn && isHardwareIExpr els
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimCase)) [ITNum _, elemTy] (idx : dflt : ces)) =
    not (null ces) && even (length ces) && isHardwareValueType elemTy &&
    all isHardwareIExpr (idx : dflt : ces)
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimBuildArray)) [elemTy] es@(_ : _)) =
    isHardwareValueType elemTy && all isHardwareIExpr es
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimArrayDynSelect)) [elemTy, ITNum _] [arr, idx]) =
    isHardwareValueType elemTy && isHardwareArrayExpr arr && isHardwareIExpr idx
  where
    isHardwareArrayExpr (ICon _ (ICValue {})) = True
    isHardwareArrayExpr a@(IAps (ICon _ (ICPrim _ PrimBuildArray)) _ _) = isHardwareIExpr a
    isHardwareArrayExpr _ = False
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimMul)) [_, _, ITNum n] [a, b]) =
    n >= 0 && isHardwareIExpr a && isHardwareIExpr b
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimQuot)) [ITNum n, _] [a, b]) =
    n >= 0 && isHardwareIExpr a && isHardwareIExpr b
isHardwareIExpr (IAps (ICon _ (ICPrim _ PrimRem)) [_, ITNum n] [a, b]) =
    n >= 0 && isHardwareIExpr a && isHardwareIExpr b
isHardwareIExpr (IAps (ICon _ (ICPrim _ p)) _ es@(_ : _))
  | isSimpleAConvPrim p =
      all isHardwareIExpr es
  where
    isSimpleAConvPrim op =
        case op of
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
            PrimBAnd -> True
            PrimBOr -> True
            PrimBNot -> True
            PrimEQ -> True
            PrimEQ3 -> True
            PrimULE -> True
            PrimULT -> True
            PrimSLE -> True
            PrimSLT -> True
            PrimStringConcat -> True
            _ -> False
isHardwareIExpr e@(IAps (ICon _ (ICSel {})) _ _) =
    isHardwareValueType (iGetType e) && okSel (unfoldSel e)
  where
    isTupSel s = s == idPrimFst || s == idPrimSnd
    isUnitArg (ICon i _) = i == idPrimUnit
    isUnitArg _ = False
    okArg a = isUnitArg a || isHardwareIExpr a
    unfoldSel :: IExpr b -> ([Id], [IExpr b])
    unfoldSel (IAps (ICon i (ICSel {})) _ [e']) =
        let (ss, base) = unfoldSel e'
        in (i : ss, base)
    unfoldSel (IAps (ICon i (ICSel {})) _ base@(_ : _)) =
        ([i], base)
    unfoldSel base = ([], [base])
    isNoInlineForeign (ICon _ (ICForeign { isC = False, foports = Just _ })) = True
    isNoInlineForeign (IAps (ICon _ (ICForeign { isC = False, foports = Just _ })) _ _) = True
    isNoInlineForeign _ = False
    okSel ([m], [ICon _ (ICForeign { foports = Nothing, fcallNo = Just _ })]) =
        m == idAVValue_
    okSel ([m], [IAps (ICon _ (ICForeign { foports = Nothing, fcallNo = Just _ })) _ fes]) =
        m == idAVValue_ && all isHardwareIExpr fes
    okSel ([m], [ICon _ (ICClock {})]) =
        m == idClockGate || m == idClockOsc
    okSel (sels@(_ : _), [fcall])
      | all isTupSel sels && isNoInlineForeign fcall =
          isHardwareIExpr fcall
    okSel (sels, ICon _ (ICStateVar {}) : es) =
        case span isTupSel sels of
            (_ : _, [_]) -> all okArg es
            (_, [iav, _]) | iav == idAVValue_ -> null es
            ([], [_]) -> all okArg es
            _ -> False
    okSel _ = False
isHardwareIExpr (IAps (ICon _ (ICCon { iConType = ITAp _ t })) _ _)
  | t == itBit1 = True
isHardwareIExpr e@(IAps (ICon _ (ICForeign { foports = Nothing })) _ es) =
    isHardwareValueType (iGetType e) && all isHardwareIExpr es
isHardwareIExpr e@(IAps (ICon _ (ICForeign { isC = False, foports = Just _,
                                             fTyVarNames = tvns })) ts es) =
    length tvns == length [ n | ITNum n <- ts ] &&
    isHardwareValueType (iGetType e) &&
    all isHardwareIExpr es
isHardwareIExpr e@(IAps (ICon i _) _ [e1, e2])
  | i == idPrimPair =
      isHardwareValueType (iGetType e) && okTupleElem e1 && okTupleElem e2
  where
    okTupleElem (IAps (ICon p _) _ [a, b]) | p == idPrimPair =
        okTupleElem a && okTupleElem b
    okTupleElem (ICon u _) | u == idPrimUnit = True
    okTupleElem x = isHardwareIExpr x
isHardwareIExpr _ = False

resolveRawTarget :: Maybe Int -> (Int -> Maybe (IExpr b)) -> RawTarget -> Maybe (IExpr b)
resolveRawTarget mSelf lookupPtr (RTCell c)
  | Just self <- mSelf, c == self = Nothing
  | otherwise = do
      e' <- lookupPtr c
      let e'' = peelHardwareWrappers e'
      if isHardwareIExpr e'' then Just e'' else Nothing
resolveRawTarget mSelf lookupPtr (RTExpr _ re) =
    let lookupGuarded p
          | Just self <- mSelf, p == self = Nothing
          | otherwise = lookupPtr p
    in rawExprToIExpr lookupGuarded re

resolveProvFact :: Maybe Int -> (Int -> Maybe (IExpr b)) -> ProvFact RawTarget -> Maybe (ProvFact (IExpr b))
resolveProvFact mSelf lookupPtr = mapMProvFact (resolveRawTarget mSelf lookupPtr)

-- Internal origin node during graph propagation:
--   NodeCell p   -> raw evaluator heap pointer p (0 .. hp-1)
--   NodeArr id   -> inline ICLazyArray assigned sequential id via StableName
--   NodeExpr id  -> inline IAps expression assigned sequential id via StableName
data NodeKey
  = NodeCell !Int
  | NodeArr !Int
  | NodeExpr !Int
  deriving (Eq, Ord)

data Origin = Origin !RawTarget [ProvStep]

originEqKey :: Origin -> (Int, [ProvStep])
originEqKey (Origin rt steps) = (rawTargetKey rt, steps)

dedupOrigins :: [Origin] -> [Origin]
dedupOrigins = go S.empty
  where
    go _ [] = []
    go seen (o : os) =
        let k = originEqKey o
        in if S.member k seen
             then go seen os
             else o : go (S.insert k seen) os

tryMaybe :: IO (Maybe a) -> IO (Maybe a)
tryMaybe act = act `catch` \(_ :: SomeException) -> return Nothing

headOf :: IType -> IType
headOf (ITAp f _) = headOf f
headOf t = t

isArrow :: IType -> Bool
isArrow (ITCon i _ _) = i == idArrow noPosition
isArrow _ = False

arrowParts :: IType -> Maybe (IType, IType)
arrowParts (ITAp (ITAp arr a) r) | isArrow arr = Just (a, r)
arrowParts _ = Nothing

instLead :: IType -> [IType] -> IType
instLead (ITForAll i _ t) (a : as) = instLead (tSubst i a t) as
instLead t _ = t

bitWidth :: IType -> Int
bitWidth (ITAp (ITCon i _ _) (ITNum n)) | i == idBit = fromInteger n
bitWidth _ = -1

iTypeKey :: IType -> [String]
iTypeKey = flip go []
  where
    go (ITCon i _ _) acc = getIdQualString i : getIdBaseString i : acc
    go (ITNum n) acc = show n : acc
    go (ITStr s) acc = show s : acc
    go (ITAp f a) acc = "(" : go f (go a (")" : acc))
    go _ acc = "?" : acc

flatAp :: IExpr a -> (IExpr a, [IType], [IExpr a])
flatAp (IAps f ts es) =
    let (h, ts0, es0) = flatAp f
    in (h, ts0 ++ ts, es0 ++ es)
flatAp e = (e, [], [])

-- Extract raw cell pointer if e is an IRefT, a held coercion, or a PrimChr
-- enum/Bool representation wrapper around one.
rawCellOf :: RawExpr -> IO (Maybe Int)
rawCellOf (RawExpr e0) = tryMaybe (ptrOf e0)
  where
    ptrOf ae0 = do
        ae <- evaluate ae0
        case ae of
          IRefT _ p _ _ | p >= 0 -> return (Just p)
          ICon _ ic0 -> do
              ic <- evaluate ic0
              case ic of
                ICLazyPack { lzApplied = inner } -> ptrOf inner
                ICLazyUnpack { lzApplied = inner } -> ptrOf inner
                _ -> return Nothing
          IAps _ _ _ ->
              case flatAp ae of
                (ICon _ ic0, _, [inner]) -> do
                    ic <- evaluate ic0
                    case ic of
                      ICPrim { primOp = PrimChr } -> ptrOf inner
                      _ -> return Nothing
                _ -> return Nothing
          _ -> return Nothing

isLeafOrAppBitExpr :: RawExpr -> IO Bool
isLeafOrAppBitExpr (RawExpr e0) =
    (do e <- evaluate e0
        case e of
          IRefT _ p _ _ -> return (p >= 0)
          IAps _ _ _ -> return True
          ICon _ ic0 -> do
              ic <- evaluate ic0
              case ic of
                ICModPort {} -> return True
                ICMethArg {} -> return True
                ICModParam {} -> return True
                ICStateVar {} -> return True
                ICValue {} -> return True
                _ -> return False
          _ -> return False)
    `catch` \(_ :: SomeException) -> return False

isConstOrUndet :: RawExpr -> IO Bool
isConstOrUndet (RawExpr e0) =
    (do e <- evaluate e0
        case e of
          ICon _ ic0 -> do
              ic <- evaluate ic0
              case ic of
                ICInt {} -> return True
                ICReal {} -> return True
                ICString {} -> return True
                ICChar {} -> return True
                ICUndet {} -> return True
                ICTuple { fieldIds = [] } -> return True
                ICPrim { primOp = PrimNoActions } -> return True
                _ -> return False
          _ -> return False)
    `catch` \(_ :: SomeException) -> return True

isUndetExpr :: RawExpr -> IO Bool
isUndetExpr (RawExpr e0) =
    (do e <- evaluate e0
        case e of
          ICon _ ic0 -> do
              ic <- evaluate ic0
              case ic of
                ICUndet {} -> return True
                _ -> return False
          _ -> return False)
    `catch` \(_ :: SomeException) -> return False

litWidthVal :: RawExpr -> IO (Maybe (Int, Integer))
litWidthVal (RawExpr e0) = tryMaybe $ do
    e <- evaluate e0
    case e of
      ICon _ (ICInt { iConType = t0, iVal = iv0 }) -> do
          t <- evaluate t0
          let w = bitWidth t
          iv <- evaluate iv0
          v <- evaluate (ilValue iv)
          if w >= 0 then return (Just (w, v)) else return Nothing
      _ -> return Nothing

-- Harvest structured provenance from TLog after runG + eqPtrs.
harvestTypeLinksProvenance
    :: TLog
    -> (IType -> IType)
    -> (IType -> Maybe Bool)
    -> (Int -> Int)
    -> (Int -> Bool)
    -> [(Int, RawExpr)]
    -> [RawExpr]
    -> IO (IM.IntMap [ProvFact RawTarget], [(RawTarget, [ProvFact RawTarget])])
harvestTypeLinksProvenance l norm hasBits ptran hasUserBinder canonCells rootExprs
  | not (tlOn l) = return (IM.empty, [])
  | otherwise = (do
      (recs, _) <- tlReadSem l
      semLits <- tlReadSemLits l

      -- Mutable graph tables during harvesting
      let canonSet = IM.fromList [ (c, ()) | (c, _) <- canonCells ]
          canonCellMap = IM.fromList canonCells
      typeWidthsRef <- newIORef (M.empty :: M.Map [String] Integer)
      arrTabRef <- newIORef (IM.empty :: IM.IntMap [(AnySN, NodeKey)])
      nextArrRef <- newIORef (0 :: Int)
      exprTabRef <- newIORef (IM.empty :: IM.IntMap [(AnySN, Int)])
      nextExprRef <- newIORef (0 :: Int)
      unpackSeedsRef <- newIORef (IM.empty :: IM.IntMap [Origin])
      exprSeedsRef <- newIORef (IM.empty :: IM.IntMap [Origin])
      inlineExprMapRef <- newIORef (IM.empty :: IM.IntMap RawTarget)
      copyAdjRef <- newIORef (IM.empty :: IM.IntMap [Int])
      stepAdjRef <- newIORef ([] :: [(NodeKey, NodeKey, [ProvStep])])
      inlineFactsRef <- newIORef ([] :: [(RawTarget, ProvFact RawTarget)])
      litMapRef <- newIORef (IM.empty :: IM.IntMap NamedLitInfo)

      let mkRTExpr :: RawExpr -> IO RawTarget
          mkRTExpr (RawExpr e0) = do
              e <- evaluate e0
              sn <- makeStableName e
              m <- readIORef exprTabRef
              let h = hashStableName sn
                  bucket = IM.findWithDefault [] h m
              case lookupSN sn bucket of
                Just eid -> return (RTExpr eid (RawExpr e))
                Nothing -> do
                    eid <- readIORef nextExprRef
                    writeIORef nextExprRef $! eid + 1
                    writeIORef exprTabRef $! IM.insert h ((AnySN sn, eid) : bucket) m
                    let rt = RTExpr eid (RawExpr e)
                    modifyIORef' inlineExprMapRef (IM.insert eid rt)
                    return rt

          lookupRTExprId :: RawExpr -> IO (Maybe Int)
          lookupRTExprId (RawExpr e0) = tryMaybe $ do
              m <- readIORef exprTabRef
              if IM.null m
                then return Nothing
                else do
                    e <- evaluate e0
                    sn <- makeStableName e
                    let bucket = IM.findWithDefault [] (hashStableName sn) m
                    return (lookupSN sn bucket)

          getArrNode :: RawArray -> Maybe Int -> IO NodeKey
          getArrNode (RawArray arr0) mCell = do
              arr <- evaluate arr0
              sn <- makeStableName arr
              m <- readIORef arrTabRef
              let h = hashStableName sn
                  bucket = IM.findWithDefault [] h m
              case lookupSN sn bucket of
                Just nk -> do
                    case (nk, mCell) of
                      (NodeCell oldP, Just newP) | oldP /= newP ->
                          modifyIORef' copyAdjRef $
                              IM.insertWith (++) newP [oldP] .
                              IM.insertWith (++) oldP [newP]
                      (NodeArr _, Just newP) | newP >= 0 ->
                          modifyIORef' stepAdjRef $
                              ((NodeCell newP, nk, []) :) .
                              ((nk, NodeCell newP, []) :)
                      _ -> return ()
                    return nk
                Nothing -> do
                    nk <- case mCell of
                            Just p | p >= 0 -> return (NodeCell p)
                            _ -> do
                                k <- readIORef nextArrRef
                                writeIORef nextArrRef $! k + 1
                                return (NodeArr k)
                    writeIORef arrTabRef $! IM.insert h ((AnySN sn, nk) : bucket) m
                    let slots = [ ac_ptr c | c <- Array.elems arr ]
                        nSlots = length slots
                    forM_ (zip [0 :: Int ..] slots) $ \(idx, sp) ->
                        when (sp >= 0) $
                            modifyIORef' stepAdjRef ((nk, NodeCell sp, [PStepElem idx nSlots]) :)
                    return nk

          nameableType :: IType -> Bool
          nameableType t =
              let nt = norm t
              in case headOf nt of
                   ITCon i _ _ | i == idBool -> False
                   _ -> hasBits nt == Just True

          dataConName :: IType -> Int -> Maybe (String, String, String, Int, Bool)
          dataConName st0 k =
              let st = norm st0
              in case headOf st of
                   ITCon ti _ (TIdata { tidata_cons = cs, tidata_enum = isEn })
                     | ti /= idBool, k >= 0, k < length cs ->
                         let ci = cs !! k
                         in Just (getIdBaseString ti, getIdQualString ti,
                                  getIdBaseString ci, length cs, isEn)
                   _ -> Nothing

          structFieldStep :: IType -> Int -> Int -> Maybe ProvStep
          structFieldStep st0 k m =
              let st = norm st0
              in case headOf st of
                   ITCon ti _ _
                     | ti == idPrimPair ->
                         let fn = "tpl_" ++ show (k + 1)
                         in Just (PStepField "" "" fn k (max m 2) True)
                   ITCon ti _ (TIstruct ss fs)
                     | k >= 0, k < length fs ->
                         let fi = fs !! k
                             n = length fs
                         in case ss of
                              SInterface _ ->
                                  Just (PStepField "" "" (getIdBaseString fi) k n True)
                              SStruct ->
                                  Just (PStepField (getIdBaseString ti) (getIdQualString ti)
                                                   (getIdBaseString fi) k n False)
                              SDataCon {} ->
                                  Just (PStepField (getIdBaseString ti) (getIdQualString ti)
                                                   (getIdBaseString fi) k n False)
                              _ -> Nothing
                   _ -> Nothing

          bitsCarrierTarget :: Int -> Maybe RawExpr -> IO (Maybe RawTarget)
          bitsCarrierTarget bp mbe
            | bp >= 0, IM.member (ptran bp) canonSet =
                return (Just (RTCell (ptran bp)))
            | Just be <- mbe = do
                mp <- rawCellOf be
                case mp of
                  Just p | IM.member (ptran p) canonSet ->
                      return (Just (RTCell (ptran p)))
                  _ -> do
                      ok <- isLeafOrAppBitExpr be
                      if ok then Just <$> mkRTExpr be else return Nothing
            | otherwise = return Nothing

          exprNodeOrCarrier :: Maybe RawExpr -> RawExpr -> IO (Either (Maybe RawTarget) NodeKey)
          exprNodeOrCarrier mown v = do
              mOwnCell <- maybe (return Nothing) rawCellOf mown
              case mOwnCell of
                Just p -> return (Right (NodeCell p))
                Nothing -> do
                    mValCell <- rawCellOf v
                    case mValCell of
                      Just p -> return (Right (NodeCell p))
                      Nothing -> do
                          -- Check if v is ICLazyArray or PrimChr <bits>
                          mArrOrChr <- tryMaybe $ do
                              RawExpr ve0 <- return v
                              ve <- evaluate ve0
                              case ve of
                                ICon _ (ICLazyArray { iArray = arr }) ->
                                    Just . Right <$> getArrNode (RawArray arr) Nothing
                                _ -> case flatAp ve of
                                       (ICon _ (ICPrim { primOp = PrimChr }), _, [x]) -> do
                                           mt <- bitsCarrierTarget (-1) (Just (RawExpr x))
                                           return (Just (Left mt))
                                       _ -> return Nothing
                          case mArrOrChr of
                            Just res -> return res
                            Nothing ->
                                case mown of
                                  Just ownE -> Left <$> bitsCarrierTarget (-1) (Just ownE)
                                  Nothing -> return (Left Nothing)

      -- Pass 1: ingest all SemRecs into graph edges and literal table
      forM_ recs $ \rec -> case rec of
          SemCopy dst src ->
              when (dst >= 0 && src >= 0 && dst /= src) $
                  modifyIORef' copyAdjRef $
                      IM.insertWith (++) dst [src] .
                      IM.insertWith (++) src [dst]

          SemCoerce dir _ ta tn bp mbe vp _ -> do
              case norm tn of
                ITNum w | w >= 0 ->
                    modifyIORef' typeWidthsRef $
                        M.insert (iTypeKey (norm ta)) w
                _ -> return ()
              when (dir == 'u' && vp >= 0) $ do
                  mc <- bitsCarrierTarget bp mbe
                  case mc of
                    Just ct ->
                        modifyIORef' unpackSeedsRef $
                            IM.insertWith (flip (++)) vp [Origin ct []]
                    Nothing -> return ()

          SemArrHeld p _ rawArr -> do
              _ <- getArrNode rawArr (Just p)
              return ()

          SemElem (RawExpr av0) idx p | p >= 0 -> do
              _ <- tryMaybe $ do
                  av <- evaluate av0
                  case av of
                    ICon _ (ICLazyArray { iArray = arr }) -> do
                        nk <- getArrNode (RawArray arr) Nothing
                        let nSlots = Array.rangeSize (Array.bounds arr)
                        modifyIORef' stepAdjRef
                            ((nk, NodeCell p, [PStepElem (fromInteger idx) nSlots]) :)
                        return (Just ())
                    _ -> return Nothing
              return ()

          SemOp _ (RawExpr hd0) tys mown v mpart res -> do
              isConstRes <- isConstOrUndet res
              when (not isConstRes) $ do
                  _ <- tryMaybe $ do
                      hd <- evaluate hd0
                      case hd of
                        ICon _ ic0 -> do
                            ic <- evaluate ic0
                            let mArrow = arrowParts (instLead (iConType ic) tys)
                            case (ic, mArrow) of
                              (ICSel { selNo = k0, numSel = m0 }, Just (st, _)) -> do
                                  k <- fromInteger <$> evaluate k0
                                  m <- fromInteger <$> evaluate m0
                                  case structFieldStep st k m of
                                    Just step -> recordStep mown v mpart res [step]
                                    Nothing -> return ()
                              (ICOut { conTagInfo = cti }, Just (st, _)) -> do
                                  k <- fromInteger <$> evaluate (conNo cti)
                                  m <- evaluate (numCon cti)
                                  if m <= 1
                                    then recordStep mown v mpart res []
                                    else case dataConName st k of
                                           Just (tb, tq, cn, nc, _) -> do
                                               let step = PStepVariantPayload tb tq cn k nc
                                               recordStep mown v mpart res [step]
                                               recordExtractedPayloadLeaf mown v res step
                                           Nothing -> return ()
                              (ICIs { conTagInfo = cti }, Just (st, _)) -> do
                                  k <- fromInteger <$> evaluate (conNo cti)
                                  m <- evaluate (numCon cti)
                                  when (m > 1) $
                                      case dataConName st k of
                                        Just (tb, tq, cn, nc, isEn) -> do
                                            recordStep mown v mpart res
                                                [PStepVariantTest tb tq cn k nc isEn]
                                            when (not isEn) $
                                                recordExtractedTagStep st tb tq nc mown v res
                                        Nothing -> return ()
                              _ -> return ()
                            return (Just ())
                        _ -> return Nothing
                  return ()
            where
              isIntLitRaw (RawExpr e0) =
                  (do e <- evaluate e0
                      case e of
                        ICon _ (ICInt {}) -> return True
                        IRefT _ p _ _
                          | Just re <- IM.lookup (ptran p) canonCellMap ->
                              isIntLitRaw re
                        _ -> return False)
                  `catch` \(_ :: SomeException) -> return False

              isSlicedRaw (RawExpr e0) =
                  (do e <- evaluate e0
                      case e of
                        IRefT _ p _ _
                          | Just re <- IM.lookup (ptran p) canonCellMap ->
                              isSlicedRaw re
                        _ -> case flatAp e of
                               (ICon _ (ICPrim { primOp = op }), _, _) ->
                                   return (op == PrimSelect || op == PrimExtract || op == PrimTrunc)
                               _ -> return False)
                  `catch` \(_ :: SomeException) -> return False

              derefRaw :: Int -> RawExpr -> IO RawExpr
              derefRaw depth re0@(RawExpr e0)
                | depth <= 0 = return re0
                | otherwise = do
                    e <- evaluate e0
                    case e of
                      IRefT _ p _ _
                        | Just re <- IM.lookup (ptran p) canonCellMap ->
                            derefRaw (depth - 1) re
                      _ -> return (RawExpr e)

              rawTargetIdKey :: RawExpr -> IO (Either Int Int)
              rawTargetIdKey re = do
                  mp <- rawCellOf re
                  case mp of
                    Just p -> return (Left (ptran p))
                    Nothing -> do
                        RTExpr eid _ <- mkRTExpr re
                        return (Right eid)

              uniqueRawByKey :: [RawExpr] -> IO (Maybe RawExpr)
              uniqueRawByKey [] = return Nothing
              uniqueRawByKey rs = do
                  keyed <- mapM (\r -> (\k -> (k, r)) <$> rawTargetIdKey r) rs
                  let uniqKeys = S.fromList (map fst keyed)
                  case (S.size uniqKeys, keyed) of
                    (1, (_, r0) : _) -> return (Just r0)
                    _ -> return Nothing

              collectTagOps :: Int -> Maybe Integer -> RawExpr -> IO [RawExpr]
              collectTagOps depth muW re0
                | depth <= 0 = return []
                | otherwise = do
                    RawExpr d0 <- derefRaw 4 re0
                    e <- evaluate d0
                    case flatAp e of
                      (ICon _ (ICPrim { primOp = PrimEQ }), [ITNum tagW], [a1, a2])
                        | tagW > 0 -> do
                            c1 <- isIntLitRaw (RawExpr a1)
                            c2 <- isIntLitRaw (RawExpr a2)
                            let mTagOp =
                                    case (c1, c2) of
                                      (False, True) -> Just (RawExpr a1)
                                      (True, False) -> Just (RawExpr a2)
                                      _ -> Nothing
                            case mTagOp of
                              Nothing -> return []
                              Just tagOp -> do
                                  okNarrow <-
                                      case muW of
                                        Just uW -> return (tagW < uW)
                                        Nothing -> isSlicedRaw tagOp
                                  return [ tagOp | okNarrow ]
                      (ICon _ (ICPrim { primOp = PrimBNot }), _, [a1]) ->
                          collectTagOps (depth - 1) muW (RawExpr a1)
                      (ICon _ (ICPrim { primOp = op }), _, [a1, a2])
                        | op == PrimBAnd || op == PrimBOr -> do
                            l1 <- collectTagOps (depth - 1) muW (RawExpr a1)
                            l2 <- collectTagOps (depth - 1) muW (RawExpr a2)
                            return (l1 ++ l2)
                      (ICon _ (ICPrim { primOp = PrimIf }), _, [c, tBr, fBr]) -> do
                          lc <- collectTagOps (depth - 1) muW (RawExpr c)
                          lt <- collectTagOps (depth - 1) muW (RawExpr tBr)
                          lf <- collectTagOps (depth - 1) muW (RawExpr fBr)
                          return (lc ++ lt ++ lf)
                      _ -> return []

              recordExtractedTagStep st tb tq nc mown' v' resRaw = do
                  _ <- tryMaybe $ do
                      tws <- readIORef typeWidthsRef
                      let muW = M.lookup (iTypeKey (norm st)) tws
                      tagOps <- collectTagOps 24 muW resRaw
                      mUnique <- uniqueRawByKey tagOps
                      case mUnique of
                        Just tagOp ->
                            recordStep mown' v' Nothing tagOp [PStepVariantTag tb tq nc]
                        Nothing -> return ()
                      return (Just ())
                  return ()

              collectPayloadLeaves :: Int -> RawExpr -> IO (Bool, [RawExpr])
              collectPayloadLeaves depth raw
                | depth <= 0 = return (False, [raw])
                | otherwise = do
                    RawExpr b0 <- derefRaw 1 raw
                    eBody <- evaluate b0
                    case eBody of
                      ICon _ (ICUndet {}) -> return (True, [])
                      _ ->
                          case flatAp eBody of
                            (ICon _ (ICPrim { primOp = PrimIf }), _, [_, tBr, fBr]) -> do
                                (uT, lT) <- collectPayloadLeaves (depth - 1) (RawExpr tBr)
                                (uF, lF) <- collectPayloadLeaves (depth - 1) (RawExpr fBr)
                                if uT || uF
                                  then return (True, lT ++ lF)
                                  else return (False, [raw])
                            (ICon _ (ICPrim { primOp = PrimCase }), _, _ : dflt : ces) -> do
                                let pairArms (_ : arm : rest) = RawExpr arm : pairArms rest
                                    pairArms _ = []
                                results <- mapM (collectPayloadLeaves (depth - 1)) (RawExpr dflt : pairArms ces)
                                if any fst results
                                  then return (True, concatMap snd results)
                                  else return (False, [raw])
                            _ -> return (False, [raw])

              recordExtractedPayloadLeaf mown' v' resRaw step = do
                  _ <- tryMaybe $ do
                      (sawUndet, leaves) <- collectPayloadLeaves 24 resRaw
                      when sawUndet $ do
                          mUnique <- uniqueRawByKey leaves
                          case mUnique of
                            Just leafOp -> do
                                isConst <- isConstOrUndet leafOp
                                when (not isConst) $
                                    recordStep mown' v' Nothing leafOp [step]
                            Nothing -> return ()
                      return (Just ())
                  return ()

              recordStep mown' v' mpart' res' steps = do
                  srcE <- exprNodeOrCarrier mown' v'
                  mPartCell <- maybe (return Nothing) rawCellOf mpart'
                  mResCell <- rawCellOf res'
                  mResArr <- tryMaybe $ do
                      RawExpr re0 <- return res'
                      re <- evaluate re0
                      case re of
                        ICon _ (ICLazyArray { iArray = arr }) ->
                            Just <$> getArrNode (RawArray arr) Nothing
                        _ -> return Nothing
                  let dstNodes =
                          [ NodeCell p | Just p <- [mPartCell, mResCell] ] ++
                          [ nk | Just nk <- [mResArr] ]
                  case srcE of
                    Right srcNk
                      | not (null dstNodes) ->
                          forM_ dstNodes $ \dstNk ->
                              modifyIORef' stepAdjRef ((srcNk, dstNk, steps) :)
                      | not (null steps) -> do
                          RTExpr eid _ <- mkRTExpr res'
                          modifyIORef' stepAdjRef ((srcNk, NodeExpr eid, steps) :)
                      | otherwise -> return ()
                    Left (Just baseT)
                      | not (null dstNodes) ->
                          forM_ dstNodes $ \dstNk ->
                              case dstNk of
                                NodeCell p ->
                                    modifyIORef' unpackSeedsRef $
                                        IM.insertWith (flip (++)) p [Origin baseT steps]
                                _ -> return ()
                      | not (null steps) -> do
                          RTExpr eid _ <- mkRTExpr res'
                          modifyIORef' exprSeedsRef $
                              IM.insertWith (flip (++)) eid [Origin baseT steps]
                    _ -> return ()

          SemOutPayload (RawExpr out0) tys res (Just scrut) -> do
              mResCell <- rawCellOf res
              mScrutCell <- rawCellOf scrut
              case (mScrutCell, mResCell) of
                (Just sp, Just rp) -> do
                    _ <- tryMaybe $ do
                        outH <- evaluate out0
                        case outH of
                          ICon _ ic0 -> do
                              ic <- evaluate ic0
                              case ic of
                                ICOut { iConType = ot, conTagInfo = cti } -> do
                                    k <- fromInteger <$> evaluate (conNo cti)
                                    m <- evaluate (numCon cti)
                                    when (m > 1) $
                                        case arrowParts (instLead ot tys) of
                                          Just (st, _)
                                            | Just (tb, tq, cn, nc, _) <- dataConName st k ->
                                                modifyIORef' stepAdjRef
                                                    ((NodeCell sp, NodeCell rp,
                                                      [PStepVariantPayload tb tq cn k nc]) :)
                                          _ -> return ()
                                    return (Just ())
                                _ -> return Nothing
                          _ -> return Nothing
                    return ()
                _ -> return ()

          SemOrd f mown v res lid -> do
              _ <- tryMaybe $ do
                  RawExpr ve0 <- return v
                  ve <- evaluate ve0
                  case flatAp ve of
                    (ICon _ (ICCon { conTagInfo = cti }), _, _)
                      | lid >= 0, nameableType f -> do
                          k <- fromInteger <$> evaluate (conNo cti)
                          mwv <- litWidthVal res
                          case (dataConName f k, mwv) of
                            (Just (tb, tq, cn, nc, _), Just (w, val)) ->
                                modifyIORef' litMapRef $
                                    IM.insert lid (NLitEnum tb tq cn k nc w val)
                            _ -> return ()
                          return (Just ())
                    (ICon _ (ICPrim { primOp = PrimChr }), _, _) -> do
                        let nf = norm f
                        case headOf nf of
                          ITCon ti _ (TIdata { tidata_cons = cs, tidata_enum = isEn })
                            | ti /= idBool, not isEn, length cs > 1 -> do
                                let step = PStepVariantTag (getIdBaseString ti)
                                                           (getIdQualString ti)
                                                           (length cs)
                                srcE <- exprNodeOrCarrier mown v
                                mResCell <- rawCellOf res
                                case (srcE, mResCell) of
                                  (Right srcNk, Just rp) ->
                                      modifyIORef' stepAdjRef
                                          ((srcNk, NodeCell rp, [step]) :)
                                  (Right srcNk, Nothing) -> do
                                      RTExpr eid _ <- mkRTExpr res
                                      modifyIORef' stepAdjRef
                                          ((srcNk, NodeExpr eid, [step]) :)
                                  (Left (Just baseT), Just rp) ->
                                      modifyIORef' unpackSeedsRef $
                                          IM.insertWith (flip (++)) rp [Origin baseT [step]]
                                  (Left (Just baseT), Nothing) -> do
                                      RTExpr eid _ <- mkRTExpr res
                                      modifyIORef' exprSeedsRef $
                                          IM.insertWith (flip (++)) eid [Origin baseT [step]]
                                  _ -> return ()
                          _ -> return ()
                        return (Just ())
                    _ -> return Nothing
              return ()

          SemConLit t cti rawLit lid | lid >= 0 -> do
              when (nameableType t) $ do
                  _ <- tryMaybe $ do
                      k <- fromInteger <$> evaluate (conNo cti)
                      mwv <- litWidthVal rawLit
                      case (dataConName t k, mwv) of
                        (Just (tb, tq, cn, nc, _), Just (w, val)) ->
                            modifyIORef' litMapRef $
                                IM.insert lid (NLitEnum tb tq cn k nc w val)
                        _ -> return ()
                      return (Just ())
                  return ()

          SemTagLit (RawExpr hd0) tys rawLit lid | lid >= 0 -> do
              _ <- tryMaybe $ do
                  hd <- evaluate hd0
                  case hd of
                    ICon _ ic0 -> do
                        ic <- evaluate ic0
                        case (ic, arrowParts (instLead (iConType ic) tys)) of
                          (ICIs { conTagInfo = cti }, Just (st, _))
                            | nameableType st -> do
                                k <- fromInteger <$> evaluate (conNo cti)
                                mwv <- litWidthVal rawLit
                                case (dataConName st k, mwv) of
                                  (Just (tb, tq, cn, nc, isEn), Just (w, val)) ->
                                      modifyIORef' litMapRef $
                                          IM.insert lid (NLitTag tb tq cn k nc w val isEn)
                                  _ -> return ()
                          _ -> return ()
                        return (Just ())
                    _ -> return Nothing
              return ()

          SemSlotLit _ slotIdx rawLit lid | lid >= 0 -> do
              mwv <- litWidthVal rawLit
              case mwv of
                Just (w, val) ->
                    modifyIORef' litMapRef $
                        IM.insert lid (NLitSlot (fromInteger slotIdx) 0 w val)
                Nothing -> return ()

          SemConcat _ vals heaped res -> do
              undets <- mapM isUndetExpr vals
              let oneUndet = length (filter id undets) == 1
              isConstRes <- isConstOrUndet res
              when (not oneUndet && not isConstRes && length heaped == 2) $ do
                  -- Only record when strict PrimConcat rewrote into a non-PrimConcat
                  isStillConcat <- (do RawExpr re0 <- return res
                                       re <- evaluate re0
                                       case re of
                                         IAps (ICon _ (ICPrim _ PrimConcat)) _ _ -> return True
                                         _ -> return False)
                                   `catch` \(_ :: SomeException) -> return True
                  when (not isStillConcat) $ do
                      parts <- mapM (\he -> bitsCarrierTarget (-1) (Just he)) heaped
                      mResTarget <- do
                          mp <- rawCellOf res
                          case mp of
                            Just p -> return (Just (RTCell (ptran p)))
                            Nothing -> do
                                ok <- isLeafOrAppBitExpr res
                                if ok then Just <$> mkRTExpr res else return Nothing
                      case (sequence parts, mResTarget) of
                        (Just ps@[_, _], Just rt) ->
                            modifyIORef' inlineFactsRef ((rt, PFactConcat ps) :)
                        _ -> return ()

          SemStored dst rawExpr | dst >= 0 -> do
              meid <- lookupRTExprId rawExpr
              case meid of
                Just eid ->
                    modifyIORef' stepAdjRef ((NodeExpr eid, NodeCell dst, []) :)
                Nothing -> return ()

          _ -> return ()

      -- Pass 2: solve NodeKey -> [Origin] by bounded fixed-point propagation
      unpackSeeds <- readIORef unpackSeedsRef
      exprSeeds <- readIORef exprSeedsRef
      inlineExprMap <- readIORef inlineExprMapRef
      copyAdj <- readIORef copyAdjRef
      stepEdgesRaw <- reverse <$> readIORef stepAdjRef
      let stepEdges = [ (s, d, st) | (s, d, st) <- stepEdgesRaw, isRealNode d ]
          isRealNode (NodeCell p) = p >= 0
          isRealNode (NodeArr _)  = True
          isRealNode (NodeExpr _) = True

          -- Prefer explicit user-binder carrier seeds; otherwise prefer deeper
          -- structured parent steps over anonymous intermediate slice unpacks.
          selectNodeOrigins :: [Origin] -> [Origin] -> [Origin]
          selectNodeOrigins seedOrigs parentOrigs =
              let userSeeds =
                      [ o | o@(Origin (RTCell c) _) <- seedOrigs, hasUserBinder c ]
                  structuredParents =
                      [ o | o@(Origin _ (_ : _)) <- parentOrigs ]
              in dedupOrigins $
                   if not (null userSeeds)
                     then userSeeds
                     else if not (null structuredParents)
                     then structuredParents
                     else parentOrigs ++ seedOrigs

          initMap :: M.Map NodeKey [Origin]
          initMap = M.fromList $
              [ (NodeCell p, dedupOrigins os)
              | (p, os) <- IM.toList unpackSeeds ] ++
              [ (NodeExpr eid, dedupOrigins os)
              | (eid, os) <- IM.toList exprSeeds ]

          allEdges :: [(NodeKey, NodeKey, [ProvStep])]
          allEdges =
              [ (NodeCell src, NodeCell dst, [])
              | (src, dsts) <- IM.toList copyAdj, dst <- dsts ] ++
              stepEdges

          stepRound :: M.Map NodeKey [Origin] -> (M.Map NodeKey [Origin], Bool)
          stepRound cur =
              let incoming = L.foldl' (\acc (src, dst, steps) ->
                      case M.lookup src cur of
                        Nothing -> acc
                        Just origs ->
                            let moved = [ Origin b (st0 ++ steps)
                                        | Origin b st0 <- origs
                                        , length st0 + length steps <= 16 ]
                            in if null moved then acc
                               else M.insertWith (flip (++)) dst moved acc)
                      M.empty allEdges
                  allKeys = S.toList (S.union (M.keysSet cur) (M.keysSet incoming))
                  rebuild (mp, chg) k =
                      let seeds = case k of
                                    NodeCell p -> IM.findWithDefault [] p unpackSeeds
                                    NodeExpr eid -> IM.findWithDefault [] eid exprSeeds
                                    NodeArr _ -> []
                          inc = M.findWithDefault [] k incoming
                          old = M.findWithDefault [] k cur
                          merged = selectNodeOrigins seeds (old ++ inc)
                          changed = map originEqKey merged /= map originEqKey old
                      in (if null merged then mp else M.insert k merged mp, chg || changed)
              in L.foldl' rebuild (M.empty, False) allKeys

          solve :: Int -> M.Map NodeKey [Origin] -> M.Map NodeKey [Origin]
          solve 0 mp = mp
          solve n mp =
              let (mp', chg) = stepRound mp
              in if chg then solve (n - 1) mp' else mp'

          solvedOrigins = solve 20 initMap

      -- Pass 3: collect W literal provenance on canonical cells and inline apps
      litMap <- readIORef litMapRef
      let lookupLitSN :: IExpr a -> IO (Maybe NamedLitInfo)
          lookupLitSN e0
            | IM.null litMap || IM.null semLits = return Nothing
            | otherwise = tryMaybe $ do
                e <- evaluate e0
                case e of
                  ICon _ (ICInt {}) -> do
                      sn <- makeStableName e
                      case lookupSN sn (IM.findWithDefault [] (hashStableName sn) semLits) of
                        Just lid -> return (IM.lookup lid litMap)
                        Nothing -> return Nothing
                  _ -> return Nothing

      -- First identify canonical cells whose whole RHS is a named literal
      cellLitMap <- foldM (\acc (c, RawExpr rhs) -> do
          mnli <- lookupLitSN rhs
          return $ case mnli of
                     Just nli -> IM.insert c nli acc
                     Nothing -> acc)
          IM.empty canonCells

      let argLitOf :: IExpr a -> IO (Maybe NamedLitInfo)
          argLitOf arg0 = do
              direct <- lookupLitSN arg0
              case direct of
                Just nli -> return (Just nli)
                Nothing -> tryMaybe $ do
                    arg <- evaluate arg0
                    case arg of
                      IRefT _ p _ _ -> return (IM.lookup (ptran p) cellLitMap)
                      _ -> return Nothing

      -- Walk an expression tree collecting inline IAps literal-argument facts
      visitedAppsRef <- newIORef (IM.empty :: IM.IntMap [AnySN])
      wFactsRef <- newIORef ([] :: [(RawTarget, ProvFact RawTarget)])

      let walkExprW :: Maybe Int -> RawExpr -> IO ()
          walkExprW mTopCell (RawExpr e0) =
              (do e <- evaluate e0
                  case e of
                    IAps f _ args -> do
                        sn <- makeStableName e
                        let h = hashStableName sn
                        vis <- readIORef visitedAppsRef
                        let bucket = IM.findWithDefault [] h vis
                        case lookupSN sn [ (s, ()) | s <- bucket ] of
                          Just () | mTopCell == Nothing -> return ()
                          _ -> do
                              when (mTopCell == Nothing) $
                                  writeIORef visitedAppsRef $! IM.insert h (AnySN sn : bucket) vis
                              kws <- fmap catMaybes2 $ mapM (\(k, a) -> do
                                  mnli <- argLitOf a
                                  return (fmap (\nli -> (k, nli)) mnli))
                                  (zip [0 :: Int ..] args)
                              when (not (null kws)) $ do
                                  tgt <- case mTopCell of
                                           Just c -> return (RTCell c)
                                           Nothing -> mkRTExpr (RawExpr e)
                                  modifyIORef' wFactsRef ((tgt, PFactArgLits kws) :)
                              walkExprW Nothing (RawExpr f)
                              mapM_ (walkExprW Nothing . RawExpr) args
                    ILam _ _ b -> walkExprW Nothing (RawExpr b)
                    ILAM _ _ b -> walkExprW Nothing (RawExpr b)
                    ICon _ (ICUndet _ _ (Just u)) -> walkExprW Nothing (RawExpr u)
                    _ -> return ())
              `catch` \(_ :: SomeException) -> return ()
            where
              catMaybes2 xs = [ x | Just x <- xs ]

      when (not (IM.null litMap)) $ do
          forM_ canonCells $ \(c, re) -> walkExprW (Just c) re
          forM_ rootExprs $ \re -> walkExprW Nothing re

      -- Assemble per-canonical-cell facts and inline expression facts
      let rawCellAccessFacts :: IM.IntMap [ProvFact RawTarget]
          rawCellAccessFacts = L.foldl' (\acc (nk, origs) ->
              case nk of
                NodeCell rawP ->
                    let c = ptran rawP
                    in if not (IM.member c canonSet) then acc
                       else let fs = [ PFactAccess base steps
                                     | Origin base steps@(_ : _) <- origs
                                     , case base of
                                         RTCell bc -> bc /= c
                                         _ -> True ]
                            in if null fs then acc
                               else IM.insertWith (flip (++)) c fs acc
                _ -> acc)
              IM.empty (M.toList solvedOrigins)

          litCellFacts :: IM.IntMap [ProvFact RawTarget]
          litCellFacts = IM.map (\nli -> [PFactLit nli]) cellLitMap

      inlineFacts0 <- reverse <$> readIORef inlineFactsRef
      wFacts <- reverse <$> readIORef wFactsRef

      let solvedInlineAccess :: [(RawTarget, ProvFact RawTarget)]
          solvedInlineAccess =
              [ (tgt, PFactAccess base steps)
              | (NodeExpr eid, origs) <- M.toList solvedOrigins
              , Just tgt <- [IM.lookup eid inlineExprMap]
              , Origin base steps@(_ : _) <- origs ]

          extraCellFacts :: IM.IntMap [ProvFact RawTarget]
          extraCellFacts = L.foldl' (\acc (tgt, f) ->
              case tgt of
                RTCell c | IM.member c canonSet -> IM.insertWith (flip (++)) c [f] acc
                _ -> acc)
              IM.empty (inlineFacts0 ++ solvedInlineAccess ++ wFacts)

          -- Deduplicate both RTCell and RTExpr facts in first-sighting order.
          dedupSafe :: [ProvFact RawTarget] -> [ProvFact RawTarget]
          dedupSafe = go S.empty
            where
              go _ [] = []
              go seen (x : xs) =
                  let kx = fmap rawTargetKey x
                  in if S.member kx seen
                       then go seen xs
                       else x : go (S.insert kx seen) xs

          cellFactsMap :: IM.IntMap [ProvFact RawTarget]
          cellFactsMap = IM.fromList
              [ (c, fs)
              | (c, _) <- canonCells
              , let fs = dedupSafe $
                         IM.findWithDefault [] c rawCellAccessFacts ++
                         IM.findWithDefault [] c litCellFacts ++
                         IM.findWithDefault [] c extraCellFacts
              , not (null fs) ]

          inlineTargets :: [(RawTarget, [ProvFact RawTarget])]
          inlineTargets =
              [ (t, [f])
              | (t@(RTExpr {}), f) <- inlineFacts0 ++ solvedInlineAccess ++ wFacts ]

          orderedTargets :: [(RawTarget, [ProvFact RawTarget])]
          orderedTargets =
              [ (RTCell c, fs)
              | (c, _) <- canonCells
              , Just fs <- [IM.lookup c cellFactsMap] ] ++
              inlineTargets

      _ <- evaluate (IM.size cellFactsMap + sum [ length fs | (_, fs) <- orderedTargets ])
      return (cellFactsMap, orderedTargets))
    `catch` \(_ :: SomeException) -> return (IM.empty, [])
