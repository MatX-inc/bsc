-- Ground evaluation of associated type functions (ATFs) directly from
-- the instance declarations in the symbol table.
--
-- An ATF's semantics is the set of equations its class's instances
-- declare: each instance contributes one equation (head patterns at the
-- ATF's parameter positions, result at the target position, plus the
-- instance context).  Reduction of a fully applied application at
-- ground arguments (no type variables anywhere) commits to the unique
-- most-specific instance whose head matches, then runs the instance
-- context's functional dependencies at ground types to bind whatever
-- variables the result still mentions.
--
-- Applications over type variables stay dormant until substitution
-- grounds them; satisfiability of the matched instance's context is
-- already checked at the original use site.

module ATFRules(
    ATFRules,
    buildATFRules,
    atfReduceGround,
    atfReduceGroundApp,
    atfReduceInType
) where

import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import Data.IORef(IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Maybe(isJust)
import Control.Monad(foldM, guard)
import System.IO.Unsafe(unsafePerformIO, unsafeDupablePerformIO)

import Id(Id)
import ErrorUtil(internalError)
import Position(noPosition)
import PPrint(ppReadable)
import CType(Type(..), TyVar(..), TyCon(..), Kind(..), TISort(..),
             CTypeclass(..))
import Pred(Pred(..), Qual(..), Class(..), Inst(..), expandSyn,
            removePredPositions)
import StdPrel(isPreClass)
import SymTab(SymTab, findSClass)
import ISyntax(IType(..), IKind(..))
import IType(iTypeNodeId, iTypeAtfFree)
import ISyntaxSubst(tSubstBatch)

-- The rules are views of the instance declarations already in the
-- symbol table; nothing here is captured from solver runs.
newtype ATFRules = ATFRules SymTab

buildATFRules :: SymTab -> ATFRules
buildATFRules = ATFRules

-- Bounds recursion through nested reductions and context solving.
maxFuel :: Int
maxFuel = 4096

-- Ground reductions memoize per applied-node intern unique at constant
-- maxFuel (never cache variable-fuel verdicts, where Nothing conflates
-- no-equation with fuel-out).
{-# NOINLINE atfRedMemo #-}
atfRedMemo :: IORef (IM.IntMap (Maybe IType))
atfRedMemo = unsafePerformIO $ newIORef IM.empty

-- Reduce through the memo, keyed on the intern unique of the full
-- application node.
atfReduceGroundApp :: ATFRules -> IType -> Id -> TISort -> [IType]
                   -> Maybe IType
atfReduceGroundApp rules app atfId so args = unsafeDupablePerformIO $ do
    let u = iTypeNodeId app
    m0 <- readIORef atfRedMemo
    case IM.lookup u m0 of
      Just r -> return r
      Nothing -> do
        let r = atfReduceGround rules atfId so args
        _ <- return $! r
        atomicModifyIORef' atfRedMemo (\ m -> (IM.insert u r m, ()))
        return r

-- Reduce a fully-applied ATF application with ground arguments to its
-- normal form.  Nothing means no equation matched, the match was
-- ambiguous, an instance context could not be grounded, or fuel ran out.
atfReduceGround :: ATFRules -> Id -> TISort -> [IType] -> Maybe IType
atfReduceGround rules atfId so args =
    case so of
      TIatf { atf_param_idxs = pIdxs }
        | length args /= length pIdxs ->
            internalError ("ATFRules.atfReduceGround: not fully applied: " ++
                           ppReadable (atfId, args))
        | otherwise -> reduceATF rules maxFuel atfId so args
      _ -> internalError ("ATFRules.atfReduceGround: not an ATF tycon: " ++
                          ppReadable atfId)

-- Reduce every ground ATF application in a type, leaving all other
-- structure (including type variables) untouched.  Returns Nothing if
-- the type contains a dormant fully-applied ATF over type variables or
-- an unreduced ground ATF application.
atfReduceInType :: ATFRules -> IType -> Maybe IType
atfReduceInType rules t0 = go maxFuel t0
  where
    go _ t | iTypeAtfFree t = Just t
    go fuel _ | fuel <= 0 = Nothing
    go fuel (ITForAll i k b) = ITForAll i k `fmap` go (fuel - 1) b
    go fuel t@(ITAp _ _) =
      case spine t [] of
        (ITCon atfId _ so@(TIatf { atf_param_idxs = pIdxs }), as)
          | length as == length pIdxs -> do
              as' <- mapM (go (fuel - 1)) as
              if all isGround as'
                then reduceATF rules (fuel - 1) atfId so as'
                else Nothing
        (f, as) -> do
              as' <- mapM (go (fuel - 1)) as
              Just (foldl ITAp f as')
    go _ t = Just t
    spine (ITAp f a) as = spine f (a : as)
    spine f as = (f, as)

reduceATF :: ATFRules -> Int -> Id -> TISort -> [IType] -> Maybe IType
reduceATF rules@(ATFRules symt) fuel atfId
          (TIatf { atf_class_id = clsId
                 , atf_param_idxs = pIdxs
                 , atf_target_idx = tIdx })
          args = do
    guard (fuel > 0)
    cls <- case findSClass symt (CTypeclass clsId) of
             Just c  -> Just c
             Nothing -> internalError ("ATFRules.reduceATF: no class: " ++
                                       ppReadable (atfId, clsId))
    outs <- resolveClass rules (fuel - 1) cls pIdxs [tIdx] args
    case outs of
      [out] -> Just out
      _     -> Nothing
reduceATF _ _ atfId _ _ =
    internalError ("ATFRules.reduceATF: not an ATF tycon: " ++ ppReadable atfId)

pickAt :: String -> [a] -> Int -> a
pickAt tag xs idx
  | idx >= 0, y : _ <- drop idx xs = y
  | otherwise = internalError ("ATFRules." ++ tag ++ ": index out of bounds: " ++ show idx)

pickIdxs :: String -> [a] -> [Int] -> [a]
pickIdxs tag xs = map (pickAt tag xs)

-- Resolve one class constraint at ground inputs: pick the unique
-- most-specific matching instance, bind its variables from the match and
-- from its context's fundeps, and return the ground-normalized outputs.
resolveClass :: ATFRules -> Int -> Class -> [Int] -> [Int] -> [IType]
             -> Maybe [IType]
resolveClass rules fuel cls inPos outPos ins = do
    guard (fuel > 0)
    let insts | isPreClass cls = preClassInsts cls inPos ins
              | otherwise      = getInsts cls
        cands = [ (n, pickIdxs "resolveClass.in" argPats inPos,
                   pickIdxs "resolveClass.out" argPats outPos,
                   map removePredPositions ctx)
                | (n, Inst _ _ (ctx :=> IsIn _ cargs) _)
                      <- zip [0 :: Int ..] insts
                , let argPats = map cvtType cargs ]
        matches = [ c | c@(_, ipats, _, _) <- cands
                      , isJust (matchMany ipats ins M.empty) ]
    (_, ipats, opats, ctx) <- pickMostSpecific matches
    b0 <- matchMany ipats ins M.empty
    b  <- solveCtx rules (fuel - 1) b0 ctx
    mapM (groundNorm rules (fuel - 1) . tSubstBatch b) opats

-- The numeric pre-classes (Add, Mul, Div, Log, Max, Min, NumEq) generate
-- equations on demand through StdPrel's genInsts clauses.
preClassInsts :: Class -> [Int] -> [IType] -> [Inst]
preClassInsts cls inPos ins = genInsts cls [] Nothing (IsIn cls qargs)
  where
    inArgs = zip inPos ins
    qargs = [ maybe (TVar v) uncvtType (lookup i inArgs)
            | (i, v) <- zip [0 ..] (csig cls) ]

pickMostSpecific :: [(Int, [IType], [IType], [Pred])]
                 -> Maybe (Int, [IType], [IType], [Pred])
pickMostSpecific matches =
    case [ m | m@(n, ipats, _, _) <- matches
             , all (\ (n', ipats', _, _) ->
                        n == n' || generalizes ipats' ipats) matches ] of
      [m] -> Just m
      _   -> Nothing
  where
    generalizes ps qs = isJust (matchMany ps qs M.empty)

solveCtx :: ATFRules -> Int -> M.Map Id IType -> [Pred]
         -> Maybe (M.Map Id IType)
solveCtx rules fuel b0 ctx = do
    guard (fuel > 0)
    let pats = [ (c, map cvtType ts) | IsIn c ts <- ctx ]
        loop f b = do
            guard (f > 0)
            r <- foldM (step f) (b, False) pats
            case r of
              (b', True)  -> loop (f - 1) b'
              (b', False) -> Just b'
        step f (b, progress) (c, ts) =
            let ts' = map (tSubstBatch b) ts
            in if all isGround ts'
               then Just (b, progress)
               else case trySolve f c ts' of
                      Nothing        -> Just (b, progress)
                      Just (Just b') -> Just (b' `M.union` b, True)
                      Just Nothing   -> Nothing
        trySolve f c ts' =
            case [ bs | bs <- funDeps c
                      , all isGround (sel (map not bs) ts')
                      , not (all isGround (sel bs ts')) ] of
              [] -> Nothing
              (bs : _) ->
                  let inPos  = [ i | (i, True) <- zip [0..] (map not bs) ]
                      outPos = [ i | (i, True) <- zip [0..] bs ]
                  in Just $ do
                       outs <- resolveClass rules (f - 1) c inPos outPos
                                            (pickIdxs "solveCtx.in" ts' inPos)
                       matchMany (pickIdxs "solveCtx.out" ts' outPos) outs M.empty
        sel flags xs = [ x | (True, x) <- zip flags xs ]
    loop fuel b0

matchPat :: IType -> IType -> M.Map Id IType -> Maybe (M.Map Id IType)
matchPat (ITVar v) t m =
    case M.lookup v m of
      Nothing -> Just (M.insert v t m)
      Just t' -> if t == t' then Just m else Nothing
matchPat (ITAp pf pa) (ITAp f a) m = matchPat pf f m >>= matchPat pa a
matchPat (ITCon i _ _) (ITCon i' _ _) m = if i == i' then Just m else Nothing
matchPat (ITNum n) (ITNum n') m = if n == n' then Just m else Nothing
matchPat (ITStr s) (ITStr s') m = if s == s' then Just m else Nothing
matchPat _ _ _ = Nothing

matchMany :: [IType] -> [IType] -> M.Map Id IType -> Maybe (M.Map Id IType)
matchMany ps ts m
  | length ps == length ts = foldM (\ m' (p, t) -> matchPat p t m') m
                                   (zip ps ts)
  | otherwise = Nothing

-- Fully normalize a ground type: reduce ATF applications innermost-first
-- and rebuild through the ITAp smart constructor, which folds primitive
-- numeric/string type functions as their arguments become literals.
groundNorm :: ATFRules -> Int -> IType -> Maybe IType
groundNorm rules fuel t0 = do
    guard (fuel > 0)
    case t0 of
      ITVar _        -> Nothing
      ITForAll _ _ _ -> Nothing
      ITCon _ _ _    -> Just t0
      ITNum _        -> Just t0
      ITStr _        -> Just t0
      ITAp _ _
        | iTypeAtfFree t0 ->
            if isGround t0 then Just t0 else Nothing
        | otherwise ->
            case spine t0 [] of
              (ITCon atfId _ so@(TIatf { atf_param_idxs = pIdxs }), as)
                | length as == length pIdxs -> do
                    as' <- mapM (groundNorm rules (fuel - 1)) as
                    reduceATF rules (fuel - 1) atfId so as'
              (f, as) -> do
                    as' <- mapM (groundNorm rules (fuel - 1)) as
                    Just (foldl ITAp f as')
  where
    spine (ITAp f a) as = spine f (a : as)
    spine f as = (f, as)

isGround :: IType -> Bool
isGround (ITVar _) = False
isGround (ITForAll _ _ _) = False
isGround (ITAp f a) = isGround f && isGround a
isGround _ = True

uncvtType :: IType -> Type
uncvtType (ITNum n) = TCon (TyNum n noPosition)
uncvtType (ITStr s) = TCon (TyStr s noPosition)
uncvtType (ITCon i k s) = TCon (TyCon i (Just (uncvtK k)) s)
uncvtType (ITAp f a) = TAp (uncvtType f) (uncvtType a)
uncvtType t =
    internalError ("ATFRules.uncvtType: not ground: " ++ ppReadable t)

uncvtK :: IKind -> Kind
uncvtK IKStar = KStar
uncvtK IKNum = KNum
uncvtK IKStr = KStr
uncvtK (IKFun k1 k2) = Kfun (uncvtK k1) (uncvtK k2)

cvtType :: Type -> IType
cvtType = cvt . expandSyn
  where
    cvt (TVar (TyVar i _ _)) = ITVar i
    cvt (TCon (TyCon i (Just k) s)) = ITCon i (cvtK k) s
    cvt (TCon (TyNum n _)) = ITNum n
    cvt (TCon (TyStr s _)) = ITStr s
    cvt (TAp t1 t2) = ITAp (cvt t1) (cvt t2)
    cvt t = internalError ("ATFRules.cvtType: " ++ ppReadable t)
    cvtK KStar = IKStar
    cvtK KNum = IKNum
    cvtK KStr = IKStr
    cvtK (Kfun k1 k2) = IKFun (cvtK k1) (cvtK k2)
    cvtK k = internalError ("ATFRules.cvtType: kind: " ++ ppReadable k)
