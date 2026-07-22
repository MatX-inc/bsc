-- | Ground evaluation of associated type functions (ATFs) directly from
-- the instance declarations in the symbol table -- the "materialized
-- rewrite rules" that replace the ATF result cache.
--
-- An ATF's semantics is the set of equations its class's instances
-- declare: each instance contributes one equation (head patterns at the
-- ATF's parameter positions, result at the target position, plus the
-- instance context).  Reduction of a FULLY APPLIED application at
-- GROUND arguments (no type variables anywhere) is pure evaluation over
-- those equations: commit to the unique most-specific instance whose
-- head matches, then run the instance context's functional dependencies
-- -- also at ground types -- to bind whatever variables the result
-- still mentions.
--
-- On ground arguments, matching is plain structural pattern matching:
-- no substitution into the argument can change which equations match,
-- so "the most specific matching instance" is exactly the instance
-- reducePred commits to (it walks candidates most-specific-first and
-- takes the first match), reached with none of the typechecker's
-- machinery: no fresh-variable supply, no unification, no improvement,
-- no deferral, no incoherence judgment.  If two matching instances are
-- incomparable, the family's ground meaning is ambiguous -- the overlap
-- check rejects that for coherent classes, so hitting it here means an
-- unsealed/incoherent family leaked into permanent construction, and
-- the evaluator answers Nothing so the caller can fail loudly.
--
-- Everything outside the ground domain stays where it belongs:
--
--  * Applications over type variables are NOT reduced here.  Whether
--    and how they reduce is scope-dependent (rigid variables act as
--    atoms; see the bound_tyvars threading in TCMisc.sat) and is the
--    typechecker's judgment, consumed at generalization time.  In
--    ISyntax such applications simply stay dormant until substitution
--    grounds them.
--
--  * Satisfiability of the matched instance's context is NOT
--    re-verified.  Type-function reduction is fundep projection; the
--    dictionaries were the typechecker's obligation at the original
--    use site.  Context predicates are consulted only to bind
--    variables that the result mentions.
--
-- Callers in permanent construction (IConv / IExpand) treat a Nothing
-- from 'atfReduceGround' as an internal error: a ground application
-- whose family cannot reduce it means the instance set visible to this
-- compilation is missing (or is ambiguous about) an equation some
-- upstream typecheck relied on -- a real bug that must be loud, never
-- an occasion to fall back into the typechecker.
--
-- Known restriction: instance heads are matched structurally, so a
-- family whose selection relies on solving numeric equations in the
-- head (mgu's deferred numeric equalities) is outside the ground
-- domain and answers Nothing.  No ATF-carrying family in the current
-- libraries does this.

module ATFRules(atfReduceGroundApp,
    ATFRules,
    buildATFRules,
    atfReduceGround,
    atfReduceInType
) where

import qualified Data.Map as M
import Data.Maybe(isJust)
import Control.Monad(foldM, guard)

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
import IType(iTypeNodeId, itHasTFun)
import TypeShareFlags(noITypeWalkMemos)
import qualified Data.IntMap.Strict as IM
import Data.IORef(IORef, newIORef, readIORef, atomicModifyIORef')
import System.IO.Unsafe(unsafePerformIO, unsafeDupablePerformIO)
import ISyntaxSubst(tSubstBatch)

-- The rules are views of the instance declarations already in the
-- symbol table; nothing here is captured from solver runs, so a value
-- of this type can never be stale relative to anything but the
-- instance declarations themselves.
newtype ATFRules = ATFRules SymTab

buildATFRules :: SymTab -> ATFRules
buildATFRules = ATFRules

-- Bounds recursion through nested reductions and context solving.
-- Exhaustion means a non-terminating family (the typechecker's
-- sat stack would have looped on the same program).
maxFuel :: Int
maxFuel = 4096

-- | Reduce a fully-applied ATF application with ground arguments to
-- its normal form.  The 'TISort' is the application head's; the
-- argument list is in @atf_param_idxs@ order.  @Nothing@ means no
-- equation matched, the match was ambiguous, an instance context
-- could not be grounded, or fuel ran out -- all internal errors at
-- permanent-construction call sites.
-- Ground reductions memoize per applied-node intern unique: the
-- retired cache's amortization with a canonical-identity key instead
-- of a deep type key.  Sound here because atfReduceGround always
-- evaluates at the constant maxFuel (never cache a variable-fuel
-- verdict: Nothing conflates no-equation with fuel-out).  The applied
-- node is interned unconditionally, so the memo is active flag-off
-- and strictly dominates the cache it replaces.
{-# NOINLINE atfRedMemo #-}
atfRedMemo :: IORef (IM.IntMap (Maybe IType))
atfRedMemo = unsafePerformIO $ newIORef IM.empty

atfReduceGroundApp :: ATFRules -> IType -> Id -> TISort -> [IType]
                   -> Maybe IType
atfReduceGroundApp rules app atfId so args
  | noITypeWalkMemos = atfReduceGround rules atfId so args
  | otherwise = unsafeDupablePerformIO $ do
    let u = iTypeNodeId app
    m0 <- readIORef atfRedMemo
    case IM.lookup u m0 of
      Just r -> return r
      Nothing -> do
        let r = atfReduceGround rules atfId so args
        _ <- return $! r
        atomicModifyIORef' atfRedMemo (\ m -> (IM.insert u r m, ()))
        return r

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

-- | Reduce every ground ATF application in a type, leaving all other
-- structure -- including type variables -- untouched.  @Nothing@ if
-- the type contains an ATF application this evaluator cannot
-- eliminate: a fully applied application over type variables
-- (dormant; possibly reducible by the typechecker's scope-relative
-- judgment, never by ours) or a ground application with no matching
-- equation.  Partially applied ATF constructors are inert structure
-- and do not count.
atfReduceInType :: ATFRules -> IType -> Maybe IType
atfReduceInType rules t0 = go maxFuel t0
  where
    -- ATF-free subtrees (memoized per intern unique) are returned
    -- unwalked: they may be exponentially shared DAGs
    go _ t | not noITypeWalkMemos, not (itHasTFun t) = Just t
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

-- Resolve one class constraint at ground inputs: pick the unique
-- most-specific instance whose head patterns (at the given input
-- positions) match the ground input types, bind its variables from the
-- match and from its context's fundeps, and return the ground-
-- normalized types at the requested output positions.
resolveClass :: ATFRules -> Int -> Class -> [Int] -> [Int] -> [IType]
             -> Maybe [IType]
resolveClass rules fuel cls inPos outPos ins = do
    guard (fuel > 0)
    let insts | isPreClass cls = preClassInsts cls inPos ins
              | otherwise      = getInsts cls
        cands = [ (n, map (argPats !!) inPos, map (argPats !!) outPos,
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

-- The numeric pre-classes (Add, Mul, Div, Log, Max, Min, NumEq)
-- declare no instances -- getInsts is empty by construction -- and
-- their equations are instead generated on demand by the genInsts
-- clauses in StdPrel.  At a query that is ground at the fundep's
-- input positions those clauses are pure arithmetic evaluation of a
-- compiler-sealed closed family (first matching clause wins, at most
-- one instance comes back), so consulting them stays inside the
-- evaluator's ground domain.  The judgment-flavored "last resort"
-- clauses (which invent structural results for unconstrained
-- variables during defaulting) are all gated on the Just-defaulting
-- context, which we never supply.  Non-input positions are filled
-- with the class's own signature variables, so only clauses that
-- compute those positions can fire.
preClassInsts :: Class -> [Int] -> [IType] -> [Inst]
preClassInsts cls inPos ins = genInsts cls [] Nothing (IsIn cls qargs)
  where
    inArgs = zip inPos ins
    qargs = [ maybe (TVar v) uncvtType (lookup i inArgs)
            | (i, v) <- zip [0 ..] (csig cls) ]

-- Among the instances that match, the committed one must generalize
-- none and be generalized by all -- i.e., be the unique minimum of the
-- specificity order restricted to the matches.  (For a coherent class
-- the overlap check has already guaranteed that any two instances that
-- can match the same ground type are comparable, so this is exactly
-- the first match in reducePred's most-specific-first candidate walk.)
pickMostSpecific :: [(Int, [IType], [IType], [Pred])]
                 -> Maybe (Int, [IType], [IType], [Pred])
pickMostSpecific matches =
    case [ m | m@(n, ipats, _, _) <- matches
             , all (\ (n', ipats', _, _) ->
                        n == n' || generalizes ipats' ipats) matches ] of
      [m] -> Just m
      _   -> Nothing
  where
    -- ps generalizes qs: the patterns ps match the patterns qs viewed
    -- as subjects (the subjects' variables act as rigid atoms: a
    -- pattern variable may bind one, but a constructor never matches
    -- one -- which is exactly "more general").
    generalizes ps qs = isJust (matchMany ps qs M.empty)

-- Bind the variables an instance's result needs by running the
-- context's functional dependencies at ground types.  Iterate to a
-- fixpoint: solving one predicate can ground another's inputs.  A
-- predicate whose fundep inputs are ground but which no instance
-- resolves is a hard failure; a predicate that never becomes solvable
-- is left alone (if the result needed its bindings, the caller's
-- groundness check fails loudly; if not, it was a dictionary
-- obligation and none of our business).
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
                      Just Nothing   -> Nothing  -- ground inputs, no instance
        -- Just (Just b)  : solved, bindings extended
        -- Just Nothing   : solvable (ground inputs) but resolution failed
        -- Nothing        : not solvable yet (inputs not ground)
        trySolve f c ts' =
            case [ bs | bs <- funDeps c
                      , all isGround (sel (map not bs) ts')
                      , not (all isGround (sel bs ts')) ] of
              [] -> Nothing
              (bs : _) ->
                  let inPos  = [ i | (i, True)  <- zip [0..] (map not bs) ]
                      outPos = [ i | (i, True)  <- zip [0..] bs ]
                  in Just $ do
                       outs <- resolveClass rules (f - 1) c inPos outPos
                                            (map (ts' !!) inPos)
                       matchMany (map (ts' !!) outPos) outs M.empty
        sel flags xs = [ x | (True, x) <- zip flags xs ]
    loop fuel b0

-- One-way structural matching: pattern variables bind (consistently,
-- for non-linear patterns); everything in the subject is inert,
-- including variables, which act as rigid atoms -- a pattern variable
-- may bind one, a pattern constructor never matches one.
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

-- Fully normalize a ground type: reduce ATF applications innermost-
-- first and rebuild through normITAp, which folds the primitive
-- numeric/string type functions as their arguments become literals.
-- Partially applied ATF constructors are inert structure, like any
-- other constructor.  Nothing if a variable survives (the input was
-- not ground, or an instance context failed to determine an output)
-- or a fully-applied ATF cannot reduce.
groundNorm :: ATFRules -> Int -> IType -> Maybe IType
groundNorm rules fuel t0 = do
    guard (fuel > 0)
    case t0 of
      ITVar _      -> Nothing
      ITForAll _ _ _ -> Nothing
      ITCon _ _ _  -> Just t0
      ITNum _      -> Just t0
      ITStr _      -> Just t0
      ITAp _ _ ->
        case spine t0 [] of
          (f@(ITCon atfId _ so@(TIatf { atf_param_idxs = pIdxs })), as)
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

-- Ground IType -> CType, the inverse of cvtType (source positions are
-- gone; these types only feed genInsts pattern matching and come
-- straight back in through cvtType).
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

-- Structural CType -> IType conversion for instance components
-- (a mirror of IConv.iConvT', which is not importable here without a
-- cycle once IConv consumes the rules).  expandSyn first: instance
-- declarations may mention synonyms, which never survive into IType,
-- and it also folds primitive type functions over literals.
cvtType :: Type -> IType
cvtType = cvt . expandSyn
  where
    cvt (TVar (TyVar i _ _)) = ITVar i
    cvt (TCon (TyCon i (Just k) s)) = ITCon i (cvtK k) s
    cvt (TCon (TyNum n _)) = ITNum n
    cvt (TCon (TyStr s _)) = ITStr s
    -- rc8: the ITAp smart constructor performs the reduction
    cvt (TAp t1 t2) = ITAp (cvt t1) (cvt t2)
    cvt t = internalError ("ATFRules.cvtType: " ++ ppReadable t)
    cvtK KStar = IKStar
    cvtK KNum = IKNum
    cvtK KStr = IKStr
    cvtK (Kfun k1 k2) = IKFun (cvtK k1) (cvtK k2)
    cvtK k = internalError ("ATFRules.cvtType: kind: " ++ ppReadable k)
