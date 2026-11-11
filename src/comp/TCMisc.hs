{-# LANGUAGE PatternGuards #-}
module TCMisc(
        splitF, satisfyFV, satisfy,
        satisfyStream, satisfyFVStream, batchSolveNumericPreds,
        satisfyG, satisfyStreamG, satisfyFVStreamG,
        reducePred, Reduction(..),
        reducePredsAggressive, SolveResult(..), expTFun, expTConPred,
        findAssump, mkQualType, closeFD, niceTypes,
        unifyFnFrom, unifyFnTo, unifyFnFromTo,
        mkSchemeNoBVs, rmPatLit, rmQualLit, expandSynN, normT, expandFullType,
        unify, unifyNoEq,
        mkVPred, mkVPredNoNewPos, mkVPredFromPred, toPredWithPositions, toPred,
        defaultClasses,
        checkForAmbiguousPreds,
        propagateFunDeps, isReduciblePred, findFunDepConflict,
        warnTransitiveIncoherent
              ) where

import Data.Maybe
import Data.List
import Control.Monad(foldM, when)
import qualified Data.IntSet as IS
import Data.Array((!))
import qualified Data.Map as M
import qualified Data.Set as S
import System.IO.Unsafe(unsafePerformIO)

import Util
import PFPrint
import Position
import CType
import FStringCompat(mkFString)
import Id
import Error(internalError, EMsg, ErrMsg(..))
import Flags(Flags, useProvisoSAT)
import Type
import TypeOps(isPrimTFunName)
import Subst
import Assump
import Scheme
import SolvedBinds
import Unify
import Pred
import SATPred
import TIMonad
import PreIds
import StdPrel(isPreClass, mkNumInstBody)
import CSyntax(CExpr(..), CPat(..), CQual(..), CLiteral(..),
               cTApply, cVApply)
import Literal
import IntLit
import SymTab
import MakeSymTab(convCQType)
import PreStrings(sAcute)
import IOUtil(progArgs)
import Debug.Trace
import Control.Monad.Except(throwError)
import qualified Data.IntMap.Lazy as IML
import qualified Data.IntMap.Strict as IM
import Flags(maxTIStackDepth)
import DictPool

-------

useLegacyInstIndex :: Bool
useLegacyInstIndex = elem "-legacy-inst-index" progArgs

-- Restore the legacy instance-resolution policy: an instance whose input
-- positions match but whose fundep-determined positions conflict is skipped
-- (allowing a later, less specific instance to be selected), and a coherent
-- match whose context is not yet satisfied is discarded and retried instead
-- of being committed.  See doc/dev-notes/typechecker-coherent-instance-
-- commitment.md.
legacyDeferInstances :: Bool
legacyDeferInstances = elem "-legacy-defer-instances" progArgs

doRTrace :: Bool
doRTrace = elem "-trace-type" progArgs
rtrace :: String -> a -> a
rtrace s x = if doRTrace then traces s x else x

doTraceSatisfy :: Bool
doTraceSatisfy = elem "-trace-tc-satisfy" progArgs

satTrace :: String -> a -> a
satTrace s x = if doTraceSatisfy then traces s x else x

satTraceM :: Monad m => String -> m ()
satTraceM s = when (doTraceSatisfy) $ traceM s

doTraceReducePred :: Bool
doTraceReducePred = elem "-trace-tc-reducepred" progArgs
rpTrace :: String -> a -> a
rpTrace s x = if doTraceReducePred then traces s x else x

doVarTrace :: Bool
doVarTrace = elem "-trace-tcvar" progArgs

-------

type DVS = Maybe [TyVar]

splitF :: [TyVar] -> [VPred] -> ([VPred], [VPred])
splitF fs ps = partition (all (`elem` fs) . tv) ps

-------

-- The following "satisfy" functions take a list of predicates
-- ("es" of type [EPred]) which can be assumed to exist (that is, they
-- are the provided context in an explicitly-typed let-binding, and
-- the converted function will take in dictionaries for these
-- predicates).  And they take a list of predicates which were
-- inferred, and need to be solved.  And they return a list of any
-- unsolved predicates, and new definitions for dictionaries of any
-- predicates which were solved.  (The dictionaries are defined in
-- terms of existing dictionaries, such as the "es".)

-- "satisfy" and "satisfyFV" are verdict-complete: numeric residuals
-- are settled (proven or refuted by the SAT solver, in one batch)
-- before the residual list is returned, so a caller may treat the
-- result as final -- an empty result means fully satisfied.  Callers
-- that instead OWN a later settlement point (tiExpl''' settles once
-- per definition, after defaulting; tiImpls hands its residuals to
-- reducePredsAggressive, which settles) use the "Stream" variants,
-- which leave numeric residuals unsolved so they flow to the owner.
-- The satisfy loop itself never consults the solver: a solver verdict
-- may reject the program but never rewinds a binding or a match.

-- Whether a partial reduction may be kept: Committable means the
-- instance choice is final -- a coherent match that no other
-- instance and no in-scope given could ever satisfy -- so the caller
-- may commit to the reduction instead of putting the predicate aside
-- to retry.
data Commitment = Committable
                | Provisional
                deriving (Eq)

-- The outcome of reducing one predicate (sat): the residual goals
-- that remain, with the bindings and substitution produced.
data SatResult = SatResult {
        satNeeded     :: [VPred],
        satBinds      :: SolvedBinds,
        satSubst      :: Subst,
        satCommitment :: Commitment,
        satRecipe     :: Recipe     -- how satBinds was built (DictPool)
    }

-- The outcome of a reduction loop over many predicates (sMany,
-- satMany, reducePredsAggressive): the predicates that could not be
-- discharged, with the accumulated bindings and substitution.
data SolveResult = SolveResult {
        solveNeeded :: [VPred],
        solveBinds  :: SolvedBinds,
        solveSubst  :: Subst
    }

satisfy :: [EPred] -> [VPred] -> TI ([VPred], SolvedBinds)
satisfy es ps = satisfyX Nothing (mkGivens es) ps

satisfyStream :: [EPred] -> [VPred] -> TI ([VPred], SolvedBinds)
satisfyStream es ps = satisfyXStream Nothing (mkGivens es) ps

-- satisfy and satisfyStream, with givens whose class index is at hand
-- (TIMonad.getExplGivens, getTopExplSeg)
satisfyG :: Givens -> [VPred] -> TI ([VPred], SolvedBinds)
satisfyG es ps = satisfyX Nothing es ps

satisfyStreamG :: Givens -> [VPred] -> TI ([VPred], SolvedBinds)
satisfyStreamG es ps = satisfyXStream Nothing es ps

-- This version also takes a list of the variables from the
-- environment, which have been filtered for dependencies.  These
-- TyVars are used only in "genInsts" (in "reducePred") for numeric
-- classes in the StdPrel.  For example (Add x y (TVar v)) only turns
-- into (Add x y (TAdd x y)) if the "v" being replaced isn't in the
-- list of TyVars.
satisfyFV :: [TyVar] -> [EPred] -> [VPred] -> TI ([VPred], SolvedBinds)
satisfyFV vs es ps =
        satTrace ("satisfyFV " ++ ppReadable ps) $
        satisfyX (Just vs) (mkGivens es) ps

satisfyFVStream :: [TyVar] -> [EPred] -> [VPred] -> TI ([VPred], SolvedBinds)
satisfyFVStream vs es ps =
        satTrace ("satisfyFVStream " ++ ppReadable ps) $
        satisfyXStream (Just vs) (mkGivens es) ps

satisfyFVStreamG :: [TyVar] -> Givens -> [VPred] -> TI ([VPred], SolvedBinds)
satisfyFVStreamG vs es ps =
        satTrace ("satisfyFVStream " ++ ppReadable ps) $
        satisfyXStream (Just vs) es ps

satisfyX :: DVS -> Givens -> [VPred] -> TI ([VPred], SolvedBinds)
satisfyX _   es [] = return ([], emptySBs)
satisfyX dvs es ps = do
        (rs, sbs) <- satisfyXStream dvs es ps
        -- settle the numeric residuals (see the contract above)
        s <- getSubst
        (rs', nsbs) <- batchSolveNumericPreds (givensList (apSub s es)) rs
        return (rs', nsbs <++ sbs)

satisfyXStream :: DVS -> Givens -> [VPred] -> TI ([VPred], SolvedBinds)
satisfyXStream _   es [] = return ([], emptySBs)
satisfyXStream dvs es ps = do
        -- satTraceM ("satisfy enter: " ++ ppString (dvs, ps))
-- it is not clear if applying the substitution here wins or not
        s0 <- getSubst
        SolveResult rs sbs s <- satisfy' dvs (apSub s0 es) (apSub s0 ps)
--      SolveResult rs sbs s <- satisfy' dvs es ps
        -- satTraceM ("satisfy exit: " ++ ppString (rs,sbs,s) ++ "\n")
        extSubst "satisfyX" s
        return $ (rs, apSub s sbs)

-- break up preds into those affected by a substitution and those not
-- in order to loop more efficiently in satisfy'
split_rs :: Types t => Subst -> [t] -> ([t], [t])
split_rs s' rs  = partition affected_pred rs
          -- should only be a few since classes are small
          -- and the substitution comes from a single reducePred
          -- or joinCtx (at the end)
    where changed_tv = getSubstDomain s'
          affected_var v = v `elem` changed_tv
          affected_pred r = any affected_var (tv r)

satisfy' :: DVS -> Givens -> [VPred] -> TI SolveResult
satisfy' dvs es ps = do
       (ps0, s0, sbs0) <- joinNeededCtxs ps
       -- satTraceM ("satisfy (join)' = " ++ ppString (ps0, sbs0, s0))
       result <- sMany dvs es [] sbs0 s0 ps0
       -- satTraceM ("satisfy (result)' = " ++ ppString result)
       return result
  where
        -- sMany
        -- Arguments:
        --   dvs - dependent vars, when attempting "last resort" satisfying
        --   es - given preds, which can be assumed for satisfying other preds
        --   rs - preds which we tried to satisfy but couldn't
        --   bs - bindings resulting from satisfying
        --   s  - substitution resulting from satisfying
        --   ps - the predicates to try satisfying
        -- Try satisfying each pred in "ps" in turn.  If "sat" reduces the
        -- pred via a committable coherent instance match (no other instance
        -- and no in-scope given could ever satisfy it), we commit: keep the
        -- bindings and substitution, continue with the residual goals in
        -- place of the pred.  If "sat" cannot reduce it entirely and the
        -- reduction is not committable (an incoherent match, or a given may
        -- yet discharge it), we ignore the result of "sat" and put the
        -- original predicate aside (in "rs") to retry.  If "sat" can reduce
        -- the pred entirely, we add the bindings and substitution to the
        -- current result.  If
        -- the subst gives us new information about any of the unsatisfied
        -- preds ("rs") then we attempt to solve those again (put them back
        -- in "ps") in case progress can be made with the new information.
        --
        -- Note that we do allow partial results from numeric typeclasses
        -- (but only when not attempting "last resort" satisfying, though
        -- is that condition even necessary?).
        --
        sMany :: DVS -> Givens -> [VPred] -> SolvedBinds -> Subst -> [VPred] ->
                 TI SolveResult
        sMany dvs es rs sbs s [] =
            {- satTrace ("sMany: rs=" ++ ppReadable rs) $ -} do
            (rs', s', sbs') <- joinNeededCtxs rs
            let (changed_rs, unchanged_rs) = split_rs s' rs'
            if null changed_rs then do
                checkJoinCtxs "satisfy" rs s' rs'
                -- the loop is done; numeric residuals are NOT solved
                -- here -- they flow to the caller's settlement point
                -- (see the satisfy/satisfyStream contract above)
                return (SolveResult rs' (sbs' <++ sbs) (s' @@ s))
             else
                sMany (dvsSub s' dvs) (apSub s' es) unchanged_rs (sbs' <++ sbs) (s' @@ s) (apSub s' changed_rs)
        sMany dvs es rs sbs s (p:ps) = do
            SatResult x_needed x_sbs x_s x_commit _ <- sat dvs es p
            -- satTrace ("sMany: sat=" ++ ppReadable (p, x_needed)) $ return ()
            case x_needed of
                -- a committable partial reduction: the instance choice is
                -- final, so commit it rather than retrying from scratch.
                -- (The committed pred is NOT added to "es": discharging a
                -- later equal pred against its dictionary would create a
                -- binding that references an earlier-merged binding, which
                -- the SolvedBinds merge order forbids; later equal preds
                -- simply reduce through the same instance again.)
                needed@(_:_) | x_commit == Committable && not (vpIsPreClass p) ->
                    let (changed_rs, unchanged_rs) = split_rs x_s rs
                    in  sMany (dvsSub x_s dvs) (apSub x_s es) unchanged_rs
                              (x_sbs <++ sbs) (x_s @@ s)
                              (apSub x_s (needed ++ changed_rs ++ ps))
                -- if the predicate failed to reduce away completely,
                -- put it aside and ignore its result,
                -- except when it's a numeric typeclass and we're not doing
                -- "last resort" satisfying
                (_:_) | not (vpIsPreClass p) || isJust dvs ->
                    sMany dvs es (p:rs) sbs s ps
                new_ps ->
                    if isNullSubst x_s then
                        sMany dvs es (new_ps ++ rs) (x_sbs <++ sbs) s ps
                    else do
                        let (changed_rs, unchanged_rs) = split_rs x_s rs
                        sMany (dvsSub x_s dvs) (apSub x_s es) (new_ps ++ unchanged_rs) (x_sbs <++ sbs) (x_s @@ s) (apSub x_s (changed_rs ++ ps))

expTConPred :: VPred -> TI [VPred]
expTConPred v@(VPred _ (PredWithPositions (IsIn c ts) pos anc)) = do
        vsts <- mapM expTFun ts
        let (vss, ts') = unzip vsts
        return (vpWith v (PredWithPositions (IsIn c ts') pos anc): concat vss)

-- Build a class predicate from a fully-applied ATF.  Given the ATF's
-- class, param indices, target index, the ATF arguments, and the type
-- to place at the target position, fills in fresh vars for any class
-- params not covered by the ATF.
mkATFClassPred :: String -> Type -> Id -> [Int] -> Int -> [Type] -> Type
               -> TI (Class, [Type])
mkATFClassPred tag posType clsId pIdxs tIdx atfArgs targetType = do
    cls <- findCls (CTypeclass clsId)
    let nParams = length (csig cls)
    freshVars <- mapM (\tv -> newTVar tag (kind tv) posType) (csig cls)
    let classArgs = [ if idx == tIdx then targetType
                      else case elemIndex idx pIdxs of
                            Just j  -> atfArgs !! j
                            Nothing -> freshVars !! idx
                    | idx <- [0..nParams-1] ]
    return (cls, classArgs)

-- Compute the transitive incoherence closure on a fully-merged SolvedBinds,
-- emitting a warning or error for each binding that becomes transitively incoherent.
-- Behaviour depends on the class annotation (allowIncoherent) of the depending bind:
--   incoherent (Just True)  -- suppress: the class explicitly allows incoherence
--   coherent   (Just False) -- error T0158: coherence promise violated
--   default    (Nothing)    -- error T0158 if -incoherent-instance-matches is off
--                           -- warn  T0158 if -incoherent-instance-matches is on
-- Uses the stored DirectIncoherence info for accurate position and root-cause message.
warnTransitiveIncoherent :: SolvedBinds -> TI SolvedBinds
warnTransitiveIncoherent sbs = do
    let sbs' = computeTransitiveIncoherent sbs
        newlyIncoherent = getIncoherentIds sbs' `S.difference` getIncoherentIds sbs
    mapM_ (diagnoseOne (bindTypes sbs') (directIncoherences sbs') (bindClasses sbs'))
          (S.toList newlyIncoherent)
    return sbs'
  where
    diagnoseOne typeMap diMap clsMap i = do
      ai <- getAllowIncoherent
      case M.lookup i clsMap >>= allowIncoherent of
        Just True -> return ()   -- incoherent class: suppress
        mallow    ->
          let msg = (pos, WTransitiveIncoherentMatch tStr rootPredStr rootInstStr)
          in  if fromMaybe ai mallow then twarn msg else err msg
      where
        t  = fromJustOrErr "warnTransitiveIncoherent: id not in bindTypes"
                           (M.lookup i typeMap)
        di = fromJustOrErr "warnTransitiveIncoherent: id not in directIncoherences"
                           (M.lookup i diMap)
        pos = diPos di
        (rootPredStr, rootInstStr) = let (np, ni) = niceTypes (diPred di, diInst di)
                                     in (pfpString np, pfpString ni)
        tStr = pfpString t

-- expand all type functions
expTFun :: Type -> TI ([VPred], Type)
-- A normal type (CType.isNormalType) has no TIatf head and no Id__
-- leaf anywhere, so the equations below would only recurse and rebuild
-- an equal value; the newTVar/mkVPred effects need an ATF head, so
-- the equations below perform none on such a type either.
expTFun t0 | isNormalType t0 = return ([], t0)
-- Type function application: try to generate a class constraint so
-- the result gets bound when the class is resolved.
expTFun t0
  | let (f, as) = splitTAp t0,
    TCon (TyCon _ _ (TIatf { atf_class_id = clsId
                           , atf_param_idxs = pIdxs
                           , atf_target_idx = tIdx })) <- f,
    -- Don't attempt to expand partially applied type functions.
    length as == length pIdxs = do
        cls <- findCls (CTypeclass clsId)
        v <- newTVar "expTFun" (kind (csig cls !! tIdx)) t0
        (_, classArgs) <- mkATFClassPred "expTFun" t0 clsId pIdxs tIdx as v
        vps <- mkVPred (getPosition t0) $ mkPredWithPositions [] (IsIn cls classArgs)
        return (vps, v)
-- eliminate the identity type constructor.
expTFun (TAp (TCon (TyCon idId_ _ _)) t) | idId_ == idId = return ([], t)
expTFun (TAp f a) = do
        (vfs, f') <- expTFun f
        (vas, a') <- expTFun a
        return (vfs++vas, TAp f' a')
expTFun t = return ([], t)

-- This is faster than the old joinCtxs in the pathological case
-- where no contexts are found because we discover that with
-- an O (n log n) sort, instead of an O(n^2) scan
-- We sort by the class, the fundep pattern used and the compressed tyvars
-- More complex, so possibly less efficient than scanning
-- when a suitable match is found early in the list
-- Hopefully a good trade-off
-- XXX For efficiency, we should "join in parallel",
-- i.e. attempt to merge possible substitutions
-- so that we can handle more than one duplicate pred at once
joinCtxs :: [TyVar] -> [VPred] -> Maybe ([VPred], Subst, SolvedBind)
joinCtxs bound_tyvars vps = fmap (\ (rs, s, sb, _) -> (rs, s, sb)) (joinCtxsN bound_tyvars vps)

-- joinCtxs, with the numbers the bound and the used dictionary names
-- were drawn with (vpNum)
joinCtxsN :: [TyVar] -> [VPred] -> Maybe ([VPred], Subst, SolvedBind, (Int, Int))
joinCtxsN bound_tyvars vps = fmap fst (joinCtxsNG bound_tyvars vps)

-- A goal of the join rounds.  Its blobs are, per fundep row of its class,
-- the row's sort key (the class, the row, the types at the row's
-- undetermined positions) beside the goal.
class JoinGoal g where
    goalPred :: g -> VPred
    goalBlobs :: g -> [((Class, Int, [Type]), VPred)]
    toGoal :: VPred -> g

instance JoinGoal VPred where
    goalPred = id
    goalBlobs = predBlobs
    toGoal = id

-- A goal with its blobs, built lazily and once: the rounds after a join
-- carry over the goals it left unchanged, which keep their blobs instead
-- of being keyed again by every round.  A key holds the goal's own class
-- and type objects and building it forces no type, so the sort compares
-- the same objects in the same order as with keys built afresh.
-- Its "tv" is kept the same way, for every round's split: a scan
-- reading further than earlier rounds did makes the comparisons a fresh
-- list would make there, and the cells before only repeat comparisons
-- already made on the same type variables.
data JGoal = JGoal VPred [((Class, Int, [Type]), VPred)] [TyVar]

instance JoinGoal JGoal where
    goalPred (JGoal vp _ _) = vp
    goalBlobs (JGoal _ bs _) = bs
    toGoal vp = JGoal vp (predBlobs vp) (tv vp)

instance Types JGoal where
    apSub s (JGoal vp _ _) = toGoal (apSub s vp)
    tv (JGoal _ _ vs) = vs

{-# INLINE predBlobs #-}
predBlobs :: VPred -> [((Class, Int, [Type]), VPred)]
predBlobs vp@(VPred _ (PredWithPositions (IsIn c ts) _ _)) =
    [ ((c, n, boolCompress (map not bs) ts), vp) | (n, bs) <- zip [0..] (funDeps c) ]

-- joinCtxsN, with the joined group: its fundep row and its members, in
-- the order the group lists them (the order of "vps", the sort being stable)
{-# SPECIALIZE joinCtxsNG :: [TyVar] -> [VPred] ->
      Maybe (([VPred], Subst, SolvedBind, (Int, Int)), (Int, [VPred], VPred, VPred)) #-}
{-# SPECIALIZE joinCtxsNG :: [TyVar] -> [JGoal] ->
      Maybe (([JGoal], Subst, SolvedBind, (Int, Int)), (Int, [VPred], VPred, VPred)) #-}
joinCtxsNG :: JoinGoal g => [TyVar] -> [g] ->
              Maybe (([g], Subst, SolvedBind, (Int, Int)), (Int, [VPred], VPred, VPred))
joinCtxsNG bound_tyvars vps = listToMaybe (mapMaybe matchBlobs joined_blob_list)
  where blob_list = [ b | g <- vps, b <- goalBlobs g ]
        joined_blob_list = joinByFst blob_list
        matchBlobs ((_,n,_), gvs) =
            fmap (\ (r, (v1, v2)) -> (r, (n, gvs, v1, v2))) (joinInGroup bound_tyvars vps n gvs)

-- uniquePairs because we need to try every possible pair of matching VPreds
joinInGroup :: JoinGoal g => [TyVar] -> [g] -> Int -> [VPred] ->
               Maybe (([g], Subst, SolvedBind, (Int, Int)), (VPred, VPred))
joinInGroup bound_tyvars vps n gvs =
    listToMaybe $ mapMaybe (\ (v1, v2) -> fmap (\ r -> (r, (v1, v2)))
                                                (joinPair bound_tyvars vps n v1 v2)) $
        uniquePairs gvs

joinPair :: JoinGoal g => [TyVar] -> [g] -> Int -> VPred -> VPred ->
            Maybe ([g], Subst, SolvedBind, (Int, Int))
joinPair bound_tyvars vps n v1@(VPred i pp@(PredWithPositions (IsIn c ts) pos anc))
                            v2@(VPred i' pp'@(PredWithPositions (IsIn _ ts') pos' anc')) = do
          let bs = funDeps c !! n
          -- we could try to capture and use numeric equalities here
          -- but that would require being on the TI monad,
          -- which doesn't seem worth it
          (s, []) <- mgu bound_tyvars (boolCompress bs ts) (boolCompress bs ts')
          let p'' = vpWith v2 (PredWithPositions (IsIn c ts') (pos' ++ pos) (anc' ++ anc))
              -- (these comparisons intern the names; a pool hit makes
              -- them again, in this order: joinForce)
              rs  = toGoal p'':[ g | g <- vps, residual (goalPred g) ]
              residual (VPred j _) = j /= i && j /= i'
              pr = removePredPositions pp
              b = (i, predToType (apSub s pr), CVar i')
              sb = mkSolvedBind b False -- Joining equivalent predicates, not recursive
          return (rs, s, sb, (vpNum v1, vpNum v2))

-- This function is successful if the list of VPred is empty, indicating
-- that the pred was fully satisfied.  In which case, the bindings and subst
-- can be used freely.
-- If the list of VPred is non-empty, that means that we were able to reduce
-- the given VPred to the list of preds, and the bindings indicate how we got
-- there.  The subst may still be useful b/c it includes what we learned from
-- functional dependencies whether or not the VPred is satisfied "all the way"
--
-- sat corresponds to section 7.2 "Entailment" in the paper
-- Typing Haskell in Haskell
--
-- The Commitment in the result is only meaningful when the returned
-- [VPred] is non-empty (a partial reduction): Committable means the
-- reduction came from a coherent instance match that no other instance
-- and no in-scope given could ever satisfy, so the caller may commit to
-- it (keep the bindings and continue with the residual goals) instead of
-- putting the original predicate aside to retry.
sat :: DVS -> Givens -> VPred -> TI SatResult
sat dvs ps p =
    satTrace ("sat: trying " ++ ppReadable p ++ " in " ++ ppReadable (givensList ps)) $ do
    countDP CSat 1
    recLog (case toPred p of IsIn c ts -> EvGoal c ts)
    whole_stack <- getSatStack
    stack_idx <- getSatStackIdx
    bound_tyvars <- getBoundTVs
    let IsIn p_cl p_tys = toPred p
        recordATFs s_final =
          let resolvedTys = apSub s_final p_tys
          in sequence_
               [ recordATFResult (tcon_name atfTC)
                                 [resolvedTys !! i | i <- paramIdxs]
                                 (resolvedTys !! targetIdx)
               | (atfTC, paramIdxs, targetIdx) <- assocTypes p_cl ]
    let lookfor_result :: Maybe (Bind, (Subst, [(Type,Type)]))
        -- I'm a little worried that matching against the stack
        -- will have lexical scoping issues where p will match against
        -- something very "old", that it should not be able to "see"
        -- due to shadowing or something.  We are probably OK because
        -- the TIMonad is reset for each top-level definition.
        -- (lookfor is given the stack's entries of the goal's class key,
        -- newest first, as with the givens below)
        lookfor_result = lookfor bound_tyvars p (classIdxLookup stack_idx (vpClassKey p))
    case lookfor_result of
      Just (b, (s, [])) -> recLog EvLookfor >>
         -- tie the recursive knot!
         -- traces ("recursive knot: " ++ (ppString p) ++ " = " ++ (ppString lookfor_result))$
         case p of
           (VPred vp (PredWithPositions (IsIn cl tys) poss _))
            | (name cl) == (CTypeclass idBits)
               -> err (makeERecursiveBitsErrorMessage poss tys cl)
                  {-
                  internalError (
                                   "cl: " ++ (ppString cl) ++
                                   "\n\ntys: " ++ (ppString tys) ++
                                   "\n\npos: " ++ (ppString pos) ++
                                   "\n\nlf: " ++ (ppString lookfor_result) ++
                                   "\n\nvp: " ++ (ppString vp) ++
                                   "\n\nvp tys: " ++ (pfpString tys) ++
                                    ""
                                  )
                                  -}
           -- if we satisfy numeric provisos recursively, we haven't proven it
           (VPred vp (PredWithPositions (IsIn cl _) poss _)) | isPreClass cl ->
                   return (SatResult [p] emptySBs nullSubst Provisional RcEmpty)
           _ -> satTrace ("sat recursive: " ++ ppReadable (p, b, s)) $
                let sb = mkSolvedBind b True -- Found on stack, recursive
                in return (SatResult [] (fromSB sb) s Provisional RcOpaque)
      _ -> do
       let this_point :: TSSatElement
           this_point = (mkTSSatElement dvs ps p)
       incrementSatStack this_point
       -- (lookfor compares the class of every entry with the goal's first
       -- and goes no further on another class, so it is given only the
       -- entries of the goal's class)
       GivenExp p_entries ps_trees <- return (givenExp p ps)
       let ps_exp = concatMap (concatMap flattenT . snd) ps_trees
       return_val <- case lookfor bound_tyvars p p_entries of
         Just (b, (s, [])) -> do
             recLog EvLookfor
             satTrace ("sat in super: " ++ ppReadable (p, ps_exp)) $ return ()
             let sb = mkSolvedBind b False -- Satisfied via superclass from source, not recursive
             return (SatResult [] (fromSB sb) s Provisional RcOpaque)
         -- we might introduce a numeric equality here, so try instance reduction first
         m_equals -> do
          -- if instance reduction fails, fall back to introducing an equality
          let fail msg =
                case m_equals of
                  Just (b, (s, num_eqs)) -> do
                    recLog EvLookfor
                    satTrace ("sat in super (num eq): " ++ ppReadable (p, ps_exp, num_eqs)) $ return ()
                    let VPred _ p_pwp = p
                    eq_ps <- concatMapM (eqToVPred (getPredAncestors p_pwp) (getVPredPositions p)) num_eqs
                    let sb = mkSolvedBind b False -- From superclass, not recursive
                    SolveResult rs sbs s' <- satMany (dvsSub s dvs) (apSub s ps) [] (fromSB sb) s eq_ps
                    return (SatResult rs sbs s' Provisional RcOpaque)
                  Nothing -> satTrace msg $ return (SatResult [p] emptySBs nullSubst Provisional RcEmpty)
          ai <- getAllowIncoherent
          -- the solve proper: instance reduction and the goals it leaves;
          -- the flag marks a full solve by a coherent match (the only
          -- result the dictionary pool records)
          let notFull m = do { r <- m; return (r, False) }
              solveFor = do
                  x  <- reducePred (givensList ps) dvs p
                  case x of
                    Nothing -> notFull (fail "sat unreduced")
                    Just (Reduction qs sb us Nothing mpkg ns) ->
                        satTrace ("sat calls satMany ") $ do
                        (result, rc) <- satManyR (dvsSub us dvs) (apSub us ps) [] (fromSB sb) (RcInst ns sb) us qs -- qs should have us applied already
                        case result of
                          SolveResult [] sbs s_final -> do
                            recLog (EvRoot mpkg)
                            recordPackageUse mpkg
                            recordATFs s_final
                            return (SatResult [] sbs s_final Provisional rc, True)
                          SolveResult needed sbs s_final -> do
                            -- A coherent match whose context is not yet satisfied:
                            -- under ordered-clause fundep semantics no other
                            -- instance can ever satisfy this predicate, so the
                            -- choice is final and the caller may commit to it --
                            -- unless an in-scope given (which is not an instance)
                            -- could still discharge the predicate whole after
                            -- refinement.
                            commitment <-
                              if legacyDeferInstances
                              then return Provisional
                              else do
                                stack_eps <- getExplGivens
                                let p_pred = apSub s_final (toPred p)
                                    -- the entries of concatMap bySuperE
                                    -- (ps ++ stack_eps) of the goal's
                                    -- class (predUnify compares the
                                    -- classes first), built afresh as
                                    -- that expansion was
                                    givens = freshGivenEntries p (givensAppend ps stack_eps)
                                    -- Modal check, so unify unguarded ([]): a
                                    -- given from an enclosing frame quantifies
                                    -- over its own rigid variables, which are
                                    -- distinct TyVars from (but instantiable
                                    -- to) this definition's; the bound-variable
                                    -- guards would hide such a given and let
                                    -- the commit freeze an instance reduction
                                    -- the outer given was meant to discharge
                                    -- whole.
                                    unifiable (EPred _ gp) =
                                        predUnify [] p_pred gp
                                    prov = any unifiable givens
                                -- (a given decided the verdict, and with it
                                -- the package use: no pool record keys it)
                                when prov $ recLog EvLookfor
                                return (if prov then Provisional else Committable)
                            when (commitment == Committable) $ recordPackageUse mpkg
                            return (SatResult needed sbs s_final commitment rc, False)
                    Just (Reduction qs sb us (Just (h@(IsIn c _))) mpkg _) | fromMaybe ai (allowIncoherent c) ->
                      satTrace ("sat calls satMany (incoherent) ") $ do
                      result0 <- satMany (dvsSub us dvs) (apSub us ps) [] (fromSB sb) us qs
                      -- An incoherent match is deliberately revocable until its
                      -- context is fully discharged (it must never adopt on
                      -- numeric debt), so render the verdict here: settle its
                      -- numeric residuals in a local batch before deciding
                      -- full-vs-partial.
                      result <- case result0 of
                        SolveResult needed0@(_:_) sbs s_final -> do
                            (needed, nsbs) <- batchSolveNumericPreds
                                                  (givensList (apSub s_final ps)) needed0
                            return (SolveResult needed (nsbs <++ sbs) s_final)
                        r -> return r
                      case result of
                        SolveResult ps@(_:_) sbs s_final -> return (SatResult ps sbs s_final Provisional RcOpaque, False)
                        SolveResult [] sbs s_final -> do
                          recordPackageUse mpkg
                          -- No recordATFs here: an incoherent match is an
                          -- information-dependent choice (context-, flag- and
                          -- import-order-dependent), so its ATF projection must
                          -- not be recorded where the .bo cache would deliver it
                          -- to importing compiles as if it were canonical.
                          let (vp_pred, inst_pred) = niceTypes (apSub s_final (toPred p, h))
                          let pos = getPosition $ getVPredPositions p
                          let VPred dictId _ = p
                              di = DirectIncoherence (apSub s_final (toPred p)) (apSub s_final h) pos
                              sbs' = addDirectIncoherence dictId di sbs
                          when (allowIncoherent c /= Just True) $
                            twarn (pos, WIncoherentMatch (pfpString vp_pred) (pfpString inst_pred))
                          return (SatResult [] sbs' s_final Provisional RcOpaque, False)
                    bad_match -> notFull (fail ("sat incoherent disallowed: " ++ ppReadable bad_match))
          mdp <- getDictPool
          case mdp of
            Just dp | not dictPoolOff ->
              poolSat dp ai dvs ps p whole_stack ps_trees bound_tyvars
                      recordATFs solveFor
            _ -> fmap fst solveFor
       decrementSatStack
       return return_val

-- ==============================
-- The dictionary pool
--
-- Within one package a coherent full solve of a predicate whose fundep
-- inputs are ground is the same wherever it recurs, up to the names it
-- draws, as long as nothing outside the solve takes part in it.  The
-- pool (DictPool, threaded through the package's definitions by
-- TypeCheck.tiDefns) records such a solve of a key -- the class, the
-- keys of the input positions (canonical words, or for a literal
-- type-level string already evaluated its intern id), and per output
-- position its key if ground, or else the rank of its variable's number
-- among the output variables' (the solve orders variables by number) -- and
-- answers a later occurrence of the key from the record:
--
--  * the counter that numbers generated names advances by the number of
--    draws the solve made (nested hits included), so every name made
--    later in the definition keeps the number it has without the pool;
--  * the bindings are the recorded ones, each dictionary the solve drew
--    renamed to the one the same draw makes here, built by the same tree
--    of fromSB and (<++) as the solve built them (DictPool.Recipe), so
--    whatever consumes them forces the same names in the same order, and
--    LiftDicts lifts the same dictionaries under the same names;
--  * the output variables are bound to the recorded output types, and
--    the package and ATF records of the solve are made again;
--  * the solve's joins (joinNeededCtxs', nested solves' included) are
--    made again where it made them: each join's residual filter compares
--    the names of the residual goals' dictionaries, which interns them,
--    so a hit makes the same comparisons in the same order on the names
--    the same draws make here (joinForce), and the intern table grows
--    as it would without the pool.  A solve with a join of a dictionary
--    not drawn in it is not recorded.
--
-- Recording forces no name: the bindings are read from the recipe kept
-- beside the SolvedBinds (never from the SolvedBinds, whose (<++) guards
-- force the names), and the dictionaries they bind and use are told by
-- the numbers they were drawn with (TIMonad.newDictN, carried by VPred),
-- which place them among the solve's draws.
--
-- Numeric classes are not pooled (the numeric solver resolves them
-- directly).  A solve is recorded only when it is a full solve by a
-- coherent match, met no goal from a given or the stack (nor one that a
-- given unifies with in the Commitment check), made no newVar
-- draw and no numeric-equality dependency, ran no satisfy in normT, has
-- only non-recursive, coherent, ground bindings closed over their own
-- names (Monad ActionValue excluded), recorded only ground ATF results,
-- and has no output variable among the last-resort variables.  A hit is
-- taken only when the stacks have room for the solve, no given, explicit
-- given or outer stack entry could meet a nested goal of the solve
-- (lookfor's equality on undetermined positions, decided on keys without
-- forcing, where a position without a key is no witness that they
-- differ), no such entry or last-resort variable holds an output
-- variable, no bound or last-resort variable has a number the solve
-- draws, and the output variables unify with the recorded types;
-- otherwise the predicate is solved as without the pool (and after
-- refuseLimit refusals by the context guard the key is not looked up
-- again).  Keys are
-- computed without forcing anything (DictPool.typeKey); a key that would
-- need a part not evaluated yet is not looked up.  A key whose solve
-- cannot be recorded for a reason that recurs is not tried again.
--
-- What a hit does not do that the solve would: force the solve's own
-- lazy results (the instance-variable lookups in its bindings' types and
-- evidence, byInst's recursion checks, nested ATF results), which a
-- consumer of the solve's results forces later.  The names the solve
-- itself forces are those of its joins, which a hit forces as above.  Under
-- -tc-dict-pool-verify the predicate is solved where a hit would be
-- taken, counting the solves that draw another number of names.

data KeyInfo = KeyInfo {
        kiKey :: PoolKey,
        kiOutVars :: [(Int, TyVar)],  -- output positions holding variables
        kiStr :: Bool,                -- a key used a string key
        kiTable :: KeyTable
    }

poolKey :: Bool -> KeyTable -> Bool -> DVS -> Class -> [Type] -> Either Cnt KeyInfo
poolKey alloc kt0 ai dvs c ts = poolKeyOf ai dvs c ts (typeKeys alloc kt0 ts)

-- The keys of the types, whether one used a string key, and the table.
typeKeys :: Bool -> KeyTable -> [Type] -> ([PK], Bool, KeyTable)
typeKeys _ kt [] = ([], False, kt)
typeKeys alloc kt (t:rest) = let (k, s1, kt') = typeKey alloc kt t
                                 (ks, s2, kt'') = typeKeys alloc kt' rest
                             in  (k : ks, s1 || s2, kt'')

-- poolKey, from the types' keys
poolKeyOf :: Bool -> DVS -> Class -> [Type] -> ([PK], Bool, KeyTable) -> Either Cnt KeyInfo
poolKeyOf ai dvs c ts (pks, str, kt1) =
    case [ (r, bs) | (r, bs) <- zip [0..] (funDeps c), inputsOK bs ] of
      [] | any (== PUneval) pks -> Left CSkipUnevaluated
         | any (== PNotNormal) pks -> Left CSkipNotNormal
         | otherwise -> Left CSkipNoRow
      ((r, bs) : _) -> do
        outs <- mapM output [ (i, t, k) | (i, True, t, k) <- zip4 [0..] bs ts pks ]
        let vars = [ (i, v) | (i, Just v) <- outs ]
            varNums = nub [ n | (_, TyVar _ n _) <- vars ]
            code _ (False, k) = Left (keyOf k)
            code i (True, k) = case lookup i outs of
                                 Just (Just (TyVar _ n _)) ->
                                     Right (length (filter (< n) varNums))
                                 _ -> Left (keyOf k)
            key = (typeclassId (name c), ai, isJust dvs, r,
                   zipWith code [0..] (zip bs pks))
        return (KeyInfo key vars str kt1)
  where
    isKey (PKey _) = True
    isKey _ = False
    keyOf (PKey k) = k
    keyOf _ = internalError "poolKey: no key"
    inputsOK bs = and [ isKey k | (False, k) <- zip bs pks ]
    output (i, t, k) = case k of
        PKey _ -> Right (i, Nothing)
        PVar _ -> case evalTVar t of
                    Just v@(TyVar _ n _) | n >= 0 -> Right (i, Just v)
                                         | otherwise -> Left CSkipOther
                    Nothing -> Left CSkipPartialOut
        PUneval -> Left CSkipUnevaluated
        PNotNormal -> Left CSkipNotNormal
    evalTVar t = case t of
        TVar v | isEvaluated t -> Just v
        _ -> Nothing

-- reducePred's normT forces the predicate's types to weak head normal
-- form in order, but walks a type that is not normal (expanding synonyms
-- and type functions, satisfying associated-type goals) before it forces
-- the next.  The pool forces them in that order as far as they can be
-- key positions: each type is forced, and the next only after a key or a
-- type variable (on which normT does nothing more).  A predicate with
-- any other type has no key, so no lookup is lost.  The keys (typeKeys)
-- are those of the walk, each forced type's taken after its forcing, and
-- the unforced rest's computed when asked for.
forceKeys :: KeyTable -> [Type] -> ([PK], Bool, KeyTable)
forceKeys kt ts0 = case go ts0 of (ks, s) -> (ks, s, kt)
  where
    go [] = ([], False)
    go (t : rest) =
      t `seq` case typeKey False kt t of
        (k, s, _)
          | keyable k t -> case go rest of (ks, s') -> (k : ks, s || s')
          | otherwise -> let (ks, s', _) = typeKeys False kt rest
                         in  (k : ks, s || s')
    keyable (PKey _) _ = True
    keyable (PVar _) (TVar _) = True
    keyable _ _ = False

poolSat :: DictPool -> Bool -> DVS -> Givens -> VPred -> [EPred] -> [(GSeg, [ETree])]
        -> [TyVar] -> (Subst -> TI ()) -> TI (SatResult, Bool) -> TI SatResult
poolSat dp ai dvs ps p@(VPred w (PredWithPositions (IsIn c ts) _ _)) whole_stack gtrees bvs
        recordATFs solveFor
  | isPreClass c = countDP CNumericSkip 1 >> plain
  | otherwise = do
      let kt = dpKeyTab dp
      keys@(pks, _, _) <- return $! forceKeys kt ts
      -- (the goal's sat-stack entry has the same types, for the hit guards
      -- of the goals its solve meets)
      setTopSatKeys (GKeys (keyTableVersion kt) pks)
      case poolKeyOf ai dvs c ts keys of
        Left why -> countDP why 1 >> plain
        Right ki -> do
          countDP CLookup 1
          case M.lookup (kiKey ki) (dpKeys dp) of
            Just (KNoRec n why) | n >= noRecTries -> do
              countDP CSkipNoRec 1
              countDPAt (noRecByBase + fromEnum why) 1
              plain
            Just (KEntry e) | eRefused e >= refuseLimit -> do
              countDP CSkipRefused 1
              plain
            Just (KEntry e) -> do
              g <- hitGuards dp e ki dvs whole_stack gtrees bvs
              case g of
                Left CFallbackContext -> do
                  countDP CFallbackContext 1
                  refused (kiKey ki) e
                  plain
                Left why -> countDP why 1 >> plain
                Right (c0, satSz, superSz, s_hit)
                  | dictPoolVerify -> verifyHit c e c0
                  | otherwise ->
                      replayHit c e ki w c0 satSz superSz s_hit recordATFs
            _ -> do
              countDP CMissNoEntry 1
              recordSolve ai dvs c ts w ki solveFor
  where
    plain = fmap fst solveFor
    -- the context guard refused a lookup of the key
    refused key e = do
      mdp' <- getDictPool
      case mdp' of
        Just dp' -> setDictPool (dp' { dpKeys = M.insert key (KEntry e { eRefused = eRefused e + 1 })
                                                         (dpKeys dp') })
        Nothing -> return ()
    -- the solve, where a hit would be taken: count one that draws another
    -- number of names than the record
    verifyHit cl e c0 = do
      r <- plain
      c1 <- getTyVarCounter
      if c1 - c0 == eDraws e
        then countDP CVerifyOk 1
        else do countDP CVerifyDraws 1
                traceM ("dict pool verify: " ++ pfpString (name cl) ++
                        " draws " ++ show (c1 - c0) ++ " recorded " ++
                        show (eDraws e) ++ " at " ++ show c0)
      return r

hitGuards :: DictPool -> Entry -> KeyInfo -> DVS -> [EPred] -> [(GSeg, [ETree])] -> [TyVar]
          -> TI (Either Cnt (Int, Int, Int, Subst))
hitGuards dp e ki dvs whole_stack gtrees bvs = do
    (satSz, superSz) <- getSatStackSizes
    flags <- getFlags
    c0 <- getTyVarCounter
    -- normT inside the solve satisfies type-function goals from these
    stack_segs <- fmap givensSegs getExplGivens
    -- (the goal's own entry is on top of the whole_stack's)
    own_keys <- getSatKeys
    let lim = maxTIStackDepth flags
        outNums = [ n | (_, TyVar _ n _) <- kiOutVars ki ]
        dvsOK = case dvs of
                  Nothing -> True
                  Just vs -> null outNums || all (\ (TyVar _ n _) -> n `notElem` outNums) vs
        outTys = [ fromJustOrErr "hitGuards: output" (lookup i (eOuts e))
                 | (i, _) <- kiOutVars ki ]
        kt = dpKeyTab dp
        clear = goalsClear kt (eGoals e) outNums c0
        (top_key, stack_keys) = case own_keys of
                                  (k : ks) -> (k, ks)
                                  [] -> internalError "hitGuards: sat keys"
        (stackOK, updates) = stackClear kt (eGoals e) outNums c0 whole_stack stack_keys
        -- the entries of the givens' expansions (sat's trees) and of the
        -- explicit preds' (as concatMap bySuperE builds them), a GRigid
        -- level's as guardEntries reads them
        givens_lab = concat [ guardEntries kt seg ets | (seg, ets) <- gtrees ]
        expl_lab = concat [ guardEntries kt seg (map bySuperT (gsList seg)) | seg <- stack_segs ]
    if not (satSz + eMaxSat e < lim && superSz + eMaxSuper e < lim)
      then return (Left CFallbackDepth)
      else do
        let ok = dvsOK && stackOK && clear (map snd givens_lab) && clear (map snd expl_lab)
        -- keys computed for the stack's entries are kept for later guards
        when (dvsOK && not (null updates)) $
          setSatKeys (top_key : applyKeyUpdates updates stack_keys)
        when (traceDictPool && not ok) $ do
          let (cnt, why) = refusalCnt kt (eGoals e) outNums c0 dvsOK stackOK givens_lab expl_lab
          countDP cnt 1
          when traceDictPoolEntries $
            traceM ("tc dict pool refuse " ++ show (eId e) ++ " " ++ why)
        if not ok
          then return (Left CFallbackContext)
          else do
            -- (none of the solve's draws is a bound or last-resort variable
            -- already, which every guard above takes for granted)
            bvss <- getBoundTVStack
            if not (freshVars c0 dvs bvss)
              then return (Left CFallbackFresh)
              else case mgu bvs (map (TVar . snd) (kiOutVars ki)) outTys of
                     Just (s, []) -> return (Right (c0, satSz, superSz, s))
                     _ -> return (Left CFallbackUnify)

-- No variable of dvs or the bound stack has a number from c0 on, which
-- the solve's draws take.  (The stack is walked by level: the
-- concatenation getBoundTVs gives would be built for each hit.)
freshVars :: Int -> DVS -> [[TyVar]] -> Bool
freshVars c0 dvs bvss = all older (fromMaybe [] dvs) && all (all older) bvss
  where older v = maybe True (< c0) (genTyVarNum v)

-- No entry of a goal class among the givens and outer stack entries can
-- meet a nested goal of the solve through lookfor (whose matchTop under
-- equality needs, for some fundep row, the undetermined positions
-- equal), and none holds an output variable or a variable the solve's
-- draws could number.  A position with a type variable never equals a
-- nested goal's (whose variables are the solve's own or the outputs).
goalsClear :: KeyTable -> GoalSum -> [Int] -> Int -> [EPred] -> Bool
goalsClear kt gs outNums c0 eps = M.null gs || all clear eps
  where
    clear (EPred _ (IsIn c ts)) =
      case M.lookup (typeclassId (name c)) gs of
        Nothing -> True
        Just rows -> predClear outNums c0 c rows (fst3 (typeKeys False kt ts))

-- The entries of a level's expansion that the hit guard keys, each
-- labelled (a given of the level, a GRigid level): a GFlex level's
-- expansion (its trees, ets) as the solve reads it.  A GRigid level's
-- substitutions give its next levels (rigidChain), which the solve's
-- nested sats read; the guard keys the level's givens and the expansions
-- of the level and of those it becomes.  Where every type of the list the
-- level was made with is keyed PKey or PVar without forcing (stableNF:
-- evaluated, normal apart from type variables, so no synonym or type
-- function for expandSyn or normTAp to rewrite), they are all the same
-- value, so are their expansions, and the guard keys the last one's
-- alone: the one a level's sats force, through lookfor, from the second
-- substitution on.
guardEntries :: KeyTable -> GSeg -> [ETree] -> [((Bool, Bool), EPred)]
guardEntries kt seg ets = case gsBound seg of
    GFlex -> concatMap (labT False) ets
    GRigid _ _ _ raw ->
      let chain = seg : rigidChain seg
          keyed | stableNF kt raw = [last chain]
                | otherwise = chain
      in  [ ((True, True), ep) | ep <- gsList seg ] ++
          [ ((False, True), ep) | s' <- keyed, et <- segTrees s', ep <- flattenT et ]
  where
    labT rigid (ETree ep subs comm) =
        ((True, rigid), ep) :
        [ ((False, rigid), ep') | et <- subs, ep' <- flattenT et ] ++
        [ ((False, rigid), ep') | ep' <- comm ]

-- Whether the list's spine, givens and type lists are evaluated and every
-- type is keyed PKey or PVar (DictPool.typeKey), read without forcing.
stableNF :: KeyTable -> [EPred] -> Bool
stableNF kt eps = maybe False (all ok) (listNF eps)
  where
    ok ep
      | isEvaluated ep, EPred _ p <- ep, isEvaluated p, IsIn _ ts <- p,
        Just ts' <- listNF ts = all keyed ts'
      | otherwise = False
    keyed t = case typeKey False kt t of
                (PKey _, _, _) -> True
                (PVar _, _, _) -> True
                _ -> False
    listNF xs
      | not (isEvaluated xs) = Nothing
      | otherwise = case xs of
          [] -> Just []
          (x : rest) -> (x :) <$> listNF rest

-- The counter of a refusal by the context guard (-trace-tc-dict-pool) and a
-- description (-trace-tc-dict-pool-entries): the first part that is not
-- clear, and for an entry of the givens' or explicit preds' expansions,
-- whether it is a given or an expansion entry, whether its level is
-- GRigid, and whether it has a PUneval key.  (Keys are computed as
-- goalsClear's, forcing nothing.)
refusalCnt :: KeyTable -> GoalSum -> [Int] -> Int -> Bool -> Bool
           -> [((Bool, Bool), EPred)] -> [((Bool, Bool), EPred)] -> (Cnt, String)
refusalCnt kt gs outNums c0 dvsOK stackOK givens_lab expl_lab
  | not dvsOK = (CRefDvs, "dvs")
  | not stackOK = (CRefStack, "stack")
  | (b@((root, rigid), _, ks) : _) <- blocks givens_lab =
      (case (root, rigid, any (== PUneval) ks) of
        (True, True, True) -> CRefRootRigidUneval
        (True, True, False) -> CRefRootRigidKeyed
        (True, False, True) -> CRefRootFlexUneval
        (True, False, False) -> CRefRootFlexKeyed
        (False, True, True) -> CRefSuperRigidUneval
        (False, True, False) -> CRefSuperRigidKeyed
        (False, False, True) -> CRefSuperFlexUneval
        (False, False, False) -> CRefSuperFlexKeyed, "given " ++ desc b)
  | (b@((_, rigid), _, ks) : _) <- blocks expl_lab =
      (if not (any (== PUneval) ks) then CRefExplKeyed
       else if rigid then CRefExplRigidUneval else CRefExplFlexUneval, "expl " ++ desc b)
  | otherwise = (CRefExplKeyed, "none")
  where
    blocks les = [ (l, c, ks)
                 | (l, EPred _ (IsIn c ts)) <- les,
                   Just rows <- [M.lookup (typeclassId (name c)) gs],
                   let ks = fst3 (typeKeys False kt ts),
                   not (predClear outNums c0 c rows ks) ]
    desc ((root, rigid), c, ks) =
        (if rigid then "rigid " else "flex ") ++ (if root then "root " else "exp ") ++
        pfpString (name c) ++ " " ++ show ks

-- goalsClear over the sat stack, whose entries' keys are kept from the
-- goal's own lookup (poolSat) or an earlier guard, and computed again
-- only where they may have changed (gkValid).  With the verdict, the
-- keys computed (by position in the stack), which are what a later
-- guard computes while they stay valid.
stackClear :: KeyTable -> GoalSum -> [Int] -> Int -> [EPred] -> [GKeys]
           -> (Bool, [(Int, GKeys)])
stackClear kt gs outNums c0 eps0 gks0
  | M.null gs = (True, [])
  | otherwise = go 0 eps0 gks0 []
  where
    ver = keyTableVersion kt
    go _ [] _ ups = (True, ups)
    go i (EPred _ (IsIn c ts) : eps) gks ups =
      let (gk, gks') = case gks of
                         (k : ks) -> (k, ks)
                         [] -> internalError "stackClear: keys"
      in  case M.lookup (typeclassId (name c)) gs of
            Nothing -> go (i + 1) eps gks' ups
            Just rows ->
              let (pks, ups') = case gk of
                    GKeys v ks | gkValid ver v ks -> (ks, ups)
                    _ -> let ks = fst3 (typeKeys False kt ts)
                         in  (ks, if gkValid ver ver ks then (i, GKeys ver ks) : ups
                                  else ups)
              in  if predClear outNums c0 c rows pks
                    then go (i + 1) eps gks' ups'
                    else (False, ups')

-- The stack's keys with the updates (by position, last first) made.
applyKeyUpdates :: [(Int, GKeys)] -> [GKeys] -> [GKeys]
applyKeyUpdates ups0 gks0 = go 0 (reverse ups0) gks0
  where
    go _ [] gks = gks
    go i ups@((j, k) : rest) (g : gks)
      | i == j = let gks' = go (i + 1) rest gks in gks' `seq` (k : gks')
      | otherwise = let gks' = go (i + 1) ups gks in gks' `seq` (g : gks')
    go _ _ [] = internalError "applyKeyUpdates"

-- One given or stack entry of class c, whose types have keys pks, can
-- meet no nested goal of the solve (see goalsClear).  A row clears on a
-- witness that the positions differ: a type variable, or keys that no
-- nested goal had.  A position without a key is no witness: it can hold
-- an output variable (a given with flexible variables, such as a
-- polymorphic field's proviso), which varsOK sees only in a PVar key.
predClear :: [Int] -> Int -> Class -> IM.IntMap (S.Set [Int], Bool) -> [PK] -> Bool
predClear outNums c0 c rows pks = varsOK && all rowClear (IM.toList rows)
  where
    varsOK = and [ n < c0 && n `notElem` outNums | PVar ns <- pks, n <- ns ]
    rowClear (r, (set, unknown)) =
      let und = [ k | (False, k) <- zip (funDeps c !! r) pks ]
      in  any isPVar und ||
          (all isPKey und && not unknown && not (S.member [ k | PKey k <- und ] set))

isPVar :: PK -> Bool
isPVar (PVar _) = True
isPVar _ = False

isPKey :: PK -> Bool
isPKey (PKey _) = True
isPKey _ = False

replayHit :: Class -> Entry -> KeyInfo -> Id -> Int -> Int -> Int -> Subst
          -> (Subst -> TI ()) -> TI SatResult
replayHit c e ki w c0 satSz superSz s_hit recordATFs = do
    advanceTyVarCounter (eDraws e)
    -- (one name per slot, shared by the bindings that use it)
    let ids = IML.fromList [ (k, dictIdFor (c0 + k)) | k <- tmplSlots (eTmpl e) ]
        nm SRoot = w
        nm (SDict k) = case IML.lookup k ids of
                         Just i -> i
                         Nothing -> internalError "replayHit: slot"
    recLog (EvSub (emptySummary { smGoals = eGoals e }))
    bumpRecMaxima (satSz + eMaxSat e) (superSz + eMaxSuper e)
    ropeMapM_ (if eJoined e then replayJoined (c0 - eBase e) else replayEv)
              (eEvents e)
    recordPackageUse (eRootPkg e)
    recordATFs s_hit
    countDP (if kiStr ki then CHitStr else CHitWord) 1
    when (not (null (kiOutVars ki))) $ countDP CHitVarOut 1
    countDP CHitBinds (tmplLeaves (eTmpl e))
    countDP CHitDraws (eDraws e)
    when traceDictPoolEntries $
      traceM ("tc dict pool hit " ++ show (eId e) ++ " " ++ pfpString (name c))
    return (SatResult [] (rebuild nm (eTmpl e)) s_hit Provisional (RcPool c0 (eTmpl e)))

-- A record of a solve made again by a hit (replayJoined for an entry
-- with joins, whose names are drawn "off" after the record's).
replayEv :: ReplayEv -> TI ()
replayEv (RATF i as r) = recordATFResult i as r
replayEv (RPkg pkg) = recordPackageUse (Just pkg)
replayEv (RJoin _ _ _) = internalError "replayEv: join"

replayJoined :: Int -> ReplayEv -> TI ()
replayJoined off (RJoin a b ns) = do
    let drawn n = dictIdFor (n + off)
    () <- return $! joinForce (drawn a) (drawn b) (map drawn ns)
    countDP CHitJoins 1
    logJoin (a + off) (b + off) (map (+ off) ns)
replayJoined _ ev = replayEv ev

-- The comparisons of a join's residual filter (joinCtxsN), made in its
-- order on names of the same spellings: the joined pair's and those of
-- the goals the filter runs over.  They force, so intern, the names.
joinForce :: Id -> Id -> [Id] -> ()
joinForce i i' js = length [ () | j <- js, j /= i && j /= i' ] `seq` ()

-- The SolvedBinds a recorded solve built, with its names given by "nm":
-- the same bindings, built by the same fromSB and (<++) tree, so that
-- whatever later forces it forces the same things in the same order.
rebuild :: (Slot -> Id) -> Tmpl -> SolvedBinds
rebuild _ TEmpty = emptySBs
rebuild nm (TLeaf s t et cls) =
    fromSB ((mkSolvedBind (nm s, t, inst et) False) { solvedClass = cls })
  where inst (ESlot s') = CVar (nm s')
        inst (EConst x) = x
        inst (EApp f as) = CApply (inst f) (map inst as)
rebuild nm (TMerge x y) = rebuild nm x <++ rebuild nm y
rebuild nm (TSub r k t) = rebuild nm' t
  where nm' SRoot = nm r
        nm' (SDict j) = nm (SDict (j + k))

-- A recording window's events, with its direct goals keyed (each event is
-- summarised once, in the innermost window around it).
summarize :: KeyTable -> [RecEv] -> (Summary, KeyTable)
summarize kt0 = go emptySummary kt0
  where
    go sm kt [] = (sm, kt)
    go sm kt (ev : evs) = case ev of
      EvVar -> go sm { smTaint = True, smTaintPerm = True } kt evs
      EvTaint -> go sm { smTaint = True, smTaintPerm = True } kt evs
      EvLookfor -> go sm { smTaint = True } kt evs
      EvATF i as r ->
        -- (this forces the ATF result's structure, not its names, which
        -- the solve leaves to the .bo writer)
        let g = not (any hasTVar as || hasTVar r)
        in  g `seq` go sm { smEvents = RCat (smEvents sm) (RLeaf (RATF i as r)),
                            smTaint = smTaint sm || not g,
                            smTaintPerm = smTaintPerm sm || not g } kt evs
      EvPkg pkg -> go sm { smEvents = RCat (smEvents sm) (RLeaf (RPkg pkg)) } kt evs
      EvJoin a b ns ->
        go sm { smEvents = RCat (smEvents sm) (RLeaf (RJoin a b ns)),
                smJoined = True } kt evs
      EvGoal gc gts ->
        let (pks, kt') = keyList kt gts
            gs' = addGoal gc pks (smGoals sm)
        in  M.size gs' `seq` go sm { smGoals = gs' } kt' evs
      EvSub sub -> go (catSummary sm sub) kt evs
      EvRoot _ -> go sm kt evs
    keyList kt [] = ([], kt)
    keyList kt (t : rest) = let (k, _, kt') = typeKey True kt t
                                (ks, kt'') = keyList kt' rest
                            in  (k : ks, kt'')

-- Add a goal (its class and position keys) to a goal summary.
addGoal :: Class -> [PK] -> GoalSum -> GoalSum
addGoal gc pks gs = M.insertWith (IM.unionWith merge) (typeclassId (name gc)) rows gs
  where
    rows = IM.fromList [ (r, one und) | (r, bs) <- zip [0..] (funDeps gc),
                                        let und = [ k | (False, k) <- zip bs pks ] ]
    one und
      | any isPVar und = (S.empty, False)
      | all isPKey und = (S.singleton [ k | PKey k <- und ], False)
      | otherwise = (S.empty, True)
    merge (x1, u1) (x2, u2) = (S.union x1 x2, u1 || u2)

recordSolve :: Bool -> DVS -> Class -> [Type] -> Id -> KeyInfo -> TI (SatResult, Bool)
            -> TI SatResult
recordSolve ai dvs c ts w ki solveFor = do
    c0 <- getTyVarCounter
    (sat0, super0) <- getSatStackSizes
    win <- openRecWindow
    r <- fmap Right solveFor `handle` (\ es -> return (Left es))
    (evs, msat, msuper) <- closeRecWindow win
    c_end <- getTyVarCounter
    outer <- recActive
    -- a window that met a given or the stack, or that the solve itself
    -- makes unreplayable, is left unread (the window around it inherits
    -- the taint); so is one with a join of a dictionary it did not draw,
    -- which a hit could not name (a nested window's joins are its own)
    let joinOut = any (\ ev -> case ev of
                                 EvJoin a b ns -> any (\ n -> n < c0 || n >= c_end) (a : b : ns)
                                 _ -> False) evs
        (taint0, perm0) = foldr (\ ev (t, p) -> case ev of
                                    EvVar -> (True, True)
                                    EvTaint -> (True, True)
                                    EvLookfor -> (True, p)
                                    EvSub sub -> (t || smTaint sub, p || smTaintPerm sub)
                                    _ -> (t, p)) (joinOut, joinOut) evs
        -- (for the counters) what made the window unreplayable
        permWhy = case [ y | ev <- evs, Just y <- [permOf ev] ] of
                    (y : _) -> y
                    [] -> if joinOut then CRecJoinOut else CNoRecTaint
        permOf EvVar = Just CNoRecVar
        permOf EvTaint = Just CNoRecTaint
        permOf (EvSub sub) | smTaintPerm sub = Just CNoRecNested
        permOf _ = Nothing
        fullRes = case r of
                    Right (res, True) -> Just res
                    _ -> Nothing
    when joinOut $ countDP CRecJoinOut 1
    recorded <-
      if taint0 && (outer || isJust fullRes)
        then do
          when outer $ recLog (if perm0 then EvTaint else EvLookfor)
          countDP CRecOpen 1
          when (isJust fullRes && perm0) $ noRec ki True permWhy
          return Nothing
        else do
          mdp <- getDictPool
          case mdp of
            Just dp0 | outer || isJust fullRes -> do
              -- the goals are keyed now, before anything later can
              -- evaluate more of them
              let (sm, kt1) = summarize (dpKeyTab dp0) evs
                  dp1 = dp0 { dpKeyTab = kt1 }
              () <- return $! (smTaint sm `seq` smTaintPerm sm `seq` kt1 `seq` ())
              setDictPool dp1
              let logSub sm' = when outer $ recLog (EvSub sm')
              case fullRes of
                Nothing -> logSub sm >> return Nothing
                Just res -> do
                  c1 <- getTyVarCounter
                  case buildEntry dp1 ai dvs c ts ki c0 c1 (msat - sat0) (msuper - super0)
                                  evs sm res of
                    Left why -> do
                      countDP why 1
                      noRec ki (why `elem` permanentCnts) why
                      logSub sm
                      return Nothing
                    Right (key, e, kt) -> do
                      () <- return $! forceEntry e
                      when traceDictPoolEntries $
                        traceM ("tc dict pool rec " ++ show (eId e) ++ " " ++
                                pfpString (name c) ++ " draws=" ++ show (eDraws e))
                      setDictPool (dp1 { dpKeys = M.insert key (KEntry e) (dpKeys dp1),
                                         dpKeyTab = kt,
                                         dpNext = dpNext dp1 + 1 })
                      countDP CRecOk 1
                      logSub sm
                      return (Just e)
            _ -> return Nothing
    case r of
      Left es -> countDP CRecError 1 >> throwError es
      Right (res, isFull) -> do
        when (not isFull) $ countDP CRecNotFull 1
        -- an enclosing record reads this solve's bindings from its entry
        return (case recorded of
                  Just e -> res { satRecipe = RcPool c0 (eTmpl e) }
                  Nothing -> res)

-- A key whose solve could not be recorded: for a reason that recurs
-- wherever the key does, it is not tried again; for another, it is
-- tried noRecTries times.
noRec :: KeyInfo -> Bool -> Cnt -> TI ()
noRec ki perm why = do
  mdp <- getDictPool
  case mdp of
    Just dp -> do
      let n = if perm then noRecTries else 1
          upd (Just (KNoRec a _)) = Just (KNoRec (a + n) why)
          upd _ = Just (KNoRec n why)
      setDictPool (dp { dpKeys = M.alter upd (kiKey ki) (dpKeys dp) })
      countDP CNoRecAdd 1
    Nothing -> return ()

noRecTries :: Int
noRecTries = 4

-- Lookups of a key that the context guard refuses this many times are no
-- longer made (a refused key is seldom met in a context that admits it;
-- a hit and a solve make the same result, so this changes only cost).
refuseLimit :: Int
refuseLimit = 8

permanentCnts :: [Cnt]
permanentCnts = [CRecRecursive, CRecIncoherent, CRecMonadAV, CRecShape, CRecShapePool,
                 CRecShapeRoot, CRecUnbound, CRecNonGround]

-- (The events are spine-strict and their types were walked by summarize;
-- a nested pooled solve's template was forced when it was recorded.)
forceEntry :: Entry -> ()
forceEntry e = eDraws e `seq` ft (eTmpl e) `seq`
               forceList (\ (i, t) -> i `seq` t `seq` ()) (eOuts e) `seq`
               M.size (eGoals e) `seq` ()
  where forceList f xs = foldr (\ x r -> f x `seq` r) () xs
        ft TEmpty = ()
        ft (TLeaf sl t ex cls) = sl `seq` t `seq` fe ex `seq` cls `seq` ()
        ft (TMerge x y) = ft x `seq` ft y
        ft (TSub sl k _) = sl `seq` k `seq` ()
        fe (ESlot sl) = sl `seq` ()
        fe (EConst x) = x `seq` ()
        fe (EApp f as) = fe f `seq` forceList fe as

-- The slots a template names, nested pooled solves' included (offset).
tmplSlots :: Tmpl -> [Int]
tmplSlots t0 = IS.toList (go 0 t0)
  where go _ TEmpty = IS.empty
        go k (TLeaf sl _ et _) = IS.union (slotSet k sl) (exprSlots k et)
        go k (TMerge x y) = IS.union (go k x) (go k y)
        go k (TSub sl j t) = IS.union (slotSet k sl) (go (k + j) t)

-- The bindings a hit on a template rebuilds.
tmplLeaves :: Tmpl -> Int
tmplLeaves TEmpty = 0
tmplLeaves (TLeaf _ _ _ _) = 1
tmplLeaves (TMerge x y) = tmplLeaves x + tmplLeaves y
tmplLeaves (TSub _ _ t) = tmplLeaves t

slotSet :: Int -> Slot -> IS.IntSet
slotSet k (SDict j) = IS.singleton (k + j)
slotSet _ SRoot = IS.empty

exprSlots :: Int -> ExprT -> IS.IntSet
exprSlots k (ESlot sl) = slotSet k sl
exprSlots _ (EConst _) = IS.empty
exprSlots k (EApp f as) = IS.unions (exprSlots k f : map (exprSlots k) as)

-- A full solve's entry.  Nothing here forces a name: the bindings come
-- from the recipe, whose leaves are read without the (<++) guards, and
-- the dictionaries they bind and use are told by their numbers.
buildEntry :: DictPool -> Bool -> DVS -> Class -> [Type] -> KeyInfo -> Int -> Int
           -> Int -> Int -> [RecEv] -> Summary -> SatResult
           -> Either Cnt (PoolKey, Entry, KeyTable)
buildEntry dp ai dvs c ts ki c0 c1 dsat dsuper evs sm (SatResult _ _ s_final _ rc)
  | smTaint sm = Left CRecOpen
  | otherwise = do
      tmpl <- toTmpl (Just SRoot) rc
      when (not (hasRoot tmpl)) $ Left CRecShapeRoot
      -- every name a binding refers to is bound (closed over its own
      -- names; a nested pooled solve is closed, and binds its root)
      when (not (IS.null (IS.difference (refsOf tmpl) (boundOf tmpl)))) $
        Left CRecUnbound
      -- the last-resort rules (StdPrel.checkDVS) can fire on an output in
      -- dvs, and a hit's solve has none there (hitGuards' dvsOK)
      when (not (null outNums) &&
            any (\ (TyVar _ n _) -> n `elem` outNums) (fromMaybe [] dvs)) $
        Left CRecOutDvs
      outs <- mapM (\ (i, v) -> (,) i <$> subT (TVar v)) (kiOutVars ki)
      KeyInfo { kiKey = key, kiTable = kt1 } <- poolKey True (dpKeyTab dp) ai dvs c ts
      let (before, rootPkg) = splitRoot evs
          events = foldl' addEv RNil before
      -- (the solve's own records join nothing and nest no solve)
      when (any joinOrSub (drop (length before) evs)) $ Left CRecShape
      return (key,
              Entry { eId = dpNext dp,
                      eBase = c0,
                      eDraws = c1 - c0,
                      eTmpl = tmpl,
                      eOuts = outs,
                      eEvents = events,
                      eJoined = any joined before,
                      eRootPkg = rootPkg,
                      eGoals = smGoals sm,
                      eMaxSat = max 0 dsat,
                      eMaxSuper = max 0 dsuper,
                      eRefused = 0 },
              kt1)
  where
    -- The record must not force a name the solve left unforced (forcing
    -- a type variable's name forces its position, which can intern a
    -- generated value name), and apSub's lookups compare names: so the
    -- results are substituted by variable number instead, the same types
    -- apSub makes (a number held by two substituted variables, or by
    -- none, is not substituted).  The types and evidence substituted are
    -- still the solve's own thunks (byInst's instance substitution, a
    -- join's), which this forces.
    numSub = IM.fromListWith (\ _ _ -> Nothing)
                             [ (n, Just t) | (TyVar _ n _, t) <- getSubstAssocs s_final, n >= 0 ]
    subNumT t
      | isNormalNode t = Just t
    subNumT (TVar (TyVar _ n _)) = case IM.lookup n numSub of
                                     Just (Just t) -> Just t
                                     _ -> Nothing
    subNumT (TAp l r) = normTAp <$> subNumT l <*> subNumT r
    subNumT t = Just t
    subNumE e = case e of
        CVar _ -> Just e
        CAny {} -> Just e
        CLit _ -> Just e
        CApply f es -> CApply <$> subNumE f <*> mapM subNumE es
        CTApply f ts' -> CTApply <$> subNumE f <*> mapM subNumT ts'
        CSelectT _ _ -> Just e
        CLitT t l -> (\ t' -> CLitT t' l) <$> subNumT t
        CAnyT pos uk t -> CAnyT pos uk <$> subNumT t
        CConT t i es -> CConT t i <$> mapM subNumE es
        CStructT t fs -> CStructT <$> subNumT t <*> mapM (\ (f, x) -> (,) f <$> subNumE x) fs
        _ -> Nothing
    hasTVarE e = case e of
        CApply f es -> hasTVarE f || any hasTVarE es
        CTApply f ts' -> hasTVarE f || any hasTVar ts'
        CLitT t _ -> hasTVar t
        CAnyT _ _ t -> hasTVar t
        CConT _ _ es -> any hasTVarE es
        CStructT t fs -> hasTVar t || any (hasTVarE . snd) fs
        _ -> False
    subT t0 = case subNumT t0 of
                Just t | not (hasTVar t) -> Right t
                _ -> Left CRecNonGround
    subE e0 = case subNumE e0 of
                Just e | not (hasTVarE e) -> Right e
                Just _ -> Left CRecNonGround
                Nothing -> Left CRecShape
    outNums = [ n | (_, TyVar _ n _) <- kiOutVars ki ]
    -- a dictionary drawn in the solve
    slotNum n | n >= c0 && n < c1 = Just (SDict (n - c0))
              | otherwise = Nothing
    shape = maybe (Left CRecShape) Right
    -- "root" is the slot of the dictionary the goal being read binds
    toTmpl _ RcEmpty = Right TEmpty
    toTmpl root (RcInst nums sb)
      | sbIsRecursive sb = Left CRecRecursive
      | isIncoherent sb = Left CRecIncoherent
      | otherwise = case sbBind sb of
        -- the evidence is the instance's expression (which names no
        -- dictionary of the solve) applied to the drawn dictionaries,
        -- whose thunks (byInst's recursion checks) are not evaluated here
        (_, t0, CApply f0 args) | length args == length nums -> do
          sl <- shape root
          t <- subT t0
          when (isMonadActionValueDict t) $ Left CRecMonadAV
          f <- subE f0
          as <- mapM (fmap ESlot . shape . slotNum) nums
          return (TLeaf sl t (EApp (EConst f) as) (solvedClass sb))
        _ -> Left CRecShape
    toTmpl _ (RcJoin a b sb) = case sbBind sb of
        (_, t0, CVar _) -> do
          sa <- shape (slotNum a)
          sb' <- shape (slotNum b)
          t <- subT t0
          when (isMonadActionValueDict t) $ Left CRecMonadAV
          return (TLeaf sa t (ESlot sb') (solvedClass sb))
        _ -> Left CRecShape
    toTmpl root (RcMerge x y) = TMerge <$> toTmpl root x <*> toTmpl root y
    toTmpl _ (RcGoal n x) = toTmpl (slotNum n) x
    toTmpl root (RcPool c0' t) = do
      sl <- maybe (Left CRecShapePool) Right root
      return (TSub sl (c0' - c0) t)
    toTmpl _ RcOpaque = Left CRecOpen
    hasRoot TEmpty = False
    hasRoot (TLeaf SRoot _ _ _) = True
    hasRoot (TLeaf _ _ _ _) = False
    hasRoot (TMerge x y) = hasRoot x || hasRoot y
    hasRoot (TSub sl _ _) = sl == SRoot
    boundOf TEmpty = IS.empty
    boundOf (TLeaf sl _ _ _) = slotSet 0 sl
    boundOf (TMerge x y) = IS.union (boundOf x) (boundOf y)
    boundOf (TSub sl _ _) = slotSet 0 sl
    refsOf TEmpty = IS.empty
    refsOf (TLeaf _ _ et _) = exprSlots 0 et
    refsOf (TMerge x y) = IS.union (refsOf x) (refsOf y)
    refsOf (TSub _ _ _) = IS.empty
    -- the solve's own records (after its own EvRoot) are made again by
    -- a hit from its own types
    splitRoot es = case break isRoot (reverse es) of
        (_, EvRoot mpkg : rest) -> (reverse rest, mpkg)
        _ -> (es, Nothing)
    isRoot (EvRoot _) = True
    isRoot _ = False
    addEv acc (EvATF i as x) = RCat acc (RLeaf (RATF i as x))
    addEv acc (EvPkg pkg) = RCat acc (RLeaf (RPkg pkg))
    addEv acc (EvJoin a b ns) = RCat acc (RLeaf (RJoin a b ns))
    addEv acc (EvSub sub) = RCat acc (smEvents sub)
    addEv acc _ = acc
    joinOrSub (EvJoin {}) = True
    joinOrSub (EvSub _) = True
    joinOrSub _ = False
    joined (EvJoin {}) = True
    joined (EvSub sub) = smJoined sub
    joined _ = False

-- Whether a type holds a type variable (comparing no names, as tv does).
hasTVar :: Type -> Bool
hasTVar t | isGroundNode t = False
hasTVar (TVar _) = True
hasTVar (TAp l r) = hasTVar l || hasTVar r
hasTVar _ = False

-- The Monad ActionValue dictionary, which LiftDicts never lifts.
isMonadActionValueDict :: Type -> Bool
isMonadActionValueDict t
  | (f, [arg]) <- splitTAp t =
      leftCon f == Just (idMonad noPosition) && leftCon arg == Just idActionValue
  | otherwise = False

makeERecursiveBitsErrorMessage :: [Position] -> [Type] -> Class -> EMsg
-- a somewhat hacky function which tries to come up with the best
-- error message possible
makeERecursiveBitsErrorMessage poss tys cl =
    let remove_tyvars :: Type -> Bool
        -- often TVars are compiler generated and not useful.
        remove_tyvars (TVar _) = False
        remove_tyvars _ = True
    in
    ((case poss of
            [] -> noPosition
            -- first position seems as good as any
            pos1:_ -> pos1 ),
         (ERecursiveBits
          (pfpString (name cl))
          (case (filter remove_tyvars tys) of
             [] -> pfpString tys
             [singleton] -> pfpString singleton
             list -> pfpString list)
         ))

-- make sure we've joined contexts "all the way"
checkJoinCtxs :: String -> [VPred] -> Subst -> [VPred] -> TI ()
checkJoinCtxs tag rs s rs' = do
  bound_tyvars <- getBoundTVs
  when (isJust (joinCtxs bound_tyvars rs')) $
    internalError ("incomplete join (" ++ tag ++ "): " ++ ppReadable (rs, rs', s))

-- Batched proviso SAT solving, run at the base of a satisfy loop
-- (sMany's and satMany''s base cases, so nested reductions inside
-- "sat" batch their own residuals too) over the whole numeric residual
-- set, instead of once per predicate per reduction attempt with a
-- fresh solver each time.  One fresh solver session serves each batch
-- (deliberately not kept between batches; see below), and each
-- predicate is proven from the definition's givens alone (see the XXX
-- below for the richer sibling-residual assumption scheme, which is
-- sound but parked).
-- Ground proven facts are immutable and are cached in the TI state,
-- so later passes discharge them without consulting the solver at
-- all.
batchSolveNumericPreds :: [EPred] -> [VPred] -> TI ([VPred], SolvedBinds)
batchSolveNumericPreds es rs = do
    flags <- getFlags
    let isNumVP (VPred _ (PredWithPositions (IsIn c _) _ _)) = isPreClass c
        nps = filter isNumVP rs
    if not (useProvisoSAT flags) || null nps
      then return (rs, emptySBs)
      else do
        proven_cache <- getNumProven
        refuted_cache <- getNumRefuted
        -- Solve and cache the normalized form (synonyms and associated
        -- type functions expanded), as the per-predicate code did: the
        -- SAT converters model an unexpanded application as an opaque
        -- uninterpreted term, losing its arithmetic meaning, and one
        -- logical predicate would otherwise occupy several cache keys.
        let normPred (IsIn c ts) = IsIn c <$> mapM normT ts
        nnps <- mapM (\ vp -> do p <- normPred (toPred vp)
                                 return (vp, p)) nps
        -- A fully ground numeric predicate's truth is context-free, so
        -- both cached outcomes are final: proven facts discharge without
        -- the solver, refuted facts stay residual without re-asking.
        let (hits, rest) =
                partition (\ (_, p) -> p `S.member` proven_cache) nnps
            (known_bad, misses) =
                partition (\ (_, p) -> p `S.member` refuted_cache) rest
            mkNumBind ((VPred w _), p) =
                let t = predToType p
                in  mkSolvedBind (w, t, mkNumInstBody t) False
            givens = [ p | EPred _ p <- es ]
            ground (_, p) = null (tv p)
            -- discharged preds are removed in place: the survivors keep
            -- their arrival order, which flows onward into retry order,
            -- inferred contexts, and error messages
            residualsWithout discharged =
                let ds = S.fromList [ i | ((VPred i _), _) <- discharged ]
                in  [ vp | vp@(VPred i _) <- rs, i `S.notMember` ds ]
            -- Prove each pending pred from the definition's givens, as
            -- the per-predicate code did.  Ground preds are closed
            -- arithmetic: they are queried with no assumptions at all,
            -- so their outcome is their truth -- cacheable in both
            -- directions.
            --
            -- XXX Passing the sibling residuals as additional
            -- assumptions (proving each pred from the givens plus the
            -- residuals that survive to the context) would let jointly-
            -- constrained sets discharge that individual queries
            -- cannot; it is sound with a shrinking assumption set, but
            -- empirically the extra assertions make later queries in
            -- the batch answer wrongly (Bug782_Div regressions even
            -- with a fresh session and a givens-only retry).  The root
            -- cause is unidentified -- static review found the
            -- per-query push/pop hygiene clean in both backends -- so
            -- it needs a solver-layer investigation first.
            loop st done [] = return (done, st)
            loop st done (x@(_, p):pend) = do
                let assums | ground x = []
                           | otherwise = givens
                (res, st') <- solvePred st assums p
                loop st' ((x, isJust res) : done) pend
        if null misses
          then return (residualsWithout hits,
                       foldr ((<++) . fromSB . mkNumBind) emptySBs hits)
          else do
            satTraceM ("proviso SAT batch: " ++ show (length misses) ++
                       " preds, " ++ show (length hits + length known_bad) ++
                       " cache hits")
            -- a fresh solver session per batch, living entirely within
            -- this one atomic IO block (solvePred push/pops around
            -- each query, so queries in the batch share converted
            -- terms).  The session is deliberately NOT kept between
            -- batches: the vendored Yices binding resets the whole
            -- library whenever any context is created, so a session
            -- may not outlive an evaluation window in which another
            -- one could be created -- confining it to a single block
            -- makes the one-live-context assumption hold by
            -- construction.  Only the ground verdict caches persist
            -- across batches, and they are solver-independent.
            let (solved_results, _) = unsafePerformIO $ do
                    st0 <- initSATPredState flags
                    loop st0 [] misses
            let proven   = [ x | (x, True)  <- solved_results ]
                unproven = [ x | (x, False) <- solved_results ]
                new_proven  = [ p | x@(_, p) <- proven,   ground x ]
                -- an unproven ground pred can never become provable
                -- (its arithmetic is closed and it was queried with no
                -- assumptions)
                new_refuted = [ p | x@(_, p) <- unproven, ground x ]
                sbs = foldr ((<++) . fromSB . mkNumBind) emptySBs
                            (hits ++ proven)
            when (not (null new_proven && null new_refuted)) $
                addNumDecided new_proven new_refuted
            return (residualsWithout (hits ++ proven), sbs)

joinNeededCtxs :: [VPred] -> TI ([VPred], Subst, SolvedBinds)
joinNeededCtxs ps = do
  (ps', s, sbs, _) <- joinNeededCtxsR ps
  return (ps', s, sbs)

-- with the recipe of the bindings (DictPool.Recipe)
joinNeededCtxsR :: [VPred] -> TI ([VPred], Subst, SolvedBinds, Recipe)
joinNeededCtxsR ps = do
  bvs <- getBoundTVs
  case (joinCtxsNG bvs ps) of
    Nothing ->
        return (ps, nullSubst, emptySBs, RcEmpty)
    Just ((ps', s', sb, nn), g) -> do
        -- most calls join nothing, so only the rounds after a join carry
        -- their goals' blobs
        (ps'', s, sbs, rc) <- joinNext bvs nullSubst emptySBs RcEmpty (map vpNum ps)
                                       ((map toGoal ps', s', sb, nn), g)
        return (map goalPred ps'', s, sbs, rc)

joinNeededCtxs' :: Subst -> SolvedBinds -> Recipe -> [JGoal]
                -> TI ([JGoal], Subst, SolvedBinds, Recipe)
joinNeededCtxs' s sbs rc ps = do
  bvs <- getBoundTVs
  case (joinCtxsNG bvs ps) of
    Nothing ->
        return (ps, s, sbs, rc)
    Just j -> joinNext bvs s sbs rc (map (vpNum . goalPred) ps) j

-- A round's join, of goals numbered "ns", and the rounds after it.
joinNext :: [TyVar] -> Subst -> SolvedBinds -> Recipe -> [Int]
         -> (([JGoal], Subst, SolvedBind, (Int, Int)), (Int, [VPred], VPred, VPred))
         -> TI ([JGoal], Subst, SolvedBinds, Recipe)
joinNext bvs s sbs rc ns ((ps', s', sb, (nb, nu)), (n, gvs, v1, v2)) = do
        -- the next joinCtxsN, splitting ps', runs this one's residual
        -- filter, which interns the residual names (DictPool.RJoin)
        logJoin nb nu ns
        let (changed_rs, unchanged_rs) = splitGoals s' ps'
            rs' = (apSub s' changed_rs) ++ unchanged_rs
            s1 = s' @@ s
            sbs1 = fromSB sb <++ sbs
            rc1 = RcMerge (RcJoin nb nu sb) rc
        -- The next round's first act is to force rs' (splitGoals's scan:
        -- the residual filter and "tv" of each goal, in order); do it
        -- here.  The scan has then forced s' if any goal it reached has a
        -- type variable (the first such goal is no further than the scan
        -- went), so reading s' below forces nothing new.
        rs' `seq`
          if any (not . null . tv) ps' && isNullSubst s'
            then joinBatch bvs s sbs1 rc1 rs' n (groupAfter v1 v2 (goalPred (head ps')) gvs)
            else joinNeededCtxs' s1 sbs1 rc1 rs'

-- The rounds after one whose substitution was null.  Such a round
-- changed nothing but the joined group G: the goals in the groups sorted
-- before G are the same goals, or a joined goal p'' with the types of the
-- goal it replaced, so no pair of them joins (every such pair was tried
-- and failed in the round the batch started from), and the next round's
-- join is G's first joining pair, if G has one.  The round's sort, its
-- scan of the groups before G and its split_rs (on goals whose "tv" an
-- earlier round computed, with a null substitution) only repeat
-- comparisons already made on the same objects, so try G directly.
-- "cur" is the goal list the next round would get, "gm" G's members in
-- its order (p'' first, as the stable sort puts it).
joinBatch :: [TyVar] -> Subst -> SolvedBinds -> Recipe -> [JGoal] -> Int -> [VPred]
          -> TI ([JGoal], Subst, SolvedBinds, Recipe)
joinBatch bvs s sbs rc cur n gm =
  case joinInGroup bvs cur n gm of
    Nothing -> joinNeededCtxs' s sbs rc cur
    Just ((ps', s', sb, (nb, nu)), (v1, v2)) -> do
        logJoin nb nu (map (vpNum . goalPred) cur)
        let sbs1 = fromSB sb <++ sbs
            rc1 = RcMerge (RcJoin nb nu sb) rc
        -- every goal's "tv" was computed by an earlier round's scan; the
        -- next round's scan would force s' at the first goal with a type
        -- variable, with nothing effectful before it
        if any (not . null . tv) ps' && isNullSubst s'
          then joinBatch bvs s sbs1 rc1 ps' n (groupAfter v1 v2 (goalPred (head ps')) gm)
          else let (changed_rs, unchanged_rs) = splitGoals s' ps'
                   rs' = (apSub s' changed_rs) ++ unchanged_rs
               in  joinNeededCtxs' (s' @@ s) sbs1 rc1 rs'

-- split_rs for the join rounds, giving the goal list itself when the
-- substitution affects no goal.  Both callers demand the changed goals
-- first, so the scan that finds none is partition's; when it finds one,
-- partition repeats the comparisons it made on the goals up to that one
-- (whose "tv" the goals keep).  A null substitution is scanned too: the
-- scan runs the join's residual filter (see joinNext).
splitGoals :: Subst -> [JGoal] -> ([JGoal], [JGoal])
splitGoals s' rs
    | any affected rs = partition affected rs
    | otherwise = ([], rs)
  where changed_tv = getSubstDomain s'
        affected r = any (`elem` changed_tv) (tv r)

-- The joined group after joining v1 and v2 into p'': p'' first, then the
-- members the residual filter keeps (joinPair's), in order.
groupAfter :: VPred -> VPred -> VPred -> [VPred] -> [VPred]
groupAfter (VPred i _) (VPred i' _) p'' gm = p'' : [ vp | vp@(VPred j _) <- gm, j /= i && j /= i' ]

-- A join, for the recording windows: the numbers of the joined pair's
-- dictionaries and of the residual goals'.  (joinCtxsN has evaluated
-- every residual goal, so reading the numbers forces no name.)
logJoin :: Int -> Int -> [Int] -> TI ()
logJoin nb nu ns = do
  active <- recActive
  when active $ foldr seq () ns `seq` recLog (EvJoin nb nu ns)

-- as we commit to substitutions, we extend the monad with them
-- we preserve them this far so we can efficiently decide which
-- preds to retry satisfying
--
-- Return values are similar to "sat" since they recursively call each other.
satMany :: DVS -> Givens -> [VPred] -> SolvedBinds -> Subst -> [VPred] ->
           TI SolveResult
satMany dvs es rs_accum sbs s ps =
    fmap fst (satManyR dvs es rs_accum sbs RcOpaque s ps)

satMany' :: DVS -> Givens -> [VPred] -> SolvedBinds -> Subst -> [VPred] ->
    TI SolveResult
satMany' dvs es rs_accum sbs s ps =
    fmap fst (satManyR' dvs es rs_accum sbs RcOpaque s ps)

-- satMany and satMany', with the recipe of the bindings beside them
-- (DictPool.Recipe): "rc" is how "sbs" was built.
satManyR :: DVS -> Givens -> [VPred] -> SolvedBinds -> Recipe -> Subst -> [VPred] ->
            TI (SolveResult, Recipe)
-- Heuristic: sort predicates so those with concrete type arguments are processed
-- before those with all-variable arguments.  This ensures fundep-producing
-- predicates (e.g. Bits (UInt 32) n) resolve type variables before
-- predicates that depend on those variables (e.g. Bits a n), avoiding
-- expensive scans of all instances for unresolvable predicates.
satManyR dvs es rs_accum sbs rc s ps =
    satManyR' dvs es rs_accum sbs rc s (sortBy cmpPredConcreteness ps)
    where
        cmpPredConcreteness :: VPred -> VPred -> Ordering
        cmpPredConcreteness (VPred _ (PredWithPositions (IsIn _ ts1) _ _))
                            (VPred _ (PredWithPositions (IsIn _ ts2) _ _)) =
            compare (concreteness ts2) (concreteness ts1) -- higher first
        concreteness ts = length (filter (not . isTVar) ts)

satManyR' :: DVS -> Givens -> [VPred] -> SolvedBinds -> Recipe -> Subst -> [VPred] ->
    TI (SolveResult, Recipe)
-- rs_accum is an accumulating parameter of "needed" VPreds
-- if satisfying fails these are returned (for error messages and ctxReduce)
satManyR' dvs es [] sbs rc s [] = return (SolveResult [] sbs s, rc)
satManyR' dvs es rs_accum sbs rc s [] = do
  (final_rs, s', sbs', rc') <- joinNeededCtxsR rs_accum
  -- the loop is done; numeric residuals are NOT solved here -- they
  -- flow to the caller's settlement point (see the
  -- satisfy/satisfyStream contract above)
  return (SolveResult final_rs (sbs' <++ sbs) (s' @@ s), RcMerge rc' rc)
satManyR' dvs es rs_accum sbs rc s (p:ps) = do
    -- this path always commits partial reductions, so the Commitment
    -- from "sat" is not consulted
    SatResult x_needed x_sbs x_subst _ x_rc0 <- sat dvs es p
    -- p's bindings, read by the pool as those of the dictionary p names
    let x_rc = RcGoal (vpNum p) x_rc0
    let x = (x_needed, x_sbs, x_subst)
    rtrace ("satMany: sat="++ ppReadable (p,x)) $ return ()
    case x of
        (needed@(_:_), sbs', s') ->
            -- If we are unable to satisfy "p", we add the "needed"
            -- preds to the "rs_accum" argument, potentially heading toward
            -- the "return Nothing" case above.
            -- We also accum the bindings to get from "p" to "needed".
            -- we keep growing the substitution because anything we
            -- have learned we can commit to
            --
            -- We apply the substitution to the "needed" preds because we
            -- encounted an example where they were unsubstituted and that
            -- prevented satisfying -- is this apSub just covering for an
            -- issue elsewhere (that should not have returned unsubst'd preds)?
            --
            -- Since we apply "s" to "needed", we also apply it to the not-yet-
            -- processed preds "ps".
            rtrace ("satMany Right: " ++ ppReadable needed) $
            satManyR' dvs es ((apSub s' needed) ++ rs_accum) (sbs' <++ sbs) (RcMerge x_rc rc) (s' @@ s) (apSub s' ps)
        ([], sbs', s') ->
            -- If p is satisfied, we "drop" it, but add its binding and
            -- substitution.
            if isNullSubst s' then
                -- Straight-up drop it
                rtrace ("satMany Left True") $
                satManyR' dvs es rs_accum (sbs' <++ sbs) (RcMerge x_rc rc) s ps
            else do
              let (changed_rs, unchanged_rs) = split_rs s' rs_accum
              if null changed_rs then
                 -- no impact on accumulated predicates
                 rtrace ("satMany Left False True") $
                 satManyR' dvs es rs_accum (sbs' <++ sbs) (RcMerge x_rc rc) (s' @@ s) (apSub s' ps)
               else
                -- Drop it, but try from the beginning again to see if any of
                -- the accumulated rs's can now be
                -- satisfied in the New State Of The World.  This might lead to
                -- horrible running time in the worst case.
                -- Re-sort via satMany since we have a fresh predicate list.
                rtrace ("satMany Left False False") $ do
                let rs' = (apSub s' changed_rs) ++ unchanged_rs
                (rs'', s1, sbs1, rc1) <- joinNeededCtxsR rs'
                let s2 = s1 @@ s'
                satManyR (dvsSub s2 dvs) (apSub s2 es) [] (sbs1 <++ sbs' <++ sbs)
                         (RcMerge (RcMerge rc1 x_rc) rc)
                         (s2 @@ s) (rs'' ++ apSub s2 ps)


-- try to reduce the supplied VPreds as far as possible
-- returning the underlying preds required
reducePredsAggressive :: DVS -> [EPred] -> [VPred] -> TI SolveResult
reducePredsAggressive dvs es vps0 = do
  -- traceM ("reducePredsAggressive (enter): " ++ ppReadable vps0)
  (vps1, s1, sbs1) <- joinNeededCtxs vps0
  checkJoinCtxs "reducePredsAggressive 1" vps0 s1 vps1
  reducePredsAggressive' dvs (mkGivens es) sbs1 s1 vps1

reducePredsAggressive' :: DVS -> Givens -> SolvedBinds -> Subst -> [VPred] ->
                          TI SolveResult
reducePredsAggressive' dvs es sbs1 s1 vps1 = do
  -- Note that we are calling satMany' here, which does not apply the sorting optimization.
  -- There are some existing bugs where the order in which preds are reduced affects whether
  -- instance resolution succeeds (see bsc-contrib/Libraries/GenC/GenCMsg/GenCMsg.bs),
  -- so we conservatively preserve the original order.
  -- Sorting or not sorting here does not seem to have a measurable effect on the overall
  -- performance of type checking.
  SolveResult vps2 sbs2 s2 <- maskAllowIncoherent $ satMany' dvs es [] emptySBs s1 vps1
  checkJoinCtxs "reducePredsAggressive 2" vps1 s2 vps2
  -- A normal type (CType.isNormalType) contains no TItype, TIatf or
  -- Id__ constructor, so leaving such types out of the scan does not
  -- change the badCon answer and skips the union walk over them.
  let allPredTyCons = concat [ concatMap allTyCons (filter (not . isNormalType) ts)
                             | IsIn _ ts <- map toPred vps2 ]
  let badCon (TyCon _ _ (TItype _ _)) = True
      badCon (TyCon _ _ (TIatf {})) = True
      badCon (TyCon i _ _) | i == idId = True
      badCon _ = False
  if (any badCon allPredTyCons) then do
    -- loop to keep synonyms and type functions out of instance heads
    vps2' <- concatMapM (expTConPred . expandSynVPred) vps2
    reducePredsAggressive' dvs es (sbs2 <++ sbs1) s2 vps2'
   else do
    -- satMany is inside a loop, so it doesn't consistently apply
    -- its accumulated substitution to the reduced predicates.
    -- Apply the substitution here to clean that up before returning
    -- to the external caller.
    --
    -- Aggressive reduction is a settlement owner: its callers (CtxRed
    -- signature simplification, tiImpls' proviso generation, the
    -- error reporters) all need a final residual, so settle the
    -- numeric preds here.
    let vps2' = apSub s2 vps2
    (vps3, nsbs) <- batchSolveNumericPreds (givensList (apSub s2 es)) vps2'
    return (SolveResult vps3 (nsbs <++ sbs2 <++ sbs1) s2)

-- note that the subst we return is safe to commit to as long as the
-- instance match isn't incoherent (or if we are ok committing to an
-- incoherent match)
-- A successful predicate reduction: the residual goals (the matched
-- instance's context), the dictionary binding, the fundep improvement
-- to apply, the matched instance head when the match was
-- information-dependent (Nothing for a coherent match), and the
-- defining package.
data Reduction = Reduction {
        redGoals      :: [VPred],
        redBind       :: SolvedBind,
        redSubst      :: Subst,
        redIncoherent :: Maybe Pred,
        redPackage    :: Maybe Id,
        -- the numbers the binding's dictionary arguments were drawn with
        -- (TIMonad.newDictN), for the dictionary pool
        redArgNums    :: [Int]
    }

instance PPrint Reduction where
    pPrint d p (Reduction qs sb us minst mpkg _) =
        pPrint d p ((qs, sb, us), (minst, mpkg))

-- The predicate's class as the current symbol table has it.  A Pred
-- embeds its Class by value, and a record built against an earlier table
-- can lack the instances GenWrap and Deriving added since; only the
-- instance fields can differ, so read instances through this.
currentClass :: Class -> TI Class
currentClass c = do
    r <- getSymTab
    return $ case findSClass r (name c) of
               Just c' | name c' == name c -> c'
               _ -> c

reducePred :: [EPred] -> DVS -> VPred -> TI (Maybe Reduction)
reducePred _eps dvs vp@(VPred w pp@(PredWithPositions pr@(IsIn c ts) pos anc)) = do
    countDP CReducePred 1
    pushSatStackContext
    bound_tyvars <- getBoundTVs
    ts' <- mapM normT ts
    let pr' = IsIn c ts'
        pp' = PredWithPositions pr' pos anc
        v' = vpWith vp pp'
        -- Quick pre-check: can the instance head possibly match the predicate?
        -- Compares head type constructors at non-determined fundep positions.
        -- If both sides have a concrete non-synonym constructor and they
        -- differ, no match is possible so we skip the expensive newInst call.
        -- We only check non-fundep positions because fundep-determined
        -- positions are resolved via mgu after the initial match.
        -- matchTop picks the first fundep row that works, so we need ANY
        -- fundep to work.
        canMatch :: Pred -> Pred -> Bool
        canMatch (IsIn c1 pred_ts) (IsIn _ inst_ts) =
            any checkFD $ funDeps c1
          where
            checkFD bs = and
                [ case (leftNonSynTyCon pt, leftNonSynTyCon it) of
                    (Just pc, Just ic) -> pc == ic
                    _ -> True
                | (False, pt, it) <- zip3 bs pred_ts inst_ts ]
            -- Extract head TyCon Id only if it's not a type synonym, ATF,
            -- or primitive type function (those could expand/evaluate to
            -- match anything, so a head mismatch is not a clash)
            leftNonSynTyCon t =
                case leftTyCon t of
                    Just (TyCon _ _ (TItype {})) -> Nothing  -- synonym
                    Just (TyCon _ _ (TIatf {}))  -> Nothing  -- ATF
                    Just (TyCon i _ _)
                        | isPrimTFunName i       -> Nothing  -- TAdd etc.
                        | otherwise              -> Just i
                    _                            -> Nothing

        -- The predicate's own variables, for the capture check of a
        -- ruled-out instance below, and the numbers of the bound type
        -- variables, for its draws (skipInst; the bound stack is fixed
        -- while the walk runs: nothing in it pushes or pops, and
        -- expandSynN runs in a TI of its own); each demanded at most
        -- once per reduction
        pr_tvs = tv ts'
        bound_nums = IS.fromList [ n | TyVar _ n _ <- bound_tyvars ]

        -- The walk over the instances, in the order genInsts lists them.
        -- One the lookup ruled out (RuledOut: a constructor head at a
        -- pure-input position where the predicate has a type-variable
        -- head, which the one-way match of byInst can never bind) is
        -- passed over without being instantiated, but with the two
        -- things the walk did for it when it tried every instance.  It
        -- draws from the counter what newInst would have drawn
        -- (skipInst), so that every generated name made later in this
        -- definition keeps its number.  And it asks what the NoMatch arm
        -- asks of every instance it passes, whether a fresh copy could
        -- still capture the predicate after refinement, but of the
        -- declared head: the same question when the instance's variables
        -- are apart from the predicate's (renaming them then changes
        -- nothing) or when the accumulator is already set; in the other
        -- case the instance takes the general path and is rejected
        -- there, with the same draws.
        f :: Bool -> [Candidate Inst] -> TI (Maybe Reduction)
        f incoherent [] = return Nothing
        f incoherent (cand : is)
          | useLegacyInstIndex
          , Inst _ _ (_ :=> h_orig) _ <- candidateInst cand
          , not (canMatch pr' h_orig) = f incoherent is
        f incoherent (RuledOut i@(Inst _ vs (_ :=> h_orig) _) : is)
          | incoherent || all (`notElem` pr_tvs) vs = do
                skipInst bound_tyvars bound_nums i (getVPredPositions v')
                f (incoherent || earlierInstanceMayCapture pr' (declaredHead h_orig)) is
        f incoherent (cand : is) = do
                let i = candidateInst cand
                (m_tv, i'@(Inst _ _ (_ :=> h) _)) <- newInst i (getVPredPositions v')
                x <- byInst v' i'
                case x of
                   NoMatch -> do
                     let chk = earlierInstanceMayCapture pr' h
                     f (chk || incoherent) is
                   Defer ->
                     -- a rigid deferral: the row's determined positions
                     -- unify unguarded, so the whole head does -- this
                     -- instance may capture the predicate after
                     -- refinement, which is exactly what the
                     -- accumulator tracks
                     f True is
                   Conflict -> do
                     ai <- getAllowIncoherent
                     if outputFallThroughAllowed ai c
                       then
                         -- Legacy/relational policy: skip the instance,
                         -- allowing a later (less specific) one to be
                         -- selected by its outputs.  An `incoherent' class
                         -- has declared that its resolution is not a
                         -- function of its inputs (e.g. the Has_tpl_n
                         -- tuple machinery), so the ordered-clause
                         -- discipline does not apply to it.  An
                         -- output-conflict skip is the definition of
                         -- output-driven selection -- the demanded
                         -- output just chose against this clause -- so
                         -- the eventual match past it is
                         -- information-dependent and MUST carry the
                         -- flag: an unmarked match would be treated as
                         -- coherent evidence (unique, so eligible for
                         -- reuse that would freeze one context's
                         -- relational choice into another's).  Under
                         -- the -legacy-defer-instances hatch the old
                         -- behavior is restored exactly (unmarked).
                         f (incoherent || not legacyDeferInstances) is
                       else
                         -- Ordered-clause fundep semantics: this instance's
                         -- input positions match, so it is the selected
                         -- clause, and inputs only become more concrete
                         -- under refinement, so no other clause can ever be
                         -- selected.  Its determined positions cannot unify
                         -- with the predicate's, so the predicate is
                         -- unsatisfiable as stated: falling through to a
                         -- later clause would let the demanded output
                         -- choose the clause, violating the functional
                         -- dependency.  Stop the walk and leave the
                         -- predicate unreduced; the standard
                         -- unresolved-context reporting applies.
                         return Nothing
                   Match (InstMatch qs sb inst_subst fd_subst mpkg ns) -> do
                     -- when ((not $ null qs) && (not $ isNullSubst inst_subst)) $
                     --     traceM("qs, inst_subst, fd_subst: " ++ ppReadable (qs, inst_subst, fd_subst))
                     -- does the inst_subst affect anything *outside* of the instance?
                     let bad_inst_subst = fromMaybe inst_subst $
                                          fmap (flip trimSubst $ inst_subst) m_tv
                     when (not (isNullSubst bad_inst_subst)) $
                       internalError("reducePred - bad inst subst: " ++
                                     ppReadable (v', i', m_tv, inst_subst, fd_subst, incoherent))
                     let Inst _ _ (_ :=> h) _ = i
                     let minst = toMaybe incoherent h
                         -- Mark the binding incoherent, so that warnTransitiveIncoherent
                         -- can identify bindings that transitively depend on it.
                         sb'   = if incoherent then markIncoherent sb else sb
                         -- Record the class so warnTransitiveIncoherent can check allowIncoherent.
                         sb''  = sb' { solvedClass = Just c }
                     return $ Just (Reduction qs sb'' fd_subst minst mpkg ns)

    ic <- currentClass c
    let cands = genCandidates ic bound_tyvars dvs pr'
    r <- f False cands
    rpTrace ("reducePred " ++ ppReadable (dvs, pr', [ i | Try i <- cands ], r)) $ return ()
    popSatStackContext
    -- Numeric predicates that instance reduction cannot discharge are
    -- no longer sent to the proviso SAT solver here, one at a time
    -- with a fresh solver per query; they flow to the end of the
    -- enclosing satisfy loop, where batchSolveNumericPreds proves the
    -- whole residual set in one solver session (see there).
    return r

predUnify :: [TyVar] -> Pred -> Pred -> Bool
predUnify bound_tyvars (IsIn c1 ts1) (IsIn c2 ts2)
    | c1 == c2 = isJust (mgu bound_tyvars ts1 ts2)
    | otherwise = False

-- Modal check shared by the walk (reducePred) and the reducibility
-- probe (isReduciblePred): could this earlier instance still capture
-- the predicate under SOME refinement?  Refinement includes call-site
-- instantiation of the definition's rigid variables, so the check
-- must not treat the bound set as fixed (it unifies with an empty
-- bound set): otherwise a rigid-headed predicate would prune every
-- specific instance here and a catch-all below would look
-- uniquely-matching (coherent) inside the definition while call
-- sites would select the specific instance -- exactly the
-- incoherence this check exists to detect.
earlierInstanceMayCapture :: Pred -> Pred -> Bool
earlierInstanceMayCapture = predUnify []

-- The head byInst matches is the declared head after apSub, which
-- rebuilds it through normTAp and expandSyn: a head free of synonyms
-- and primitive type functions comes through unchanged (up to the
-- renaming of its variables), and any other is put through the same
-- normalisation here, so that the capture check reducePred takes on
-- a declared head sees what it would see on a fresh copy.
declaredHead :: Pred -> Pred
declaredHead h@(IsIn _ ts)
    | all normal ts = h
    | otherwise     = apSub nullSubst h
  where normal (TAp f a) = normal f && normal a
        normal (TCon (TyCon _ _ (TItype {}))) = False
        normal (TCon (TyCon i _ _)) = not (isPrimTFunName i)
        normal _ = True

-- The hatches that license output-driven fall-through (assumption A5
-- in the design note): the class's own `incoherent' annotation, with
-- the global -legacy-defer-instances flag or the hidden
-- -incoherent-instance-matches default (the "ai" argument) as
-- fallbacks.  This is THE single statement of the exemption; the
-- walk, the reporting probe, and the reducibility probe all consult
-- it.
outputFallThroughAllowed :: Bool -> Class -> Bool
outputFallThroughAllowed ai c =
    legacyDeferInstances || fromMaybe ai (allowIncoherent c)

-- One fundep row of an instance-head comparison: non-fundep positions
-- via the caller's match function, then the determined positions via
-- unification.  This is the single statement of the row core;
-- matchTop and matchTopIsReducible both consume it.
-- The three-way result of matching a predicate against an instance
-- head (or one fundep row of it).  The reducibility probes reuse the
-- shape with a = () and a weaker reading: there NoMatch means "no
-- conclusion from this instance".
data Match a =
      NoMatch    -- the input (non-determined) positions do not match
    | Defer      -- the inputs match and the determined positions fail
                 -- to unify only because of the bound-variable guard:
                 -- the improvement would have to substitute a rigid
                 -- variable, which must become an equality constraint,
                 -- never a substitution.  Neither a match nor a
                 -- conflict yet -- retry when the predicate is more
                 -- refined
    | Conflict   -- the inputs match but the fundep-determined
                 -- positions clash: under ordered-clause fundep
                 -- semantics this instance is the selected clause and
                 -- the predicate is unsatisfiable as stated
    | Match a    -- the head matches, carrying evidence

-- The first Match wins; otherwise any Conflict decides (a conflict in
-- one row's determined positions blocks a one-way match of those
-- positions in the mirror row, preserving the legacy row fall-back
-- for multi-fundep classes); otherwise any Defer survives (a rigid
-- improvement is pending); otherwise NoMatch.
firstMatch :: [Match a] -> Match a
firstMatch ms = case [ m | m@(Match _) <- ms ] of
                  (m:_) -> m
                  [] | any isConflict ms -> Conflict
                     | any isDefer ms    -> Defer
                     | otherwise         -> NoMatch
  where isConflict Conflict = True
        isConflict _        = False
        isDefer Defer = True
        isDefer _     = False

-- Compare one fundep row of a predicate against an instance head:
-- the row's input positions must match, per the caller's match
-- function, and the determined positions must then unify.  On Match,
-- the evidence is the input-match substitution (which specializes
-- the instance) paired with the fundep improvement (to apply to the
-- monad) and any numeric equalities it requires.
matchFDRow :: [TyVar] -> ([Type] -> [Type] -> Maybe Subst)
           -> [Type] -> [Type] -> [Bool]
           -> Match (Subst, (Subst, [(Type, Type)]))
matchFDRow bound_tyvars mtch ts1 ts2 bs =
    let nbs = map not bs
        v1 = boolCompress nbs ts1
        v2 = boolCompress nbs ts2
    in  case mtch v1 v2 of
          Nothing -> NoMatch
          Just ms ->
              let fd1 = apSub ms (boolCompress bs ts1)
                  fd2 = apSub ms (boolCompress bs ts2)
              in  case mgu bound_tyvars fd1 fd2 of
                    Just us -> Match (ms, us)
                    Nothing ->
                        -- Distinguish a genuine structural clash in the
                        -- determined positions (final, under
                        -- ordered-clause fundep semantics) from a
                        -- failure induced only by the bound-variable
                        -- guard: if the unguarded unifier succeeds, the
                        -- improvement would have to substitute a rigid
                        -- variable, so defer rather than commit a false
                        -- conflict.
                        case mgu [] fd1 fd2 of
                          Nothing -> Conflict
                          Just _  -> Defer

-- Probe for error reporting: is this predicate unsatisfiable as stated
-- because of an ordered-clause fundep conflict?  Under ordered-clause
-- semantics the instances of a coherent class are clauses selected by
-- the fundep input positions; when the predicate's inputs select an
-- instance whose determined positions cannot unify with the
-- predicate's, reducePred stops the walk and the predicate surfaces as
-- an unresolved context.  This re-runs the walk at reporting time
-- (when the predicate is maximally refined) to recover the selected
-- instance for a dedicated error message.  Returns the normalized
-- predicate and the instantiated head of the conflicting instance.
findFunDepConflict :: VPred -> TI (Maybe (Pred, Pred))
findFunDepConflict (VPred _ (PredWithPositions (IsIn c ts) pos _)) = do
    ai <- getAllowIncoherent
    if isPreClass c || outputFallThroughAllowed ai c
      then return Nothing
      else do
        bound_tyvars <- getBoundTVs
        ts' <- mapM normT ts
        let pr' = IsIn c ts'
            -- probe with matchTop directly: this runs on the error
            -- path only, and byInst's extra work (minting subgoal
            -- dictionaries and bindings) is a pure relabeling of
            -- matchTop's verdict that would be discarded here
            walk [] = return Nothing
            walk (i:is) = do
                (_, Inst _ _ (_ :=> h) _) <- newInst i pos
                case matchTop bound_tyvars matchList h pr' of
                  NoMatch   -> walk is
                  Defer     -> walk is
                  Conflict  -> return (Just (pr', h))
                  Match _   -> return Nothing
        ic <- currentClass c
        walk (genInsts ic bound_tyvars Nothing pr')

dvsSub :: Subst -> DVS -> DVS
dvsSub s dvs = dvs
{-
dvsSub _ Nothing = Nothing
dvsSub s dvs =
    if isNullSubst s then
        dvs
    else
        traces ("dvsSub " ++ ppReadable (dvs, s)) dvs
-}

-- A successful instance match: the instance's context as new goals,
-- the dictionary binding, the specializing and fundep-improvement
-- substitutions, and the defining package (for import tracking).
data InstMatch = InstMatch {
        imGoals     :: [VPred],
        imBind      :: SolvedBind,
        imInstSubst :: Subst,
        imFdSubst   :: Subst,
        imPackage   :: Maybe Id,
        imArgNums   :: [Int]      -- see redArgNums
    }

byInst :: VPred -> Inst -> TI (Match InstMatch)
byInst (VPred i p) (Inst e _ (ps :=> h) pkg) = do
    -- no longer necessary because reducePred now provides a fresh instance
    -- Inst e _ (ps :=> h) <- newInst ii (getPredPositions p)
    bound_tyvars <- getBoundTVs
    let m = matchTop bound_tyvars matchList
                     h (removePredPositions p)
    -- rtrace ("byInst " ++ ppReadable (p, ps :=> h, m)) $ return ()
    case m of
     NoMatch -> return NoMatch
     Defer -> return Defer
     Conflict -> return Conflict
     Match (inst_subst, (fd_subst, num_eqs)) -> do
        let s = fd_subst @@ inst_subst
        vns <- mapM (const newDictN) ps
        let vs = map snd vns
        -- if the instance is recursive (has a proviso for itself and expects
        -- a dictionary argument for that), then don't use a new dictionary
        -- for that argument, just pass the instance to itself
        let isSelfRec = any (== p) (apSub s ps)
            vs' = zipWith (\ x y -> if (y == p) then i else x) vs (apSub s ps)
        eq_ps <- mapM (eqToPred (getPredPositions p)) num_eqs
        let eq_pwps = map (mkPredWithPositions []) eq_ps
        eq_vns <- mapM (const newDictN) eq_pwps
        let eq_vs = map snd eq_vns
        -- the binding's free names then hold more than its expression's
        -- (addBindDeps below), which the dictionary pool does not record
        when (not (null eq_vs)) $ recLog EvTaint
        -- if p introduces a new predicate, carry on the position info,
        -- and record p (and its own reduction chain) as the ancestry of
        -- the residual, for error reporting in terms of the user-written
        -- root predicate
        let p_ancestors = mkPredAncestor p : getPredAncestors p
            mkvpred (n, x) y = mkVPredN n x (addPredAncestors
                                                (addPredPositions y (getPredPositions p))
                                                p_ancestors)
            -- if the instance is recursive one of these will be unused?
            ps' = zipWith mkvpred (vns ++ eq_vns) (map (apSub s) (ps ++ eq_pwps))
            t = predToType (apSub s h)
            e' = apSub s e
        ps'' <- concatMapM (expTConPred . expandSynVPred) ps'
        -- rtrace ("byInst: " ++ ppReadable (ps', e', t)) $ return ()
        let binding = (i, t, CApply e' (map CVar vs'))
            -- The binding's expression does not reference the deferred
            -- numeric-equality dictionaries (eq_vs), but it is only
            -- valid evidence if those equalities hold; record them as
            -- semantic dependencies so anything walking a binding's
            -- free variables (emission ordering, later reuse checks)
            -- sees the obligation (see SolvedBinds.addBindDeps).
            solvedBind = addBindDeps eq_vs (mkSolvedBind binding isSelfRec)
        -- (the binding's arguments are vs, except where the instance is
        -- recursive, and then the binding is recursive)
        return (Match (InstMatch ps'' solvedBind inst_subst fd_subst pkg (map fst vns)))

-- Create a new instance by replacing the type variables in the instance
-- with fresh variables.
-- Give the variables a position based on where the predicate was
-- introduced (the list of positions is the positions where the predicate
-- was introduced). XXX unfortunately we can only pass one pos to the var
-- Also the lowest TVar, for use in trimming
newInst :: Inst -> [Position] -> TI (Maybe TyVar, Inst)
newInst ii@(Inst _ vs _ _) poss = do
    let getpos v = getMostUsefulPosition poss (getPosition v)
    when doVarTrace $ traceM ("newInst " ++ ppReadable ii)
    ts <- mapM (\ v -> newTVar "newInst" (kind v) (getpos v)) vs
    let v = case ts of
                (TVar x:xs) -> Just x
                _ -> Nothing
    return (v, apSub (mkSubst (zip vs ts)) ii)

-- Pass an instance over without instantiating it, drawing from the
-- type-variable counter exactly what newInst draws for it: one draw
-- per instance variable, at its kind and position, the bound-variable
-- check included (skipTVar, with the bound variables and their numbers
-- the caller fetched once).  The counter numbers every generated name
-- of the run (_tctyvar, _tcdict, _tctemp) and restarts per definition,
-- so a generated name's spelling recurs across definitions and
-- packages and its interning order, which the order IConv gives the
-- binders of a Cletrec group rests on, is fixed by whoever interned the
-- spelling first: a walk that drew fewer numbers would renumber the
-- names made after it in the definition and could move a __h wire
-- name.
skipInst :: [TyVar] -> IS.IntSet -> Inst -> [Position] -> TI ()
skipInst bvs bvnums ii@(Inst _ vs _ _) poss = do
    when doVarTrace $ traceM ("skipInst " ++ ppReadable ii)
    mapM_ (\ v -> skipTVar bvs bvnums "skipInst" (kind v)
                           (getMostUsefulPosition poss (getPosition v))) vs

lookfor :: [TyVar] -> VPred -> [EPred] -> Maybe (Bind, (Subst, [(Type, Type)]))
lookfor bound_tyvars v@(VPred i pp) eps0 = go eps0
  where
    pr = removePredPositions pp
    meq x y = if x == y then Just nullSubst else Nothing
    go [] = Nothing
    go eps@(EPred e pr':ps)
        -- Under meq a fundep row matches only if its undetermined
        -- positions are equal, so a given with no such row cannot
        -- match; skip it without building the rows' type lists.
        | not (mayEqMatch pr pr') = go ps
        | otherwise =
        -- traces ("lookfor " ++ ppReadable (pr, pr', ps)) $
        -- A given is not an instance clause, so Conflict here just means
        -- this given does not discharge the predicate; keep looking.
        case matchTop bound_tyvars meq pr pr' of
        Match (inst_subst, fd_subst_and_eqs) | isNullSubst inst_subst ->
            Just ((i, predToType pr, e), fd_subst_and_eqs)
        Match (inst_subst, fd_subst) ->
            internalError ("lookfor bad: " ++
                           ppReadable (bound_tyvars, v, eps, inst_subst, fd_subst))
        _ -> go ps

-- Can matchTop with an equality matcher return a match?  Only through a
-- fundep row whose undetermined positions (False in the row) are equal.
mayEqMatch :: Pred -> Pred -> Bool
mayEqMatch (IsIn c1 ts1) (IsIn c2 ts2) =
    c1 == c2 && any (\ bs -> eqUndetermined bs ts1 ts2) (funDeps c1)
  where eqUndetermined (b:row) (t1:rest1) (t2:rest2) =
            (b || t1 == t2) && eqUndetermined row rest1 rest2
        eqUndetermined _ _ _ = True

bySuperE :: EPred -> [EPred]
bySuperE ep@(EPred e p@(IsIn c ts)) = ep : eps ++ comm
  where s       = mkSubst (zip (csig c) ts)
        esupers = map (\ (CTypeclass i, p) ->
                        let p' = apSub s p
                        in  EPred (CApply (cTApply (CSelectT (typeclassId $ name c) i) ts) [e]) p') (super c)
        eps     = concatMap bySuperE esupers
        comm    = if (isComm c) then {- (traces ("expr: " ++ ppReadable e) $-}
                     [(EPred e (IsIn c (commute ts)))]
                  else []

-- (ETree and bySuperT, bySuperE's expansion of a given as a tree, are in
-- TIMonad, which keeps a GRigid level's trees.)

flattenT :: ETree -> [EPred]
flattenT (ETree ep subs comm) = ep : concatMap flattenT subs ++ comm

-- The entries of flattenT whose class has key k (TIMonad.classKey), in
-- its order, entering only the subtrees whose class tree holds k.  The
-- class tree is the given's (GTree): the tree of its class, which
-- bySuperT walks the same way.
entriesK :: Int -> GTree -> ETree -> [EPred]
entriesK k (GTree key _ gsubs) (ETree ep esubs comm) =
    (if key == k then (ep :) else id)
    (concat [ entriesK k gt et | (gt, et) <- zip gsubs esubs, IS.member k (gtReach gt) ] ++
     (if key == k then comm else []))

-- The elements at the given ascending positions, with their positions.
pickAt :: [Int] -> [a] -> [(Int, a)]
pickAt is0 xs0 = go 0 is0 xs0
  where go _ [] _ = []
        go j (i:is) xs = case drop (i - j) xs of
                           (x:xs') -> (i, x) : go (i + 1) is xs'
                           [] -> internalError "TCMisc.pickAt"

-- The entries of key k of the givens' expansions (concatMap bySuperE of
-- the givens), in order, from the givens' trees (givenExp's).
givenEntries :: Int -> [(GSeg, [ETree])] -> [EPred]
givenEntries k segs =
    concat [ entriesK k (gsTrees seg ! i) et
           | (seg, ets) <- segs,
             (i, et) <- pickAt (classIdxLookup (gsIdx seg) k) ets ]

-- The givens' expansion as sat reads it for a goal: the entries of the
-- goal's class, and the trees of every level (the pool's guards).
-- Without givens both are empty and nothing is built or read: the goal's
-- class is not looked at.
data GivenExp = GivenExp [EPred] [(GSeg, [ETree])]

givenExp :: VPred -> Givens -> GivenExp
givenExp vp gv =
    case givensSegs gv of
      [] -> GivenExp [] []
      segs -> let trees = [ (seg, segTrees seg) | seg <- segs ]
              in  GivenExp (givenEntries (vpClassKey vp) trees) trees

-- A level's trees: a GRigid level's own, built once for every sat that
-- reads the level, or built for this one.
segTrees :: GSeg -> [ETree]
segTrees seg = case gsBound seg of
                 GRigid _ _ ets _ -> ets
                 GFlex -> map bySuperT (gsList seg)

vpClassKey :: VPred -> Int
vpClassKey vp = case toPred vp of IsIn c _ -> classKey c

-- The goal's givenEntries from trees built for the call alone, as an
-- expansion written out at the call is built
{-# NOINLINE freshGivenEntries #-}
freshGivenEntries :: VPred -> Givens -> [EPred]
freshGivenEntries vp gv =
    concat [ entriesK k (gsTrees seg ! i) (bySuperT ep)
           | seg <- givensSegs gv,
             (i, ep) <- pickAt (classIdxLookup (gsIdx seg) k) (gsList seg) ]
  where k = vpClassKey vp

-- The givens themselves (no superclass entries) whose class has c's key,
-- in order.
{-# NOINLINE directEntries #-}
directEntries :: Class -> Givens -> [EPred]
directEntries c gv =
    [ ep | seg <- givensSegs gv,
           (i, ep) <- pickAt (classIdxLookup (gsIdx seg) k) (gsList seg),
           gtKey (gsTrees seg ! i) == k ]
  where k = classKey c

-- Match a predicate against an instance head: each fundep row goes
-- through matchFDRow and the rows join with firstMatch.  The "match"
-- function decides how non-fundep positions compare: byInst passes
-- one-way list matching; lookfor passes equality.
-- XXX the fundep lists are not used jointly?
matchTop :: [TyVar] ->
            ([Type] -> [Type] -> Maybe Subst) ->
            Pred -> Pred -> Match (Subst, (Subst, [(Type, Type)]))
matchTop bound_tyvars mtch (IsIn c1 ts1) (IsIn c2 ts2)
    | c1 /= c2  = NoMatch
    | otherwise = firstMatch [ matchFDRow bound_tyvars mtch ts1 ts2 bs
                             | bs <- funDeps c1 ]


-------

findAssump :: Id -> [Assump] -> TI Assump
findAssump i as =
    case [ sc | (i':>:sc) <- as, i==i' ] of
    [] -> do
        s <- getSymTab
        case findVar s i of
         Nothing -> errorAtId EUnboundVar i
         Just (VarInfo _ a d pkg) -> do
            recordPackageUse pkg
            case d of
                Nothing -> return ()
                Just str -> twarn (getPosition i,
                                   WDeprecated "definition" (pfpString i) str)
            return (updAssumpPos i a)
    sc : _ -> return (i :>: sc)

-------


mkSchemeNoBVs :: CQType -> TI Scheme
mkSchemeNoBVs cqt = do
    sy <- getSymTab
    bvs <- getBoundTVs
    case convCQType sy cqt of
     Left emsg -> err emsg
     Right qt -> return (quantify (tv qt \\ bvs) qt)

mkQualType :: CQType -> TI (Qual Type)
mkQualType cqt = do
    s <- getSymTab
    case convCQType s cqt of
     Left emsg -> err emsg
     Right qt -> return qt

-------

rmPatLit :: CPat -> TI (CPat, [CQual])
--rmPatLit p = return (p, [])
rmPatLit (CPCon i ps) = do
    pqs <- mapM rmPatLit ps
    let (ps', qss) = unzip pqs
    return (CPCon i ps', concat qss)
rmPatLit (CPstruct mb i ips) = do
    let f (i, p) = do (p, qs) <- rmPatLit p; return ((i, p), qs)
    ipqs <- mapM f ips
    let (ips', qss) = unzip ipqs
    return (CPstruct mb i ips', concat qss)
rmPatLit p@(CPVar _) = return (p, [])
rmPatLit (CPAs i p) = do
    (p', qs) <- rmPatLit p
    return (CPAs i p', qs)
rmPatLit p@(CPAny {}) = return (p, [])
rmPatLit (CPLit (CLiteral p l)) = do
    v <- newVar p "rmPatLit"
    return (CPVar v, [CQFilter (cVApply idEqual [CVar v, CLit (CLiteral p l)])])
rmPatLit (CPMixedLit pos base ps) = do
    -- only binary, hex and octal are legal
    let bitwidth = log2 base
    v <- newVar pos "rmPatLit"
    let mkLit len val = CLit (CLiteral pos (LInt (IntLit { ilWidth = Just len,
                                                           ilBase = base,
                                                           ilValue = val })))
        mkUnsizedLit val = CLit (CLiteral pos (LInt (ilDec val)))
    let foldfunc (len, Nothing)  (idx, quals) = (idx + (len * bitwidth), quals)
        foldfunc (len, Just val) (idx, quals) =
            let bitsize = len * bitwidth
                lo = mkUnsizedLit idx
                hi = mkUnsizedLit (idx + bitsize - 1)
                extract_expr = CSub2 (CVar v) hi lo
                val_expr = mkLit bitsize val
                q = CQFilter (cVApply idEqual [extract_expr, val_expr])
            in  (idx + bitsize, q:quals)
        (_, quals) = foldr foldfunc (0,[]) ps
    return (CPVar v, quals)
rmPatLit (CPCon1 ti i p) = do
    (p', qs) <- rmPatLit p
    return (CPCon1 ti i p', qs)
rmPatLit (CPConTs ti i ts ps) = do
    pqs <- mapM rmPatLit ps
    let (ps', qss) = unzip pqs
    return (CPConTs ti i ts ps', concat qss)
rmPatLit _ = internalError "TCMisc.rmPatLit"

rmQualLit :: CQual -> TI [CQual]
rmQualLit q@(CQFilter _) = return [q]
rmQualLit (CQGen i p e) = do
    (p', qs) <- rmPatLit p
    return (CQGen i p' e : qs)

-------

-- only for expanding synonyms when you are NOT on a type-checking monad
-- will substitute in fresh variables for all type variables in the input type
-- to avoid variable capture
expandSynN :: Flags -> SymTab -> Type -> Type
-- A normal type (CType.isNormalType) has no type variables (tv t = [])
-- and normT is the identity on it, so the local runTI below would
-- return Right t with no effects.
expandSynN _ _ t | isNormalType t = t
expandSynN flags s t =
   -- should only need to match instances for coherent typeclasses
   -- XXX user code corner-case?
   case tiResult $ runTI flags False s $
                do addBoundTVs (tv t) -- to prevent generated variable capture
                   normT t
   of  Left msg -> internalError ("expandSynN " ++ ppReadable msg)
       Right t  -> t

normT :: Type -> TI Type
-- On a normal type (CType.isNormalType) expandSyn is the identity (no
-- synonym, no primitive type function to fold), expTFun returns ([], t)
-- and ps is null, so the equation below returns t with no effects.
normT t | isNormalType t = return t
normT t = do
    let t' = expandSyn t
    (ps, t'') <- expTFun t'
    -- traceM ("normT: " ++ ppReadable (t,t',(ps,t'')))
    if null ps then
        return t''
     else do
        -- A type function can expand to a pred that is satisfied locally by an explicit pred.
        eps <- getExplGivens
        -- (no dictionary-pool record holds this satisfy: it reads the explicit
        -- givens, the proviso-SAT flag and the global substitution, and warns)
        recLog EvTaint
        (ps', _) <- satisfyG eps ps
        --unless (null ps') (internalError ("expandSynN " ++ ppReadable (t, ps')))
        if not (null ps') then
            return t'                -- XXX could expand some
         else do
            s <- getSubst
            let t''' = expandSyn (apSub s t'')
            --traces ("normT " ++ ppReadable (ps', t''', s)) $ return ()
            return t'''

-- expands synonyms and applies the substitution
-- to get a "fully desugared" type based on all
-- available information
expandFullType :: Type -> TI (Type)
expandFullType t = do
                      s <- getSubst
                      tunexp  <- normT t
                      let t' = expandSyn (apSub s tunexp)
                      return (t')

----
-- a::x is only used for error reporting
unify :: (PPrint a, PVPrint a, HasPosition a)
      => a -> Type -> Type -> TI ([VPred])
unify x t1 t2 = do
    let pos = getPosition x
    -- traceM("unify 1: " ++ ppReadable(t1, t2))
    s <- getSubst
    bound_vars <- getBoundTVs
    let t1' = apSub s t1
        t2' = apSub s t2
    -- traceM("unify 2: " ++ ppReadable(t1', t2'))
    case mgu bound_vars t1' t2' of
     Just (u,[])  -> do extSubst "unify" u; return []
     otherwise -> do
        t1'' <- normT t1'
        t2'' <- normT t2'
        -- traceM ("unify 3:\n" ++ ppReadable (t1'', t2''))
        case mgu bound_vars t1'' t2'' of
          Just (u,eqs)  ->
              do extSubst "unify" u
                 concatMapM (eqToVPred [] [pos]) eqs
          Nothing -> let (t1'', t2'') = niceTypes (t1', t2')
                     in reportUnifyError bound_vars x t1'' t2''

eqToPred :: [Position] -> (Type, Type) -> TI Pred
eqToPred poss (t1, t2) = do
    mp <- tryATFClassPred t1 t2
    case mp of
      Just pred -> return pred
      Nothing -> do
        mp2 <- tryATFClassPred t2 t1
        case mp2 of
          Just pred -> return pred
          Nothing ->
            case kind t1 of
              KNum  -> do cls <- numEqCls; return $ IsIn cls [t1, t2]
              k     -> internalError $
                "eqToPred: required unexpected sort of type equality: " ++
                ppReadable(t1, t2, k)

-- | Given an ATF application type and a target type, try to build the
-- corresponding class predicate.  For example, if atfType is @Elem x@
-- (from @class Container f e | f -> e where type Elem f = e@), and
-- targetType is @T@, this produces @Just (IsIn Container [x, T])@.
tryATFClassPred :: Type -> Type -> TI (Maybe Pred)
tryATFClassPred atfType targetType = do
    let (f, atfArgs) = splitTAp atfType
    case f of
      TCon (TyCon _ _ (TIatf { atf_class_id = clsId, atf_param_idxs = pIdxs
                              , atf_target_idx = tIdx }))
        | length atfArgs == length pIdxs -> do
        (cls, classArgs) <- mkATFClassPred "tryATFClassPred" atfType
                              clsId pIdxs tIdx atfArgs targetType
        return $ Just (IsIn cls classArgs)
      _ -> return Nothing

eqToVPred :: [PredAncestor] -> [Position] -> (Type, Type) -> TI [VPred]
eqToVPred ancs poss ty_eq = do
  p <- eqToPred poss ty_eq
  mkVPredNoNewPos $ addPredAncestors (mkPredWithPositions poss p) ancs

unifyNoEq :: (PPrint a, PVPrint a, HasPosition a)
          => [Char] -> a -> Type -> Type -> TI ()
unifyNoEq loc x t1 t2 = do
  ps <- unify x t1 t2
  when (not $ null ps) $
    internalError ("unexpected unification equalities (" ++ loc ++ "): " ++
                   ppReadable(t1, t2, ps))
  return ()

-----

reportUnifyError :: (PPrint a, PVPrint a, HasPosition a)
                 => [TyVar] -> a -> Type -> Type -> TI b
reportUnifyError bound_vars x orig_t1 orig_t2 =
    let
        def_err = defaultUnifyError x orig_t1 orig_t2

        -- readable versions for err msgs
        x'  = pfpReadable x
        orig_t1' = pfpReadable orig_t1
        orig_t2' = pfpReadable orig_t2

        -- return value mismatch
        err_res r1 r2 =
            err (getPosition x,
                 EFuncMismatchResult x' orig_t1' orig_t2'
                                     (pfpReadable r1) (pfpReadable r2))

        -- mismatch in the number of args
        err_wrong_arg_num n1 n2 =
            err (getPosition x,
                 EFuncMismatchNumArgs x' orig_t1' orig_t2' n1 n2 Nothing)

        -- mismatch in num of args, but we know where the first
        -- type mismatch occurred, so maybe the arg is missing there?
        err_wrong_arg_num_maybe_n n1 n2 startnum =
            err (getPosition x,
                 EFuncMismatchNumArgs x' orig_t1' orig_t2' n1 n2 (Just startnum))

        -- mismatch in the type of argument n
        err_arg_n num a1 a2 =
            err (getPosition x,
                 EFuncMismatchArgN x' orig_t1' orig_t2'
                                   num (pfpReadable a1) (pfpReadable a2))

        -- not expecting args
        err_no_args_expected =
            err (getPosition x,
                 EFuncMismatchNoArgsExpected x' orig_t1' orig_t2')

        -- expecting args, but given none
        err_args_expected n =
            err (getPosition x,
                 EFuncMismatchArgsExpected x' orig_t1' orig_t2' n)

        -- This allows more unification that would actually be allowed
        -- because it does not take into account substitutions from previous
        -- unifications.  Therefore a->a->a would match Bool->Int->Int.
        -- This is ok, because it will be reported with the default error.
        canUnify t1 t2 =
            case (mgu bound_vars t1 t2) of
                Nothing -> do -- call normT because unify does
                              t1' <- normT t1
                              t2' <- normT t2
                              case (mgu bound_vars t1' t2') of
                                  Nothing -> return False
                                  Just _  -> return True
                Just _  -> return True

        -- find the number of arguments expected by a type
        findNumArgs :: Type -> Integer
        findNumArgs t = genericLength $ fst $ getArrows t

        -- Try to find the specific mismatch, else report the default
        -- mismatch error on the entire original type.
        -- The 'n' is the number of args which matched so far.
        findMismatch n (TAp (TAp (TCon arr1) a1) r1) (TAp (TAp (TCon arr2) a2) r2)
                      | isTConArrow arr1 && isTConArrow arr2 = do
            b <- canUnify a1 a2
            if (b)
              then findMismatch (n+1) r1 r2
              else let num_args1 = n + 1 + findNumArgs r1
                       num_args2 = n + 1 + findNumArgs r2
                   in  if (num_args1 == num_args2)
                       then err_arg_n (n+1) a1 a2
                       else err_wrong_arg_num_maybe_n num_args1 num_args2 (n+1)
        findMismatch n (TAp (TAp (TCon arr1) a1) r1) t1 | isTConArrow arr1 =
            if (n == 0)
            then err_args_expected (1 + findNumArgs r1)
            else -- see if the return value matches?
                 err_wrong_arg_num (n + 1 + findNumArgs r1) n
        findMismatch n t1 (TAp (TAp (TCon arr2) a2) r2) | isTConArrow arr2 =
            if (n == 0)
            then err_no_args_expected
            else err_wrong_arg_num n (n + 1 + findNumArgs r2)
        findMismatch n t1 t2 =
            if (n == 0)
            then def_err
            else do b <- canUnify t1 t2
                    if (b)
                      then def_err -- couldn't find the mismatch
                      else err_res t1 t2
    in
        findMismatch 0 orig_t1 orig_t2

defaultUnifyError :: (HasPosition a, PPrint a, PVPrint a)
                  => a -> Type -> Type -> TI d
defaultUnifyError x t1 t2 =
    -- A generated temporary (enumId names them "_tc...", with unstable
    -- numbering) means nothing to the user; suppress the subject and
    -- let the position locate the error.
    let subj = pfpReadable x
        subj' = if "_tc" `isPrefixOf` subj then "" else subj
    in  err (getPosition x,
             EUnify subj' (pfpReadable t1) (pfpReadable t2))

-----

-- unify e td (a `fn` rt)
unifyFnFrom :: (HasPosition a, HasPosition b, PPrint b, PVPrint b)
            => a -> b -> Type -> Type -> TI (Type, [VPred])
unifyFnFrom x e (TAp (TAp arr a) r) rt | arr == tArrow = do
    ps <- unify e r rt
    return (a, ps)
unifyFnFrom x e t rt = do
{-
  s <- getSubst
  case apSub s t of
   (TAp (TAp arr a) r) | arr == tArrow -> do
    unify e r rt
    return a
   _ -> do
-}
    v <- newTVar "unifyFnFrom" KStar x
    ps <- unify e t (v `fn` rt)
    return (v, ps)

-- unify e td (at `fn` r)
unifyFnTo :: (HasPosition a, HasPosition b, PPrint b, PVPrint b)
          => a -> b -> Type -> Type -> TI (Type, [VPred])
unifyFnTo x e (TAp (TAp arr a) r) at | arr == tArrow = do
    ps <- unify e a at
    return (r, ps)
unifyFnTo x e t at = do
{-
  s <- getSubst
  case apSub s t of
   (TAp (TAp arr a) r) | arr == tArrow -> do
    unify e a at
    return r
   _ -> do
-}
    v <- newTVar "unifyFnTo" KStar x
    ps <- unify e t (at `fn` v)
    return (v, ps)

-- because we merely break apart the type if it has function structure
-- and create new type variables only if it doesn't
-- we never do anything that might generate numeric equalities
-- unify e td (a `fn` r)
unifyFnFromTo :: (HasPosition a, HasPosition b, PPrint b, PVPrint b)
              => a -> b -> Type -> TI (Type, Type)
unifyFnFromTo x e (TAp (TAp arr a) r) | arr == tArrow = do
    return (a, r)
unifyFnFromTo x e t = do
{-
  s <- getSubst
  case apSub s t of
   (TAp (TAp arr a) r) | arr == tArrow -> do
    return (a, r)
   _ -> do
-}
    v <- newTVar "unifyFnFromTo" KStar x
    r <- newTVar "unifyFnFromTo" KStar x
    unifyNoEq "unifyFnFromTo" e t (v `fn` r)
    return (v, r)

-----

-- clean up synonyms and TCons when VPreds are made
-- they should not be re-introduced later (by predicate reduction)o
mkVPred :: Position -> PredWithPositions -> TI [VPred]
mkVPred pos p = do
    (n, v) <- newDictN
    expTConPred (expandSynVPred $ mkVPredN n v (addPredPositions p [pos]))

mkVPredNoNewPos :: PredWithPositions -> TI [VPred]
mkVPredNoNewPos p = do
    (n, v) <- newDictN
    expTConPred (expandSynVPred $ mkVPredN n v p)

-- entry point when we don't need synonyms reduced
-- mainly deriving and other places where we check reducibility
mkVPredFromPred :: [Position] -> Pred -> TI VPred
mkVPredFromPred poss p = do
  (n, v) <- newDictN
  return (mkVPredN n v $ mkPredWithPositions poss p)

toPredWithPositions :: VPred -> PredWithPositions
toPredWithPositions (VPred _ p) = p

toPred :: VPred -> Pred
toPred p = removePredPositions (toPredWithPositions p)

vpIsPreClass :: VPred -> Bool
vpIsPreClass (VPred _ (PredWithPositions (IsIn cl _) _ _)) = isPreClass cl


-- Close a set of variables with respect to predicates and their functional
-- dependencies.
-- In otherwords: Extend a set of variables to includes all the variables
-- which depend on them, as indicated in a list of predicates.
closeFD :: [VPred] -> [TyVar] -> [TyVar]
closeFD ps vs =
    let ps' = map toPred ps
    in  closeFD_pred ps' vs

closeFD_pred :: [Pred] -> [TyVar] -> [TyVar]
closeFD_pred ps vs =
    let
        -- extend given one dependency
        step1 ts bs vars_in_set =
            let dependee_exprs  = (boolCompress (map not bs) ts)
                dependee_vars   = tv dependee_exprs
                dependent_exprs = (boolCompress bs ts)
                dependent_vars  = tv dependent_exprs
            in
                -- This intentionally includes situations where the dependee
                -- set is null
                if all (`elem` vars_in_set) dependee_vars
                then vars_in_set `union` dependent_vars
                else vars_in_set

        -- extend for all dependencies for a given predicate
        step p vars_in_set =
            let (IsIn c ts) = p
            in  foldr (step1 ts) vars_in_set (funDeps c)

        vs' = foldr step vs ps
    in
        -- recurse until no new vars are found
        if (length vs == length vs')
        then vs
        else closeFD_pred ps vs'

-------

-- Note that "t" can be a tuple of types, or list of types, or a predicate,
-- because class Types just means that "t" *contains* types.
niceTypes :: (Types t) => t -> t
niceTypes given_type =
        let
            -- Variables in the given "t"
            type_vars :: [TyVar]
            type_vars = tv given_type

            type_var_names = map (getIdString . getTyVarId) type_vars

            -- Make an inexaustible list of nice names
            from_a_to_z prefix = [prefix ++ [letter] | letter <- ['a'..'z']]
            inexhaustible_var_names =
                (map (:[]) ['a'..'z'] ++
                 concatMap from_a_to_z inexhaustible_var_names)

            -- Make the names end with an identifiable suffix
            new_inexhaustible_var_names =
                -- and rule out any existing names
                filter (`notElem` type_var_names) $
                map (\ s -> s ++ sAcute) inexhaustible_var_names

            -- Function to substitute in the new names
            subst_vars :: [TyVar] -> [String] -> [(TyVar, Type)]
            subst_vars (orig_var:rest_orig_vars)
                       new_names@(new_name:rest_new_names)
                | isNewTVar orig_var =
                    let new_id  =
                            -- preserve the "isNewTVar" property
                            setBadId $
                            mkId (getPosition orig_var) (mkFString new_name)
                        new_var = cTVarKind new_id (kind orig_var)
                    in  (orig_var, new_var) :
                        subst_vars rest_orig_vars rest_new_names
                | otherwise =
                    subst_vars rest_orig_vars new_names
            subst_vars [] _ = []
            subst_vars _ [] =
                internalError ("TCMisc.niceTypes: " ++
                               "inexhaustible name supply exhausted")

            -- Create a substitution pairing the existing vars with the new
            var_subst_pairs = subst_vars type_vars new_inexhaustible_var_names
            substitution = mkSubst var_subst_pairs
        in
            apSub substitution given_type

-------

-- The following code is for defaulting types when an explicit type
-- declaration is reached but for which type inference has inferred
-- more contexts than appear in the explicit type.  Any contexts which
-- have variables that appear in the base type result in "context too
-- weak" errors.  For multi-parameter type classes, if some variables
-- are not in the base type, we may want to default them, which could
-- cause the context to be resolved.

-- In the following code we define the classes that are defaultable
-- and how to compute potential default assignments from an
-- unsatisfied predicate. Defaulting can fill in, at most, one
-- "hole" (i.e. potentially ambiguous type variable) in a predicate.
-- Some classes (e.g. Literal) have multiple defaults worth trying
-- while others (e.g. BitExtend) need to compute the default from
-- the other arguments of the predicate.

-- Ideally, Pred.Class would have a field "getDefaults" that would replace
-- getStarDefaults / getNumDefaults, but we'd need syntax for specifying
-- the defaults for of a class (steal / extend the Haskell' proposal?)

-- If more than one class proposes defaults for a particular
-- ambiguous type variable, we currently intersect the proposed defaults
-- (which handles the PrimIndex/Literal/RealLiteral possible overlap with
-- no wasted work). In some cases (particularly if we made defaults more
-- general) it might be better to union the proposed defaults.

-- Classes with *-kind types which can be defaulted
getStarDefaults :: S.Set TyVar -> VPred -> [(TyVar, [Type])]
getStarDefaults fvs (VPred i (PredWithPositions (IsIn (Class {name=(CTypeclass cid)}) ts) _ _)) =
    case ts of
      [TVar v, _] | cid == idPrimIndex,
                     not (S.member v fvs) -> [(v, [tInteger, tNat noPosition])]
      [TVar v]    | cid == idLiteral,
                     not (S.member v fvs) -> [(v, [tInteger, tNat noPosition, tReal])]
      [TVar v]    | cid == idRealLiteral,
                     not (S.member v fvs) -> [(v, [tReal])]
      [TVar v, n] | cid == idSizedLiteral,
                     not (S.member v fvs) -> [(v, [TAp tBit n, TAp tUInt n, TAp tInt n])]
      [TVar v]    | cid == idStringLiteral,
                     not (S.member v fvs) -> [(v, [tString, tChar])]
      _ -> []

-- Classes with #-kind types which can be defaulted
getNumDefaults :: S.Set TyVar -> VPred -> [(TyVar, [Type])]
getNumDefaults fvs (VPred i (PredWithPositions (IsIn (Class {name=(CTypeclass cid)}) ts) _ _)) =
    case ts of
      [TVar v, t, _] | cid == idBitExtend,
                        not (S.member v fvs),
                        all (flip S.member $ fvs) (tv t) -> [(v, [t])]
      [t, TVar v, _] | cid == idBitExtend,
                        not (S.member v fvs),
                        all (flip S.member $ fvs) (tv t) -> [(v, [t])]
      [TVar v, t, _] | cid == idAdd,
                        not (S.member v fvs),
                        all (flip S.member $ fvs) (tv t) -> [(v, [cTNum 0 noPosition])]
      [t, TVar v, _] | cid == idAdd,
                        not (S.member v fvs),
                        all (flip S.member $ fvs) (tv t) -> [(v, [cTNum 0 noPosition])]
      _ -> []

getForceDefaults :: S.Set TyVar -> VPred -> [(TyVar, Type)]
getForceDefaults fvs (VPred i (PredWithPositions (IsIn (Class {name=(CTypeclass cid)}) ts) _ _)) =
    case ts of
      [TVar v]    | cid == idStringLiteral,
                     not (S.member v fvs) -> [(v, tString)]
      _ -> []

-- Takes
--   fixedVars = list of variables from the base type unioned with a
--       list of variables from the context (bound variables, assumptions)
--   givenPreds = explicit predicates provided by the user
--   unsatisfiedPreds = unsatisfied predicates on the base type
--
-- Then, the function identifies ambiguous variables in the predicates
-- and tries to default them.
--
-- It returns:
--   any unsatisfied predicates,
--   any bindings resulting from satisfying predicates, and
--   any remaining ambiguous type variables (which were not defaultable or
--       defaulting didn't succeed).
--
defaultClasses :: [TyVar] -> [EPred] -> [VPred] ->
                  TI ([VPred], SolvedBinds, [TyVar])
defaultClasses fixedVars givenPreds unsatisfiedPreds =
   let
       givenG = mkGivens givenPreds

       -- all variables which are not ambiguous
       fixedVars_closed = closeFD unsatisfiedPreds fixedVars

       fixedVarsSet :: S.Set TyVar
       fixedVarsSet = S.fromList fixedVars_closed

       -- whether a substitution substitutes for a fixed variable
       -- (we use "fixedVars_closed" because we do not want to limit the
       -- fixed variables at all; we'd rather report an ambiguity error)
       subsFixedVar s = any (`S.member` fixedVarsSet) (getSubstDomain s)

       -- identify the ambiguous variables
       -- (we leave it to "handleAmbiguousContext" to filter out any ambig
       -- vars that are determined by other ambig vars)
       ambVarsSet :: S.Set TyVar
       ambVarsSet = S.fromList (concatMap tv unsatisfiedPreds) `S.difference` fixedVarsSet

       isAmbiguousVar v = S.member v ambVarsSet

       preds_with_vars :: [(VPred, [TyVar])]
       preds_with_vars = [(p, filter isAmbiguousVar (tv p)) | p <- unsatisfiedPreds ]

       -- produce a list of
       -- pairs of ambiguous vars and the preds they appear in
       vars_with_preds :: [(TyVar, [VPred])]
       vars_with_preds = M.toList . (M.fromListWith (++)) $
                             [(v,[p]) | (p, vs) <- preds_with_vars, v <- vs]

       preds_to_default = map fst preds_with_vars

       num_defaults  :: [(TyVar, [Type])]
       num_defaults  = concatMap (getNumDefaults  fixedVarsSet) preds_to_default

       star_defaults :: [(TyVar, [Type])]
       star_defaults = concatMap (getStarDefaults fixedVarsSet) preds_to_default

       defaultsMap :: M.Map TyVar [Type]
       defaultsMap = M.fromListWith (intersect) $ num_defaults ++ star_defaults

       -- Given a type variable, a list of predicates in which it
       -- appears, and a type to default to, substitute the default
       -- for the variable in the predicates and determine if "sat"
       -- succeeds for all predicates.  If so, return that substitution.
       -- If a substitution was found by a prior call, just pass its
       -- value through.
       tryDefault :: TyVar -> [VPred] -> (Maybe Subst) -> Type ->
                     TI (Maybe Subst)

       tryDefault i ps Nothing t =
           do
              let s = mkSubst [(i,t)]
                  ps' = apSub s ps

              answer0 <- satMany (Just fixedVars) givenG [] emptySBs s ps'
              -- candidate validation is a verdict: settle the numeric
              -- residuals under this candidate before requiring emptiness
              -- (this speculative pass is quarantined -- only the
              -- substitution of an accepted candidate is merged)
              answer <- case answer0 of
                SolveResult rs0@(_:_) sbs0 s0 -> do
                    (rs1, nsbs) <- batchSolveNumericPreds
                                       (apSub s0 givenPreds) rs0
                    return (SolveResult rs1 (nsbs <++ sbs0) s0)
                r -> return r
              return $ case answer of
              -- we reject this default if it does not work for some pred
              -- or if it results in a substitution of a bound variable
                SolveResult [] _ s' | not $ subsFixedVar s' -> Just s'
                _ -> Nothing
       tryDefault i ps s@(Just _) t = return s

       tryDefaults :: Subst -> (TyVar, [VPred]) -> TI Subst
       tryDefaults subst (i,ps) =
           do
              let typesToTry = M.findWithDefault [] i defaultsMap
              r <- foldM (tryDefault i ps) Nothing typesToTry
              return $ case r of
                         Nothing -> subst
                         -- only merge agreements so we are (mostly)
                         -- order-independent
                         Just s -> mergeAgreements s subst

       -- take a new subst and the remaining unsatisfied preds, and simplify
       makeResult amb_set s ps = do
         -- see how many total preds are satisfied now
         -- this is different because we're trying to satisfy preds
         -- whether or not they contain ambiguous vars
         (remaining_ps, new_bindings)
             <- satisfyFV fixedVars givenPreds (apSub s ps)

         -- find the remaining ambiguous variables
         -- (this used to intersect the tyvars of the remaining preds with
         -- the original set of ambig vars, but that doesn't work after
         -- forcing, only after defaulting, so we do the real calculation)
         let new_fixedVarsSet = S.fromList (closeFD remaining_ps fixedVars)
             remaining_amb_set = S.fromList (concatMap tv remaining_ps)
                                     `S.difference` new_fixedVarsSet

         return (remaining_ps, new_bindings, remaining_amb_set)

   in
       if (S.null ambVarsSet) then
           -- no defaulting to do
           return (unsatisfiedPreds, emptySBs, [])
       else do
            let s0 = nullSubst
            s1 <- foldM tryDefaults s0 vars_with_preds

            let defaulted_vars = getSubstDomain s1
            -- extend the substitution
            extSubst "defaultClasses" s1

            -- numeric defaulting is now an unknown-size error
            let mkNumError v
                    | v `elem` defaulted_vars = [(getPosition v, EUnknownSize)]
                    | otherwise = []
                msgs = nub $ concatMap (mkNumError . fst) num_defaults
            when (not $ null msgs) $ errs "defaultClasses unknown size" msgs

            -- simplify, given what we know
            (remaining_preds, new_bindings, remaining_amb_set)
                <- makeResult ambVarsSet s1 unsatisfiedPreds

            -- see if we need to force any preds
            let typesToForce = concatMap (getForceDefaults fixedVarsSet)
                                   remaining_preds
            if (null typesToForce)
              then return (remaining_preds,
                           new_bindings,
                           S.toList remaining_amb_set)
              else do
                -- otherwise, force these variables and simplify again
                (res_ps, res_bs, res_aset)
                    <-  makeResult remaining_amb_set
                                   (mkSubst typesToForce)
                                   remaining_preds
                -- XXX issue a warning that the types have been forced?
                return (res_ps, res_bs, S.toList res_aset)

-------

-- Given the fixed variables from the context (that is, any vars in
-- the assumptions and any bound vars) and a qualified type, determine
-- if any of the preds in the qualified type are unsatisfiable because
-- they contain variables not fixed by the caller.

checkForAmbiguousPreds :: Id -> [TyVar] -> Qual Type -> TI ()
checkForAmbiguousPreds i fvs1 qt = do
  -- in order to catch ambiguous variables arising from uses of TLog etc,
  -- expand such TCons into additional predicates
  (qs :=> t) <- expandTCons qt

  let
        -- the explicit predicates
        ps = map removePredPositions qs

        -- fixed vars from the base type
        fvs2 = tv t
        -- all fixed vars
        fvs = fvs1 `union` fvs2
        -- close the fixed vars over fundeps
        cfvs = closeFD_pred ps fvs

        -- ambiguous variables in the predicates
        amb_vars = tv ps \\ cfvs

        hasAmbigVar p = any (`elem` amb_vars) (tv p)

        -- ambiguous predicates
        amb_preds = filter hasAmbigVar ps

        -- pprint
        pfp :: (PPrint a, PVPrint a) => a -> Doc
        pfp = pfPrint PDReadable 0

        -- rename generated typevars with "a__", "b__", etc
        -- (names are only generated by TCon expansion)
        (amb_preds', amb_vars', t') =
            niceTypes (amb_preds, map TVar amb_vars, t)

  if (null amb_vars)
        then return ()
        else err (getPosition i,
                  EAmbiguousExplCtx (map pfp amb_vars')
                                    (map pfp amb_preds')
                                    (pfp t'))

expandTCons :: Qual Type -> TI (Qual Type)
expandTCons (orig_qs :=> orig_t) =
  let
      mkResult cls_id ts v = do
        symt <- getSymTab
        let cls = mustFindClass symt (CTypeclass cls_id)
            p = PredWithPositions (IsIn cls ts) [] []
        return ([p], v)

      expQual :: PredWithPositions -> TI ([PredWithPositions], PredWithPositions)
      expQual (PredWithPositions (IsIn c ts) poss anc) = do
        (qs, ts') <- mapM exp ts >>= return . apFst concat . unzip
        return (qs, PredWithPositions (IsIn c ts') poss anc)

      -- all TCon could be expanded as NumEq#(var,TC) but we go ahead and
      -- expand to the associate typeclass (is this better for err msgs?)
      exp :: Type -> TI ([PredWithPositions], Type)
      exp (TAp tc t)
          | tc == tLog = do v <- newTVar "expandTCons" KNum tc
                            mkResult idLog [t, v] v
          | tc == tExp = do v <- newTVar "expandTCons" KNum tc
                            -- There's no class for this, so use NumEq
                            mkResult idNumEq [v, TAp tc t] v
      exp (TAp (TAp tc t1) t2)
          | tc == tAdd = do v <- newTVar "expandTCons" KNum tc
                            mkResult idAdd [t1, t2, v] v
          | tc == tSub = do v <- newTVar "expandTCons" KNum tc
                            mkResult idAdd [t1, v, t2] v
          | tc == tMul = do v <- newTVar "expandTCons" KNum tc
                            mkResult idMul [t1, t2, v] v
          | tc == tDiv = do v <- newTVar "expandTCons" KNum tc
                            mkResult idDiv [t1, t2, v] v
          | tc == tMax = do v <- newTVar "expandTCons" KNum tc
                            mkResult idMax [t1, t2, v] v
          | tc == tMin = do v <- newTVar "expandTCons" KNum tc
                            mkResult idMin [t1, t2, v] v
      -- Associated type functions: expand like expTFun
      exp t0
          | let (f, as) = splitTAp t0,
            TCon (TyCon _ _ (TIatf { atf_class_id = clsId
                                   , atf_param_idxs = pIdxs
                                   , atf_target_idx = tIdx })) <- f,
            length as == length pIdxs = do
                cls <- findCls (CTypeclass clsId)
                v <- newTVar "expandTCons" (kind (csig cls !! tIdx)) t0
                (_, classArgs) <- mkATFClassPred "expandTCons" t0
                                    clsId pIdxs tIdx as v
                let p = PredWithPositions (IsIn cls classArgs) [] []
                return ([p], v)
      exp (TAp t1 t2) = do (ps1, t1') <- exp t1
                           (ps2, t2') <- exp t2
                           return (ps1 ++ ps2, TAp t1' t2')
      exp t = return ([], t)
  in
      do
        (new_qs1, orig_qs') <- mapM expQual orig_qs >>=
                                   return . apFst concat . unzip
        (new_qs2, orig_t') <- exp orig_t
        return ((new_qs1 ++ new_qs2 ++ orig_qs') :=> orig_t')


-------

-- Propagation of functional dependencies was more complex
-- before we required fundeps to be full. Now that we have,
-- propagateFunDeps just uses satisfy (for full fundeps,
-- propagation and matching are the same thing).

-- propagateFunDeps returns the supplied predicates updated with
-- any information learned by matching instances. It does not
-- drop any matched predicates because it doesn't bind dictionaries.
-- propagateFunDeps also returns the (possibly smaller) list of
-- unsatisfied predicates for use in fail-fast context reduction
-- (see updateContexts)
propagateFunDeps :: [VPred] -> TI ([VPred], [VPred])
propagateFunDeps ps0 =
  do
    s <- getSubst
    eps <- getExplGivens

    let ps = apSub s ps0
    satTraceM("propagateFunDeps satisfy: " ++ ppReadable ps)

    -- SAT solving of numeric provisos isn't needed for propagating fundeps,
    -- so turn it off, since it appears to be expensive here
    flags <- getFlags
    setFlags (flags { useProvisoSAT = False })

    (ps_unsat, _) <- satisfyG eps ps

    -- restore the flags
    setFlags flags

    s' <- getSubst
    return (apSub s' ps, ps_unsat)

-------

-- like "reducePred", but we want to differentiate between those which
-- don't reduce now (not enough type info yet) and those which could
-- never reduce (which we'll want to report an error about now, not later)
--
isReduciblePred :: VPred -> TI Bool
isReduciblePred (VPred i pp@(PredWithPositions p@(IsIn c ts) pos _)) |
    -- XXX for now, we don't consider Add, Max, Min, Log, Mul, Div, NumEq
    -- XXX (the "genInsts" of these classes doesn't handle "maybe reducible")
    (isPreClass c) = return True
isReduciblePred (VPred i pp@(PredWithPositions p@(IsIn c ts) pos anc)) = do
    ts' <- mapM normT ts
    ai <- getAllowIncoherent
    bvs <- getBoundTVs
    let p' = IsIn c ts'
        pp' = PredWithPositions p' pos anc
        v' = VPred i pp'
        -- Under ordered-clause fundep semantics, an instance whose input
        -- positions match but whose determined positions conflict makes
        -- the predicate unsatisfiable -- final, unless an earlier (more
        -- specific) instance could still be selected once the inputs
        -- refine, which is possible exactly when that earlier instance
        -- unifies with the predicate.  The walk mirrors reducePred:
        -- track whether any earlier instance unifies, and treat a
        -- conflict as conclusive only when none does.  Incoherent
        -- classes (and the legacy hatch) retain fall-through, so a
        -- conflict is never conclusive for them.
        conflict_stops = not (outputFallThroughAllowed ai c)
        -- (matchExplPred compares the classes first)
        f _ [] = getExplGivens >>= (g . directEntries c)
        f unifiable_seen (i:is) = do
            (isReducible, may_capture) <- byInstIsReducible v' i
            -- if we discover that it could match or definitely
            -- does not match, then stop; otherwise, try the
            -- remaining instances
            case (isReducible) of
                Conflict | conflict_stops && not unifiable_seen
                           -> return False
                         | otherwise
                           -- fall-through (or an earlier instance may
                           -- yet be selected): inconclusive, keep
                           -- walking
                           -> f unifiable_seen is
                Match () -> return True
                _       -> f (unifiable_seen || may_capture) is
        g [] = return False
        g (ep:eps) = do isReducible <- byExplPredIsReducible v' ep
                        case (isReducible) of
                            Conflict -> return False
                            Match () -> return True
                            _       -> g eps
    ic <- currentClass c
    let is' = genInsts ic bvs Nothing p'
    r <- f False is'
    return r


-- Alongside the match result, report whether the instance could
-- still capture the predicate under refinement (a modal judgment;
-- see earlierInstanceMayCapture) -- isReduciblePred uses it to
-- decide whether a later conflict is conclusive.
byInstIsReducible :: VPred -> Inst -> TI (Match (), Bool)
byInstIsReducible (VPred i p) ii = do
    (mv, Inst e _ (ps :=> h) _) <- newInst ii (getPredPositions p)
    bound_tyvars <- getBoundTVs
    let p_bare = removePredPositions p
    m <- matchTopIsReducible bound_tyvars h p_bare
    return (m, earlierInstanceMayCapture p_bare h)


-- This is like "matchTop" except that we need to try matching first
-- and if it fails, then we try unifying.  Also, when we try matching,
-- we need to differentiate between failure to match the non-fundep types
-- (which means we still have a chance of matching somewhere else) and
-- matching the non-fundep types but failing to match the dependent types
-- (which is an immediate error, because the fundeps should be unique).
-- Returns:
--  * Conflict if the predicate can be proven unsatisfiable (because
--    the fundeps don't match the selected instance)
--  * Match () if the predicate matches (or could, after refinement,
--    match) this instance
--  * NoMatch for no conclusion from this instance
--
-- In TI because the unify path must normalize type functions under
-- the trial substitution before judging the determined positions.
matchTopIsReducible :: [TyVar] -> Pred -> Pred -> TI (Match ())
matchTopIsReducible bound_tyvars p1@(IsIn c1 ts1) p2@(IsIn c2 ts2) =
  if c1 /= c2 then
      return NoMatch
  else
      let
          try_match :: [Bool] -> TI (Match ())
          try_match bs =
              let nbs = map not bs
                  v1 = (boolCompress nbs ts1)
                  v2 = (boolCompress nbs ts2)
                  -- whether the non-fundeps match
                  mv = matchList v1 v2
                  -- whether the non-fundeps unify: a modal check ("could
                  -- this instance still match after refinement?"), so it
                  -- runs unguarded -- rigid variables are instantiable at
                  -- call sites, and the bound-variable guards would turn
                  -- "not yet" into "never"
                  uv = mgu [] v1 v2
                  -- check if the fundeps unify, given a subst for the
                  -- non-fundeps (modal, as above).  Normalize type
                  -- functions under the trial substitution first: a
                  -- predicate arising from a derived instance's context
                  -- carries ATF applications of its inputs (e.g.
                  -- "TilePred tag"), which only reduce once the
                  -- substitution grounds the inputs.  Comparing them
                  -- unreduced against the instance's ground outputs
                  -- misjudges a satisfiable-after-refinement predicate
                  -- as never reducible, and fail-fast context reduction
                  -- then reports a committed reduction's residual that
                  -- ordinary unification would have discharged.
                  check_fds s = do
                      fd1 <- mapM normT (apSub s (boolCompress bs ts1))
                      fd2 <- mapM normT (apSub s (boolCompress bs ts2))
                      return (isJust (mgu [] fd1 fd2))
              in
                  case (mv) of
                    Nothing ->
                        -- doesn't match, so try unify
                        case (uv) of
                           Nothing -> return NoMatch
                           Just (s,ty_eqs)  -> do
                               fds_ok <- check_fds s
                               return $ if (fds_ok && null ty_eqs)
                                        then Match ()
                                        else NoMatch
                    Just s  ->
                        -- the inputs match: the shared row core renders
                        -- the conclusive answer (a conflict is final
                        -- pending the caller's earlier-unifiable check;
                        -- a rigid deferral is no conclusion from this
                        -- instance)
                        return $ case matchFDRow bound_tyvars matchList ts1 ts2 bs of
                          Match _  -> Match ()
                          Defer    -> NoMatch
                          Conflict -> Conflict
                          NoMatch  -> NoMatch
      in
          do ms <- mapM try_match (funDeps c1)
             return (firstMatch ms)


byExplPredIsReducible :: VPred -> EPred -> TI (Match ())
byExplPredIsReducible (VPred _ p) (EPred _ ep) = do
    bound_tyvars <- getBoundTVs
    return $
        matchExplPred bound_tyvars (removePredPositions p) ep


matchExplPred :: [TyVar] -> Pred -> Pred -> Match ()
matchExplPred bound_tyvars p1@(IsIn c1 ts1) p2@(IsIn c2 ts2) =
  if c1 /= c2 then
      NoMatch
  else
      case (matchList ts1 ts2) of
          Nothing -> NoMatch
          Just s  -> Match ()

-------
