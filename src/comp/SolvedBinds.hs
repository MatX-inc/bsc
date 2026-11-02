-- | SolvedBinds are sets of solved dictionary bindings.
-- They group recursive and non-recursive dictionary bindings separately, ensure there are
-- no references to recursive bindings from non-recursive bindings and keep non-recursive
-- bindings in topologically sorted order.
module SolvedBinds(SolvedBind, mkSolvedBind, SolvedBinds, Bind,
                   markIncoherent, isIncoherent, solvedClass,
                   DirectIncoherence(..), addDirectIncoherence,
                   addBindDeps,
                   sbsEmpty, fromSB, (<++), emptySBs,
                   recursiveBinds, nonRecursiveBinds,
                   bindClasses, bindTypes, directIncoherences,
                   getRecursiveDefls, getNonRecursiveDefls,
                   getIncoherentIds, computeTransitiveIncoherent,
                   bindTyVars, renameBinds) where

import Prelude hiding ((<>))

import Data.List(union, partition)
import Data.Maybe(listToMaybe)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import ErrorUtil(internalError)
import Id
import CSyntax
import CSyntaxTypes()
import CFreeVars(getFVE)
import CSubst(CSEnv, cSubstN)
import CType(hasTyVar)
import PPrint
import Position
import Pred(Class, Pred)
import Subst

-- Dictionary binding representation
-- The CExpr produces the dictionary value by constructing it or referring to another dictionary
type Bind = (Id, Type, CExpr)

mkDefl :: Bind -> CDefl
mkDefl (i, t, e) = CLValueSign (CDefT i [] (CQType [] t) [CClause [] [] e]) []

-- Root cause of a direct incoherent match, for use in transitive-incoherence warnings.
data DirectIncoherence = DirectIncoherence {
  diPred :: Pred,    -- the pred being satisfied (after final substitution)
  diInst :: Pred,    -- the incoherent instance head (after final substitution)
  diPos  :: Position -- position where the direct match occurred
} deriving (Show)

data SolvedBind = SolvedBind {
  bind :: Bind,
  freeVars :: S.Set Id,
  isRecursive :: Bool,
  isIncoherent :: Bool,
  solvedClass :: Maybe Class  -- the class this bind resolves (for allowIncoherent check)
} deriving (Show)

instance PPrint SolvedBind where
  pPrint d p (SolvedBind bind fv isRec isInc _) =
    text "SolvedBind" <+> braces (
      text "bind:" <+> pPrint d p bind <> semi <+>
      text "freeVars:" <+> pPrint d p fv <> semi <+>
      text "isRecursive:" <+> pPrint d p isRec <> semi <+>
      text "isIncoherent:" <+> pPrint d p isInc
    )

mkSolvedBind :: Bind -> Bool -> SolvedBind
mkSolvedBind b@(_,_,e) isRec =
  SolvedBind { bind = b, freeVars = snd (getFVE e), isRecursive = isRec,
               isIncoherent = False, solvedClass = Nothing }

markIncoherent :: SolvedBind -> SolvedBind
markIncoherent sb = sb { bind = mark (bind sb), isIncoherent = True }
  where mark (i, t, e) = (addIdProp i IdPIncoherent, t, e)

-- Record additional semantic dependencies of a binding that its
-- expression does not reference: the dictionaries of the numeric
-- equalities deferred by the fundep improvement that produced it.
-- The binding is only valid evidence if those equalities hold, so
-- they must be visible to extractClosures' completeness walk (an
-- unresolved equality forbids reuse; a solved one is walked through).
addBindDeps :: [Id] -> SolvedBind -> SolvedBind
addBindDeps is sb = sb { freeVars = foldr S.insert (freeVars sb) is }

-- Collection of bindings categorized by recursion
-- nonRecursiveBinds are maintained in topologically sorted order
data SolvedBinds = SolvedBinds {
  recursiveBinds :: [(Bind, S.Set Id)], -- binding and free variables
  nonRecursiveBinds :: [(Bind, S.Set Id)],
  recursiveIds :: S.Set Id,
  nonRecursiveIds :: S.Set Id,
  incoherentIds :: S.Set Id,
  bindClasses :: M.Map Id Class,                   -- class per bind (for allowIncoherent check)
  bindTypes :: M.Map Id Type,                      -- type per bind (for diagnostic messages)
  directIncoherences :: M.Map Id DirectIncoherence, -- root cause per directly-incoherent bind
  -- the type variables of the bindings' types and expressions, carried
  -- along the merges so that a record of a block (TCMisc.recordWalk)
  -- does not walk the types of every block merged into it again
  bindTyVars :: S.Set TyVar
} deriving (Show)

instance Types SolvedBinds where
  apSub s sbs = sbs {
    recursiveBinds = [ ((i, apSub s t, apSub s e), fv) | ((i, t, e), fv) <- recursiveBinds sbs ],
    nonRecursiveBinds = [ ((i, apSub s t, apSub s e), fv) | ((i, t, e), fv) <- nonRecursiveBinds sbs ],
    bindTypes = M.map (apSub s) (bindTypes sbs),
    bindTyVars = substTyVars s (bindTyVars sbs)
  }
  tv sbs = recTVs `union` nonRecTVs
    where recTVs    = tv [ (t, e) | ((_, t, e), _) <- recursiveBinds sbs ]
          nonRecTVs = tv [ (t, e) | ((_, t, e), _) <- nonRecursiveBinds sbs ]

sbsEmpty :: SolvedBinds -> Bool
sbsEmpty sbs = null (recursiveBinds sbs) && null (nonRecursiveBinds sbs)

-- Create singleton SolvedBinds from SolvedBind
-- Note: Both self-recursive and non-recursive bindings are independent of accum
-- Self-recursive bindings depend only on themselves, fresh variables, and source EPreds
fromSB :: SolvedBind -> SolvedBinds
fromSB (SolvedBind b@(i, t, e) fv isRec isInc cls) =
  if isRec
    then SolvedBinds {
      recursiveBinds = [(b, fv)],
      nonRecursiveBinds = [],
      recursiveIds = S.singleton i,
      nonRecursiveIds = S.empty,
      incoherentIds = if isInc then S.singleton i else S.empty,
      bindClasses = maybe M.empty (M.singleton i) cls,
      bindTypes = M.singleton i t,
      directIncoherences = M.empty,
      bindTyVars = vs
    }
    else SolvedBinds {
      recursiveBinds = [],
      nonRecursiveBinds = [(b, fv)],
      recursiveIds = S.empty,
      nonRecursiveIds = S.singleton i,
      incoherentIds = if isInc then S.singleton i else S.empty,
      bindClasses = maybe M.empty (M.singleton i) cls,
      bindTypes = M.singleton i t,
      directIncoherences = M.empty,
      bindTyVars = vs
    }
  where vs = exprTyVars e (typeTyVars t S.empty)

infixl 6 <++ -- directional append new <++ old

-- Merge bindings, maintaining categorization and topological order
-- CRITICAL: Always merge as (new <++ old), never swap!
-- Invariant: new.nonRec doesn't depend on old
-- The invariants are checked at the first demand of either list of
-- bindings, not when the blocks are merged: recording a block for
-- sharing (TCMisc.recordWalk) demands neither list, so it runs no
-- check; the checks run where a list is first demanded: for a
-- recorded block, at the first lookup hit on it, where TCMisc.sat's
-- wholeBlock tests its recursive list (whether or not the replay then
-- proceeds), or by the consumer of the satisfy call's result, whose
-- lists contain the block's, whichever comes first; a replay's
-- renameBinds walks the lists after wholeBlock.
(<++) :: SolvedBinds -> SolvedBinds -> SolvedBinds
new <++ old = result
    where dependsOn s ((_,_,_), fv) = not $ S.disjoint fv s
          checked xs
            | any (dependsOn oldAllIds) (nonRecursiveBinds new) =
                internalError $ "SolvedBinds invariant violated: new depends on old!\n" ++
                                "new: " ++ ppReadable new ++ "\n" ++
                                "old: " ++ ppReadable old
            | noBadDeps = xs
            -- printed with the unchecked lists: the checked ones are the
            -- thunks under evaluation
            | otherwise = internalError $ "nonRecursive depending on recursive in result: " ++
                                          ppReadable (new, old, result { recursiveBinds = recs,
                                                                         nonRecursiveBinds = nonRec })
          oldAllIds = recursiveIds old `S.union` nonRecursiveIds old
          promoteTransitively [] nonRecs = ([], nonRecs)
          promoteTransitively promoted nonRecs = (promoted ++ transitivePromoted, finalNonRecs)
            where promotedIds = S.fromList [ i | ((i, _, _), _) <- promoted ]
                  (newlyPromoted, stillNonRecs) = partition (dependsOn promotedIds) nonRecs
                  (transitivePromoted, finalNonRecs) = promoteTransitively newlyPromoted stillNonRecs
          (newlyRec, stillNonRec) = uncurry promoteTransitively $
                                    partition (dependsOn $ recursiveIds new) (nonRecursiveBinds old)
          -- Updates for cached sets
          newRecIds = S.fromList [i | ((i, _, _), _) <- recursiveBinds new ++ newlyRec]
          nowNotNonRecIds = S.fromList [ i | ((i, _ , _), _)  <- newlyRec ]
          recs = newlyRec ++ recursiveBinds new ++ recursiveBinds old
          nonRec = nonRecursiveBinds new ++ stillNonRec
          result = SolvedBinds {
                      recursiveBinds = checked recs,
                      nonRecursiveBinds = checked nonRec,
                      recursiveIds = newRecIds `S.union` recursiveIds old,
                      nonRecursiveIds = nonRecursiveIds new `S.union` (nonRecursiveIds old `S.difference` nowNotNonRecIds),
                      incoherentIds = incoherentIds new `S.union` incoherentIds old,
                      bindClasses = bindClasses new `M.union` bindClasses old,
                      bindTypes = bindTypes new `M.union` bindTypes old,
                      directIncoherences = directIncoherences new `M.union` directIncoherences old,
                      bindTyVars = bindTyVars new `S.union` bindTyVars old
                   }
          noBadDeps = all (not . dependsOn (recursiveIds result)) nonRec

-- Compute the transitive closure of incoherence across all bindings: any
-- binding whose free variables reference an incoherent Id is itself incoherent.
-- Works across both recursive and non-recursive bindings simultaneously.
-- Marks newly-incoherent binding Ids with IdPIncoherent and updates incoherentIds.
-- Also propagates DirectIncoherence root-cause info through the closure.
-- Call this once on a fully-merged SolvedBinds before consuming it.
computeTransitiveIncoherent :: SolvedBinds -> SolvedBinds
computeTransitiveIncoherent sbs = sbs {
    recursiveBinds    = map markBind (recursiveBinds sbs),
    nonRecursiveBinds = map markBind (nonRecursiveBinds sbs),
    incoherentIds     = finalIds,
    directIncoherences = propagatedSources
  }
  where
    direct = incoherentIds sbs
    allBindings = recursiveBinds sbs ++ nonRecursiveBinds sbs
    finalIds = go direct
    go known
      | S.null newIds = known
      | otherwise    = go (known `S.union` newIds)
      where newIds = S.fromList [ i | ((i, _, _), fv) <- allBindings
                                    , not (S.member i known)
                                    , not (S.disjoint fv known) ]
    markBind b@((i, t, e), fv)
      | S.member i finalIds && not (S.member i direct) =
          ((addIdProp i IdPIncoherent, t, e), fv)
      | otherwise = b
    -- Propagate root-cause DirectIncoherence through the transitive closure:
    -- for each newly-transitively-incoherent bind, inherit the source from
    -- the first free var that already has a known source.
    propagateSources known
      | M.null newEntries = known
      | otherwise = propagateSources (M.union known newEntries)
      where
        newEntries = M.fromList
          [ (i, src)
          | ((i, _, _), fv) <- allBindings
          , S.member i finalIds
          , not (M.member i known)
          , Just src <- [listToMaybe [s | j <- S.toList fv
                                        , Just s <- [M.lookup j known]]]
          ]
    propagatedSources = propagateSources (directIncoherences sbs)

-- Record the root cause of a direct incoherent match for the given dict Id.
-- Called from sat after niceTypes provides the pretty-printed pred/inst strings.
addDirectIncoherence :: Id -> DirectIncoherence -> SolvedBinds -> SolvedBinds
addDirectIncoherence i di sbs =
  sbs { directIncoherences = M.insert i di (directIncoherences sbs) }

emptySBs :: SolvedBinds
emptySBs = SolvedBinds {
             recursiveBinds = [],
             nonRecursiveBinds = [],
             recursiveIds = S.empty,
             nonRecursiveIds = S.empty,
             incoherentIds = S.empty,
             bindClasses = M.empty,
             bindTypes = M.empty,
             directIncoherences = M.empty,
             bindTyVars = S.empty
           }

instance PPrint SolvedBinds where
  pPrint d p sbs =
    text "SolvedBinds" <+> braces (
      text "rec:" <+> pPrint d p (recursiveBinds sbs) <> semi <+>
      text "nonRec:" <+> pPrint d p (nonRecursiveBinds sbs)
    )

getRecursiveDefls :: SolvedBinds -> [CDefl]
getRecursiveDefls = map (mkDefl . fst) . recursiveBinds

getNonRecursiveDefls :: SolvedBinds -> [CDefl]
getNonRecursiveDefls = map (mkDefl . fst) . nonRecursiveBinds

getIncoherentIds :: SolvedBinds -> S.Set Id
getIncoherentIds = incoherentIds

-- The same bindings under other names: the substitution renames (or
-- binds) type variables, the map sends every binding's id to its new
-- id, and references to ids outside the map (the given dictionaries)
-- stay.  Used by TCMisc.replaySolvedBinds to give a later occurrence
-- of a ground predicate the derivation recorded for the first one under
-- the names the instance walk would draw now.  Only non-recursive,
-- coherent bindings are copied (sat records no others), so the
-- recursive and incoherence fields are checked empty.  A type or
-- expression without a type variable is kept rather than copied.
renameBinds :: Subst -> M.Map Id Id -> SolvedBinds -> SolvedBinds
renameBinds s m sbs
  | not (null (recursiveBinds sbs)) || not (S.null (incoherentIds sbs)) =
      internalError ("SolvedBinds.renameBinds: recursive or incoherent: " ++
                     ppReadable sbs)
  | otherwise = sbs {
      nonRecursiveBinds = [ ((ren i, subT t, cSubstN env (subE e)), S.map ren fv)
                          | ((i, t, e), fv) <- nonRecursiveBinds sbs ],
      nonRecursiveIds = S.map ren (nonRecursiveIds sbs),
      bindClasses = M.mapKeys ren (bindClasses sbs),
      bindTypes = M.map subT (M.mapKeys ren (bindTypes sbs)),
      bindTyVars = substTyVars s (bindTyVars sbs) }
  where ren i = M.findWithDefault i i m
        ground = isNullSubst s
        subT t = if ground || not (hasTyVar t) then t else apSub s t
        subE e = if ground || S.null (exprTyVars e S.empty) then e else apSub s e
        env :: CSEnv
        env = (M.empty, M.empty, M.map CVar m, M.empty)

-- The type variables of the types the substitution gives these
-- variables (a substituted block's bindTyVars)
substTyVars :: Subst -> S.Set TyVar -> S.Set TyVar
substTyVars s vs
  | isNullSubst s = vs
  | otherwise = S.foldr (\ v acc -> typeTyVars (apSub s (TVar v)) acc) S.empty vs

-- The type variables of a binding's type and expression, collected
-- without the lists tv builds
typeTyVars :: Type -> S.Set TyVar -> S.Set TyVar
typeTyVars (TVar v) acc = S.insert v acc
typeTyVars (TAp l r) acc = typeTyVars l (typeTyVars r acc)
typeTyVars _ acc = acc

-- A dictionary expression is an instance applied to its types and its
-- context's dictionaries (TCMisc.byInst); any other shape goes through tv
exprTyVars :: CExpr -> S.Set TyVar -> S.Set TyVar
exprTyVars (CVar _) acc = acc
exprTyVars (CApply f es) acc = foldr exprTyVars (exprTyVars f acc) es
exprTyVars (CTApply e ts) acc = foldr typeTyVars (exprTyVars e acc) ts
exprTyVars e acc = foldr S.insert acc (tv e)
