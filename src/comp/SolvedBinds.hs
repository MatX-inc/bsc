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
                   Shape(..), Deriv, sbsDeriv,
                   derivTyVars, derivTypes, derivBinders, derivWhole,
                   Renum(..), replayDeriv, exprTyVars) where

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
import PPrint
import Position
import Pred(Class, Pred)
import StdPrel(mkNumInstBody)
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

-- How a binding's expression refers to the dictionaries of the instance
-- walk that built it.  The solver names a dictionary variable by the
-- counter number newDict drew for it (TIMonad.VPred carries the
-- number), never by its identifier, so that a recorded derivation
-- (Deriv) can be rebuilt under other numbers (replayDeriv) without
-- evaluating an identifier: the spelling of a generated identifier is
-- interned when it is first evaluated, and the interning order reaches
-- the .bo through IConv.iConvLet's ordering of a recursive dictionary
-- group, so the sharing of derivations must evaluate none before the
-- compiler without sharing would.
data Shape = ShInst CExpr [Int]  -- an instance's dictionary (TCMisc.byInst:
                                 -- the instance expression with its types
                                 -- substituted) applied to the dictionaries
                                 -- of its context, by number
           | ShVar !Int          -- another predicate's dictionary
                                 -- (TCMisc.joinCtxs)
           | ShOuter CExpr       -- an expression from outside the walk: a
                                 -- given, a superclass selection from one
                                 -- or an entry of the sat stack (a
                                 -- recursive knot); never renumbered
           | ShNum               -- a numeric class instance, a function of
                                 -- the binding's type (StdPrel.mkNumInstBody)
  deriving (Show)

data SolvedBind = SolvedBind {
  bind :: Bind,
  freeVars :: S.Set Id,
  isRecursive :: Bool,
  isIncoherent :: Bool,
  solvedClass :: Maybe Class,  -- the class this bind resolves (for allowIncoherent check)
  -- the binding without its identifiers, for a record of the walk
  -- that built it (see Shape)
  binderNum :: Int,            -- the bound dictionary's number
  shape :: Shape,
  depNums :: [Int]             -- the numbers of the dictionaries
                               -- addBindDeps added
} deriving (Show)

instance PPrint SolvedBind where
  pPrint d p (SolvedBind bind fv isRec isInc _ _ _ _) =
    text "SolvedBind" <+> braces (
      text "bind:" <+> pPrint d p bind <> semi <+>
      text "freeVars:" <+> pPrint d p fv <> semi <+>
      text "isRecursive:" <+> pPrint d p isRec <> semi <+>
      text "isIncoherent:" <+> pPrint d p isInc
    )

mkSolvedBind :: Int -> Shape -> Bind -> Bool -> SolvedBind
mkSolvedBind n sh b@(_,_,e) isRec =
  SolvedBind { bind = b, freeVars = snd (getFVE e), isRecursive = isRec,
               isIncoherent = False, solvedClass = Nothing,
               binderNum = n, shape = sh, depNums = [] }

markIncoherent :: SolvedBind -> SolvedBind
markIncoherent sb = sb { bind = mark (bind sb), isIncoherent = True }
  where mark (i, t, e) = (addIdProp i IdPIncoherent, t, e)

-- Record additional semantic dependencies of a binding that its
-- expression does not reference: the dictionaries of the numeric
-- equalities deferred by the fundep improvement that produced it.
-- The binding is only valid evidence if those equalities hold, so
-- they must be visible to extractClosures' completeness walk (an
-- unresolved equality forbids reuse; a solved one is walked through).
addBindDeps :: [(Id, Int)] -> SolvedBind -> SolvedBind
addBindDeps is sb = sb { freeVars = foldr (S.insert . fst) (freeVars sb) is,
                         depNums = map snd is ++ depNums sb }

-- Collection of bindings categorized by recursion
-- nonRecursiveBinds are maintained in topologically sorted order
data Block = Block {
  recursiveBindsB :: [(Bind, S.Set Id)], -- binding and free variables
  nonRecursiveBindsB :: [(Bind, S.Set Id)],
  recursiveIdsB :: S.Set Id,
  nonRecursiveIdsB :: S.Set Id,
  incoherentIdsB :: S.Set Id,
  bindClassesB :: M.Map Id Class,                   -- class per bind (for allowIncoherent check)
  bindTypesB :: M.Map Id Type,                      -- type per bind (for diagnostic messages)
  directIncoherencesB :: M.Map Id DirectIncoherence -- root cause per directly-incoherent bind
} deriving (Show)

instance Types Block where
  apSub s sbs = sbs {
    recursiveBindsB = [ ((i, apSub s t, apSub s e), fv) | ((i, t, e), fv) <- recursiveBindsB sbs ],
    nonRecursiveBindsB = [ ((i, apSub s t, apSub s e), fv) | ((i, t, e), fv) <- nonRecursiveBindsB sbs ],
    bindTypesB = M.map (apSub s) (bindTypesB sbs)
  }
  tv sbs = recTVs `union` nonRecTVs
    where recTVs    = tv [ (t, e) | ((_, t, e), _) <- recursiveBindsB sbs ]
          nonRecTVs = tv [ (t, e) | ((_, t, e), _) <- nonRecursiveBindsB sbs ]

-- The bindings (the block) with a description of how the solver built
-- them (the derivation), which TCMisc.recordWalk keeps for a ground
-- predicate and TCMisc.replaySolvedBinds rebuilds for a later equal
-- one.  The block is a lazy field holding the merge, so building or
-- merging a SolvedBinds evaluates nothing of the bindings: the merge
-- checks its invariants when the block is first demanded (by the
-- consumer of the satisfy call's result), as it did before derivations
-- were recorded.
data SolvedBinds = SolvedBinds {
  sbsDeriv :: Deriv,
  sbsBlock :: Block
}

instance Show SolvedBinds where
  showsPrec d sbs = showsPrec d (sbsBlock sbs)

-- A substitution applied to the block leaves no derivation: the
-- recorded form holds the walk's types as built, and the walk's own
-- substitution is part of the record (TCMisc.recordWalk)
instance Types SolvedBinds where
  apSub s sbs = SolvedBinds { sbsDeriv = DOpaque, sbsBlock = apSub s (sbsBlock sbs) }
  tv sbs = tv (sbsBlock sbs)

recursiveBinds :: SolvedBinds -> [(Bind, S.Set Id)]
recursiveBinds = recursiveBindsB . sbsBlock

nonRecursiveBinds :: SolvedBinds -> [(Bind, S.Set Id)]
nonRecursiveBinds = nonRecursiveBindsB . sbsBlock

bindClasses :: SolvedBinds -> M.Map Id Class
bindClasses = bindClassesB . sbsBlock

bindTypes :: SolvedBinds -> M.Map Id Type
bindTypes = bindTypesB . sbsBlock

directIncoherences :: SolvedBinds -> M.Map Id DirectIncoherence
directIncoherences = directIncoherencesB . sbsBlock

sbsEmpty :: SolvedBinds -> Bool
sbsEmpty sbs = null (recursiveBinds sbs) && null (nonRecursiveBinds sbs)

-- Create singleton SolvedBinds from SolvedBind
-- Note: Both self-recursive and non-recursive bindings are independent of accum
-- Self-recursive bindings depend only on themselves, fresh variables, and source EPreds
fromSB :: SolvedBind -> SolvedBinds
fromSB sb = SolvedBinds { sbsDeriv = DLeaf (leafOf sb), sbsBlock = blockFromSB sb }

blockFromSB :: SolvedBind -> Block
blockFromSB (SolvedBind b@(i, t, _) fv isRec isInc cls _ _ _) =
  if isRec
    then Block {
      recursiveBindsB = [(b, fv)],
      nonRecursiveBindsB = [],
      recursiveIdsB = S.singleton i,
      nonRecursiveIdsB = S.empty,
      incoherentIdsB = if isInc then S.singleton i else S.empty,
      bindClassesB = maybe M.empty (M.singleton i) cls,
      bindTypesB = M.singleton i t,
      directIncoherencesB = M.empty
    }
    else Block {
      recursiveBindsB = [],
      nonRecursiveBindsB = [(b, fv)],
      recursiveIdsB = S.empty,
      nonRecursiveIdsB = S.singleton i,
      incoherentIdsB = if isInc then S.singleton i else S.empty,
      bindClassesB = maybe M.empty (M.singleton i) cls,
      bindTypesB = M.singleton i t,
      directIncoherencesB = M.empty
    }

infixl 6 <++ -- directional append new <++ old

-- Merge bindings, maintaining categorization and topological order
-- CRITICAL: Always merge as (new <++ old), never swap!
-- Invariant: new.nonRec doesn't depend on old
(<++) :: SolvedBinds -> SolvedBinds -> SolvedBinds
new <++ old = SolvedBinds { sbsDeriv = mergeDerivs (sbsDeriv new) (sbsDeriv old),
                            sbsBlock = mergeBlocks (sbsBlock new) (sbsBlock old) }

mergeBlocks :: Block -> Block -> Block
mergeBlocks new old
    -- Check invariant here
    | any (dependsOn oldAllIds) (nonRecursiveBindsB new) =
        internalError $ "SolvedBinds invariant violated: new depends on old!\n" ++
                        "new: " ++ ppReadable new ++ "\n" ++
                        "old: " ++ ppReadable old
    | noBadDeps = result
    | otherwise = internalError $ "nonRecursive depending on recursive in result: " ++
                                  ppReadable (new, old, result)
    where dependsOn s ((_,_,_), fv) = not $ S.disjoint fv s
          oldAllIds = recursiveIdsB old `S.union` nonRecursiveIdsB old
          promoteTransitively [] nonRecs = ([], nonRecs)
          promoteTransitively promoted nonRecs = (promoted ++ transitivePromoted, finalNonRecs)
            where promotedIds = S.fromList [ i | ((i, _, _), _) <- promoted ]
                  (newlyPromoted, stillNonRecs) = partition (dependsOn promotedIds) nonRecs
                  (transitivePromoted, finalNonRecs) = promoteTransitively newlyPromoted stillNonRecs
          (newlyRec, stillNonRec) = uncurry promoteTransitively $
                                    partition (dependsOn $ recursiveIdsB new) (nonRecursiveBindsB old)
          -- Updates for cached sets
          newRecIds = S.fromList [i | ((i, _, _), _) <- recursiveBindsB new ++ newlyRec]
          nowNotNonRecIds = S.fromList [ i | ((i, _ , _), _)  <- newlyRec ]
          result = Block {
                      recursiveBindsB = newlyRec ++ recursiveBindsB new ++ recursiveBindsB old,
                      nonRecursiveBindsB = nonRecursiveBindsB new ++ stillNonRec,
                      recursiveIdsB = newRecIds `S.union` recursiveIdsB old,
                      nonRecursiveIdsB = nonRecursiveIdsB new `S.union` (nonRecursiveIdsB old `S.difference` nowNotNonRecIds),
                      incoherentIdsB = incoherentIdsB new `S.union` incoherentIdsB old,
                      bindClassesB = bindClassesB new `M.union` bindClassesB old,
                      bindTypesB = bindTypesB new `M.union` bindTypesB old,
                      directIncoherencesB = directIncoherencesB new `M.union` directIncoherencesB old
                   }
          noBadDeps = all (not . dependsOn (recursiveIdsB result)) (nonRecursiveBindsB result)

-- Compute the transitive closure of incoherence across all bindings: any
-- binding whose free variables reference an incoherent Id is itself incoherent.
-- Works across both recursive and non-recursive bindings simultaneously.
-- Marks newly-incoherent binding Ids with IdPIncoherent and updates incoherentIds.
-- Also propagates DirectIncoherence root-cause info through the closure.
-- Call this once on a fully-merged SolvedBinds before consuming it.
computeTransitiveIncoherent :: SolvedBinds -> SolvedBinds
computeTransitiveIncoherent sbs =
  SolvedBinds { sbsDeriv = DOpaque, sbsBlock = closeBlock (sbsBlock sbs) }

closeBlock :: Block -> Block
closeBlock sbs = sbs {
    recursiveBindsB    = map markBind (recursiveBindsB sbs),
    nonRecursiveBindsB = map markBind (nonRecursiveBindsB sbs),
    incoherentIdsB     = finalIds,
    directIncoherencesB = propagatedSources
  }
  where
    direct = incoherentIdsB sbs
    allBindings = recursiveBindsB sbs ++ nonRecursiveBindsB sbs
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
    propagatedSources = propagateSources (directIncoherencesB sbs)

-- Record the root cause of a direct incoherent match for the given dict Id.
-- Called from sat after niceTypes provides the pretty-printed pred/inst strings.
addDirectIncoherence :: Id -> DirectIncoherence -> SolvedBinds -> SolvedBinds
addDirectIncoherence i di sbs =
  SolvedBinds { sbsDeriv = DOpaque,
                sbsBlock = blk { directIncoherencesB = M.insert i di (directIncoherencesB blk) } }
  where blk = sbsBlock sbs

emptySBs :: SolvedBinds
emptySBs = SolvedBinds { sbsDeriv = DEmpty, sbsBlock = emptyBlock }

emptyBlock :: Block
emptyBlock = Block {
             recursiveBindsB = [],
             nonRecursiveBindsB = [],
             recursiveIdsB = S.empty,
             nonRecursiveIdsB = S.empty,
             incoherentIdsB = S.empty,
             bindClassesB = M.empty,
             bindTypesB = M.empty,
             directIncoherencesB = M.empty
           }

instance PPrint Block where
  pPrint d p sbs =
    text "SolvedBinds" <+> braces (
      text "rec:" <+> pPrint d p (recursiveBindsB sbs) <> semi <+>
      text "nonRec:" <+> pPrint d p (nonRecursiveBindsB sbs)
    )

instance PPrint SolvedBinds where
  pPrint d p sbs = pPrint d p (sbsBlock sbs)

getRecursiveDefls :: SolvedBinds -> [CDefl]
getRecursiveDefls = map (mkDefl . fst) . recursiveBinds

getNonRecursiveDefls :: SolvedBinds -> [CDefl]
getNonRecursiveDefls = map (mkDefl . fst) . nonRecursiveBinds

getIncoherentIds :: SolvedBinds -> S.Set Id
getIncoherentIds = incoherentIdsB . sbsBlock

-- ---------------
-- The derivation of a block: the fromSB and (<++) calls the solver
-- made, with each binding kept without its identifiers (Leaf), and the
-- blocks a replay inserted (DReplay), with the renumbering the replay
-- applied to the recorded derivation beneath.  TCMisc.recordWalk keeps
-- the derivation of a ground predicate's walk, with the numbers the
-- walk drew from the counter (TIMonad.WalkSpan); replayDeriv rebuilds
-- the block for a later occurrence under the numbers that occurrence
-- draws, by the same fromSB and merge calls, so that the consumer
-- evaluates the same bindings in the same order.  Numbering invariant:
-- a leaf of a recorded walk refers only to numbers of the walk's span
-- and to the walk's root (the predicate's own dictionary number); the
-- numbers in a subtree under DReplay are the recorded entry's own, and
-- its Replayed maps them into the numbering of the enclosing walk.
-- DOpaque: a block another operation changed (a substitution, the
-- incoherence closure), which no derivation describes; a walk whose
-- block holds one is not recorded.

-- A dictionary binding as the solver built it, without its identifiers
data Leaf = Leaf {
  lfBinder :: !Int,       -- the bound dictionary's number
  lfType :: Type,         -- the binding's type, shared with the Bind
  lfShape :: !Shape,
  lfDeps :: [Int],        -- the dictionaries addBindDeps added
  lfRecursive :: !Bool,
  lfIncoherent :: !Bool,
  lfClass :: Maybe Class
}

data Deriv = DEmpty
           | DLeaf !Leaf
           | DMerge DerivInfo Deriv Deriv   -- new <++ old
           | DReplay Replayed Deriv
           | DOpaque

-- What a record asks of a subtree, computed once per merge (the fields
-- are lazy: a record demands them, nothing else does), so that a
-- record nested in another is not walked again by the enclosing one
data DerivInfo = DerivInfo {
  diTyVars :: S.Set TyVar,  -- the type variables of the leaves' types
                            -- and expressions
  diWhole :: Bool           -- no leaf recursive or incoherent, no
                            -- DOpaque: a block a replay may rebuild
}

-- The renumbering a replay applied to a recorded derivation, kept with
-- the replayed subtree so that a record of the enclosing walk can be
-- replayed in turn (nestRenum)
data Replayed = Replayed {
  rpNum :: Int -> Int,       -- a recorded number to the number it has in
                             -- the enclosing walk (the entry's root to
                             -- the occurrence's own number)
  rpType :: Type -> Type,    -- a recorded type to its form in the
                             -- enclosing walk
  rpExpr :: CExpr -> CExpr,  -- likewise for an instance expression
  rpTyVars :: S.Set TyVar    -- diTyVars of the replayed subtree
}

leafOf :: SolvedBind -> Leaf
leafOf sb =
  case bind sb of
    (_, t, _) -> Leaf { lfBinder = binderNum sb, lfType = t, lfShape = shape sb,
                        lfDeps = depNums sb, lfRecursive = isRecursive sb,
                        lfIncoherent = isIncoherent sb, lfClass = solvedClass sb }

mergeDerivs :: Deriv -> Deriv -> Deriv
mergeDerivs a b = DMerge info a b
  where info = DerivInfo { diTyVars = S.union (derivTyVars a) (derivTyVars b),
                           diWhole = derivWhole a && derivWhole b }

-- The type variables of the leaves' types and expressions, in the
-- numbering of the enclosing walk (the type variables of the Bind
-- values a replay would build)
derivTyVars :: Deriv -> S.Set TyVar
derivTyVars DEmpty = S.empty
derivTyVars (DLeaf lf) = leafTyVars lf
derivTyVars (DMerge info _ _) = diTyVars info
derivTyVars (DReplay rp _) = rpTyVars rp
derivTyVars DOpaque = S.empty

-- A dictionary expression is an instance applied to its types and its
-- context's dictionaries (TCMisc.byInst), another dictionary, an
-- expression from outside the walk, or a numeric instance body
-- (CStructT over the binding's type, StdPrel.mkNumInstBody)
leafTyVars :: Leaf -> S.Set TyVar
leafTyVars lf = typeTyVars (lfType lf) exprVars
  where exprVars = case lfShape lf of
                     ShInst e _ -> exprTyVars e S.empty
                     ShVar _ -> S.empty
                     ShOuter e -> exprTyVars e S.empty
                     ShNum -> S.empty

-- Whether a replay may rebuild the block: no recursive binding (a
-- knot on the sat stack or a self-recursive instance), no incoherent
-- match, nothing opaque.  A replayed subtree is whole: only a whole
-- block is recorded.
derivWhole :: Deriv -> Bool
derivWhole DEmpty = True
derivWhole (DLeaf lf) = not (lfRecursive lf) && not (lfIncoherent lf)
derivWhole (DMerge info _ _) = diWhole info
derivWhole (DReplay _ _) = True
derivWhole DOpaque = False

-- The leaves' types, those under a replay in the numbering of the
-- enclosing walk (TCMisc.derivationUnchanged)
derivTypes :: Deriv -> [Type]
derivTypes = go id []
  where go _ acc DEmpty = acc
        go f acc (DLeaf lf) = f (lfType lf) : acc
        go f acc (DMerge _ a b) = go f (go f acc b) a
        go f acc (DReplay rp sub) = go (f . rpType rp) acc sub
        go _ acc DOpaque = acc

-- The leaves' binder numbers, those under a replay in the numbering of
-- the enclosing walk (TCMisc.replaySolvedBinds)
derivBinders :: Deriv -> [Int]
derivBinders = go id []
  where go _ acc DEmpty = acc
        go f acc (DLeaf lf) = f (lfBinder lf) : acc
        go f acc (DMerge _ a b) = go f (go f acc b) a
        go f acc (DReplay rp sub) = go (f . rpNum rp) acc sub
        go _ acc DOpaque = acc

-- The renumbering a replay applies to a recorded derivation
-- (TCMisc.replaySolvedBinds): what a recorded number is now, the
-- dictionary variable it names now (the entry's root: the occurrence's
-- own variable; any other: an identifier built from the new number,
-- unevaluated), and the recorded types and instance expressions with
-- the walk's variables renumbered and its own substitution applied.
-- rnNum raises an internalError on a number the walk did not draw
-- that is not its root (the numbering invariant above).
data Renum = Renum {
  rnNum :: Int -> Int,
  rnDict :: Int -> Id,
  rnType :: Type -> Type,
  rnExpr :: CExpr -> CExpr
}

-- The block of a replayed walk: the recorded derivation rebuilt by the
-- same fromSB and merge calls under the occurrence's numbers.  Nothing
-- here is evaluated at the replay: the consumer demands the block as
-- it demands a walk's own, and every identifier a leaf names is a
-- thunk (rnDict) until then.
replayDeriv :: Renum -> Deriv -> SolvedBinds
replayDeriv r d = SolvedBinds { sbsDeriv = DReplay rp d, sbsBlock = instantiate r d }
  where rp = Replayed { rpNum = rnNum r, rpType = rnType r, rpExpr = rnExpr r,
                        rpTyVars = mapTyVars (rnType r) (derivTyVars d) }

instantiate :: Renum -> Deriv -> Block
instantiate _ DEmpty = emptyBlock
instantiate r (DLeaf lf) = blockFromSB (leafBind r lf)
instantiate r (DMerge _ a b) = mergeBlocks (instantiate r a) (instantiate r b)
instantiate r (DReplay rp sub) = instantiate (nestRenum r rp) sub
instantiate _ DOpaque = internalError "SolvedBinds.instantiate: opaque derivation"

-- The numbers of a replayed subtree are the recorded entry's own: map
-- them into the enclosing walk's first, then as the enclosing walk's (nestRenum)
nestRenum :: Renum -> Replayed -> Renum
nestRenum r rp = Renum { rnNum = rnNum r . rpNum rp,
                         rnDict = rnDict r . rpNum rp,
                         rnType = rnType r . rpType rp,
                         rnExpr = rnExpr r . rpExpr rp }

leafBind :: Renum -> Leaf -> SolvedBind
leafBind r lf = sb { solvedClass = lfClass lf }
  where n = lfBinder lf
        t = rnType r (lfType lf)
        (sh, e) = case lfShape lf of
                    ShInst f ds -> let f' = rnExpr r f
                                   in  (ShInst f' (map (rnNum r) ds),
                                        CApply f' (map (CVar . rnDict r) ds))
                    ShVar m -> (ShVar (rnNum r m), CVar (rnDict r m))
                    ShOuter x -> (ShOuter x, x)
                    ShNum -> (ShNum, mkNumInstBody t)
        sb0 = addBindDeps [ (rnDict r d, rnNum r d) | d <- lfDeps lf ]
                          (mkSolvedBind (rnNum r n) sh (rnDict r n, t, e) (lfRecursive lf))
        sb = if lfIncoherent lf then markIncoherent sb0 else sb0

-- The type variables of the types a substitution-like map gives these
-- variables
mapTyVars :: (Type -> Type) -> S.Set TyVar -> S.Set TyVar
mapTyVars f vs = S.foldr (\ v acc -> typeTyVars (f (TVar v)) acc) S.empty vs

-- The type variables of a type, collected without the lists tv builds
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
