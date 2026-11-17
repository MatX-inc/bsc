-- SolvedBinds are sets of solved dictionary bindings.
-- They group recursive and non-recursive dictionary bindings separately.
-- At emission time (getRecursiveDefls/getNonRecursiveDefls) the groups are
-- normalized: non-recursive bindings that reference recursive ones are
-- promoted into the recursive group, and the remaining non-recursive
-- bindings are put in dependency order (definitions before uses), as
-- required by the positional scoping of the Cletseq they are emitted into.
module SolvedBinds(SolvedBind, mkSolvedBind, SolvedBinds, Bind,
                   markIncoherent, isIncoherent, solvedClass,
                   DirectIncoherence(..), addDirectIncoherence,
                   addBindDeps,
                   sbsEmpty, fromSB, (<++), emptySBs,
                   extractClosures,
                   recursiveBinds, nonRecursiveBinds,
                   bindClasses, bindTypes, directIncoherences,
                   getRecursiveDefls, getNonRecursiveDefls,
                   getIncoherentIds, newlyIncoherentIds,
                   computeTransitiveIncoherent) where

import Prelude hiding ((<>))

import Data.List(union, partition, nub)
import Data.Maybe(listToMaybe)
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import ErrorUtil(internalError)
import Id
import CSyntax
import CSyntaxTypes()
import CFreeVars(getFVE, getPV, getVQuals, getLDefs)
import PPrint
import Position
import Pred(Class, Pred)
import PreIds(idComma, idPrimGetName)
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
  freeVarsList :: [Id],
  isRecursive :: Bool,
  isIncoherent :: Bool,
  solvedClass :: Maybe Class  -- the class this bind resolves (for allowIncoherent check)
} deriving (Show)

instance PPrint SolvedBind where
  pPrint d p (SolvedBind bind fv _ isRec isInc _) =
    text "SolvedBind" <+> braces (
      text "bind:" <+> pPrint d p bind <> semi <+>
      text "freeVars:" <+> pPrint d p fv <> semi <+>
      text "isRecursive:" <+> pPrint d p isRec <> semi <+>
      text "isIncoherent:" <+> pPrint d p isInc
    )

-- Collect free value variables of a CExpr in left-to-right syntax encounter
-- order (filtered against getFVE's exact free-variable set and deduplicated
-- on first encounter) so SolvedBinds walks never depend on Ord Id Set order.
freeVarsEncounterOrder :: S.Set Id -> CExpr -> [Id]
freeVarsEncounterOrder fvSet e0
  | S.null fvSet = []
  | otherwise = reverse (snd (walkE S.empty (S.empty, []) e0))
  where
    emit bound st@(seen, acc) i
      | i `S.member` bound = st
      | not (i `S.member` fvSet) = st
      | i `S.member` seen = st
      | otherwise = (S.insert i seen, i : acc)

    walkList bound st es = foldl (walkE bound) st es

    walkDefl bound st (CLValueSign (CDef i _ cs) qs) =
        walkList (S.insert i bound) (foldl (walkClause (S.insert i bound)) st cs)
                 [ e | CQFilter e <- qs ]
    walkDefl bound st (CLValueSign (CDefT i _ _ cs) qs) =
        walkList (S.insert i bound) (foldl (walkClause (S.insert i bound)) st cs)
                 [ e | CQFilter e <- qs ]
    walkDefl bound st (CLValue i cs qs) =
        walkList (S.insert i bound) (foldl (walkClause (S.insert i bound)) st cs)
                 [ e | CQFilter e <- qs ]
    walkDefl bound st (CLMatch p e) =
        walkE (bound `S.union` getPV p) st e

    walkClause bound st (CClause ps qs e) =
        let boundP = bound `S.union` S.unions (map getPV ps)
            (boundQ, stQ) = walkQuals boundP st qs
        in  walkE boundQ stQ e

    walkQuals bound st [] = (bound, st)
    walkQuals bound st (CQGen _ p e : qs) =
        let st' = walkE bound st e
        in  walkQuals (bound `S.union` getPV p) st' qs
    walkQuals bound st (CQFilter e : qs) =
        walkQuals bound (walkE bound st e) qs

    walkSeqDefls bound st [] = (bound, st)
    walkSeqDefls bound st (d : ds) =
        let st' = walkDefl bound st d
            bound' = bound `S.union` S.fromList (getLDefs d)
        in  walkSeqDefls bound' st' ds

    walkStmts _ st [] = st
    walkStmts bound st (CSBindT p me _ _ e : ss) =
        let st1 = walkE bound st e
            st2 = maybe st1 (walkE bound st1) me
        in  walkStmts (bound `S.union` getPV p) st2 ss
    walkStmts bound st (CSBind p me _ e : ss) =
        let st1 = walkE bound st e
            st2 = maybe st1 (walkE bound st1) me
        in  walkStmts (bound `S.union` getPV p) st2 ss
    walkStmts bound st (CSletseq ds : ss) =
        let (bound', st') = walkSeqDefls bound st ds
        in  walkStmts bound' st' ss
    walkStmts bound st (CSletrec ds : ss) =
        let bound' = bound `S.union` S.fromList (concatMap getLDefs ds)
            st' = foldl (walkDefl bound') st ds
        in  walkStmts bound' st' ss
    walkStmts bound st (CSExpr _ e : ss) =
        walkStmts bound (walkE bound st e) ss

    walkMStmts _ st [] = st
    walkMStmts bound st (CMStmt s : ms) =
        let st' = walkStmts bound st [s]
            bound' = case s of
                       CSBindT p _ _ _ _ -> bound `S.union` getPV p
                       CSBind p _ _ _ -> bound `S.union` getPV p
                       CSletseq ds -> bound `S.union` S.fromList (concatMap getLDefs ds)
                       CSletrec ds -> bound `S.union` S.fromList (concatMap getLDefs ds)
                       CSExpr _ _ -> bound
        in  walkMStmts bound' st' ms
    walkMStmts bound st (CMrules e : ms) = walkMStmts bound (walkE bound st e) ms
    walkMStmts bound st (CMinterface e : ms) = walkMStmts bound (walkE bound st e) ms
    walkMStmts bound st (CMTupleInterface _ es : ms) =
        walkMStmts bound (walkList bound st es) ms

    walkRule bound st (CRule _ ml qs e) =
        let st0 = maybe st (walkE bound st) ml
            (boundQ, st1) = walkQuals bound st0 qs
        in  walkE boundQ st1 e
    walkRule bound st (CRuleNest _ ml qs rs) =
        let st0 = maybe st (walkE bound st) ml
            (boundQ, st1) = walkQuals bound st0 qs
        in  foldl (walkRule boundQ) st1 rs

    walkE bound st (CLam (Right i) e) = walkE (S.insert i bound) st e
    walkE bound st (CLam (Left _) e) = walkE bound st e
    walkE bound st (CLamT (Right i) _ e) = walkE (S.insert i bound) st e
    walkE bound st (CLamT (Left _) _ e) = walkE bound st e
    walkE bound st (Cletseq ds e) =
        let (bound', st') = walkSeqDefls bound st ds
        in  walkE bound' st' e
    walkE bound st (Cletrec ds e) =
        let bound' = bound `S.union` S.fromList (concatMap getLDefs ds)
            st' = foldl (walkDefl bound') st ds
        in  walkE bound' st' e
    walkE bound st (CSelect e _) = walkE bound st e
    walkE bound st (CSelectTT _ e _) = walkE bound st e
    walkE bound st (CCon _ es) = walkList bound st es
    walkE bound st (Ccase _ e as) =
        foldl (\s arm ->
                let boundP = bound `S.union` getPV (cca_pattern arm)
                    (_, sQ) = walkQuals boundP s (cca_filters arm)
                    boundPQ = boundP `S.union` getVQuals (cca_filters arm)
                in  walkE boundPQ sQ (cca_consequent arm))
              (walkE bound st e) as
    walkE bound st (CStruct _ _ ies) = walkList bound st (map snd ies)
    walkE bound st (CStructUpd e ies) = walkList bound st (e : map snd ies)
    walkE bound st (Cwrite _ e v) = walkList bound st [e, v]
    walkE _ st (CAny {}) = st
    walkE bound st (CVar i) = emit bound st i
    walkE bound st (CApply (CVar i) _) | i == idPrimGetName = emit bound st idPrimGetName
    walkE bound st (CApply e es) = walkList bound st (e : es)
    walkE bound st (CTaskApply e es) = walkList bound st (e : es)
    walkE bound st (CTaskApplyT e _ es) = walkList bound st (e : es)
    walkE _ st (CLit _) = st
    walkE bound st (CBinOp l i r)
      | i == idComma = walkList bound st [l, r]
      | otherwise = walkE bound (emit bound (walkE bound st l) i) r
    walkE bound st (CHasType e _) = walkE bound st e
    walkE bound st (Cif _ c t e) = walkList bound st [c, t, e]
    walkE bound st (CSub _ e1 e2) = walkList bound st [e1, e2]
    walkE bound st (CSub2 e1 e2 e3) = walkList bound st [e1, e2, e3]
    walkE bound st (CSubUpdate _ ev (eh, el) er) = walkList bound st [ev, eh, el, er]
    walkE bound st (CCon1 _ _ e) = walkE bound st e
    walkE bound st (Cmodule _ ms) = walkMStmts bound st ms
    walkE bound st (Cinterface _ _ ds) =
        let bound' = bound `S.union` S.fromList (concatMap getLDefs ds)
        in  foldl (walkDefl bound') st ds
    walkE bound st (CmoduleVerilog e _ _ _ ses _ _ _) = walkList bound st (e : map snd ses)
    walkE bound st (CmoduleVerilogT _ e _ _ _ ses _ _ _) = walkList bound st (e : map snd ses)
    walkE _ st (CForeignFuncC {}) = st
    walkE _ st (CForeignFuncCT {}) = st
    walkE bound st (Cdo _ ss) = walkStmts bound st ss
    walkE bound st (Caction _ ss) = walkStmts bound st ss
    walkE bound st (Crules _ rs) = foldl (walkRule bound) st rs
    walkE bound st (COper ops) =
        foldl (\s op -> case op of
                          CRand e -> walkE bound s e
                          CRator _ i -> emit bound s i) st ops
    walkE bound st (CTApply e _) = walkE bound st e
    walkE _ st (CSelectT _ _) = st
    walkE _ st (CCon0 _ _) = st
    walkE bound st (CConT _ _ es) = walkList bound st es
    walkE bound st (CStructT _ ies) = walkList bound st (map snd ies)
    walkE _ st (CLitT _ _) = st
    walkE _ st (CAnyT {}) = st
    walkE _ st (Cattributes _) = st

mkSolvedBind :: Bind -> Bool -> SolvedBind
mkSolvedBind b@(_,_,e) isRec =
  let fv = snd (getFVE e)
      fvl = freeVarsEncounterOrder fv e
  in  SolvedBind { bind = b, freeVars = fv, freeVarsList = fvl,
                   isRecursive = isRec, isIncoherent = False,
                   solvedClass = Nothing }

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
addBindDeps is sb =
  let fv0 = freeVars sb
      extra = [ i | i <- nub is, not (i `S.member` fv0) ]
  in  sb { freeVars = foldr S.insert fv0 extra,
           freeVarsList = freeVarsList sb ++ extra }

-- Collection of bindings categorized by recursion
-- nonRecursiveBinds are maintained in topologically sorted order
data SolvedBinds = SolvedBinds {
  recBinds :: [(Bind, S.Set Id, [Id])], -- binding, free-var set, encounter-order free-var list
  nonRecBinds :: [(Bind, S.Set Id, [Id])],
  recursiveIds :: S.Set Id,
  nonRecursiveIds :: S.Set Id,
  incoherentIds :: S.Set Id,
  bindClasses :: M.Map Id Class,                   -- class per bind (for allowIncoherent check)
  bindTypes :: M.Map Id Type,                      -- type per bind (for diagnostic messages)
  directIncoherences :: M.Map Id DirectIncoherence -- root cause per directly-incoherent bind
} deriving (Show)

recursiveBinds :: SolvedBinds -> [(Bind, S.Set Id)]
recursiveBinds sbs = [ (b, fv) | (b, fv, _) <- recBinds sbs ]

nonRecursiveBinds :: SolvedBinds -> [(Bind, S.Set Id)]
nonRecursiveBinds sbs = [ (b, fv) | (b, fv, _) <- nonRecBinds sbs ]

instance Types SolvedBinds where
  apSub s sbs = sbs {
    recBinds = [ ((i, apSub s t, apSub s e), fv, fvl) | ((i, t, e), fv, fvl) <- recBinds sbs ],
    nonRecBinds = [ ((i, apSub s t, apSub s e), fv, fvl) | ((i, t, e), fv, fvl) <- nonRecBinds sbs ],
    bindTypes = M.map (apSub s) (bindTypes sbs)
  }
  tv sbs = recTVs `union` nonRecTVs
    where recTVs    = tv [ (t, e) | ((_, t, e), _, _) <- recBinds sbs ]
          nonRecTVs = tv [ (t, e) | ((_, t, e), _, _) <- nonRecBinds sbs ]

sbsEmpty :: SolvedBinds -> Bool
sbsEmpty sbs = null (recBinds sbs) && null (nonRecBinds sbs)

-- Create singleton SolvedBinds from SolvedBind
-- Note: Both self-recursive and non-recursive bindings are independent of accum
-- Self-recursive bindings depend only on themselves, fresh variables, and source EPreds
fromSB :: SolvedBind -> SolvedBinds
fromSB (SolvedBind b@(i, t, _) fv fvl isRec isInc cls) =
  if isRec
    then SolvedBinds {
      recBinds = [(b, fv, fvl)],
      nonRecBinds = [],
      recursiveIds = S.singleton i,
      nonRecursiveIds = S.empty,
      incoherentIds = if isInc then S.singleton i else S.empty,
      bindClasses = maybe M.empty (M.singleton i) cls,
      bindTypes = M.singleton i t,
      directIncoherences = M.empty
    }
    else SolvedBinds {
      recBinds = [],
      nonRecBinds = [(b, fv, fvl)],
      recursiveIds = S.empty,
      nonRecursiveIds = S.singleton i,
      incoherentIds = if isInc then S.singleton i else S.empty,
      bindClasses = maybe M.empty (M.singleton i) cls,
      bindTypes = M.singleton i t,
      directIncoherences = M.empty
    }

infixl 6 <++ -- directional append new <++ old

-- Merge bindings: plain accumulation, newest first.  During solving a
-- binding's rhs references the fresh dictionary ids of its subgoals,
-- which are solved and merged later -- i.e. sit earlier in the
-- newest-first list -- so accumulation order is already close to
-- dependency order.  A binding that discharges against a previously
-- solved (committed) dictionary references an id merged before it;
-- such out-of-chronology references are repaired at emission time
-- (normalizeSBs), not here.
(<++) :: SolvedBinds -> SolvedBinds -> SolvedBinds
new <++ old = SolvedBinds {
    recBinds = recBinds new ++ recBinds old,
    nonRecBinds = nonRecBinds new ++ nonRecBinds old,
    recursiveIds = recursiveIds new `S.union` recursiveIds old,
    nonRecursiveIds = nonRecursiveIds new `S.union` nonRecursiveIds old,
    incoherentIds = incoherentIds new `S.union` incoherentIds old,
    bindClasses = bindClasses new `M.union` bindClasses old,
    bindTypes = bindTypes new `M.union` bindTypes old,
    directIncoherences = directIncoherences new `M.union` directIncoherences old
  }

-- Normalization for emission.
--
-- Non-recursive bindings are emitted as a Cletseq, whose scoping is
-- positional: a binding may only reference bindings emitted before it.
-- The accumulated newest-first order is already definition-before-use
-- except where a binding references a previously solved dictionary, so
-- emission performs a stable topological repair: bindings keep their
-- accumulated relative order except where a reference forces a
-- definition earlier.
--
-- The Cletseq scopes outside the Cletrec of the recursive bindings, so
-- a non-recursive binding that (transitively) references a recursive
-- binding cannot stay in the Cletseq; it is promoted to the recursive
-- group here.
normalizeSBs :: SolvedBinds -> ([Bind], [Bind])
normalizeSBs sbs = checkFVs `seq` ([ b | (b, _, _) <- recs ], [ b | (b, _, _) <- orderBinds nonrecs ])
  where
    checkFVs =
        let bad = [ (b, S.toList missing, fvl)
                  | (b@(_, _, e), fv, fvl) <- recBinds sbs ++
                                              nonRecBinds sbs,
                    let missing = snd (getFVE e) `S.difference` fv,
                    not (S.null missing) ]
        in  if null bad
              then ()
              else internalError ("SolvedBinds.normalizeSBs: freeVars " ++
                                  "out of date:\n" ++ ppReadable bad)
    dependsOn ids (_, fv, _) = not (S.disjoint fv ids)
    promote rids promoted rest =
        case partition (dependsOn rids) rest of
          ([], _) -> (promoted, rest)
          (newly, rest') ->
              let rids' = rids `S.union` S.fromList [ i | ((i, _, _), _, _) <- newly ]
              in  promote rids' (promoted ++ newly) rest'
    (promoted, nonrecs) = promote (recursiveIds sbs) [] (nonRecBinds sbs)
    recs = recBinds sbs ++ promoted

-- Stable topological order, definitions before uses: repeatedly emit
-- the earliest remaining binding all of whose references are already
-- emitted.  Input that is already dependency-ordered is returned
-- unchanged.
orderBinds :: [(Bind, S.Set Id, [Id])] -> [(Bind, S.Set Id, [Id])]
orderBinds bs =
    let n = length bs
        idx_bs = zip [(0 :: Int)..] bs
        bind_at = IM.fromList idx_bs
        pos_of_id = M.fromList [ (i, k) | (k, ((i, _, _), _, _)) <- idx_bs ]
        deps k fvl = [ j | i <- fvl,
                           Just j <- [M.lookup i pos_of_id], j /= k ]
        dep_lists = [ (k, deps k fvl) | (k, (_, _, fvl)) <- idx_bs ]
        dependents = IM.fromListWith (++) [ (j, [k]) | (k, ds) <- dep_lists, j <- ds ]
        indeg0 = IM.fromList [ (k, length ds) | (k, ds) <- dep_lists ]
        ready0 = S.fromList [ k | (k, 0) <- IM.toList indeg0 ]
        emit d (rdy, ind) =
            let c = IM.findWithDefault 1 d ind - 1
            in  (if c == 0 then S.insert d rdy else rdy, IM.insert d c ind)
        go ready indeg acc =
            case S.minView ready of
              Nothing ->
                if length acc == n
                  then reverse acc
                  else internalError $
                         "SolvedBinds.orderBinds: dependency cycle among " ++
                         "non-recursive bindings:\n" ++
                         ppReadable [ b | (b, _, _) <- bs ]
              Just (k, ready') ->
                let (ready'', indeg') =
                        foldr emit (ready', indeg)
                              (IM.findWithDefault [] k dependents)
                    acc' = case IM.lookup k bind_at of
                             Just b -> b : acc
                             Nothing -> internalError "SolvedBinds.orderBinds: missing index"
                in  go ready'' indeg' acc'
    in  go ready0 indeg0 []

-- Compute the transitive closure of incoherence across all bindings: any
-- binding whose free variables reference an incoherent Id is itself incoherent.
-- Works across both recursive and non-recursive bindings simultaneously.
-- Marks newly-incoherent binding Ids with IdPIncoherent and updates incoherentIds.
-- Also propagates DirectIncoherence root-cause info through the closure.
-- Call this once on a fully-merged SolvedBinds before consuming it.
computeTransitiveIncoherent :: SolvedBinds -> SolvedBinds
computeTransitiveIncoherent sbs = sbs {
    recBinds          = map markBind (recBinds sbs),
    nonRecBinds       = map markBind (nonRecBinds sbs),
    incoherentIds     = finalIds,
    directIncoherences = propagatedSources
  }
  where
    direct = incoherentIds sbs
    allBindings = recBinds sbs ++ nonRecBinds sbs
    finalIds = go direct
    go known
      | S.null newIds = known
      | otherwise    = go (known `S.union` newIds)
      where newIds = S.fromList [ i | ((i, _, _), fv, _) <- allBindings
                                    , not (S.member i known)
                                    , not (S.disjoint fv known) ]
    markBind b@((i, t, e), fv, fvl)
      | S.member i finalIds && not (S.member i direct) =
          ((addIdProp i IdPIncoherent, t, e), fv, fvl)
      | otherwise = b
    -- Propagate root-cause DirectIncoherence through the transitive closure:
    -- for each newly-transitively-incoherent bind, inherit the source from
    -- the first free var (in encounter order) that already has a known source.
    propagateSources known
      | M.null newEntries = known
      | otherwise = propagateSources (M.union known newEntries)
      where
        newEntries = M.fromList
          [ (i, src)
          | ((i, _, _), _, fvl) <- allBindings
          , S.member i finalIds
          , not (M.member i known)
          , Just src <- [listToMaybe [s | j <- fvl
                                        , Just s <- [M.lookup j known]]]
          ]
    propagatedSources = propagateSources (directIncoherences sbs)

-- Newly-incoherent binding Ids in accumulation order (for deterministic diagnostics).
newlyIncoherentIds :: SolvedBinds -> SolvedBinds -> [Id]
newlyIncoherentIds before after =
    let newly = getIncoherentIds after `S.difference` getIncoherentIds before
    in  nub [ i | ((i, _, _), _, _) <- recBinds after ++ nonRecBinds after
                , i `S.member` newly ]

-- Record the root cause of a direct incoherent match for the given dict Id.
-- Called from sat after niceTypes provides the pretty-printed pred/inst strings.
addDirectIncoherence :: Id -> DirectIncoherence -> SolvedBinds -> SolvedBinds
addDirectIncoherence i di sbs =
  sbs { directIncoherences = M.insert i di (directIncoherences sbs) }

emptySBs :: SolvedBinds
emptySBs = SolvedBinds {
             recBinds = [],
             nonRecBinds = [],
             recursiveIds = S.empty,
             nonRecursiveIds = S.empty,
             incoherentIds = S.empty,
             bindClasses = M.empty,
             bindTypes = M.empty,
             directIncoherences = M.empty
           }

instance PPrint SolvedBinds where
  pPrint d p sbs =
    text "SolvedBinds" <+> braces (
      text "rec:" <+> pPrint d p (recursiveBinds sbs) <> semi <+>
      text "nonRec:" <+> pPrint d p (nonRecursiveBinds sbs)
    )

-- Extract the transitive closure of bindings reachable from the given
-- root ids, for reuse elsewhere.  A root is accepted only if its
-- closure never references an id in the forbidden set (the dictionary
-- of an unsolved predicate -- such a closure is incomplete) and never
-- passes through a binding marked incoherent (evidence from an
-- incoherent instance match is information-dependent and must not be
-- frozen for reuse).  References to ids bound nowhere in this
-- collection (top-level instance ids, lambda-bound given dictionaries)
-- are permitted; their validity is the caller's scoping concern.
-- Returns the closure bindings of all accepted roots (each binding
-- once) and the set of accepted roots.
extractClosures :: S.Set Id -> [Id] -> SolvedBinds -> (SolvedBinds, S.Set Id)
extractClosures forbidden roots sbs = (closure_sbs, S.fromList ok_roots)
  where
    tagged = [ (b, fv, fvl, True)  | (b, fv, fvl) <- recBinds sbs ] ++
             [ (b, fv, fvl, False) | (b, fv, fvl) <- nonRecBinds sbs ]
    bind_map = M.fromList [ (i, x) | x@((i, _, _), _, _, _) <- tagged ]
    incoh = incoherentIds sbs
    ok _ [] = True
    ok seen (i:is)
      | i `S.member` forbidden = False
      | i `S.member` seen = ok seen is
      | otherwise =
          case M.lookup i bind_map of
            Nothing -> ok (S.insert i seen) is
            Just ((i', _, _), _, fvl, _)
              | hasIdProp i' IdPIncoherent || i' `S.member` incoh -> False
              | otherwise -> ok (S.insert i seen) (fvl ++ is)
    ok_roots = [ r | r <- roots, ok S.empty [r] ]
    collect _ [] acc = acc
    collect seen (i:is) acc
      | i `S.member` seen = collect seen is acc
      | otherwise =
          case M.lookup i bind_map of
            Nothing -> collect (S.insert i seen) is acc
            Just x@(_, _, fvl, _) ->
                collect (S.insert i seen) (fvl ++ is) (x : acc)
    collected = collect S.empty ok_roots []
    kept_ids = S.fromList [ i | ((i, _, _), _, _, _) <- collected ]
    closure_sbs = SolvedBinds {
        recBinds = [ (b, fv, fvl) | (b, fv, fvl, True) <- collected ],
        nonRecBinds = [ (b, fv, fvl) | (b, fv, fvl, False) <- collected ],
        recursiveIds = S.fromList [ i | ((i, _, _), _, _, True) <- collected ],
        nonRecursiveIds = S.fromList [ i | ((i, _, _), _, _, False) <- collected ],
        incoherentIds = S.empty,
        bindClasses = M.restrictKeys (bindClasses sbs) kept_ids,
        bindTypes = M.restrictKeys (bindTypes sbs) kept_ids,
        directIncoherences = M.empty
      }

getRecursiveDefls :: SolvedBinds -> [CDefl]
getRecursiveDefls = map mkDefl . fst . normalizeSBs

getNonRecursiveDefls :: SolvedBinds -> [CDefl]
getNonRecursiveDefls = map mkDefl . snd . normalizeSBs

getIncoherentIds :: SolvedBinds -> S.Set Id
getIncoherentIds = incoherentIds
