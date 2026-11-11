module ISimplify(iSimplify) where

import Data.List((\\), findIndex)
import qualified Data.Map as M
import qualified Data.Set as S
import Util(fromJustOrErr)
import IOUtil(progArgs)
import PreIds(idPack, idUnpack)
import PPrint
import IntLit
import ErrorUtil
import Id
import ISyntax
import ISyntaxSubst(eSubst, etSubst)
import Prim
import ISyntaxUtil(aitBit, isTrue, isFalse, isitActionValue_, isitActionValue, iDefsMap)
import ISyntaxXRef(mapIExprPosition)
import Eval
import CType(CTypeStat(..), ctypeStat)
--import Debug.Trace
--import Util(traces)
--import Position

--
-- TODO
--  Useful transformations (from looking at .bo)
--   * let x = (a,b) in ... fst x ... snd x ...  -->  ... a ... b ...
--     note that the tuple transformation is important to inline dictionaries (I think)
--   * PrimConcat 0 n n _ x  -->  x
--   * let d = Literal-Bits n in ... .fromInteger d NNN  -->   NNN

-- The two maps are the def bodies the self-reference guard of
-- selectTuple and selectDictMethod is memoized over (SelfRefMemo
-- below): the bodies fixupDefs wired the ICDef nodes to, for this
-- package's defs and the imported ones (bsc.hs's alldefs), and the
-- bodies iSimpDicts left for this package's defs (its fixUp map).
iSimplify :: (NFData a) => M.Map Id (IExpr a) -> M.Map Id (IExpr a)
          -> IPackage a -> IPackage a
iSimplify alldefs dictBodies (IPackage pi lps ps ds atfCache) =
    let memo1 = SelfRefMemo { memoFixup = M.mapWithKey reachSet alldefs,
                              memoDicts = M.empty }
        memo2 = memo1 { memoDicts = M.mapWithKey reachSet dictBodies }
    in  IPackage pi lps ps ({-iSimpDefs-} (iSimpDefs memo2 (iSimpDefs memo1 ds))) atfCache        -- XXX

iSimpDefs :: NFData a => SelfRefMemo -> [IDef a] -> [IDef a]
iSimpDefs memo ds = fixUpDefs $ iDefsMap (iSimp memo True) ds

iSimp :: (NFData a) => SelfRefMemo -> Bool -> IExpr a -> IExpr a
iSimp memo n (ILam i t e) = ILam i t (iSimp memo n e)
iSimp memo n (IAps e ts as) = iSimpAp' memo n (iSimp memo n (expDef e)) ts (map (iSimp memo n) as)
iSimp _ _ e@(IVar _) = e
iSimp memo n (ILAM i k e) = ILAM i k (iSimp memo n e)
iSimp _ _ e@(ICon _ _) = e
iSimp _ _ e = internalError ("iSimp: " ++ ppReadable e)

expDef :: IExpr a -> IExpr a
expDef (ICon _ (ICDef _ e)) | isHarmless e = e
expDef e = e

iSimpAp' :: NFData a => SelfRefMemo -> Bool -> IExpr a -> [IType] -> [IExpr a] -> IExpr a
iSimpAp' memo b f ts es = mapIExprPosition True (f, iSimpAp memo b f ts es)

iSimpAp :: (NFData a) => SelfRefMemo -> Bool -> IExpr a -> [IType] -> [IExpr a] -> IExpr a
iSimpAp memo n (ILAM i _ e) (t:ts) as = iSimpAp memo n (etSubst i t e) ts as
iSimpAp memo n (ILam i _ e) [] (a:as)
    | not (isKeepId i) && (isTriv a || countOcc i e <= 1) =
        let e' = eSubst i a e
        in iSimpAp memo n e' [] as
iSimpAp _ _ (ICon _ (ICPrim _ prim)) ts es | m /= Nothing = r
  where m = doPrim prim ts es
        r = fromJustOrErr "iSimpAp ICPrim Nothing" m
-- Under -hack-eager-pack-unpack (a debugging aid, see IExpand), resolve
-- pack/unpack coercion primitives against statically-known dictionaries,
-- like the ICSel case below does for method selections -- restoring the
-- static resolution a compiler without coercion holding performs.  The
-- method is located by field name (not a hard-coded slot) so a change to
-- the Bits class shape cannot silently pick the wrong field.
-- Zero- and one-bit coercions are resolved unconditionally when the
-- dictionary is statically known: the evaluator never holds them (a
-- lawful one-bit instance is at most an inverter -- there is no
-- cancellation win to defer for -- and held one-bit values would
-- otherwise permeate the condition machinery), so resolving them here
-- restores the old static folding for Bool logic and does the unfold
-- once per .bo instead of once per elaboration use.
iSimpAp memo n (ICon _ (ICPrim _ p)) ts (dict : es)
    | p == PrimPack || p == PrimUnpack, n,
      doEagerPackUnpack || smallWidth ts,
      Just meth <- selectDictMethod memo meth_i dict
    = iSimpAp memo n meth [] es
  where meth_i = if p == PrimPack then idPack else idUnpack
        smallWidth [_, ITNum w] = w <= 1
        smallWidth _ = False
iSimpAp memo n f@(ICon _ (ICSel { selNo = k })) ts
        es@(def : as) | n && m /= Nothing = {-trace (ppReadable (IAps f ts es, e'))-} e'
  where m = selectTuple memo (fromInteger k) def
        e = fromJustOrErr "iSimpAp ICSel Nothing" m
        e' = iSimpAp memo n e [] as
iSimpAp _ n e [] [] = e -- iSimp has already been called
iSimpAp _ n f ts es = IAps f ts es

selectTuple :: (NFData a) => SelfRefMemo -> Int -> IExpr a -> Maybe (IExpr a)
selectTuple memo k (ICon di (ICDef { iConDef = def@(IAps (ICon _ (ICTuple { })) _ ms) })) | notSelfRef memo di e = Just e
  where e = ms !! k
selectTuple memo k (IAps (ICon iii (ICDef { iConDef = body })) ts []) =
        -- trace ("getTuple " ++ ppReadable (iii,body)) $
        case iSimpAp memo False body ts [] of
        IAps (ICon _ (ICTuple { })) _ ms -> Just $ ms !! k
        _ -> Nothing
selectTuple _ _ _ = Nothing

-- like selectTuple, but locating the field by name in the dictionary's
-- fieldIds (used for the pack/unpack coercion primitives under
-- -hack-eager-pack-unpack)
selectDictMethod :: (NFData a) => SelfRefMemo -> Id -> IExpr a -> Maybe (IExpr a)
selectDictMethod memo meth (ICon di (ICDef { iConDef = IAps (ICon _ (ICTuple { fieldIds = fs })) _ ms }))
    | Just k <- findIndex (qualEq meth) fs, k < length ms,
      let e = ms !! k, notSelfRef memo di e = Just e
selectDictMethod memo meth (IAps (ICon _ (ICDef { iConDef = body })) ts []) =
        case iSimpAp memo False body ts [] of
        IAps (ICon _ (ICTuple { fieldIds = fs })) _ ms
            | Just k <- findIndex (qualEq meth) fs, k < length ms -> Just (ms !! k)
        _ -> Nothing
selectDictMethod _ _ _ = Nothing

doEagerPackUnpack :: Bool
doEagerPackUnpack = elem "-hack-eager-pack-unpack" progArgs

-- XXX should we do more PrimOps here?
doPrim :: PrimOp -> [IType] -> [IExpr a] -> Maybe (IExpr a)
doPrim PrimIntegerToBit [t@(ITNum s)] [ICon i l@(ICInt { iVal = v })] | ilValue v >= 0 &&
                                                                        s >=0 &&
                                                                        ilValue v < 2^s = Just $ ICon i (l { iConType = aitBit t })
doPrim PrimOrd          [t,s] [IAps (ICon _ (ICPrim _ PrimChr)) [s',t'] [e]] | s == s' && t == t' = Just e
doPrim PrimIf _ [c, t, e] | isTrue  c = Just t
                          | isFalse c = Just e
doPrim _ _ _ = Nothing

{-
data Occ = None | One | Many
  deriving(Eq)
-}


countOcc :: Id -> IExpr a -> Int
countOcc i e = countOcc' i e 0

countOcc' :: Id -> IExpr a -> Int -> Int
countOcc' _ _ occ | occ > 1   = occ
countOcc' i (ILam i' _ e) occ = if i == i' then occ else countOcc' i e occ
countOcc' i (IAps f _ es) occ = foldr (countOcc' i) (countOcc' i f occ) es
countOcc' i (IVar i') occ     = if i == i' then occ + 1 else occ
countOcc' i (ILAM _ _ e) occ  = countOcc' i e occ
countOcc' _ _ occ             = occ

{-
size :: IExpr -> Int
size (ILam _ _ e) = size e
size (IAps f _ es) = size f + sum (map size es) + length es
size (ILAM _ _ e) = size e
size _ = 0
-}

isTriv :: IExpr a -> Bool
isTriv (IVar _) = True
-- do not inline ActionValue constants
-- may break correlations for foreign functions
isTriv (ICon _ ci) | isitActionValue_ t || isitActionValue t = False
  where t = iConType ci
isTriv (ICon _ (ICInt { })) = True
isTriv (ICon _ (ICReal { })) = True
isTriv (ICon _ (ICChar { })) = True
isTriv (ICon _ (ICString { })) = True
isTriv (ICon _ (ICUndet { })) = True
isTriv (ICon _ (ICTuple { fieldIds = [] })) = True
isTriv (ICon _ (ICDef { })) = True
isTriv _ = False

isHarmless :: IExpr a -> Bool
isHarmless e =
        --trace (ppReadable (e, onlySimple e, isPerm [] e)) $
        onlySimple e && isPerm [] e

-- expression is a proper combinator: somehow permutes arguments and constants
-- has no free variables and no embedded lambda expressions
--   is: accumulated list of lambda-bound parameters
isPerm :: [Id] -> IExpr a -> Bool
isPerm is (ILAM _ _ e) = isPerm is e
isPerm is (ILam i _ e) = isPerm (i:is) e
isPerm is e = null (gVars e \\ is)

gVars :: IExpr a -> [Id]
gVars (ILam i t e) = gVars e
gVars (IVar i) = [i]
gVars (ILAM i _ e) = gVars e
gVars (IAps f ts es) = gVars f ++ concatMap gVars es
gVars (ICon _ _) = []
gVars (IRefT _ _ _ _) = []

-- computes the top-level definitions the expression depends onb
-- dVars :: IExpr a -> [Id]
-- dVars (ILam _ _ e) = dVars e
-- dVars (IVar _) = []
-- dVars (ILAM _ _ e) = dVars e
-- dVars (IAps f _ es) = dVars f ++ concatMap dVars es
-- dVars (ICon i (ICDef { })) = [i]
-- dVars (ICon _ _) = []
-- dVars (IRefT _ _ _ _) = []

dVars :: IExpr a -> S.Set Id
dVars e = dVars' S.empty e

-- auxiliary function to guard against circular traversals
-- and to accumulate definitions that have been processed
dVars' :: S.Set Id -> IExpr a -> S.Set Id
dVars' ids (ILam _ _ e) = dVars' ids e
dVars' ids (IVar _) = ids
dVars' ids (ILAM _ _ e) = dVars' ids e
-- accumulate vars with a fold so we do not waste work processing the
-- same definitions over and over again across f and es.
dVars' ids (IAps f _ es) = foldl dVars' (dVars' ids f) es
-- guarding against circular traversal
dVars' ids (ICon i (ICDef { })) | i `S.member` ids = ids
dVars' ids (ICon i (ICDef {iConDef = e})) = dVars' (S.insert i ids) e
dVars' ids (ICon _ _) = ids
dVars' ids (IRefT _ _ _ _) = ids

-- -----
-- The self-reference guard of selectTuple and selectDictMethod,
-- "di `notElem` dVars e": the selected dictionary di must not be
-- reachable from the selected field e of its tuple body through the
-- ICDef nodes.  dVars e walks e's whole closure at every selection
-- site; the guard is answered instead from per-def sets (SelfRefMemo),
-- each the walk dVars' makes when it expands a node of the def d
-- carrying one body with nothing but d visited, computed once:
--
--  - memoFixup, for every def, over the body fixupDefs wired the ICDef
--    nodes to (a node carries a position-restamped copy of it that
--    shares its payloads: ISyntaxXRef.updateIExprPosition keeps the
--    IConInfo).  In the first pass every direct reference inside a
--    selected field is such a node: a field lies inside a def body as
--    iSimpDicts left it, whose ICDef nodes are the ones fixupDefs made
--    (simpExpr inlines a dictionary constructor's wired body; the
--    substitutions return ICDef nodes unchanged), or inside a wired
--    body.
--  - memoDicts, for this package's defs, over the body iSimpDicts left
--    (what its fixUp wired the top-level ICDef nodes of each def body
--    to).  In the second pass a direct reference inside a selected
--    field carries either that body (a top-level node of the first
--    pass's input, which the first pass returns unchanged) or a wired
--    body (a node the first pass inlined from a body); the first pass's
--    fixUp rewires only the top-level nodes of its output, which are
--    selection sites and never references inside a field.
--
-- The nodes nested inside any of those bodies carry wired bodies.
-- Which of the two bodies a second-pass direct reference carries is
-- not recorded on the node, so a site is answered from the sets only
-- when at every direct reference d of e (other than di itself) the
-- sets of d's two bodies agree whether d reaches di; otherwise the
-- site is answered by the original walk over e.  Under that agreement
-- the sets give the original walk's answer.  The original walk expands
-- each def once, with the body its node carries when first met, and
-- skips it afterwards, so it finds di exactly when some direct
-- reference d starts a chain to di whose first step is a reference in
-- the body d was expanded with, whose later steps are references in
-- wired bodies, and which repeats no def; such a chain lies in d's set
-- for that body, hence (agreement) in the set consulted.  Conversely a
-- chain in the consulted set that the walk does not follow was cut at
-- a def x expanded earlier; the chain's remainder puts di in x's set
-- for its wired body, so (agreement, if x is a direct reference
-- expanded with its iSimpDicts body) in the set of the body x was
-- expanded with, and x's own walk was cut, if at all, at a def
-- expanded earlier still; the earliest such cut cannot exist, so the
-- walk reaches di (induction on the index, in walk order, of the
-- direct references expanded with their iSimpDicts body; a cut at a
-- def expanded with its alldefs body needs no induction, because the
-- walk expands that whole body).  An entry is computed with d itself
-- visited first, as dVars' does when it expands d's node: a nested
-- node of d is not expanded again by the original walk, and two
-- mutually recursive dictionaries, one of whose iSimpDicts bodies lost
-- a dictionary argument, would otherwise have both sets of the first
-- reach di through the second and the first's wired body while the
-- walk does not.
--
-- The walk over e has the shape of dVars' (the same recursion, the
-- accumulator demanded at the same ICDef nodes), so e's spine, which
-- in the second pass is the first pass's lazy result, is forced in the
-- order the original walk forced it; the sets are over bodies that are
-- already forced (the fixup and isimpdicts dumps) or lazily decoded
-- imports, which intern nothing.  That the nodes carry the bodies
-- described above is checked at every site by -check-isimplify-memo
-- (an internal error on disagreement) and witnessed by the generated
-- output; -hack-no-isimplify-memo keeps the original walk.  Both are
-- argv strings in FlagsDecode.traceflags.  Under -trace-ctype-stats:
-- isimplify_query guards evaluated, isimplify_self_ref of them
-- answered self-referential (the selection is left alone),
-- isimplify_memo_ref direct references answered from the sets,
-- isimplify_memo_fill sets computed (one closure walk each),
-- isimplify_memo_walk sites answered by the original walk because the
-- sets disagreed at a direct reference or a def was in neither map.
data SelfRefMemo = SelfRefMemo {
    memoFixup :: M.Map Id (S.Set Id),
    memoDicts :: M.Map Id (S.Set Id)
  }

-- a memo entry: the defs the original walk visits when it expands a
-- node of d carrying this body with nothing else visited (d first, as
-- dVars' inserts the def before walking the body)
reachSet :: Id -> IExpr a -> S.Set Id
reachSet d body = ctypeStat ISimplifyMemoFill (dVars' (S.singleton d) body)

-- (the counters are bumped through a per-call value, as a bump on a
-- Bool a guard has fixed would be floated out as a constant)
notSelfRef :: SelfRefMemo -> Id -> IExpr a -> Bool
notSelfRef memo di e
    | not useSelfRefMemo = tally walk
    | checkSelfRefMemo && r /= walk =
        internalError ("ISimplify.notSelfRef: memo disagrees with the walk: "
                       ++ ppReadable (di, r, directRefs e))
    | otherwise = tally r
  where walk = di `notElem` dVars e
        r = case notSelfRef' memo di (Just True) e of
              Just b -> b
              Nothing -> ctypeStat ISimplifyMemoWalk e `seq` walk
        tally b = ctypeStat ISimplifyQuery e `seq`
                  (if b then b else ctypeStat ISimplifySelfRef e `seq` b)

-- the walk over e, in the shape of dVars': the accumulator is the
-- conjunction over the direct references met so far, Nothing once one
-- could not be answered from the sets, and it is demanded at each
-- ICDef node, where dVars' demands its visited set (the lazy foldl is
-- dVars''s, not an oversight)
notSelfRef' :: SelfRefMemo -> Id -> Maybe Bool -> IExpr a -> Maybe Bool
notSelfRef' memo di ok (ILam _ _ e) = notSelfRef' memo di ok e
notSelfRef' _ _ ok (IVar _) = ok
notSelfRef' memo di ok (ILAM _ _ e) = notSelfRef' memo di ok e
notSelfRef' memo di ok (IAps f _ es) =
    foldl (notSelfRef' memo di) (notSelfRef' memo di ok f) es
notSelfRef' memo di ok (ICon d (ICDef { })) =
    ok `seq` r `seq` (case (ok, r) of
                        (Just o, Just r') -> Just (o && not r')
                        _ -> Nothing)
  where r = reaches memo di d
notSelfRef' _ _ ok (ICon _ _) = ok
notSelfRef' _ _ ok (IRefT _ _ _ _) = ok

-- whether the def d, referenced directly in the field, is di or
-- reaches it from the body its node carries: Nothing when the sets
-- cannot say (they disagree about di, or d is in neither map).  The
-- arms for d in memoDicts only and for d in neither map are not
-- reached in practice: memoDicts' keys are this package's defs, a
-- subset of memoFixup's, and the def lookup raises on an unknown id;
-- both are kept as fallbacks.
reaches :: SelfRefMemo -> Id -> Id -> Maybe Bool
reaches memo di d
    | d == di = Just True
    | otherwise =
        case (reachesIn (memoDicts memo), reachesIn (memoFixup memo)) of
          (Nothing, Just r) -> ctypeStat ISimplifyMemoRef d `seq` (Just $! r)
          (Just r1, Just r0) | r1 == r0 -> ctypeStat ISimplifyMemoRef d `seq` Just r1
          _ -> Nothing
  where reachesIn m = fmap (S.member di) (M.lookup d m)

-- the defs an expression references directly (for the oracle's error)
directRefs :: IExpr a -> [Id]
directRefs (ILam _ _ e) = directRefs e
directRefs (IVar _) = []
directRefs (ILAM _ _ e) = directRefs e
directRefs (IAps f _ es) = directRefs f ++ concatMap directRefs es
directRefs (ICon i (ICDef { })) = [i]
directRefs (ICon _ _) = []
directRefs (IRefT _ _ _ _) = []

{-# NOINLINE useSelfRefMemo #-}
useSelfRefMemo :: Bool
useSelfRefMemo = not ("-hack-no-isimplify-memo" `elem` progArgs)

{-# NOINLINE checkSelfRefMemo #-}
checkSelfRefMemo :: Bool
checkSelfRefMemo = "-check-isimplify-memo" `elem` progArgs

onlySimple :: IExpr a -> Bool
onlySimple (ILam _ _ e) = onlySimple e
onlySimple (ILAM _ _ e) = onlySimple e
onlySimple (IAps f _ es) = onlySimple f && all onlySimple es
onlySimple (IVar _) = True
onlySimple (ICon _ (ICPrim { })) = True
onlySimple (ICon _ (ICInt { })) = True
onlySimple (ICon _ (ICReal { })) = True
onlySimple (ICon _ (ICChar { })) = True
onlySimple (ICon _ (ICString { })) = True
onlySimple (ICon _ (ICUndet { })) = True
onlySimple (ICon _ (ICTuple { fieldIds = [] })) = True
-- note that foreign function calls are not simple
onlySimple e = False

-----

-- should combine with FixupDefs

fixUpDefs :: [IDef a] -> [IDef a]
fixUpDefs ds =
    let m = M.fromList [ (i, e) | IDef i _ e _ <- ds ]
    in  iDefsMap (fixUp m) ds

fixUp :: M.Map Id (IExpr a) -> IExpr a -> IExpr a
fixUp m (ILam i t e) = ILam i t (fixUp m e)
fixUp m (ILAM i k e) = ILAM i k (fixUp m e)
fixUp m (IAps f ts es) = IAps (fixUp m f) ts (map (fixUp m) es)
fixUp m (ICon i (ICDef t d)) = ICon i (ICDef t (M.findWithDefault d i m))
fixUp m e = e
