module FixupDefs(fixupDefs, updDef,
                 DictBuckets, mkDictBuckets,
                 DictEvMap, DictRedirects,
                 mkDictRedirects, mkDictEvMapAndRedirects,
                 extendDictRedirects) where

import Control.Monad.State.Strict(State, evalState, gets, modify)
import Data.List(nub)
import Data.Maybe(isJust)
import qualified Data.Map as M
import qualified Data.Set as S
import PFPrint
import ErrorUtil(internalError)
import IOUtil(progArgs)
import Id
import DefProp(DefProp(..), defPropsDictEvidence)
import ISyntax
import ISyntaxXRef(updateIExprPosition)
import ISyntaxUtil(iDefsMap, itIsDictType)
import Util(tracep)
--import Debug.Trace

trace_drop_dicts :: Bool
trace_drop_dicts = "-trace-drop-dicts" `elem` progArgs

-- ===============

-- The evidence identity of a lifted dictionary, as recorded in its
-- DefProps at lift time (see "renderEvidence" in LiftDicts): the
-- slot-normalized rendering of its own evidence level, the lifted
-- dictionaries it references (in slot order), and the types it
-- references (in slot order).  A dictionary without one (incoherent,
-- or evidence outside the renderable shapes) is never merged with
-- anything but itself.
data DictEv = DictEv {
    dev_rendering :: String,
    dev_kids :: [Id],
    dev_types :: [IType]
}

getDictEv :: [DefProp] -> Maybe DictEv
getDictEv props = case defPropsDictEvidence props of
                    Just (str, ks, ts) -> Just (DictEv str ks ts)
                    Nothing -> Nothing

-- The evidence identity (and interned type) of every lifted dictionary
-- in scope -- imported or local -- keyed by name; the verification
-- procedure reads kid identities from here.
type EvMap = M.Map Id (IType, Maybe DictEv)

-- The lifted dictionaries of the imported packages that carry an
-- evidence identity, bucketed by their interned type
-- (position-insensitive cmpT order), each bucket in import order.  A
-- dictionary may be replaced by a bucket candidate only when -- in
-- addition to this type equality -- its evidence identity is verified
-- equal ("mkRedirects" below).  Type equality alone is not sound:
-- packages with different visible instance sets (orphan instances,
-- T0127) can each coherently resolve the same predicate to different
-- instances, and deduplicating across that divergence silently swaps
-- the selected methods.
--
-- This map depends only on the imported packages, which are fixed for
-- the entire compilation of a package; so it is built once (in
-- "compilePackage" in bsc.hs) and passed to every call of "fixupDefs"
-- and "updDef" (which is called once per synthesized module), rather
-- than rebuilt on each call.  Within a bucket the first verified-equal
-- def in import order wins; any verified-equal candidate is
-- semantically interchangeable, and import order is fixed by the
-- source, so the choice is deterministic.
type DictBuckets = M.Map IType [Id]

mkDictBuckets :: [(IPackage a, String)] -> DictBuckets
mkDictBuckets ipkgs =
    M.fromListWith (\ new old -> old ++ new)
        [ (t, [i])
        | (p, _) <- ipkgs, IDef i t _ props <- ipkg_defs p,
          isLiftedDict i, itIsDictType t,
          isJust (defPropsDictEvidence props) ]

-- The canonical redirection for every lifted dictionary (local or
-- imported): the first import-order bucket candidate of the same
-- interned type whose evidence identity verifies equal, when that is
-- a different def.  Computed once per compile over the dictionary
-- defs only; fixUp then redirects by plain Id lookup, with no
-- per-occurrence type work.
type DictRedirects = M.Map Id Id

-- The redirect map for a whole compile, computed ONCE (in
-- "compilePackage") over the package's own post-liftdicts defs plus
-- all imported defs, and passed to every "fixupDefs"/"updDef" call.
-- It is invariant across those calls: the imported packages never
-- change, and the local dictionaries it redirects are dropped from the
-- package by the first "fixupDefs", after which the extra entries
-- match nothing (redirection is idempotent).  Recomputing it per call
-- -- once per synthesized module via "updDef" -- rebuilt the evidence
-- map over every imported def each time.
type DictEvMap = EvMap

mkDictEvMapAndRedirects :: DictBuckets -> IPackage a -> [(IPackage a, String)]
                        -> (DictEvMap, DictRedirects)
mkDictEvMapAndRedirects buckets (IPackage _ _ _ ds) ipkgs =
    let ads = concat (ds : [ ds' | (IPackage _ _ _ ds', _) <- ipkgs ])
        entries = [ (i, (t, getDictEv props))
                  | IDef i t _ props <- ads, isLiftedDict i ]
        evmap = M.fromList entries
        dicts = [ (i, t) | (i, (t, Just _)) <- entries ]
    in  (evmap, mkRedirectsFor buckets evmap dicts)

mkDictRedirects :: DictBuckets -> IPackage a -> [(IPackage a, String)]
                -> DictRedirects
mkDictRedirects buckets ipkg ipkgs =
    snd (mkDictEvMapAndRedirects buckets ipkg ipkgs)

-- Incrementally extend the evidence and redirect maps with dictionaries
-- lifted during wrapper compilation (liftDictsWrapper), without rescanning
-- imported packages.
extendDictRedirects :: DictBuckets -> DictEvMap -> [IDef a] -> DictRedirects
                    -> (DictEvMap, DictRedirects)
extendDictRedirects buckets evmap new_defs redirects =
    let new_entries = [ (i, (t, getDictEv props))
                      | IDef i t _ props <- new_defs, isLiftedDict i ]
    in  if null new_entries
        then (evmap, redirects)
        else let evmap' = foldr (uncurry M.insert) evmap new_entries
                 new_dicts = [ (i, t) | (i, (t, Just _)) <- new_entries ]
                 extra = mkRedirectsFor buckets evmap' new_dicts
             in  (evmap', M.union extra redirects)

-- Verification of a pair of dictionaries ("verifyPair"): equal names
-- are one def; otherwise both must carry evidence identities, and the
-- interned types must be equal, the renderings must be EQUAL STRINGS
-- (sound: the rendering is injective over the dictionary's own
-- evidence level), the type slots must be equal element-wise (interned
-- ITypes, so O(1) per slot), and the kid slots must verify pairwise by
-- the same procedure.  A kid without an evidence identity therefore
-- matches only itself (by name) -- exactly the never-dedup behavior
-- its own package gave it.  No digest ever commits a merge; every
-- merge decision reads the evidence identities themselves.
--
-- The kid recursion terminates because lifted evidence is acyclic
-- (letrec-bound dictionaries are never lifted); results are memoized
-- on the (Id, Id) pair across the whole computation, and a visited
-- set guards defensively against a cycle in corrupted input by
-- refusing (never trusting) a revisited pair.
mkRedirectsFor :: DictBuckets -> EvMap -> [(Id, IType)] -> DictRedirects
mkRedirectsFor buckets evmap dicts =
    M.fromList (concat (evalState (mapM tryRedirect dicts) M.empty))
  where

    tryRedirect :: (Id, IType) -> State (M.Map (Id, Id) Bool) [(Id, Id)]
    tryRedirect (i, t) =
        let scan [] = return []
            -- reaching itself: no earlier candidate matched, so this
            -- (imported) dict is its own canonical def
            scan (c : _) | c `qualEq` i = return []
            scan (c : rest) = do
                ok <- verifyPair S.empty i c
                if ok then return [(i, c)] else scan rest
        in  case M.lookup t buckets of
              Nothing -> return []
              Just cands -> scan cands

    verifyPair :: S.Set (Id, Id) -> Id -> Id
               -> State (M.Map (Id, Id) Bool) Bool
    verifyPair visited i1 i2
      | i1 `qualEq` i2 = return True
        -- a revisited pair means a reference cycle, which lifted
        -- evidence never has; refuse the merge rather than trust it
      | (i1, i2) `S.member` visited = return False
      | otherwise = do
          memoized <- gets (M.lookup (i1, i2))
          case memoized of
            Just r -> return r
            Nothing -> do
              r <- case (M.lookup i1 evmap, M.lookup i2 evmap) of
                     (Just (t1, Just ev1), Just (t2, Just ev2)) ->
                         verifyEv (S.insert (i1, i2) visited) t1 ev1 t2 ev2
                     _ -> return False
              modify (M.insert (i1, i2) r)
              return r

    verifyEv visited t1 ev1 t2 ev2 =
        if t1 /= t2
           || dev_rendering ev1 /= dev_rendering ev2
           || length (dev_kids ev1) /= length (dev_kids ev2)
           || dev_types ev1 /= dev_types ev2
        then return False
        else andM [ verifyPair visited k1 k2
                  | (k1, k2) <- zip (dev_kids ev1) (dev_kids ev2) ]

    andM [] = return True
    andM (mx : rest) = do x <- mx
                          if x then andM rest else return False

-- When a dictionary is dropped in favor of a canonical equivalent, the
-- kid lists of the surviving dictionaries must follow: a downstream
-- package verifies kid slots against the defs it can see, so a kid
-- entry naming a dropped local def must be rewritten to name the
-- canonical def instead.
redirectDictProps :: DictRedirects -> IDef a -> IDef a
redirectDictProps redirects d@(IDef i t e props)
  | M.null redirects = d
  | otherwise = IDef i t e (map upd props)
  where upd (DefP_DictKids ks) =
            DefP_DictKids [ M.findWithDefault k k redirects | k <- ks ]
        upd p = p

-- ===============

-- This does two things:
-- (1) Insert imported packages into the current package (including their
--     pragmas and defs, and recording their signatures)
-- (2) Find references to top-level variables and insert the definitions
--     (to avoid lookups when evaluating the code).  This creates a cyclic
--     data structure when defs call each other recursively.
--
-- It also deduplicates lifted dictionaries against the imported
-- packages: every lifted dictionary whose evidence identity verifies
-- equal to an earlier (import-order) one is redirected to it, and the
-- package's own dictionaries that were redirected away are dropped
-- from the package (they remain in the returned alldefs).
--
-- The first argument must be "mkDictRedirects" computed from the same
-- imported packages that are passed as the third argument (and this
-- package's own defs; see mkDictRedirects on why one map serves every
-- call).
fixupDefs :: DictRedirects -> IPackage a -> [(IPackage a, String)] -> (IPackage a, [IDef a])
fixupDefs redirects (IPackage mi _ ps ds) ipkgs =
    let
        (ms, _) = unzip ipkgs

        -- Combine the pragmas from the imported packages into this one
        -- XXX The nub is needed (at least) because we call "fixupDefs"
        -- XXX multiple times on a package and so we may be adding the ipkg
        -- XXX pragmas multiple times.
        ps' = nub $ concat $ ps : [ ps | IPackage _ _ ps _ <- ms ]

        -- Get all the defs from this package and the imported packages
        ads = concat (ds : map (\ (IPackage _ _ _ ds) -> ds) ms)

        -- Create a recursive data structure by populating the map "m"
        -- with defs created using the map itself
        m = M.fromList [ (i, e) | (IDef i _ e _) <- ads' ]
        ads' = map (redirectDictProps redirects)
                   (iDefsMap (fixUp redirects m) ads)

        -- The new package contents
        ipkg_sigs = [ (mi, s) | (m@(IPackage mi _ _ _), s) <- ipkgs ]
        ds' = map (redirectDictProps redirects)
                  (iDefsMap (fixUp redirects m) ds)
        dropDict i t = case M.lookup i redirects of
                         Just _ -> tracep trace_drop_dicts
                                       ("dropDict: " ++ ppReadable (i, t))
                                       True
                         Nothing -> False
        ds'' = [ d | d@(IDef i t _ _) <- ds', not (dropDict i t) ]
    in
        --trace ("fixup " ++ ppReadable (map fst (M.toList m))) $
        (IPackage mi ipkg_sigs ps' ds'', ads')


-- ===============

-- Replace the definition for a top-level variable with a new definition
-- (used to replace the pre-synthesis definition for a module with its
-- post-synthesis wrapper definition) along with any dictionaries freshly
-- lifted while compiling that wrapper.
--
-- Only the newly compiled wrapper definition and newly lifted dictionaries
-- undergo initial fixUp (with updateIExprPosition).  Existing definitions in
-- ipkg have already been simplified by iSimplify: definitions that do not
-- transitively reference the replaced module are preserved untouched (so
-- synthesizing one sibling module never mutates positions inside shared
-- helpers or sibling modules), while callers that transitively reference the
-- replaced module have their ICDef pointers surgically re-tied.
updDef :: DictRedirects -> IDef a -> [IDef a] -> IPackage a -> [(IPackage a, String)] -> IPackage a
updDef redirects d@(IDef mod_i _ _ _) new_lifted_defs ipkg@(IPackage { ipkg_defs = ds }) ips =
    let
        directRefs (ILam _ _ body) = directRefs body
        directRefs (ILAM _ _ body) = directRefs body
        directRefs (IAps f _ es) = S.unions (directRefs f : map directRefs es)
        directRefs (ICon ref_i (ICDef _ _)) = S.singleton ref_i
        directRefs _ = S.empty

        callersOf =
            M.fromListWith S.union
                [ (r, S.singleton j)
                | IDef j _ ej _ <- ds
                , j /= mod_i
                , r <- S.toList (directRefs ej) ]

        findAffected visited [] = visited
        findAffected visited (cur : queue) =
            let cs = S.toList (M.findWithDefault S.empty cur callersOf)
                fresh = [ c | c <- cs, not (c `S.member` visited) ]
            in  findAffected (foldr S.insert visited fresh) (fresh ++ queue)

        -- Local definitions (excluding mod_i itself) that transitively reference mod_i
        affectedSet = findAffected S.empty [mod_i]

        -- Imported definitions fixed up lazily on demand if referenced by d or new_lifted_defs
        imp_ds = concat [ ds_imp | (IPackage _ _ _ ds_imp, _) <- ips ]
        imp_ds' = map (redirectDictProps redirects)
                      (iDefsMap (fixUp redirects mAll) imp_ds)
        mImp = M.fromList [ (imp_i, imp_e) | IDef imp_i _ imp_e _ <- imp_ds' ]

        -- Fresh definitions (the new wrapper + newly lifted dictionaries)
        fresh_ds' = map (redirectDictProps redirects)
                        (iDefsMap (fixUp redirects mAll) (d : new_lifted_defs))
        (d'@(IDef _ _ mod_e' _), new_lifted_defs') =
            case fresh_ds' of
              (hd : tl) -> (hd, tl)
              [] -> internalError "FixupDefs.updDef: empty fresh_ds'"

        retieLocal (ILam bi bt body) = ILam bi bt (retieLocal body)
        retieLocal (ILAM bi bk body) = ILAM bi bk (retieLocal body)
        retieLocal (IAps f ts es) = IAps (retieLocal f) ts (map retieLocal es)
        retieLocal e@(ICon ref_i (ICDef t _))
          | ref_i == mod_i =
              ICon ref_i (ICDef t (updateIExprPosition (getIdPosition ref_i) mod_e'))
          | ref_i `S.member` affectedSet, Just target_e <- M.lookup ref_i mAffected =
              ICon ref_i (ICDef t target_e)
          | otherwise = e
        retieLocal e = e

        ds_existing' =
            [ if i' == mod_i then d'
              else if i' `S.member` affectedSet
                   then IDef i' t' (retieLocal e') props'
                   else d0
            | d0@(IDef i' t' e' props') <- ds ]

        mAffected =
            M.fromList [ (i', e')
                       | IDef i' _ e' _ <- ds_existing'
                       , i' `S.member` affectedSet ]

        mLocalAndFresh =
            M.fromList ( [ (i', e') | IDef i' _ e' _ <- ds_existing' ] ++
                         [ (li, le) | IDef li _ le _ <- new_lifted_defs' ] )
        mAll = M.union mLocalAndFresh mImp

        dropDict li lt =
            case M.lookup li redirects of
              Just _ -> tracep trace_drop_dicts
                            ("dropDict: " ++ ppReadable (li, lt))
                            True
              Nothing -> False
        new_lifted_defs'' =
            [ ld | ld@(IDef li lt _ _) <- new_lifted_defs', not (dropDict li lt) ]
    in
        ipkg { ipkg_defs = ds_existing' ++ new_lifted_defs'' }


-- ===============

fixUp :: DictRedirects -> M.Map Id (IExpr a) -> IExpr a -> IExpr a
fixUp r m (ILam i t e) = ILam i t (fixUp r m e)
fixUp r m (ILAM i k e) = ILAM i k (fixUp r m e)
fixUp r m (IAps f ts es) = IAps (fixUp r m f) ts (map (fixUp r m) es)
fixUp r m (ICon i (ICDef t _)) =
    let i' = M.findWithDefault i i r
    in  ICon i' (ICDef t (get m i'))
fixUp _ _ e = e

get :: M.Map Id (IExpr a) -> Id -> IExpr a
get m i = let value = get2 m i
              pos = (getIdPosition i)
          in -- trace("LookupX "
                -- ++ (ppReadable i) ++ " => "
                -- ++ (ppReadable (updateIExprPosition pos value))) $
             (updateIExprPosition pos value)

get2 :: M.Map Id (IExpr a) -> Id -> IExpr a
get2 m i =
    case M.lookup i m of
    Just e -> e
    Nothing -> internalError (
        "fixupDefs.get: "
        ++ pfpString i ++ "\n"
        ++ ppReadable (map fst (M.toList m)))

-- ===============
