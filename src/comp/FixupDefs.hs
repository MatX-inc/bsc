module FixupDefs(fixupDefs, updDef, markRecursiveDefs,
                 DictBuckets, mkDictBuckets,
                 DictRedirects, mkDictRedirects) where

import Control.Monad.State.Strict(State, evalState, gets, modify)
import Data.List(nub)
import qualified Data.List as L
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
import Position(Position)
import FStringCompat(FString)
import ISyntaxUtil(iDefsMap, itIsDictType)
import SCC(scc)
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
mkDictRedirects :: DictBuckets -> IPackage a -> [(IPackage a, String)]
                -> DictRedirects
mkDictRedirects buckets (IPackage _ _ _ ds _) ipkgs =
    let ads = concat (ds : [ ds' | (IPackage _ _ _ ds' _, _) <- ipkgs ])
        evmap = M.fromList [ (i, (t, getDictEv props))
                           | IDef i t _ props <- ads, isLiftedDict i ]
    in  mkRedirects buckets evmap

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
mkRedirects :: DictBuckets -> EvMap -> DictRedirects
mkRedirects buckets evmap =
    M.fromList (concat (evalState (mapM tryRedirect dicts) M.empty))
  where
    dicts = [ (i, t) | (i, (t, Just _)) <- M.toList evmap ]

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
fixupDefs redirects (IPackage mi _ ps ds own_atf_cache) ipkgs =
    let
        (ms, _) = unzip ipkgs

        -- Combine the pragmas from the imported packages into this one
        -- XXX The nub is needed (at least) because we call "fixupDefs"
        -- XXX multiple times on a package and so we may be adding the ipkg
        -- XXX pragmas multiple times.
        ps' = nub $ concat $ ps : [ ps | IPackage _ _ ps _ _ <- ms ]

        -- The defs of this package and of the imported packages, each
        -- list with the name of the package that defines them
        pkgDefs = (mi, ds) : [ (n, defs) | IPackage n _ _ defs _ <- ms ]

        -- Create a recursive data structure by populating the map "m"
        -- with defs created using the map itself; an entry carries the
        -- name of its package, which "get" compares with the package of
        -- the definition it is tying a reference in
        m = M.fromList [ (i, (getIdBase pmi, d))
                       | (pmi, pds) <- pkgDefs', d@(IDef i _ _ _) <- pds ]
        pkgDefs' = [ (pmi, fixPkg pmi pds) | (pmi, pds) <- pkgDefs ]
        ads' = concatMap snd pkgDefs'
        fixPkg pmi pds = map (redirectDictProps redirects)
                             (iDefsMap (fixUp redirects (getIdBase pmi) m) pds)

        -- The new package contents
        ipkg_sigs = [ (mi, s) | (m@(IPackage mi _ _ _ _), s) <- ipkgs ]
        ds' = fixPkg mi ds
        dropDict i t = case M.lookup i redirects of
                         Just _ -> tracep trace_drop_dicts
                                       ("dropDict: " ++ ppReadable (i, t))
                                       True
                         Nothing -> False
        ds'' = [ d | d@(IDef i t _ _) <- ds', not (dropDict i t) ]
        -- Note that the package keeps only its own ATF cache entries, so
        -- that .bo files stay proportional to their own package.  The union
        -- with the imported packages' caches (for use during elaboration)
        -- is built in bsc.hs and is never stored in an IPackage.  Do not
        -- merge caches here: "fixupDefs" is re-invoked once per synthesized
        -- module (via "updDef"), so any merging added here is multiplied by
        -- the number of modules.
    in
        --trace ("fixup " ++ ppReadable (map fst (M.toList m))) $
        (IPackage mi ipkg_sigs ps' ds'' own_atf_cache, ads')


-- ===============

-- Mark the recursion points of a package: every definition in a
-- nontrivial strongly connected component of the package's definition
-- reference graph, and every definition that references itself (primFix
-- in the Prelude is one).  The evaluator's metadata store consults a
-- frame's recursive-site map only at such definitions (IExpandMeta), so
-- the property is computed once, before the package's .bo is written,
-- and read back with the definition without decoding its body.
-- References are the ICDef heads reachable through ILam, ILAM and IAps,
-- the nodes fixUp visits, never entering iConDef; references to
-- imported definitions are dropped, since package imports form a DAG
-- and no cycle can pass through them.  A property set earlier is
-- stripped first, so the marking is idempotent.
--
-- The recursion edges, the references from a recursion point to a
-- definition of its own component (its self-references among them),
-- are moved to the referenced definition's own position, while every
-- other reference keeps its occurrence's position (IConv.iConvVar).
-- Such a reference is an iteration of a loop, not a new use of the
-- definition: the store continues a loop's count at a reference that
-- carries the definition's own position (IExpandMeta.pushFrame) and
-- reports every iteration at the site that entered the loop.  The
-- restamp is idempotent too.
markRecursiveDefs :: [IDef a] -> [IDef a]
markRecursiveDefs ds =
    let locals = S.fromList [ i | IDef i _ _ _ <- ds ]
        nodes = [ (i, localDefRefs locals S.empty e) | IDef i _ e _ <- ds ]
        selfRefs = S.fromList [ i | (i, rs) <- nodes, S.member i rs ]
        comps = scc [ (i, S.toList rs) | (i, rs) <- nodes ]
        cyclic = S.fromList (concat [ c | c <- comps, not (null (drop 1 c)) ])
        recursive = S.union selfRefs cyclic
        -- the component of every definition, and its own position, by
        -- name (Id ordering ignores positions, so a reference finds
        -- its definition's entry)
        compOf = M.fromList [ (i, n) | (n, c) <- zip [0 :: Int ..] comps, i <- c ]
        ownPos = M.fromList [ (i, getIdPosition i) | IDef i _ _ _ <- ds ]
        -- the position a reference from the recursion point d keeps:
        -- the referenced definition's own, when both are in one component
        edgePos d j | M.lookup j compOf == M.lookup d compOf = M.lookup j ownPos
                    | otherwise = Nothing
        mark (IDef i t e ps) =
            let ps' = filter (/= DefP_Recursive) ps
            in  if S.member i recursive
                then IDef i t (restampEdges (edgePos i) e) (DefP_Recursive : ps')
                else IDef i t e ps'
    in  map mark ds

-- The body of a recursion point with every reference the function
-- maps to a position moved to that position (the recursion edges, see
-- markRecursiveDefs).  The spine is rebuilt for recursion points alone,
-- and fixUp rebuilds it again right after.
restampEdges :: (Id -> Maybe Position) -> IExpr a -> IExpr a
restampEdges f (ILam i t e) = ILam i t (restampEdges f e)
restampEdges f (ILAM i k e) = ILAM i k (restampEdges f e)
restampEdges f (IAps g ts es) = IAps (restampEdges f g) ts (map (restampEdges f) es)
restampEdges f e@(ICon i ic@(ICDef {})) =
    case f i of
      Just p -> ICon (setIdPosition p i) ic
      Nothing -> e
restampEdges _ e = e

-- the local definitions an expression references, accumulated
localDefRefs :: S.Set Id -> S.Set Id -> IExpr a -> S.Set Id
localDefRefs locals acc (ILam _ _ e) = localDefRefs locals acc e
localDefRefs locals acc (ILAM _ _ e) = localDefRefs locals acc e
localDefRefs locals acc (IAps f _ es) = L.foldl' (localDefRefs locals) (localDefRefs locals acc f) es
localDefRefs locals acc (ICon i (ICDef {}))
    | S.member i locals = S.insert i acc
localDefRefs _ acc _ = acc

-- ===============

-- Replace the definition for a top-level variable with a new definition.
-- (This is used to replace the pre-synthesis definition for a module with
-- the post-synthesis definition.)
-- The first argument must be "mkDictRedirects" computed from the same
-- imported packages that are passed as the fourth argument.
updDef :: DictRedirects -> IDef a -> IPackage a -> [(IPackage a, String)] -> IPackage a
updDef redirects d@(IDef i _ _ _) ipkg@(IPackage { ipkg_defs = ds }) ips =
    let
        -- replace the def in the list
        ds' = [ if i == i' then d else d' | d'@(IDef i' _ _ _) <- ds ]
        ipkg' = ipkg { ipkg_defs = ds' }

        -- The new definition is in ISyntax but it does not yet have
        -- top-level defs inlined into the variable references, so we
        -- need to call "fixup" on the def.
        --
        -- Further, any top-level def that referred to this module
        -- need to have the inlined old def replaced with the new def.
        --
        -- We use "fixupDefs" to perform both changes.
        -- XXX However, "fixupDefs" is overkill, for just one def.
        -- XXX Note that we throw away alldefs, when we could return it.
        (ipkg'', _) = fixupDefs redirects ipkg' ips
    in
        ipkg''


-- ===============

-- The definitions in scope by name, each with the name of the package
-- that defines it
type DefMap a = M.Map Id (FString, IDef a)

-- fixUp ties the references in the definitions of the package named p
fixUp :: DictRedirects -> FString -> DefMap a -> IExpr a -> IExpr a
fixUp r p m (ILam i t e) = ILam i t (fixUp r p m e)
fixUp r p m (ILAM i k e) = ILAM i k (fixUp r p m e)
fixUp r p m (IAps f ts es) = IAps (fixUp r p m f) ts (map (fixUp r p m) es)
fixUp r p m (ICon i (ICDef t _)) =
    let i' = M.findWithDefault i i r
    in  ICon i' (ICDef t (get p m i'))
fixUp _ _ _ e = e

-- The body tied to a reference in a definition of the package p: a
-- copy of the referenced definition's body, stamped over its spine
-- and sole arguments (as far as updateIExprPosition reaches) with the
-- definition's own position when the definition is p's own, and with
-- the reference's position, its use site, when it is imported.  Every
-- reference carries its occurrence's position (IConv.iConvVar, the
-- .bo of an imported package), which the evaluator's metadata store
-- reads from the reference itself (IExpandMeta.mintDefSite); the
-- stamp is read by the position-valued primitives
-- (PrimGetStringPosition and kin, IExpand) from the unfolded body and
-- printed into the generated code, so a same-package unfolding keeps
-- reporting its definition.
get :: FString -> DefMap a -> Id -> IExpr a
get p m i = let (dp, IDef di _ value _) = get2 m i
                pos = if dp == p then getIdPosition di else getIdPosition i
            in -- trace("LookupX "
                -- ++ (ppReadable i) ++ " => "
                -- ++ (ppReadable (updateIExprPosition pos value))) $
             (updateIExprPosition pos value)

get2 :: DefMap a -> Id -> (FString, IDef a)
get2 m i =
    case M.lookup i m of
    Just x -> x
    Nothing -> internalError (
        "fixupDefs.get: "
        ++ pfpString i ++ "\n"
        ++ ppReadable (map fst (M.toList m)))

-- ===============
