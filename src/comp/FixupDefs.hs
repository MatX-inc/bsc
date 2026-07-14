module FixupDefs(fixupDefs, updDef, mkCoherentDictMap) where

import Data.List(nub)
import qualified Data.Map as M
import PFPrint
import CType
import ISyntaxUtil
import ErrorUtil(internalError)
import IOUtil(progArgs)
import Id
import FStringCompat(FString)
import ISyntax
import ISyntaxXRef(updateIExprPosition)
import Util(tracep)

trace_drop_dicts :: Bool
trace_drop_dicts = "-trace-drop-dicts" `elem` progArgs

-- ===============



-- Map from the type of a coherent dictionary to the imported top-level
-- dictionaries of that type, keyed by their evidence fingerprints.  A
-- local dictionary may be replaced by an imported one only when their
-- fingerprints are EQUAL: type equality alone is not sound, because
-- packages with different visible instance sets (orphan instances) can
-- each coherently resolve the same predicate to different instances
-- (see the evidence-fingerprint note in LiftDicts).  Dictionaries
-- without a fingerprint (older .bo files, unfingerprintable evidence
-- shapes) are never deduplication targets.
--
-- This map depends only on the imported packages, which are fixed for
-- the entire compilation of a package; so it can be built once (in
-- "compilePackage" in bsc.hs) and passed to every call of "fixupDefs"
-- and "updDef" (which is called once per synthesized module), rather
-- than rebuilt on each call.  Within one fingerprint the first def in
-- import order wins; any same-fingerprint candidate is semantically
-- interchangeable, and import order is fixed by the source, so the
-- choice is deterministic.
mkCoherentDictMap :: [(IPackage a, String)] -> M.Map IType (M.Map FString Id)
mkCoherentDictMap ipkgs =
    let
        -- Get all the defs from the imported packages
        ams = concatMap (ipkg_defs . fst) ipkgs

        coherent_dicts = [ (t, (fp, i))
                         | IDef i t _ _ <- ams,
                           itIsDictType t, isDictId i,
                           not (isIncoherentDict i),
                           Just fp <- [getEvidenceFP i] ]
    in
        M.fromListWith (M.unionWith (\_new old -> old))
            [ (t, M.singleton fp i) | (t, (fp, i)) <- coherent_dicts ]

-- This does two things:
-- (1) Insert imported packages into the current package (including their
--     pragmas and defs, and recording their signatures)
-- (2) Find references to top-level variables and insert the definitions
--     (to avoid lookups when evaluating the code).  This creates a cyclic
--     data structure when defs call each other recursively.
--
-- The first argument must be "mkCoherentDictMap" applied to the same
-- imported packages that are passed as the third argument.
fixupDefs :: M.Map IType (M.Map FString Id) -> IPackage a -> [(IPackage a, String)] -> (IPackage a, [IDef a])
fixupDefs coherent_dict_map (IPackage mi _ ps ds own_atf_cache) ipkgs =
    let
        (ms, _) = unzip ipkgs

        -- Combine the pragmas from the imported packages into this one
        -- XXX The nub is needed (at least) because we call "fixupDefs"
        -- XXX multiple times on a package and so we may be adding the ipkg
        -- XXX pragmas multiple times.
        ps' = nub $ concat $ ps : [ ps | IPackage _ _ ps _ _ <- ms ]

        -- Get all the defs from this package and the imported packages
        ads = concat (ds : map (\ (IPackage _ _ _ ds _) -> ds) ms)

        -- Create a recursive data structure by populating the map "m"
        -- with defs created using the map itself
        m = M.fromList [ (i, e) | (IDef i _ e _) <- ads' ]
        ads' = iDefsMap (fixUp coherent_dict_map m0 m) ads

        -- pre-fixup definitions, used to verify fingerprint matches
        -- against each package's own evidence as it was serialized
        -- (before any redirection this pass performs)
        m0 = M.fromList [ (i, e) | (IDef i _ e _) <- ads ]

        -- The new package contents
        ipkg_sigs = [ (mi, s) | (m@(IPackage mi _ _ _ _), s) <- ipkgs ]
        ds' = iDefsMap (fixUp coherent_dict_map m0 m) ds
        dropDict i t = tracep (trace_drop_dicts && result) ("dropDict: " ++ ppReadable (i,t)) result
          where result = case coherentTarget coherent_dict_map m0 i t of
                           Just i' -> i' /= i
                           Nothing -> False
        ds'' = [ d' | d'@(IDef i t _ _) <- ds', not (dropDict i t) ]
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

-- Replace the definition for a top-level variable with a new definition.
-- (This is used to replace the pre-synthesis definition for a module with
-- the post-synthesis definition.)
-- The first argument must be "mkCoherentDictMap" applied to the same
-- imported packages that are passed as the fourth argument.
updDef :: M.Map IType (M.Map FString Id) -> IDef a -> IPackage a -> [(IPackage a, String)] -> IPackage a
updDef coherent_dict_map d@(IDef i _ _ _) ipkg@(IPackage { ipkg_defs = ds }) ips =
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
        (ipkg'', _) = fixupDefs coherent_dict_map ipkg' ips
    in
        ipkg''


-- ===============

fixUp :: M.Map IType (M.Map FString Id) -> M.Map Id (IExpr a) -> M.Map Id (IExpr a) -> IExpr a -> IExpr a
fixUp cm m0 m (ILam i t e) = ILam i t (fixUp cm m0 m e)
fixUp cm m0 m (ILAM i k e) = ILAM i k (fixUp cm m0 m e)
fixUp cm m0 m (IAps f ts es) = IAps (fixUp cm m0 m f) ts (map (fixUp cm m0 m) es)
fixUp cm m0 m (ICon i (ICDef t _))
  | Just i' <- coherentTarget cm m0 i t = ICon i' (ICDef t (get m i'))
fixUp cm m0 m (ICon i (ICDef t _)) = ICon i (ICDef t (get m i))
fixUp _ _ _ e = e

-- The canonical imported def a coherent dictionary may be replaced by:
-- one of the same type AND the same evidence fingerprint, VERIFIED
-- against the pre-fixup definitions.  The fingerprint is a digest, so
-- equality alone is not proof of identical evidence; the structural
-- check makes a collision harmless (the redirection is skipped and
-- both dictionaries are kept -- the only cost is a lost
-- deduplication).
coherentTarget :: M.Map IType (M.Map FString Id) -> M.Map Id (IExpr a) -> Id -> IType -> Maybe Id
coherentTarget cm m0 i t
  | isDictId i, not (isIncoherentDict i), itIsDictType t,
    Just fp <- getEvidenceFP i,
    Just fpm <- M.lookup t cm,
    Just i' <- M.lookup fp fpm =
      if i' == i then Just i'
      else case (M.lookup i m0, M.lookup i' m0) of
             (Just e, Just e') | eqEvidence e e' -> Just i'
             (Just _, Just _) ->
                 tracep trace_drop_dicts
                     ("coherentTarget: digest matched but evidence"
                      ++ " differs, keeping both: " ++ ppReadable (i, i'))
                     Nothing
             _ -> Nothing
  | otherwise = Nothing

-- Structural equality of dictionary evidence definitions.  Dictionary
-- references compare by qualified name or by evidence digest, because
-- each package lifts its own copy of shared evidence under a local
-- name.  Anything unrecognized falls back to plain equality, erring
-- toward keeping both definitions.
eqEvidence :: IExpr a -> IExpr a -> Bool
eqEvidence (ILam i1 t1 e1) (ILam i2 t2 e2) =
    i1 == i2 && t1 == t2 && eqEvidence e1 e2
eqEvidence (ILAM i1 k1 e1) (ILAM i2 k2 e2) =
    i1 == i2 && k1 == k2 && eqEvidence e1 e2
eqEvidence (IAps f1 ts1 es1) (IAps f2 ts2 es2) =
    ts1 == ts2 && length es1 == length es2 &&
    eqEvidence f1 f2 && and (zipWith eqEvidence es1 es2)
eqEvidence (ICon i1 (ICDef t1 _)) (ICon i2 (ICDef t2 _)) =
    t1 == t2 &&
    (qualEq i1 i2 ||
     case (getEvidenceFP i1, getEvidenceFP i2) of
       (Just d1, Just d2) -> d1 == d2
       _ -> False)
eqEvidence e1 e2 = e1 == e2

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

