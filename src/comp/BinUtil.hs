module BinUtil (
                BinMap, BinFile,
                HashMap, BinaryDependencyCache, withBinaryDependencyCache,
                readImports,
                readBin, readBinaryDependenciesPlan, readPackageDependenciesPlan, sortImportedSignatures,
                replaceImportedSignatures
               ) where

import Control.Monad(when, foldM)
import qualified Data.ByteString as BS
import Control.Exception(evaluate)
import qualified Data.Set as Set
import System.Directory(makeAbsolute)
import System.FilePath(normalise)
import BuildPlan
import DependencyReport(DependencyCandidate(..), tryDependency)
import qualified Data.Map as M
import Flags(Flags,
             ifcPath,
             enablePoisonPills,
             usePrelude,
             verbose)
import Position(noPosition)
import Id
import PreIds
import CSyntax
import ISyntax
import Prim
import Error(internalError, ErrMsg(..), ErrorHandle, bsError, bsWarning)
import PFPrint
import SCC
import FileNameUtil(binSuffix)
import FileIOUtil(readBinFilePath)
import GenBin(readBinFile, decodeBinFile)
import Util(fromJustOrErr, fromMaybeM,
            map_insertManyWith, map_insertManyWithKeyM)


-- =========================

-- the contents of the .bo package
type BinFile a = ( String       -- filename
                 , CSignature   -- signature of user-visible defs)
                 , CSignature   -- signature of all defs
                 , IPackage a   -- the package
                 , String       -- hash
                 )

-- a map of the hashes for a package (associated with the source)
-- (more than one hash indicates a mismatch error)
type HashMap = M.Map Id (String, [Id])

-- a map containing the binfiles that have been loaded, indexed by pkg name
type BinMap a = M.Map String (BinFile a)


-- =========================

-- Read all .bo files imported by this package
readImports :: ErrorHandle -> Flags -> BinMap a -> HashMap -> CPackage ->
               IO (CPackage, BinMap a, HashMap)
readImports errh flags binmap0 hashmap0
            (CPackage pkgId exps imps old_impsigs fixs ds includes) = do
  when (not (null old_impsigs)) $
      internalError "readImports: unexpected non-empty impsigs"
  let
      pkgName = getIdString pkgId

      -- Replace qualified importing only (True) with unqualified and
      -- qualified (False)
      qualMergeFn newQual oldQual = if (oldQual) then newQual else oldQual

      -- load files as necessary, keeping track of what has been loaded
      -- and with what qualifiers they have been imported
      -- (avoids loading packages twice and filters duplicates)
      fn (binmap, hashmap, qualmap) (CImpId q i) =
          do (binmap', hashmap', bininfo, _)
                 <- readBin errh flags (Just pkgName) binmap hashmap i
             let (_, _, _, ipkg, _) = bininfo
             let deps = map (getIdString . fst) $ ipkg_depends ipkg
                 dep_quals = map (\x -> (x,True)) deps
             -- add the dependencies as qualified-only imports
             let qualmap' = map_insertManyWith qualMergeFn dep_quals qualmap
             -- add the current package with the user-specified import
             let qualmap'' = M.insertWith qualMergeFn (getIdString i) q qualmap'
             return (binmap', hashmap', qualmap'')

  (binmap, hashmap, qualmap)
      <- foldM fn (binmap0, hashmap0, M.empty) (addPrelude flags imps)

  let mkCImp (s, q) = case (M.lookup s binmap) of
                        Just (fn, bi_sig, _, _, _) -> CImpSign fn q bi_sig
                        Nothing -> internalError ("mkCImp: " ++ ppReadable s)
  let impsigs' = map mkCImp (M.toList qualmap)

  let sortedImpsigs = sortImportedSignatures impsigs'
  let cpkg' = CPackage pkgId exps imps sortedImpsigs fixs ds includes

  return (cpkg', binmap, hashmap)


-- helper function that reads in a .bo file, and any .bo files that it needs
readBin :: ErrorHandle -> Flags -> (Maybe String) ->
           BinMap a -> HashMap -> Id ->
           IO (BinMap a, HashMap, BinFile a, [Id])
readBin errh flags maybePkgName binmap0 hashmap0 p0 = executeResultPlan $ do
   let
       -- if compiling a source package (that imports p0), detect when p0
       -- imports a bin-file with the same name as the source package
       checkPkgName p =
           case maybePkgName of
             Just pkgName | (getIdString p == pkgName) ->
                 bsError errh
                     [(getPosition p0,
                       ECircularImportsViaBinFile pkgName (getIdString p0))]
             _ -> return ()

       seen (bins, _, _) p = M.member (getIdString p) bins
       load (bins, hashes, ps_read) _ p = observe "read imported package" $ do
         checkPkgName p
         (fname, bi_sig, bo_sig, bo_pkg, hash, hashes', impNames) <-
           doImport errh flags hashes p
         let bins' = M.insert (getIdBaseString p) (fname, bi_sig, bo_sig, bo_pkg, hash) bins
         return $ Just ((bins', hashes', p : ps_read), fname, impNames)

   graph <- walkBinaryPlan seen load (binmap0, hashmap0, []) [(maybe "" id maybePkgName,p0)]
   return $ fmap (\(binmap', hashmap', reversed) ->
       let p0_bininfo = fromJustOrErr "readBin" $ M.lookup (getIdString p0) binmap'
       in (binmap', hashmap', p0_bininfo, reverse reversed)) graph


-- Sort signatures topologically: output signature list such that,
-- if signature s1 comes before signature s2, then s1 does not import s2
sortImportedSignatures :: [CImportedSignature] -> [CImportedSignature]
sortImportedSignatures signatures =
    let
        -- We map the signatures to strings and sort a graph of strings,
        -- so that sorting is stable (the Ord instance for the tuple includes
        -- Ids, whose Ord instance depends on when the Id was created)
        sMap = M.fromList [ (getIdString name, sign)
                            | sign@(CImpSign _ _ (CSignature name _  _ _))
                                <- signatures]
        addImplicitPreludeDependency (name, imports)
            | name == strPrelude = (name, imports)
            | name == strPreludeBSV = (name, imports)
            | otherwise = (name, strPrelude :
                           strPreludeBSV :
                           filter (\ x -> x /= strPrelude && x /= strPreludeBSV)
                                  imports)
            where strPrelude = getIdString idPrelude
                  strPreludeBSV = getIdString idPreludeBSV
        sGraph = [ addImplicitPreludeDependency (getIdString name,
                                                 map getIdString imports)
                   | (CImpSign _ _ (CSignature name imports _ _))
                       <- signatures]
        lookupFn i = case (M.lookup i sMap) of
                       Just s -> s
                       Nothing -> internalError ("sortImportedSignatures: " ++ i)
    in  case tsort sGraph of
        Left cycle -> internalError ("import cycle:\n" ++ ppString cycle)
        Right order -> map lookupFn order


-- Add the Prelude to the list of imports, unless compiling the Prelude itself
addPrelude :: Flags -> [CImport] -> [CImport]
addPrelude flags imps | usePrelude flags = CImpId False idPrelude :
                                           CImpId False idPreludeBSV :
                                           imps
                      | otherwise = imps

-- Shared binary closure: a consumer supplies the information it needs from
-- each object. Compilation runs the ordinary IO import at its consumption
-- point. Dependency input contracts attach candidate facts and read only
-- import names, reporting unavailable headers as open boundaries.
-- Metadata discovery starts with an empty loaded map and keeps sibling state
-- separate, so only ancestors can be loaded already. The cycle guard handles
-- those; loaded-map checks do not represent additional dependency choices.
walkBinaryPlan :: (s -> Id -> Bool) ->
                  (s -> String -> Id -> BuildPlan (Maybe (s, String, [Id]))) ->
                  s -> [(String, Id)] -> BuildPlan (BuildResult s)
walkBinaryPlan seen load initial roots = do
    let visit state (ancestors, owner, p)
          | Set.member (getIdString p) ancestors = return (Right (state, []))
          | seen state p = return (Right (state, []))
          | otherwise = do
              next <- load state owner p
              case next of
                Nothing -> return (Right (state, []))
                Just (state', path, imported) ->
                  return (Right (state',
                    [(Set.insert (getIdString p) ancestors, path, i) |
                     i <- imported]))
    graph <- traverseState DepthFirst visit initial
               [(Set.empty, owner, p) | (owner,p) <- roots]
    -- Execution returns the selected graph. Discovery retains real ancestor
    -- state within each alternative, without inventing a combined graph.
    return $ fmap (\result -> case result of
      Left () -> internalError "walkBinaryPlan: unexpected traversal error"
      Right state -> state) graph

binaryCandidates :: Flags -> String -> Id -> BuildPlan [DependencyCandidate]
binaryCandidates flags owner i =
    requireFiles owner ("package-import:" ++ getIdString i) "one-of"
      [("object", dir ++ "/" ++ getIdString i ++ "." ++ binSuffix) | dir <- ifcPath flags]
      ["A compatible object is required in search-path order; source cannot substitute on this import edge.",
       "Binary-to-binary imports do not independently schedule source recompilation, even under -u."]

selectBinary :: String -> Id -> [DependencyCandidate] ->
                (FilePath -> BuildPlan a) -> BuildPlan a
selectBinary owner i candidates load =
    let available = map candidatePath (filter candidateExists candidates)
    in select (owner ++ ": object for " ++ getIdString i) 0 (map load available)

-- One interpretation owns this cache. Only decoded observations of its input
-- snapshot are memoized; traversal and requirement facts are replayed for every
-- branch. BuildPlan owns cache allocation and updates. All header reads occur
-- inside declareInputs, so execution does not speculatively intern identifiers.
type BinaryDependencyCache = FilePath -> BuildPlan (Either String [Id])

withBinaryDependencyCache :: (BinaryDependencyCache -> BuildPlan a) -> BuildPlan a
withBinaryDependencyCache = withCachedRead ("inspect package header " ++) $ \path -> do
    result <- tryDependency $ do
      bytes <- BS.readFile path
      case decodeBinFile path bytes of
        Left err -> return (Left (show err))
        Right (_, _, pkg, _) -> do
          let imported = map fst (ipkg_depends pkg)
          _ <- evaluate (sum (map (length . getIdString) imported))
          return (Right imported)
    return (either Left id result)

-- Read only the information required for dependency planning. Errors are
-- values here: reporting an unavailable header must not impose speculative
-- validation on a normal invocation which may never consume this object.
objectImports :: BinaryDependencyCache -> FilePath -> BuildPlan (Maybe [Id])
objectImports cache path = do
    key <- observe ("normalize package path " ++ path) $
      normalise <$> makeAbsolute path
    result <- cache key
    case result of
      Left reason -> incomplete (path ++ ": " ++ reason) >> return Nothing
      Right imported -> return (Just imported)

-- These input contracts inspect the same candidates and binary closure as
-- actual imports. They supply no value to execution: binary decoding interns
-- names globally, so executing speculative reads here would perturb compiler
-- ordering before readImports reaches the real consumption point.
readPackageDependenciesPlan :: BinaryDependencyCache -> ErrorHandle -> Flags -> String -> [Id] -> BuildPlan ()
readPackageDependenciesPlan cache _ flags owner imported = declareInputs $ do
    let seen packages i = M.member (getIdString i) packages
        load packages importer i = do
          candidates <- binaryCandidates flags importer i
          if null (filter candidateExists candidates)
            then incomplete (importer ++ ": package " ++ getIdString i ++
                   " has no available object; transitive dependencies are unknown") >> return Nothing
            else selectBinary importer i candidates $ \path -> do
              deps <- objectImports cache path
              return $ fmap (\ids -> (M.insert (getIdString i) () packages, path, ids)) deps
    _ <- walkBinaryPlan seen load M.empty [(owner,i) | i <- imported]
    return ()

readBinaryDependenciesPlan :: BinaryDependencyCache -> ErrorHandle -> Flags -> FilePath -> BuildPlan ()
readBinaryDependenciesPlan cache errh flags path = declareInputs $ do
    imported <- objectImports cache path
    case imported of
      Nothing -> return ()
      Just ids -> readPackageDependenciesPlan cache errh flags path ids

-- Import one .bo file
doImport :: ErrorHandle -> Flags -> HashMap -> Id ->
            IO (String, CSignature, CSignature, IPackage a, String,
                HashMap, [Id])
doImport errh flags hashmap i = do
    let binname = getIdString i ++ "." ++ binSuffix
        missingErr = (getIdPosition i,
                      EMissingBinFile binname (pfpString i))
        pillMsg = if (enablePoisonPills flags)
                  then bsWarning errh
                  else bsError errh
    (file, name) <- fromMaybeM (bsError errh [missingErr]) $
                      readBinFilePath errh (getIdPosition i)
                          (verbose flags) binname (ifcPath flags)
    (bi_sig, bo_sig, ipkg@(IPackage pi impHashes _ _ _), hash)
        <- readBinFile errh name file
    when (pi /= i) $
        bsError errh [(noPosition, EBinFilePkgNameMismatch name
                                       (pfpString i) (pfpString pi))]
    when (any hasPoisonPill [ e | IDef _ _ e _ <- ipkg_defs ipkg ]) $
        pillMsg [(getIdPosition pi, WPoisonedDefFile binname)]
    hashmap' <- mergeHashes errh hashmap pi hash impHashes
    let impNames = map fst impHashes
    return (name, bi_sig, bo_sig, ipkg, hash, hashmap', impNames)

hasPoisonPill :: IExpr a -> Bool
hasPoisonPill (ILam _ _ e)  = hasPoisonPill e
hasPoisonPill (ILAM _ _ e)  = hasPoisonPill e
hasPoisonPill (IAps f _ es) = any hasPoisonPill (f:es)
hasPoisonPill (ICon _ (ICPrim _ p)) = p == PrimPoisonedDef
hasPoisonPill _ = False

mergeHashes :: ErrorHandle -> HashMap -> Id -> String -> [(Id, String)] ->
               IO HashMap
mergeHashes errh hashmap binId binhash impHashes =
  let
      -- a package and its importer disagree about the hash
      mismatchErr1 pkg importer =
          let pkgfile = (pfpString pkg) ++ "." ++ binSuffix
          in  bsError errh
                  [(getIdPosition binId,
                    EBinFileSignatureMismatch pkgfile (pfpString importer))]

      -- two packages disagree about an imported file
      -- XXX we could determine which is wrong by loading the file itself
      mismatchErr2 pkg importer1 importer2 =
          let pkgfile = (pfpString pkg) ++ "." ++ binSuffix
          in  bsError errh
                  [(getIdPosition binId,
                    EBinFileSignatureMismatch2 pkgfile
                        (pfpString importer1) (pfpString importer2))]

      mergeFn :: Id -> (String, [Id]) -> (String, [Id]) ->
                 IO (String, [Id])
      mergeFn k (new_s, [new_i]) (old_s, old_is@(old_i:_))
          | new_s == old_s = return (old_s, (new_i:old_is))
          | new_i == k
              -- the "old_is" expected a different hash than the
              -- package "new_i" actually has
              = mismatchErr1 new_i old_i
          | k `elem` old_is
              -- the "new_i" is expecting different than the package
              -- actually has (and possibly other "old_is" agree)
              = mismatchErr1 k new_i
          | otherwise
              -- we don't yet know what the package's hash is,
              -- but two users disagree on its hash
              = mismatchErr2 k new_i old_i
      mergeFn _ new_val _ =
          internalError ("mergeHashes: " ++ ppReadable new_val)


      new_pairs = let mkImpPair (i,s) = (i, (s, [binId]))
                      imp_pairs = map mkImpPair impHashes
                      bin_pair = (binId, (binhash, [binId]))
                  in  (bin_pair : imp_pairs)
  in
      map_insertManyWithKeyM mergeFn new_pairs hashmap


-- =========================

-- Replace existing imports in a package with new "internal" ones
-- (where all defs are visible, not just the ones visible to the user).
-- This is used to create the full symbol table used for generating a
-- module.  (XXX can we get rid of this?)
replaceImportedSignatures :: CPackage -> [CSignature] -> CPackage
replaceImportedSignatures (CPackage i exps imps impsigs fixs defs includes) newsigs =
    CPackage i exps imps impsigs' fixs defs includes
  where sigMap = M.fromList [(i, sig) | sig@(CSignature i _ _ _) <- newsigs]
        impsigs' = map replaceSig impsigs
        replaceSig (CImpSign n q (CSignature i _ _ _)) =
            let errstr = "replaceImportedSignatures: missing sig: " ++ ppReadable i
            in  CImpSign n q (fromJustOrErr errstr (M.lookup i sigMap))

-- =========================
