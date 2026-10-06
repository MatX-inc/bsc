module ABinUtil (
                 HierMap, InstModMap, ABinMap, ABIHierarchy,
                 getABIHierarchy, assertNoSchedErr,
                 readAndCheckABin,
                 readAndCheckABinPath,
                 readAndCheckABinPathCatch,
                 ) where

import Data.List(nub, partition)
import Data.Maybe(isJust, fromJust)
import Control.Monad(when, unless)
import Control.Exception(evaluate)
import qualified Data.ByteString as BS
import Control.Monad.Except(ExceptT(..), runExceptT, throwError, catchError)
import Control.Monad.State(StateT, lift, get, put)
import System.FilePath((</>), normalise)
import BuildPlan
import DependencyReport(DependencyCandidate(..))

import Version(bscVersionStr)
import Backend
import FileNameUtil(abinSuffix)
import FileIOUtil(readBinaryFileCatch, readBinFilePath)
import Util(fromMaybeM)

import Error(internalError, EMsg, EMsgs(..), ErrMsg(..),
             ErrorHandle, bsError, bsWarning, convExceptTToIO)
import Id(Id, getIdString)
import Position(cmdPosition, noPosition, getPosition)
import PPrint
import ASyntax
import ASyntaxUtil(getForeignCallNames)
import VModInfo(vName, getVNameString)
import ForeignFunctions(ForeignFunction(..), ForeignFuncMap)
import ABin
import GenABin(readABinFile)

import qualified Data.Map as M

--import Debug.Trace(traceM)

-- ===============

-- Routines for processing ABI files to find the hierarchy,
-- identify missing or unused files, and return easy to use structures
-- for traversing the hierarchy.

-- ---------------
-- Data types

-- a map from a module name to pairs of (inst,mod) for each
-- non-prim submodule instantiation.  thus, by following from the top,
-- the entire hierarchy can be reconstructed.
-- (one list for normal modules and one list for no-inline modules)
type HierMap = M.Map String ([(String, String)], [(String, String)])

-- map from hierarchical instance name ("" for topmod, "foo" for
-- submod instance foo, "foo.bar" for instance bar inside foo, etc)
-- to name of the module of which it is an instance
type InstModMap = M.Map String String

-- map from module name to the name of the file it was read from
type ABinMap = M.Map String FilePath

type ABIHierarchy =
    (Id, HierMap, InstModMap, ForeignFuncMap, ABinMap, [String],
     [(String, (ABinEitherModInfo, String))])

-- ---------------

-- Monad for reading in .ba files
--
-- When linking Verilog, we want to try reading in a .ba hierarchy,
-- but fall back to using .v files if it fails.
-- Therefore, ExceptT is used to catch errors.  Serious failures can
-- still be reported immediately, via IO -- such as file version mismatch,
-- or read errors, etc.
--
type M = StateT MState (ExceptT EMsgs BuildPlan)

type ABinPathReader = ([String], String) ->
    BuildPlan (Maybe (String, Either EMsgs ABin))

-- monad state
data MState = MState {
         m_verbose :: Bool
       , m_ifc_path :: [String]
       , m_backend :: Maybe Backend
       , m_foreign_mods :: [String]
       , m_abmis_used :: [(String, (ABinEitherModInfo, String))]
       , m_abis_unused :: [(String, (String, ABin))]
       , m_foundmod_map :: HierMap
       , m_foundffunc_map :: ForeignFuncMap
       , m_abmi_file_map :: ABinMap
       , m_on_artifact :: FilePath -> String -> ABin -> BuildPlan ()
       , m_read_abi :: ABinPathReader
     }

addMod :: String -> ABinEitherModInfo -> String -> M ()
addMod name abmi ver = do
    s <- get
    let abmis = m_abmis_used s
    put (s { m_abmis_used = ((name,(abmi,ver)):abmis) })

addForeignMod :: String -> M ()
addForeignMod name = do
    s <- get
    let fms = m_foreign_mods s
        -- we know that "addForeignMod" is only called on new names,
        -- so "name" does not exist in "fms"
        fms' = (name:fms)
    put (s { m_foreign_mods = fms' })

recordFile :: String -> FilePath -> M ()
recordFile name file = do
    s <- get
    let new_map = M.insert name file (m_abmi_file_map s)
    put (s { m_abmi_file_map = new_map })

getABIs :: M [(String, (String, ABin))]
getABIs = get >>= return . m_abis_unused

setABIs :: [(String, (String, ABin))] -> M ()
setABIs abis = get >>= \s -> put (s { m_abis_unused = abis })

getBackend :: M (Maybe Backend)
getBackend = get >>= return . m_backend

getHierMap :: M HierMap
getHierMap = get >>= return . m_foundmod_map

putHierMap :: HierMap -> M ()
putHierMap m = get >>= \s -> put (s { m_foundmod_map = m })

-- ---------------


-- prim_names = list of primtives which don't need .ba files
getABIHierarchy ::
    ErrorHandle -> Bool -> [String] -> Maybe Backend ->
    [String] -> String -> [(String, ABin)] ->
    (FilePath -> String -> ABin -> BuildPlan ()) ->
    BuildPlan (BuildResult (Either EMsgs ABIHierarchy))
getABIHierarchy errh be_verbose ifc_path backend prim_names topname fabis onArtifact =
  withABinPathCache errh backend $ \readPath -> do
    -- pair the abis with their module name
    let
        pair_with_name (f,abi) = (getIdString (getABIName abi), (f,abi))
        fabis_by_name = map pair_with_name fabis

    -- create the initial state
    let state0 = MState {
                     m_verbose = be_verbose,
                     m_ifc_path = ifc_path,
                     m_backend = backend,
                     m_foreign_mods = [],
                     m_abmis_used = [],
                     m_abis_unused = fabis_by_name,
                     m_foundmod_map = start_hiermap,
                     m_foundffunc_map = start_ffuncmap,
                     m_abmi_file_map = start_filemap,
                     m_on_artifact = onArtifact,
                     m_read_abi = readPath
                 }
        existing_mods = prim_names
        no_mod_children m = (m,([],[]))
        start_hiermap = M.fromList (map no_mod_children existing_mods)
        start_ffuncmap = M.empty
        start_filemap = M.fromList [ (n,f) | (n,(f,_)) <- fabis_by_name ]

    let follow = followABIHierarchy Nothing topname `catchError` \err -> do
            lift $ lift $ incomplete
                ("Cannot discover the complete elaboration hierarchy for " ++ topname ++
                 "; its transitive artifact dependencies are not known.")
            throwError err
    result <- runStatePlan follow state0
    performResult (either (return . Left)
        (runExceptT . finishABIHierarchy errh topname) <$> result)

finishABIHierarchy :: ErrorHandle -> String -> (Id, MState) ->
                      ExceptT EMsgs IO ABIHierarchy
finishABIHierarchy errh topname (topmodId, end_state) = do
    let hiermap0  = m_foundmod_map end_state
        ffuncmap = m_foundffunc_map end_state
        filemap  = m_abmi_file_map end_state
        modinfos_used_by_name = m_abmis_used end_state
        foreign_mods = nub $ m_foreign_mods end_state

    -- if a module constructor is both an unresolved foreign import
    -- and something we already know about, something has gone
    -- badly wrong
    let foreignHierErr mod =
          internalError ("ABinUtil: inconsistent hiermap - " ++ mod ++
                         ppReadable (foreign_mods, hiermap0))

    -- we add foreign_mods to the hiermap after followABIHierarchy
    -- so we can construct the the InstModMap even if there are
    -- import "BVI"s present (for bluetcl)
    -- we don't add them earlier so that Bluesim can error
    -- if there is an unsupported import (in followABIHierarchy)
    let hiermap = M.unionWithKey foreignHierErr
                                 hiermap0
                                 (M.fromList [(name, ([], [])) | name <- foreign_mods])

    -- report warnings for any unused abi files
    let remaining_mods = m_abis_unused end_state
        remaining_fnames = map (fst . snd) remaining_mods
    when (not (null remaining_mods)) $
        lift $ bsWarning errh [(cmdPosition, WExtraABinFiles remaining_fnames)]

    -- this is a mapping from a hierarchical instance name
    -- to the name of the module of which it is an instance
    instmap <- case (hierMapToInstModMap hiermap topname) of
                 Left emsgs -> lift $ bsError errh emsgs
                 Right res -> return res
    --traceM("instmap = " ++ ppReadable (M.toList instmap))

    return (topmodId, hiermap0, instmap, ffuncmap, filemap, foreign_mods,
            modinfos_used_by_name)


-- ---------------

-- function to confirm that the modules do not have errors and to return
-- back the abmis with just the success data types

assertNoSchedErr :: [(String, (ABinEitherModInfo, String))] ->
                    ExceptT EMsgs IO
                        [(String, (ABinModInfo, String))]
assertNoSchedErr modinfos_by_name =
    let assertOne :: (String, (ABinEitherModInfo, String)) ->
                     ExceptT EMsgs IO
                         (String, (ABinModInfo, String))
        assertOne (name, (eabmi, ver)) =
            case eabmi of
              Right abmi -> return (name, (abmi, ver))
              Left _ -> throwError
                            (EMsgs [(cmdPosition, EABinModSchedErr name Nothing)])
    in  mapM assertOne modinfos_by_name


-- ---------------

-- Given:
--   * maybe the name of the parent module (Nothing if this is the topmod)
--   * the ABI for the module
-- And from the monad state
--   * a hiermap containing the modules we know about so far
--     (used to not descend into instances of modules we've already done)
--   * a list of the ABI provided by the user, which should contain any
--     submodules we find (if not, we try to find the file, then error)
-- find the submodules, add them to the map, and descend into any new modules.
-- Updates the map in the monad state and leaves behind any extra ABIs
-- (which the caller should treat as a warning condition if non-empty).
-- Returns the Id of the module which was processed.
followABIHierarchy :: Maybe String -> String -> M Id
followABIHierarchy mparent curmod_name = do
    e_abmi <- findModABI mparent curmod_name
    let apkg = abemi_apkg e_abmi
    followABMIHierarchy apkg
    return $ apkg_name apkg

followABMIHierarchy :: APackage -> M ()
followABMIHierarchy curpkg = do

    -- ----------
    -- get the instantiated module instances

    let
        getModuleName avi = getVNameString $ vName $ avi_vmi avi
        getInstanceName avi = getIdString $ avi_vname avi
        mkPair avi = (getInstanceName avi, getModuleName avi)

        curmod_avinsts = apkg_state_instances curpkg

        -- identify the foreign imports
        (foreign_avis, native_avis) =
            partition avi_user_import curmod_avinsts

        -- info for all submodules
        submod_pairs = map mkPair curmod_avinsts

        -- just the module names of submods
        -- (nub because could be instances of the same module)
        native_submod_names = nub $ map getModuleName native_avis

    -- ----------
    -- add the foreign imports

    let addFModUse :: AVInst -> M ()
        addFModUse avi = do
            let mod = getModuleName avi
            hmap <- getHierMap
            -- primitives will already be in the map
            -- (as well as foreign imports already encountered)
            if mod `M.member` hmap
              then return ()
              else do
                  -- error if the backend is Bluesim
                  backend <- getBackend
                  when ((backend == (Just Bluesim)) &&
                        (not (null foreign_avis))) $
                      let parent = getIdString (apkg_name curpkg)
                          pos = getPosition (avi_vname avi)
                          inst = getInstanceName avi
                          err = (pos, EBSimForeignImport mod inst parent)
                      in  throwError (EMsgs [err])
                  -- add the use
                  addForeignMod mod

    independentlyStateT (map addFModUse foreign_avis)

    -- ----------
    -- get the noinline functions (which are also modules)

    let
        curmod_defs = apkg_local_defs curpkg

        func_pairs =
            [ (inst_name, mod_name)
                | (ADef _ _
                    (ANoInlineFunCall _ _
                      (ANoInlineFun mod_name _ _ (Just inst_name))
                      _) _) <- curmod_defs ]

        func_names = nub $ map snd func_pairs

    -- ----------
    -- get the foreign function uses

        ffunc_names = getForeignCallNames curpkg

    -- ----------
    -- extend the found module map

    foundmod_map <- getHierMap
    let
        -- extend the map with the pairs found
        curmodname = getIdString $ apkg_name curpkg
        new_foundmap =
            M.insert curmodname (submod_pairs,func_pairs) foundmod_map

    putHierMap new_foundmap

    -- ----------
    -- function to add the ffunc uses

    let
        addFFuncUse :: String -> M ()
        addFFuncUse ffunc_name = do
            s <- get
            let ffunc_map = m_foundffunc_map s
            if ffunc_name `M.member` ffunc_map
              then return ()
              else do abi <- findForeignFuncABI curmodname ffunc_name
                      let ffinfo = abffi_foreign_func abi
                      let ffunc_map' = M.insert ffunc_name ffinfo ffunc_map
                      s <- get
                      put (s { m_foundffunc_map = ffunc_map' })

    independentlyStateT (map addFFuncUse ffunc_names)

    -- ----------
    -- function to traverse the submods

    let
        followOneSubMod :: String -> M ()
        followOneSubMod modname = do
            s <- get
            let hier_map = m_foundmod_map s
            if modname `M.member` hier_map
              then return ()
              else followABIHierarchy (Just curmodname) modname >> return ()

    -- we don't follow foreign modules (which includes primitives)
    independentlyStateT (map followOneSubMod (native_submod_names ++ func_names))

-- ---------------

findModABI :: Maybe String -> String -> M ABinEitherModInfo
findModABI mparent modname = do
    mod <- findABI True mparent modname
    case (mod) of
        (ABinMod modinfo ver) -> do addMod modname (Right modinfo) ver
                                    return (Right modinfo)
        (ABinModSchedErr modinfo ver) ->
            -- only the top module can have a schedule error
            case mparent of
              Nothing -> do addMod modname (Left modinfo) ver
                            return (Left modinfo)
              Just parent -> throwError
                                 (EMsgs [(cmdPosition,
                                          EABinModSchedErr modname mparent)])
        _ -> throwError
                 (EMsgs [(cmdPosition, EWrongABinTypeExpectedModule modname mparent)])


findForeignFuncABI :: String -> String -> M ABinForeignFuncInfo
findForeignFuncABI parent ffuncname = do
    ffunc <- findABI False (Just parent) ffuncname
    case (ffunc) of
        (ABinForeignFunc ffuncinfo _) -> return ffuncinfo
        _ -> throwError
                 (EMsgs [(cmdPosition,
                          EWrongABinTypeExpectedForeignFunc ffuncname parent)])


-- The first two arguments are for better reporting of errors.
-- The first is whether this is a module being looked up (not a foreignfunc).
-- The second indicates whether we are looking for the top module (Nothing)
-- or a module instantiated by another module in the design (Just parent).
findABI :: Bool -> Maybe String -> String -> M ABin
findABI isMod mparent lookup_name = do
    -- first, try to find it in the provided modules
    abis <- getABIs
    let (found_abis, other_abis) =
            partition (\ (i,a) -> i == lookup_name) abis
        explicit_paths = map (normalise . fst . snd) found_abis
        matchesRequestedKind (ABinMod {}) = isMod
        matchesRequestedKind (ABinModSchedErr {}) = isMod
        matchesRequestedKind (ABinForeignFunc {}) = not isMod
        fromPath :: Bool -> M ABin
        fromPath fallback = do
            s <- get
            let be_verbose = m_verbose s
                ifc_path   = m_ifc_path s
                backend    = m_backend s
                err = if (isMod)
                      then (cmdPosition,
                            EMissingABinModFile lookup_name mparent)
                      else
                       case (mparent) of
                         Just parent ->
                           (cmdPosition,
                            EMissingABinForeignFuncFile lookup_name parent)
                         Nothing -> internalError "findABI: ffunc mparent"
            let owner = maybe lookup_name id mparent
                role | fallback = "consumed-explicit-elaboration-fallback"
                     | not isMod = "foreign-function-elaboration"
                     | isJust mparent = "module-elaboration"
                     | otherwise = "top-elaboration"
                policy | fallback = "optional"
                       | backend == Just Bluesim = "one-of"
                       | otherwise = "optional"
            candidates <- lift $ lift $
                requireFiles owner role policy
                    [("elaboration", dir </> (lookup_name ++ "." ++ abinSuffix))
                        | dir <- ifc_path]
                    ["Candidates are in interface-search-path order; absent candidates remain existence dependencies."
                    ,"A .ba input is not replaced by source, a package object, or generated Verilog."]
            let paths = [(dir, candidate) | (dir, candidate) <- zip ifc_path candidates,
                         not fallback || (candidateExists candidate &&
                           normalise (candidatePath candidate) `notElem` explicit_paths)]
                -- Candidate probes are discovery-only; the ordinary lookup
                -- always uses the configured search path directly.
                search_path = if fallback then map fst paths else ifc_path
            when (fallback && null paths) $ lift $ lift $
                noAlternative ("No distinct provider after consuming explicit elaboration " ++ lookup_name)
            when (not fallback && null (filter candidateExists candidates) && backend == Just Bluesim) $
                lift $ lift $ incomplete
                    ("Missing elaboration metadata for " ++ lookup_name ++
                     "; its transitive artifact dependencies are not known.")
            -- The real lookup stays execution-only. Discovery checks each
            -- candidate before opening it, so a distribution .ba need not be
            -- decoded merely to discover that its whole subtree is opaque.
            let selected = runExceptT $
                    readAndCheckABinPathPlan (m_read_abi s) be_verbose search_path lookup_name
                available = [(dir, candidate) | (dir, candidate) <- paths,
                             candidateExists candidate]
                inspect (dir, candidate) = do
                    external <- inspectDependency (candidatePath candidate)
                    unless external $ noAlternative
                        ("Distribution elaboration " ++ candidatePath candidate)
                    runExceptT $
                        readAndCheckABinPathPlan (m_read_abi s) be_verbose [dir] lookup_name
                alternatives = if null available then [selected] else map inspect available
            result <- lift $ lift $ searchAlternatives
                ("elaboration search for " ++ lookup_name) selected alternatives
            mfile <- either throwError return result
            (file, abi) <- fromMaybeM (throwError (EMsgs [err])) (return mfile)
            recordFile lookup_name file
            return abi
        fromExplicit :: M ABin
        fromExplicit = case found_abis of
            [(_,(_,abi))] -> setABIs other_abis >> return abi
            files -> do
                let fnames = map (fst . snd) files
                lift $ lift $ incomplete
                    ("Multiple explicit elaboration inputs define " ++ lookup_name ++
                     "; ordinary linking rejects this ambiguity.")
                selectStateT ("explicit elaboration alternatives for " ++ lookup_name) 0 $
                    throwError (EMsgs [(cmdPosition,
                        EMultipleABinFilesForName lookup_name fnames)]) :
                    [setABIs other_abis >> recordFile lookup_name path >> return abi
                        | (_, (path, abi)) <- files]
    abi <- case found_abis of
        [] -> fromPath False
        [(_, (_, abi))] | matchesRequestedKind abi ->
            -- A prior successful same-kind use would be in its visited map;
            -- an other-kind use would reject this record and stop. Neither
            -- can consume it and leave this lookup needing a path fallback.
            fromExplicit
        _ ->
            -- Foreign functions and modules have separate visited maps but
            -- share the explicit inputs. A preceding foreign-function use
            -- can consume an explicit record with this module's link name.
            -- Retain that path-search possibility without probing it during
            -- execution or treating absent speculative providers as errors.
            selectStateT ("explicit or previously consumed elaboration " ++ lookup_name) 0
                [fromExplicit, setABIs other_abis >> fromPath True]
    s <- get
    let path = M.findWithDefault (lookup_name ++ "." ++ abinSuffix) lookup_name (m_abmi_file_map s)
    lift $ lift $ m_on_artifact s path lookup_name abi
    return abi

-- ---------------

hierMapToInstModMap :: HierMap -> String -> Either [EMsg] InstModMap
hierMapToInstModMap hiermap topmod =
    let
        addSubMods _ _ _ res@(Left _) = res
        addSubMods mods_so_far inst_so_far (inst, mod) (Right imap) =
          if (isJust (lookup mod mods_so_far))
          then let cycle = (mod, inst) :
                           takeWhile ((/= mod) . fst) mods_so_far
               in  Left [(noPosition, ECircularABin mod (reverse cycle))]
          else
            let inst_so_far' = if (inst_so_far == "")
                               then inst
                               else inst_so_far ++ "." ++ inst
                mods_so_far' = ((mod, inst) : mods_so_far)
                imap' = Right $ M.insert inst_so_far' mod imap
                ims = case (M.lookup mod hiermap) of
                          Nothing -> internalError ("hierMapToInstModMap" ++ ppReadable (mod, hiermap))
                          Just (xs,ys) -> xs ++ ys
            in  foldr (addSubMods mods_so_far' inst_so_far') imap' ims
    in
        addSubMods [] "" ("", topmod) (Right M.empty)


-- ===============

getABIName :: ABin -> Id
-- for modules, the abiname is qualified and ends in "_"
getABIName (ABinMod modinfo _) = apkg_name (abmi_apkg modinfo)
getABIName (ABinModSchedErr modinfo _) = apkg_name (abmsei_apkg modinfo)
-- for funcs, refer to the link name
-- (because, for now, the APackage references only the link
-- name and does not include the source name)
getABIName (ABinForeignFunc funcinfo _) =
    ff_name (abffi_foreign_func funcinfo)

-- ===============

-- given a relative filename for an ABin file,
-- returns the filename and the contents
readAndCheckABin :: ErrorHandle -> Maybe Backend -> String -> IO (String, ABin)
readAndCheckABin errh backend filename = do
    contents <- readBinaryFileCatch errh noPosition filename
    abin <- either (bsError errh) return $
                decodeABin errh backend filename contents
    return (filename,abin)

-- given a module name, looks through the path for the ABin file,
-- returns the filename and the contents
readAndCheckABinPath :: ErrorHandle ->
                        Bool -> [String] -> (Maybe Backend) -> String ->
                        (ExceptT EMsgs IO) (Maybe (String, ABin))
readAndCheckABinPath errh be_verbose path backend mod_name = ExceptT $
    executePlan $ withABinPathCache errh backend $ \readPath -> runExceptT $
        readAndCheckABinPathPlan readPath be_verbose path mod_name

-- Cache immutable reads and decoded metadata without merging the conditions
-- under which each hierarchy edge is visited. Validation stays lazy here so
-- the caller can emit the ordinary progress message before a validation error.
withABinPathCache :: ErrorHandle -> Maybe Backend ->
                    (ABinPathReader -> BuildPlan a) -> BuildPlan a
withABinPathCache errh backend = withCachedRead label readPath
  where
    label (_, mod_name) = "open elaboration " ++ mod_name ++ "." ++ abinSuffix
    readPath (path, mod_name) = do
        let binname = mod_name ++ "." ++ abinSuffix
        mread <- readBinFilePath errh noPosition False binname path
        return $ case mread of
          Nothing -> Nothing
          Just (contents, filename) -> Just (filename,
            case decodeABin errh backend binname contents of
              Left msgs -> Left (EMsgs msgs)
              Right abi ->
                let file_mod_name = getIdString (getABIName abi)
                in if file_mod_name == mod_name
                   then Right abi
                   else Left (EMsgs [(noPosition,
                       EABinNameMismatch mod_name filename file_mod_name)]))

-- Opening, progress output and validation remain in their original order.
-- Discovery can read and validate the same bytes while suppressing progress
-- output, including when validation rejects an artifact.
readAndCheckABinPathPlan :: ABinPathReader -> Bool -> [String] -> String ->
                            ExceptT EMsgs BuildPlan (Maybe (String, ABin))
readAndCheckABinPathPlan readPath be_verbose path mod_name = do
    mread <- lift $ readPath (path, mod_name)
    case mread of
      Nothing -> return Nothing
      Just (filename, result) -> do
          lift $ perform $ when be_verbose (putStrLn ("read " ++ filename))
          ExceptT $ observe ("validate elaboration " ++ filename) $ evaluate $
              fmap (\abi -> Just (filename, abi)) result

readAndCheckABinPathCatch ::
    ErrorHandle -> Bool -> [String] -> (Maybe Backend) -> String -> EMsg ->
    IO (String, ABin)
readAndCheckABinPathCatch errh be_verbose path backend mod_name errmsg = do
    mabi <- convExceptTToIO errh $
            readAndCheckABinPath errh be_verbose path backend mod_name
    case mabi of
      Nothing -> bsError errh [errmsg]
      Just abi -> return abi

decodeABin :: ErrorHandle -> Maybe Backend -> String -> BS.ByteString ->
              Either [EMsg] ABin
decodeABin errh backend filename contents =
    let (abi, _) = readABinFile errh filename contents
    in
      -- XXX do something to check the sig?
      -- XXX check that each module has the signature of the others?
      -- does the ABI BSC version match?
      if (ab_version abi /= bscVersionStr True)
      then
          -- reuse message for Bin rather than create a new error for ABin
          Left [(noPosition, EBinFileVerMismatch filename)]
      else
          -- does the backend match?
          case (abi) of
            -- foreign funcs .ba-files aren't specific to a backend
            (ABinForeignFunc {}) -> Right abi
            -- check the backend
            (ABinMod modinfo _) ->
                let mod_backend = apkg_backend (abmi_apkg modinfo)
                in  if (not (backendMatches backend mod_backend))
                    then Left [(noPosition,
                                EABinFileBackendMismatch filename
                                  -- we know that the backends were not Nothing
                                  (ppString (fromJust backend))
                                  (ppString (fromJust mod_backend)))]
                    else Right abi
            -- backend check not required, since codegen cannot proceed
            -- from this point
            (ABinModSchedErr {}) -> Right abi

-- ===============
