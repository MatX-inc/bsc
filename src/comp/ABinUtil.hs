module ABinUtil (
                 HierMap, InstModMap, ABinMap,
                 getABIHierarchy, assertNoSchedErr,
                 readAndCheckABin,
                 readAndCheckABinPath,
                 readAndCheckABinPathCatch,
                 readAndCheckForeignPathCatch,
                 isStaleABinFile,
                 ) where

import Data.List(nub, partition)
import Data.Maybe(isJust, fromJust)
import Control.Monad(when)
import Control.Monad.Except(ExceptT, throwError)
import Control.Monad.State(StateT, runStateT, lift, get, put)
import System.Directory(doesFileExist)
import System.Exit(ExitCode)
import System.IO.Error(ioeGetErrorString)

import Version(bscVersionStr)
import Backend
import Flags(Flags)
import qualified PhaseConfig as PC
import FileNameUtil(abinSuffix, bdpiSuffix, bmodSuffix, bschedSuffix, hasDotSuf, dropSuf)
import FileIOUtil(readBinaryFileCatch, readBinFilePath, readBinaryFileMaybe)
import Util(fromMaybeM)

import Error(internalError, EMsg, EMsgs(..), ErrMsg(..),
             ErrorHandle, bsError, bsWarning, convExceptTToIO)
import Eval(rnf)
import Id(Id, getIdString)
import Position(cmdPosition, noPosition, getPosition)
import PPrint
import ASyntax
import ASyntaxUtil(getForeignCallNames)
import VModInfo(vName, getVNameString)
import ForeignFunctions(ForeignFunction(..), ForeignFuncMap)
import ABin
import GenABin(readABinFile, readABinFileMaybe)
import GenBDPI(BDPI(..), readBDPIFile, readBDPIFileMaybe)
import GenModule(readModulePair, readBModFileMaybe, readBSchedFileMaybe)
import AModule(AModule(..), BSched(..))

import qualified Control.Exception as CE

import qualified Data.Map as M
import qualified Data.ByteString as B

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

-- ---------------

-- Monad for reading module and foreign metadata artifacts
--
-- When linking Verilog, we want to try reading the artifact hierarchy,
-- but fall back to using .v files if it fails.
-- Therefore, ExceptT is used to catch errors.  Serious failures can
-- still be reported immediately, via IO -- such as file version mismatch,
-- or read errors, etc.
--
type M = StateT MState (ExceptT EMsgs IO)

-- monad state
data MState = MState {
         m_errHandle :: ErrorHandle
       , m_flags :: Flags
       , m_verbose :: Bool
       , m_ifc_path :: [String]
       , m_backend :: Maybe Backend
       , m_foreign_mods :: [String]
       , m_abmis_used :: [(String, (ABinEitherModInfo, String))]
       , m_abis_unused :: [(String, (String, ABin))]
       , m_foundmod_map :: HierMap
       , m_foundffunc_map :: ForeignFuncMap
       , m_abmi_file_map :: ABinMap
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
    ErrorHandle -> Flags -> Bool -> [String] -> (Maybe Backend) ->
    [String] -> String -> [(String, ABin)] ->
    ExceptT EMsgs IO
        (Id, HierMap, InstModMap, ForeignFuncMap, ABinMap, [String],
         [(String, (ABinEitherModInfo, String))])
getABIHierarchy errh flags be_verbose ifc_path backend prim_names topname fabis = do
    -- pair the abis with their module name
    let
        pair_with_name (f,abi) = (getIdString (getABIName abi), (f,abi))
        fabis_by_name = map pair_with_name fabis

    -- create the initial state
    let state0 = MState {
                     m_errHandle = errh,
                     m_flags = flags,
                     m_verbose = be_verbose,
                     m_ifc_path = ifc_path,
                     m_backend = backend,
                     m_foreign_mods = [],
                     m_abmis_used = [],
                     m_abis_unused = fabis_by_name,
                     m_foundmod_map = start_hiermap,
                     m_foundffunc_map = start_ffuncmap,
                     m_abmi_file_map = start_filemap
                 }
        existing_mods = prim_names
        no_mod_children m = (m,([],[]))
        start_hiermap = M.fromList (map no_mod_children existing_mods)
        start_ffuncmap = M.empty
        start_filemap = M.fromList [ (n,f) | (n,(f,abi)) <- fabis_by_name,
                                           isModuleABI abi ]

    (topmodId, end_state)
        <- runStateT (followABIHierarchy Nothing topname) state0

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
                                 (M.fromList (map no_mod_children foreign_mods))

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

    mapM_ addFModUse foreign_avis

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

    mapM_ addFFuncUse ffunc_names

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
    mapM_ followOneSubMod (native_submod_names ++ func_names)

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


-- The first argument selects the module or foreign-function namespace.
-- The second indicates whether we are looking for the top module (Nothing)
-- or a module instantiated by another module in the design (Just parent).
findABI :: Bool -> Maybe String -> String -> M ABin
findABI isMod mparent lookup_name = do
    -- A module and foreign function may share a name. Keep the other kind
    -- available for its own lookup, even when both were provided explicitly.
    abis <- getABIs
    let (found_abis, other_abis) =
            partition (\ (i,(_,abi)) -> i == lookup_name &&
                                       isModuleABI abi == isMod) abis
    case found_abis of
        [(_,(_,abi))] -> setABIs other_abis >> return abi
        [] -> do -- try to find the module in the path
            s <- get
            let be_verbose = m_verbose s
                ifc_path   = m_ifc_path s
                backend    = m_backend s
                errh       = m_errHandle s
                flags      = m_flags s
                -- Preserve the wrong-kind diagnostic for an explicit input
                -- only when no artifact in the requested namespace is found.
                wrongKindProvided = any ((== lookup_name) . fst) abis
                err = if wrongKindProvided
                      then (cmdPosition,
                            if isMod
                            then EWrongABinTypeExpectedModule lookup_name mparent
                            else case mparent of
                              Just parent -> EWrongABinTypeExpectedForeignFunc
                                                 lookup_name parent
                              Nothing -> internalError "findABI: ffunc mparent")
                      else if (isMod)
                      then (cmdPosition,
                            EMissingABinModFile lookup_name mparent)
                      else
                       case (mparent) of
                         Just parent ->
                           (cmdPosition,
                            EMissingABinForeignFuncFile lookup_name parent)
                         Nothing -> internalError "findABI: ffunc mparent"
            (file, abi) <-
                fromMaybeM (throwError (EMsgs [err])) $
                lift $ readAndCheckArtifactPath isMod errh flags be_verbose ifc_path backend
                           lookup_name
            -- This map is consumed by module code-generation reuse checks;
            -- foreign metadata must not replace a same-named module's path.
            when isMod $ recordFile lookup_name file
            return abi
        files -> let fnames = map (fst . snd) files
                 in  throwError
                         (EMsgs [(cmdPosition,
                                  EMultipleABinFilesForName lookup_name fnames)])

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

isModuleABI :: ABin -> Bool
isModuleABI (ABinMod {}) = True
isModuleABI (ABinModSchedErr {}) = True
isModuleABI (ABinForeignFunc {}) = False

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

-- Given a module pair member, foreign metadata file, or legacy ABin file,
-- returns the filename and the contents
readAndCheckABin :: ErrorHandle -> Flags -> Maybe Backend -> String -> IO (String, ABin)
readAndCheckABin errh flags backend filename = do
    (canonical, abi) <-
        if hasDotSuf bmodSuffix filename || hasDotSuf bschedSuffix filename
        then readModulePair errh (PC.materializeConfig flags) filename
        else do contents <- readBinaryFileCatch errh noPosition filename
                abi <- if hasDotSuf bdpiSuffix filename
                       then loadBDPI errh filename contents
                       else return (fst (readABinFile errh filename contents))
                return (filename, abi)
    checked <- either (bsError errh) return (checkABin backend canonical abi)
    return (canonical, checked)

-- Keep the existing hierarchy API while foreign metadata has its own format.
bdpiToABin :: BDPI -> ABin
bdpiToABin bdpi =
    ABinForeignFunc
        (ABinForeignFuncInfo (bdpi_src_name bdpi) (bdpi_foreign_func bdpi))
        (bdpi_version bdpi)

-- Force the complete payload before returning it to a backend. Preserve
-- cancellation and diagnostics already reported by the version checks.
loadBDPI :: ErrorHandle -> FilePath -> B.ByteString -> IO ABin
loadBDPI errh filename bytes = load `CE.catch` handler
  where
    load = do
        let bdpi = readBDPIFile errh filename bytes
        _ <- CE.evaluate (rnf bdpi)
        return (bdpiToABin bdpi)
    handler :: CE.SomeException -> IO ABin
    handler exception =
        case CE.fromException exception :: Maybe CE.AsyncException of
          Just _ -> CE.throwIO exception
          Nothing -> case CE.fromException exception :: Maybe ExitCode of
            Just _ -> CE.throwIO exception
            Nothing -> bsError errh [(noPosition, EFileReadFailure filename
                ("invalid foreign metadata artifact: " ++ CE.displayException exception))]

-- Search for a module pair in each directory, falling back to a legacy .ba
-- there. A readable pair with a missing/bad partner is an error; an unreadable
-- candidate is warned about and skipped, just as for legacy path searches.
-- Pair members are never taken from different directories.
readAndCheckABinPath :: ErrorHandle -> Flags ->
                        Bool -> [String] -> Maybe Backend -> String ->
                        ExceptT EMsgs IO (Maybe (String, ABin))
readAndCheckABinPath = readAndCheckArtifactPath True

data ArtifactCandidate = ModulePairCandidate String Bool Bool
                       | BDPICandidate String
                       | LegacyABinCandidate String

readAndCheckArtifactPath :: Bool -> ErrorHandle -> Flags ->
                           Bool -> [String] -> Maybe Backend -> String ->
                           ExceptT EMsgs IO (Maybe (String, ABin))
readAndCheckArtifactPath isModule errh flags be_verbose path backend mod_name = do
    candidates <- lift $ fmap concat (mapM findCandidate path)
    selected <- lift $ findReadable candidates
    case selected of
      Nothing -> return Nothing
      Just (candidate, load) -> do
        let chosen = candidateName candidate
            others = filter (/= chosen) (map candidateName candidates)
        when (length candidates > 1) $
            lift $ bsWarning errh
                [(noPosition, WMultipleFilesInPath chosen others)]
        (canonical, raw) <- lift load
        abi <- either (throwError . EMsgs) return (checkABin backend canonical raw)
        let file_mod_name = getIdString (getABIName abi)
        if file_mod_name == mod_name
          then return (Just (canonical, abi))
          else throwError (EMsgs [(noPosition,
                   EABinNameMismatch mod_name canonical file_mod_name)])
  where
    binname suffix = mod_name ++ "." ++ suffix
    filename dir suffix = dir ++ "/" ++ binname suffix
    candidateName (ModulePairCandidate dir _ _) = filename dir bschedSuffix
    candidateName (BDPICandidate dir) = filename dir bdpiSuffix
    candidateName (LegacyABinCandidate dir) = filename dir abinSuffix

    findCandidate dir = do
        hasSchedule <- if isModule
                       then doesFileExist (filename dir bschedSuffix)
                       else return False
        hasModule <- if isModule
                     then doesFileExist (filename dir bmodSuffix)
                     else return False
        if hasSchedule || hasModule
          then return [ModulePairCandidate dir hasModule hasSchedule]
          else do hasBDPI <- if isModule
                             then return False
                             else doesFileExist (filename dir bdpiSuffix)
                  if hasBDPI
                    then return [BDPICandidate dir]
                    else do hasLegacy <- doesFileExist (filename dir abinSuffix)
                            return [LegacyABinCandidate dir | hasLegacy]

    findReadable [] = return Nothing
    findReadable (candidate:rest) = do
        load <- readCandidate candidate
        case load of
          Just action -> return (Just (candidate, action))
          Nothing -> findReadable rest

    readCandidate candidate@(ModulePairCandidate dir hasModule hasSchedule) = do
        -- Readability failures retain the existing S0088 warning. Skip the
        -- entire pair so its other half cannot be selected a second time.
        -- Absent siblings are left for readModulePair to diagnose as errors.
        scheduleReadable <- readableIfPresent dir bschedSuffix hasSchedule
        moduleReadable <- if scheduleReadable
                          then readableIfPresent dir bmodSuffix hasModule
                          else return False
        return $ if scheduleReadable && moduleReadable
                 then Just (readModulePair errh (PC.materializeConfig flags)
                                (candidateName candidate))
                 else Nothing
    readCandidate (BDPICandidate dir) = do
        -- Prefer the dedicated format within each search directory. Decode
        -- only after selecting it, so corrupt metadata cannot fall back to .ba.
        found <- readBinFilePath errh noPosition be_verbose (binname bdpiSuffix) [dir]
        return $ case found of
          Nothing -> Nothing
          Just (contents, name) ->
              Just (do abi <- loadBDPI errh name contents
                       return (name, abi))
    readCandidate (LegacyABinCandidate dir) = do
        found <- readBinFilePath errh noPosition be_verbose (binname abinSuffix) [dir]
        return $ case found of
          Nothing -> Nothing
          Just (contents, name) ->
              Just (return (name, fst (readABinFile errh name contents)))

    readableIfPresent _ _ False = return True
    readableIfPresent dir suffix True = do
        let name = filename dir suffix
            handler :: CE.IOException -> IO Bool
            handler ioe = do
                bsWarning errh [(noPosition,
                    WFileExistsButUnreadable name (ioeGetErrorString ioe))]
                return False
            -- Use a strict read so the probe closes its handle before the
            -- pair reader runs and catches failures anywhere in the file.
            probe = do
                _ <- B.readFile name
                when be_verbose $ putStrLn ("read " ++ name)
                return True
        probe `CE.catch` handler

readAndCheckABinPathCatch ::
    ErrorHandle -> Flags -> Bool -> [String] -> (Maybe Backend) -> String -> EMsg ->
    IO (String, ABin)
readAndCheckABinPathCatch errh flags be_verbose path backend mod_name errmsg = do
    mabi <- convExceptTToIO errh $
            readAndCheckABinPath errh flags be_verbose path backend mod_name
    case mabi of
      Nothing -> bsError errh [errmsg]
      Just abi -> return abi

-- Foreign metadata prefers .bdpi, with legacy .ba fallback in each directory.
-- This lookup is separate from modules; the namespaces may share a basename.
readAndCheckForeignPathCatch ::
    ErrorHandle -> Flags -> Bool -> [String] -> Maybe Backend -> String -> EMsg ->
    IO (String, ABin)
readAndCheckForeignPathCatch errh flags be_verbose path backend name errmsg = do
    found <- convExceptTToIO errh $
        readAndCheckArtifactPath False errh flags be_verbose path backend name
    maybe (bsError errh [errmsg]) return found

checkABin :: Maybe Backend -> String -> ABin -> Either [EMsg] ABin
checkABin backend filename abi =
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
            -- Foreign metadata is independent of the backend.
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

-- Tolerant counterpart of the reader checks, for deciding whether
-- existing artifacts can be used by the current compilation: they must
-- be readable, in the current format and BSC version, and elaborated
-- for a compatible backend.  A missing file returns False (not stale);
-- absence is the timestamp check's concern.  Used by the -u
-- recompilation check, where an unusable artifact (e.g. one left by
-- "-verilog -g" when compiling with -sim, or written by another BSC
-- version) must force re-elaboration rather than be trusted as an
-- up-to-date generated product.
isStaleABinFile :: Maybe Backend -> String -> IO Bool
isStaleABinFile be fname = check `CE.catch` handler
  where
    check
      | hasDotSuf bmodSuffix fname || hasDotSuf bschedSuffix fname = do
          let stem = dropSuf fname
          mb <- readBinaryFileMaybe (stem ++ "." ++ bmodSuffix)
          ms <- readBinaryFileMaybe (stem ++ "." ++ bschedSuffix)
          case (mb, ms) of
            (Just b, Just s) -> CE.evaluate $
                case (readBModFileMaybe b, readBSchedFileMaybe s) of
                  (Just (amod, hash), Just sched) ->
                    let scheduleBackend = case sched of
                          BSched {} -> bs_backend sched
                          BSchedError {} -> apkg_backend (amod_body amod)
                    in hash /= bs_module_hash sched ||
                       not (backendMatches be scheduleBackend)
                  _ -> True
            _ -> return True
      | otherwise = do
          mbytes <- readBinaryFileMaybe fname
          case mbytes of
            Nothing -> return False
            Just bytes -> CE.evaluate $
              case if hasDotSuf bdpiSuffix fname
                   then case readBDPIFileMaybe bytes of
                          Nothing -> Nothing
                          Just bdpi -> rnf bdpi `seq` Just (bdpiToABin bdpi)
                   else readABinFileMaybe bytes of
                Nothing -> True
                Just abin -> ab_version abin /= bscVersionStr True ||
                             not (backendMatches be (beOf abin))
    beOf (ABinMod mi _) = apkg_backend (abmi_apkg mi)
    beOf (ABinModSchedErr mi _) = apkg_backend (abmsei_apkg mi)
    beOf (ABinForeignFunc {}) = Nothing
    handler :: CE.SomeException -> IO Bool
    handler _ = return True

-- ===============
