{-# LANGUAGE CPP #-}
module Depend(chkDeps, sourceDependencyPlan, parseFile, parseFilePlan, chkParse, doCPP, genDepend, genFileDepend,
              outlaw_sv_kws_as_classic_ids) where

import Data.Maybe(isJust)
import Data.List(nub)
import Control.Monad(when, unless, void)
import BuildPlan
import DependencyReport(DependencyCandidate(..))
import BinUtil(withBinaryDependencyCache, readBinaryDependenciesPlan, readPackageDependenciesPlan)
import System.Process(system)
import System.Exit(ExitCode(..))
import System.Directory(getModificationTime, getCurrentDirectory)
import System.Time -- XXX: in old-time package
import System.IO.Error(ioeGetErrorType)
import GHC.IO.Exception(IOErrorType(..))
import Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import qualified Control.Exception as CE
import qualified Data.ByteString as BS
import qualified Data.Map as DM
import qualified Data.Set as Set

import TmpNam(tmpNam, localTmpNam)
import SCC(tsort)
import Flags
import Backend
import Pragma(Pragma(..),PProp(..))
import Position(noPosition, filePosition)
import Error(internalError, EMsg, ErrMsg(..), ErrorHandle, bsError,
             exitFailWith, bsWarning, WMsg)
import PFPrint
import FStringCompat
import Lex
import Parse
import FileNameUtil(hasDotSuf, dropSuf, baseName, dirName,
                    bscSrcSuffix, bsvSrcSuffix, binSuffix,
                    mkAName, mkVName, mkVPIHName, mkVPICName,
                    createEncodedFullFilePath)
import FileIOUtil(readFilesPath, readBinFilePath, readFileCatch, writeFileCatch,
                  removeFileCatch)
import Id
import PreIds(idPrelude, idPreludeBSV)
import Parser.Classic(pPackage, errSyntax, classicWarnings)
import Parser.BSV(bsvParseStringPlan)
import CSyntax
import GenFuncWrap(makeGenFuncId)
import IOUtil(getEnvDef, progArgs)
import TopUtils
--import Debug.Trace

outlaw_sv_kws_as_classic_ids :: Bool
outlaw_sv_kws_as_classic_ids = "-outlaw-sv-kws-as-classic-ids" `elem` progArgs

type PkgName = Id
type ModName = Id
type ForeignName = Id

type MClockTime = Maybe ClockTime

-- Compilation status for a package
data CompileStatus = Binary                      -- .bo file, no recompilation needed
                   | UpToDate CPackage [WMsg]    -- parsed source, dependencies up to date
                   | Recompile CPackage [WMsg]   -- parsed source, needs recompilation
                   deriving (Show)

data PkgInfo = PkgInfo {
        pkgName :: PkgName,
        fileName :: FilePath,
        srcMod :: MClockTime,
        lastMod :: MClockTime,
        imports :: [PkgName],
        includes :: [FilePath],
        gens :: [ModName],
        foreigns :: [ForeignName],
        compileStatus :: CompileStatus
        }
    deriving (Show)

getModificationTime' :: FilePath -> IO ClockTime
getModificationTime' file =
  do utcTime <- getModificationTime file
     let s = (floor . utcTimeToPOSIXSeconds) utcTime
     return (TOD s 0)

-- returns a list of Bluespec source files which need recompiling.
-- (This used to also return a list of all generated files which would
-- result from codegen, so that a later stage could link them.  But this
-- feature is no longer supported.)
chkDeps :: ErrorHandle -> Flags -> String -> IO [(FilePath, CPackage, [WMsg])]
chkDeps errh flags name = executeResultPlan $ do
    graph <- gatherPackagesPlan errh flags False name
    performResult (fmap (uncurry (schedulePackages errh flags)) graph)

-- The invocation runs one read/choice plan. Its result carries the actual
-- compilation jobs during execution; discovery never fabricates a selected
-- graph or the jobs that would be produced from one.
-- Direct compilation retains the parser's end time for the next phase;
-- update mode starts a fresh phase clock when each scheduled job is compiled.
sourceDependencyPlan :: ErrorHandle -> Flags -> FilePath ->
                        BuildPlan (BuildResult
                            (Maybe TimeInfo, [(FilePath, CPackage, [WMsg])]))
sourceDependencyPlan errh flags name = do
    note "Requirements owned by an alternative source or object apply when that artifact is used."
    note "Missing candidates describe absence dependencies, not files the caller must manufacture."
    note "Potential outputs are not exhaustive and are not guaranteed after a compilation error."
    _ <- requireFiles name "compilation-source" "required" [("source", name)]
      ["An existing object cannot substitute for the explicit source argument."]
    sourceOutputs flags name []
    when (preprocessOnly flags) $
      incomplete "Preprocessing-only (-E) termination is not simulated: source is parsed conservatively."
    when (isJust (kill flags)) $
      incomplete "Stage termination (-KILL) is not simulated: source is parsed conservatively."
    if updCheck flags
      then do
        graph <- gatherPackagesPlan errh flags False name
        jobs <- performResult (fmap (uncurry (schedulePackages errh flags)) graph)
        return (fmap ((,) Nothing) jobs)
      else withBinaryDependencyCache $ \binaryCache -> do
        (pkg, parseTime, warns) <- parseFilePlan errh flags False name
        declareInputs $ do
          let gflags = [mkId noPosition (mkFString n) | n <- genName flags]
          pi <- getInfo errh flags gflags name pkg warns
          readPackageDependenciesPlan binaryCache errh flags name (imports pi)
        return (pure (Just parseTime, [(name, pkg, warns)]))

sourceOutputs :: Flags -> FilePath -> [FilePath] -> BuildPlan ()
sourceOutputs flags name generated =
    unless (preprocessOnly flags || isJust (kill flags)) $
      outputs (putInDir (bdir flags) name binSuffix : generated)

type PackageGraph = ([EMsg], DM.Map PkgName PkgInfo)
type PackageVisit = (Set.Set PkgName, String, PkgName)

-- Execution threads the selected graph through the breadth-first queue.
-- Discovery carries each real alternative's state to its descendants while
-- keeping independent siblings separate; there is no union of package bodies.
-- Its selected map therefore contains only the root and current ancestors,
-- already covered by the cycle guard. Visited checks are execution bookkeeping,
-- not dependency alternatives; separate paths still inspect their own choices.
gatherPackagesPlan :: ErrorHandle -> Flags -> Bool -> FilePath ->
                      BuildPlan (BuildResult PackageGraph)
gatherPackagesPlan errh flags fatalRoot name = withBinaryDependencyCache $ \binaryCache -> do
    let gflags = [mkId noPosition (mkFString n) | n <- genName flags]
    (pkg, _, warns) <- parseFilePlan errh flags fatalRoot name
    root <- getInfo errh flags gflags name pkg warns
    let initial = ([], DM.singleton (pkgName root) root)
        visit :: PackageGraph -> PackageVisit -> BuildPlan (Either () (PackageGraph, [PackageVisit]))
        visit state@(errors, selected) (ancestors, owner, n)
          | Set.member n ancestors = do
              incomplete (owner ++ ": circular source import involving " ++ getIdString n)
              return (Right (state, []))
          | DM.member n selected = return (Right (state, []))
          | otherwise = do
              epi <- getPkgInfo errh flags owner n
              case epi of
                Left err -> do
                  incomplete (owner ++ ": package " ++ getIdString n ++
                    " has no available candidate; transitive dependencies are unknown")
                  return (Right ((err : errors, selected), []))
                Right pi -> do
                  let state' = (errors, DM.insert n pi selected)
                  case compileStatus pi of
                    Binary -> do
                      readBinaryDependenciesPlan binaryCache errh flags (fileName pi)
                      return (Right (state', []))
                    _ -> return (Right (state',
                      [(Set.insert n ancestors, fileName pi, i) | i <- imports pi]))
    -- Preserve transClose's source-loading order: finish the pending queue
    -- before reading any newly discovered imports. Discovery inspects each
    -- branch's child jobs independently without constructing a joined graph.
    graph <- traverseState BreadthFirst visit initial
      [(Set.singleton (pkgName root), name, i) | i <- imports root]
    return (fmap (either (const (internalError "gatherPackagesPlan: unexpected traversal error")) id) graph)

schedulePackages :: ErrorHandle -> Flags -> [EMsg] -> DM.Map PkgName PkgInfo ->
                    IO [(FilePath, CPackage, [WMsg])]
schedulePackages errh flags errs packages = do
    when (not (null errs)) $ bsError errh errs
    let pis = DM.elems packages
    case tsort [(pkgName pi, imports pi) | pi <- pis] of
      Left (firstImport:rest) ->
        bsError errh [(getPosition firstImport,
          ECircularImports (map ppReadable (firstImport:rest)))]
      Left [] -> internalError "Depend.schedulePackages: tsort empty cycle"
      Right names -> do
        let lookupPkg n = case DM.lookup n packages of
              Just pi -> pi
              Nothing -> internalError "Depend.schedulePackages: lookupPkg"
        checked <- chkUpd flags DM.empty [] (map lookupPkg names)
        return (reverse [(fileName pi, pkg, warns) |
          pi@PkgInfo { compileStatus = Recompile pkg warns } <- checked])

-- Resolve the same source-first package choice for both interpretations.
-- Execution keeps its normal lookup and diagnostics. Discovery visits the
-- available alternatives, including shadowed objects, before opening any of
-- them; the distribution cutoff can therefore prune an entire package branch.
getPkgInfo :: ErrorHandle -> Flags -> String -> PkgName -> BuildPlan (Either EMsg PkgInfo)
getPkgInfo errh flags owner pname = do
    let name = getIdString pname
        paths ext = [dir ++ "/" ++ name ++ "." ++ ext | dir <- ifcPath flags]
        sourcePaths = paths bsvSrcSuffix ++ paths bscSrcSuffix
        objectPaths = paths binSuffix
        missing = (getIdPosition pname, EMissingPackage (pfpString pname))
    candidates <- requireFiles owner ("package-import:" ++ name) "source-or-object"
      ([("source", p) | p <- sourcePaths] ++ [("object", p) | p <- objectPaths])
      ["-u looks for source (.bsv before .bs, in search-path order), parses available source and may regenerate its object.",
       "An existing object is reusable only under the compiler's normal freshness and compatibility checks.",
       "When no source is found, a compatible object is required."]
    let inspect (kind, path) = do
          external <- inspectDependency path
          unless external $ noAlternative ("Distribution package " ++ path)
          if kind == "source" then do
            sourceOutputs flags path []
            (pkg, _, warns) <- parseFilePlan errh flags True path
            Right <$> getInfo errh flags [] path pkg warns
          else do
            t <- observe ("object timestamp " ++ path) $ getModTime path
            return $ Right $ PkgInfo pname path Nothing t [] [] [] [] Binary
        -- Keep the ordinary source-first lookup, including its diagnostics
        -- and completed file reads, out of speculative dependency discovery.
        selected = do
          selectedSource <- observe ("resolve source package " ++ name) $
            readFilesPath errh noPosition False [name ++ "." ++ bsvSrcSuffix, name ++ "." ++ bscSrcSuffix] (ifcPath flags)
          actual <- case selectedSource of
            Just (_, path) -> return (Just ("source", path))
            Nothing -> observe ("resolve object package " ++ name) $ do
              object <- readBinFilePath errh noPosition False (name ++ "." ++ binSuffix) (ifcPath flags)
              case object of
                Nothing -> return Nothing
                Just (bytes, path) -> do
                  -- Complete the original lookup's lazy read before loading
                  -- another package, preserving its handle lifetime.
                  _ <- CE.evaluate (BS.length bytes)
                  return (Just ("object", path))
          maybe (return (Left missing)) inspect actual
        available = [(candidateKind c, candidatePath c) |
                     c <- candidates, candidateExists c]
        alternatives = if null available then [selected] else map inspect available
    searchAlternatives (owner ++ ": source or object for " ++ name) selected alternatives

-- Extract PkgInfo from a parsed CPackage
getInfo :: ErrorHandle -> Flags -> [ModName] -> FilePath -> CPackage -> [WMsg] -> BuildPlan PkgInfo
getInfo errh flags gflags fname pkg@(CPackage i _ imps _ _ defs incs) warns = do
    -- the mod time of the source file
    tbs <- observe ("source timestamp " ++ fname) $ getModTime fname

    -- function to change fname's path to a new directory
    -- (like TopUtils::putInDir)
    let mkdname dir suf = dir ++ "/" ++ baseName (dropSuf fname) ++ "." ++ suf

    -- find the mod time of the bo file (either in same dir or in the bdir)
    when (updCheck flags) $ do
      _ <- requireFiles fname "freshness-object" "optional"
        [("object", p) | p <- nub [dropSuf fname ++ "." ++ binSuffix, putInDir (bdir flags) fname binSuffix]]
        ["Under -u, existing objects and their timestamps influence whether this source is recompiled.",
         "The -bdir object is preferred for freshness when present; otherwise the same-directory object is checked."]
      return ()
    tbo_samedir <- observe ("object timestamp " ++ fname) $ getModTime (dropSuf fname ++ "." ++ binSuffix)
    tbo_bdir <- case (bdir flags) of
                    Nothing -> return Nothing
                    Just dir -> observe ("object timestamp " ++ mkdname dir binSuffix) $ getModTime (mkdname dir binSuffix)
    let tbo = if (isJust tbo_bdir) then tbo_bdir else tbo_samedir

    -- include the prelude to avoid failures when predule was updated.
    let prelude
           | not (usePrelude flags) = []
           | i == idPrelude = []
           | i == idPreludeBSV = [idPrelude]
           | otherwise = [idPrelude, idPreludeBSV]
    let status = if tbo < tbs then Recompile pkg warns else UpToDate pkg warns
    let pi = PkgInfo {
                      pkgName = i,
                      fileName = fname,
                      srcMod = tbs,
                      lastMod = tbs `max` tbo,
                      imports = [ i | CImpId _ i <- imps] ++ prelude,
                      includes = [i |  CInclude i <- incs],
                      gens = [ i | CPragma (Pproperties i pps) <- defs,
                                   PPverilog `elem` pps ] ++
                             [ i | CValueSign (CDef i _ _) <- defs,
                                   i `elem` gflags ] ++
                             [ makeGenFuncId i
                                 | CPragma (Pnoinline is) <- defs,
                                   i <- is ],
                      foreigns = [ i | CPragma (Pproperties _ pps) <- defs,
                                       (PPforeignImport i) <- pps ],
                      compileStatus = status }
    let generated = getGenFs flags pi
    sourceOutputs flags fname generated
    when (updCheck flags && not (null generated)) $ do
      _ <- requireFiles fname "freshness-generated-outputs" "optional"
        [("generated-output", p) | p <- generated]
        ["Missing or older generated outputs can cause -u to recompile the source."]
      return ()
    when (backend flags /= Nothing && not (null (gens pi))) $ do
      incomplete (fname ++ ": module elaboration can open dynamically named files, including absolute paths; relative names use " ++
        maybe "the invocation working directory" ("-fdir " ++) (fdir flags) ++
        ". Elaboration is not executed and these file accesses are not scanned by dependency discovery.")
      when (or [not (null options) | CPragma (Pproperties _ props) <- defs, PPoptions options <- props]) $
        incomplete (fname ++ ": module options pragmas may change input/output paths or generation behavior during elaboration.")
    return pi

-- This tries to return a list of all files that will be generated from
-- this file after codegen.
-- XXX This needs to be kept in sync with what the backend actually does!
getGenFs :: Flags -> PkgInfo -> [String]
getGenFs flags pi =
    let prefix = dirName (fileName pi) ++ "/"
        getName = getIdString . unQualId
        mkABinFileName i = mkAName (bdir flags) prefix (getName i)
        mkVerFileName i = mkVName (vdir flags) prefix (getName i)
        mkVPIFileNames i = [ mkVPIHName (vdir flags) prefix (getName i),
                             mkVPICName (vdir flags) prefix (getName i) ]
        -- files common to all backends
        foreign_abin_files = map mkABinFileName (foreigns pi)
    in case backend flags of
         Just Bluesim ->
            let mod_abin_files = map mkABinFileName (gens pi)
            in  foreign_abin_files ++ mod_abin_files
         Just Verilog ->
            let mod_ver_files = map mkVerFileName (gens pi)
                foreign_vpi_files = if (useDPI flags)
                                    then []
                                    else concatMap mkVPIFileNames (foreigns pi)
                mod_abin_files =
                    if (genABin flags)
                    then map mkABinFileName (gens pi)
                    else []
            in  foreign_abin_files ++ foreign_vpi_files ++
                mod_ver_files ++ mod_abin_files
         Nothing ->
            foreign_abin_files

-- Update the compile status in all the PkgInfo.
-- Transforms UpToDate -> Recompile when dependencies require it.
-- Uses both a Map (for efficient import lookups) and a List (to preserve dependency order).
chkUpd :: Flags -> DM.Map PkgName PkgInfo -> [PkgInfo] -> [PkgInfo] -> IO [PkgInfo]
chkUpd flags doneMap resultList [] = return resultList
chkUpd flags doneMap resultList (pi:pis) = do
    --putStrLn ("chkUpd " ++ show pi)
    case compileStatus pi of
      UpToDate pkg warns | not (isPreludePkg flags (fileName pi)) -> do
        -- Check if recompilation is needed
        let genfs = getGenFs flags pi
            incfs = includes pi
        --putStrLn (show genfs)
        genfsClks <- mapM getModTime genfs
        incfsClks <- mapM getModTime incfs
        let needGenUpd = any (srcMod pi >) genfsClks
            needIncUpd = any (lastMod pi <) incfsClks
        --putStrLn (show (fileName pi, genfs, map (srcMod pi >) genfsClks))
        --putStr (ppReadable (pkgName pi, imports pi, DM.keys doneMap))
            lastCompTime = minimum ((lastMod pi) : genfsClks)
        let stale = any (needsUpd lastCompTime doneMap) (imports pi) || needGenUpd || needIncUpd
            pi' = if stale then pi { compileStatus = Recompile pkg warns } else pi
        chkUpd flags (DM.insert (pkgName pi') pi' doneMap) (pi' : resultList) pis
      _ ->
        -- Binary, Recompile, or Prelude packages: no change needed
        chkUpd flags (DM.insert (pkgName pi) pi doneMap) (pi : resultList) pis

-- Is this an installed library?
isPreludePkg :: Flags -> FilePath -> Bool
isPreludePkg flags n =
    let sl = bluespecDir flags ++ "/Libraries/"
    in  take (length sl) n == sl

-- Check if out-of-date with respect to an imported module.
-- Recompilation is needed if the imported file will be
-- recompiled or if it has a later date stamp.
needsUpd :: MClockTime -> DM.Map PkgName PkgInfo -> PkgName -> Bool
needsUpd myMod piMap n =
    case DM.lookup n piMap of
    Nothing -> internalError ("needsUpd " ++ pfpString n)
    Just pi -> case compileStatus pi of
                 Recompile _ _ -> True
                 _ -> lastMod pi > myMod

getModTime :: String -> IO MClockTime
getModTime f = CE.catch (getModificationTime' f >>= return . Just) handler
  where handler :: CE.IOException -> IO MClockTime
        handler _ = return Nothing

-----

doCPP :: ErrorHandle -> Flags -> String -> IO String
doCPP errh flags name =
    if cpp flags
    then do
        tempName <- tmpNam
        topNameRoot <- localTmpNam
        let topName = topNameRoot ++ ".c"
            tmpNameOut = tempName ++ ".out"
        writeFileCatch errh topName ("#include \""++name++"\"\n")
        comp <- getEnvDef "CC" dfltCCompile

{- If the compiler specified in the CC environment variable has a
spaces in its name, for example CC="/usr/local/my c compiler/bin/cc",
then this will fail.  You need to properly quote the spaces.  As a
side effect, (and in fact, this was the reason why), if you include
flags in the CC variable, for example CC="cc -g", then it will work.
-}

        let backend_def = case backend flags of
                            Just Bluesim -> ["-D__GENC__"]
                            Just Verilog -> ["-D__GENVERILOG__"]
                            Nothing -> []
            cmd = unwords ([comp] ++ backend_def ++
                           ["-D__BSC__", "-E", "-nostdinc", "-traditional"] ++
                           -- the show function quotes things
                           (map show (cppFlags flags))
                           ++ [ topName, ">", tmpNameOut])
        when (verbose flags) $ putStrLn ("exec: " ++ cmd)
        rc <- system cmd
        case rc of
         ExitSuccess -> do
                file <- readFileCatch errh noPosition tmpNameOut
                removeFileCatch errh topName
                removeFileCatch errh tmpNameOut
                return file
         ExitFailure n -> do
                removeFileCatch errh topName
                removeFileCatch errh tmpNameOut
                exitFailWith errh n
    else readFileCatch errh noPosition name

-- Parse a file: run CPP, dump CPP output, parse, check name, dump CSyntax, stats
-- Returns CPackage, TimeInfo, and warnings for passing to compilation
-- If fatal_name_mismatch is True, package name mismatch causes bsError (aborts)
-- If False, it's just a bsWarning
parseFile :: ErrorHandle -> Flags -> Bool -> FilePath -> IO (CPackage, TimeInfo, [WMsg])
parseFile errh flags fatal_name_mismatch fname =
    executePlan $ parseFilePlan errh flags fatal_name_mismatch fname

parseFilePlan :: ErrorHandle -> Flags -> Bool -> FilePath -> BuildPlan (CPackage, TimeInfo, [WMsg])
parseFilePlan errh flags fatal_name_mismatch fname = do
    external <- inspectDependency fname
    unless external $ noAlternative ("Distribution source " ++ fname)
    -- Installed include text can still be needed while preprocessing an
    -- external source. Do not skip that text: it can determine imports and
    -- active branches. Its reported file requirements are pruned centrally.
    let isClassic = hasDotSuf bscSrcSuffix fname

    t <- observe "parser clock" getNow
    let dumpnames = (Just (baseName (dropSuf fname)), Nothing, Nothing)

    -- parseSrc needs encoded path for position tracking
    pwd <- observe "working directory" getCurrentDirectory
    let fname_encoded = createEncodedFullFilePath fname pwd

    when (hasDotSuf bsvSrcSuffix fname && vpp flags) $ do
      _ <- requireFiles fname "include-search" "search"
        [("include-search-directory", path) | path <- ifcPath flags]
        ["Recursive directory snapshots conservatively cover include lookup alternatives, including currently absent files.",
         "Only the compiler's active preprocessing branches are parsed; changed source or defines require a new query."]
      return ()

    perform $ start flags DFcpp
    file <- if cpp flags
      then produce (fname ++ ": external C preprocessing is not executed by dependency discovery") $
             doCPP errh flags fname_encoded
      else observe ("read source " ++ fname) $ doCPP errh flags fname_encoded
    perform $ void $ dumpStr errh flags t DFcpp dumpnames file

    -- parseSrc handles its own dump stages (DFparsed, DFvpp, etc.)
    (pkg@(CPackage i _ _ _ _ _ _), t', warns) <- parseSrcPlan isClassic errh flags fname_encoded file

    -- Check for package name mismatch
    let reportMismatch = if fatal_name_mismatch then bsError else bsWarning
    -- Use getIdString rather than pfpString here: pfpString calls isClassic(),
    -- which reads a global IORef.  If it fires before compilePackage calls
    -- setSyntax, GHC can memoize the result as CLASSIC and corrupt the print
    -- mode for the entire subsequent compilation.  Package names are always
    -- simple unqualified identifiers, so getIdString is equivalent.
    observe ("check package name " ++ fname) $ when (getIdString i /= baseName (dropSuf fname)) $
         reportMismatch errh
             [(getPosition i, WFilePackageNameMismatch fname (getIdString i))]

    -- dump CSyntax
    perform $ when (showCSyntax flags) (putStrLnF (show pkg))
    -- dump stats
    perform $ stats flags DFparsed pkg

    return (pkg, t', warns)

-- Parsing reads source and included files. Dumps, progress output and stage
-- termination are separate execution effects; dependency discovery runs the
-- same parser and retains its errors as unresolved branches.
parseSrcPlan :: Bool -> ErrorHandle -> Flags -> String -> String ->
                BuildPlan (CPackage, TimeInfo, [WMsg])
parseSrcPlan True errh flags filename inp = do
  t <- observe "parser clock" getNow
  let dumpnames = (Just (baseName (dropSuf filename)), Nothing, Nothing)
      lflags = LFlags { lf_is_stdlib = stdlibNames flags,
                       lf_allow_sv_kws = not outlaw_sv_kws_as_classic_ids }
  perform $ start flags DFparsed
  pkg <- observe ("parse " ++ filename) $
    CE.handleJust isEncErr handleErr $
      case chkParse pPackage (lexStart lflags (mkFString filename) inp) of
        Right p -> return p
        Left errs -> bsError errh errs
  perform $ void $ dump errh flags t DFparsed dumpnames pkg
  t' <- observe "parser clock" getNow
  return (pkg, t', classicWarnings pkg)
  where isEncErr :: CE.IOException -> Maybe CE.IOException
        isEncErr e | InvalidArgument <- ioeGetErrorType e = Just e
                   | otherwise = Nothing
        handleErr _ = bsError errh [(filePosition $ mkFString filename, ENotUTF8)]
parseSrcPlan False errh flags filename inp =
  bsvParseStringPlan errh flags filename (baseName $ dropSuf filename) inp

chkParse :: Parser [Token] a -> [Token] -> Either [EMsg] a
chkParse p ts =
    case parse p ts of
        Right ((m,_):_) -> Right m
        Left  (ss,ts)   -> Left [errSyntax (filter (not . null ) ss) ts]
        Right []        -> internalError "Depend.chkParse: Right []"

----
findPackages :: ErrorHandle -> Flags -> FilePath -> IO ([EMsg],[PkgInfo])
findPackages errh flags name = executeResultPlan $
  fmap (fmap (\(errs, packages) -> (errs, DM.elems packages))) $
    gatherPackagesPlan errh flags True name

-- generate the file name dependencies for filename
-- A package depends on its own source file name
-- plus the imported packages (.bo)
-- plus the included files
genDepend :: ErrorHandle -> Flags -> FilePath ->
             IO ([EMsg],[(FilePath, [FilePath])])
genDepend errh flags name = do
  (errs,pis) <- findPackages errh flags name
  let pmap :: DM.Map PkgName PkgInfo
      pmap = DM.fromList [(pkgName pki, pki) | pki <- pis]
      lookupP p =
            case (DM.lookup p pmap) of
              Just pinfo -> [pinfo]
              Nothing    -> [] -- internalError $ "Depend:genDepend " ++ show p
      --
      -- bo name and location
      boName p  = putInDir (bdir flags) (fileName p) binSuffix
      -- bsv source file (if it exists)
      getSelf p | Binary <- compileStatus p = []
                | otherwise = [fileName p]
      --
      getImports p = -- package name .bo
          let fnp p | Binary <- compileStatus p = fileName p
                    | otherwise = boName p
          in map fnp (concatMap lookupP (imports p))
      --
      extr :: PkgInfo -> [(FilePath, [FilePath])]
      extr pki | Binary <- compileStatus pki = []
      extr pki = [(boName pki, getSelf pki ++ getImports pki ++ includes pki)]
  return (errs,concatMap extr pis)

genFileDepend :: ErrorHandle -> Flags -> FilePath -> IO ([EMsg],[FilePath])
genFileDepend errh flags name = do
    (errs,pis) <- findPackages errh flags name
    let extrFiles :: PkgInfo -> [FilePath]
        extrFiles p | Binary <- compileStatus p = []
                    | otherwise = fileName p : includes p
    return (errs, nub $ concatMap extrFiles pis)
