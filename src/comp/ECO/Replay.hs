{-# LANGUAGE ScopedTypeVariables #-}
-- Native scheduled-package replay.  The bootstrap half of this module uses
-- only ordinary Strings: it must run before a compiler FString can be forced.
module ECO.Replay
    ( ReplayBackend(..), ReplayManifest(..), FileIdentity(..)
    , captureReplay, prepareReplay, loadReplay
    , verifyReplayOutputs, checkReplayOutputPaths, outputPathsOverlap, inspectReplay
    , prepareReplayAdaptation, runReplayControl, finishReplayAdaptation
    , replayTargetRevision, replaySchema, fileIdentity, sameFileIdentity
    ) where

import Control.Exception(evaluate)
import Control.Monad(forM, forM_, unless, when)
import qualified Data.ByteString as BS
import Data.Char(isHexDigit, isSpace)
import Data.List(nub)
import System.Directory(canonicalizePath, copyFile, createDirectoryIfMissing,
                        doesFileExist, findExecutable, getCurrentDirectory, listDirectory,
                        renameFile, withCurrentDirectory)
import System.Environment(getExecutablePath, getEnvironment)
import System.FilePath((</>), takeFileName, takeExtension, dropExtension)
import System.Posix.Files(getFileStatus, deviceID, fileID)
import System.Process(readProcessWithExitCode, readCreateProcessWithExitCode,
                      proc, CreateProcess(..))
import System.Exit(ExitCode(..))
import Text.Read(readMaybe)

import ABin(ABin(..), ABinModInfo(..))
import ABinUtil(readAndCheckABin)
import ASyntax(APackage(..))
import Id(getIdBaseString)
import Backend(Backend(..))
import BinData(encode, decode)
import Error(ErrorHandle)
import Flags(Flags(..))
import GenABin() -- native Bin Flags instance, identical to the producer
import SpeedyString(InternSnapshot, snapshotInternState, restoreInternState)
import Version(bscVersionStr)

replayTargetRevision :: String
replayTargetRevision = "79e3ff1717fc1fc1447e6d4ee23d37b2cdb175ae"

replaySchema :: String
replaySchema = "bsc-eco-native-replay-1"

-- Supplied by the existing backend after it has written normal output.  Keep
-- the record lazy until capture; merely observing must not force compiler IR.
data ReplayBackend = ReplayBackend
    { rbPrefix :: String
    , rbModuleName :: String
    , rbHeader :: [String]
    , rbMethodConflict :: [String]
    , rbMethodBVI :: [String]
    , rbDependencies :: [FilePath]
    , rbOutputs :: [FilePath]
    } deriving (Read, Show)

data FileIdentity = FileIdentity
    { fiPath :: FilePath
    , fiSHA256 :: String
    } deriving (Eq, Read, Show)

data ReplayManifest = ReplayManifest
    { rmSchema :: String
    , rmTargetRevision :: String
    , rmWorkingDirectory :: FilePath
    , rmNativeVersion :: String
    , rmProducer :: FileIdentity
    , rmABin :: FileIdentity
    , rmFlags :: FileIdentity
    , rmIntern :: FileIdentity
    , rmBackend :: ReplayBackend
    , rmDependencies :: [FileIdentity]
    , rmFilters :: [(String, FileIdentity)]
    , rmOutputs :: [(FilePath, FileIdentity)]
    } deriving (Read, Show)

failReplay :: String -> IO a
failReplay = ioError . userError . ("ECO replay: " ++)

-- Invoke the platform's SHA-256 utility without a shell. Hashes cover exact
-- bytes, not timestamps; shasum is available on macOS without GNU coreutils.
fileIdentity :: FilePath -> IO FileIdentity
fileIdentity path = do
    absolute <- canonicalizePath path
    gnu <- findExecutable "sha256sum"
    (executable, arguments) <- case gnu of
        Just tool -> return (tool, ["--", absolute])
        Nothing -> do
            perl <- findExecutable "shasum"
            case perl of
                Just tool -> return (tool, ["-a", "256", "--", absolute])
                Nothing -> failReplay "SHA-256 hashing requires sha256sum or shasum on PATH"
    (status, output, err) <- readProcessWithExitCode executable arguments ""
    -- Both utilities prefix the complete output with a backslash when a
    -- filename needs escaping. It is not part of the digest.
    let unescaped = case output of '\\':rest -> rest; _ -> output
    case (status, words unescaped) of
        (ExitSuccess, digest:_) | length digest == 64 && all isHexDigit digest ->
            return (FileIdentity absolute digest)
        _ -> failReplay ("cannot hash " ++ absolute ++ ": " ++ err)

checkIdentity :: FileIdentity -> IO ()
checkIdentity expected = do
    actual <- fileIdentity (fiPath expected)
    same <- sameFileIdentity actual expected
    unless same $
        failReplay ("file changed since capture: " ++ fiPath expected)

-- A capture may move together with its dependencies while its original
-- directory remains a compatibility symlink. Compare both path spellings in
-- the current filesystem; the recorded digest remains immutable. This is an
-- identity comparison, not a replacement for hashing the actual file above.
sameFileIdentity :: FileIdentity -> FileIdentity -> IO Bool
sameFileIdentity a b = do
    ap <- canonicalizePath (fiPath a)
    bp <- canonicalizePath (fiPath b)
    return (ap == bp && fiSHA256 a == fiSHA256 b)

-- A shell command with arguments can read arbitrary unrecorded input files.
-- The MVP fails closed for those filters.  Bare executable filters are
-- recorded by resolved path and exact bytes, as well as their command text.
identifyFilter :: String -> IO (String, FileIdentity)
identifyFilter command = do
    unless (not (null command) && not (any isSpace command) &&
            not (any (`elem` "'\"`$;&|<>(){}*?[]\\") command)) $
        failReplay "capture requires each Verilog filter to be a bare executable path (no arguments or shell syntax)"
    found <- findExecutable command
    case found of
        Nothing -> failReplay ("cannot resolve Verilog filter: " ++ command)
        Just path -> do identity <- fileIdentity path
                        return (command, identity)

-- Call only after writeABin and the ordinary Verilog output have completed.
-- The .ba is copied unchanged.  Effective flags have a separate native sidecar
-- because the ordinary .ba may intentionally contain remapped path flags.
captureReplay :: FilePath -> FilePath -> Flags -> ReplayBackend -> IO FilePath
captureReplay directory nativePath flags info = do
    let moduleName = rbModuleName info
    when (synthesize flags) $ failReplay "Synthesize must be disabled"
    unless (backend flags == Just Verilog) $ failReplay "capture requires the Verilog backend"
    unless (takeFileName moduleName == moduleName && moduleName /= "." &&
            moduleName /= ".." && not (null moduleName)) $
        failReplay "invalid capture module name"
    createDirectoryIfMissing True directory
    absoluteDir <- canonicalizePath directory
    let stem = absoluteDir </> moduleName
        nativeCopy = stem ++ ".ba"
        flagsPath = stem ++ ".flags"
        internPath = stem ++ ".intern"
        manifestPath = stem ++ ".replay"
    original <- canonicalizePath nativePath
    unless (original == nativeCopy) $ copyFile original nativeCopy
    -- Preserve effective native flags rather than reconstructing command-line
    -- spelling. Invocation-only ECO controls are excluded by the native codec.
    BS.writeFile flagsPath (BS.pack (encode flags))
    deps <- mapM fileIdentity (nub (rbDependencies info))
    filters <- mapM identifyFilter (verilogFilter flags)
    outputs <- forM (zip [0 :: Int ..] (rbOutputs info)) $ \(n,path) -> do
        let savedPath = stem ++ ".output-" ++ show n ++ takeExtension path
        copyFile path savedPath
        identity <- fileIdentity savedPath
        return (path, identity)
    workingDirectory <- getCurrentDirectory >>= canonicalizePath
    producer <- getExecutablePath >>= fileIdentity
    baIdentity <- fileIdentity nativeCopy
    flagsIdentity <- fileIdentity flagsPath
    -- Snapshot only after all ordinary output and native serialization have
    -- been forced.  Nothing after this point interns new compiler strings.
    snapshot <- snapshotInternState
    writeFile internPath (show snapshot ++ "\n")
    internIdentity <- fileIdentity internPath
    let manifest = ReplayManifest
            { rmSchema = replaySchema
            , rmTargetRevision = replayTargetRevision
            , rmWorkingDirectory = workingDirectory
            , rmNativeVersion = bscVersionStr True
            , rmProducer = producer
            , rmABin = baIdentity
            , rmFlags = flagsIdentity
            , rmIntern = internIdentity
            , rmBackend = info
            , rmDependencies = deps
            , rmFilters = filters
            , rmOutputs = outputs
            }
        temporary = manifestPath ++ ".tmp"
    writeFile temporary (show manifest ++ "\n")
    renameFile temporary manifestPath
    return manifestPath

readTextRecord :: Read a => FilePath -> IO a
readTextRecord path = do
    contents <- readFile path
    -- Complete the read before parsing/using the record, so the handle cannot
    -- remain open across a restore or another file operation.
    _ <- evaluate (length contents)
    maybe (failReplay ("invalid sidecar: " ++ path)) return (readMaybe contents)

-- This is the ONLY entry allowed before startup restore.  In particular do
-- not decode flags, .ba files or even create an ErrorHandle before this call.
prepareReplay :: FilePath -> IO ReplayManifest
prepareReplay path = do
    manifest <- readTextRecord path
    validateReplayFiles manifest
    producer <- getExecutablePath >>= fileIdentity
    unless (fiSHA256 producer == fiSHA256 (rmProducer manifest)) $
        failReplay "compiler executable differs from capture producer"
    restoreReplayIntern manifest
    return manifest

validateReplayFiles :: ReplayManifest -> IO ()
validateReplayFiles manifest = do
    unless (rmSchema manifest == replaySchema &&
            rmTargetRevision manifest == replayTargetRevision &&
            rmNativeVersion manifest == bscVersionStr True) $
        failReplay "producer/native schema mismatch; regenerate the capture"
    workingDirectory <- getCurrentDirectory >>= canonicalizePath
    capturedDirectory <- canonicalizePath (rmWorkingDirectory manifest)
    unless (workingDirectory == capturedDirectory) $
        failReplay ("relative-path context changed; run replay from " ++ rmWorkingDirectory manifest)
    mapM_ checkIdentity (rmABin manifest : rmFlags manifest : rmIntern manifest :
                        rmDependencies manifest ++ map snd (rmFilters manifest))
    forM_ (rmFilters manifest) $ \(command,expected) -> do
        (_,actual) <- identifyFilter command
        same <- sameFileIdentity actual expected
        unless same $ failReplay ("filter resolution changed: " ++ command)
restoreReplayIntern :: ReplayManifest -> IO ()
restoreReplayIntern manifest = do
    snapshot <- readTextRecord (fiPath (rmIntern manifest)) :: IO InternSnapshot
    restoreInternState snapshot

-- Canonical paths detect symlinks; device/inode pairs also detect hard links.
-- Missing destination paths have no inode yet and are compared by path only.
outputPathsOverlap :: [FilePath] -> [FilePath] -> IO Bool
outputPathsOverlap inputs outputs = do
    protected <- mapM canonicalizePath inputs
    destinations <- mapM canonicalizePath outputs
    protectedKeys <- mapM fileKey protected
    destinationKeys <- mapM fileKey destinations
    return (any (`elem` protected) destinations ||
            any (\key -> key /= Nothing && key `elem` protectedKeys) destinationKeys)
  where
    fileKey path = do
        exists <- doesFileExist path
        if not exists then return Nothing else do
            status <- getFileStatus path
            return (Just (deviceID status, fileID status))

-- Every file named by the manifest, including the saved reference outputs.
replayFiles :: ReplayManifest -> [FileIdentity]
replayFiles manifest =
    [rmProducer manifest, rmABin manifest, rmFlags manifest, rmIntern manifest] ++
    rmDependencies manifest ++ map snd (rmFilters manifest) ++ map snd (rmOutputs manifest)

-- Output directories may be reused for different modules, but no output may
-- replace an input or companion belonging to this checked capture. Resolve
-- symlinks before the backend opens any output file for truncation.
checkReplayOutputPaths :: FilePath -> ReplayManifest -> [FilePath] -> IO ()
checkReplayOutputPaths manifestPath manifest outputs = do
    source <- canonicalizePath manifestPath
    let astStem = dropExtension source ++ ".eco-ast"
        companions = [astStem, astStem ++ ".bin", astStem ++ ".output.v"]
    overlap <- outputPathsOverlap (source : companions ++ map fiPath (replayFiles manifest)) outputs
    unless (not overlap) $
        failReplay "output would overwrite a bound capture or snapshot artifact"

-- Explicit, separately audited migration entry. The ordinary replay entry
-- above still requires the exact producer SHA. Migration first proves that
-- the original executable can replay its own immutable capture, then restores
-- that capture for the current backend's separate ordinary-byte control.
prepareReplayAdaptation :: FilePath -> FilePath -> IO (ReplayManifest, FileIdentity)
prepareReplayAdaptation path directory = do
    -- The controls and new capture use fixed names below this directory.
    -- Reject reuse before either producer can overwrite source artifacts or
    -- leave old control files masquerading as output from this invocation.
    createDirectoryIfMissing True directory
    contents <- listDirectory directory
    unless (null contents) $ failReplay "adaptation output directory must be empty"
    sourceIdentity <- fileIdentity path
    manifest <- readTextRecord path
    checkIdentity sourceIdentity
    validateReplayFiles manifest
    checkIdentity (rmProducer manifest)
    mapM_ (checkIdentity . snd) (rmOutputs manifest)
    runReplayControl (fiPath (rmProducer manifest)) path manifest (directory </> "original-control")
    -- The child cannot affect this process's interning. Revalidate source
    -- artifacts after the child, before restoring or decoding any native IR.
    validateReplayFiles manifest
    checkIdentity (rmProducer manifest)
    checkIdentity sourceIdentity
    restoreReplayIntern manifest
    return (manifest, sourceIdentity)

runReplayControl :: FilePath -> FilePath -> ReplayManifest -> FilePath -> IO ()
runReplayControl executable manifestPath manifest directory = do
    sourceIdentity <- fileIdentity manifestPath
    producerIdentity <- fileIdentity executable
    createDirectoryIfMissing True directory
    out <- canonicalizePath directory
    (source,controlInputs) <- relocatedReplayControl manifestPath manifest out
    environment <- getEnvironment
    let arguments = ["-eco-replay",source,"-eco-output",out]
        cleanEnvironment = filter (\(key,_) -> key /= "BSC_OPTIONS" && key /= "GHCRTS") environment
        command = (proc executable arguments) { cwd = Just (rmWorkingDirectory manifest),
                                                env = Just cleanEnvironment }
    (status,stdoutText,stderrText) <- readCreateProcessWithExitCode command ""
    writeFile (out </> "replay-control.log") (stdoutText ++ stderrText)
    writeFile (out </> "replay-control.command") (show (executable,arguments,rmWorkingDirectory manifest,status) ++ "\n")
    unless (status == ExitSuccess) $ failReplay ("ordinary replay control failed; see " ++ out </> "replay-control.log")
    verifyReplayOutputs manifest [out </> takeFileName original | (original,_) <- rmOutputs manifest]
    mapM_ checkIdentity controlInputs
    checkIdentity sourceIdentity
    checkIdentity producerIdentity
    validateReplayFiles manifest

-- Old frozen producers compare saved canonical path strings literally. Give
-- that unchanged executable a separate, checked control manifest when a tree
-- has moved behind compatibility symlinks. Only identity path spellings and
-- cwd change: native bytes, flags, dependency lookup names, filter commands,
-- producer SHA and expected output SHAs are unchanged. The original manifest
-- is never rewritten. This is not an alternate replay guard.
relocatedReplayControl :: FilePath -> ReplayManifest -> FilePath -> IO (FilePath,[FileIdentity])
relocatedReplayControl originalPath manifest directory = do
    let relocate identity = do
          checkIdentity identity
          path <- canonicalizePath (fiPath identity)
          return (identity {fiPath=path})
    cwd <- canonicalizePath (rmWorkingDirectory manifest)
    producer <- relocate (rmProducer manifest)
    abin <- relocate (rmABin manifest)
    flags <- relocate (rmFlags manifest)
    intern <- relocate (rmIntern manifest)
    dependencies <- mapM relocate (rmDependencies manifest)
    filters <- mapM (\(command,identity) -> do r<-relocate identity; return (command,r)) (rmFilters manifest)
    outputs <- mapM (\(name,identity) -> do r<-relocate identity; return (name,r)) (rmOutputs manifest)
    let relocated = manifest {rmWorkingDirectory=cwd,rmProducer=producer,
          rmABin=abin,rmFlags=flags,rmIntern=intern,rmDependencies=dependencies,
          rmFilters=filters,rmOutputs=outputs}
    source <- fileIdentity originalPath
    if show relocated == show manifest
      then return (fiPath source,[]) else do
      let controlPath = directory </> "relocated-input.replay"
          ledgerPath = directory </> "relocation-receipt"
      exists <- doesFileExist controlPath
      when exists $ failReplay "relocation control input already exists"
      writeFile controlPath (show relocated ++ "\n")
      control <- fileIdentity controlPath
      let mapping = zip (replayFiles manifest) (replayFiles relocated)
      -- Preserve the versioned receipt's optional companion-binding slots.
      writeFile ledgerPath (show ("bsc-eco-relocation-1",source,(Nothing :: Maybe FileIdentity),
          control,(Nothing :: Maybe FileIdentity),(rmWorkingDirectory manifest,cwd),mapping) ++ "\n")
      ledger <- fileIdentity ledgerPath
      return (controlPath,[control,ledger])

finishReplayAdaptation :: FileIdentity -> ReplayManifest -> FilePath -> FilePath -> IO ()
finishReplayAdaptation sourceIdentity source directory adaptedPath = do
    checkIdentity sourceIdentity
    validateReplayFiles source
    checkIdentity (rmProducer source)
    mapM_ (checkIdentity . snd) (rmOutputs source)
    adaptedIdentity <- fileIdentity adaptedPath
    producer <- getExecutablePath >>= fileIdentity
    originalOutputs <- mapM fileIdentity [directory </> "original-control" </> takeFileName original |
                                           (original,_) <- rmOutputs source]
    currentOutputs <- mapM fileIdentity [directory </> "current-control" </> takeFileName original |
                                          (original,_) <- rmOutputs source]
    verifyReplayOutputs source (map fiPath originalOutputs)
    verifyReplayOutputs source (map fiPath currentOutputs)
    let relocationPath = directory </> "original-control" </> "relocation-receipt"
    hasRelocation <- doesFileExist relocationPath
    relocation <- if hasRelocation then fmap Just (fileIdentity relocationPath) else return Nothing
    let receipt = ("bsc-eco-native-adapter-1", sourceIdentity, rmProducer source,
                   producer, adaptedIdentity, originalOutputs, currentOutputs, relocation,
                   "Both ordinary backend controls equal every captured RTL digest; native schema/version and dependencies checked; no frontend run.")
    writeFile (directory </> "adapter-receipt") (show receipt ++ "\n")

-- Read-only AST/package inspection is deliberately separate from native replay.
-- It does not restore interning or generate compiler names. Serialized FString
-- spellings from every inspected capture join the one live table, so multiple
-- old/new VPrograms can safely coexist in an external Haskell process.
inspectReplay :: ErrorHandle -> FilePath -> IO (ReplayManifest, ABinModInfo, Flags)
inspectReplay errh path = do
    source <- fileIdentity path
    manifest <- readTextRecord (fiPath source)
    withCurrentDirectory (rmWorkingDirectory manifest) $ do
        validateReplayFiles manifest
        checkIdentity (rmProducer manifest)
        mapM_ (checkIdentity . snd) (rmOutputs manifest)
        (info,flags) <- loadReplay errh manifest
        checkIdentity source
        return (manifest,info,flags)

-- Keep the normal native reader's producer-version and backend checks.  The
-- driver calls its existing genModuleVerilog with these values and the exact
-- captured header instead of regenerating a timestamped header.
loadReplay :: ErrorHandle -> ReplayManifest -> IO (ABinModInfo, Flags)
loadReplay errh manifest = do
    (_, abin) <- readAndCheckABin errh (Just Verilog) (fiPath (rmABin manifest))
    flagBytes <- BS.readFile (fiPath (rmFlags manifest))
    flags <- evaluate (decode flagBytes)
    when (synthesize flags || backend flags /= Just Verilog) $
        failReplay "captured configuration is outside the supported backend"
    unless (verilogFilter flags == map fst (rmFilters manifest)) $
        failReplay "filter commands disagree with captured effective configuration"
    -- A newly installed file earlier in the search path must not silently
    -- shadow the dependency whose digest was recorded.
    -- Canonicalization may change the basename (foo.ba -> foo-v1.ba).
    -- Resolve the name actually requested by the backend, not the canonical
    -- target's basename.  Capture records these two lists in the same order.
    let dependencyPaths = nub (rbDependencies (rmBackend manifest))
    unless (length dependencyPaths == length (rmDependencies manifest)) $
        failReplay "dependency lookup paths disagree with captured identities"
    forM_ (zip dependencyPaths (rmDependencies manifest)) $ \(lookupPath, dependency) -> do
        let name = takeFileName lookupPath
            resolve [] = failReplay ("dependency no longer resolves: " ++ name)
            resolve (directory:rest) = do
                let candidate = directory </> name
                exists <- doesFileExist candidate
                if exists then canonicalizePath candidate else resolve rest
        resolved <- resolve (ifcPath flags)
        captured <- canonicalizePath (fiPath dependency)
        unless (resolved == captured) $
            failReplay ("dependency search path changed resolution: " ++ name)
    case abin of
        ABinMod info _ -> do
            unless (getIdBaseString (apkg_name (abmi_apkg info)) ==
                    rbModuleName (rmBackend manifest)) $
                failReplay "native module name disagrees with manifest"
            return (info, flags)
        _ -> failReplay "native payload is not a successfully scheduled module"


-- Check ordinary replay before any matching/renaming observation rewrites RTL.
-- Output paths may change, but every generated byte must match the captured
-- compilation.  This also detects filters with unrecorded ambient inputs or
-- nondeterministic behavior, whose executable digest alone is insufficient.
verifyReplayOutputs :: ReplayManifest -> [FilePath] -> IO ()
verifyReplayOutputs manifest generated = do
    let expected = rmOutputs manifest
    unless (length generated == length expected) $
        failReplay "generated output count differs from capture"
    forM_ (zip expected generated) $ \((original, identity), path) -> do
        actual <- fileIdentity path
        unless (fiSHA256 actual == fiSHA256 identity) $
            failReplay ("generated RTL differs from captured output " ++ original ++
                        " (expected SHA-256 " ++ fiSHA256 identity ++
                        ", got " ++ fiSHA256 actual ++ ")")
