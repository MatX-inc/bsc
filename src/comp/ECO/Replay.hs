{-# LANGUAGE ScopedTypeVariables #-}
-- Native scheduled-package replay.  The bootstrap half of this module uses
-- only ordinary Strings: it must run before a compiler FString can be forced.
module ECO.Replay
    ( ReplayBackend(..), ReplayManifest(..), FileIdentity(..), AdapterReceipt(..)
    , captureReplay, prepareReplay, loadReplay
    , verifyReplayOutputs, checkReplayOutputPaths, outputPathsOverlap, inspectReplay
    , prepareReplayAdaptation, writeAdapterReceipt
    , replaySchema, adapterSchema, scrubbedEnvironment, fileIdentity, sameFileIdentity
    , producerIdentity, resolveProgram
    ) where

import Control.Exception(SomeException, evaluate, try)
import Control.Monad(filterM, forM, forM_, unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char(isHexDigit, isSpace)
import Data.List(isPrefixOf, nub)
import System.Directory(canonicalizePath, copyFile, createDirectoryIfMissing,
                        doesDirectoryExist, doesFileExist, findExecutable, getCurrentDirectory,
                        listDirectory, renameFile, withCurrentDirectory)
import System.Environment(getExecutablePath, getProgName)
import System.FilePath((</>), takeDirectory, takeFileName, takeExtension)
import System.IO(IOMode(..), withBinaryFile)
import System.Posix.Files(getFileStatus, deviceID, fileID)
import System.Process(readProcessWithExitCode)
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

replaySchema :: String
replaySchema = "bsc-eco-native-replay-2"

adapterSchema :: String
adapterSchema = "bsc-eco-native-adapter-2"

-- Environment variables no child compiler process may inherit. ECO options
-- come from argv only and a replay decodes no ordinary flags, so only the
-- GHC runtime options remain a hidden channel.
scrubbedEnvironment :: [String]
scrubbedEnvironment = ["GHCRTS"]

-- Supplied by the existing backend after it has written normal output.  Keep
-- the record lazy until capture; merely observing must not force compiler IR.
data ReplayBackend = ReplayBackend
    { rbModuleName :: String
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

-- Written by adaptation beside its recaptured capture. The original producer
-- is recorded by identity only; it is never executed.
data AdapterReceipt = AdapterReceipt
    { arSchema :: String
    , arSource :: FileIdentity
    , arOriginalProducer :: FileIdentity
    , arAdapterProducer :: FileIdentity
    , arAdapted :: FileIdentity
    , arCurrentOutputs :: [FileIdentity]
    , arNote :: String
    } deriving (Eq, Read, Show)

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
-- | The program that is running, seen through an explicit dynamic loader. A
-- release install launches bin/core/<tool> on lib/rt's ld-linux, so the
-- executable path is the loader's; the program is then the first word of the
-- command line after the loader that names an existing file called like the
-- program. Elsewhere it is the executable path itself.
runningProgram :: IO FilePath
runningProgram = do
    exe <- getExecutablePath >>= canonicalizePath
    if not (isLoader (takeFileName exe)) then return exe else do
        name <- getProgName
        raw <- try (BS.readFile "/proc/self/cmdline") :: IO (Either SomeException BS.ByteString)
        let args = either (const []) (filter (not . null) . map BC.unpack . BC.split '\0') raw
        found <- filterM doesFileExist [a | a <- drop 1 args, takeFileName a == name]
        case found of
            (p:_) -> canonicalizePath p
            [] -> return exe
  where isLoader n = any (`isPrefixOf` n) ["ld-linux", "ld.so", "ld64.so", "ld-musl"]

-- | The compiler's identity as the producer of a capture: the program itself,
-- never the launcher or loader that started it.
producerIdentity :: IO FileIdentity
producerIdentity = runningProgram >>= fileIdentity

-- | A configured compiler path seen the same way: a launcher script whose
-- bin/core/<name> sibling exists names that program, so its identity is the
-- one the compiler records for itself.
resolveProgram :: FilePath -> IO FilePath
resolveProgram path = do
    p <- canonicalizePath path
    let core = takeDirectory p </> "core" </> takeFileName p
    script <- withBinaryFile p ReadMode (\h -> (== BC.pack "#!") <$> BS.hGet h 2)
    coreExists <- doesFileExist core
    return (if script && coreExists then core else p)

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
-- Every fallible check precedes the first write, so a failed capture leaves
-- nothing behind and an existing capture stem is never overwritten.
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
        savedOutputs = [stem ++ ".output-" ++ show n ++ takeExtension path |
                            (n,path) <- zip [0 :: Int ..] (rbOutputs info)]
    original <- canonicalizePath nativePath
    existing <- filterM doesFileExist ([nativeCopy | original /= nativeCopy] ++
                                       flagsPath : internPath : manifestPath : savedOutputs)
    forM_ (take 1 existing) $ \path -> failReplay ("capture already exists: " ++ path)
    forM_ (original : rbOutputs info) $ \path -> do
        exists <- doesFileExist path
        unless exists $ failReplay ("capture input is missing: " ++ path)
    -- Hashing the producer first also proves the SHA-256 utility is usable.
    producer <- producerIdentity
    deps <- mapM fileIdentity (nub (rbDependencies info))
    filters <- mapM identifyFilter (verilogFilter flags)
    workingDirectory <- getCurrentDirectory >>= canonicalizePath
    unless (original == nativeCopy) $ copyFile original nativeCopy
    -- Preserve effective native flags rather than reconstructing command-line
    -- spelling. Invocation-only ECO controls are excluded by the native codec.
    BS.writeFile flagsPath (BS.pack (encode flags))
    outputs <- forM (zip (rbOutputs info) savedOutputs) $ \(path,savedPath) -> do
        copyFile path savedPath
        identity <- fileIdentity savedPath
        return (path, identity)
    baIdentity <- fileIdentity nativeCopy
    flagsIdentity <- fileIdentity flagsPath
    -- Snapshot only after all ordinary output and native serialization have
    -- been forced.  Nothing after this point interns new compiler strings.
    snapshot <- snapshotInternState
    writeFile internPath (show snapshot ++ "\n")
    internIdentity <- fileIdentity internPath
    let manifest = ReplayManifest
            { rmSchema = replaySchema
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

-- A manifest of another schema need not parse as this record at all.
readManifest :: FilePath -> IO ReplayManifest
readManifest path = do
    contents <- readFile path
    _ <- evaluate (length contents)
    case readMaybe contents of
        Just manifest -> return manifest
        Nothing | "ReplayManifest {rmSchema = \"bsc-eco-native-replay-" `isPrefixOf` contents ->
                    failReplay "producer/native schema mismatch; regenerate the capture"
                | otherwise -> failReplay ("invalid sidecar: " ++ path)

-- This is the ONLY entry allowed before startup restore.  In particular do
-- not decode flags, .ba files or even create an ErrorHandle before this call.
prepareReplay :: FilePath -> IO ReplayManifest
prepareReplay path = do
    manifest <- readManifest path
    validateReplayFiles manifest
    producer <- producerIdentity
    unless (fiSHA256 producer == fiSHA256 (rmProducer manifest)) $
        failReplay "compiler executable differs from capture producer"
    restoreReplayIntern manifest
    return manifest

validateReplayFiles :: ReplayManifest -> IO ()
validateReplayFiles manifest = do
    unless (rmSchema manifest == replaySchema &&
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
-- replace a file the manifest names or lie anywhere inside the capture
-- directory, which also holds the capture's companions. Resolve symlinks
-- before the backend opens any output file for truncation.
checkReplayOutputPaths :: FilePath -> ReplayManifest -> [FilePath] -> IO ()
checkReplayOutputPaths manifestPath manifest outputs = do
    source <- canonicalizePath manifestPath
    let captureDir = takeDirectory source
    siblings <- listDirectory captureDir
    destinations <- mapM canonicalizePath outputs
    overlap <- outputPathsOverlap (map (captureDir </>) siblings ++ map fiPath (replayFiles manifest)) outputs
    unless (not overlap && not (any ((captureDir ++ "/") `isPrefixOf`) destinations)) $
        failReplay "output would overwrite a bound capture or snapshot artifact"

-- Explicit, separately audited migration entry. The ordinary replay entry
-- above still requires the exact producer SHA. Adaptation validates the
-- capture, restores its table for the current backend's ordinary-byte control
-- and then recaptures; the original producer is never executed.
prepareReplayAdaptation :: FilePath -> FilePath -> IO (ReplayManifest, FileIdentity)
prepareReplayAdaptation path directory = do
    -- The control and new capture use fixed names below this directory.
    -- Reject reuse before anything can overwrite source artifacts or leave
    -- old control files masquerading as output from this invocation.
    exists <- doesDirectoryExist directory
    contents <- if exists then listDirectory directory else return []
    unless (null contents) $ failReplay "adaptation output directory must be empty"
    sourceIdentity <- fileIdentity path
    manifest <- readManifest (fiPath sourceIdentity)
    validateReplayFiles manifest
    mapM_ (checkIdentity . snd) (rmOutputs manifest)
    restoreReplayIntern manifest
    return (manifest, sourceIdentity)

-- Call after the current-control replay reproduced the captured bytes and the
-- recapture completed; the source capture must still be intact.
writeAdapterReceipt :: FileIdentity -> ReplayManifest -> FilePath -> FilePath -> IO ()
writeAdapterReceipt sourceIdentity source directory adaptedPath = do
    checkIdentity sourceIdentity
    validateReplayFiles source
    producer <- producerIdentity
    adaptedIdentity <- fileIdentity adaptedPath
    currentOutputs <- mapM fileIdentity [directory </> "current-control" </> takeFileName original |
                                          (original,_) <- rmOutputs source]
    verifyReplayOutputs source (map fiPath currentOutputs)
    let receipt = AdapterReceipt
            { arSchema = adapterSchema
            , arSource = sourceIdentity
            , arOriginalProducer = rmProducer source
            , arAdapterProducer = producer
            , arAdapted = adaptedIdentity
            , arCurrentOutputs = currentOutputs
            , arNote = "The current compiler's ordinary backend reproduced every captured RTL digest from the restored capture; native schema/version, dependencies and filters checked; no frontend run; the original producer was not executed."
            }
    writeFile (directory </> "adapter-receipt") (show receipt ++ "\n")

-- Read-only AST/package inspection is deliberately separate from native replay.
-- It does not restore interning or generate compiler names. Serialized FString
-- spellings from every inspected capture join the one live table, so multiple
-- old/new VPrograms can safely coexist in an external Haskell process.
inspectReplay :: ErrorHandle -> FilePath -> IO (ReplayManifest, ABinModInfo, Flags)
inspectReplay errh path = do
    source <- fileIdentity path
    manifest <- readManifest (fiPath source)
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
