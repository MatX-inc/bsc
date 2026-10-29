-- Run with the existing bluehs runtime, built with the paired native compiler.
-- This copies the registered external sources and scripts beside a manifest
-- naming the runtime they were packaged against; it creates no launcher,
-- compiler archive, package database or Haskell runtime. The manifest is
-- provenance (the SDK tree and runtime are unchanged since packaging); the
-- soundness binding of every artifact is the native compiler version stamp.
module Main where
import Control.Monad (filterM, forM, forM_, unless)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isSuffixOf, nub, sort, (\\))
import qualified Data.Set as S
import Data.Version (showVersion)
import System.Directory
import System.Environment (getArgs, getEnv)
import System.FilePath
import System.Exit (die)
import System.Info (fullCompilerVersion)
import System.Process (readProcess)
import ECO.Replay (FileIdentity(..), fileIdentity)
import Version (bscVersionStr)
import Text.Read (readMaybe)

-- Runtime identities are absolute (the runtime is not part of the SDK);
-- sources and scripts are relative to the directory holding the manifest.
data SDKManifest = SDKManifest
  { sdkSchema :: Int, sdkCompiler :: FileIdentity
  , sdkCompilerVersion :: String, sdkGHCVersion :: String
  , sdkRuntime :: FileIdentity, sdkEnvironment :: FileIdentity
  , sdkLibraries :: [FileIdentity], sdkSources :: [FileIdentity], sdkScripts :: [FileIdentity]
  } deriving (Read,Show)
need b m = unless b (die m)
readSDKList path = do
    entries <- fmap (map (unwords . words . takeWhile (/= '#')) . lines) (readFile path)
    let names = filter (not . null) entries
    need (all (all (\c -> isAlphaNum c || c `elem` "._-")) names)
         ("invalid module/script list: " ++ path)
    need (length names == S.size (S.fromList names)) ("duplicate list entry: " ++ path)
    pure names
haskellFiles directory = sort . filter (".hs" `isSuffixOf`) <$> listDirectory directory
-- The compiled bsc library the environment exposes, from that unit's own
-- registration file; no dependency closure and no package tool.
bscLibraries environment = do
    envLines <- map words . lines <$> readFile environment
    unit <- case [unit | ["package-id",unit] <- envLines, "bsc-" `isPrefixOf` unit] of
      [unit] -> pure unit
      _ -> die "bluehs environment must expose exactly one bsc package unit"
    confs <- filterM doesFileExist
      [takeDirectory environment </> db </> unit <.> "conf" | ["package-db",db] <- envLines]
    conf <- case confs of
      [conf] -> pure conf
      _ -> die "bluehs environment must register the bsc unit in exactly one package database"
    fields <- fieldMap <$> readFile conf
    let get key = maybe [] words (lookup key fields)
        dirs = nub (get "library-dirs" ++ get "dynamic-library-dirs")
    files <- fmap concat $ forM dirs $ \directory -> do
      names <- listDirectory directory
      pure [directory </> name | name <- sort names, stem <- get "hs-libraries",
            ("lib" ++ stem) `isPrefixOf` name, any (`isSuffixOf` name) [".a",".so",".dylib"]]
    need (not (null files)) "no compiled bsc library found"
    mapM fileIdentity (nub files)
  where
    fieldMap = reverse . foldl add [] . lines
    add ((key,value):rest) line@(c:_) | isSpace c = (key,value ++ " " ++ line):rest
    add acc line = case break (== ':') line of
      (key,':':value) -> (key,value):acc
      _ -> acc
checkManifest path = do
    manifest <- readFile path >>= maybe (die "invalid SDK manifest") pure . readMaybe
    need (sdkSchema manifest == 4) "unsupported SDK manifest schema"
    sdk <- canonicalizePath (takeDirectory path)
    currentEnvironment <- getEnv "GHC_ENVIRONMENT" >>= fileIdentity
    need (currentEnvironment == sdkEnvironment manifest &&
          bscVersionStr True == sdkCompilerVersion manifest &&
          showVersion fullCompilerVersion == sdkGHCVersion manifest)
         "current bluehs runtime does not match this SDK"
    let runtime = [sdkCompiler manifest,sdkRuntime manifest,sdkEnvironment manifest] ++ sdkLibraries manifest
        packaged = sdkSources manifest ++ sdkScripts manifest
        contained p = isRelative p && ".." `notElem` splitDirectories p
    need (all (isAbsolute . fiPath) runtime && all (contained . fiPath) packaged)
         "SDK manifest paths are not in their recorded form"
    forM_ (runtime ++ [i {fiPath = sdk </> fiPath i} | i <- packaged]) $ \expected -> do
      actual <- fileIdentity (fiPath expected)
      need (fiSHA256 actual == fiSHA256 expected) ("SDK/runtime identity changed: " ++ fiPath expected)
    forM_ [("lib",sdkSources manifest),("scripts",sdkScripts manifest)] $ \(directory,entries) -> do
      present <- haskellFiles (sdk </> directory)
      need (present == sort [takeFileName (fiPath i) | i <- entries])
           ("SDK " ++ directory ++ " tree differs from the manifest")
    putStrLn "SDK compiler, runtime, environment, bsc library and source identities match"
main = do
    args <- getArgs
    case args of
      ["--check",manifest] -> checkManifest manifest
      [b,h,r,o] -> packageSDK b h r o
      _ -> die "usage: bluehs package-sdk.hs BSC BLUEHS REPO FRESH_OUTPUT | --check MANIFEST"
packageSDK bsc0 bluehs0 repo0 out0 = do
    bsc <- canonicalizePath bsc0; bluehs <- canonicalizePath bluehs0
    repo <- canonicalizePath repo0
    -- The actual bluehs launcher sets this to its matched compiled bsc package.
    environment <- getEnv "GHC_ENVIRONMENT" >>= canonicalizePath
    version <- readProcess bsc ["-v"] ""
    need (bscVersionStr True `elem` lines version)
         "bluehs library and bsc version stamps differ; rebuild the paired runtime"
    runtime <- mapM fileIdentity [bsc,bluehs,environment]
    libraries <- bscLibraries environment
    names <- readSDKList (repo </> "util/eco/bluehs.modules")
    scripts <- readSDKList (repo </> "util/eco/bluehs.scripts")
    -- The registered lists are the whole external surface: a module or script
    -- in the checkout that is not listed would run from a stale SDK unnoticed.
    forM_ [("util/eco/lib",map (<.> "hs") names),("util/eco",scripts)] $ \(directory,listed) -> do
      present <- haskellFiles (repo </> directory)
      need (present == sort listed) (directory ++ " and its registration list differ: " ++
            unwords ((present \\ listed) ++ (listed \\ present)))
    exists <- doesPathExist out0
    need (not exists) "output exists; use a fresh versioned SDK directory"
    createDirectoryIfMissing True out0
    out <- canonicalizePath out0
    let copySet from to entries = do
          createDirectoryIfMissing True (out </> to)
          forM entries $ \name -> do
            let source = from </> name; destination = out </> to </> name
            before <- fileIdentity source
            copyFile source destination
            after <- fileIdentity source
            copied <- fileIdentity destination
            need (before == after && fiSHA256 before == fiSHA256 copied)
                 ("source changed while packaging: " ++ source)
            pure copied {fiPath = to </> name}
    sourceIds <- copySet (repo </> "util/eco/lib") "lib" (map (<.> "hs") names)
    scriptIds <- copySet (repo </> "util/eco") "scripts" scripts
    forM_ (runtime ++ libraries) $ \expected -> do
      actual <- fileIdentity (fiPath expected)
      need (actual == expected) ("runtime changed while packaging: " ++ fiPath expected)
    let [compilerId,runtimeId,environmentId] = runtime
    writeFile (out </> "build.manifest") (show (SDKManifest 4 compilerId
      (bscVersionStr True) (showVersion fullCompilerVersion) runtimeId environmentId libraries sourceIds scriptIds) ++ "\n")
    putStrLn ("ECO sources ready: " ++ out ++ "; run bluehs -i -i" ++ (out </> "lib") ++ " SCRIPT [ARGS]")
