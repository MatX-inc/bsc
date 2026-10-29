-- Run with the existing bluehs runtime, built with the paired native compiler.
-- This packages external sources and scripts; it does not create a launcher,
-- compiler archive, package database, or another Haskell runtime.
module Main where
import Control.Monad (forM, forM_, unless, filterM)
import qualified Data.Map.Strict as M
import Data.List (isPrefixOf, isSuffixOf, nub, sort)
import qualified Data.Set as S
import Data.Char (isAlphaNum, isSpace)
import Data.Version (showVersion)
import System.Directory
import System.Environment (getArgs, getEnv, getExecutablePath, lookupEnv)
import System.FilePath
import System.Exit (die)
import System.Info (compilerVersion)
import System.Process (readProcess)
import ECO.Replay (FileIdentity(..), fileIdentity)
import Version (bscVersionStr)
import Text.Read (readMaybe)

data SDKManifest = SDKManifest
  { sdkSchema :: Int, sdkCompiler :: FileIdentity
  , sdkCompilerVersion :: String, sdkGHCVersion :: String
  , sdkRuntime :: FileIdentity, sdkEnvironment :: FileIdentity
  , sdkPackageMetadata :: [(String,String)], sdkRuntimeFiles :: [FileIdentity]
  , sdkSources :: [FileIdentity], sdkScripts :: [FileIdentity]
  } deriving (Read,Show)
need b m = unless b (die m)
readSDKList path = do
    entries <- fmap (map (unwords . words . takeWhile (/= '#')) . lines) (readFile path)
    let names = filter (not . null) entries
    need (all (all (\c -> isAlphaNum c || c `elem` "._-")) names)
         ("invalid module/script list: " ++ path)
    need (length names == S.size (S.fromList names)) ("duplicate list entry: " ++ path)
    pure names
-- Ask the existing runtime's ghc-pkg about the exact exposed bsc unit and its
-- dependency closure. No package registration or launcher is manufactured here.
-- GHC expands package-root variables in these fields before returning them.
fieldMap = M.fromList . reverse . foldl add [] . lines
  where add acc line | null line = acc
                     | isSpace (head line) = case acc of
                         (key,value):rest -> (key,value ++ " " ++ line):rest
                         [] -> []
                     | otherwise = case break (== ':') line of
                         (key,':':value) -> (key,value):acc
                         _ -> acc
pathWords text = case dropWhile isSpace text of
    [] -> []
    quoted@('"':_) -> case reads quoted of
        [(word,rest)] -> word : pathWords rest
        _ -> error "invalid quoted ghc-pkg field"
    rest -> let (word,more) = break isSpace rest in word : pathWords more
filesBelow directory = do
    exists <- doesDirectoryExist directory
    if not exists then pure [] else do
      names <- sort <$> listDirectory directory
      fmap concat $ forM names $ \name -> do
        let path = directory </> name
        directoryEntry <- doesDirectoryExist path
        if directoryEntry then filesBelow path else pure [path]
runtimeMetadata environment = do
    -- The launcher names its GHC; a compiled script's own path is not it.
    interpreter <- lookupEnv "BLUEHS_GHC" >>= maybe getExecutablePath pure >>= canonicalizePath
    let packageTool = takeDirectory interpreter </> "ghc-pkg"
    envText <- readFile environment
    let envLines = map words (lines envText)
        roots = [unit | ["package-id",unit] <- envLines, "bsc-" `isPrefixOf` unit]
        dbs = [normalise (takeDirectory environment </> path) |
                 line <- lines envText, Just path <- [stripDB line]]
        args = ["--global","--no-user-package-db"] ++ map ("--package-db=" ++) dbs
        describe unit = readProcess packageTool (args ++ ["describe","--unit-id",unit]) ""
        visit seen [] = pure (M.toAscList seen)
        visit seen (unit:rest) | M.member unit seen = visit seen rest
        visit seen (unit:rest) = do
          metadata <- describe unit
          let deps = words (M.findWithDefault "" "depends" (fieldMap metadata))
          visit (M.insert unit metadata seen) (deps ++ rest)
    need (length roots == 1) "bluehs environment must expose exactly one bsc package unit"
    metadata <- visit M.empty roots
    pure (interpreter,packageTool,metadata)
  where stripDB line = case splitAt 11 line of
          ("package-db ",path) -> Just (dropWhile isSpace path)
          _ -> Nothing
runtimeFiles metadata = do
    paths <- fmap concat $ forM metadata $ \(_,description) -> do
      let fields = fieldMap description
          get key = pathWords (M.findWithDefault "" key fields)
          libraries = get "hs-libraries"
          dirs = nub (get "library-dirs" ++ get "dynamic-library-dirs")
      interfaces <- concat <$> mapM filesBelow (get "import-dirs")
      archives <- fmap concat $ forM dirs $ \directory -> do
        exists <- doesDirectoryExist directory
        if not exists then pure [] else do
          names <- listDirectory directory
          pure [directory </> name | name <- names,
                any (\library -> ("lib" ++ library ++ ".") `isPrefixOf` name ||
                                  ("lib" ++ library ++ "-") `isPrefixOf` name) libraries]
      pure ([path | path <- interfaces, any (`isSuffixOf` path) [".hi",".dyn_hi"]] ++ archives)
    regular <- filterM doesFileExist paths
    mapM fileIdentity . sort . nub =<< mapM canonicalizePath regular
checkManifest path = do
    manifest <- readFile path >>= maybe (die "invalid SDK manifest") pure . readMaybe
    need (sdkSchema manifest == 3) "unsupported SDK manifest schema"
    currentEnvironment <- getEnv "GHC_ENVIRONMENT" >>= fileIdentity
    need (currentEnvironment == sdkEnvironment manifest &&
          bscVersionStr True == sdkCompilerVersion manifest &&
          showVersion compilerVersion == sdkGHCVersion manifest)
         "current bluehs runtime does not match this SDK"
    let identities = [sdkCompiler manifest,sdkRuntime manifest,sdkEnvironment manifest] ++
                     sdkRuntimeFiles manifest ++ sdkSources manifest ++ sdkScripts manifest
    forM_ identities $ \expected -> do
      actual <- fileIdentity (fiPath expected)
      need (actual == expected) ("SDK/runtime identity changed: " ++ fiPath expected)
    (_,_,metadata) <- runtimeMetadata (fiPath (sdkEnvironment manifest))
    need (metadata == sdkPackageMetadata manifest) "bluehs package registration changed"
    putStrLn "SDK compiler, runtime/library, environment and source identities match"
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
    compilerId <- fileIdentity bsc
    runtimeId <- fileIdentity bluehs
    environmentId <- fileIdentity environment
    (interpreter,packageTool,metadata) <- runtimeMetadata environment
    libraryIds <- runtimeFiles metadata
    toolIds <- mapM fileIdentity [interpreter,packageTool]
    need (not (null libraryIds)) "no compiled runtime libraries found"
    names <- readSDKList (repo </> "util/eco/bluehs.modules")
    scripts <- readSDKList (repo </> "util/eco/bluehs.scripts")
    exists <- doesPathExist out0
    need (not exists) "output exists; use a fresh versioned SDK directory"
    createDirectoryIfMissing True out0
    out <- canonicalizePath out0
    let copySet from to entries = do
          createDirectoryIfMissing True to
          forM entries $ \name -> do
            let source = from </> name; destination = to </> name
            before <- fileIdentity source
            copyFile source destination
            after <- fileIdentity source
            copied <- fileIdentity destination
            need (before == after && fiSHA256 before == fiSHA256 copied)
                 ("source changed while packaging: " ++ source)
            pure copied
    sourceIds <- copySet (repo </> "util/eco/lib") (out </> "lib") (map (<.> "hs") names)
    scriptIds <- copySet (repo </> "util/eco") (out </> "scripts") scripts
    forM_ ([compilerId,runtimeId,environmentId] ++ toolIds ++ libraryIds) $ \expected -> do
      actual <- fileIdentity (fiPath expected)
      need (actual == expected) ("runtime changed while packaging: " ++ fiPath expected)
    writeFile (out </> "build.manifest") (show (SDKManifest 3 compilerId
      (bscVersionStr True) (showVersion compilerVersion) runtimeId environmentId metadata (toolIds ++ libraryIds) sourceIds scriptIds) ++ "\n")
    putStrLn ("ECO sources ready: " ++ out ++ "; run bluehs -i" ++ (out </> "lib") ++ " SCRIPT [ARGS]")
