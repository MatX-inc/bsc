-- | Materialize the first Buck2 backend without putting scheduler policy in
-- TestPlan.  The generated cell is a snapshot: tracked suite files, an installed
-- compiler, this planner executable, and the repository's canonical rules.
-- Regenerate it when any of those inputs or the host toolchain changes.
--
-- Deliberately conservative: every action declares the full suite snapshot.
-- This is not dependency discovery.  Only the implemented compilation vocabulary
-- is emitted; planning and execution gaps remain explicit in targets.json.
module Buck2 (EmitConfig(..), emitBuck2, renderBuildFile) where

import Control.Monad (forM, forM_, unless, when)
import Data.Char (isControl, ord)
import Data.List (isPrefixOf, nub, sort)
import Numeric (showHex)
import System.Directory
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process
import System.IO.Error (catchIOError, isDoesNotExistError)
import Lower (lowerPlan)
import Procedures (internalChecksFor)
import TestPlan

data EmitConfig = EmitConfig
  { emitSuite :: FilePath, emitInstallation :: FilePath, emitOutput :: FilePath }
  deriving (Eq, Show)

-- | Output must not exist. Git supplies the source inventory, but file contents
-- come from the working tree so reviewed, uncommitted source edits are included.
-- Untracked/generated test output is never mistaken for compiler input.
emitBuck2 :: EmitConfig -> TestPlan -> IO (Int, Int)
emitBuck2 config plan = do
  checked (validatePlan plan)
  suite <- canonicalizePath (emitSuite config)
  installation <- canonicalizePath (emitInstallation config)
  output <- canonicalizePath (emitOutput config)
  runner <- getExecutablePath >>= canonicalizePath
  exists <- doesPathExist output
  symbolic <- pathIsSymbolicLink output `catchMissing` pure False
  when (exists || symbolic) $ failure "output already exists; choose a fresh snapshot directory"
  when (within installation output) $ failure "output must not be inside the compiler installation"
  tracked <- splitNul <$> command "git" ["-C", suite, "ls-files", "-z", "--", "."]
  when (null tracked) $ failure "suite has no Git-tracked inputs"
  let candidates = [(test, executionGap (planConfig plan) test) | test <- plannedTests plan]
      supported = [test | (test, Nothing) <- candidates]
      gaps = [(test, reason) | (test, Just reason) <- candidates]
      selectedScripts = [script | script <- planScripts plan,
          scriptPath script `elem` map (identifierTest . testId) supported]
  when (null supported) $ failure "plan has no executable tests in this backend"
  -- Unsupported scripts can contain generated aliases or missing setup inputs.
  -- Only scripts whose tests we actually execute must be bound and revalidated.
  forM_ selectedScripts $ \script ->
    unless (scriptPath script `elem` tracked) $ failure
      ("planned script is not a tracked input: " ++ scriptPath script)
  sources <- forM selectedScripts $ \script -> do
    source <- readFile (suite </> scriptPath script)
    pure (scriptPath script, source)
  fresh <- checked (lowerPlan (planConfig plan) sources)
  unless (planScripts fresh == selectedScripts) $ failure "plan differs from current source scripts; regenerate the plan"
  repository <- trim <$> command "git" ["-C", suite, "rev-parse", "--show-toplevel"]
  when (within (repository </> "rules") output) $ failure "output must not be inside the rule sources"
  forM_ ["bin/core/bsc", "bin/core/dumpbo", "lib/Libraries"] $ \path -> do
    found <- doesPathExist (installation </> path)
    unless found $ failure ("incomplete compiler installation: " ++ path)
  -- Preflight all links before starting a potentially expensive copy. Absolute
  -- or escaping links cannot be carried into a relocatable input snapshot.
  forM_ tracked $ \path -> validateSource suite (suite </> path)
  host <- hostIdentity installation runner
  createDirectoryIfMissing True (takeDirectory output)
  createDirectory output
  forM_ tracked $ \path -> copyInput suite (suite </> path) (output </> "suite" </> path)
  copyTree installation installation (output </> "installation")
  forM_ ["bluespec", "bsctest"] $ \name -> do
    let source = repository </> "rules" </> name
    copyTree source source (output </> "rules" </> name)
  createDirectoryIfMissing True (output </> "tools")
  copyFileWithMetadata runner (output </> "tools/bsc-test-plan")
  writeFile (output </> "plan.json") (encodePlan plan)
  writeFile (output </> "host-identity.json") host
  writeFile (output </> "source-paths.txt") (unlines tracked)
  writeFile (output </> "targets.json") (targetManifest plan supported gaps)
  writeFile (output </> "targets.txt") (unlines ["//:" ++ targetName index | index <- [1 .. length supported]])
  writeFile (output </> "BUCK") (renderBuildFile supported)
  -- Written last: until here a partially copied directory is not a Buck cell.
  template <- readFile (repository </> "testsuite/buck2/buckconfig")
  writeFile (output </> ".buckconfig") template
  pure (length supported, length gaps)

executionGap :: PlanConfig -> Test -> Maybe String
executionGap config test = case internalChecksFor config (testKind test) of
  Left message -> Just message
  Right _
    | not (compilationDependencies (testCompilation (testKind test))) -> Just
        "compilation without -u may depend on prior artifacts; shared-state execution is not implemented"
    | otherwise -> Nothing

renderBuildFile :: [Test] -> String
renderBuildFile tests = unlines
  (["# Generated from the semantic plan. Edit the source plan, not this file."
   ,"load(\"//rules/bluespec:defs.bzl\", \"bluespec_toolchain\")"
   ,"load(\"//rules/bsctest:defs.bzl\", \"bsc_test\")", ""
   ,"bluespec_toolchain(name = \"bsc\", installation = \"installation\", host_identity = \"host-identity.json\")", ""] ++
   concat [ ["bsc_test(", "    name = " ++ quote (targetName index) ++ ","
            ,"    plan = \"plan.json\",", "    identifier = " ++ quote (renderIdentifier (testId test)) ++ ","
            ,"    suite = \"suite\",", "    toolchain = \":bsc\","
            ,"    runner = \"tools/bsc-test-plan\",", ")", ""]
          | (index, test) <- zip [1..] tests])

targetName :: Int -> String
targetName index = "test_" ++ replicate (max 0 (6 - length digits)) '0' ++ digits
  where digits = show index

targetManifest :: TestPlan -> [Test] -> [(Test, String)] -> String
targetManifest plan tests gaps = "{\n  \"schema\":\"bsc-buck2-targets\",\n  \"version\":1,\n" ++
  "  \"planned\":" ++ show planned ++ ",\n  \"unsupported\":" ++ show unsupported ++
  ",\n  \"unresolved\":" ++ show unresolved ++ ",\n  \"targets\":[" ++
  comma ["{\"target\":" ++ quote ("//:" ++ targetName index) ++ ",\"id\":" ++ quote (renderIdentifier (testId test)) ++ "}"
        | (index, test) <- zip [1..] tests] ++ "],\n  \"execution_gaps\":[" ++
  comma ["{\"id\":" ++ quote (renderIdentifier (testId test)) ++ ",\"reason\":" ++ quote reason ++ "}"
        | (test, reason) <- gaps] ++ "]\n}\n"
  where (planned, unsupported, unresolved) = planCounts plan

-- The installation is a declared input, while these bytes describe external
-- Linux runtime dependencies. This initial backend is local-only and never
-- uploads action results. Host identity is captured at emission time, not a
-- promise that arbitrary later changes to the host are detected by Buck2.
hostIdentity :: FilePath -> FilePath -> IO String
hostIdentity installation runner = do
  kernel <- command "uname" ["-srm"]
  let binaries = [runner, installation </> "bin/core/bsc", installation </> "bin/core/dumpbo",
                  "/bin/bash", "/usr/bin/env"]
  dependencyLists <- forM binaries $ \binary -> do
    (status, out, err) <- readCreateProcessWithExitCode
      (proc "ldd" [binary]) { env = Just
        [("PATH", "/usr/bin:/bin"), ("LC_ALL", "C"),
         ("LD_LIBRARY_PATH", installation </> "lib/SAT")] } ""
    unless (status == ExitSuccess) $ failure ("cannot identify runtime dependencies: " ++ err ++ out)
    when ("not found" `contains` out) $ failure ("missing runtime dependency: " ++ out)
    pure [token | token <- words out, "/" `isPrefixOf` token]
  let hostFiles = sort . nub $ ["/bin/bash", "/usr/bin/env"] ++
        [path | path <- concat dependencyLists, not (within installation path)]
  hashes <- forM hostFiles $ \path -> do
    digest <- command "sha256sum" [path]
    case words digest of
      value:_ -> pure ("{\"path\":" ++ quote path ++ ",\"sha256\":" ++ quote value ++ "}")
      _ -> failure "sha256sum produced no digest"
  pure ("{\"schema\":\"bsc-local-host\",\"version\":1,\"kernel\":" ++ quote (trim kernel) ++
        ",\"path\":\"/usr/bin:/bin\",\"locale\":\"C\",\"files\":[" ++ comma hashes ++ "]}\n")

validateSource :: FilePath -> FilePath -> IO ()
validateSource root path = do
  resolved <- canonicalizePath path
  unless (within root resolved) $ failure ("source link escapes snapshot root: " ++ makeRelative root path)
  found <- doesPathExist path
  unless found $ failure ("missing tracked input: " ++ makeRelative root path)
  symbolic <- pathIsSymbolicLink path
  when symbolic $ do
    target <- getSymbolicLinkTarget path
    when (isAbsolute target) $ failure ("absolute source link is not relocatable: " ++ makeRelative root path)

copyInput :: FilePath -> FilePath -> FilePath -> IO ()
copyInput root source destination = do
  validateSource root source
  createDirectoryIfMissing True (takeDirectory destination)
  symbolic <- pathIsSymbolicLink source
  if symbolic then do
    target <- getSymbolicLinkTarget source
    directory <- doesDirectoryExist source
    (if directory then createDirectoryLink else createFileLink) target destination
  else copyFileWithMetadata source destination

copyTree :: FilePath -> FilePath -> FilePath -> IO ()
copyTree root source destination = do
  validateSource root source
  symbolic <- pathIsSymbolicLink source
  directory <- doesDirectoryExist source
  if directory && not symbolic then do
    createDirectoryIfMissing True destination
    names <- sort <$> listDirectory source
    forM_ names $ \name -> copyTree root (source </> name) (destination </> name)
  else copyInput root source destination

within :: FilePath -> FilePath -> Bool
within root path = let relative = makeRelative root path
  in not (isAbsolute relative) && ".." `notElem` splitDirectories relative

command :: FilePath -> [String] -> IO String
command program arguments = do
  (status, out, err) <- readProcessWithExitCode program arguments ""
  unless (status == ExitSuccess) $ failure (program ++ " failed: " ++ err)
  pure out

checked :: Either String a -> IO a
checked = either failure pure

failure :: String -> IO a
failure = ioError . userError . ("emit-buck2: " ++)

splitNul :: String -> [String]
splitNul "" = []
splitNul input = let (first, rest) = break (== '\0') input
  in first : case rest of [] -> []; _:more -> splitNul more

trim :: String -> String
trim = reverse . dropWhile (`elem` "\r\n") . reverse

contains :: String -> String -> Bool
contains needle haystack = any (needle `isPrefixOf`) (tails haystack)
  where tails [] = [[]]; tails xs@(_:rest) = xs : tails rest

comma :: [String] -> String
comma [] = ""
comma (x:xs) = x ++ concatMap ("," ++) xs

quote :: String -> String
quote value = '"' : concatMap escape value ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c | isControl c = let digits = showHex (ord c) ""
                            in "\\u" ++ replicate (4 - length digits) '0' ++ digits
             | otherwise = [c]

catchMissing :: IO a -> IO a -> IO a
catchMissing action fallback = catchIOError action $ \exception ->
  if isDoesNotExistError exception then fallback else ioError exception
