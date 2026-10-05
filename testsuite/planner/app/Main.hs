-- | Command-line I/O for the testsuite migration tools.
--
-- The commands separate planning, correspondence, and legacy comparison:
--
-- * plan: discover/read scripts with Census, lower Tcl with Lower, and
--   serialize semantic tests and explicit unsupported/unresolved items.
--   explain reads that plan back and describes a test or issue.
-- * correlate: check a saved plan against opt-in legacy-harness observations
--   of supported invocations, using numbers plus source and argument checks.
-- * import-sum/compare: use Verdict to preserve and compare legacy DejaGNU
--   observations while the new planner is being developed.
-- * emit-buck2/execute: snapshot explicit inputs and execute one supported
--   semantic test in a private workspace. Ordinary test failures are data;
--   the result checker determines whether the selected run passed.
--
-- Census also exposes a lexical inventory; successful parsing there does not
-- mean that Lower supports a script. A structurally valid plan preserves every
-- selected script, including issues that prevent individual tests being planned.
-- emit-buck2 snapshots explicit inputs into a self-contained local Buck2 cell.
-- The wider test vocabulary remains separate work.
module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad (forM)
import Data.List (isPrefixOf, sort)
import System.Directory
  ( doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, pathIsSymbolicLink )
import System.Environment (getArgs)
import System.Exit (ExitCode(..), die, exitWith)
import System.FilePath
  ( (</>), dropTrailingPathSeparator, isAbsolute, makeRelative, normalise
  , splitDirectories, takeExtension, takeFileName )
import System.IO (hPutStrLn, stderr)

import Census
import Buck2
import Execute
import Correlate
import Lower
import Procedures (explainTest)
import qualified TestPlan as Plan
import Verdict

main :: IO ()
main = (getArgs >>= run) `catch` ioErrorReport
  where
    ioErrorReport :: IOException -> IO ()
    ioErrorReport = die . ("bsc-test-plan: " ++) . show

run :: [String] -> IO ()
run ["census", path] = census path >>= putStr . renderCensusText
run ["census", "--json", path] = census path >>= putStrLn . renderCensusJson
run ("plan":arguments) = planCommand arguments
run ["execute", planPath, identifier, "--installation", installation,
     "--suite", suite, "--output", output] = do
  plan <- readFile planPath >>= checked . Plan.decodePlan
  report <- executeTest (ExecutionConfig installation suite output defaultExecutionTimeoutMicros)
    plan identifier >>= checked
  putStrLn ("Executed " ++ identifier ++ "; " ++ show (length (executionChecks report)) ++ " checks.")
  if hasInfrastructureFailure report then exitWith (ExitFailure 1) else pure ()
run ["emit-buck2", planPath, "--suite-root", suite, "--installation", installation,
     "--output", output] = do
  plan <- readFile planPath >>= checked . Plan.decodePlan
  (count, gaps) <- emitBuck2 (EmitConfig suite installation output) plan
  putStrLn ("Emitted " ++ show count ++ " Buck2 test targets; " ++ show gaps ++ " execution gaps.")
run ["explain", planPath, identifier] = do
  plan <- readFile planPath >>= checked . Plan.decodePlan
  checked (explainTest plan identifier) >>= putStr
run ["correlate", planPath, logPath] = do
  plan <- readFile planPath >>= checked . Plan.decodePlan
  files <- testLogFiles logPath
  traces <- fmap concat $ forM files $ \file -> do
    contents <- readFile file
    checked (decodeTestLog file contents)
  report <- checked (correlatePlan plan traces)
  putStr (renderCorrelation report)
  if null (correlationProblems report) then pure () else exitWith (ExitFailure 1)
run ["import-sum", "--config", config, "--suite-root", root,
     "--expected", expected, summaries, output] = do
  suiteRoot <- makeAbsolute root
  sourceRoot <- makeAbsolute summaries
  files <- summaryFiles sourceRoot
  if null files then die "No per-directory testrun.sum files found" else pure ()
  pieces <- forM files $ \file -> do
    contents <- readFile file
    -- The archive mirrors the original execution-directory hierarchy.
    let original = suiteRoot </> makeRelative sourceRoot file
    checked (parseSummary config suiteRoot original contents)
  manifest <- checked (mergeManifests pieces)
  expectedTests <- readExpected expected
  checked (validateDiscovery expectedTests manifest)
  writeFile output (encodeManifest manifest ++ "\n")
  putStrLn ("Imported " ++ show (length files) ++ " summaries; " ++
            show (length (manifestTests manifest)) ++ " test scripts; " ++
            show (length (manifestVerdicts manifest)) ++ " checks")
run ["compare", "--expected", expected, leftPath, rightPath] = do
  left <- readFile leftPath >>= checked . decodeManifest
  right <- readFile rightPath >>= checked . decodeManifest
  expectedTests <- readExpected expected
  checked (validateDiscovery expectedTests left)
  checked (validateDiscovery expectedTests right)
  differences <- checked (compareManifests left right)
  putStrLn ("Baseline checks: " ++ show (length (manifestVerdicts left)))
  putStrLn ("Baseline dispositions: " ++ show (dispositionCounts left))
  putStrLn ("Candidate checks: " ++ show (length (manifestVerdicts right)))
  putStrLn ("Candidate dispositions: " ++ show (dispositionCounts right))
  mapM_ print differences
  putStrLn ("Population differences: " ++ show (length differences))
  if null differences then pure () else exitWith (ExitFailure 1)
run ["--help"] = putStr usage
run [] = putStr usage
run _ = die usage

usage :: String
usage = unlines
  [ "Usage: bsc-test-plan census [--json] TESTSUITE-OR-EXP"
  , "       bsc-test-plan plan --config NAME --suite-root ROOT"
  , "         [--internal-checks 0|1] [--compiler-option OPTION]... TARGET"
  , "       bsc-test-plan explain PLAN.json TEST-OR-ISSUE-ID"
  , "       bsc-test-plan correlate PLAN.json LOG-DIRECTORY-OR-FILE"
  , "       bsc-test-plan emit-buck2 PLAN.json --suite-root ROOT"
  , "         --installation INST --output NEW-CELL"
  , "       bsc-test-plan execute PLAN.json TEST-ID --installation INST"
  , "         --suite SNAPSHOT --output NEW-OUTPUT"
  , "       bsc-test-plan import-sum --config NAME --suite-root ORIGINAL-ROOT"
  , "         --expected TEST-LIST SUMMARY-TREE OUTPUT.json"
  , "       bsc-test-plan compare --expected TEST-LIST BASELINE.json CANDIDATE.json"
  , ""
  , "TEST-LIST contains one suite-relative .exp path per line."
  , "SUMMARY-TREE preserves the test directory layout, containing testrun.sum."
  , "The census reports lexical sites, not successful semantic lowering."
  , "import-sum uses conservative legacy identities; see IDENTITY.md."
  , "plan writes semantic tests and explicit unsupported/unresolved items as JSON."
  , "Issues and planned/unsupported/unresolved counts are reported on stderr."
  , "A structurally valid plan succeeds even when it contains issues."
  , "explain accepts a v3:LENGTH:FILE:NUMBER test or numbered issue identifier."
  , "correlate matches supported invocations and their result roles to a saved plan."
  , "It reads BSC-TEST markers and single-line verdicts from ordinary testrun.log files."
  , "TARGET is the suite root, a bsc.* group, or one .exp file."
  , "Internal checks default to enabled. Compiler options are explicit inputs."
  ]

data PlanArguments = PlanArguments
  { argumentConfig :: Maybe String, argumentRoot :: Maybe FilePath
  , argumentInternal :: Maybe Bool, argumentFlags :: [String]
  , argumentTarget :: Maybe FilePath
  }

parsePlanArguments :: [String] -> Either String (Plan.PlanConfig, FilePath, FilePath)
parsePlanArguments = parse (PlanArguments Nothing Nothing Nothing [] Nothing)
  where
    parse options [] = case (argumentConfig options, argumentRoot options, argumentTarget options) of
      (Just name, Just root, Just target) -> Right
        (Plan.PlanConfig name (maybe True id (argumentInternal options)) (reverse (argumentFlags options)), root, target)
      _ -> Left "plan requires --config NAME, --suite-root ROOT, and TARGET"
    parse options ("--config":value:rest)
      | argumentConfig options == Nothing = parse options {argumentConfig = Just value} rest
    parse options ("--suite-root":value:rest)
      | argumentRoot options == Nothing = parse options {argumentRoot = Just value} rest
    parse options ("--internal-checks":value:rest)
      | argumentInternal options == Nothing, value `elem` ["0", "1"] =
          parse options {argumentInternal = Just (value == "1")} rest
    parse options ("--compiler-option":value:rest) =
      parse options {argumentFlags = value : argumentFlags options} rest
    parse options (value:rest)
      | not ("-" `isPrefixOf` value), argumentTarget options == Nothing =
          parse options {argumentTarget = Just value} rest
    parse _ _ = Left "invalid or repeated plan argument; use --help for syntax"

planCommand :: [String] -> IO ()
planCommand arguments = do
  (configuration, rootArgument, targetArgument) <- checked (parsePlanArguments arguments)
  root <- dropTrailingPathSeparator . normalise <$> makeAbsolute rootArgument
  target <- dropTrailingPathSeparator . normalise <$> makeAbsolute targetArgument
  rootExists <- doesDirectoryExist root
  if rootExists then pure () else die "plan: suite root is not a directory"
  let relative = makeRelative root target
  if isAbsolute relative || ".." `elem` splitDirectories relative
    then die "plan: target must be inside the suite root"
    else pure ()
  directory <- doesDirectoryExist target
  files <- if directory then do
      if target == root || "bsc." `isPrefixOf` takeFileName target
        then censusFiles <$> censusTree target
        else die "plan: directory target must be the suite root or a bsc.* group"
    else do
      exists <- doesFileExist target
      if exists && takeExtension target == ".exp"
        then pure [target]
        else die "plan: target must be an existing .exp file"
  if null files then die "plan: no active test scripts selected" else pure ()
  inputs <- forM files $ \file -> do
    source <- readFile file
    pure (makeRelative root file, source)
  plan <- checked (lowerPlan configuration inputs)
  mapM_ (hPutStrLn stderr . renderIssue) (Plan.planIssues plan)
  let (planned, unsupported, unresolved) = Plan.planCounts plan
  hPutStrLn stderr ("Plan: " ++ show planned ++ " planned, " ++
    show unsupported ++ " unsupported, " ++ show unresolved ++ " unresolved.")
  putStr (Plan.encodePlan plan)

checked :: Either String a -> IO a
checked = either (die . ("bsc-test-plan: " ++)) pure

census :: FilePath -> IO CensusReport
census path = do
  directory <- doesDirectoryExist path
  report <- if directory then censusTree path else censusFile path
  if null (censusFiles report)
    then die "No active test .exp files found; pass the testsuite root, a bsc.* group, or an .exp file"
    else pure report

readExpected :: FilePath -> IO [FilePath]
readExpected path = do
  entries <- filter (not . null) . lines <$> readFile path
  if null entries then die "Expected test manifest is empty" else pure entries

summaryFiles :: FilePath -> IO [FilePath]
summaryFiles root = walk root
  where
    walk directory = do
      names <- sort <$> listDirectory directory
      fmap concat $ forM names $ \name -> do
        let path = directory </> name
        isDirectory <- doesDirectoryExist path
        symbolic <- pathIsSymbolicLink path
        if isDirectory && not symbolic &&
           (directory /= root || "bsc." `isPrefixOf` name)
          then walk path
          else pure [path | directory /= root && not isDirectory && not symbolic
                          && takeFileName path == "testrun.sum"]

-- A directory may contain one ordinary testrun.log per test group. Never
-- follow symlinks and accidentally mix a second run into the selected logs.
testLogFiles :: FilePath -> IO [FilePath]
testLogFiles root = do
  symbolic <- pathIsSymbolicLink root
  if symbolic then die "correlate: symbolic links are not accepted" else pure ()
  directory <- doesDirectoryExist root
  if directory then walk root else do
    exists <- doesFileExist root
    if exists
      then pure [root]
      else die "correlate: expected a log directory or file"
  where
    walk directory = do
      names <- sort <$> listDirectory directory
      fmap concat $ forM names $ \name -> do
        let path = directory </> name
        symbolic <- pathIsSymbolicLink path
        nested <- doesDirectoryExist path
        if symbolic then pure []
          else if nested then walk path
          else do
            file <- doesFileExist path
            pure [path | file && takeFileName path == "testrun.log"]
