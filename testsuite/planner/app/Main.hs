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

import BscTestsuite.Census
import BscTestsuite.Lower
import qualified BscTestsuite.TestPlan as Plan
import BscTestsuite.Verdict

main :: IO ()
main = (getArgs >>= run) `catch` ioErrorReport
  where
    ioErrorReport :: IOException -> IO ()
    ioErrorReport = die . ("bsc-test-plan: " ++) . show

run :: [String] -> IO ()
run ["census", path] = census path >>= putStr . renderCensusText
run ["census", "--json", path] = census path >>= putStrLn . renderCensusJson
run ("plan":arguments) = planCommand arguments
run ["explain", planPath, identifier] = do
  plan <- readFile planPath >>= checked . Plan.decodePlan
  checked (Plan.explainCheck plan identifier) >>= putStr
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
  , "       bsc-test-plan explain PLAN.json CHECK-ID"
  , "       bsc-test-plan import-sum --config NAME --suite-root ORIGINAL-ROOT"
  , "         --expected TEST-LIST SUMMARY-TREE OUTPUT.json"
  , "       bsc-test-plan compare --expected TEST-LIST BASELINE.json CANDIDATE.json"
  , ""
  , "TEST-LIST contains one suite-relative .exp path per line."
  , "SUMMARY-TREE preserves the test directory layout, containing testrun.sum."
  , "The census reports lexical sites, not successful semantic lowering."
  , "import-sum uses conservative legacy identities; see IDENTITY.md."
  , "plan writes versioned JSON only if every selected script lowers."
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
  case lowerPlan configuration inputs of
    Left issues -> do
      mapM_ (hPutStrLn stderr . renderIssue) issues
      hPutStrLn stderr ("No plan emitted: " ++ show (length issues) ++ " of " ++
        show (length files) ++ " selected scripts could not be lowered.")
      exitWith (ExitFailure 2)
    Right plan -> putStrLn (Plan.encodePlan plan)

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
