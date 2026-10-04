module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad (forM)
import Data.List (isPrefixOf, sort)
import System.Directory
  ( doesDirectoryExist, listDirectory, makeAbsolute, pathIsSymbolicLink )
import System.Environment (getArgs)
import System.Exit (ExitCode(..), die, exitWith)
import System.FilePath ((</>), makeRelative, takeFileName)

import BscTestsuite.Census
import BscTestsuite.Verdict

main :: IO ()
main = (getArgs >>= run) `catch` ioErrorReport
  where
    ioErrorReport :: IOException -> IO ()
    ioErrorReport = die . ("bsc-test-plan: " ++) . show

run :: [String] -> IO ()
run ["census", path] = census path >>= putStr . renderCensusText
run ["census", "--json", path] = census path >>= putStrLn . renderCensusJson
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
  , "       bsc-test-plan import-sum --config NAME --suite-root ORIGINAL-ROOT"
  , "         --expected TEST-LIST SUMMARY-TREE OUTPUT.json"
  , "       bsc-test-plan compare --expected TEST-LIST BASELINE.json CANDIDATE.json"
  , ""
  , "TEST-LIST contains one suite-relative .exp path per line."
  , "SUMMARY-TREE preserves the test directory layout, containing testrun.sum."
  , "The census reports lexical sites, not successful semantic lowering."
  , "import-sum uses conservative legacy identities; see IDENTITY.md."
  ]

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
