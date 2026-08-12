-- Cabal test driver for the (dejagnu) BSC testsuite.
--
-- cabal provides the freshly-built executables (via build-tool-depends); we
-- point the existing testsuite harness at them with the TEST_<TOOL> make
-- vars and let it run.  BLUESPECDIR defaults (in the testsuite Makefile) to
-- ./inst/lib, the make-built runtime, so 'make install-src' must have been
-- run first.
--
-- One driver, several test-suites, dispatched on the executable name:
--   smoke          a quick SystemC-free spread (front end, scheduler/SAT,
--                  both backends, a bluetcl query)
--   bluetcl-tests  the full bsc.bluetcl tree
--   utils          the dirs exercising the utility exes (showrules, vcdcheck,
--                  dumpbo, dumpba, bsc2bsv), with internal checks enabled
--   testsuite      the whole testsuite, in parallel (the equivalent of
--                  'make check-suite-parallel'); pass --full (through
--                  cabal's --test-options) to also enable the long tests
--
-- Tests whose tools this machine lacks are skipped, following the harness's
-- own conventions: without a Verilog simulator we set VTEST=0, without a C++
-- compiler CTEST=0, and without SystemC headers SYSTEMCTEST=0 -- unless the
-- caller has already set those variables.
module Main (main) where

import Control.Monad (unless, when)
import Data.Char (toUpper)
import Data.List (sort)
import Data.Maybe (fromMaybe, isJust, isNothing)
import GHC.Conc (getNumProcessors)
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable)
import System.Environment (getArgs, getExecutablePath, getProgName, lookupEnv, setEnv)
import System.Exit (ExitCode (..), die, exitWith)
import System.FilePath (dropExtension, makeRelative, takeDirectory, (</>))
import System.Process (rawSystem, readProcess, readProcessWithExitCode)

main :: IO ()
main = do
  name <- getProgName
  args <- getArgs

  haveRuntime <- doesDirectoryExist "inst/lib"
  unless haveRuntime $
    die "inst/lib not found - build the runtime ('make install-src') first."

  skipUnsupportedTests
  (flags, targets, toolVars) <- plan name args
  ec <- rawSystem "make" (["-C", "testsuite"] ++ flags ++ toolVars ++ targets)
  exitWith ec

-- For a suite (matched by name), the extra make flags, the make targets to
-- run, and the TEST_<TOOL> make vars pointing the harness at the cabal-built
-- tools it needs.
plan :: String -> [String] -> IO ([String], [String], [String])
plan name args = case name of
  "smoke" -> do
    tools <- toolVars ["bsc", "bluetcl"]
    return ([], smokeTargets, tools)
  "bluetcl-tests" -> do
    tools <- toolVars ["bsc", "bluetcl"]
    exps <- lines <$> readProcess "find" ["testsuite/bsc.bluetcl", "-name", "*.exp"] ""
    let ts = sort [ makeRelative "testsuite" (dropExtension e) ++ ".check"
                  | e <- exps, not (null e) ]
    when (null ts) (die "no bsc.bluetcl tests found")
    return ([], ts, tools)
  "utils" -> do
    -- The vcdcheck/dumpbo/dumpba/bsc2bsv assertions are no-ops unless the
    -- internal checks are enabled (and enabling them requires providing all
    -- four tools).
    setEnv "DO_INTERNAL_CHECKS" "1"
    tools <- toolVars allTools
    return ([], utilTargets, tools)
  "testsuite" -> do
    tools <- toolVars allTools
    jobs <- getNumProcessors
    let target = if "--full" `elem` args then "fullparallel" else "checkparallel"
    return (["-j" ++ show jobs], [target], tools)
  _ -> die ("unknown test suite: " ++ name)
  where
    allTools = ["bsc", "bluetcl", "showrules", "vcdcheck", "dumpbo", "dumpba", "bsc2bsv"]
    toolVars exes = sequence [ (\p -> makeVar exe ++ "=" ++ p) <$> need exe | exe <- exes ]
    makeVar exe = "TEST_" ++ map toUpper exe

-- A small SystemC-free spread: preprocessor (front end), FloatingPoint
-- (typecheck + codegen + both sims + RTS opts), schedule (scheduler/SAT) on
-- both backends, and a bluetcl query.
smokeTargets :: [String]
smokeTargets =
  [ "bsc.preprocessor/misc/misc.check"
  , "bsc.lib/FloatingPoint/FloatTest.check"
  , "bsc.bluesim/schedule/schedule.check"
  , "bsc.verilog/schedule/schedule.check"
  , "bsc.bluetcl/commands/commands.check"
  ]

-- Dirs that exercise the utility executables: showrules + vcdcheck
-- (dedicated dirs) and the internal-check tools (gensign / noinline).
utilTargets :: [String]
utilTargets =
  [ "bsc.showrules/showrules.check"
  , "bsc.vcdcheck/vcdcheck.check"
  , "bsc.driver/gensign/gensign.check"
  , "bsc.verilog/noinline/noinline.check"
  ]

-- Set VTEST/CTEST/SYSTEMCTEST to 0 when the tools they need are missing,
-- unless the caller has already chosen.  (The harness has no auto-detection
-- of its own; without this, e.g. every Verilog test fails when iverilog is
-- absent.)
skipUnsupportedTests :: IO ()
skipUnsupportedTests = do
  cxx <- fromMaybe "c++" <$> lookupEnv "CXX"
  vsim <- fromMaybe "iverilog" <$> lookupEnv "TEST_BSC_VERILOG_SIM"
  defaultTo "VTEST" (isJust <$> findExecutable vsim)
  defaultTo "CTEST" (isJust <$> findExecutable cxx)
  defaultTo "SYSTEMCTEST" (haveSystemC cxx)
  where
    defaultTo var probe = do
      chosen <- lookupEnv var
      when (isNothing chosen) $ do
        ok <- probe
        unless ok $ do
          putStrLn ("setting " ++ var ++ "=0 (missing tools)")
          setEnv var "0"
    haveSystemC cxx = do
      inc <- lookupEnv "SYSTEMC_INC"
      home <- lookupEnv "SYSTEMC"
      let incArgs = [ "-I" ++ d | Just d <- [inc, (</> "include") <$> home] ]
      (ec, _, _) <- readProcessWithExitCode cxx
                      (incArgs ++ ["-E", "-x", "c++", "-"])
                      "#include <systemc.h>\n"
      return (ec == ExitSuccess)

-- Locate a sibling cabal executable.  build-tool-depends guarantees it has
-- been built, and with the (per-package) build layout cabal uses for this
-- package, test-suite and executable binaries are siblings:
-- .../build/<component>/<component>.  Fail loudly rather than falling back
-- to PATH, which could silently test some other installation's binaries.
need :: String -> IO FilePath
need exe = do
  self <- getExecutablePath
  let sibling = takeDirectory (takeDirectory self) </> exe </> exe
  here <- doesFileExist sibling
  unless here $
    die ("cannot find the cabal-built " ++ exe ++ " (expected it at "
         ++ sibling ++ "); has the dist-newstyle layout changed?")
  return sibling
