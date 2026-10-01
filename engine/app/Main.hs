-- | bsc-engine: the orchestration engine of doc/engine-first-plan.md.
--
-- Phase P1: @bsc-engine libraries@ builds src/Libraries as one Shake graph.
-- The command-line surface is deliberately small; the Makefile variables it
-- replaces (PREFIX, BUILDDIR, BSC, BSCFLAGS, BO2BLOOGLE, TCONCHECK) are the
-- options.
module Main (main) where

import Warmup ()

import BscEngine.Libraries
import Control.Monad (when)
import Data.List (isPrefixOf)
import System.Directory (canonicalizePath, doesDirectoryExist, getCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)

usage :: String -> String
usage prog = unlines
  [ "usage: " ++ prog ++ " libraries [options] [targets]"
  , ""
  , "Build the Bluespec libraries (src/Libraries) as one dependency graph."
  , "Targets: build (default: install), tconcheck, bloogle, install, plan."
  , ""
  , "options (defaults reproduce src/Libraries/common.mk):"
  , "  --top DIR          repository root (default: the nearest ancestor of the"
  , "                     working directory holding src/Libraries)"
  , "  --prefix DIR       install prefix (default: TOP/inst; PREFIX)"
  , "  --builddir DIR     where .bo/.ba are built (default: TOP/build/bsvlib; BUILDDIR)"
  , "  --bsc PATH         the compiler (default: PREFIX/bin/bsc; BSC)"
  , "  --bscdeps PATH     the discovery tool (default: beside bsc, else PATH)"
  , "  --bo2bloogle PATH  (default: PREFIX/bin/bo2bloogle; BO2BLOOGLE)"
  , "  --tconcheck PATH   (default: PREFIX/bin/tconcheck; TCONCHECK)"
  , "  --bsc-flags FLAGS  extra flags for every bsc invocation (BSCFLAGS), one"
  , "                     string, split on spaces"
  , "  --shake-dir DIR    Shake's database (default: TOP/.bsc-engine/libraries;"
  , "                     outside build/ and inst/ on purpose)"
  , "  --compiler-key K   what a compiled package depends on in the compiler:"
  , "                     'binary' (default: the bsc executable's content; P2's"
  , "                     conservative whole-compiler key) or 'closure'"
  , "                     (experimental preview of P3: the object files of the"
  , "                     components in the .bo producer's closure under the"
  , "                     dist-newstyle tree the executable was built in, so an"
  , "                     edit to a backend does not re-key the libraries)"
  , "  -j N               worker processes (default: the number of CPUs)"
  , "  -V                 verbose (print every command)"
  , "  --lint             Shake lint checks"
  ]

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case args of
    ("libraries" : rest) -> do
      cfg <- parseArgs prog rest defaultConfig
      cfg' <- resolveConfig cfg
      runLibraries cfg'
    _ -> do
      hPutStrLn stderr (usage prog)
      exitFailure

parseArgs :: String -> [String] -> Config -> IO Config
parseArgs _ [] c = pure c
parseArgs p ("--top" : v : r) c = parseArgs p r c {cfgTop = Just v}
parseArgs p ("--prefix" : v : r) c = parseArgs p r c {cfgPrefix = Just v}
parseArgs p ("--builddir" : v : r) c = parseArgs p r c {cfgBuildDir = Just v}
parseArgs p ("--bsc" : v : r) c = parseArgs p r c {cfgBsc = Just v}
parseArgs p ("--bscdeps" : v : r) c = parseArgs p r c {cfgBscDeps = Just v}
parseArgs p ("--bo2bloogle" : v : r) c = parseArgs p r c {cfgBo2Bloogle = Just v}
parseArgs p ("--tconcheck" : v : r) c = parseArgs p r c {cfgTconCheck = Just v}
parseArgs p ("--bsc-flags" : v : r) c = parseArgs p r c {cfgBscFlags = cfgBscFlags c ++ words v}
parseArgs p ("--shake-dir" : v : r) c = parseArgs p r c {cfgShakeDir = Just v}
parseArgs p ("--compiler-key" : v : r) c
  | v `elem` ["binary", "closure"] = parseArgs p r c {cfgCompilerKey = v}
  | otherwise = die' p ("--compiler-key must be binary or closure, got " ++ show v)
parseArgs p ("-j" : v : r) c = case reads v of
  [(n, "")] | n >= 0 -> parseArgs p r c {cfgJobs = n}
  _ -> die' p ("-j needs a non-negative number, got " ++ show v)
parseArgs p ("-V" : r) c = parseArgs p r c {cfgVerbose = True}
parseArgs p ("--lint" : r) c = parseArgs p r c {cfgLint = True}
parseArgs p (t : r) c
  | "-" `isPrefixOf` t = die' p ("unknown option " ++ show t)
  | otherwise = parseArgs p r c {cfgTargets = cfgTargets c ++ [t]}

die' :: String -> String -> IO a
die' p msg = do
  hPutStrLn stderr (p ++ ": " ++ msg)
  hPutStrLn stderr (usage p)
  exitFailure

-- | Fill in the defaults of common.mk and make every path absolute, so that
-- the actions can run with the library directory as their working directory.
resolveConfig :: Config -> IO Config
resolveConfig c = do
  top <- case cfgTop c of
    Just t -> canonicalizePath t
    Nothing -> getCurrentDirectory >>= findTop
  let abs' rel = canonicalizePath (top </> rel)
  prefix <- maybe (abs' "inst") canonicalizePath (cfgPrefix c)
  buildDir <- maybe (abs' ("build" </> "bsvlib")) canonicalizePath (cfgBuildDir c)
  let binDir = prefix </> "bin"
  bsc <- maybe (pure (binDir </> "bsc")) canonicalizePath (cfgBsc c)
  bscdeps <- maybe (pure (takeDirectory bsc </> "bscdeps")) canonicalizePath (cfgBscDeps c)
  bo2bloogle <- maybe (pure (binDir </> "bo2bloogle")) canonicalizePath (cfgBo2Bloogle c)
  tconcheck <- maybe (pure (binDir </> "tconcheck")) canonicalizePath (cfgTconCheck c)
  shakeDir <- maybe (pure (top </> ".bsc-engine" </> "libraries")) canonicalizePath (cfgShakeDir c)
  when (null (cfgTargets c)) $ pure ()
  pure c
    { cfgTop = Just top
    , cfgPrefix = Just prefix
    , cfgBuildDir = Just buildDir
    , cfgBsc = Just bsc
    , cfgBscDeps = Just bscdeps
    , cfgBo2Bloogle = Just bo2bloogle
    , cfgTconCheck = Just tconcheck
    , cfgShakeDir = Just shakeDir
    , cfgTargets = if null (cfgTargets c) then ["install"] else cfgTargets c
    }

findTop :: FilePath -> IO FilePath
findTop dir = do
  here <- doesDirectoryExist (dir </> "src" </> "Libraries")
  if here
    then pure dir
    else if takeDirectory dir == dir
      then do
        hPutStrLn stderr "bsc-engine: no src/Libraries above the working directory; use --top"
        exitFailure
      else findTop (takeDirectory dir)
