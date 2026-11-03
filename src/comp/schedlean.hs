{- |
Module      : Main_schedlean
Description : Prove in Lean 4 that bsc's schedule of a module preserves rule atomicity

The driver only: it parses the options, reads the @.ba@, calls the
schedule verifier ("Lean.Sched.*") and writes what it returns.
-}
module Main_schedlean (main) where

import ABin (ABin(..), ABinModInfo(..))
import AState (aState)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Error (initErrorHandle)
import GenABin (readABinFile)
import PPrint (ppReadable)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt, usageInfo)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith, exitSuccess)
import System.FilePath (takeBaseName)
import System.IO (IOMode(..), hPutStr, hPutStrLn, hSetEncoding, stderr, stdout, utf8, withFile)
import Version (bluespec, versionStr)
import qualified Data.ByteString as BS (readFile)

import Lean.Sched.APkg (parseAPkg, showAPkg, showAPkgNoSched, memAPkgText)
import Lean.Sched.Bounds (applyLimits, defaultBounds)
import Lean.Sched.BscFacts (disjointCandidates, confirmDisjointnessMore)
import Lean.Sched.Emit (Emitted(..), emitModule)
import Lean.Sched.Expr (memAPkg)
import Lean.Sched.Extract (extractDesign, renameSchedulerUses)
import Lean.Sched.Flat (Proof(..), analyse, emitLean)
import Lean.Sched.Gate (gateReport)
import Lean.Sched.Run (Input(..))
import Lean.Sched.Show (dumpDesign)
import Lean.Sched.Time (newTimer, phase, reportTotals)
import Lean.Sched.Types (Design(..), leanNamespace)
import Lean.Sched.Views (showView, viewNames, solverCheck)

versionString :: String
versionString = versionStr True (bluespec ++ " schedlean utility")

data Options = Options
    { optOutput    :: Maybe FilePath
    , optShow      :: Maybe String
    , optGate      :: Bool
    , optTime      :: Bool
    , optFlat      :: Bool
    , optLean      :: Maybe FilePath
    , optCheck     :: Bool
    , optMutants   :: Maybe Int
    , optLimits    :: [(String, Int)]
    , optFirstRules :: Maybe Int
    , optNoTasks   :: Bool
    , optHelp      :: Bool
    , optVersion   :: Bool
    , optSolverCheck :: Maybe FilePath
    , optErrs      :: [String]
    }

defaultOptions :: Options
defaultOptions = Options Nothing Nothing False False False Nothing False Nothing [] Nothing False False False Nothing []

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['o'] ["output"] (ReqArg (\f o -> o { optOutput = Just f }) "FILE")
        "write the Lean file to FILE (default: <Module>.lean)"
    , Option [] ["show-model"] (OptArg (\w o -> o { optShow = Just (fromMaybe "" w) }) "WHAT")
        ("print an intermediate view instead (WHAT: empty for the extracted design; apkg, apkg-tree, apkg-text, apkg-mem, apkg-memtext: "
         ++ "the APackage; sites, the site conditions; model, the module model and scan; facts, the gate's facts and shadow classes)")
    , Option [] ["gate"] (NoArg (\o -> o { optGate = True }))
        "report whether the modular proof applies (GATE record and summary row)"
    , Option [] ["time-phases"] (NoArg (\o -> o { optTime = True }))
        "print each phase's wall and CPU time on stderr"
    , Option [] ["flat"] (NoArg (\o -> o { optFlat = True }))
        "write the old flat (whole-module) theorem, for comparison (temporary)"
    , Option [] ["lean"] (ReqArg (\f o -> o { optLean = Just f }) "PATH")
        "the Lean executable for --check (default: $LEAN, then PATH)"
    , Option [] ["check"] (NoArg (\o -> o { optCheck = True }))
        "run Lean on the file written (not implemented yet)"
    , Option [] ["mutants"] (ReqArg (\n o -> num n (\k -> o { optMutants = Just k }) o) "N")
        "check that N mutants of the hardware model fail (not implemented yet)"
    , Option [] ["limit"] (ReqArg limit "NAME=N")
        "measurement: lower a gate bound or table setting (never raises one)"
    , Option [] ["first-rules"] (ReqArg (\n o -> num n (\k -> o { optFirstRules = Just k }) o) "N")
        "measurement: keep only the first N steps (a partial theorem, labelled in the file)"
    , Option [] ["no-task-agreement"] (NoArg (\o -> o { optNoTasks = True }))
        "measurement: leave out task agreement (a weaker theorem, labelled in the file)"
    , Option [] ["solver-check"] (ReqArg (\d o -> o { optSolverCheck = Just d }) "DIR")
        "test: bsc's solver on logged CNF queries (DIR/verdicts.log), against their verdicts"
    , Option ['V'] ["version"] (NoArg (\o -> o { optVersion = True }))
        "show version information and exit"
    , Option ['h', '?'] ["help"] (NoArg (\o -> o { optHelp = True }))
        "print usage information and exit"
    ]
  where
    num s k o | not (null s), all isDigit s = k (read s)
              | otherwise = o { optErrs = optErrs o ++ ["not a number: " ++ s ++ "\n"] }
    limit s o = case break (== '=') s of
        (n, '=' : v) | not (null n) -> num v (\k -> o { optLimits = optLimits o ++ [(n, k)] }) o
        _ -> o { optErrs = optErrs o ++ ["--limit wants NAME=N, not " ++ s ++ "\n"] }

usageHeader :: String
usageHeader = "Usage: schedlean [OPTIONS] MODULE.ba"

die :: Int -> String -> IO a
die rc msg = hPutStrLn stderr ("schedlean: " ++ msg) >> exitWith (ExitFailure rc)

main :: IO ()
main = do
    argv <- getArgs
    let (fs, args, errs0) = getOpt Permute options argv
        opts = foldl (flip id) defaultOptions fs
        errs = errs0 ++ optErrs opts
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    when (optHelp opts) $ putStr (usageInfo usageHeader options) >> exitSuccess
    when (optVersion opts) $ putStrLn versionString >> exitSuccess
    input <- case (errs, args) of
        ([], [ba]) -> pure ba
        _ -> do mapM_ (hPutStr stderr) errs
                hPutStr stderr (usageInfo usageHeader options)
                exitWith (ExitFailure 2)
    when (optCheck opts) $ die 2 "--check is not implemented yet"
    when (optMutants opts /= Nothing) $ die 2 "--mutants is not implemented yet"
    timer <- newTimer (optTime opts)
    errh <- initErrorHandle
    (abmi, version) <- phase timer "read" $ do
        bytes <- BS.readFile input
        let (abin, hash) = readABinFile errh input bytes
        -- the printout dumpba gives, which the prototype read
        let baText = ppReadable abin ++ "Hash: " ++ hash ++ "\n"
        when (optShow opts == Just "apkg") $ putStr baText >> exitSuccess
        when (optShow opts == Just "apkg-tree") $ mapM_ putStrLn (showAPkg (parseAPkg baText)) >> exitSuccess
        when (optShow opts == Just "apkg-text") $ mapM_ putStrLn (showAPkgNoSched (parseAPkg baText)) >> exitSuccess
        case abin of
            ABinMod m _ | optShow opts == Just "apkg-mem" -> mapM_ putStrLn (showAPkgNoSched (memAPkg (abmi_apkg m))) >> exitSuccess
                        | optShow opts == Just "apkg-memtext" -> mapM_ putStrLn (showAPkgNoSched (memAPkgText (abmi_apkg m))) >> exitSuccess
            _ -> return ()
        case abin of
            ABinMod abmi ver   -> evaluate (force (abmi_apkg abmi, abmi_aschedinfo abmi)) >> pure (abmi, ver)
            ABinForeignFunc {} -> die 1 (input ++ " describes a foreign function, not a module")
            ABinModSchedErr {} -> die 1 (input ++ " is from a module that did not schedule")
    let flags = abmi_flags abmi
    forM_ (optSolverCheck opts) $ \dir -> solverCheck errh flags dir >>= mapM_ putStrLn >> exitSuccess
    let known = maybe True (`elem` ("" : viewNames)) (optShow opts)
    when (not known) $ die 2 ("--show-model=" ++ fromMaybe "" (optShow opts) ++ " is not implemented yet")
    aspkg <- phase timer "astate" $
        aState errh flags (abmi_pps abmi) (abmi_aschedinfo abmi) (abmi_apkg abmi) >>= evaluate . force
    let abmi' = abmi { abmi_apkg = renameSchedulerUses (abmi_apkg abmi) }
        -- the CF-swap orders (cubic in methods) only for the extraction dump and the flat file
        full = optFlat opts || optShow opts `elem` [Just "", Just "inverted-check"]
    design <- phase timer "extract" $
        either (die 1) (evaluate . force) (extractDesign full input version abmi' aspkg)
    (cands, facts, more) <- phase timer "bsc-facts" $ do
        let cands = force (disjointCandidates design)
        (facts, more) <- confirmDisjointnessMore errh flags (abmi_apkg abmi') design cands
        _ <- evaluate (force (cands, facts))
        return (cands, facts, more)
    bounds <- either (die 2) return (applyLimits (optLimits opts) defaultBounds)
    let inp = Input design cands facts more abmi bounds (takeBaseName input) flags errh timer
        ok = either (die 1) return
    case () of
      _ | Just "" <- optShow opts -> do
            ls <- phase timer "emit" $ evaluate (force (dumpDesign design cands facts))
            phase timer "write" $ mapM_ putStrLn ls
        | Just what <- optShow opts -> showView inp what >>= ok >>= phase timer "write" . mapM_ putStrLn
        | optGate opts -> gateReport inp >>= ok >>= mapM_ putStrLn
        | optFlat opts -> do
            let file = fromMaybe (leanNamespace design ++ ".lean") (optOutput opts)
            proof <- phase timer "analyse" $ evaluate (analyse design facts)
            case pfErrors proof of
                [] -> return ()
                es -> die 1 (intercalate "; " es)
            text <- phase timer "emit" $ let t = emitLean design proof in evaluate (length t) >> pure t
            phase timer "write" $ withFile file WriteMode $ \h -> hSetEncoding h utf8 >> hPutStr h text
            forM_ (dWarnings design ++ pfWarnings proof) $ \w ->
                hPutStrLn stderr ("schedlean: warning: " ++ w)
            putStrLn ("schedlean: wrote " ++ file)
        | otherwise -> do
            -- the modular file: its notes, its parts, then the summary lines
            let file = fromMaybe (leanNamespace design ++ ".lean") (optOutput opts)
                base = reverse (drop 1 (dropWhile (/= '.') (reverse file)))
            em <- emitModule inp (not (optNoTasks opts)) (optFirstRules opts) file >>= ok
            let (notes, rest) = span (\l -> take 5 l == "note:") (emLog em)
            mapM_ putStrLn notes
            phase timer "write" $ forM_ (emParts em) $ \(pn, t) -> do
                let f = maybe file (\i -> base ++ "_p" ++ show i ++ ".lean") pn
                withFile f WriteMode $ \h -> hSetEncoding h utf8 >> hPutStr h t
            mapM_ putStrLn rest
    reportTotals timer
