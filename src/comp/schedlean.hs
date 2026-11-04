{- |
Module      : Main_schedlean
Description : Prove in Lean 4 that bsc's schedule of a module preserves rule atomicity

The driver only: it parses the options, reads the @.ba@, calls the
schedule verifier ("Lean.Sched.*") and writes what it returns.
-}
module Main_schedlean (main) where

import ABin (ABin(..), ABinModInfo(..))
import AState (aState)
import ASyntax (APackage(apkg_name), apkgIsMCD)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Error (initErrorHandle)
import GenABin (readABinFile)
import Id (getIdString)
import PPrint (ppReadable)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt, usageInfo)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith, exitSuccess)
import System.FilePath (takeBaseName)
import System.IO (IOMode(..), hFlush, hPutStr, hPutStrLn, hSetEncoding, stderr, stdout, utf8, withFile)
import Version (bluespec, versionStr)
import qualified Data.ByteString as BS (readFile)

import Lean.Run (Result(..), cannotRunCode, defaultTimeout, verdictCode)
import Lean.Sched.APkg (parseAPkg, showAPkg, showAPkgNoSched, memAPkgText)
import Lean.Sched.Bounds (applyLimits, defaultBounds)
import Lean.Sched.BscFacts (disjointCandidates, confirmDisjointnessMore)
import Lean.Sched.Check (CheckOpts(..), checkFiles, checkModule, finishChecker, newChecker, reportCheck, setupLean, writeEmitted)
import Lean.Sched.Hier (HierFile(..), hierEmit, hierGate, loadHier, synthInsts)
import Lean.Sched.Domain (domainOf, multiDomain, multiDomainVerdict)
import Lean.Sched.Emit (emitModule)
import Lean.Sched.Expr (memAPkg)
import Lean.Sched.Extract (extractDesign, renameSchedulerUses)
import Lean.Sched.Flat (Proof(..), analyse, emitLean)
import Lean.Sched.Gate (gateReport, modeName, multiDomainReport)
import Lean.Sched.Run (Input(..), noHier)
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
    , optLeanLib   :: Maybe FilePath
    , optLeanTimeout :: Int
    , optCheck     :: Bool
    , optMutants   :: Maybe Int
    , optKeepMutants :: Bool
    , optLimits    :: [(String, Int)]
    , optFirstRules :: Maybe Int
    , optNoTasks   :: Bool
    , optHelp      :: Bool
    , optVersion   :: Bool
    , optSolverCheck :: Maybe FilePath
    , optSearch    :: [FilePath]
    , optErrs      :: [String]
    }

defaultOptions :: Options
defaultOptions = Options Nothing Nothing False False False Nothing Nothing defaultTimeout False Nothing False [] Nothing False False False Nothing [] []

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['o'] ["output"] (ReqArg (\f o -> o { optOutput = Just f }) "FILE")
        "write the Lean file to FILE (default: <Module>.lean)"
    , Option [] ["show-model"] (OptArg (\w o -> o { optShow = Just (fromMaybe "" w) }) "WHAT")
        ("print an intermediate view instead (WHAT: empty for the extracted design; apkg, apkg-tree, apkg-text, apkg-mem, apkg-memtext: "
         ++ "the APackage; sites, the site conditions; model, the module model and scan; facts, the gate's facts and shadow classes)")
    , Option [] ["gate"] (NoArg (\o -> o { optGate = True }))
        "report whether the modular proof applies (GATE record and summary row; a module of several clock domains: not checked: multi-domain)"
    , Option [] ["time-phases"] (NoArg (\o -> o { optTime = True }))
        "print each phase's wall and CPU time on stderr"
    , Option [] ["flat"] (NoArg (\o -> o { optFlat = True }))
        "write the old flat (whole-module) theorem, for comparison (temporary)"
    , Option [] ["check"] (NoArg (\o -> o { optCheck = True }))
        "check the file written (its parts in order) in Lean: proved, counterexample, vacuous, timeout or error"
    , Option [] ["mutants"] (ReqArg (\n o -> num n (\k -> o { optMutants = Just k }) o) "N")
        ("check the modular file, then N mutants of its hardware (enable drop or force, argument, mux, guard read, "
         ++ "CF order, last-caller order, conflict_free condition), each of which must fail; mutant i is the same for every N >= i; "
         ++ "one the emitter does not write is counted apart, not as killed")
    , Option [] ["keep-mutants"] (NoArg (\o -> o { optKeepMutants = True }))
        "keep every mutant's file and log (<stem>_mut<i>.lean; by default only those not killed)"
    , Option [] ["lean"] (ReqArg (\f o -> o { optLean = Just f }) "PATH")
        "the Lean executable (default: $LEAN, then lean on PATH)"
    , Option [] ["lean-lib"] (ReqArg (\f o -> o { optLeanLib = Just f }) "DIR")
        ("the Lean library: its .olean files, or its sources, compiled once into $SCHEDLEAN_CACHE, else "
         ++ "~/.cache/schedlean (default: $BLUESPECDIR/schedlean, else the checkout's util/schedlean/theory/modular)")
    , Option [] ["lean-timeout"] (ReqArg (\n o -> num n (\k -> o { optLeanTimeout = k }) o) "SECONDS")
        ("each Lean run's wall-clock limit (default " ++ show defaultTimeout ++ "); every run is capped at 24 GiB")
    , Option [] ["limit"] (ReqArg limit "NAME=N")
        "measurement: lower a gate bound or table setting (never raises one)"
    , Option [] ["first-rules"] (ReqArg (\n o -> num n (\k -> o { optFirstRules = Just k }) o) "N")
        "measurement: keep only the first N steps (a partial theorem, labelled in the file)"
    , Option [] ["no-task-agreement"] (NoArg (\o -> o { optNoTasks = True }))
        "measurement: leave out task agreement (a weaker theorem, labelled in the file)"
    , Option ['p'] ["search-path"] (ReqArg (\d o -> o { optSearch = optSearch o ++ splitColon d }) "DIRS")
        ("where synthesized children's .ba files are, after the parent's directory (colon-separated; may repeat); "
         ++ "each child gets its own file beside the top one, children first (design.md §3.1)")
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

splitColon :: String -> [FilePath]
splitColon str = case break (== ':') str of
    (a, ':' : r) -> [ a | not (null a) ] ++ splitColon r
    (a, _) -> [ a | not (null a) ]

usageHeader :: String
usageHeader = "Usage: schedlean [OPTIONS] MODULE.ba"

exitText :: String
exitText = unlines
    [ ""
    , "Exit status:"
    , "  0  the file is written (with --check: it proves; with --mutants: and every mutant fails)"
    , "  1  no file: the emitter stops (the message says why), or the .ba is not a module"
    , "  2  usage error"
    , "  3  --check: Lean finds a counterexample (decide: a proposition is false; bv_decide: an assignment)"
    , "  4  --check: vacuous (the hypotheses' consistency check fails; --flat files)"
    , "  5  --check: Lean timed out (or a mutant did)"
    , "  6  --check: any other Lean error, a sorry, or an axiom outside propext, Classical.choice, Quot.sound"
    , "  7  --check: Lean could not run (not found, or its library did not build)"
    , "  8  --mutants: a mutant's file checks (a missed mutant; its file is kept)"
    , "  9  --mutants: a mutant's file is not written (the emitter stops on it), no mutant missed or undecided" ]

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
    when (optHelp opts) $ putStr (usageInfo usageHeader options ++ exitText) >> exitSuccess
    when (optVersion opts) $ putStrLn versionString >> exitSuccess
    input <- case (errs, args) of
        ([], [ba]) -> pure ba
        _ -> do mapM_ (hPutStr stderr) errs
                hPutStr stderr (usageInfo usageHeader options)
                exitWith (ExitFailure 2)
    let nmut = fromMaybe 0 (optMutants opts)
        lean = optCheck opts || nmut > 0
    when (lean && (optGate opts || optShow opts /= Nothing)) $ die 2 "--check and --mutants check the file schedlean writes, not --gate or --show-model"
    when (nmut > 0 && optFlat opts) $ die 2 "--mutants mutates the modular file (util/schedlean/mutate.py mutates --flat files)"
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
    -- one clock domain is checked; a module of several is reported, with no fallback
    let apkg0 = abmi_apkg abmi
        modName = getIdString (apkg_name apkg0)
    when (optFlat opts && apkgIsMCD apkg0) $ die 1 (modName ++ ": modules with several clocks or resets are not supported (--flat)")
    forM_ (multiDomain (domainOf apkg0)) $ \why ->
        if optGate opts then mapM_ putStrLn (multiDomainReport modName (modeName flags) why) >> exitSuccess
        else die 1 (modName ++ ": " ++ multiDomainVerdict ++ " (" ++ why ++ ")")
    aspkg <- phase timer "astate" $
        aState errh flags (abmi_pps abmi) (abmi_aschedinfo abmi) (abmi_apkg abmi) >>= evaluate . force
    let abmi' = abmi { abmi_apkg = renameSchedulerUses (abmi_apkg abmi) }
        -- the CF-swap orders (cubic in methods) only for the extraction dump and the flat file
        full = optFlat opts || optShow opts `elem` [Just "", Just "inverted-check"]
    design <- phase timer "extract" $
        either (die 1) (evaluate . force) (extractDesign full input version abmi' aspkg)
    (cands, facts, more, sv) <- phase timer "bsc-facts" $ do
        let cands = force (disjointCandidates design)
        (facts, more, sv) <- confirmDisjointnessMore errh flags (abmi_apkg abmi') design cands
        _ <- evaluate (force (cands, facts))
        return (cands, facts, more, sv)
    bounds <- either (die 2) return (applyLimits (optLimits opts) defaultBounds)
    let inp = Input design cands facts more sv abmi bounds (takeBaseName input) flags errh timer noHier
        ok = either (die 1) return
        co = CheckOpts (optLean opts) (optLeanLib opts) (optLeanTimeout opts) (optTime opts) (optKeepMutants opts)
    -- synthesized children (design.md §3.1): their files first, each module once
    let hierRun = not (null (synthInsts (abmi_apkg abmi))) && optShow opts == Nothing && not (optFlat opts)
        file = fromMaybe (leanNamespace design ++ ".lean") (optOutput opts)
    hier <- if hierRun then Just <$> phase timer "hier" (loadHier errh timer bounds (optSearch opts) input inp) else return Nothing
    -- the modular files: each written as it is emitted, then checked in Lean with its mutants
    ck <- newChecker co hierRun file
    let onFile isTop f em muts = do
            paths <- writeEmitted timer f em
            when lean $ checkModule ck isTop paths nmut muts
    rc <- case () of
      _ | Just h <- hier, optGate opts -> hierGate h >>= ok >>= mapM_ putStrLn >> return 0
        | Just h <- hier -> do
            (notes, stop) <- hierEmit h file (not (optNoTasks opts)) (optFirstRules opts) nmut
                                 (\hf -> onFile (hfTop hf) (hfFile hf) (hfEmitted hf) (hfMutants hf))
            mapM_ putStrLn notes
            rc' <- finishChecker ck
            forM_ stop (\msg -> hFlush stdout >> die 1 msg)
            return rc'
        | Just "" <- optShow opts -> do
            ls <- phase timer "emit" $ evaluate (force (dumpDesign design cands facts))
            phase timer "write" $ mapM_ putStrLn ls
            return 0
        | Just what <- optShow opts -> showView inp what >>= ok >>= phase timer "write" . mapM_ putStrLn >> return 0
        | optGate opts -> gateReport inp >>= ok >>= mapM_ putStrLn >> return 0
        | optFlat opts -> do
            proof <- phase timer "analyse" $ evaluate (analyse design facts)
            case pfErrors proof of
                [] -> return ()
                es -> die 1 (intercalate "; " es)
            text <- phase timer "emit" $ let t = emitLean design proof in evaluate (length t) >> pure t
            phase timer "write" $ withFile file WriteMode $ \h -> hSetEncoding h utf8 >> hPutStr h text
            forM_ (dWarnings design ++ pfWarnings proof) $ \w ->
                hPutStrLn stderr ("schedlean: warning: " ++ w)
            putStrLn ("schedlean: wrote " ++ file)
            if not lean then return 0 else do
                c <- setupLean co False >>= either (die cannotRunCode) return
                r <- checkFiles co c [file]
                reportCheck file r
                return (verdictCode (rVerdict (fst r)))
        | otherwise -> do
            (em, muts) <- emitModule inp (not (optNoTasks opts)) (optFirstRules opts) file nmut >>= ok
            onFile True file em muts
            finishChecker ck
    reportTotals timer
    when (rc /= 0) $ exitWith (ExitFailure rc)
