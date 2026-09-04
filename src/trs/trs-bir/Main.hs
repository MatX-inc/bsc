-- trs-bir: export a Bluespec design's post-schedule IR as BIR.
--
-- bsc already reaches this exact point when it links a Bluesim design:
-- read the .ba hierarchy, expand it into a SimSystem, optimize the
-- packages, and serialize the result.  This tool is that path and
-- nothing else, built from the compiler's own modules so that the
-- semantics come from reuse rather than from a second implementation.
--
--     trs-bir sysTop
--     trs-bir -p build -o top.bir sysTop
--
-- The command line is this program's own.  bsc's flag surface is a
-- compiler's -- source paths, code generators, optimizer dials, warning
-- policy -- and almost none of it reaches an export that starts from an
-- already-elaborated, already-scheduled design.  The few settings that
-- do reach it appear below as options of this program, so nothing here
-- depends on a flag surviving in bsc.
module Main(main) where

import Control.Monad(when)
import Data.List(nub, partition, isSuffixOf)
import Data.Maybe(fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Console.GetOpt
import System.Directory(getCurrentDirectory)
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess, ExitCode(..))
import System.FilePath(takeExtension, takeFileName, replaceExtension)
import System.Info(os)
import System.Process(rawSystem)
import System.IO(hSetBuffering, hSetEncoding, stdout, stderr, hPutStr,
                 hPutStrLn, BufferMode(..), utf8)

import ABin(abemi_flags)
import ABinUtil(readAndCheckABin, getABIHierarchy)
import Backend(Backend(..))
import SimCCBlock(sb_name, primBlocks)
import ForeignFunctions(ForeignFunction(..))
import Id(getIdString)
import ASyntax(apkg_state_instances, avi_vname)
import ABin(abmi_apkg)
import Error(ErrorHandle, initErrorHandle, setErrorHandleFlags, exitOK,
             convExceptTToIO)
import Exceptions(bsCatch)
import FileNameUtil(baseName, dirName, createEncodedFullFilePath)
import Flags(Flags(..), Verbosity(..), verbose)
import FlagsDecode(defaultFlags)
import IOUtil(getEnvDef)
import SimExpand(simExpandWith)
import SimExportIR(writeBirFile)
import SimPackage(SimSystem(..))
import SimPackageOpt(simPackageOpt)
import TopUtils(dfltBluespecDir)
import Version(bscVersionStr)

-- ========================================================================
-- Command line
--

data Options = Options
    { optOut       :: Maybe FilePath
    , optPath      :: [FilePath]
    , optVerbose   :: Bool
    , optVersion   :: Bool
    , optHelp      :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { optOut       = Nothing
    , optPath      = []
    , optVerbose   = False
    , optVersion   = False
    , optHelp      = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['o'] ["output"]
        (ReqArg (\d o -> o { optOut = Just d }) "FILE")
        "write the BIR here (default <top>.bir)"
    , Option ['p'] ["path"]
        (ReqArg (\d o -> o { optPath = optPath o ++ [d] }) "DIR")
        "search DIR for .ba files (repeatable)"
    , Option ['v'] ["verbose"]
        (NoArg (\o -> o { optVerbose = True }))
        "report progress while reading the hierarchy"
    , Option []    ["version"]
        (NoArg (\o -> o { optVersion = True }))
        "print the compiler build this program exports for"
    , Option ['h'] ["help"]
        (NoArg (\o -> o { optHelp = True }))
        "print this message"
    ]

usage :: String -> String
usage prog = usageInfo header options ++ trailer
  where
    header = unlines
        [ "Usage: " ++ prog ++ " [OPTION]... <top> [FILE.ba]..."
        , ""
        , "Export the design rooted at the module <top> as BIR."
        , ""
        , "Any .ba files named on the command line are read directly;"
        , "the rest of the hierarchy is found on the search path."
        , ""
        , "Options:"
        ]
    trailer = unlines
        [ ""
        , "The search path ends with the working directory and the"
        , "libraries under $BLUESPECDIR, in that order."
        , ""
        , "The file holds <top> and nothing else: neither the synthesized"
        , "modules it instantiates nor anything design-level.  Export one"
        , "per synthesized module -- the same set bsc wrote a .ba for --"
        , "and hand them to `trs link --multi-fragments', which derives"
        , "what the individual files leave out."
        ]

-- ========================================================================

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    args <- getArgs
    bsCatch (hmain args)

hmain :: [String] -> IO ()
hmain argv = do
    prog <- fmap baseName getProgName
    (opts, rest) <- case getOpt Permute options argv of
        (fs, rest, []) -> return (foldl (flip id) defaultOptions fs, rest)
        (_, _, errs)   -> die prog (concat errs)

    when (optHelp opts) $ do
        putStr (usage prog)
        exitSuccess
    when (optVersion opts) $ do
        -- A .ba names the compiler build that wrote it and this program
        -- reads no other, so the caller can compare this against bsc's
        -- own -v before blaming the export for a version refusal.
        putStrLn (bscVersionStr True)
        exitSuccess

    (toplevel, abinFiles) <- case partition (".ba" `isSuffixOf`) rest of
        (bas, [top]) -> return (top, bas)
        (_, [])      -> die prog "no top-level module named\n"
        (_, tops)    -> die prog ("more than one top-level module named: "
                                  ++ unwords tops ++ "\n")

    cdir <- getEnvDef "BLUESPECDIR" dfltBluespecDir
    let flags = birFlags cdir opts
    errh <- initErrorHandle
    setErrorHandleFlags errh flags
    exportBir errh flags opts toplevel abinFiles
    exitOK errh

die :: String -> String -> IO a
die prog msg = do
    hPutStr stderr (prog ++ ": " ++ msg)
    hPutStr stderr (usage prog)
    exitFailure

-- | The compiler settings the export actually reads.
--
-- Everything else keeps bsc's default.
birFlags :: String -> Options -> Flags
birFlags bluespecdir opts = (defaultFlags bluespecdir)
    { backend   = Just Bluesim
    , ifcPath   = optPath opts ++ [".", bluespecdir ++ "/Libraries"]
    , verbosity = if optVerbose opts then Verbose else Normal
    }

-- | The BIR export, following bsc's genModuleC through the point where
-- it writes the .bir.
--
-- Two steps of that function have no counterpart here.  bsc refuses a
-- dynamically scheduled design unless the trs backend was asked for --
-- a check with nothing to decide when the trs exporter *is* the program
-- being run.  And it analyzes which generated C++ objects a previous
-- link left reusable, which feeds the C++ code generator further down
-- and never reaches the exported IR.
exportBir :: ErrorHandle -> Flags -> Options -> String -> [String] -> IO ()
exportBir errh flags opts toplevel afilenames = do
    pwd <- getCurrentDirectory
    let prefix = dirName (createEncodedFullFilePath "placeholder" pwd) ++ "/"
        birfile = fromMaybe (prefix ++ toplevel ++ ".bir") (optOut opts)

    -- the same file twice on the command line is not an error; two .ba
    -- for one module is, and simExpand is what catches it
    abis <- mapM (readAndCheckABin errh (Just Bluesim)) (nub afilenames)

    -- The hierarchy, read before anything is derived from it: the
    -- .ba files carry the flags each module was elaborated with, and
    -- -keep-fires is one of them.
    (_, _, _, _, _, _, emis) <- convExceptTToIO errh $
        getABIHierarchy errh (verbose flags) (ifcPath flags) (Just Bluesim)
                        (map sb_name primBlocks) toplevel abis

    -- Whether a module's fire signals were kept is that module's own
    -- setting, asked for when it was elaborated, and it is written on
    -- the module.
    --
    -- The passes below take one Flags for the whole run, and this run
    -- writes one module, so that module decides.  Nothing leaks in
    -- from the subtree walked around it.
    let keepMap = M.fromList [ (nm, keepFires (abemi_flags emi))
                             | (nm, (emi, _)) <- emis ]
        flags' = flags
                   { keepFires =
                       M.findWithDefault False toplevel keepMap }

    -- No top-level check here.  This export writes one synthesis
    -- boundary, and whether a module can be the top of a running
    -- design is a question about the design -- `trs link` asks it of
    -- the one that turns out to be top (trs_interp::topbind).  A
    -- boundary bsc was willing to elaborate is one this will export.
    sim_system <- simExpandWith errh flags' (\_ -> return ()) toplevel abis
    sim_system_opt <- simPackageOpt errh flags' sim_system

    -- The order each module elaborated its instances in.  A SimPackage
    -- keys its instances by name and so loses it, but the primitives'
    -- ticks accumulate in this order, so it is read back off the
    -- APackages the .ba files carry.
    let elabs = M.fromList
                  [ (nm, map avi_vname (apkg_state_instances (abmi_apkg mi)))
                  | (nm, (Right mi, _)) <- emis ]

    writeBirFile birfile keepMap elabs toplevel sim_system_opt
