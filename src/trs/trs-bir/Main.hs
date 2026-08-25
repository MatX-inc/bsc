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
import System.Exit(exitFailure, exitSuccess)
import System.IO(hSetBuffering, hSetEncoding, stdout, stderr, hPutStr,
                 BufferMode(..), utf8)

import ABinUtil(readAndCheckABin)
import Backend(Backend(..))
import Error(ErrorHandle, initErrorHandle, setErrorHandleFlags, exitOK)
import Exceptions(bsCatch)
import FileNameUtil(baseName, dirName, createEncodedFullFilePath)
import Flags(Flags(..), Verbosity(..))
import FlagsDecode(defaultFlags)
import IOUtil(getEnvDef)
import SimCCBlock
import SimCOpt(simCOpt)
import SimExpand(simExpand)
import SimExportIR(writeBirFile)
import SimMakeCBlocks(simMakeCBlocks)
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
    , optKeepFires :: Bool
    , optVerbose   :: Bool
    , optVersion   :: Bool
    , optHelp      :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { optOut       = Nothing
    , optPath      = []
    , optKeepFires = False
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
    , Option []    ["keep-fires"]
        (NoArg (\o -> o { optKeepFires = True }))
        "export CAN_FIRE/WILL_FIRE definitions"
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
    exportBir errh flags (optOut opts) toplevel abinFiles
    exitOK errh

die :: String -> String -> IO a
die prog msg = do
    hPutStr stderr (prog ++ ": " ++ msg)
    hPutStr stderr (usage prog)
    exitFailure

-- | The compiler settings the export actually reads.
--
-- Everything else keeps bsc's default.  Two of these are a seam rather
-- than a setting: genTrs and genBir pick the trs backend's answers in
-- SimExpand, which refuses a narrower set of top-level modules for the
-- C++ backend than for this one.  Those checks belong to this program.
birFlags :: String -> Options -> Flags
birFlags bluespecdir opts = (defaultFlags bluespecdir)
    { backend   = Just Bluesim
    , ifcPath   = optPath opts ++ [".", bluespecdir ++ "/Libraries"]
    , keepFires = optKeepFires opts
    , verbosity = if optVerbose opts then Verbose else Normal
    , genTrs    = True
    , genBir    = True
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
exportBir :: ErrorHandle -> Flags -> Maybe FilePath -> String -> [String]
          -> IO ()
exportBir errh flags out toplevel afilenames = do
    pwd <- getCurrentDirectory
    let prefix = dirName (createEncodedFullFilePath "placeholder" pwd) ++ "/"
        birfile = fromMaybe (prefix ++ toplevel ++ ".bir") out

    -- the same file twice on the command line is not an error; two .ba
    -- for one module is, and simExpand is what catches it
    abis <- mapM (readAndCheckABin errh (Just Bluesim)) (nub afilenames)

    sim_system <- simExpand errh flags toplevel abis
    sim_system_opt <- simPackageOpt errh flags sim_system

    -- The debug-tier symbol set: the defs that survive as C++ members,
    -- which is what names a signal in an interactive session.  Deriving
    -- it means running the C++ block construction, but only for its
    -- answer -- the blocks themselves are discarded.
    let (sbs, sscheds, scgs, sgis, _sbtop) = simMakeCBlocks flags sim_system_opt
        (sbs_opt, _, _, _) =
            simCOpt flags (ssys_instmap sim_system_opt)
                    (sbs, sscheds, scgs, sgis)
        symMap = M.fromListWith S.union
                     [ (sb_name sb,
                        S.fromList [ i | (_, i) <- sb_publicDefs sb, isOkId i ])
                     | sb <- sbs_opt ]

    writeBirFile birfile (keepFires flags) symMap sim_system_opt
