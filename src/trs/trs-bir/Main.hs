-- trs-bir: export one .ba's post-schedule IR as BIR.
--
-- bsc reaches this exact point when it links a Bluesim design: read a
-- .ba, expand it into a SimPackage, optimize it, and serialize the
-- result.  This tool is that path and nothing else, built from the
-- compiler's own modules so that the semantics come from reuse rather
-- than from a second implementation.
--
-- The read stops where elaboration stopped.  bsc builds a .ba out of
-- .bo files alone: it inlines up to a synthesis boundary and goes no
-- further, so a .ba stands on its own and exporting it needs no other.
-- bsc writes one per synthesized module and one per `import "BDPI"';
-- this writes a .bir per .ba, its flavor following the .ba's, and a
-- design is the set of them that `trs link --multi-fragments' puts
-- back together.
--
--     trs-bir sysTop.ba
--     trs-bir -o build/top.bir build/sysTop.ba
--
-- The command line is this program's own.  bsc's flag surface is a
-- compiler's -- source paths, code generators, optimizer dials, warning
-- policy -- and almost none of it reaches an export that starts from an
-- already-elaborated, already-scheduled design.  The few settings that
-- do reach it appear below as options of this program, so nothing here
-- depends on a flag surviving in bsc.
module Main(main) where

import Control.Monad(when)
import Data.Maybe(fromMaybe)
import qualified Data.Map as M
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import System.Exit(exitFailure, exitSuccess)
import System.IO(hSetBuffering, hSetEncoding, stdout, stderr, hPutStr,
                 hPutStrLn, BufferMode(..), utf8)

import ABin(ABin(..), ABinModInfo(..), ABinForeignFuncInfo(..))
import ABinUtil(readAndCheckABin, getABIName)
import Backend(Backend(..))
import Id(getIdString)
import ASyntax(apkg_name, apkg_state_instances, avi_vname)
import ASyntaxUtil(getForeignCallNames)
import Error(ErrorHandle, initErrorHandle, setErrorHandleFlags, exitOK)
import Exceptions(bsCatch)
import FileNameUtil(baseName)
import Flags(Flags(..))
import FlagsDecode(defaultFlags)
import IOUtil(getEnvDef)
import SimExpand(simExpandABin, simTopClockReset)
import SimExportIR(writeModuleBir, writeForeignBir)
import SimPackage(SimSystem(..), SimPackage(..))
import SimPackageOpt(simPackageOpt)
import TopUtils(dfltBluespecDir)
import Version(bscVersionStr)

-- ========================================================================
-- Command line
--

data Options = Options
    { optOut       :: Maybe FilePath
    , optVersion   :: Bool
    , optHelp      :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { optOut       = Nothing
    , optVersion   = False
    , optHelp      = False
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['o'] ["output"]
        (ReqArg (\d o -> o { optOut = Just d }) "FILE")
        "write the BIR here (default ./<name>.bir)"
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
        [ "Usage: " ++ prog ++ " [OPTION]... FILE.ba"
        , ""
        , "Export FILE.ba as BIR: one .ba in, one .bir out.  The .ba"
        , "holds a synthesized module or a foreign function, and the"
        , ".bir flavor follows whichever it is."
        , ""
        , "No other file is read.  bsc builds a .ba from .bo files"
        , "alone -- elaboration stops at a synthesis boundary and never"
        , "reads a child\'s .ba -- so a .ba stands on its own."
        , ""
        , "Options:"
        ]
    trailer = unlines
        [ ""
        , "By default the .bir goes to the working directory under the"
        , "name of the module or function the .ba holds, because that"
        , "is the name a link looks it up by.  -o is taken as given:"
        , "a fragment written under any other name is one a link will"
        , "not find on its own."
        , ""
        , "The file holds that one thing and nothing else: for a module,"
        , "neither the boundaries it instantiates, nor the signatures of"
        , "the imports it calls, nor anything design-level.  Export one"
        , "per .ba -- the set bsc wrote -- and hand them to `trs link"
        , "--multi-fragments\', which derives what the individual files"
        , "leave out."
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

    abinFile <- case rest of
        [f] -> return f
        []  -> die prog "no .ba file named\n"
        fs  -> die prog ("more than one .ba file named: "
                         ++ unwords fs ++ "\n")

    cdir <- getEnvDef "BLUESPECDIR" dfltBluespecDir
    let flags = birFlags cdir
    errh <- initErrorHandle
    setErrorHandleFlags errh flags
    exportBir errh flags opts abinFile
    exitOK errh

die :: String -> String -> IO a
die prog msg = do
    hPutStr stderr (prog ++ ": " ++ msg)
    hPutStr stderr (usage prog)
    exitFailure

-- | Give up on the export itself, as opposed to on the command line:
-- no usage text, because the command was well formed.
abort :: String -> IO a
abort msg = do
    prog <- getProgName
    hPutStrLn stderr (prog ++ ": " ++ msg)
    exitFailure

-- | The compiler settings the export actually reads.
--
-- Everything else keeps bsc's default.
birFlags :: String -> Flags
birFlags bluespecdir = (defaultFlags bluespecdir) { backend = Just Bluesim }

-- | The BIR export: one .ba in, one .bir out.
exportBir :: ErrorHandle -> Flags -> Options -> FilePath -> IO ()
exportBir errh flags opts abinFile = do
    -- Exactly the file named, and no other.  bsc built it from .bo
    -- files alone -- elaboration stops at a synthesis boundary and
    -- never reads a child's .ba -- and this reads it the same way.
    -- What a module instantiates and which imports it calls are
    -- recorded in its own APackage as names; putting the pieces
    -- together is the link's job.
    (_, abin) <- readAndCheckABin errh (Just Bluesim) abinFile

    -- Named for what the file holds, not for what the file is called:
    -- a link looks a fragment up by the name of the thing inside it.
    let name = getIdString (getABIName abin)
        birfile = fromMaybe (name ++ ".bir") (optOut opts)

    -- One .ba, one .bir, the flavor of the second following the first.
    case abin of
      ABinForeignFunc ffi _ ->
          writeForeignBir birfile name (abffi_foreign_func ffi)
      ABinMod modinfo ver -> do
        -- Whether a module's fire signals were kept is that module's
        -- own setting, asked for when it was elaborated.
        let keep = keepFires (abmi_flags modinfo)
            flags' = flags { keepFires = keep }
            apkg = abmi_apkg modinfo

        simpkg <- simExpandABin errh flags' (modinfo, ver)

        -- The default clock and reset are this module's own pragmas.
        -- bsc derives them for whichever module an export is rooted at,
        -- which for a fragment is always this one; the link reads them
        -- off whichever fragment the assembled design is topped by.
        let (top_clk, top_rst) = simTopClockReset (abmi_pps modinfo)

        -- ssys_schedules, ssys_instmap and ssys_filemap all describe a
        -- whole hierarchy, which a fragment does not have and the
        -- exported IR does not carry.  ssys_ffuncmap would hold the
        -- signatures of the imports this module calls, which are files
        -- of their own.  simPackageOpt rewrites only the packages.
        let ssys = SimSystem
                     { ssys_packages    = M.singleton (sp_name simpkg) simpkg
                     , ssys_schedules   = []
                     , ssys_top         = apkg_name apkg
                     , ssys_instmap     = M.empty
                     , ssys_ffuncmap    = M.empty
                     , ssys_filemap     = M.empty
                     , ssys_default_clk = top_clk
                     , ssys_default_rst = top_rst
                     }
        sim_system_opt <- simPackageOpt errh flags' ssys

        -- The order this module elaborated its instances in.  A
        -- SimPackage keys its instances by name and so loses it, but
        -- the primitives' ticks accumulate in this order, so it is read
        -- back off the APackage the .ba carries.
        let elabs = map avi_vname (apkg_state_instances apkg)

        writeModuleBir birfile keep elabs (getForeignCallNames apkg)
                       name sim_system_opt
      ABinModSchedErr _ _ ->
          abort (abinFile ++ ": `" ++ name ++ "' failed to schedule when \
                 \it was compiled; bsc reports why")
