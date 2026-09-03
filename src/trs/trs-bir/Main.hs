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
import SimCCBlock(SimCCBlock(..), primBlocks)
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
import SimCCBlock
import SimCOpt(simCOpt)
import SimExpand(simExpandWith)
import SimExportIR(writeBirFile, writeSchedFile, Scope(..))
import SimMakeCBlocks(simMakeCBlocks)
import SimPackage(SimSystem(..))
import SimPackageOpt(simPackageOpt)
import TopUtils(dfltBluespecDir)
import TrsTopLevel(checkTrsTop)
import Version(bscVersionStr)

-- ========================================================================
-- Command line
--

data Options = Options
    { optOut       :: Maybe FilePath
    , optPath      :: [FilePath]
    , optBdpi      :: [FilePath]
    , optLibPath   :: [String]
    , optLibs      :: [String]
    , optDumpSched :: Maybe FilePath
    , optFragment  :: Bool
    , optVerbose   :: Bool
    , optVersion   :: Bool
    , optHelp      :: Bool
    }

defaultOptions :: Options
defaultOptions = Options
    { optOut       = Nothing
    , optPath      = []
    , optBdpi      = []
    , optLibPath   = []
    , optLibs      = []
    , optDumpSched = Nothing
    , optFragment  = False
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
    , Option []    ["bdpi"]
        (ReqArg (\d o -> o { optBdpi = optBdpi o ++ [d] }) "FILE")
        "a BDPI implementation: .c/.cxx compiled here, .o/.a taken as is"
    , Option ['L'] []
        (ReqArg (\d o -> o { optLibPath = optLibPath o ++ [d] }) "DIR")
        "search DIR for BDPI libraries (repeatable)"
    , Option ['l'] []
        (ReqArg (\d o -> o { optLibs = optLibs o ++ [d] }) "LIB")
        "link LIB into the BDPI companion (repeatable)"
    , Option []    ["single-fragment"]
        (NoArg (\o -> o { optFragment = True }))
        "write <top> alone: no submodules, no design-level data"
    , Option []    ["dump-schedule"]
        (ReqArg (\d o -> o { optDumpSched = Just d }) "FILE")
        "also write the merged design schedule here, for inspection"
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
        , "Given --bdpi or -l, a <top>.bdpi.so companion is written beside"
        , "the BIR for the runtime to load.  CC and CXX name the compilers."
        , ""
        , "With --single-fragment the file holds <top> and nothing else:"
        , "neither the synthesized modules it instantiates nor the"
        , "design-level data.  Export one per synthesized module -- the"
        , "same set bsc wrote a .ba for -- and hand them to `trs link"
        , "--multi-fragments', which derives what the fragments leave out."
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
    -- The passes below take one Flags for the whole run, so only the
    -- modules this export WRITES get a say.  A fragment writes one, so
    -- that one decides and nothing leaks in from the subtree walked
    -- around it.  A whole-design export writes them all and one Flags
    -- cannot say different things about different modules, so it keeps
    -- whatever any of them asked for; a module that did not ask still
    -- carries its own answer in the BIR, so nothing gives it waveforms
    -- it did not want.
    let keepMap = M.fromList [ (nm, keepFires (abemi_flags emi))
                             | (nm, (emi, _)) <- emis ]
        scope = if optFragment opts then Fragment toplevel else WholeDesign
        flags' = flags
                   { keepFires = case scope of
                       Fragment m  -> M.findWithDefault False m keepMap
                       WholeDesign -> or (M.elems keepMap) }

    sim_system <- simExpandWith errh flags' (checkTrsTop errh) toplevel abis
    sim_system_opt <- simPackageOpt errh flags' sim_system

    -- The debug-tier symbol set: the defs that survive as C++ members,
    -- which is what names a signal in an interactive session.  Deriving
    -- it means running the C++ block construction, but only for its
    -- answer -- the blocks themselves are discarded.
    let (sbs, sscheds, scgs, sgis, _sbtop) = simMakeCBlocks flags' sim_system_opt
        (sbs_opt, _, _, _) =
            simCOpt flags' (ssys_instmap sim_system_opt)
                    (sbs, sscheds, scgs, sgis)
        symMap = M.fromListWith S.union
                     [ (sb_name sb,
                        S.fromList [ i | (_, i) <- sb_publicDefs sb, isOkId i ])
                     | sb <- sbs_opt ]

    -- The order each module elaborated its instances in.  A SimPackage
    -- keys its instances by name and so loses it, but the primitives'
    -- ticks accumulate in this order, so it is read back off the
    -- APackages the .ba files carry.
    let elabs = M.fromList
                  [ (nm, map avi_vname (apkg_state_instances (abmi_apkg mi)))
                  | (nm, (Right mi, _)) <- emis ]

    writeBirFile birfile keepMap symMap elabs scope sim_system_opt
    case optDumpSched opts of
      Nothing -> return ()
      Just p  -> writeSchedFile p keepMap symMap elabs sim_system_opt
    writeBdpiSo opts toplevel prefix sim_system_opt

-- | The companion the trs runtime dlopens for BDPI imports.
--
-- The runtime resolves each import out of <top>.bdpi.so beside the .bir,
-- so the implementations go into a shared object of their own rather
-- than into an executable.  Nothing inside that object references them.
-- An object file contributes its symbols anyway; an archive contributes
-- only the members something asks for, so when one is on the line every
-- foreign function the design calls is named with -u to hold it in.  The
-- hierarchy's own foreign-function map is the authority on which those
-- are.
writeBdpiSo :: Options -> String -> FilePath -> SimSystem -> IO ()
writeBdpiSo opts toplevel prefix ssys
    | null (optBdpi opts) && null (optLibs opts) = return ()
    | otherwise = do
        cxx <- getEnvDef "CXX" "c++"
        objs <- mapM (compileBdpi opts) (optBdpi opts)
        let so = prefix ++ toplevel ++ ".bdpi.so"
            undef | null (optLibs opts) = []
                  | otherwise =
                      [ "-Wl,-u," ++ asmName (getIdString (ff_name ff))
                      | ff <- M.elems (ssys_ffuncmap ssys) ]
        tool opts cxx $ ["-shared", "-fPIC"]
                        ++ map ("-L" ++) (optLibPath opts)
                        ++ undef ++ ["-o", so]
                        ++ objs ++ map ("-l" ++) (optLibs opts)

-- | A BDPI source becomes an object here; an object or archive is taken
-- as given.  The object lands in the working directory, beside the rest
-- of the output, rather than next to the source.
compileBdpi :: Options -> FilePath -> IO FilePath
compileBdpi opts f
    | ext `elem` [".o", ".a"] = return f
    | otherwise = do
        cc <- getEnvDef (if ext == ".c" then "CC" else "CXX")
                        (if ext == ".c" then "cc" else "c++")
        let o = replaceExtension (takeFileName f) ".bdpi.o"
        tool opts cc ["-fPIC", "-c", "-o", o, f]
        return o
  where ext = takeExtension f

-- | A C function's name as the linker spells it.  Mach-O prefixes an
-- underscore to every C symbol; ELF does not.  -u names a symbol for
-- the linker to demand, so it has to be the linker's spelling.
asmName :: String -> String
asmName n = if os == "darwin" then '_' : n else n

tool :: Options -> String -> [String] -> IO ()
tool opts prog args = do
    when (optVerbose opts) $ hPutStrLn stderr (unwords (prog : args))
    rc <- rawSystem prog args
    case rc of
        ExitSuccess -> return ()
        ExitFailure n -> do
            hPutStrLn stderr (prog ++ " exited " ++ show n)
            exitFailure
