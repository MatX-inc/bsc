-- Verilog artifact-link orchestration and simulator invocation.
module VerilogLink(vLinkPlan) where

import Control.Concurrent(forkIO)
import Control.Concurrent.MVar(newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as CE
import Control.Monad(when, unless)
import Control.Monad.Except(runExceptT)
import Data.Char(isSpace)
import Data.List(nub, partition, sort, isPrefixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Directory(getDirectoryContents, doesFileExist, getCurrentDirectory)
import System.Exit(ExitCode(ExitFailure, ExitSuccess))
import System.FilePath(takeDirectory)
import System.IO(hGetContents, hClose)
import System.Posix.Files(fileAccess)
import System.Process(runInteractiveProcess, waitForProcess, system)

import ABin(ABin(..), ABinForeignFuncInfo(..))
import ABinUtil(assertNoSchedErr)
import Backend(Backend(..))
import qualified BuildPlan as BP
import DependencyArtifacts(ArtifactActions(..), ArtifactResult(..), artifactPlan)
import Error(ErrorHandle, ErrMsg(..), internalError, bsError, bsWarning, exitFailWith)
import FileIOUtil(readFilePath)
import FileNameUtil(hasDotSuf, dirName, mkVPICName,
                    cSuffix, cxxSuffix, cppSuffix, ccSuffix,
                    createEncodedFullFilePath)
import Flags(Flags(..), DumpFlag(..), verbose, quiet)
import ForeignFunctions(ForeignFunction(..))
import Id(getIdString)
import IOUtil(getEnvDef)
import NativeCompile(compileVPICFile, compileUserCFile, missingUserFiles)
import PFPrint
import Position(noPosition, cmdPosition)
import TopUtils
import Util(joinByFst)
import VFileName
import VPIWrappers(genVPIRegistrationArray)

vLinkPlan :: ErrorHandle -> Flags -> String -> [VFileName] -> [String] -> [String] ->
             BP.BuildPlan ()
vLinkPlan errh flags topmod_name vfilenames afilenames cfilenames =
    let dumpnames = (Nothing, Nothing, Nothing)
        cfilenames_unique = nub cfilenames
        check = do
            tStart <- getNow
            pwd <- getCurrentDirectory
            let name = createEncodedFullFilePath "placeholder" pwd
                prefix = dirName name ++ "/"
            missing <- missingUserFiles flags cfilenames_unique
            when (not (null missing)) $
                bsError errh [(cmdPosition, EMissingUserFile f ["."]) | f <- missing]
            t <- timestampStr flags "confirm C files exist" tStart
            start flags DFreadelab
            return (tStart, t, prefix)
        generate (tStart, t, prefix) user_abis mhier0 = BP.produce "Verilog artifact preparation" $ do
            mhier <- case mhier0 of
                          Left msgs -> return (Left msgs)
                          Right (a, b, c, d, e, f, emodinfos) -> do
                              mres <- runExceptT (assertNoSchedErr emodinfos)
                              case mres of
                                Left msgs -> return (Left msgs)
                                Right modinfos -> return $
                                                  Right (a, b, c, d, e, f, modinfos)

            (ffuncs, mod_abmis) <-
                case (mhier) of
                  Left _ -> do
                    -- this design doesn't exist as .ba file
                    --traceM("Elaboration files not loaded for this design")
                    -- resort to what we know from the command line

                    when ((null user_abis) && (not (null cfilenames))) $
                         bsError errh [(cmdPosition, EVPIFilesWithNoABin cfilenames)]

                    -- identify which are ffunc and which are mods
                    let (ffunc_abis, mod_abis) =
                            let isFF (ABinForeignFunc {}) = True
                                isFF _                    = False
                            in  partition (isFF . snd) user_abis

                    -- confirm that there are no duplicate imports of
                    -- the same link name
                    let -- pair the ffunc name with the filename, for error reporting
                        ff_pairs =
                            [ (name, filename)
                            | (filename, a@(ABinForeignFunc {})) <- ffunc_abis
                            , let name = ff_name (abffi_foreign_func (ab_ffuncinfo a))
                            ]
                        ff_duplicates = filter ((>1) . length . snd) (joinByFst ff_pairs)
                    case (ff_duplicates) of
                      [] -> return ()
                      ((link_id, file_names):_) ->
                          let link_name = getIdString link_id
                          in  bsError errh
                                  [(cmdPosition,
                                    EMultipleABinFilesForName link_name file_names)]

                    -- XXX Until we allow re-generation of Verilog files,
                    -- XXX the module .ba files are unused
                    when (not (null (mod_abis))) $
                        bsWarning errh
                            [(cmdPosition, WExtraABinFiles (map fst mod_abis))]

                    let ffuncs = [ abffi_foreign_func abfi
                                   | (_, (ABinForeignFunc abfi _)) <- ffunc_abis ]
                        abmis  = [ (filename, abmi)
                                   | (filename, (ABinMod abmi _)) <- mod_abis ]
                    return (ffuncs, abmis)

                  Right (_, _, _, ffuncmap, filemap, _, mod_infos) -> do
                    --traceM("Elaboration files loaded for this design")
                    let user_ffuncs = M.elems ffuncmap
                        findModFilename m =
                            case (M.lookup m filemap) of
                              Just filename -> filename
                              Nothing -> internalError ("findModFilename: " ++
                                                        ppReadable m)
                        abmis = [ (findModFilename modname, abmi)
                                  | (modname, (abmi, _)) <- mod_infos ]
                    return (user_ffuncs, abmis)

            t <- dump errh flags t DFreadelab dumpnames
                     (map (pfpString . ff_name) ffuncs ++ map fst mod_abmis)

            return (tStart, t, prefix, ffuncs)
        compile (tStart, t, prefix, ffuncs) = do
            start flags DFcompileVPI
            (t', ofiles) <- vGenFFuncs errh flags t prefix cfilenames_unique ffuncs
            t'' <- dump errh flags t' DFcompileVPI dumpnames ofiles
            return (tStart, t'', prefix, ofiles)
        link (tStart, t, prefix, ofiles) = do
            start flags DFveriloglink
            vSimLink errh flags topmod_name prefix vfilenames ofiles
            t' <- dump errh flags t DFveriloglink dumpnames
                (map vfnString vfilenames ++ ofiles)
            return (tStart, t')
        finish result = do
            let tStart = case result of
                    AfterGeneration (started, _, _, _) -> started
                    AfterCompilation (started, _, _, _) -> started
                    AfterLink (started, _) -> started
            _ <- timestampStr flags "total" tStart
            return ()
    in artifactPlan errh flags Verilog topmod_name afilenames
        (map vfnString vfilenames) cfilenames $
        ArtifactActions check (\checked _ -> return checked) generate compile link finish

-- ===============

-- build a verilog simulator by calling bsc_build_vsim_simname with appropriate
-- arguments (where simname is the relevant simulator)
-- simname is determined from (in order of decreasing priority):
--   - the command-line flag -vsim
--   - the environment variable BSC_VERILOG_SIM
--   - any auto-detected simulator
vSimLink ::  ErrorHandle -> Flags ->
             String -> String -> [VFileName] -> [String] -> IO ()
vSimLink errh flags toplevel prefix vfiles ofiles = do
    build_script <- getVerilogSim errh flags
    let bsdir = bluespecDir flags
        libdirflags = map ("-L "++) (cLibPath flags)
        userlibs = map ("-l "++) (cLibs flags)
        macrodefs = map ("-D " ++) (defines flags)
        pathlibs = map ("-y "++) (vPath flags)
        outFile = oFile flags
        veriflags = map ("-Xv "++) (vFlags flags)
        linkerflags = map ("-Xl "++) (linkFlags flags)
        verboseflag = if (verbose flags) then ["-verbose"] else []
        dpiflag = if (useDPI flags) then ["-dpi"] else []
        args = (["link"
                , outFile
                , toplevel ] ++
                verboseflag ++
                dpiflag ++
                libdirflags ++
                userlibs ++
                linkerflags ++
                macrodefs ++
                pathlibs ++
                veriflags ++
                veriFiles bsdir ++
                (map vfnString vfiles) ++
                ofiles)
        cmd = unwords (build_script : args)
    when (verbose flags) $ putStrLnF ("exec: " ++ cmd)
    rc <- system cmd
    case rc of
        ExitSuccess -> unless (quiet flags) $ putStrLnF ("Verilog binary file created: " ++ outFile)
        ExitFailure n -> exitFailWith errh n

veriFiles :: String -> [String]
veriFiles path =
        map ((path ++ "/Verilog/") ++) ["main.v"]

-- return the path to a Verilog simulator or die with an error
getVerilogSim :: ErrorHandle -> Flags -> IO String
getVerilogSim errh flags = do
    vsim_env <- getEnvDef "BSC_VERILOG_SIM" ""
    let vsim_name_flags_or_env = maybe vsim_env id (vsim flags)
    vsim_name <- if vsim_name_flags_or_env == ""
                 then do avail_vsims <- findAllAvailableSims flags
                         if (null avail_vsims)
                          then return ""
                          else return $ head avail_vsims
                 else return vsim_name_flags_or_env
    when (vsim_name == "") $ give_error flags vsim_name
    let vsim_path | any (== '/') vsim_name = vsim_name
                  | otherwise = (bluespecDir flags ++ "/exec/bsc_build_vsim_" ++
                                 vsim_name)
    valid_path <- doesFileExist vsim_path
    when (not valid_path) $ give_error flags vsim_name
    isExec <- fileAccess vsim_path False False True
    when (not isExec) $ give_error flags vsim_name
    return vsim_path
  where give_error flags vsim_name =
            do avail_vsims <- findAllAvailableSims flags
               let no_sims_err = (cmdPosition, EUnknownVerilogSim vsim_name (sort avail_vsims) True)
               bsError errh [no_sims_err]

-- find all available simulators
findAllAvailableSims :: Flags -> IO [String]
findAllAvailableSims flags =
    do let script_dir = bluespecDir flags ++ "/exec"
       all_files <- getDirectoryContents script_dir
       let sim_scripts = filter ("bsc_build_vsim_" `isPrefixOf`) all_files
       sim_script_results <- mapM (checkSimScript script_dir) sim_scripts
       let successful_sim_scripts = map snd (filter fst sim_script_results)
       return (nub successful_sim_scripts)
    `CE.catch` return_empty_list
    where
    return_empty_list :: CE.SomeException -> IO [String]
    return_empty_list e = return []

-- XXX: replace this implementation with one using readProcessWithExitCode
-- XXX: once everyone is using GHC >= 6.10.
checkSimScript :: String -> String -> IO (Bool,String)
checkSimScript dir script =
    do (hin, hout, _, pid) <- runInteractiveProcess (dir ++ "/" ++ script) ["detect"] Nothing Nothing
       outMVar <- newEmptyMVar
       out <- hGetContents hout
       _ <- forkIO $ CE.evaluate (length out) >> putMVar outMVar ()
       hClose hin
       takeMVar outMVar
       hClose hout
       status <- waitForProcess pid
       let canonical_sim_name = takeWhile (not.isSpace) out
       return ((status == ExitSuccess),canonical_sim_name)

-- ===============

vGenFFuncs :: ErrorHandle -> Flags -> TimeInfo -> String ->
              [String] -> [ForeignFunction] ->
              IO (TimeInfo, [String])
vGenFFuncs errh flags t prefix cfilenames_unique [] = return (t,[])
vGenFFuncs errh flags t prefix cfilenames_unique ffuncs = do
      (t, vpiarray_filenames) <-
        if (useDPI flags) then return (t, [])
        else do
          -- generate the vpi_startup_array file
          blurb <- mkGenFileHeader flags
          filenames <- genVPIRegistrationArray errh flags prefix blurb ffuncs
          t <- timestampStr flags "generate VPI registration array" t
          return (t, filenames)

      -- compile user-supplied C files
      let (cfiles1, ofiles1) = partition (\f -> hasDotSuf cSuffix f   ||
                                                hasDotSuf cxxSuffix f ||
                                                hasDotSuf cppSuffix f ||
                                                hasDotSuf ccSuffix f
                                         )
                                         cfilenames_unique
      ofiles2 <- mapM (compileUserCFile errh flags True) cfiles1
      t <- timestampStr flags "compile user-provided C files" t

      (t, ofiles3) <-
        if (useDPI flags) then return (t, [])
        else do
          -- compile all necessary vpi wrapper files

          -- first, find the VPI wrapper files in the vsearch path
          let findVPIWrapperFile ffunc = do
                let ffunc_name = getIdString (ff_name ffunc)
                    vpiwrapper_filename = mkVPICName Nothing "" ffunc_name
                mfile <- readFilePath errh noPosition False vpiwrapper_filename (vPath flags)
                case mfile of
                  Nothing -> bsError errh [(noPosition, EMissingVPIWrapperFile vpiwrapper_filename False)]
                  Just (_, filename) -> return filename
          vpiwrapper_filenames <- mapM findVPIWrapperFile ffuncs

          -- collect the directories of the VPI wrapper files
          -- to use as a search path for header files when compiling
          -- the VPI registration array file
          let vpidirs = S.toList (S.fromList (map takeDirectory vpiwrapper_filenames))

          -- include the vpi registration array file
          wrapper_files <- mapM (compileVPICFile errh flags []) vpiwrapper_filenames
          array_files <- mapM (compileVPICFile errh flags vpidirs) vpiarray_filenames
          let files = wrapper_files ++ array_files
          t <- timestampStr flags "compile VPI wrapper files" t
          return (t, files)

      return (t, ofiles1 ++ ofiles2 ++ ofiles3)

-- ===============

{-
vGenMods :: TimeInfo -> Flags -> [(String, ABinModInfo)] ->
            IO (TimeInfo, [VFileName])
vGenMods t0 flags abmis = do
    -- function to generate an individual module
    let genV (t, vfilenames_so_far) (filename, abmi@(ABinModInfo {})) = do
            let modId = apkg_name (abmi_apkg abmi)
                modstr = getIdString (unQualId modId)
            -- XXX should the file and package name be set?
            let dumpnames = (Nothing, Nothing, Just modstr)
            -- verbose message
            when (verbose flags) $ putStrLnF ("*****")
            when (showCodeGen flags || verbose flags) $
                putStrLnF ("Verilog generation for " ++ modstr ++ " starts")
            -- prepare directory info
            pwd <- getCurrentDirectory
            let filename' = createEncodedFullFilePath filename pwd
                prefix = dirName filename' ++ "/"
            -- call into the regular flow
            blurb <- mkGenFileHeader flags
            let apkg = abmi_apkg abmi
                pps = abmi_pps abmi
                methodConflict = abmi_method_dump abmi
                methodConflictBlurb
                  | methodConf flags =
                      ["Method conflict info:"]
                      ++ lines (pretty 78 78
                                   (vcat (dumpMethodInfo flags methodConflict)))
                  | otherwise = []
                pathinfo = abmi_pathinfo abmi
                aschedinfo = abmi_aschedinfo abmi
            (t, _, _) <-
                genModuleVerilog pps flags dumpnames t prefix modstr
                    blurb methodConflictBlurb pathinfo aschedinfo apkg
            -- result
            return (t, vfilenames ++ vfilenames_so_far)

    -- generate the Verilog files
    foldM genV (t0,[]) abmis
-}
