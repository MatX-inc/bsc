-- Bluesim code generation and artifact-link orchestration.
module SimLink(simLinkPlan) where

import Control.Monad(when, unless, liftM)
import Data.List(nub, partition, isSuffixOf)
import qualified Data.Map as M
import System.Directory(getCurrentDirectory)

import ABinUtil(ABIHierarchy)
import Backend(Backend(..))
import qualified BuildPlan as BP
import DependencyArtifacts(ArtifactActions(..), ArtifactResult(..), artifactPlan)
import Error(ErrorHandle, ErrMsg(..), EMsgs(..), bsError, bsMessage)
import FileIOUtil(writeFileCatch)
import FileNameUtil(hasDotSuf, dropSuf, dirName, mkNameWithoutSuffix, mkObjName,
                    hSuffix, cSuffix, cxxSuffix, cppSuffix, ccSuffix, objSuffix,
                    genFileName, createEncodedFullFilePath,
                    getFullFilePath, getRelativeFilePath)
import Flags(Flags(..), DumpFlag(..), quiet)
import ForeignFunctions(mkImportDeclarations)
import IOUtil(getEnvDef)
import NativeCompile(compileBluesimCFile, compileUserCFile,
                     compileParallelCFiles, cxxLink, missingUserFiles)
import PFPrint
import Position(cmdPosition)
import SimBlocksToC(simBlocksToC)
import SimCCBlock
import SimCOpt(simCOpt)
import SimExpand(simExpand)
import SimFileUtils(analyzeBluesimDependencies)
import SimMakeCBlocks(simMakeCBlocks)
import SimPackage(SimSystem(..))
import SimPackageOpt(simPackageOpt)
import SystemCWrapper(checkSystemCIfc, wrapSystemC)
import TopUtils

-- might as well inline this into simLink (no need to be separate)
genModuleC :: ErrorHandle
           -> Flags
           -> DumpNames
           -> TimeInfo
           -> String
           -> ABIHierarchy
           -> BP.BuildPlan (TimeInfo, [String], [String], TimeInfo)
genModuleC errh flags dumpnames time0 toplevel hierarchy = do
    (prefix, sim_system, time) <- BP.produce "Bluesim expansion" $ do
       pwd <- getCurrentDirectory
       let name = createEncodedFullFilePath "placeholder" pwd
           prefix = (dirName name) ++ "/"

       -- create SimSystem which contains SimPackages and SimSchedules
       start flags DFsimExpand
       sim_system <- simExpand errh flags toplevel hierarchy
       time <- dump errh flags time0 DFsimExpand dumpnames sim_system
       return (prefix, sim_system, time)

    -- Reuse is a read/choice plan between expansion and optimization, sharing
    -- its per-artifact input contract with conservative dependency discovery.
    BP.perform $ start flags DFsimDepend
    reused <- analyzeBluesimDependencies flags sim_system
    BP.produce "Bluesim C++ generation" $ do
       time <- dump errh flags time DFsimDepend dumpnames reused

       -- optimize the SimPackages and SimSchedules
       start flags DFsimPackageOpt
       sim_system_opt <- simPackageOpt errh flags sim_system
       time <- dump errh flags time DFsimPackageOpt dumpnames sim_system_opt

       -- convert SimPackages and SimSchedules to SimCCBlocks and SimCCScheds
       start flags DFsimMakeCBlocks
       let (simblocks, simCCscheds, clk_groups, gate_info, top_id) =
                simMakeCBlocks flags sim_system_opt
       let simBlockInfo = (simblocks, simCCscheds, clk_groups, gate_info)
       time <- dump errh flags time DFsimMakeCBlocks dumpnames simBlockInfo

       -- optimize the SimCCBlocks and SimCCScheds
       start flags DFsimCOpt
       let (simblocks_opt, simCCscheds_opt, clk_groups_opt, gate_info_opt) =
               simCOpt flags
                       (ssys_instmap sim_system_opt)
                       (simblocks, simCCscheds, clk_groups, gate_info)
       let simBlockInfo = (simblocks_opt, simCCscheds_opt, clk_groups_opt, gate_info_opt)
       time <- dump errh flags time DFsimCOpt dumpnames simBlockInfo

       -- get the map of ForeignFunctions
       let ff_map = ssys_ffuncmap sim_system_opt

       blurb <- mkGenFileHeader flags
       let mkEncodedName s = genFileName mkNameWithoutSuffix (cdir flags) prefix s
           -- write CCSyntax to file and return the relative file name
           writeFileC :: String -> String -> IO String
           writeFileC name file = do
             t_start <- getNow
             start flags DFwriteC
             encoded_name <- mkEncodedName name
             let full_name = getFullFilePath encoded_name
                 rel_name = getRelativeFilePath encoded_name
             writeFileCatch errh full_name ((commentC blurb) ++ file)
             _<- dumpStr errh flags t_start DFwriteC dumpnames name
             return rel_name

       -- convert SimCCBlocks and SimCCScheds to CCSyntax
       start flags DFsimBlocksToC
       let sb_map = M.fromList $ map (\sb -> (sb_id sb,sb))
                                     (simblocks_opt ++ primBlocks)
           creation_time = time

       block_names <- simBlocksToC flags
                                   creation_time
                                   top_id
                                   (ssys_default_clk sim_system_opt)
                                   (ssys_default_rst sim_system_opt)
                                   sb_map
                                   ff_map
                                   reused
                                   simblocks_opt
                                   simCCscheds_opt
                                   clk_groups_opt
                                   gate_info_opt
                                   writeFileC

       -- generate a header with imported function declarations
       let import_header =
             if M.null ff_map
             then []
             else [("imported_BDPI_functions.h",
                    ppReadable (mkImportDeclarations ff_map))]

       imp_names <- mapM (uncurry writeFileC) import_header

       let core_names = imp_names ++ block_names

       time <- dumpStr errh flags time DFsimBlocksToC dumpnames (unlines core_names)

       -- if generation target is SystemC, generate the wrapper file too
       start flags DFgenSystemC
       when (genSysC flags) $
            do bsMessage errh [(cmdPosition, MRestrictions "creating SystemC models" "the -systemc option")]
               checkSystemCIfc errh flags sim_system_opt
       sysc_files <- if (genSysC flags)
                     then wrapSystemC flags sim_system_opt
                     else return []
       time <- dump errh flags time DFgenSystemC dumpnames sysc_files

       sysc_names <- mapM (uncurry writeFileC) sysc_files

       let names = core_names ++ sysc_names

       reused_names <- mapM (\s -> do let cxx = mkObjName Nothing "" s
                                      n <- mkEncodedName cxx
                                      return $ getRelativeFilePath n)
                            reused

       -- XXX return the headers separate from the files which need to be
       -- XXX compiled
       return (time, names, reused_names, creation_time)

-- Plan construction is pure. Timing and generated filenames are ordinary
-- stage results, available only to execution and passed explicitly onward.
simLinkPlan :: ErrorHandle -> Flags -> String -> [String] -> [String] ->
               BP.BuildPlan ()
simLinkPlan errh flags toplevel afilenames cfilenames =
    let dumpnames = (Nothing, Nothing, Nothing)
        cfilenames_unique = nub cfilenames
        check = do
            tStart <- getNow
            missing <- missingUserFiles flags cfilenames_unique
            when (not (null missing)) $
                bsError errh [(cmdPosition, EMissingUserFile f ["."]) | f <- missing]
            t <- timestampStr flags "confirm C files exist" tStart
            start flags DFreadelab
            return (tStart, t)
        readInputs (tStart, t) abis = do
            t' <- dump errh flags t DFreadelab dumpnames (map fst abis)
            return (tStart, t')
        generate (tStart, t) _ ehierarchy = do
            hierarchy <- either
                (BP.produce "Bluesim hierarchy validation" . bsError errh . errmsgs)
                return ehierarchy
            result <- genModuleC errh flags dumpnames t toplevel hierarchy
            return (tStart, result)
        compile (tStart, (t, to_compile, to_reuse, creation_time)) = do
            ofiles_reused <- mapM (reuseBluesimCFile flags) to_reuse
            let (_, gen_cfiles) = partition (hasDotSuf hSuffix) to_compile
                (user_cfiles, user_ofiles) = partition
                    (\f -> hasDotSuf cSuffix f || hasDotSuf cxxSuffix f ||
                           hasDotSuf cppSuffix f || hasDotSuf ccSuffix f)
                    cfilenames_unique
            start flags DFbluesimcompile
            (gen_ofiles, compiled_user_ofiles) <-
                if parallelSimLink flags > 1
                then compileParallelCFiles errh flags False toplevel gen_cfiles user_cfiles
                else do
                    ofiles0 <- mapM (compileBluesimCFile errh flags) gen_cfiles
                    t' <- timestampStr flags "compile generated C++ files" t
                    ofiles1 <- mapM (compileUserCFile errh flags False) user_cfiles
                    _ <- timestampStr flags "compile user-provided C/C++ files" t'
                    return (ofiles0, ofiles1)
            let ofiles = gen_ofiles ++ user_ofiles ++ compiled_user_ofiles ++ ofiles_reused
            t' <- dump errh flags t DFbluesimcompile dumpnames ofiles
            return (tStart, t', ofiles, creation_time)
        link (tStart, t, ofiles, creation_time) = do
            start flags DFbluesimlink
            cxxLink errh flags toplevel ofiles creation_time
            t' <- dump errh flags t DFbluesimlink dumpnames toplevel
            return (tStart, t')
        finish result = do
            let (tStart, t) = case result of
                    AfterGeneration (started, (finished, _, _, _)) -> (started, finished)
                    AfterCompilation (started, finished, _, _) -> (started, finished)
                    AfterLink timing -> timing
            -- SystemC omits the native link, but the driver has always exposed
            -- its final diagnostic/stop stage after object compilation.
            when (genSysC flags) $ do
                start flags DFbluesimlink
                _ <- dump errh flags t DFbluesimlink dumpnames toplevel
                return ()
            _ <- timestampStr flags "total" tStart
            return ()
    in artifactPlan errh flags Bluesim toplevel afilenames [] cfilenames $
        ArtifactActions check readInputs generate compile link finish

-- Reuse a Bluesim generated object file
-- returns the name of the object file being reused
reuseBluesimCFile :: Flags -> String -> IO String
reuseBluesimCFile flags oName = do
    -- show is used for quoting
    systemc <- liftM show $ getEnvDef "SYSTEMC" ""
    let engine = if (genSysC flags) && ("_systemc" `isSuffixOf` (dropSuf oName))
                 then "SystemC"
                 else "Bluesim"
        oNameRel = getRelativeFilePath oName
    -- we lie here and mention both header and object (un-mangled name)
    let msg = engine ++ " object reused: " ++ (dropSuf oNameRel) ++
              ".{" ++ hSuffix ++ "," ++ objSuffix ++ "}"
    unless (quiet flags) $ putStrLnF msg
    return oName
