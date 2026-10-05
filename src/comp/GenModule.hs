-- | Elaboration, scheduling, and per-module artifact generation.
module GenModule (genModule) where

import Control.Monad (when, unless)
import Data.Char (isSpace)
import Data.Maybe (isJust, isNothing)
import qualified Data.Map as M
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.Process (system)

import PFPrint
import Util (quote)
import FileNameUtil (dropSuf, mkAName, mkVName, useSuffix, genFileName,
                     getFullFilePath, getRelativeFilePath)
import FileIOUtil (writeFileCatch)
import TopUtils
import Flags (Flags(..), DumpFlag(..), verbose, quiet)
import FlagsDecode (updateFlags)
import Error (internalError, ErrMsg(..), ErrorHandle, bsError, bsWarning, exitFail)
import Position (noPosition, cmdPosition, getPosition)
import CVPrint (CDefn, CQType)
import Id
import Backend (Backend(..))
import Pragma (PProp(..), isAlwaysRdy)
import VModInfo (VPathInfo, VPort)
import SymTab (SymTab)
import ISyntax (IModule(..), IATFCache, IEFace(..), IDef(..), IExpr)
import ISyntaxUtil (isTrue)
import InstNodes (getIStateLocs, flattenInstTree)
import ISyntaxCheck (tCheckIModule)
import GenWrap (WrapInfo(..))
import IExpand (iExpand)
import IExpandUtils (HeapData)
import ITransform (iTransform)
import IInline (iInline)
import IInlineFmt (iInlineFmt)
import Params (iParams)
import ASyntax (APackage(..), ASPackage, ppeAPackage, getAPackageFieldInfos)
import ASyntaxUtil (getForeignCallNames)
import ACheck (aMCheck, aSMCheck, aSignalCheck, aSMethCheck)
import AConv (aConv)
import IDropRules (iDropRules)
import ARankMethCalls (aRankMethCalls)
import AState (aState)
import ARenameIO (aRenameIO)
import ASchedule (AScheduleInfo(..), AScheduleErrInfo(..), aSchedule)
import AAddScheduleDefs (aAddScheduleDefs)
import APaths (aPathsPreSched, aPathsPostSched)
import AProofs (aCheckProofs)
import ADropDefs (aDropDefs)
import AOpt (aOpt)
import AVerilog (aVerilog)
import AVeriQuirks (aVeriQuirks)
import VIOProps (VIOProps, getIOProps)
import VFinalCleanup (finalCleanup)
import Synthesize (aSynthesize)
import ABin (ABin(..), ABinModInfo(..), ABinForeignFuncInfo(..), ABinModSchedErrInfo(..))
import ABinUtil (readAndCheckABinPathCatch)
import GenABin (genABinFile)
import ForeignFunctions (ForeignFunction(..), ForeignFuncMap)
import SimExpand (simCheckPackage)
import Verilog (VProgram, vGetMainModName, getVeriInsts)
import Version (bscVersionStr)
import ILift (iLift)
import ACleanup (aCleanup)
import ATaskSplice (aTaskSplice)
import ADumpSchedule (MethodDumpInfo, aDumpSchedule, aDumpScheduleErr,
                      dumpMethodInfo, dumpMethodBVIInfo)
import ANoInline (aNoInline)
import AAddSchedAssumps (aAddSchedAssumps, aAddCFConditionWires)
import ARemoveAssumps (aRemoveAssumps)
import ADropUndet (aDropUndet)
import InlineWires (aInlineWires)
import InlineCReg (aInlineCReg)
import LambdaCalc (convAPackageToLambdaCalc)
import SAL (convAPackageToSAL)
import VVerilogDollar (removeDollarsFromVerilog)
import ISplitIf (iSplitIf)
import VFileName (VFileName(..), vfnString)

genModule ::
    ErrorHandle ->
    WrapInfo ->
    Bool ->
    Flags ->
    DumpNames ->
    String -> -- prefix
    String -> -- source package name
    SymTab ->
    M.Map Id (IExpr HeapData) ->
    IATFCache ->
    IDef HeapData ->
    IO (CDefn)

-- [IDef HeapData], VSchedInfo, VPathInfo, VWireInfo, [VFieldInfo], [VPort])
genModule
    errh
    wi
    fwrapper
    flags0
    dumpnames
    prefix
    srcName
    symt
    alldefs
    atf_cache
    def  =

  do
    let pps = wi_prags wi
        def_pos = let (IDef i _ _ _) = def
                  in  getPosition i
    flags <- updateFlags errh def_pos [ s | PPoptions ss <- pps, s <- ss ] flags0

    let modstr = getIdString (unQualId (mod_nm wi))

    when (verbose flags) $ putStrLnF ("*****")
    when (showCodeGen flags || verbose flags) $ putStrLnF ("code generation for " ++ modstr ++ " starts")
    t <- getNow

    -- "run" it
    start flags DFexpanded
    imod0 <- iExpand errh flags symt alldefs atf_cache fwrapper pps def
    iMCheck flags symt imod0 "expanded"
    t <- dump errh flags t DFexpanded dumpnames imod0
    when (showIESyntax flags) (putStrLnF (show imod0))
    stats flags DFexpanded imod0

    progArgs <- getArgs
    when ("-trace-state-loc" `elem` progArgs) $ do
      let (inst_locs, rule_locs) = getIStateLocs imod0
      putStrLn "Instance state locs"
      putStr (ppReadable inst_locs)
      putStrLn "Rule state locs"
      putStr (ppReadable rule_locs)

    -- We no longer normalize types here, since the evaluator now normalizes as it goes.

    start flags DFinlineFmt
    imod_fmt <- iInlineFmt errh imod0
    iMCheck flags symt imod_fmt "Fmt inline"
    t <- dump errh flags t DFinlineFmt dumpnames imod_fmt
    stats flags DFinlineFmt imod_fmt

    -- Inline defs
    start flags DFinline
    let imod_inline@(IModule { imod_interface = ifc }) =
            if inlineISyntax flags
            then (iInline (inlineSimple flags) imod_fmt)
            else imod_fmt
    iMCheck flags symt imod_inline "inline"
    t <- dump errh flags t DFinline dumpnames imod_inline
    stats flags DFinline imod_inline

    start flags DFtransform
    let imod_trans = iTransform errh flags "__i" imod_inline
    iMCheck flags symt imod_trans "transform"
    t <- dump errh flags t DFtransform dumpnames imod_trans
    stats flags DFtransform imod_trans

    -- Split rules containing if statements
    start flags DFsplitIf
    let imod_splitif = iSplitIf flags imod_trans
    iMCheck flags symt imod_splitif "splitIf"
    t <- dump errh flags t DFsplitIf dumpnames imod_splitif
    stats flags DFsplitIf imod_splitif

    -- Lift where possible
    start flags DFlift
    let imod_lift = iLift errh flags imod_splitif
    iMCheck flags symt imod_lift "lift"
    t <- dump errh flags t DFlift dumpnames imod_lift
    stats flags DFlift imod_lift

    -- Transform to simpler form (take 2)
    start flags DFtransform
    let imod_trans2 = iTransform errh flags "__j" imod_lift
    iMCheck flags symt imod_trans2 "transform"
    t <- dump errh flags t DFtransform dumpnames imod_trans2
    stats flags DFtransform imod_trans2

    -- Inline expressions for submodule instantiation parameters
    -- Or separate them into localparam defs instead of wire defs
    start flags DFiparams
    imod_param <- iParams errh imod_trans2
    t <- dump errh flags t DFiparams dumpnames imod_param

    start flags DFidroprules
    imod_drop <- iDropRules errh flags imod_param
    stats flags DFidroprules imod_drop
    t <- dump errh flags t DFidroprules dumpnames imod_drop

    -- Generate ATS
    -- ATS stands for "Abstract Transition System" from James C. Hoe's Ph.D. thesis
    start flags DFATS
    amod <- aConv errh pps flags imod_drop
    aCheck flags amod "ATS"
    t <- dump errh flags t DFATS dumpnames amod
    amod == amod `seq` return ()
    t <- if (isJust $ lookup DFATSexpand $ dumps flags)
         then dumpStr errh flags t DFATSexpand dumpnames $
              pretty 120 100 $ ppeAPackage (expandATSlimit flags) PDReadable amod
         else return t
    stats flags DFATS amod

    let flat_inst_tree = flattenInstTree (apkg_inst_tree amod)
    when ("-hack-strict-inst-tree" `elem` progArgs) $ do
      when (any isNothing (map snd (concatMap snd flat_inst_tree))) $
        internalError ("Bad inst tree:\n " ++ ppReadable flat_inst_tree)

    when ("-trace-inst-tree" `elem` progArgs) $ do
      putStrLn "Instantiation tree"
      putStr (ppReadable (apkg_inst_tree amod))
--      putStrLn "Instantiation tree"
--      putStr (ppReadable flat_inst_tree)

-- append ranks to method names and calls for "performance guarantees"
    start flags DFATSperfspec
    amod_ranked <- aRankMethCalls errh pps amod
    aCheck flags amod_ranked "ATSperfspec"
    t <- dump errh flags t DFATSperfspec dumpnames amod_ranked

-- splice in the identifiers set by ActionValue tasks into the ATaskAction calls
    start flags DFATSsplice
    let amod_splice = aTaskSplice amod_ranked
    aCheck flags amod_splice "ATSsplice"
    t <- dump errh flags t DFATSsplice dumpnames amod_splice
    stats flags DFATSsplice amod_splice

-- clean up ATS (merge any ME calls to the same action in the same rule)
    start flags DFATSclean
    amod_clean <- aCleanup errh flags amod_splice
    aCheck flags amod_clean "ATSclean"
    t <- dump errh flags t DFATSclean dumpnames amod_clean

    start flags DFdumpLambdaCalculus
    lc_pkg <- convAPackageToLambdaCalc errh flags amod_clean
    t <- dump errh flags t DFdumpLambdaCalculus dumpnames lc_pkg

    start flags DFdumpSAL
    sal_ctx <- convAPackageToSAL errh flags amod_clean
    t <- dump errh flags t DFdumpSAL dumpnames sal_ctx

    -- Build path graph for everything except rules
    start flags DFpathsPreSched
    (pathGraphInfo, urgency_pairs) <- aPathsPreSched errh flags amod_clean
    t <- dump errh flags t DFpathsPreSched dumpnames pathGraphInfo

    -- Schedule
    start flags DFschedule
    (schedule_info, amod_sched)
        <- do res <- aSchedule errh flags prefix urgency_pairs pps amod_clean
              case res of
                Right r -> return r
                Left schedule_info -> do
                    t <- dump errh flags t DFschedule dumpnames
                             (asei_schedule schedule_info)
                    -- dump the schedule, if requested
                    t <- if (showSchedule flags)
                         then do start flags DFdumpschedule
                                 aDumpScheduleErr errh flags prefix
                                                  amod_clean schedule_info
                                 dump errh flags t DFdumpschedule dumpnames ()
                         else return t
                    -- generate a .ba file, if requested
                    t <- if (genABin flags)
                         then writeABinSchedErr errh pps flags dumpnames t
                                  prefix modstr srcName (orig_cqt wi)
                                  schedule_info amod_clean
                         else return t
                    exitFail errh
    t <- dump errh flags t DFschedule dumpnames (asi_schedule schedule_info)
    stats flags DFschedule amod_sched
    start flags DFresources
    t <- dump errh flags t DFresources dumpnames
         (asi_resource_alloc_table schedule_info)
    start flags DFvschedinfo
    t <- dump errh flags t DFvschedinfo dumpnames (asi_v_sched_info schedule_info)

    -- Add CAN_FIRE and WILL_FIRE defs based on the schedule
    start flags DFscheduledefs
    amod_scheduled <- aAddScheduleDefs flags
                                       pps
                                       amod_sched
                                       schedule_info
    t <- dump errh flags t DFscheduledefs dumpnames amod_scheduled

    start flags DFdumpschedule
    -- only difference from amod_scheduled to amod_dump is some rules removed
    (amod_dump, schedule_info_updated, methodConflict)
        <- aDumpSchedule errh flags pps prefix amod_scheduled schedule_info
    let schedule_final = asi_schedule schedule_info_updated
    let ms = (amod_dump, schedule_final)
    t <- dump errh flags t DFdumpschedule dumpnames ms

    -- prove any proof obligations accumulated at this point
    start flags DFcheckproofs
    -- the obligations are removed from the module
    (amod_check, ok) <- aCheckProofs errh flags amod_dump
    t <- dump errh flags t DFcheckproofs dumpnames ok

    -- Add scheduler edges to the path graph
    -- (we assume that pathGraphInfo is consistent with amod_checked, because
    -- at most only a few rules have been removed since amod_clean)
    start flags DFpathsPostSched
    vPathInfo <- aPathsPostSched flags pps amod_check pathGraphInfo schedule_final
    t <- dump errh flags t DFpathsPostSched dumpnames vPathInfo

    -- lift all no-inline func calls and assign instance names to them
    start flags DFnoinline
    let amod_noinline = aNoInline flags amod_check
    t <- dump errh flags t DFnoinline dumpnames amod_noinline

    -- add scheduling assumptions (ME, CF, etc.) to APackage (in two steps)
    start flags DFaddSchedAssumps
    (amod_wires, sched_info')
        <- aAddCFConditionWires errh symt alldefs flags
                                amod_noinline schedule_info_updated
    let (amod_assumps, sched_info'') =
            aAddSchedAssumps amod_wires schedule_final sched_info'
    aCheck flags amod_assumps "addSchedAssumps"
    t <- dump errh flags t DFaddSchedAssumps dumpnames
             (amod_assumps,
              asi_method_uses_map sched_info'',
              asi_resource_alloc_table sched_info'')

    -- move assumption actions into rule bodies
    start flags DFremoveAssumps
    let amod_no_assumps = aRemoveAssumps amod_assumps
    aCheck flags amod_no_assumps "aRemoveAssumps"
    t <- dump errh flags t DFremoveAssumps dumpnames amod_no_assumps

    -- drop ASAny with "chosen" values
    -- just before the split because other paths add new expressions
    -- and because we might choose more undets in pathsPostSched
    start flags DFdropundet
    let amod_no_undet = aDropUndet errh flags amod_no_assumps
    t <- dump errh flags t DFdropundet dumpnames amod_no_undet

    -- the amod doesn't change beyond this point
    let amod_final = amod_no_undet

    -- save wireinfo
    let wireinfo = apkg_external_wires amod_final
    let fieldinfo = getAPackageFieldInfos amod_final

    blurb <- mkGenFileHeader flags
    let methodConflictBlurb :: [String] -- string printed in top of Verilog file
        methodConflictBlurb
            | methodConf flags =
                ["Method conflict info:"]
                ++ lines (pretty 78 78 (vcat (dumpMethodInfo flags methodConflict)) )
            | otherwise = []

    let methodConflictBVI :: [String] -- string printed in top of Verilog file
        methodConflictBVI
            | methodBVI flags =
                ["BVI format method schedule info:"]
                ++ lines (pretty 78 78 (vcat (dumpMethodBVIInfo methodConflict)) )
            | otherwise = []
    -- Additional info from the Verilog backend
    -- * veriPortProps =
    --           IO properties which can be included as attributes in the
    --           Cmoduleverilog (import-BVI)
    --           XXX it would be nice if the Bluesim backend had the same info
    -- * vprog = the Verilog data structure, for recording in the .ba file,
    --           so that it's available to bluetcl
    (t, veriPortProps, vprog)
        <- if (backend flags == Just Verilog)
           then do (t', ips, v)
                       <- genModuleVerilog
                             errh pps flags dumpnames t prefix modstr
                             blurb methodConflictBlurb methodConflictBVI
                             vPathInfo sched_info'' amod_final
                   return (t', ips, Just v)
           else return (t, [], Nothing)

    t <- if (genABin flags)
         then writeABin errh pps flags dumpnames t prefix
                  modstr srcName (orig_cqt wi)
                  sched_info'' methodConflict vPathInfo
                  amod_final vprog
         else return t

    -- Wrapper generation
    start flags DFwrappergen

    -- ids of value methods with constant True output
    -- (any rdy signals in this list don't need to be wired up
    -- in the wrapper; it can assume a value of 1)
    let true_ifc_ids  = [ i | IEFace i _ (Just (e, t)) _ _ _ <- ifc, isTrue e || isAlwaysRdy pps i ]
    def <- (deffun wi)
                 fwrapper
                 wireinfo
                 (asi_v_sched_info schedule_info)
                 vPathInfo
                 veriPortProps
                 symt
                 fieldinfo
                 true_ifc_ids

    -- mainly because hypering the def will force any embedded exceptions
    t <- dump errh flags t DFwrappergen dumpnames def
    return (def)


writeABin :: ErrorHandle -> [PProp] -> Flags -> DumpNames -> TimeInfo ->
             String -> String -> String -> CQType ->
             AScheduleInfo -> MethodDumpInfo -> VPathInfo ->
             APackage -> Maybe VProgram -> IO (TimeInfo)
writeABin errh pps flags dumpnames t prefix modstr srcName oqt
          sched_info methodConflict vPathInfo amod vprog =
    do
       start flags DFwriteABin

       -- Don't generate .ba file if the backend is Bluesim and the
       -- module has features not supported by Bluesim.
       -- XXX For Verilog, this currently does nothing, but it could be
       -- XXX made to taint the .ba and issue a warning.
       amod_for_abin
           <- simCheckPackage errh (backend flags == Just Bluesim) amod

       -- generate the abin file
       let afilename = mkAName (bdir flags) prefix modstr
           afilename_rel = getRelativeFilePath afilename
           backend = apkg_backend amod_for_abin
           abinPrintPrefix =
              case (backend) of
                  Nothing -> "Elaborated module file created: "
                  Just be ->
                      "Elaborated " ++ ppString be ++ " module file created: "
           modinfo = ABinModInfo {
                          abmi_path = prefix,
                          abmi_src_name = srcName,
                          --abmi_time = now,
                          abmi_apkg        = amod_for_abin,
                          abmi_aschedinfo  = sched_info,
                          abmi_pps         = pps,
                          abmi_oqt         = oqt,
                          abmi_method_dump = methodConflict,
                          abmi_pathinfo = vPathInfo,
                          abmi_flags       = flags,
                          abmi_vprogram    = if (genABinVerilog flags)
                                             then vprog else Nothing
                     }
           abin = ABinMod modinfo (bscVersionStr True)
       genABinFile errh afilename abin
       unless (quiet flags) $ putStrLnF $ abinPrintPrefix ++ afilename_rel
       dump errh flags t DFwriteABin dumpnames afilename


writeABinSchedErr :: ErrorHandle -> [PProp] -> Flags -> DumpNames -> TimeInfo ->
                     String -> String -> String -> CQType ->
                     AScheduleErrInfo -> APackage -> IO (TimeInfo)
writeABinSchedErr errh pps flags dumpnames t prefix modstr srcName oqt
                  sched_info amod =
    do
       start flags DFwriteABin

       -- generate the abin file
       let afilename = mkAName (bdir flags) prefix modstr
           afilename_rel = getRelativeFilePath afilename
           abinPrintPrefix = "Elaborated error module file created: "
           modinfo = ABinModSchedErrInfo {
                          abmsei_path          = prefix,
                          abmsei_src_name      = srcName,
                          abmsei_apkg          = amod,
                          abmsei_aschederrinfo = sched_info,
                          abmsei_pps           = pps,
                          abmsei_oqt           = oqt,
                          abmsei_flags         = flags
                     }
           abin = ABinModSchedErr modinfo (bscVersionStr True)
       genABinFile errh afilename abin
       unless (quiet flags) $ putStrLnF $ abinPrintPrefix ++ afilename_rel
       dump errh flags t DFwriteABin dumpnames afilename


-- ===============
-- genModuleVerilog

genModuleVerilog :: ErrorHandle
                 -> [PProp]
                 -> Flags
                 -> DumpNames
                 -> TimeInfo
                 -> String -- prefix
                 -> String -- top module name
                 -> [String] -- header blurb lines
                 -> [String] -- method conflict blurb lines
                 -> [String] -- method bvi format blurb lines
                 -> VPathInfo -- used to create path info blurb lines
                 -> AScheduleInfo
                 -> APackage
                 -> IO (TimeInfo,
                        [VPort],   -- port properties for the import-BVI
                        VProgram)  -- generated Verilog
genModuleVerilog errh pprops flags dumpnames time0 prefix moduleName
                 blurb methodConflictBlurb methodConflictBVI vPathInfo scheduleInfo
                 atsPackage =
    do
       -- Read in foreign function info from .ba files for
       -- all foreign functions used in the design, and build a
       -- map to be used when generating verilog
       start flags DFforeignMap
       let foreign_func_names = getForeignCallNames atsPackage
           readABin ffname =
               let err = (noPosition,
                          EMissingABinForeignFuncFile ffname moduleName)
               in  readAndCheckABinPathCatch errh
                       (verbose flags) (ifcPath flags) (Just Verilog)
                       ffname err
       abis <- mapM readABin foreign_func_names
       ff_map <- buildForeignFunctionMap errh abis
       t <- dump errh flags time0 DFforeignMap dumpnames ff_map

       -- Generate muxes etc.
       start flags DFastate
       asmod <- aState errh flags pprops scheduleInfo atsPackage
       -- after aState, method calls should no longer exist
       asMethCheck flags asmod "astate"
       t <- dump errh flags t DFastate dumpnames asmod
       stats flags DFastate asmod

       -- Get rid of wires (and warn about BypassWire where necessary)
       start flags DFrwire
       let (asmodNoWires, wireWarnings, wireErrors)
               | removeRWire flags = aInlineWires flags asmod
               | otherwise = (asmod, [], [])
       when (not (null wireWarnings)) $ bsWarning errh wireWarnings
       when (not (null wireErrors))   $ bsError errh wireErrors
       asCheck flags asmodNoWires "ainlinewires"
       t <- dump errh flags t DFrwire dumpnames asmodNoWires
       stats flags DFrwire asmodNoWires

       -- Inline CReg modules (replacing with Reg modules)
       start flags DFcreg
       let asmodNoCReg
               | removeCReg flags = aInlineCReg asmodNoWires
               | otherwise = asmodNoWires
       asCheck flags asmodNoCReg "ainlinecreg"
       t <- dump errh flags t DFcreg dumpnames asmodNoCReg
       stats flags DFrwire asmodNoCReg

       -- Rename submodule ports from the method-notation to the actual
       -- Verilog port names
       start flags DFrenameio
       let armod = aRenameIO flags asmodNoCReg
       asCheck flags armod "renameio"
       t <- dump errh flags t DFrenameio dumpnames armod
       stats flags DFrenameio armod

       -- drop unused defs
       start flags DFadropdefs
       let adropmod = aDropDefs armod
       asCheck flags adropmod "adropdefs"
       t <- dump errh flags t DFadropdefs dumpnames adropmod
       stats flags DFadropdefs adropmod

       -- Improve
       start flags DFaopt
       aomod <- aOpt errh flags adropmod
       asCheck flags aomod "aopt"
       t <- dump errh flags t DFaopt dumpnames aomod
       stats flags DFaopt aomod

       -- Expand some primitives
       start flags DFsynthesize
       let asynmod = (aSynthesize flags aomod)
       asCheck flags asynmod "asynthesize"
       -- Check that referenced signal names exist
       asSignalCheck flags asynmod "synthesize"
       t <- dump errh flags t DFsynthesize dumpnames asynmod
       stats flags DFsynthesize asynmod

       -- Transform to adapt to Verilog quirks
       start flags DFveriquirks
       let aqmod = aVeriQuirks flags asynmod
       -- this check is too strict on plus operator output size
       -- XXX fix the check, don't disable it
       when (not (keepAddSize flags))
            (asCheck flags aqmod "veriquirks")
       t <- dump errh flags t DFveriquirks dumpnames aqmod
       stats flags DFveriquirks aqmod

       -- Transform to adapt to Verilog quirks
       start flags DFfinalcleanup
       let aumod =  finalCleanup flags aqmod
       -- this check is too strict on plus operator output size
       -- XXX fix the check, don't disable it
       when (not (keepAddSize flags))
            (asCheck flags aumod "finalcleanup")
       t <- dump errh flags t DFfinalcleanup dumpnames aumod
       stats flags DFfinalcleanup aumod

       start flags DFIOproperties
       let (ioprops, ips) = getIOProps flags aumod
       t <- dump errh flags t DFIOproperties dumpnames ioprops

       -- Generate Verilog
       start flags DFverilog
       -- This is monadic because it can report an error
       vprog0 <- aVerilog errh flags pprops aumod ff_map
       t <- dump errh flags t DFverilog dumpnames vprog0

       -- Remove dollar signs from Verilog identifiers
       start flags DFverilogDollar
       let vprog = if (removeVerilogDollar flags)
                   then (removeDollarsFromVerilog vprog0)
                   else vprog0
       t <- dump errh flags t DFverilogDollar dumpnames vprog

       -- Write the Verilog files
       start flags DFwriteVerilog
       vfilenames <- writeVerilog errh flags prefix
                         blurb methodConflictBlurb methodConflictBVI ioprops vPathInfo
                         vprog
       t <- dump errh flags t DFwriteVerilog dumpnames vfilenames

       -- Return
       -- * the port properties (to be included in the import-BVI)
       -- * the Verilog structure (for accessing in bluetcl)
       return (t, ips, vprog)


-- Write a Verilog program to file (along with its use file)
writeVerilog :: ErrorHandle -> Flags -> String ->
                [String] -> [String] -> [String] -> VIOProps -> VPathInfo ->
                VProgram -> IO ([VFileName])
writeVerilog errh flags prefix
             blurb methodConflictBlurb methodConflictBVI ioprops vPathInfo
             vprog =
    do
       let modName :: String
           modName = (vGetMainModName vprog)

       -- Make the filename (full path, but with the relative prefix encoded)
       vName_init <- genFileName mkVName (vdir flags) prefix modName
       -- The relative filename, for reporting to the user
       let vNameRel = (getRelativeFilePath vName_init)
       -- The final name used to write the file (full path)
       let vName = VFileName (getFullFilePath vName_init)

       -- Names for the use file
       let useNameRel = dropSuf vNameRel ++ "." ++ useSuffix
           useName = dropSuf (vfnString vName) ++ "." ++ useSuffix

       -- Comments at the top of the Verilog file
       let pathInfoBlurb = lines (pp80 vPathInfo)
           ioblurb = "" : "Ports:" : lines (pp80 ioprops)
           comment = commentV (blurb ++ methodConflictBlurb ++ methodConflictBVI ++
                               ioblurb ++
                               pathInfoBlurb ++ [""])

       -- The contents of the Verilog file
       let vstring = comment ++ pp80 vprog

       writeVFileCatch errh flags vName vstring
       unless (quiet flags) $ putStrLnF ("Verilog file created: " ++ vNameRel)

       -- if generating a use file
       when (showModuleUse flags) $ do
           writeFileCatch errh useName (unlines (getVeriInsts vprog))
           unless (quiet flags) $ putStrLnF ("Use file created: " ++ useNameRel)

       return [vName]


writeVFileCatch :: ErrorHandle -> Flags -> VFileName -> String -> IO ()
writeVFileCatch errh flags (VFileName fn) s = do
    let dropTrailingBlanks :: String -> String
        dropTrailingBlanks s | null s           = s
                             | isSpace $ last s = dropTrailingBlanks $ init s
                             | otherwise        = s
        --
        s' = unlines $  map dropTrailingBlanks $ lines s
    writeFileCatch errh fn s'
    let applyFilter :: String -> IO ()
        applyFilter command = do
          let cmdstr = command ++ " " ++ fn
          when (verbose flags) $
            unless (quiet flags) $ putStrLnF ("Executing Verilog filter " ++ quote cmdstr)
          code <- system cmdstr
          case code of
            ExitSuccess     -> return ()
            (ExitFailure n) -> bsError errh [(cmdPosition, EVerilogFilterError command fn n)]
    mapM_ applyFilter $ reverse $ verilogFilter flags


iMCheck :: Flags -> SymTab -> IModule a -> String -> IO ()
iMCheck flags symt imod desc =
    if doICheck flags && not (tCheckIModule flags symt imod)
        then internalError (
                "internal typecheck failed (iMCheck after " ++
                desc ++ ")")
        else
            if (verbose flags)
                then putStrLnF "types OK"
                else return ()

aCheck :: Flags -> APackage -> String -> IO ()
aCheck flags amod desc =
    if doICheck flags && not (aMCheck amod)
        then internalError (
                "internal typecheck failed (aCheck after " ++
                desc ++ ")")
        else
            if (verbose flags)
                then putStrLnF "types OK"
                else return ()

asCheck :: Flags -> ASPackage -> String -> IO ()
asCheck flags asmod desc
    | doICheck flags && not (aSMCheck asmod) =
        internalError ("internal typecheck failed (asCheck after "
                       ++ desc ++ ")")
    | verbose flags = putStrLnF "types OK"
    | otherwise = return ()

asMethCheck :: Flags -> ASPackage -> String -> IO ()
asMethCheck flags asmod desc =
    if (doICheck flags && not (aSMethCheck asmod))
    then internalError ("internal method check failed (asMethCheck after "
                        ++ desc ++ ")")
    else return ()  --putStrLnF "method check OK"

asSignalCheck :: Flags -> ASPackage -> String -> IO ()
asSignalCheck flags asmod desc =
    let undefined_names = aSignalCheck asmod
    in  if (doICheck flags) && (length undefined_names > 0)
        then internalError
                 ("internal signal check failed (asSignalCheck after "
                  ++ desc ++ "): " ++ ppReadable undefined_names)
        else if (verbose flags)
             then putStrLnF "signals OK"
             else return ()


-- builds a map from link name to ForeignFunction
buildForeignFunctionMap :: ErrorHandle -> [(String,ABin)] -> IO ForeignFuncMap
buildForeignFunctionMap errh abis = build abis M.empty
  where build [] ff_map = return ff_map
        build ((name, (ABinMod {})):_) _ =
          bsError errh [(noPosition, EWrongABinTypeExpectedForeignFunc name "")]
        build ((name, abi):abis) ff_map =
          do let ff = abffi_foreign_func (ab_ffuncinfo abi)
                 ff_map' = M.insert (getIdString (ff_name ff)) ff ff_map
             build abis ff_map'
