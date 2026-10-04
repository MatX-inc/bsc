-- | Compatibility adapters for implementation passes which still accept
-- 'Flags'. Only explicit phase inputs and shared policy are restored; all
-- unrelated fields use the fixed CLI defaults. Keep this boundary out of
-- the pure configuration module so phase records cannot retain legacy state.
module PhaseConfigLegacy (PhaseFlags, legacyFlags, internalTypecheckFlags) where

import Flags (Flags)
import qualified Flags as F
import FlagsDecode (defaultFlags)
import PhaseConfig

class PhaseFlags a where
    restorePhaseFlags :: a -> Flags -> Flags

legacyFlags :: PhaseFlags a => PhaseConfig a -> Flags
legacyFlags config =
    restorePhaseFlags (phaseFlags config) $
    restoreRunOptions (phaseRunOptions config) $
    restoreRunFlags (phaseRunFlags config) $
    defaultFlags (runBluespecDir (phaseRunFlags config))

-- | Generated expressions use coherent matching, no implicit local
-- generalization, and no poison recovery. Only the solver policy comes from
-- the enclosing phase; CLI diagnostics, dumps, paths and other controls do
-- not enter this internal checking service.
internalTypecheckFlags :: TypeSolverFlags -> Flags
internalTypecheckFlags solver = (defaultFlags "")
    { F.maxTIStackDepth = typeSolverMaxTIStackDepth solver
    , F.useProvisoSAT = typeSolverUseProvisoSAT solver
    , F.satBackend = typeSolverSatBackend solver
    , F.allowIncoherentMatches = False
    , F.letGen = False
    , F.enablePoisonPills = False
    }


restoreRunFlags :: RunFlags -> Flags -> Flags
restoreRunFlags config flags = flags
    { F.bluespecDir = runBluespecDir config
    , F.promoteWarnings = runPromoteWarnings config
    , F.demoteErrors = runDemoteErrors config
    , F.suppressWarnings = runSuppressWarnings config
    , F.enablePoisonPills = runEnablePoisonPills config
    , F.doICheck = runDoICheck config
    , F.kill = runKill config
    , F.remapPathPrefix = runRemapPathPrefix config
    }

restoreRunOptions :: RunOptions -> Flags -> Flags
restoreRunOptions config flags = flags
    { F.verbosity = runVerbosity config
    , F.dumpAll = runDumpAll config
    , F.dumps = runDumps config
    , F.showStats = runShowStats config
    , F.showCodeGen = runShowCodeGen config
    , F.showCSyntax = runShowCSyntax config
    , F.showISyntax = runShowISyntax config
    , F.showIESyntax = runShowIESyntax config
    , F.showElabProgress = runShowElabProgress config
    , F.showModuleUse = runShowModuleUse config
    , F.showUpds = runShowUpds config
    , F.tclShowHidden = runTclShowHidden config
    , F.infoDir = runInfoDir config
    , F.schedDOT = runSchedDOT config
    , F.showSchedule = runShowSchedule config
    }

instance PhaseFlags ParseFlags where
    restorePhaseFlags config flags = flags
        { F.cpp = parseCpp config
        , F.cppFlags = parseCppFlags config
        , F.backend = parseBackend config
        , F.vpp = parseVpp config
        , F.defines = parseDefines config
        , F.ifcPath = parseIfcPath config
        , F.stdlibNames = parseStdlibNames config
        , F.disableAssertions = parseDisableAssertions config
        , F.passThroughAssertions = parsePassThroughAssertions config
        , F.genName = parseGenName config
        , F.preprocessOnly = parsePreprocessOnly config
        }

instance PhaseFlags ReduceFlags where
    restorePhaseFlags config flags = flags
        { F.maxTIStackDepth = reduceMaxTIStackDepth config
        , F.useProvisoSAT = reduceUseProvisoSAT config
        , F.satBackend = reduceSatBackend config
        , F.resetName = reduceResetName config
        , F.backend = reduceBackend config
        , F.genName = reduceGenName config
        , F.usePrelude = reduceUsePrelude config
        , F.ifcPath = reduceIfcPath config
        }

instance PhaseFlags TypecheckFlags where
    restorePhaseFlags config flags = flags
        { F.maxTIStackDepth = typecheckMaxTIStackDepth config
        , F.useProvisoSAT = typecheckUseProvisoSAT config
        , F.satBackend = typecheckSatBackend config
        , F.allowIncoherentMatches = typecheckAllowIncoherentMatches config
        , F.letGen = typecheckLetGen config
        }

instance PhaseFlags InternalFlags where
    restorePhaseFlags config flags = flags
        { F.maxTIStackDepth = internalMaxTIStackDepth config
        , F.useProvisoSAT = internalUseProvisoSAT config
        , F.satBackend = internalSatBackend config
        , F.simplifyCSyntax = internalSimplifyCSyntax config
        , F.liftDicts = internalLiftDicts config
        , F.backend = internalBackend config
        , F.useDPI = internalUseDPI config
        , F.bdir = internalBdir config
        , F.vdir = internalVdir config
        , F.showVersion = internalShowVersion config
        , F.timeStamps = internalTimeStamps config
        , F.testAssert = internalTestAssert config
        }

instance PhaseFlags ElabFlags where
    restorePhaseFlags config flags = flags
        { F.backend = elabBackend config
        , F.bdir = elabBdir config
        , F.fdir = elabFdir config
        , F.aggImpConds = elabAggImpConds config
        , F.methodConditions = elabMethodConditions config
        , F.resetName = elabResetName config
        , F.ruleNameCheck = elabRuleNameCheck config
        , F.inlineISyntax = elabInlineISyntax config
        , F.inlineSimple = elabInlineSimple config
        , F.optBool = elabOptBool config
        , F.expandIf = elabExpandIf config
        , F.ifLift = elabIfLift config
        , F.removeFalseRules = elabRemoveFalseRules config
        , F.removeEmptyRules = elabRemoveEmptyRules config
        , F.stableVerilog = elabStableVerilog config
        , F.useProvisoSAT = elabUseProvisoSAT config
        , F.satBackend = elabSatBackend config
        , F.redStepsWarnInterval = elabRedStepsWarnInterval config
        , F.redStepsMaxIntervals = elabRedStepsMaxIntervals config
        , F.maxTIStackDepth = elabMaxTIStackDepth config
        , F.warnUndetPred = elabWarnUndetPred config
        , F.expandATSlimit = elabExpandATSlimit config
        , F.optFinalPass = elabOptFinalPass config
        }

instance PhaseFlags SchedFlags where
    restorePhaseFlags config flags = flags
        { F.backend = schedBackend config
        , F.biasMethodScheduling = schedBiasMethodScheduling config
        , F.genABin = schedGenABin config
        , F.relaxMethodEarliness = schedRelaxMethodEarliness config
        , F.resource = schedResource config
        , F.satBackend = schedSatBackend config
        , F.schedConds = schedSchedConds config
        , F.schedTransposed = schedSchedTransposed config
        , F.schedQueries = schedSchedQueries config
        , F.strictMethodSched = schedStrictMethodSched config
        , F.stableVerilog = schedStableVerilog config
        , F.unsafeAlwaysRdy = schedUnsafeAlwaysRdy config
        , F.removeStarvedRules = schedRemoveStarvedRules config
        , F.warnActionShadowing = schedWarnActionShadowing config
        , F.warnMethodUrgency = schedWarnMethodUrgency config
        , F.showRangeConflict = schedShowRangeConflict config
        , F.expandATSlimit = schedExpandATSlimit config
        , F.timeStamps = schedTimeStamps config
        , F.showVersion = schedShowVersion config
        }

instance PhaseFlags MaterializeFlags where
    restorePhaseFlags config flags = flags
        { F.backend = materializeBackend config
        , F.stableVerilog = materializeStableVerilog config
        , F.optUndet = materializeOptUndet config
        , F.unSpecTo = materializeUnSpecTo config
        }

instance PhaseFlags VerilogGenFlags where
    restorePhaseFlags config flags = flags
        { F.useDPI = verilogGenUseDPI config
        , F.stableVerilog = verilogGenStableVerilog config
        , F.removeRWire = verilogGenRemoveRWire config
        , F.removeCross = verilogGenRemoveCross config
        , F.removeCReg = verilogGenRemoveCReg config
        , F.removeReg = verilogGenRemoveReg config
        , F.removeInoutConnect = verilogGenRemoveInoutConnect config
        , F.removeUnusedMods = verilogGenRemoveUnusedMods config
        , F.removePrimModules = verilogGenRemovePrimModules config
        , F.removeVerilogDollar = verilogGenRemoveVerilogDollar config
        , F.keepFires = verilogGenKeepFires config
        , F.keepInlined = verilogGenKeepInlined config
        , F.keepAddSize = verilogGenKeepAddSize config
        , F.inlineBool = verilogGenInlineBool config
        , F.optATS = verilogGenOptATS config
        , F.optSched = verilogGenOptSched config
        , F.optJoinDefs = verilogGenOptJoinDefs config
        , F.optIfMux = verilogGenOptIfMux config
        , F.optIfMuxSize = verilogGenOptIfMuxSize config
        , F.optMux = verilogGenOptMux config
        , F.optMuxExpand = verilogGenOptMuxExpand config
        , F.optMuxConst = verilogGenOptMuxConst config
        , F.optBitConst = verilogGenOptBitConst config
        , F.optAggInline = verilogGenOptAggInline config
        , F.optAndOr = verilogGenOptAndOr config
        , F.optFinalPass = verilogGenOptFinalPass config
        , F.satBackend = verilogGenSatBackend config
        , F.synthesize = verilogGenSynthesize config
        , F.useNegate = verilogGenUseNegate config
        , F.readableMux = verilogGenReadableMux config
        , F.finalcleanup = verilogGenFinalcleanup config
        , F.unSpecTo = verilogGenUnSpecTo config
        , F.systemVerilogOutput = verilogGenSystemVerilogOutput config
        , F.verilogDeclareAllFirst = verilogGenVerilogDeclareAllFirst config
        , F.semanticPortsComment = verilogGenSemanticPortsComment config
        , F.vdir = verilogGenVdir config
        , F.verilogFilter = verilogGenVerilogFilter config
        , F.ifcPath = verilogGenIfcPath config
        , F.methodConf = verilogGenMethodConf config
        , F.methodBVI = verilogGenMethodBVI config
        , F.timeStamps = verilogGenTimeStamps config
        , F.showVersion = verilogGenShowVersion config
        }

instance PhaseFlags BluesimGenFlags where
    restorePhaseFlags config flags = flags
        { F.blockCodegen = bluesimGenBlockCodegen config
        , F.genSysC = bluesimGenGenSysC config
        , F.resetName = bluesimGenResetName config
        , F.unSpecTo = bluesimGenUnSpecTo config
        , F.keepFires = bluesimGenKeepFires config
        , F.dumpFormats = bluesimGenDumpFormats config
        , F.timeStamps = bluesimGenTimeStamps config
        , F.showVersion = bluesimGenShowVersion config
        , F.ifcPath = bluesimGenIfcPath config
        , F.cdir = bluesimGenCdir config
        , F.backend = bluesimGenBackend config
        , F.optUndet = bluesimGenOptUndet config
        , F.stableVerilog = bluesimGenStableVerilog config
        }

instance PhaseFlags HostCompileFlags where
    restorePhaseFlags config flags = flags
        { F.genSysC = hostCompileGenSysC config
        , F.cxxFlags = hostCompileCxxFlags config
        , F.cDebug = hostCompileCDebug config
        , F.cIncPath = hostCompileCIncPath config
        , F.cFlags = hostCompileCFlags config
        , F.cdir = hostCompileCdir config
        , F.parallelSimLink = hostCompileParallelSimLink config
        }

instance PhaseFlags VerilogLinkFlags where
    restorePhaseFlags config flags = flags
        { F.cLibPath = verilogLinkCLibPath config
        , F.cLibs = verilogLinkCLibs config
        , F.defines = verilogLinkDefines config
        , F.vPath = verilogLinkVPath config
        , F.oFile = verilogLinkOFile config
        , F.vFlags = verilogLinkVFlags config
        , F.linkFlags = verilogLinkLinkFlags config
        , F.useDPI = verilogLinkUseDPI config
        , F.dumpFormats = verilogLinkDumpFormats config
        , F.vsim = verilogLinkVsim config
        }

instance PhaseFlags BluesimLinkFlags where
    restorePhaseFlags config flags = flags
        { F.oFile = bluesimLinkOFile config
        , F.cLibPath = bluesimLinkCLibPath config
        , F.cLibs = bluesimLinkCLibs config
        , F.linkFlags = bluesimLinkLinkFlags config
        , F.dumpFormats = bluesimLinkDumpFormats config
        , F.cDebug = bluesimLinkCDebug config
        , F.timeStamps = bluesimLinkTimeStamps config
        }

instance PhaseFlags ForeignGenFlags where
    restorePhaseFlags config flags = flags
        { F.useDPI = foreignGenUseDPI config
        , F.vPath = foreignGenVPath config
        , F.vdir = foreignGenVdir config
        , F.timeStamps = foreignGenTimeStamps config
        , F.showVersion = foreignGenShowVersion config
        }
