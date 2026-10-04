-- | Explicit configuration inputs for compiler phases. The CLI's historical
-- 'Flags' record is projected at orchestration boundaries; no phase value
-- retains that record or a function which can recover unrelated settings.
--
-- 'RunOptions' contains observation and ordinary dump requests. These do not
-- belong in a future primary-artifact cache key. Failure policy and path
-- remapping remain in 'RunFlags'; algorithm and product choices live in the
-- appropriate phase record. This module does not implement caching.
module PhaseConfig
    ( PhaseConfig(..)
    , RunFlags(..)
    , RunOptions(..)
    , TypeSolverFlags(..)
    , ParseFlags(..)
    , ReduceFlags(..)
    , TypecheckFlags(..)
    , InternalFlags(..)
    , ElabFlags(..)
    , SchedFlags(..)
    , MaterializeFlags(..)
    , VerilogGenFlags(..)
    , BluesimGenFlags(..)
    , HostCompileFlags(..)
    , VerilogLinkFlags(..)
    , BluesimLinkFlags(..)
    , ForeignGenFlags(..)
    , parseConfig
    , reduceConfig
    , typecheckConfig
    , internalConfig
    , elabConfig
    , schedConfig
    , materializeConfig
    , verilogGenConfig
    , bluesimGenConfig
    , hostCompileConfig
    , verilogLinkConfig
    , bluesimLinkConfig
    , foreignGenConfig
    , typeSolverFlags
    ) where

import Backend (Backend)
import Flags (Flags, DumpFlag, MsgListFlag, ResourceFlag, SATFlag, Verbosity)
import qualified Flags as F

-- | Common failure and execution policy, including artifact path remapping.


data RunFlags = RunFlags
    { runBluespecDir :: String
    , runPromoteWarnings :: MsgListFlag
    , runDemoteErrors :: MsgListFlag
    , runSuppressWarnings :: MsgListFlag
    , runEnablePoisonPills :: Bool
    , runDoICheck :: Bool
    , runKill :: Maybe (DumpFlag, Maybe String)
    , runRemapPathPrefix :: [(String, String)]
    } deriving (Eq, Show)


-- | Observation settings shared by the phase drivers.


data RunOptions = RunOptions
    { runVerbosity :: Verbosity
    , runDumpAll :: Maybe (Maybe FilePath)
    , runDumps :: [(DumpFlag, Maybe FilePath)]
    , runShowStats :: Bool
    , runShowCodeGen :: Bool
    , runShowCSyntax :: Bool
    , runShowISyntax :: Bool
    , runShowIESyntax :: Bool
    , runShowElabProgress :: Bool
    , runShowModuleUse :: Bool
    , runShowUpds :: Bool
    , runTclShowHidden :: Bool
    , runInfoDir :: Maybe String
    , runSchedDOT :: Bool
    , runShowSchedule :: Bool
    } deriving (Eq, Show)


-- | The complete explicit input to one phase. It is deliberately independent
-- of the legacy 'Flags' representation used by unmigrated implementation code.
data PhaseConfig a = PhaseConfig
    { phaseRunFlags :: RunFlags
    , phaseRunOptions :: RunOptions
    , phaseFlags :: a
    } deriving (Eq, Show)

-- | Solver policy shared with internal checks of generated expressions.
-- These checks choose their own language and recovery policies; they do not
-- inherit the source package's complete typechecking or execution settings.
data TypeSolverFlags = TypeSolverFlags
    { typeSolverMaxTIStackDepth :: Int
    , typeSolverUseProvisoSAT :: Bool
    , typeSolverSatBackend :: SATFlag
    } deriving (Eq, Show)

typeSolverFlags :: Flags -> TypeSolverFlags
typeSolverFlags flags = TypeSolverFlags
    { typeSolverMaxTIStackDepth = F.maxTIStackDepth flags
    , typeSolverUseProvisoSAT = F.useProvisoSAT flags
    , typeSolverSatBackend = F.satBackend flags
    }


data ParseFlags = ParseFlags
    { parseCpp :: Bool
    , parseCppFlags :: [String]
    , parseBackend :: Maybe Backend
    , parseVpp :: Bool
    , parseDefines :: [String]
    , parseIfcPath :: [String]
    , parseStdlibNames :: Bool
    , parseDisableAssertions :: Bool
    , parsePassThroughAssertions :: Bool
    , parseGenName :: [String]
    , parsePreprocessOnly :: Bool
    } deriving (Eq, Show)

data ReduceFlags = ReduceFlags
    { reduceMaxTIStackDepth :: Int
    , reduceUseProvisoSAT :: Bool
    , reduceSatBackend :: SATFlag
    , reduceResetName :: String
    , reduceBackend :: Maybe Backend
    , reduceGenName :: [String]
    , reduceUsePrelude :: Bool
    , reduceIfcPath :: [String]
    } deriving (Eq, Show)

data TypecheckFlags = TypecheckFlags
    { typecheckMaxTIStackDepth :: Int
    , typecheckUseProvisoSAT :: Bool
    , typecheckSatBackend :: SATFlag
    , typecheckAllowIncoherentMatches :: Bool
    , typecheckLetGen :: Bool
    } deriving (Eq, Show)

data InternalFlags = InternalFlags
    { internalMaxTIStackDepth :: Int
    , internalUseProvisoSAT :: Bool
    , internalSatBackend :: SATFlag
    , internalSimplifyCSyntax :: Bool
    , internalLiftDicts :: Bool
    , internalBackend :: Maybe Backend
    , internalUseDPI :: Bool
    , internalBdir :: Maybe String
    , internalVdir :: Maybe String
    , internalShowVersion :: Bool
    , internalTimeStamps :: Bool
    , internalTestAssert :: Bool
    } deriving (Eq, Show)

-- optFinalPass also preserves the optional LambdaCalc/SAL dump lowering.


data ElabFlags = ElabFlags
    { elabBackend :: Maybe Backend
    , elabBdir :: Maybe String
    , elabFdir :: Maybe String
    , elabAggImpConds :: Bool
    , elabMethodConditions :: Bool
    , elabResetName :: String
    , elabRuleNameCheck :: Bool
    , elabInlineISyntax :: Bool
    , elabInlineSimple :: Bool
    , elabOptBool :: Bool
    , elabExpandIf :: Bool
    , elabIfLift :: Bool
    , elabRemoveFalseRules :: Bool
    , elabRemoveEmptyRules :: Bool
    , elabStableVerilog :: Bool
    , elabUseProvisoSAT :: Bool
    , elabSatBackend :: SATFlag
    , elabRedStepsWarnInterval :: Integer
    , elabRedStepsMaxIntervals :: Integer
    , elabMaxTIStackDepth :: Int
    , elabWarnUndetPred :: Bool
    , elabExpandATSlimit :: Int
    , elabOptFinalPass :: Bool
    } deriving (Eq, Show)

-- genABin and schedQueries affect relation coverage, not just reporting.


data SchedFlags = SchedFlags
    { schedBackend :: Maybe Backend
    , schedBiasMethodScheduling :: Bool
    , schedGenABin :: Bool
    , schedRelaxMethodEarliness :: Bool
    , schedResource :: ResourceFlag
    , schedSatBackend :: SATFlag
    , schedSchedConds :: Bool
    , schedSchedTransposed :: Bool
    , schedSchedQueries :: [(String,String)]
    , schedStrictMethodSched :: Bool
    , schedStableVerilog :: Bool
    , schedUnsafeAlwaysRdy :: Bool
    , schedRemoveStarvedRules :: Bool
    , schedWarnActionShadowing :: Bool
    , schedWarnMethodUrgency :: Bool
    , schedShowRangeConflict :: Bool
    , schedExpandATSlimit :: Int
    , schedTimeStamps :: Bool
    , schedShowVersion :: Bool
    } deriving (Eq, Show)

data MaterializeFlags = MaterializeFlags
    { materializeBackend :: Maybe Backend
    , materializeStableVerilog :: Bool
    , materializeOptUndet :: Bool
    , materializeUnSpecTo :: String
    } deriving (Eq, Show)

data VerilogGenFlags = VerilogGenFlags
    { verilogGenUseDPI :: Bool
    , verilogGenStableVerilog :: Bool
    , verilogGenRemoveRWire :: Bool
    , verilogGenRemoveCross :: Bool
    , verilogGenRemoveCReg :: Bool
    , verilogGenRemoveReg :: Bool
    , verilogGenRemoveInoutConnect :: Bool
    , verilogGenRemoveUnusedMods :: Bool
    , verilogGenRemovePrimModules :: Bool
    , verilogGenRemoveVerilogDollar :: Bool
    , verilogGenKeepFires :: Bool
    , verilogGenKeepInlined :: Bool
    , verilogGenKeepAddSize :: Bool
    , verilogGenInlineBool :: Bool
    , verilogGenOptATS :: Bool
    , verilogGenOptSched :: Bool
    , verilogGenOptJoinDefs :: Bool
    , verilogGenOptIfMux :: Bool
    , verilogGenOptIfMuxSize :: Integer
    , verilogGenOptMux :: Bool
    , verilogGenOptMuxExpand :: Bool
    , verilogGenOptMuxConst :: Bool
    , verilogGenOptBitConst :: Bool
    , verilogGenOptAggInline :: Bool
    , verilogGenOptAndOr :: Bool
    , verilogGenOptFinalPass :: Bool
    , verilogGenSatBackend :: SATFlag
    , verilogGenSynthesize :: Bool
    , verilogGenUseNegate :: Bool
    , verilogGenReadableMux :: Bool
    , verilogGenFinalcleanup :: Int
    , verilogGenUnSpecTo :: String
    , verilogGenSystemVerilogOutput :: Bool
    , verilogGenVerilogDeclareAllFirst :: Bool
    , verilogGenSemanticPortsComment :: Bool
    , verilogGenVdir :: Maybe String
    , verilogGenVerilogFilter :: [String]
    , verilogGenIfcPath :: [String]
    , verilogGenMethodConf :: Bool
    , verilogGenMethodBVI :: Bool
    , verilogGenTimeStamps :: Bool
    , verilogGenShowVersion :: Bool
    } deriving (Eq, Show)

-- Includes materialization policy because Bluesim loads child module pairs.


data BluesimGenFlags = BluesimGenFlags
    { bluesimGenBlockCodegen :: Bool
    , bluesimGenGenSysC :: Bool
    , bluesimGenResetName :: String
    , bluesimGenUnSpecTo :: String
    , bluesimGenKeepFires :: Bool
    , bluesimGenDumpFormats :: [String]
    , bluesimGenTimeStamps :: Bool
    , bluesimGenShowVersion :: Bool
    , bluesimGenIfcPath :: [String]
    , bluesimGenCdir :: Maybe String
    , bluesimGenBackend :: Maybe Backend
    , bluesimGenOptUndet :: Bool
    , bluesimGenStableVerilog :: Bool
    } deriving (Eq, Show)

data HostCompileFlags = HostCompileFlags
    { hostCompileGenSysC :: Bool
    , hostCompileCxxFlags :: [String]
    , hostCompileCDebug :: Bool
    , hostCompileCIncPath :: [String]
    , hostCompileCFlags :: [String]
    , hostCompileCdir :: Maybe String
    , hostCompileParallelSimLink :: Integer
    } deriving (Eq, Show)

data VerilogLinkFlags = VerilogLinkFlags
    { verilogLinkCLibPath :: [String]
    , verilogLinkCLibs :: [String]
    , verilogLinkDefines :: [String]
    , verilogLinkVPath :: [String]
    , verilogLinkOFile :: String
    , verilogLinkVFlags :: [String]
    , verilogLinkLinkFlags :: [String]
    , verilogLinkUseDPI :: Bool
    , verilogLinkDumpFormats :: [String]
    , verilogLinkVsim :: Maybe String
    } deriving (Eq, Show)

data BluesimLinkFlags = BluesimLinkFlags
    { bluesimLinkOFile :: String
    , bluesimLinkCLibPath :: [String]
    , bluesimLinkCLibs :: [String]
    , bluesimLinkLinkFlags :: [String]
    , bluesimLinkDumpFormats :: [String]
    , bluesimLinkCDebug :: Bool
    , bluesimLinkTimeStamps :: Bool
    } deriving (Eq, Show)

data ForeignGenFlags = ForeignGenFlags
    { foreignGenUseDPI :: Bool
    , foreignGenVPath :: [String]
    , foreignGenVdir :: Maybe String
    , foreignGenTimeStamps :: Bool
    , foreignGenShowVersion :: Bool
    } deriving (Eq, Show)

runFlags :: Flags -> RunFlags
runFlags flags = RunFlags
    { runBluespecDir = F.bluespecDir flags
    , runPromoteWarnings = F.promoteWarnings flags
    , runDemoteErrors = F.demoteErrors flags
    , runSuppressWarnings = F.suppressWarnings flags
    , runEnablePoisonPills = F.enablePoisonPills flags
    , runDoICheck = F.doICheck flags
    , runKill = F.kill flags
    , runRemapPathPrefix = F.remapPathPrefix flags
    }

runOptions :: Flags -> RunOptions
runOptions flags = RunOptions
    { runVerbosity = F.verbosity flags
    , runDumpAll = F.dumpAll flags
    , runDumps = F.dumps flags
    , runShowStats = F.showStats flags
    , runShowCodeGen = F.showCodeGen flags
    , runShowCSyntax = F.showCSyntax flags
    , runShowISyntax = F.showISyntax flags
    , runShowIESyntax = F.showIESyntax flags
    , runShowElabProgress = F.showElabProgress flags
    , runShowModuleUse = F.showModuleUse flags
    , runShowUpds = F.showUpds flags
    , runTclShowHidden = F.tclShowHidden flags
    , runInfoDir = F.infoDir flags
    , runSchedDOT = F.schedDOT flags
    , runShowSchedule = F.showSchedule flags
    }

parseConfig :: Flags -> PhaseConfig ParseFlags
parseConfig flags = PhaseConfig (runFlags flags) (runOptions flags)
    ParseFlags
        { parseCpp = F.cpp flags
        , parseCppFlags = F.cppFlags flags
        , parseBackend = F.backend flags
        , parseVpp = F.vpp flags
        , parseDefines = F.defines flags
        , parseIfcPath = F.ifcPath flags
        , parseStdlibNames = F.stdlibNames flags
        , parseDisableAssertions = F.disableAssertions flags
        , parsePassThroughAssertions = F.passThroughAssertions flags
        , parseGenName = F.genName flags
        , parsePreprocessOnly = F.preprocessOnly flags
        }

reduceConfig :: Flags -> PhaseConfig ReduceFlags
reduceConfig flags = PhaseConfig (runFlags flags) (runOptions flags)
    ReduceFlags
        { reduceMaxTIStackDepth = F.maxTIStackDepth flags
        , reduceUseProvisoSAT = F.useProvisoSAT flags
        , reduceSatBackend = F.satBackend flags
        , reduceResetName = F.resetName flags
        , reduceBackend = F.backend flags
        , reduceGenName = F.genName flags
        , reduceUsePrelude = F.usePrelude flags
        , reduceIfcPath = F.ifcPath flags
        }

typecheckConfig :: Flags -> PhaseConfig TypecheckFlags
typecheckConfig flags = PhaseConfig (runFlags flags) (runOptions flags)
    TypecheckFlags
        { typecheckMaxTIStackDepth = F.maxTIStackDepth flags
        , typecheckUseProvisoSAT = F.useProvisoSAT flags
        , typecheckSatBackend = F.satBackend flags
        , typecheckAllowIncoherentMatches = F.allowIncoherentMatches flags
        , typecheckLetGen = F.letGen flags
        }

internalConfig :: Flags -> PhaseConfig InternalFlags
internalConfig flags = PhaseConfig (runFlags flags) (runOptions flags)
    InternalFlags
        { internalMaxTIStackDepth = F.maxTIStackDepth flags
        , internalUseProvisoSAT = F.useProvisoSAT flags
        , internalSatBackend = F.satBackend flags
        , internalSimplifyCSyntax = F.simplifyCSyntax flags
        , internalLiftDicts = F.liftDicts flags
        , internalBackend = F.backend flags
        , internalUseDPI = F.useDPI flags
        , internalBdir = F.bdir flags
        , internalVdir = F.vdir flags
        , internalShowVersion = F.showVersion flags
        , internalTimeStamps = F.timeStamps flags
        , internalTestAssert = F.testAssert flags
        }

elabConfig :: Flags -> PhaseConfig ElabFlags
elabConfig flags = PhaseConfig (runFlags flags) (runOptions flags)
    ElabFlags
        { elabBackend = F.backend flags
        , elabBdir = F.bdir flags
        , elabFdir = F.fdir flags
        , elabAggImpConds = F.aggImpConds flags
        , elabMethodConditions = F.methodConditions flags
        , elabResetName = F.resetName flags
        , elabRuleNameCheck = F.ruleNameCheck flags
        , elabInlineISyntax = F.inlineISyntax flags
        , elabInlineSimple = F.inlineSimple flags
        , elabOptBool = F.optBool flags
        , elabExpandIf = F.expandIf flags
        , elabIfLift = F.ifLift flags
        , elabRemoveFalseRules = F.removeFalseRules flags
        , elabRemoveEmptyRules = F.removeEmptyRules flags
        , elabStableVerilog = F.stableVerilog flags
        , elabUseProvisoSAT = F.useProvisoSAT flags
        , elabSatBackend = F.satBackend flags
        , elabRedStepsWarnInterval = F.redStepsWarnInterval flags
        , elabRedStepsMaxIntervals = F.redStepsMaxIntervals flags
        , elabMaxTIStackDepth = F.maxTIStackDepth flags
        , elabWarnUndetPred = F.warnUndetPred flags
        , elabExpandATSlimit = F.expandATSlimit flags
        , elabOptFinalPass = F.optFinalPass flags
        }

schedConfig :: Flags -> PhaseConfig SchedFlags
schedConfig flags = PhaseConfig (runFlags flags) (runOptions flags)
    SchedFlags
        { schedBackend = F.backend flags
        , schedBiasMethodScheduling = F.biasMethodScheduling flags
        , schedGenABin = F.genABin flags
        , schedRelaxMethodEarliness = F.relaxMethodEarliness flags
        , schedResource = F.resource flags
        , schedSatBackend = F.satBackend flags
        , schedSchedConds = F.schedConds flags
        , schedSchedTransposed = F.schedTransposed flags
        , schedSchedQueries = F.schedQueries flags
        , schedStrictMethodSched = F.strictMethodSched flags
        , schedStableVerilog = F.stableVerilog flags
        , schedUnsafeAlwaysRdy = F.unsafeAlwaysRdy flags
        , schedRemoveStarvedRules = F.removeStarvedRules flags
        , schedWarnActionShadowing = F.warnActionShadowing flags
        , schedWarnMethodUrgency = F.warnMethodUrgency flags
        , schedShowRangeConflict = F.showRangeConflict flags
        , schedExpandATSlimit = F.expandATSlimit flags
        , schedTimeStamps = F.timeStamps flags
        , schedShowVersion = F.showVersion flags
        }

materializeConfig :: Flags -> PhaseConfig MaterializeFlags
materializeConfig flags = PhaseConfig (runFlags flags) (runOptions flags)
    MaterializeFlags
        { materializeBackend = F.backend flags
        , materializeStableVerilog = F.stableVerilog flags
        , materializeOptUndet = F.optUndet flags
        , materializeUnSpecTo = F.unSpecTo flags
        }

verilogGenConfig :: Flags -> PhaseConfig VerilogGenFlags
verilogGenConfig flags = PhaseConfig (runFlags flags) (runOptions flags)
    VerilogGenFlags
        { verilogGenUseDPI = F.useDPI flags
        , verilogGenStableVerilog = F.stableVerilog flags
        , verilogGenRemoveRWire = F.removeRWire flags
        , verilogGenRemoveCross = F.removeCross flags
        , verilogGenRemoveCReg = F.removeCReg flags
        , verilogGenRemoveReg = F.removeReg flags
        , verilogGenRemoveInoutConnect = F.removeInoutConnect flags
        , verilogGenRemoveUnusedMods = F.removeUnusedMods flags
        , verilogGenRemovePrimModules = F.removePrimModules flags
        , verilogGenRemoveVerilogDollar = F.removeVerilogDollar flags
        , verilogGenKeepFires = F.keepFires flags
        , verilogGenKeepInlined = F.keepInlined flags
        , verilogGenKeepAddSize = F.keepAddSize flags
        , verilogGenInlineBool = F.inlineBool flags
        , verilogGenOptATS = F.optATS flags
        , verilogGenOptSched = F.optSched flags
        , verilogGenOptJoinDefs = F.optJoinDefs flags
        , verilogGenOptIfMux = F.optIfMux flags
        , verilogGenOptIfMuxSize = F.optIfMuxSize flags
        , verilogGenOptMux = F.optMux flags
        , verilogGenOptMuxExpand = F.optMuxExpand flags
        , verilogGenOptMuxConst = F.optMuxConst flags
        , verilogGenOptBitConst = F.optBitConst flags
        , verilogGenOptAggInline = F.optAggInline flags
        , verilogGenOptAndOr = F.optAndOr flags
        , verilogGenOptFinalPass = F.optFinalPass flags
        , verilogGenSatBackend = F.satBackend flags
        , verilogGenSynthesize = F.synthesize flags
        , verilogGenUseNegate = F.useNegate flags
        , verilogGenReadableMux = F.readableMux flags
        , verilogGenFinalcleanup = F.finalcleanup flags
        , verilogGenUnSpecTo = F.unSpecTo flags
        , verilogGenSystemVerilogOutput = F.systemVerilogOutput flags
        , verilogGenVerilogDeclareAllFirst = F.verilogDeclareAllFirst flags
        , verilogGenSemanticPortsComment = F.semanticPortsComment flags
        , verilogGenVdir = F.vdir flags
        , verilogGenVerilogFilter = F.verilogFilter flags
        , verilogGenIfcPath = F.ifcPath flags
        , verilogGenMethodConf = F.methodConf flags
        , verilogGenMethodBVI = F.methodBVI flags
        , verilogGenTimeStamps = F.timeStamps flags
        , verilogGenShowVersion = F.showVersion flags
        }

bluesimGenConfig :: Flags -> PhaseConfig BluesimGenFlags
bluesimGenConfig flags = PhaseConfig (runFlags flags) (runOptions flags)
    BluesimGenFlags
        { bluesimGenBlockCodegen = F.blockCodegen flags
        , bluesimGenGenSysC = F.genSysC flags
        , bluesimGenResetName = F.resetName flags
        , bluesimGenUnSpecTo = F.unSpecTo flags
        , bluesimGenKeepFires = F.keepFires flags
        , bluesimGenDumpFormats = F.dumpFormats flags
        , bluesimGenTimeStamps = F.timeStamps flags
        , bluesimGenShowVersion = F.showVersion flags
        , bluesimGenIfcPath = F.ifcPath flags
        , bluesimGenCdir = F.cdir flags
        , bluesimGenBackend = F.backend flags
        , bluesimGenOptUndet = F.optUndet flags
        , bluesimGenStableVerilog = F.stableVerilog flags
        }

hostCompileConfig :: Flags -> PhaseConfig HostCompileFlags
hostCompileConfig flags = PhaseConfig (runFlags flags) (runOptions flags)
    HostCompileFlags
        { hostCompileGenSysC = F.genSysC flags
        , hostCompileCxxFlags = F.cxxFlags flags
        , hostCompileCDebug = F.cDebug flags
        , hostCompileCIncPath = F.cIncPath flags
        , hostCompileCFlags = F.cFlags flags
        , hostCompileCdir = F.cdir flags
        , hostCompileParallelSimLink = F.parallelSimLink flags
        }

verilogLinkConfig :: Flags -> PhaseConfig VerilogLinkFlags
verilogLinkConfig flags = PhaseConfig (runFlags flags) (runOptions flags)
    VerilogLinkFlags
        { verilogLinkCLibPath = F.cLibPath flags
        , verilogLinkCLibs = F.cLibs flags
        , verilogLinkDefines = F.defines flags
        , verilogLinkVPath = F.vPath flags
        , verilogLinkOFile = F.oFile flags
        , verilogLinkVFlags = F.vFlags flags
        , verilogLinkLinkFlags = F.linkFlags flags
        , verilogLinkUseDPI = F.useDPI flags
        , verilogLinkDumpFormats = F.dumpFormats flags
        , verilogLinkVsim = F.vsim flags
        }

bluesimLinkConfig :: Flags -> PhaseConfig BluesimLinkFlags
bluesimLinkConfig flags = PhaseConfig (runFlags flags) (runOptions flags)
    BluesimLinkFlags
        { bluesimLinkOFile = F.oFile flags
        , bluesimLinkCLibPath = F.cLibPath flags
        , bluesimLinkCLibs = F.cLibs flags
        , bluesimLinkLinkFlags = F.linkFlags flags
        , bluesimLinkDumpFormats = F.dumpFormats flags
        , bluesimLinkCDebug = F.cDebug flags
        , bluesimLinkTimeStamps = F.timeStamps flags
        }

foreignGenConfig :: Flags -> PhaseConfig ForeignGenFlags
foreignGenConfig flags = PhaseConfig (runFlags flags) (runOptions flags)
    ForeignGenFlags
        { foreignGenUseDPI = F.useDPI flags
        , foreignGenVPath = F.vPath flags
        , foreignGenVdir = F.vdir flags
        , foreignGenTimeStamps = F.timeStamps flags
        , foreignGenShowVersion = F.showVersion flags
        }
