-- | The two checkpoints in compilation of a synthesized module.
--
-- 'AModule' is the input to scheduling (the @.bmod@ payload).
-- 'ASModule' combines a scheduled implementation with the information
-- needed to finish its import wrapper. Further lowering prepares it for
-- backends. 'BSched' records the schedule and concrete IR changes to 'AModule'.
--
-- The elaboration checkpoint precedes generation of scheduling proofs;
-- those proofs are discharged before a successful 'BSched' is written.
module AModule
    ( AModuleInfo(..)
    , AModule(..)
    , ASModule(..)
    , BSched(..)
    , BSchedInfo(..)
    , BSchedErrInfo(..)
    , toBSchedInfo
    , fromBSchedInfo
    , toBSchedErrInfo
    , fromBSchedErrInfo
    ) where

import ADumpScheduleInfo (MethodDumpInfo)
import AMaterializePatch (AMaterializePatch)
import AScheduleInfo
    ( AScheduleInfo(..), AScheduleErrInfo(..), ExclusiveRulesDB
    , RuleRelationDB, SchedNode )
import ASchedulePatch (ASchedulePatch)
import ASyntax (APackage, AVInst, ASchedule, ARuleId)
import AUses (MethodUsesMap, RuleUsesMap)
import Backend (Backend)
import CSyntax (CQType)
import Id (Id)
import Position (Position)
import Pragma (PProp)
import RSchedule (RAT)
import VModInfo (VSchedInfo, VPathInfo)

-- | Source identity and declarations shared by both phases. Invocation flags
-- and execution state are explicit inputs to each phase, not artifact data.
data AModuleInfo = AModuleInfo
    { ami_name :: Id
    , ami_original_type :: CQType
    , ami_pragmas :: [PProp]
    , ami_is_function :: Bool
    , ami_source_prefix :: String
    , ami_source_package :: String
    }

-- | Elaborated and cleaned ASyntax, before path analysis and scheduling.
-- The body owns the static instantiation information: external wires,
-- clock/reset/argument descriptions, and interface fields. The source type
-- and wrapper facts belong here too, rather than in the schedule artifact.
-- The true-method facts come from the elaborated interface, before the
-- subsequent transformations. They must survive until wrapper generation.
data AModule = AModule
    { amod_info :: AModuleInfo
    , amod_body :: APackage
    , amod_true_methods :: [Id]
    -- Elaborated primitive template, needed only for conflict-free checks.
    -- Keeping it here lets scheduling and materialization run without an
    -- evaluator, symbol table or definition environment.
    , amod_cf_template :: Maybe AVInst
    }

-- | Scheduled ASyntax and its matching schedule/interface information.
-- This is distinct from 'ASyntax.ASPackage', a later backend representation.
-- Scheduling and validation produce this value before backend lowering;
-- materialization updates its body and schedule together.
data ASModule = ASModule
    { smod_info :: AModuleInfo
    , smod_body :: APackage
    , smod_schedule :: AScheduleInfo
    -- Concrete ordering constraints chosen by the scheduling phase.
    -- Each (rule, method) pair means the method executes before the rule.
    , smod_method_order :: [(ARuleId, ARuleId)]
    -- Preserve the original scheduler's exported view: the existing wrapper
    -- uses this, while code generation uses the subsequently updated schedule.
    , smod_wrapper_schedule :: VSchedInfo
    , smod_method_dump :: MethodDumpInfo
    , smod_path_info :: VPathInfo
    , smod_true_methods :: [Id]
    }

-- | A schedule is bound to the exact encoded .bmod that it was computed
-- from. It stores generated definitions and decisions, not a second body.
-- The final backend includes the compatibility restriction from simCheck.
data BSched = BSched
    { bs_module_hash :: String
    , bs_patch :: ASchedulePatch
    , bs_schedule :: BSchedInfo
    -- Preserve choices made by common lowering as IR, not invocation flags.
    -- Unchanged module objects are reused from the scheduled checkpoint.
    , bs_materialization :: AMaterializePatch
    -- Assumption instrumentation can add method uses/resource allocations.
    -- Nothing means that the original scheduling facts are unchanged.
    , bs_final_schedule :: Maybe BSchedInfo
    -- Preserve the chosen method ordering independently of later flags.
    -- Each (rule, method) pair means the method executes before the rule.
    , bs_method_order :: [(ARuleId, ARuleId)]
    , bs_wrapper_schedule :: VSchedInfo
    , bs_method_dump :: MethodDumpInfo
    -- Paths include edges introduced by the chosen schedule, so this part
    -- of the eventual VModInfo cannot belong to the unscheduled module.
    , bs_path_info :: VPathInfo
    , bs_backend :: Maybe Backend
    }
    | BSchedError
    { bs_module_hash :: String
    , bs_error :: BSchedErrInfo
    }

-- | Persisted scheduling facts. The legacy ExclusiveRulesDB is deliberately
-- absent: readers derive it from the module and these scheduling facts for
-- existing backend consumers.
data BSchedInfo = BSchedInfo
    { bsi_warnings :: [(Position, String, String)]
    , bsi_method_uses_map :: MethodUsesMap
    , bsi_rule_uses_map :: RuleUsesMap
    , bsi_resource_alloc_table :: RAT
    , bsi_sched_order :: [SchedNode]
    , bsi_schedule :: ASchedule
    , bsi_sched_graph :: [(SchedNode, [SchedNode])]
    , bsi_rule_relation_db :: RuleRelationDB
    , bsi_v_sched_info :: VSchedInfo
    } deriving (Show)

-- | Partial facts retained after a scheduling error, likewise without the
-- legacy exclusivity cache. Failed schedules cannot be used for codegen.
data BSchedErrInfo = BSchedErrInfo
    { bsei_warnings :: [(Position, String, String)]
    , bsei_errors :: [(Position, String, String)]
    , bsei_method_uses_map :: MethodUsesMap
    , bsei_rule_uses_map :: RuleUsesMap
    , bsei_resource_alloc_table :: Maybe RAT
    , bsei_sched_order :: Maybe [SchedNode]
    , bsei_schedule :: Maybe ASchedule
    , bsei_sched_graph :: Maybe [(SchedNode, [SchedNode])]
    , bsei_rule_relation_db :: Maybe RuleRelationDB
    , bsei_v_sched_info :: Maybe VSchedInfo
    } deriving (Show)

toBSchedInfo :: AScheduleInfo -> BSchedInfo
toBSchedInfo info = BSchedInfo
    { bsi_warnings = asi_warnings info
    , bsi_method_uses_map = asi_method_uses_map info
    , bsi_rule_uses_map = asi_rule_uses_map info
    , bsi_resource_alloc_table = asi_resource_alloc_table info
    , bsi_sched_order = asi_sched_order info
    , bsi_schedule = asi_schedule info
    , bsi_sched_graph = asi_sched_graph info
    , bsi_rule_relation_db = asi_rule_relation_db info
    , bsi_v_sched_info = asi_v_sched_info info
    }

fromBSchedInfo :: ExclusiveRulesDB -> BSchedInfo -> AScheduleInfo
fromBSchedInfo exclusive info = AScheduleInfo
    { asi_warnings = bsi_warnings info
    , asi_method_uses_map = bsi_method_uses_map info
    , asi_rule_uses_map = bsi_rule_uses_map info
    , asi_resource_alloc_table = bsi_resource_alloc_table info
    , asi_exclusive_rules_db = exclusive
    , asi_sched_order = bsi_sched_order info
    , asi_schedule = bsi_schedule info
    , asi_sched_graph = bsi_sched_graph info
    , asi_rule_relation_db = bsi_rule_relation_db info
    , asi_v_sched_info = bsi_v_sched_info info
    }

toBSchedErrInfo :: AScheduleErrInfo -> BSchedErrInfo
toBSchedErrInfo info = BSchedErrInfo
    { bsei_warnings = asei_warnings info
    , bsei_errors = asei_errors info
    , bsei_method_uses_map = asei_method_uses_map info
    , bsei_rule_uses_map = asei_rule_uses_map info
    , bsei_resource_alloc_table = asei_resource_alloc_table info
    , bsei_sched_order = asei_sched_order info
    , bsei_schedule = asei_schedule info
    , bsei_sched_graph = asei_sched_graph info
    , bsei_rule_relation_db = asei_rule_relation_db info
    , bsei_v_sched_info = asei_v_sched_info info
    }

fromBSchedErrInfo :: BSchedErrInfo -> AScheduleErrInfo
fromBSchedErrInfo info = AScheduleErrInfo
    { asei_warnings = bsei_warnings info
    , asei_errors = bsei_errors info
    , asei_method_uses_map = bsei_method_uses_map info
    , asei_rule_uses_map = bsei_rule_uses_map info
    , asei_resource_alloc_table = bsei_resource_alloc_table info
    , asei_exclusive_rules_db = Nothing
    , asei_sched_order = bsei_sched_order info
    , asei_schedule = bsei_schedule info
    , asei_sched_graph = bsei_sched_graph info
    , asei_rule_relation_db = bsei_rule_relation_db info
    , asei_v_sched_info = bsei_v_sched_info info
    }
