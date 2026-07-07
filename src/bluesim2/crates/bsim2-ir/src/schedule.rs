//! The static schedule — what `AScheduleInfo`/`SimSchedule` carry
//! (`AScheduleInfo.hs:48-75`, `SimPackage.hs:113-123`).

use serde::{Deserialize, Serialize};

use crate::StrId;

/// `Sched r` computes r's fire conditions; `Exec r` runs r's body.
/// The flattened order interleaves both kinds (`SchedNode`,
/// `AScheduleInfo.hs:218`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SchedNode {
    Sched(StrId),
    Exec(StrId),
}

/// Per-module schedule information.  The link planner merges these across
/// the hierarchy per clock domain (as `SimExpand.mergeSchedules` does) into
/// `MergedSchedule`s that drive codegen.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Schedule {
    /// Combined urgency+earliness order over Sched/Exec nodes.
    pub order: Vec<SchedNode>,
    /// Dependency graph edges: node -> predecessors.  Kept so the merge can
    /// re-flatten across module boundaries instead of trusting `order`
    /// blindly.
    pub graph: Vec<(SchedNode, Vec<SchedNode>)>,
    /// Esposito conflict lists: rule -> more-urgent conflicting rules whose
    /// WILL_FIRE blocks it (`ASchedEsposito`).
    pub conflicts: Vec<(StrId, Vec<StrId>)>,
    /// Rules with disjoint (mutually exclusive) predicates; drives the
    /// ME-inhibitor insertion required by destructive execution
    /// (`SimMakeCBlocks.hs:1636`).
    pub disjoint: Vec<(StrId, Vec<StrId>)>,
    /// Edges that exist only to align $display order between backends and
    /// may be dropped to break cycles (`CFFuncArbitraryChoice`).
    pub droppable_edges: Vec<(SchedNode, SchedNode)>,
}
