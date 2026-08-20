//! The static schedule — exported *hierarchically*.
//!
//! bsc's link-time merge (`SimExpand.mergeSchedules`) conceptually produces
//! one global order per clock domain, but exporting that flat order would
//! make every instance's internal scheduling manifest at the top level: a
//! grid of N tiles would export N copies of the tile's rule order, and the
//! top-level artifact would scale with instance count — the monolithic-
//! schedule problem reborn in the wire format.
//!
//! The factoring trick: the only points where a module's internal execution
//! order interacts with the outside world are its *interface methods* —
//! cross-boundary constraints attach to method nodes, which the merge fuses
//! into the calling parent rules (`SimExpand.hs:1040-1076`).  So a module's
//! internal order can be split into **segments** at the positions its
//! method nodes occupy in its own schedule, and the whole-design order
//! becomes a **composition**: an interleaving of (instance, segment)
//! references with the parent's own rule execution.  Segment structure is
//! per module *type* (shared by all instances, cacheable); the composition
//! is per link and scales with instances × segments (≈ methods), not
//! instances × rules.
//!
//! Two schedule facts do not factor by module type and live at composition
//! level instead:
//! - cross-module disjointness: the merge derives parent-rule ↔ child-rule
//!   disjoint pairs through method use (`combineSchedDRDB`,
//!   `SimExpand.hs:1362-1429`); the ME inhibitors for those pairs depend on
//!   the composed order, so they are exported as qualified pairs.
//! - primitive tick ordering across instances (producers before consumers,
//!   `sortTickCalls`) and clock-crossing "early" rules.

use serde::{Deserialize, Serialize};

use crate::StrId;

/// `Sched r` computes r's fire conditions; `Exec r` runs r's body.
/// (`SchedNode`, `AScheduleInfo.hs:218`.)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SchedNode {
    Sched(StrId),
    Exec(StrId),
}

/// Per-module (type) schedule information.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Schedule {
    /// One entry per (clock domain, edge) this module participates in.
    pub domains: Vec<ModuleSchedule>,
    /// Esposito conflict lists: rule -> more-urgent conflicting rules whose
    /// WILL_FIRE blocks it (`ASchedEsposito`).  Intra-module by
    /// construction; already reflected in the WILL_FIRE defs, carried for
    /// verification and diagnostics.
    pub conflicts: Vec<(StrId, Vec<StrId>)>,
}

/// A module's execution order within one clock domain and edge, split into
/// segments at its interface-method cut points.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleSchedule {
    pub domain: u32,
    pub posedge: bool,
    /// Ordered; execution of segment k+1 follows the interface activity
    /// named in segment k's `cut`.
    pub segments: Vec<Segment>,
    /// This module's own primitive-instance ticks, in intra-module order.
    /// Cross-instance ordering is the composition's job.
    pub ticks: Vec<TickCall>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Segment {
    /// Sched/Exec nodes over this module's own rules, in execution order.
    pub nodes: Vec<SchedNode>,
    /// Interface methods whose (parent-fused) execution sits between this
    /// segment and the next.  Empty for the final segment.
    pub cut: Vec<StrId>,
}

/// A tick on a primitive instance (`di_prims`; `doTickCall`,
/// `SimMakeCBlocks.hs:618`).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TickCall {
    pub instance: StrId,
    pub port: StrId,
}

/// The per-link, per-(clock, edge) interleaving of instance segments —
/// what the top-level edge function executes.  Instance paths are interned
/// dotted strings ("a.b.c"); rule paths are "a.b.RL_r".
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Composition {
    /// Interned name of this composition's canonical clock oscillator.
    pub clock: StrId,
    pub posedge: bool,
    /// Ordered (instance, segment) references.  Each instance's segments
    /// appear in order; runs are maximized so the common case is one entry
    /// per instance per edge.
    pub entries: Vec<CompositionEntry>,
    /// Cross-instance tick order, producers before consumers.
    pub ticks: Vec<QualifiedTick>,
    /// Clock-crossing rules run in the after-edge function (qualified).
    pub early: Vec<StrId>,
    /// Cross-module disjoint pairs (qualified rule paths) whose ME
    /// inhibitors depend on this composed order: the first rule's CAN_FIRE
    /// inhibits the second (which executes later in this composition).
    pub cross_inhibits: Vec<(StrId, StrId)>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompositionEntry {
    /// Interned instance path ("" = the top module itself).
    pub instance: StrId,
    /// Clock-domain id within that instance's module — selects which
    /// `ModuleSchedule` in `Schedule::domains` the segment index refers
    /// to (segment numbering is per domain).
    pub domain: u32,
    /// Index into that domain's `ModuleSchedule::segments`.
    pub segment: u32,
}

/// A tick with a design-relative instance path.  `reset` marks the
/// conditional reset ticks (mkResetTickStmt): while the prim's reset is
/// asserted, each posedge of its clock loads the reset state.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualifiedTick {
    pub instance: StrId,
    pub prim: StrId,
    pub port: StrId,
    pub reset: bool,
    /// Gate of the prim's clock: the tick call's gate_value argument.
    /// None = constant true (ungated).
    pub gate: Option<crate::expr::Expr>,
}
