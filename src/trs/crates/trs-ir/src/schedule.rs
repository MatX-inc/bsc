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

use crate::expr::Expr;
use crate::{MethodRef, RuleRef, SchedEntity, StrId};

/// `Sched r` computes r's fire conditions; `Exec r` runs r's body.
/// (`SchedNode`, `AScheduleInfo.hs:218`.)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SchedNode {
    Sched(SchedEntity),
    Exec(SchedEntity),
}

/// A call on a submodule, as the pair of positions naming it.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct SubMethod {
    /// the submodule instance, in this module's instance list
    pub instance: u32,
    /// the method, in that submodule's method list
    pub method: MethodRef,
}

impl SubMethod {
    #[inline]
    pub fn inst_idx(self) -> usize {
        self.instance as usize
    }
}

/// An ordering the static schedule cannot pin across a submodule
/// (`ADynSched`).  Method fusion demands one order while this module's
/// own schedule implies another; the conditions involved are disjoint,
/// so at most one constraint is live per cycle and a guard readable
/// against pre-edge state selects which.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DynSched {
    /// Two rules, each of whose flagged call must precede the other's.
    Pair {
        rule_e: RuleRef,
        /// `rule_e`'s CAN_FIRE, inlined to register reads and constants.
        guard_e: Expr,
        rule_l: RuleRef,
        /// Present when the pair is constrained in both directions;
        /// then neither constraint is live when both guards are false.
        guard_l: Option<Expr>,
        /// The flagged (early, late) calls.
        meths: Vec<(SubMethod, SubMethod)>,
        /// Submodule rules between the flagged calls, by name.  bsc
        /// leaves these unqualified -- which submodule each belongs to
        /// is settled only when the merge places them against the
        /// hierarchy -- so this is the one reference a fragment cannot
        /// resolve to a position on its own.
        between: Vec<StrId>,
    },
    /// One rule making both flagged calls (bsc G0096): it must run
    /// before the submodule rules for its early call and after them for
    /// its late one.  The rule executes either way, so only the
    /// inactive call's fused edges may drop.
    SelfCall {
        rule: RuleRef,
        /// The rule's predicate AND the early call's condition.
        guard: Expr,
        early: SubMethod,
        late: SubMethod,
        between: Vec<StrId>,
    },
}


/// Per-module (type) schedule information.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Schedule {
    /// One entry per (clock domain, edge) this module participates in.
    pub domains: Vec<ModuleSchedule>,
    /// Esposito conflict lists: a schedulable entity -> the more-urgent
    /// ones whose WILL_FIRE blocks it (`ASchedEsposito`).  Named rather
    /// than positioned because the scheduler ranks interface methods
    /// alongside rules, so an entry here is not always a rule.
    /// Intra-module by construction; already reflected in the WILL_FIRE
    /// defs, carried for verification and diagnostics.
    pub conflicts: Vec<(SchedEntity, Vec<SchedEntity>)>,
    /// Rules whose bodies call a system or foreign task.  A task's
    /// output is observable, so the relative order of two such rules is
    /// observable too, and merging this module's schedule into a design
    /// must preserve it.  Sorted.
    #[serde(default)]
    pub task_rules: Vec<RuleRef>,
    /// Rules calling $finish, $fatal or $stop.  Stronger than
    /// `task_rules`: the stop suppresses output from anything ordered
    /// after it in the same instant, so these pin order against every
    /// task-bearing rule rather than only against each other.  Sorted.
    #[serde(default)]
    pub finish_rules: Vec<RuleRef>,
    /// The module's own schedule graph: each node paired with the nodes
    /// that must follow it (`asi_sched_graph`).  The design-level merge
    /// reads this; the segments above are what the merge produces.
    #[serde(default)]
    pub sched_graph: Vec<(SchedNode, Vec<SchedNode>)>,
    /// Entities provably disjoint from each other, so their order is
    /// free (`asi_exclusive_rules_db`, reduced to the disjoint half the
    /// merge actually reads).  Methods appear here as well as rules,
    /// since the scheduler ranks both.
    #[serde(default)]
    pub disjoint_rules: Vec<(SchedEntity, Vec<SchedEntity>)>,
    /// Exec-pair edges that exist only because two rules call foreign
    /// functions whose relative order was an arbitrary choice.  The
    /// merge may drop these to break a cycle, and nothing else about
    /// the rule-relation database is read.
    #[serde(default)]
    pub ffunc_edges: Vec<(SchedEntity, SchedEntity)>,
    /// Pairs the static schedule cannot order across a submodule.
    #[serde(default)]
    pub dyn_scheds: Vec<DynSched>,
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

/// A rule named from the design root.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct QualRule {
    /// Interned instance path ("" = the top module).
    pub instance: StrId,
    /// The rule's position in the rule list of the module at
    /// `instance`.
    pub rule: RuleRef,
}

/// The per-link, per-(clock, edge) interleaving of instance segments —
/// what the top-level edge function executes.  Instance paths are interned
/// dotted strings ("a.b.c").
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
    /// Clock-crossing rules run in the after-edge function.
    pub early: Vec<QualRule>,
    /// Cross-module disjoint pairs whose ME inhibitors depend on this
    /// composed order: the first rule's CAN_FIRE inhibits the second
    /// (which executes later in this composition).
    pub cross_inhibits: Vec<(QualRule, QualRule)>,
    /// Dynamic scheduling (bsc G0100-class designs): guarded alternative
    /// interleavings for this (clock, edge).  The per-cycle execution
    /// order of rules whose cross-boundary ordering constraint is
    /// condition-disjoint cannot be pinned at link time; the compiler
    /// proves the conflicting method calls never co-execute in one cycle,
    /// so each cycle has SOME valid order — selected here at run time.
    /// At each edge the runtime evaluates the alternatives' guards in
    /// order against pre-edge state (before any rule of this composition
    /// runs) and walks the first match; when none matches, or `alts` is
    /// empty, the base `entries`/`cross_inhibits` apply.  Absent from
    /// pre-dynamic .bir files (serde default keeps them decodable).
    #[serde(default)]
    pub alts: Vec<SchedAlt>,
}

/// One guarded alternative interleaving of a `Composition`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SchedAlt {
    /// Instance path the guard is evaluated in ("" = the top module).
    pub guard_inst: StrId,
    /// Module-scoped guard over that instance (typically: the flagged
    /// rule's predicate AND its method call's condition).  The exporter
    /// must guarantee condition-stability: the guard's cone reads only
    /// state that no rule of this composition writes mid-edge
    /// (registers, not wires/EN/fire signals), so evaluating it before
    /// the walk equals evaluating it at the constraint's position.
    pub guard: Expr,
    /// The interleaving to execute when the guard holds.  Same
    /// (instance, domain, segment) reference space as the base entries.
    pub entries: Vec<CompositionEntry>,
    /// Order-derived ME inhibitors for THIS interleaving (the base
    /// `cross_inhibits` encode the base order and do not apply).
    pub cross_inhibits: Vec<(QualRule, QualRule)>,
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
