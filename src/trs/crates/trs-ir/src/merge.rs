//! Design-level schedule merge.
//!
//! bsc combines each synthesis unit's partial schedule into one design
//! schedule (`SimExpand.mergeSchedules`), and the exporter flattens that
//! into the per-(clock, edge) compositions a run walks.  Both read only
//! per-module data plus the instance hierarchy, so both can run here
//! instead -- which is what lets a `.bir` describe one module and the
//! link assemble the rest.
//!
//! While the exporter still writes `Design::compositions`, that field is
//! the oracle: `diff` reports where this merge disagrees with it, and
//! the export goes away once it does not.

use std::collections::{BTreeMap, BTreeSet};

use crate::psq;
use crate::schedule::{Composition, CompositionEntry, QualifiedTick, SchedNode};
use crate::{ClockArg, Design, Expr, RuleRef, SchedEntity, StrId};

/// The instance hierarchy, as the merge walks it: for each instance, its
/// path, the module it is of, and its children.
pub struct Hier {
    /// (path, module index), parent before child.
    pub insts: Vec<(String, usize)>,
    /// children of `insts[i]`: (instance index, that instance's
    /// position in the parent module's own instance list).  The second
    /// is what a parent's uses are keyed by, and recording it here
    /// beats recovering it by matching path text later.
    pub kids: Vec<Vec<(usize, u32)>>,
    /// the same edge from the other end: which instance holds
    /// `insts[i]`, and where in its instance list.  `None` for the top.
    pub parents: Vec<Option<(usize, u32)>>,
}

impl Hier {
    /// Walk the design from `top`, following each module's instances.
    pub fn of(design: &Design) -> Option<Hier> {
        let top = design.modules.iter().position(|m| m.name == design.top)?;
        let mut h = Hier {
            insts: vec![(String::new(), top)],
            kids: vec![Vec::new()],
            parents: vec![None],
        };
        let mut queue = vec![0usize];
        while let Some(i) = queue.pop() {
            let (path, mir) = (h.insts[i].0.clone(), h.insts[i].1);
            for (pos, inst) in design.modules[mir].instances.iter().enumerate() {
                let crate::InstanceKind::Module(xr) = inst.kind else { continue };
                let cname = design.modules[mir].extern_module(xr);
                let Some(cmir) = design.modules.iter().position(|m| m.name == cname)
                else {
                    continue;
                };
                let leaf = design.name(inst.name);
                let cpath =
                    if path.is_empty() { leaf.to_string() } else { format!("{path}.{leaf}") };
                let k = h.insts.len();
                h.insts.push((cpath, cmir));
                h.kids.push(Vec::new());
                h.parents.push(Some((i, pos as u32)));
                h.kids[i].push((k, pos as u32));
                queue.push(k);
            }
        }
        Some(h)
    }
}

/// A node of the merged graph: a schedule node of a particular
/// instance.  bsc qualifies these with a dotted instance path; an
/// instance index says the same thing without a string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct QNode {
    /// index into `Hier::insts`
    pub inst: u32,
    pub node: SchedNode,
}

/// One module's scheduling inputs, read out of its fragment, before any
/// qualification.  `makeCSIForModule`'s half of the merge.
#[derive(Debug, Default)]
pub struct ModCsi {
    /// node -> the nodes that must follow it
    pub sched: BTreeMap<SchedNode, Vec<SchedNode>>,
    /// entity -> the more-urgent entities whose WILL_FIRE blocks it
    pub conflicts: BTreeMap<SchedEntity, Vec<SchedEntity>>,
    /// entity -> entities provably disjoint from it
    pub disjoint: BTreeMap<SchedEntity, BTreeSet<SchedEntity>>,
    /// Exec pairs ordered only by foreign-call sequencing, droppable to
    /// break a cycle
    pub ffunc: BTreeSet<(SchedEntity, SchedEntity)>,
}

impl ModCsi {
    /// Read one module's inputs.  No merging and no hierarchy: this is
    /// exactly what the fragment says about itself.
    pub fn of(m: &crate::Module) -> ModCsi {
        // the fragment lists each node's predecessors, as bsc's
        // schedule map does; the merge works forwards, so turn it
        // around here.  Every node stays a key even with nothing after
        // it, or a node nothing depends on would drop out of the order.
        let mut sched: BTreeMap<SchedNode, Vec<SchedNode>> =
            m.schedule.sched_graph.iter().map(|(n, _)| (*n, Vec::new())).collect();
        for (n, priors) in &m.schedule.sched_graph {
            for p in priors {
                sched.entry(*p).or_default().push(*n);
            }
        }
        let conflicts =
            m.schedule.conflicts.iter().map(|(e, es)| (*e, es.clone())).collect();
        let disjoint = m
            .schedule
            .disjoint_rules
            .iter()
            .map(|(e, es)| (*e, es.iter().copied().collect()))
            .collect();
        let ffunc = m.schedule.ffunc_edges.iter().copied().collect();
        ModCsi { sched, conflicts, disjoint, ffunc }
    }
}

/// Everything the merge reads, read once: the design's instance tree,
/// and what each module says about itself.
///
/// A design instantiates a module as many times as it likes, and what a
/// module states about itself does not change per instance -- so the
/// cost of reading it should follow the design, not the instance count
/// times the design.  bsc memoises the same way, per module type.
pub struct Inputs<'a> {
    pub design: &'a Design,
    pub hier: Hier,
    csi: Vec<ModCsi>,
    uses: Vec<Uses>,
    domains: Vec<ModDomains>,
    segs: Vec<BTreeMap<SchedNode, (u32, u32, u32)>>,
}

impl<'a> Inputs<'a> {
    /// `None` if the design names no top module, which leaves nothing
    /// to merge.
    pub fn of(design: &'a Design) -> Option<Inputs<'a>> {
        Some(Inputs {
            design,
            hier: Hier::of(design)?,
            csi: design.modules.iter().map(ModCsi::of).collect(),
            uses: design.modules.iter().map(Uses::of).collect(),
            domains: design.modules.iter().map(ModDomains::of).collect(),
            segs: design.modules.iter().map(seg_index).collect(),
        })
    }

    /// The module `insts[i]` is an instance of.
    fn module(&self, i: u32) -> &'a crate::Module {
        &self.design.modules[self.hier.insts[i as usize].1]
    }

    /// The path `insts[i]` is known by.
    fn path(&self, i: u32) -> &str {
        &self.hier.insts[i as usize].0
    }

    /// The schedule inputs and the call map of `insts[i]`'s module:
    /// what a fold needs to absorb one instance.
    fn at(&self, i: usize) -> (&ModCsi, &Uses) {
        let m = self.hier.insts[i].1;
        (&self.csi[m], &self.uses[m])
    }

    /// The domains of `insts[i]`'s module.
    fn doms(&self, i: u32) -> &ModDomains {
        &self.domains[self.hier.insts[i as usize].1]
    }

    fn name(&self, s: StrId) -> &'a str {
        self.design.name(s)
    }
}

/// What a module's schedule needs to know about one clock domain
/// (`DomainInfo`).  Built per module here; the merge combines these
/// across the hierarchy.
#[derive(Debug, Default, Clone)]
pub struct DomainInfo {
    /// the domain's clocks, as (oscillator, gate)
    pub clocks: Vec<(Expr, Expr)>,
    pub rules: Vec<RuleRef>,
    /// the subset of `rules` marked clock-crossing, which run in the
    /// after-edge pass rather than the edge itself
    pub crossing_rules: Vec<RuleRef>,
    /// primitives clocked by this domain: (instance position, the clock
    /// argument that puts it here)
    pub prims: Vec<(u32, ClockArg)>,
    /// the subset of `prims` whose clock carries a reset, so their ticks
    /// are reset ticks
    pub prim_resets: Vec<(u32, StrId)>,
    /// interface output clocks this domain drives: (port name, osc)
    pub output_clocks: Vec<(StrId, Expr)>,
}

/// One module's domains (`makeDomainMaps`).
#[derive(Debug, Default)]
pub struct ModDomains {
    /// oscillator -> domain id.  A `Vec` rather than a map because
    /// `Expr` is only `PartialEq` and a module has a handful of
    /// domains; bsc keys the same lookup on the oscillator too.
    by_osc: Vec<(Expr, u32)>,
    /// domain id -> what is in it
    pub info: BTreeMap<u32, DomainInfo>,
}

impl ModDomains {
    /// The domain an oscillator belongs to, or None for `noClock`,
    /// whose oscillator is the constant false and which has no domain.
    pub fn domain_of(&self, osc: &Expr) -> Option<u32> {
        if is_no_clock(osc) {
            return None;
        }
        // last wins, as it does in the map bsc builds: two domains can
        // name the same oscillator, differing only in their gate, and
        // the index is keyed on the oscillator alone
        self.by_osc.iter().rev().find(|(o, _)| o == osc).map(|(_, d)| *d)
    }

    /// Read one module's domains out of its fragment.
    pub fn of(m: &crate::Module) -> ModDomains {
        let mut d = ModDomains::default();

        // the domains themselves, and the oscillator index over them
        for cd in &m.clock_domains {
            for (osc, _) in &cd.clocks {
                d.by_osc.push((osc.clone(), cd.id));
            }
            d.info.entry(cd.id).or_default().clocks.extend(cd.clocks.iter().cloned());
        }

        // rules, by the domain their wire properties put them in
        for (i, r) in m.rules.iter().enumerate() {
            let e = d.info.entry(r.clock_domain).or_default();
            e.rules.push(RuleRef(i as u32));
            if r.crossing {
                e.crossing_rules.push(RuleRef(i as u32));
            }
        }

        // primitives, by the domain of each clock they are wired with
        for (i, inst) in m.instances.iter().enumerate() {
            if !matches!(inst.kind, crate::InstanceKind::Prim(_)) {
                continue;
            }
            for ca in &inst.clock_args {
                let Some(osc) = inst.args.get(ca.arg as usize).and_then(clock_osc) else {
                    continue;
                };
                let Some(dom) = d.domain_of(osc) else { continue };
                let e = d.info.entry(dom).or_default();
                e.prims.push((i as u32, *ca));
                if ca.has_reset {
                    e.prim_resets.push((i as u32, ca.name));
                }
            }
        }

        // interface output clocks
        for (name, osc) in &m.ifc_clocks {
            if let Some(dom) = d.domain_of(osc) {
                d.info.entry(dom).or_default().output_clocks.push((*name, osc.clone()));
            }
        }
        d
    }
}

/// `noClock`'s oscillator is the constant false, and it has no domain.
fn is_no_clock(osc: &Expr) -> bool {
    matches!(osc, Expr::Const { limbs, .. } if limbs.iter().all(|&w| w == 0))
}

/// The oscillator of a clock-valued instantiation argument.
fn clock_osc(arg: &Expr) -> Option<&Expr> {
    match arg {
        Expr::Clock { osc, .. } => Some(osc),
        _ => None,
    }
}

/// Which of a submodule's methods a schedule node reaches
/// (`mkParentUseMap`), split by where the use sits.  A `Sched` node
/// stands for computing a rule's fire conditions, so it reaches only
/// what the predicate reads; an `Exec` node runs the body, so it
/// reaches what the body calls.
#[derive(Debug, Default)]
pub struct Uses {
    /// (entity, instance position) -> methods its predicate reads
    pub pred: BTreeMap<(SchedEntity, u32), Vec<StrId>>,
    /// (entity, instance position) -> methods its body calls
    pub body: BTreeMap<(SchedEntity, u32), Vec<StrId>>,
}

impl Uses {
    pub fn of(m: &crate::Module) -> Uses {
        let mut u = Uses::default();
        let inst_ix: BTreeMap<StrId, u32> =
            m.instances.iter().enumerate().map(|(i, x)| (x.name, i as u32)).collect();
        let mut acc = Vec::new();
        let mut defs = DefUses::default();

        for (ri, r) in m.rules.iter().enumerate() {
            let e = SchedEntity::Rule(RuleRef(ri as u32));
            acc.clear();
            walk_def(m, r.can_fire, &mut defs, &mut acc);
            record(&mut u.pred, e, &inst_ix, &acc);

            acc.clear();
            for st in r.body.iter() {
                walk_stmt(m, st, &mut defs, &mut acc);
            }
            record(&mut u.body, e, &inst_ix, &acc);
        }

        // A module's interface methods call submodules too, and that is
        // how a call reaches across two boundaries: the middle module's
        // method is what its parent fuses, and what the method reaches
        // has to be in the middle module's graph by then.
        for (mi, meth) in m.methods.iter().enumerate() {
            let e = SchedEntity::Method(crate::MethodRef(mi as u32));
            acc.clear();
            if let Some(rdy) = &meth.ready {
                walk_expr(m, rdy, &mut defs, &mut acc);
            }
            record(&mut u.pred, e, &inst_ix, &acc);

            acc.clear();
            for st in &meth.body {
                walk_stmt(m, st, &mut defs, &mut acc);
            }
            if let Some(res) = &meth.result {
                walk_expr(m, res, &mut defs, &mut acc);
            }
            record(&mut u.body, e, &inst_ix, &acc);
        }
        u
    }
}

/// What each of a module's defs reaches, worked out once.
///
/// A def that many rules read is walked once, not once per reader --
/// bsc keeps the same map and looks every `ASDef` up in it.
#[derive(Default)]
struct DefUses {
    reaches: BTreeMap<StrId, Vec<(StrId, StrId)>>,
    /// defs whose answer is still being worked out, so a def graph that
    /// refers back to itself stops rather than recurses forever
    open: BTreeSet<StrId>,
}

fn record(
    into: &mut BTreeMap<(SchedEntity, u32), Vec<StrId>>,
    e: SchedEntity,
    inst_ix: &BTreeMap<StrId, u32>,
    acc: &[(StrId, StrId)],
) {
    for (inst, meth) in acc {
        let Some(&i) = inst_ix.get(inst) else { continue };
        let v = into.entry((e, i)).or_default();
        if !v.contains(meth) {
            v.push(*meth);
        }
    }
}

/// Follow a def reference into its expression.  bsc resolves the same
/// way, through the module's def graph.
fn walk_def(
    m: &crate::Module,
    name: StrId,
    defs: &mut DefUses,
    out: &mut Vec<(StrId, StrId)>,
) {
    if !defs.reaches.contains_key(&name) {
        if !defs.open.insert(name) {
            return;
        }
        let mut v = Vec::new();
        if let Some(d) = m.def(name) {
            walk_expr(m, &d.expr, defs, &mut v);
        }
        // Deduplicate before storing, keeping first appearances, as
        // bsc's `mergeUses` does at every merge.  Without it a def
        // graph that fans in concatenates its children's answers over
        // and over, and the stored lists grow exponentially with depth.
        let mut seen = BTreeSet::new();
        v.retain(|x| seen.insert(*x));
        defs.open.remove(&name);
        defs.reaches.insert(name, v);
    }
    if let Some(v) = defs.reaches.get(&name) {
        out.extend_from_slice(v);
    }
}

fn walk_expr(
    m: &crate::Module,
    e: &Expr,
    defs: &mut DefUses,
    out: &mut Vec<(StrId, StrId)>,
) {
    match e {
        Expr::Def(n) => walk_def(m, *n, defs, out),
        Expr::MethCall { instance, method, args, .. } => {
            out.push((*instance, *method));
            for a in args {
                walk_expr(m, a, defs, out);
            }
        }
        Expr::Prim { args, .. } => {
            for a in args {
                walk_expr(m, a, defs, out);
            }
        }
        Expr::If { cond, then_, else_, .. } => {
            walk_expr(m, cond, defs, out);
            walk_expr(m, then_, defs, out);
            walk_expr(m, else_, defs, out);
        }
        Expr::ForeignCall { args, .. } => {
            for a in args {
                walk_expr(m, a, defs, out);
            }
        }
        Expr::Case { scrutinee, arms, default, .. } => {
            walk_expr(m, scrutinee, defs, out);
            for (_, e) in arms {
                walk_expr(m, e, defs, out);
            }
            walk_expr(m, default, defs, out);
        }
        Expr::Clock { osc, gate } => {
            walk_expr(m, osc, defs, out);
            walk_expr(m, gate, defs, out);
        }
        Expr::Reset { wire } => walk_expr(m, wire, defs, out),
        // leaves, and references that reach no method call.  Spelled out
        // rather than caught by a wildcard: a variant added later that
        // can hold a call must be considered here, and a compile error
        // is the only thing that will make that happen.
        // Reading an ActionValue's result is not a call: the action
        // side is the use, and counting the read as well would order
        // the reader against a submodule it only takes a value from
        // (`eDomain`, SimExpand.hs).
        Expr::MethValue { .. }
        | Expr::Const { .. }
        | Expr::Port(_)
        | Expr::Param(_)
        | Expr::TaskValue { .. }
        | Expr::Str(_)
        | Expr::Real(_)
        | Expr::Gate { .. }
        | Expr::ClockOut { .. } => {}
    }
}

fn walk_stmt(
    m: &crate::Module,
    st: &crate::Stmt,
    defs: &mut DefUses,
    out: &mut Vec<(StrId, StrId)>,
) {
    use crate::Stmt as S;
    match st {
        S::Def { expr, .. } => walk_expr(m, expr, defs, out),
        S::Action(a) => walk_action(m, a, defs, out),
        S::AvAction { action, .. } => walk_action(m, action, defs, out),
        S::Cond { cond, then_, else_ } => {
            walk_expr(m, cond, defs, out);
            for s in then_.iter().chain(else_.iter()) {
                walk_stmt(m, s, defs, out);
            }
        }
    }
}

fn walk_action(
    m: &crate::Module,
    a: &crate::Action,
    defs: &mut DefUses,
    out: &mut Vec<(StrId, StrId)>,
) {
    use crate::Action as A;
    match a {
        A::MethCall { instance, method, cond, args, .. } => {
            out.push((*instance, *method));
            walk_expr(m, cond, defs, out);
            for x in args {
                walk_expr(m, x, defs, out);
            }
        }
        A::Foreign { cond, args, assumption, .. }
        | A::Task { cond, args, assumption, .. } => {
            // an assumption check reads whatever it polices, but that
            // reading orders nothing -- bsc leaves it out of the use
            // map, and counting it here would order the rule against
            // submodules it only observes (`aUses`, SimExpand.hs)
            if *assumption {
                return;
            }
            walk_expr(m, cond, defs, out);
            for x in args {
                walk_expr(m, x, defs, out);
            }
        }
    }
}

/// Whether a schedule node names one of the module's interface
/// methods rather than one of its rules.
fn is_method(n: &SchedNode) -> bool {
    matches!(
        n,
        SchedNode::Sched(SchedEntity::Method(_)) | SchedNode::Exec(SchedEntity::Method(_))
    )
}

/// The node a method reference becomes once qualified to an instance.
fn q(inst: u32, n: SchedNode) -> QNode {
    QNode { inst, node: n }
}

/// A clock domain before unification: one of an instance's own, or one
/// belonging to a primitive inside it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct QDomain {
    /// index into `Hier::insts`
    pub inst: u32,
    /// the primitive within that instance, when the domain is the
    /// primitive's rather than the module's
    pub prim: Option<u32>,
    /// the domain's id within whichever of the two it belongs to
    pub id: u32,
}

impl QDomain {
    fn of_module(inst: u32, id: u32) -> QDomain {
        QDomain { inst, prim: None, id }
    }
}

/// What becomes of a domain on the far side of an instantiation
/// (`combineDomainInfoMap`'s substitution half).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fate {
    /// It is the same domain as one of the parent's.
    Joins(QDomain),
    /// It is clocked by something the parent wired to `noClock`, which
    /// never ticks, so it is not a domain of the design at all.
    Dropped,
}

/// The clocks on the far side of an instantiation: a submodule's, out
/// of its own fragment, or a primitive's, out of what the instantiating
/// module carries on its behalf.
#[derive(Clone, Copy)]
struct Clocks<'a> {
    inputs: &'a [crate::InputClock],
    domains: &'a [crate::ClockDomain],
    outputs: &'a [(StrId, Expr)],
}

impl Clocks<'_> {
    /// The domain an oscillator belongs to.  Last wins, as it does in
    /// the map bsc builds: two domains can name the same oscillator,
    /// differing only in their gate.
    fn domain_of(&self, osc: &Expr) -> Option<u32> {
        if is_no_clock(osc) {
            return None;
        }
        self.domains
            .iter()
            .rev()
            .find(|cd| cd.clocks.iter().any(|(o, _)| o == osc))
            .map(|cd| cd.id)
    }

    /// The domain a whole clock belongs to, gate included.
    ///
    /// An input clock is matched this way rather than by oscillator
    /// alone, and the difference is not academic: a primitive can
    /// declare a domain around an ungated clock while the module
    /// instantiating it wires a gated one to that input.  bsc finds no
    /// match and leaves the domain standing on its own, which is a
    /// domain of the design.
    fn domain_of_clock(&self, osc: &Expr, gate: &Expr) -> Option<u32> {
        if is_no_clock(osc) {
            return None;
        }
        self.domains
            .iter()
            .rev()
            .find(|cd| cd.clocks.iter().any(|(o, g)| o == osc && g == gate))
            .map(|cd| cd.id)
    }
}

/// Where each domain on the far side of an instantiation ends up.
///
/// A domain reached by one of the parent's clocks is the same domain as
/// the parent's -- that is what keeps a design from having one domain
/// per instance.  Two things reach across the boundary: a clock the
/// parent passes in as an argument, and a clock the far side exports
/// which the parent then uses.  A domain the parent reaches by neither
/// stays its own.
fn child_domain_map(
    parent_dom: &ModDomains,
    parent_inst: u32,
    inst: &crate::Instance,
    child: Clocks<'_>,
    key: impl Fn(u32) -> QDomain,
) -> BTreeMap<QDomain, Fate> {
    use std::collections::btree_map::Entry;
    let mut out = BTreeMap::new();

    // clocks the parent passes in
    for ca in &inst.clock_args {
        // the same clock as the far side names it: the port its
        // oscillator arrives on
        let Some(ic) = child.inputs.iter().find(|c| c.name == ca.name) else {
            continue;
        };
        let gate = match ic.gate {
            Some(g) => Expr::Port(g),
            None => Expr::Const { width: 1, limbs: vec![1] },
        };
        let Some(cd) = child.domain_of_clock(&Expr::Port(ic.osc), &gate) else {
            continue;
        };

        // and the parent's clock on this argument, which decides the
        // domain -- unless the parent passed noClock, in which case the
        // domain has no clock behind it and goes away
        let Some(arg) = inst.args.get(ca.arg as usize).and_then(clock_osc) else {
            continue;
        };
        // one domain can hold more than one input clock.  Being wired
        // to noClock settles it whichever clock says so; otherwise the
        // first one decides, as it does in bsc.
        let fate = match parent_dom.domain_of(arg) {
            Some(pd) => Fate::Joins(QDomain::of_module(parent_inst, pd)),
            None => Fate::Dropped,
        };
        match out.entry(key(cd)) {
            Entry::Vacant(e) => {
                e.insert(fate);
            }
            Entry::Occupied(mut e) => {
                if fate == Fate::Dropped {
                    e.insert(fate);
                }
            }
        }
    }

    // clocks the far side exports.  The parent names such a clock by
    // the instance that exports it, so the two sides meet on the port
    // name it leaves on.
    for (port, osc) in child.outputs {
        let Some(cd) = child.domain_of(osc) else { continue };
        if out.contains_key(&key(cd)) {
            // a domain with an input clock is settled by that; an
            // output clock only speaks for a domain nothing was wired
            // into
            continue;
        }
        let outer = Expr::ClockOut { instance: inst.name, clock: *port };
        let Some(pd) = parent_dom.domain_of(&outer) else { continue };
        out.insert(key(cd), Fate::Joins(QDomain::of_module(parent_inst, pd)));
    }

    out
}

/// An entity of a particular instance./// An entity of a particular instance./// An entity of a particular instance.
pub type QEntity = (u32, SchedEntity);

fn is_method_entity(e: &SchedEntity) -> bool {
    matches!(e, SchedEntity::Method(_))
}

/// Every edge reversed, for finding a node's predecessors.
fn reverse(g: &BTreeMap<QNode, Vec<QNode>>) -> BTreeMap<QNode, Vec<QNode>> {
    let mut r: BTreeMap<QNode, Vec<QNode>> = BTreeMap::new();
    for (from, tos) in g {
        for to in tos {
            r.entry(*to).or_default().push(*from);
        }
    }
    r
}

/// Merge a child's Esposito conflicts into its parent's
/// (`combineSchedConflicts`).
///
/// Only the body matters: a `Sched` node reaches only value methods,
/// whose blockers would show up in the RDY instead.  bsc leaves the
/// blockers direction empty -- rules cannot yet block methods -- so the
/// one live case is a rule the called method blocks, which afterwards
/// is blocked by the caller.
fn combine_conflicts(
    out: &mut BTreeMap<QEntity, Vec<QEntity>>,
    parent_inst: u32,
    child_inst: u32,
    child: &BTreeMap<QEntity, Vec<QEntity>>,
    // (parent rule, the child methods its body calls)
    body_uses: &[(SchedEntity, Vec<crate::MethodRef>)],
) {
    let seam = |e: &QEntity| e.0 == child_inst && is_method_entity(&e.1);

    for (from, tos) in child {
        if seam(from) {
            continue;
        }
        let kept: Vec<QEntity> = tos.iter().filter(|t| !seam(t)).copied().collect();
        out.entry(*from).or_default().extend(kept);
    }

    let mut rev: BTreeMap<QEntity, Vec<QEntity>> = BTreeMap::new();
    for (from, tos) in child {
        for to in tos {
            rev.entry(*to).or_default().push(*from);
        }
    }
    for (user, methods) in body_uses {
        let pq = (parent_inst, *user);
        for m in methods {
            let key = (child_inst, SchedEntity::Method(*m));
            for blockee in rev.get(&key).into_iter().flatten() {
                if !seam(blockee) {
                    out.entry(*blockee).or_default().push(pq);
                }
            }
        }
    }
}

/// A parent rule's calls into one child, as `combineSchedDRDB` reads
/// them: the rule's `Sched` and `Exec` uses merged, keeping only the
/// `Exec` side.  Reading a method's ready signal is not calling it, and
/// a use that reaches only a `Sched` node is exactly such a read.
fn flat_uses(
    cmod: &crate::Module,
    uses: &Uses,
    cpos: u32,
) -> BTreeMap<SchedEntity, Vec<crate::MethodRef>> {
    let mut out = exec_uses(cmod, &uses.pred, cpos);
    for (user, meths) in exec_uses(cmod, &uses.body, cpos) {
        let e = out.entry(user).or_default();
        for m in meths {
            if !e.contains(&m) {
                e.push(m);
            }
        }
    }
    out
}

/// One half of the use map, as calls on one child: the methods each
/// user reaches through a method's `Exec` node.
fn exec_uses(
    cmod: &crate::Module,
    from: &BTreeMap<(SchedEntity, u32), Vec<StrId>>,
    cpos: u32,
) -> BTreeMap<SchedEntity, Vec<crate::MethodRef>> {
    let mut out: BTreeMap<SchedEntity, Vec<crate::MethodRef>> = BTreeMap::new();
    for (&(user, i), meths) in from {
        if i != cpos {
            continue;
        }
        let e = out.entry(user).or_default();
        for m in meths {
            for n in use_to_nodes(cmod, *m) {
                if let SchedNode::Exec(SchedEntity::Method(mr)) = n {
                    if !e.contains(&mr) {
                        e.push(mr);
                    }
                }
            }
        }
    }
    out.retain(|_, v| !v.is_empty());
    out
}

/// Merge a child's disjointness into its parent's
/// (`combineSchedDRDB`).  Disjointness is symmetric, so unlike the
/// conflicts there is no reversed map of the child to consult -- but
/// there is a reversed *use* map, because a call carries disjointness
/// in two ways.  A child rule disjoint from a called method becomes
/// disjoint from the caller; and a child method disjoint from a called
/// method makes the two methods' callers disjoint from each other.
fn combine_disjoint(
    out: &mut BTreeMap<QEntity, BTreeSet<QEntity>>,
    parent_inst: u32,
    child_inst: u32,
    child: &BTreeMap<QEntity, BTreeSet<QEntity>>,
    flat: &BTreeMap<SchedEntity, Vec<crate::MethodRef>>,
) {
    let seam = |e: &QEntity| e.0 == child_inst && is_method_entity(&e.1);

    for (from, tos) in child {
        if seam(from) {
            continue;
        }
        let kept: BTreeSet<QEntity> = tos.iter().filter(|t| !seam(t)).copied().collect();
        out.entry(*from).or_default().extend(kept);
    }

    // child method -> the parent rules that call it
    let mut rev: BTreeMap<crate::MethodRef, Vec<SchedEntity>> = BTreeMap::new();
    for (user, meths) in flat {
        for m in meths {
            rev.entry(*m).or_default().push(*user);
        }
    }

    for (user, meths) in flat {
        let pq = (parent_inst, *user);
        for m in meths {
            let key = (child_inst, SchedEntity::Method(*m));
            let Some(ds) = child.get(&key) else { continue };
            let mut disjoints: Vec<QEntity> = Vec::new();
            for d in ds {
                if seam(d) {
                    let SchedEntity::Method(dm) = d.1 else { continue };
                    disjoints.extend(
                        rev.get(&dm).into_iter().flatten().map(|r| (parent_inst, *r)),
                    );
                } else {
                    disjoints.push(*d);
                }
            }
            for d in disjoints {
                out.entry(pq).or_default().insert(d);
                out.entry(d).or_default().insert(pq);
            }
        }
    }
}

/// Merge a child's droppable foreign-function edges into its parent's
/// (`combineSchedRuleRelDB`, reduced to the one thing `isFFuncEdge`
/// asks of it).
///
/// Each side of a pair expands: one of the child's own methods becomes
/// every parent entity that calls it, anything else stays as it is.  A
/// pair between two methods therefore becomes a pair between their
/// callers, and one between a method and a rule becomes several.
fn combine_ffunc(
    out: &mut BTreeSet<(QEntity, QEntity)>,
    parent_inst: u32,
    child_inst: u32,
    child: &BTreeSet<(QEntity, QEntity)>,
    flat: &BTreeMap<SchedEntity, Vec<crate::MethodRef>>,
) {
    let seam = |e: &QEntity| e.0 == child_inst && is_method_entity(&e.1);

    // child method -> the parent entities whose bodies call it
    let mut callers: BTreeMap<crate::MethodRef, Vec<SchedEntity>> = BTreeMap::new();
    for (user, meths) in flat {
        for m in meths {
            callers.entry(*m).or_default().push(*user);
        }
    }
    let expand = |e: &QEntity| -> Vec<QEntity> {
        if !seam(e) {
            return vec![*e];
        }
        let SchedEntity::Method(mr) = e.1 else { return Vec::new() };
        callers
            .get(&mr)
            .into_iter()
            .flatten()
            .map(|u| (parent_inst, *u))
            .collect()
    };
    for (a, b) in child {
        for x in expand(a) {
            for y in expand(b) {
                out.insert((x, y));
            }
        }
    }
}

/// The child nodes a use of `meth` names.  A use of a method's ready
/// signal is a use of that method's `Sched` node only -- the caller
/// reads whether it can fire.  A use of the method itself reaches both
/// its `Sched` and its `Exec`, because a conditional call means the
/// caller's body must run to decide whether the method is scheduled.
fn use_to_nodes(callee: &crate::Module, meth: StrId) -> Vec<SchedNode> {
    if let Some(i) = callee.methods.iter().position(|m| m.rdy == Some(meth)) {
        return vec![SchedNode::Sched(SchedEntity::Method(crate::MethodRef(i as u32)))];
    }
    match callee.methods.iter().position(|m| m.name == meth) {
        Some(i) => {
            let r = crate::MethodRef(i as u32);
            vec![SchedNode::Sched(SchedEntity::Method(r)), SchedNode::Exec(SchedEntity::Method(r))]
        }
        None => Vec::new(),
    }
}

/// Merge one child instance's schedule graph into its parent's
/// (`combineSchedMap`).
///
/// The child's own edges come across as they are, minus anything
/// touching one of its interface methods.  Then each parent node that
/// calls such a method takes the method's place: the method's
/// predecessors gain an edge to the parent node, and the parent node
/// gains an edge to the method's successors.  Methods do not survive --
/// they are the seam the two schedules are joined along.
///
/// The child graph is whatever the child came to after absorbing its
/// own children, so only the seam instance's methods are removed here;
/// anything deeper was removed when the child absorbed it.
fn combine_sched_map(
    out: &mut BTreeMap<QNode, Vec<QNode>>,
    child_inst: u32,
    child: &BTreeMap<QNode, Vec<QNode>>,
    parent_uses: &[(QNode, Vec<QNode>)],
) {
    let seam = |n: &QNode| n.inst == child_inst && is_method(&n.node);

    for (from, tos) in child {
        if seam(from) {
            continue;
        }
        let kept: Vec<QNode> = tos.iter().filter(|t| !seam(t)).copied().collect();
        out.entry(*from).or_default().extend(kept);
    }

    let rev = reverse(child);
    for (pnode, uses) in parent_uses {
        for u in uses {
            for t in child.get(u).into_iter().flatten() {
                if !seam(t) {
                    out.entry(*pnode).or_default().push(*t);
                }
            }
            for p in rev.get(u).into_iter().flatten() {
                if !seam(p) {
                    out.entry(*p).or_default().push(*pnode);
                }
            }
        }
    }
}

/// The design's schedule graph (`combineSchedInfos`).
///
/// Each instance absorbs its children before its parent absorbs it, so
/// a method call across two boundaries fuses through both.  bsc
/// recurses over module types and re-qualifies the result at each
/// instantiation; walking the instance tree says the same thing, and
/// says plainly that the result grows with the instance count.  What a
/// module says about itself is still read once, through `Inputs`.
pub fn merged_graph(inp: &Inputs) -> (BTreeMap<QNode, Vec<QNode>>, Vec<DynFact>) {
    let n = inp.hier.insts.len();
    let mut sub: Vec<BTreeMap<QNode, Vec<QNode>>> = vec![BTreeMap::new(); n];
    let mut facts: Vec<Vec<DynFact>> = (0..n).map(|_| Vec::new()).collect();

    // a parent's index is always below its children's, so counting down
    // reaches every child before its parent
    for i in (0..n).rev() {
        let (csi, uses) = inp.at(i);
        let mut g: BTreeMap<QNode, Vec<QNode>> = csi
            .sched
            .iter()
            .map(|(n, tos)| (q(i as u32, *n), tos.iter().map(|t| q(i as u32, *t)).collect()))
            .collect();
        let mut mine: Vec<DynFact> = inp
            .module(i as u32)
            .schedule
            .dyn_scheds
            .iter()
            .map(|d| DynFact {
                inst: i as u32,
                sched: d.clone(),
                drops_l: Vec::new(),
                drops_e: Vec::new(),
            })
            .collect();

        for &(c, cpos) in &inp.hier.kids[i] {
            let cmod = inp.module(c as u32);
            let mut pu: Vec<(QNode, Vec<QNode>)> = Vec::new();
            let mut add = |e: SchedEntity, meths: &Vec<StrId>, sched: bool| {
                let ns: Vec<QNode> = meths
                    .iter()
                    .flat_map(|meth| use_to_nodes(cmod, *meth))
                    .map(|n| q(c as u32, n))
                    .collect();
                let node = if sched { SchedNode::Sched(e) } else { SchedNode::Exec(e) };
                pu.push((q(i as u32, node), ns));
            };
            for (&(e, ci), meths) in &uses.pred {
                if ci == cpos {
                    add(e, meths, true);
                }
            }
            for (&(e, ci), meths) in &uses.body {
                if ci == cpos {
                    add(e, meths, false);
                }
            }

            let child = std::mem::take(&mut sub[c]);

            // a self-call fact on this child: the edges its two flagged
            // calls fuse in are what each of its states makes vacuous,
            // and the child's graph is what says so
            let rev = reverse(&child);
            for f in &mut mine {
                let crate::schedule::DynSched::SelfCall { rule, early, late, .. } = &f.sched
                else {
                    continue;
                };
                if early.instance != cpos {
                    continue;
                }
                let caller = q(i as u32, SchedNode::Exec(SchedEntity::Rule(*rule)));
                f.drops_l = fused_edges(&child, &rev, c as u32, caller, late.method);
                f.drops_e = fused_edges(&child, &rev, c as u32, caller, early.method);
            }

            combine_sched_map(&mut g, c as u32, &child, &pu);
            mine.append(&mut facts[c]);
        }
        sub[i] = g;
        facts[i] = mine;
    }

    (sub.swap_remove(0), std::mem::take(&mut facts[0]))
}

/// The design's disjointness (`combineSchedDRDB`), folded the same way.
pub fn merged_disjoint(
    inp: &Inputs,
) -> BTreeMap<QEntity, BTreeSet<QEntity>> {
    let mut sub: Vec<BTreeMap<QEntity, BTreeSet<QEntity>>> =
        vec![BTreeMap::new(); inp.hier.insts.len()];

    for i in (0..inp.hier.insts.len()).rev() {
        let (csi, uses) = inp.at(i);
        let mut d: BTreeMap<QEntity, BTreeSet<QEntity>> = csi
            .disjoint
            .iter()
            .map(|(e, es)| {
                ((i as u32, *e), es.iter().map(|x| (i as u32, *x)).collect())
            })
            .collect();

        for &(c, cpos) in &inp.hier.kids[i] {
            let cmod = &inp.module(c as u32);
            let flat = flat_uses(cmod, uses, cpos);
            let child = std::mem::take(&mut sub[c]);
            combine_disjoint(&mut d, i as u32, c as u32, &child, &flat);
        }
        sub[i] = d;
    }

    sub.swap_remove(0)
}

/// The design's Esposito conflicts (`combineSchedConflicts`), folded
/// the same way.
pub fn merged_conflicts(
    inp: &Inputs,
) -> BTreeMap<QEntity, Vec<QEntity>> {
    let mut sub: Vec<BTreeMap<QEntity, Vec<QEntity>>> =
        vec![BTreeMap::new(); inp.hier.insts.len()];

    for i in (0..inp.hier.insts.len()).rev() {
        let (csi, uses) = inp.at(i);
        let mut c: BTreeMap<QEntity, Vec<QEntity>> = csi
            .conflicts
            .iter()
            .map(|(e, es)| ((i as u32, *e), es.iter().map(|x| (i as u32, *x)).collect()))
            .collect();

        for &(k, cpos) in &inp.hier.kids[i] {
            let cmod = &inp.module(k as u32);
            // conflicts travel through calls only, never through a
            // ready-signal read, so this is the body half alone
            let body: Vec<(SchedEntity, Vec<crate::MethodRef>)> =
                exec_uses(cmod, &uses.body, cpos).into_iter().collect();
            let child = std::mem::take(&mut sub[k]);
            combine_conflicts(&mut c, i as u32, k as u32, &child, &body);
        }
        sub[i] = c;
    }

    sub.swap_remove(0)
}

/// The design's droppable foreign-function pairs
/// (`combineSchedRuleRelDB`), folded the same way.
pub fn merged_ffunc(
    inp: &Inputs,
) -> BTreeSet<(QEntity, QEntity)> {
    let mut sub: Vec<BTreeSet<(QEntity, QEntity)>> =
        vec![BTreeSet::new(); inp.hier.insts.len()];

    for i in (0..inp.hier.insts.len()).rev() {
        let (csi, uses) = inp.at(i);
        let mut e: BTreeSet<(QEntity, QEntity)> = csi
            .ffunc
            .iter()
            .map(|(a, b)| ((i as u32, *a), (i as u32, *b)))
            .collect();

        for &(k, cpos) in &inp.hier.kids[i] {
            let cmod = &inp.module(k as u32);
            let flat = flat_uses(cmod, uses, cpos);
            let child = std::mem::take(&mut sub[k]);
            combine_ffunc(&mut e, i as u32, k as u32, &child, &flat);
        }
        sub[i] = e;
    }

    sub.swap_remove(0)
}

/// One unit of a composition: a run of one instance's own schedule
/// between the interface-method cut points where another instance can
/// interleave.  `(instance, the module's own domain, segment)`.
pub type Unit = (u32, u32, u32);

/// Where each of a module's schedule nodes sits: which domain's
/// segment, and where inside that segment.
fn seg_index(m: &crate::Module) -> BTreeMap<SchedNode, (u32, u32, u32)> {
    let mut out = BTreeMap::new();
    for ms in &m.schedule.domains {
        for (si, seg) in ms.segments.iter().enumerate() {
            for (ni, n) in seg.nodes.iter().enumerate() {
                out.insert(*n, (ms.domain, si as u32, ni as u32));
            }
        }
    }
    out
}

/// The unit a node belongs to, and where it sits inside it.  A node
/// that is not part of any segment -- a top-level interface method,
/// which is a cut point rather than something executed -- has none.
fn resolve(inp: &Inputs, n: QNode) -> Option<(Unit, u32)> {
    let (dom, seg, pos) =
        *inp.segs[inp.hier.insts[n.inst as usize].1].get(&n.node)?;
    Some(((n.inst, dom, seg), pos))
}

/// Both orientations of every disjoint pair.  bsc's own map should
/// already read the same from either side, and it says so, but the
/// derivation below asks from one side only.
fn symmetric(
    d: &BTreeMap<QEntity, BTreeSet<QEntity>>,
) -> BTreeMap<QEntity, BTreeSet<QEntity>> {
    let mut out = d.clone();
    for (a, bs) in d {
        for b in bs {
            out.entry(*b).or_default().insert(*a);
        }
    }
    out
}

/// Intern every name a composition refers to by string.
///
/// A composition names its instances by path and its clock by
/// oscillator name, and reaches both through the string table.  Those
/// are strings the merge composes rather than reads, so nothing
/// guarantees a design already has them: a whole-design .bir does only
/// because the exporter wrote the same compositions.  Interning them
/// up front is what makes those lookups total, and a lookup that can
/// fail is a silently wrong name rather than an error.
pub(crate) fn intern_names(design: &mut Design) {
    let mut names: Vec<String> = Vec::new();
    if let Some(hier) = Hier::of(design) {
        names.extend(hier.insts.iter().map(|(p, _)| p.clone()));
    }
    // an oscillator can be a submodule's output clock, which is named
    // by joining the instance to the clock ("mc$CLK_OUT") -- a string
    // neither half puts in the table on its own
    for m in &design.modules {
        let mut doms: Vec<&crate::ClockDomain> = m.clock_domains.iter().collect();
        for i in &m.instances {
            if let Some(pc) = &i.prim_clocks {
                doms.extend(pc.domains.iter());
            }
        }
        let clocks = doms.iter().flat_map(|d| d.clocks.iter());
        let ifc = m.ifc_clocks.iter().chain(m.ifc_clock_gates.iter());
        for e in clocks
            .flat_map(|(osc, gate)| [osc, gate])
            .chain(ifc.map(|(_, e)| e))
        {
            if let Expr::ClockOut { instance, clock } = e {
                names.push(format!(
                    "{}${}",
                    design.name(*instance),
                    design.name(*clock)
                ));
            }
        }
    }
    for n in &names {
        design.intern(n);
    }
}

/// An instance's path, interned.
fn inst_id(inp: &Inputs, i: u32) -> StrId {
    let path = inp.path(i);
    inp.design
        .str_id(path)
        .unwrap_or_else(|| panic!("instance path {path:?} is not interned"))
}

/// The nodes a unit stands for: the segment's own, in order.
fn unit_nodes<'a>(inp: &'a Inputs, u: Unit) -> &'a [SchedNode] {
    let (inst, dom, seg) = u;
    inp.module(inst)
        .schedule
        .domains
        .iter()
        .find(|ms| ms.domain == dom)
        .and_then(|ms| ms.segments.get(seg as usize))
        .map(|s| s.nodes.as_slice())
        .unwrap_or(&[])
}

/// The mutual-exclusion inhibitors the composed order implies
/// (`mkMERuleInhibits` against the order the backend executes).
///
/// Two disjoint rules never fire together, and the one that runs first
/// is the one whose writes the other must not see -- so a rule is
/// inhibited by every rule disjoint from it whose `Exec` has already
/// happened by the time its own `Sched` comes up.  Which rules those
/// are depends on the composed order, so unlike a module's own
/// inhibitors they cannot be settled until the design is put together.
fn cross_inhibits(
    inp: &Inputs,
    units: &[Unit],
    disjoint: &BTreeMap<QEntity, BTreeSet<QEntity>>,
) -> Vec<(crate::schedule::QualRule, crate::schedule::QualRule)> {
    let name_of = |e: QEntity| -> String {
        let (path, mir) = &inp.hier.insts[e.0 as usize];
        let m = &inp.design.modules[*mir];
        let base = match e.1 {
            SchedEntity::Rule(r) => inp.name(m.rules[r.idx()].name),
            SchedEntity::Method(mr) => inp.name(m.methods[mr.idx()].name),
        };
        if path.is_empty() { base.to_string() } else { format!("{path}.{base}") }
    };
    // a method is not a rule and cannot be inhibited
    let qual = |e: QEntity| -> Option<crate::schedule::QualRule> {
        let SchedEntity::Rule(rule) = e.1 else { return None };
        Some(crate::schedule::QualRule { instance: inst_id(inp, e.0), rule })
    };

    let mut seen: BTreeSet<QEntity> = BTreeSet::new();
    let mut out = Vec::new();
    for &u in units {
        for n in unit_nodes(inp, u) {
            match *n {
                SchedNode::Exec(e) => {
                    seen.insert((u.0, e));
                }
                SchedNode::Sched(e) => {
                    let key = (u.0, e);
                    let Some(ds) = disjoint.get(&key) else { continue };
                    let Some(r) = qual(key) else { continue };
                    // ordered by qualified name, which is how the
                    // disjointness map is keyed and so the order the
                    // pairs come out in
                    let mut inh: Vec<(String, QEntity)> = ds
                        .iter()
                        .filter(|d| seen.contains(d))
                        .map(|d| (name_of(*d), *d))
                        .collect();
                    inh.sort();
                    out.extend(inh.into_iter().filter_map(|(_, d)| Some((qual(d)?, r))));
                }
            }
        }
    }
    out
}

/// One domain's composed order (`deriveComp`, `SimExportIR.hs`).
///
/// The flat merged order interleaves nodes of different instances
/// freely, so it does not collapse into segment runs on its own.
/// Instead the constraints are projected onto whole segments and those
/// are sorted, with ties broken by first appearance in the flat order
/// so the result tracks bsc's own choice.
///
/// Three things constrain a pair of segments.  The graph edges are the
/// schedule proper.  A disjoint pair carries no graph edge, yet the
/// flat order still fixes which state each guard observes, so a
/// `Sched` that precedes the other's `Exec` there must keep doing so.
/// And `$finish` ends output for the rest of the instant, which makes
/// its order against every task-bearing rule observable.
fn derive_entries(
    inp: &Inputs,
    order: &[QNode],
    graph: &BTreeMap<QNode, Vec<QNode>>,
    disjoint: &BTreeMap<QEntity, BTreeSet<QEntity>>,
) -> Result<Vec<Unit>, String> {
    let mut units: Vec<Unit> = Vec::new();
    let mut first_pos: BTreeMap<Unit, usize> = BTreeMap::new();
    for (p, n) in order.iter().enumerate() {
        let Some((u, _)) = resolve(inp, *n) else { continue };
        if first_pos.insert(u, p).is_none() {
            units.push(u);
        }
    }

    let mut edges: BTreeSet<(Unit, Unit)> = BTreeSet::new();
    for (n, tos) in graph {
        let Some((un, _)) = resolve(inp, *n) else { continue };
        for t in tos {
            let Some((ut, _)) = resolve(inp, *t) else { continue };
            if un != ut {
                edges.insert((un, ut));
            }
        }
    }

    // where each entity's two nodes sit in the flat order
    let mut sched_at: BTreeMap<QEntity, usize> = BTreeMap::new();
    let mut exec_at: BTreeMap<QEntity, usize> = BTreeMap::new();
    for (p, n) in order.iter().enumerate() {
        match n.node {
            SchedNode::Sched(e) => sched_at.insert((n.inst, e), p),
            SchedNode::Exec(e) => exec_at.insert((n.inst, e), p),
        };
    }

    for (r, ds) in disjoint {
        let Some(&ps) = sched_at.get(r) else { continue };
        for d in ds {
            let Some(&pe) = exec_at.get(d) else { continue };
            if ps >= pe {
                continue;
            }
            let sn = q(r.0, SchedNode::Sched(r.1));
            let en = q(d.0, SchedNode::Exec(d.1));
            let (Some((su, sj)), Some((eu, ej))) =
                (resolve(inp, sn), resolve(inp, en))
            else {
                continue;
            };
            if su != eu {
                edges.insert((su, eu));
            } else if sj >= ej {
                return Err(format!(
                    "disjoint pair {} / {} straddles a segment against the flat order",
                    qname(inp, sn),
                    qname(inp, en)
                ));
            }
        }
    }

    // $finish/$fatal against every task-bearing rule, pinned to the
    // flat order
    let insts: BTreeSet<u32> = units.iter().map(|(i, _, _)| *i).collect();
    let execs = |sel: fn(&crate::schedule::Schedule) -> &Vec<RuleRef>| -> Vec<(usize, Unit)> {
        let mut out = Vec::new();
        for &i in &insts {
            let m = &inp.module(i);
            for r in sel(&m.schedule) {
                let n = q(i, SchedNode::Exec(SchedEntity::Rule(*r)));
                let (Some(&p), Some((u, _))) =
                    (exec_at.get(&(i, SchedEntity::Rule(*r))), resolve(inp, n))
                else {
                    continue;
                };
                out.push((p, u));
            }
        }
        out
    };
    let finishes = execs(|s| &s.finish_rules);
    let tasks = execs(|s| &s.task_rules);
    for &(fp, fu) in &finishes {
        for &(tp, tu) in &tasks {
            if fp == tp {
                continue;
            }
            let (a, b) = if fp < tp { (fu, tu) } else { (tu, fu) };
            if a != b {
                edges.insert((a, b));
            }
        }
    }

    // Kahn's algorithm, ties broken by first appearance
    let mut succs: BTreeMap<Unit, Vec<Unit>> = BTreeMap::new();
    let mut indeg: BTreeMap<Unit, u32> = units.iter().map(|u| (*u, 0)).collect();
    for (a, b) in &edges {
        succs.entry(*a).or_default().push(*b);
        *indeg.entry(*b).or_insert(0) += 1;
    }
    let key = |u: &Unit| (first_pos.get(u).copied().unwrap_or(usize::MAX), *u);
    let mut ready: BTreeSet<(usize, Unit)> =
        indeg.iter().filter(|(_, d)| **d == 0).map(|(u, _)| key(u)).collect();
    indeg.retain(|_, d| *d != 0);

    let mut out = Vec::with_capacity(units.len());
    while let Some(k) = ready.iter().next().copied() {
        ready.remove(&k);
        let u = k.1;
        out.push(u);
        for v in succs.get(&u).into_iter().flatten() {
            match indeg.get_mut(v) {
                Some(d) if *d == 1 => {
                    indeg.remove(v);
                    ready.insert(key(v));
                }
                Some(d) => *d -= 1,
                None => {}
            }
        }
    }
    if indeg.is_empty() {
        Ok(out)
    } else {
        Err(format!("cyclic segment graph: {} units unplaced", indeg.len()))
    }
}

/// The design's compositions, computed from the fragments.
///
/// One per clock domain, on the rising edge.  Only the composed entries
/// are derived so far; `diff` compares those and names what it did not
/// compare.
pub fn compositions(inp: &Inputs) -> Result<Vec<Composition>, String> {
    let disjoint = merged_disjoint(inp);
    // the disjointness map answers from either side, so a pair reaches
    // the inhibitor walk whichever of the two it was recorded under
    let both_ways = symmetric(&disjoint);
    let up = unified_domains(inp);
    let (graph, facts) = merged_graph(inp);
    let orders = domain_orders(inp, &up, &facts, &graph)?;

    let mut out = Vec::new();
    for d in orders {
        let units =
            derive_entries(inp, &d.order, &d.graph, &disjoint)?;
        let inhibits = cross_inhibits(inp, &units, &both_ways);
        // The format records the clock by name.  The oscillator itself
        // is what this field wants to hold, and what the merge has; the
        // name is what the format spells today.
        // no name at all means the oscillator is not one the format
        // can spell (a constant); a name that is not interned means
        // intern_names missed a case, which is a bug and not a 0
        let clock = match canonical_clock(inp, d.domain)
            .and_then(|osc| osc_name(inp, osc))
        {
            None => 0,
            Some(n) => inp.design.str_id(&n).unwrap_or_else(|| {
                panic!("clock name {n:?} is not interned")
            }),
        };
        let entries_of = |units: Vec<Unit>| -> Vec<CompositionEntry> {
            units
                .into_iter()
                .map(|(i, domain, segment)| CompositionEntry {
                    instance: inst_id(inp, i),
                    domain,
                    segment,
                })
                .collect()
        };
        let mut alts = Vec::new();
        for a in &d.alts {
            let aunits = derive_entries(inp, &a.order, &a.graph, &disjoint)?;
            alts.push(crate::schedule::SchedAlt {
                guard_inst: inst_id(inp, a.inst),
                guard: a.guard.clone(),
                cross_inhibits: cross_inhibits(inp, &aunits, &both_ways),
                entries: entries_of(aunits),
            });
        }
        let (ticks, neg_ticks) = domain_ticks(inp, &up, d.domain);
        out.push(Composition {
            clock,
            posedge: true,
            entries: entries_of(units),
            ticks,
            early: early_rules(inp, &up, d.domain),
            cross_inhibits: inhibits,
            alts,
        });
        // The falling edge of the same clock runs no rules, but it does
        // tick whatever asked to be ticked there.
        if !neg_ticks.is_empty() {
            out.push(Composition {
                clock,
                posedge: false,
                entries: vec![],
                ticks: neg_ticks,
                early: vec![],
                cross_inhibits: vec![],
                alts: vec![],
            });
        }
    }
    Ok(out)
}

/// A node's name as bsc spells it: the dotted instance path, then the
/// rule or method's own name.  The top module's own nodes are bare
/// (`qualifyChildId`, `SimExpand.hs`).
pub fn qname(inp: &Inputs, n: QNode) -> String {
    let (path, mir) = &inp.hier.insts[n.inst as usize];
    let m = &inp.design.modules[*mir];
    let base = match n.node {
        SchedNode::Sched(e) | SchedNode::Exec(e) => match e {
            SchedEntity::Rule(r) => inp.name(m.rules[r.idx()].name),
            SchedEntity::Method(mr) => inp.name(m.methods[mr.idx()].name),
        },
    };
    if path.is_empty() { base.to_string() } else { format!("{path}.{base}") }
}

/// Topologically sort the merged graph the way bsc's `tsort` does
/// (`SCC.ntsort`): repeatedly take a node with no remaining
/// predecessors, popping from a priority search queue keyed by the
/// node's rank in bsc's `SchedNode` order -- every `Sched` before every
/// `Exec`, then by name (`AScheduleInfo.hs`).
///
/// Which of several ready nodes comes out is decided by the queue's
/// shape, not by its key, so the queue is reproduced rather than
/// idealised (see `psq`).  The order between nodes the schedule leaves
/// unordered is observable -- through unguarded primitives and through
/// task output -- so any other tie-break would be a different
/// simulation, not a different spelling of the same one.
///
/// `succs` must hold every node as a key, as bsc's schedule map does.
/// On a cycle, returns the nodes that could not be placed.
fn tsort(
    keys: &BTreeMap<QNode, (bool, String)>,
    succs: &BTreeMap<QNode, Vec<QNode>>,
) -> Result<Vec<QNode>, Vec<QNode>> {
    let mut rank: Vec<(&(bool, String), QNode)> = keys.iter().map(|(n, k)| (k, *n)).collect();
    rank.sort_unstable();
    let by_rank: Vec<QNode> = rank.iter().map(|(_, n)| *n).collect();
    let of_node: BTreeMap<QNode, u32> =
        by_rank.iter().enumerate().map(|(i, n)| (*n, i as u32)).collect();

    // Each node's successors, highest rank first.  bsc builds this map
    // with `fromListWith (++)`, which leaves each list reversed against
    // the order the edges were read in -- and the order matters, because
    // decrementing a priority can change which node wins a match and so
    // the shape of the queue the next pop reads.
    let mut indeg: Vec<u32> = vec![0; by_rank.len()];
    let mut after: Vec<Vec<u32>> = vec![Vec::new(); by_rank.len()];
    for (i, n) in by_rank.iter().enumerate() {
        for t in succs.get(n).into_iter().flatten() {
            if let Some(&j) = of_node.get(t) {
                indeg[j as usize] += 1;
                after[i].push(j);
            }
        }
    }
    for v in &mut after {
        v.sort_unstable_by(|a, b| b.cmp(a));
    }

    let bindings: Vec<(u32, u32)> =
        (0..by_rank.len() as u32).map(|i| (i, indeg[i as usize])).collect();
    let mut q = psq::from_ord_list(&bindings);
    let mut out = Vec::with_capacity(by_rank.len());
    while let Some(((i, p), rest)) = psq::min_view(&q) {
        if p != 0 {
            let placed: BTreeSet<QNode> = out.iter().copied().collect();
            return Err(by_rank.iter().copied().filter(|n| !placed.contains(n)).collect());
        }
        out.push(by_rank[i as usize]);
        q = rest;
        for j in &after[i as usize] {
            q = psq::adjust(&q, *j, |d| d - 1);
        }
    }
    Ok(out)
}

/// The design's execution order for one set of nodes
/// (`flattenCombSchedGraph`, `SimExpand.hs`).
///
/// A cycle is not always fatal: two rules calling foreign functions are
/// ordered by an arbitrary choice, and those edges may be dropped to
/// break one.  Only the ffunc edges among the nodes that could not be
/// placed are considered, and the sort is retried once.
///
/// Returns the order and the graph it was taken from, which is not
/// always the graph passed in: an edge dropped to break a cycle is no
/// longer an ordering, and whatever reads the order next must not see
/// it either.
pub fn flatten(
    inp: &Inputs,
    succs: &BTreeMap<QNode, Vec<QNode>>,
) -> Result<(Vec<QNode>, BTreeMap<QNode, Vec<QNode>>), Vec<QNode>> {
    let mut keys: BTreeMap<QNode, (bool, String)> = BTreeMap::new();
    let note = |n: QNode, keys: &mut BTreeMap<QNode, (bool, String)>| {
        keys.entry(n).or_insert_with(|| {
            (matches!(n.node, SchedNode::Exec(_)), qname(inp, n))
        });
    };
    for (n, tos) in succs {
        note(*n, &mut keys);
        for t in tos {
            note(*t, &mut keys);
        }
    }

    // bsc's schedule map holds every node as a key, and the sort reads
    // its keys to know what it is sorting
    let mut full: BTreeMap<QNode, Vec<QNode>> =
        keys.keys().map(|n| (*n, Vec::new())).collect();
    for (n, tos) in succs {
        full.insert(*n, tos.clone());
    }

    let stuck = match tsort(&keys, &full) {
        Ok(order) => return Ok((order, full)),
        Err(stuck) => stuck,
    };

    let breakable = ffunc_edges(inp);
    let stuck_set: BTreeSet<QNode> = stuck.iter().copied().collect();
    let mut cut = full.clone();
    for (a, b) in &breakable {
        if stuck_set.contains(a) && stuck_set.contains(b) {
            if let Some(tos) = cut.get_mut(a) {
                tos.retain(|t| t != b);
            }
        }
    }
    tsort(&keys, &cut).map(|order| (order, cut))
}

/// Exec-pair edges the merge is allowed to drop: the two rules call
/// foreign functions, and nothing but an arbitrary choice put them in
/// this order.
fn ffunc_edges(inp: &Inputs) -> BTreeSet<(QNode, QNode)> {
    merged_ffunc(inp)
        .into_iter()
        .map(|((ia, a), (ib, b))| (q(ia, SchedNode::Exec(a)), q(ib, SchedNode::Exec(b))))
        .collect()
}

/// Every instance domain, and which unified domain it belongs to.
/// Walking parent-before-child means a child's parent is already
/// resolved when the child is reached, so a chain of instances sharing
/// one clock collapses to a single domain rather than a chain of them.
pub fn unified_domains(
    inp: &Inputs,
) -> BTreeMap<QDomain, Fate> {
    let mut to: BTreeMap<QDomain, Fate> = BTreeMap::new();

    // a parent is walked before its children, so whatever the parent's
    // own domain became is already settled when a child asks -- being
    // clocked by nothing included, which the child inherits
    let settle = |to: &mut BTreeMap<QDomain, Fate>, from: QDomain, fate: Fate| {
        let settled = match fate {
            Fate::Dropped => Fate::Dropped,
            Fate::Joins(up) => *to.get(&up).unwrap_or(&Fate::Joins(up)),
        };
        to.insert(from, settled);
    };

    for (i, (_, mir)) in inp.hier.insts.iter().enumerate() {
        let m = &inp.design.modules[*mir];
        let mdom = &inp.domains[*mir];
        let inst = i as u32;

        // primitives, whose domains this module carries on their behalf
        for (pos, x) in m.instances.iter().enumerate() {
            let Some(pc) = &x.prim_clocks else { continue };
            let child =
                Clocks { inputs: &pc.inputs, domains: &pc.domains, outputs: &pc.outputs };
            let key = |id| QDomain { inst, prim: Some(pos as u32), id };
            for (from, fate) in child_domain_map(mdom, inst, x, child, key) {
                settle(&mut to, from, fate);
            }
        }

        // submodules, which state their own
        for &(c, cpos) in &inp.hier.kids[i] {
            let cmod = &inp.module(c as u32);
            let Some(x) = m.instances.get(cpos as usize) else { continue };
            let child = Clocks {
                inputs: &cmod.input_clocks,
                domains: &cmod.clock_domains,
                outputs: &cmod.ifc_clocks,
            };
            let key = |id| QDomain::of_module(c as u32, id);
            for (from, fate) in child_domain_map(mdom, inst, x, child, key) {
                settle(&mut to, from, fate);
            }
        }
    }
    to
}

/// The domain an instance domain is part of, or `None` if nothing
/// clocks it.
fn resolved(up: &BTreeMap<QDomain, Fate>, d: QDomain) -> Option<QDomain> {
    match up.get(&d) {
        None => Some(d),
        Some(Fate::Dropped) => None,
        Some(Fate::Joins(x)) => Some(*x),
    }
}

/// The design's clock domains after unification: every instance domain
/// mapped to the one domain it is part of, in a stable order.
///
/// This is what `splitCSIByClock` divides the merged schedule by, and
/// so how many compositions a design has.
pub fn merged_domains(
    inp: &Inputs,
    up: &BTreeMap<QDomain, Fate>,
) -> Vec<QDomain> {
    let mut out: Vec<QDomain> = Vec::new();
    for (i, (_, mir)) in inp.hier.insts.iter().enumerate() {
        let m = &inp.design.modules[*mir];
        let inst = i as u32;
        let mut note = |d: QDomain| {
            if let Some(d) = resolved(up, d) {
                if !out.contains(&d) {
                    out.push(d);
                }
            }
        };
        for cd in &m.clock_domains {
            note(QDomain::of_module(inst, cd.id));
        }
        for (pos, x) in m.instances.iter().enumerate() {
            let Some(pc) = &x.prim_clocks else { continue };
            for cd in &pc.domains {
                note(QDomain { inst, prim: Some(pos as u32), id: cd.id });
            }
        }
    }
    out.sort_unstable();
    out
}

/// Which unified domain a node belongs to, or `None` if it is in no
/// clock domain at all.
///
/// A method can be clocked by nothing -- an always-ready value method
/// has no fire signals and nothing to schedule -- and bsc's domain map
/// simply has no entry for it, so it takes part in no composition.
fn node_domain(
    inp: &Inputs,
    up: &BTreeMap<QDomain, Fate>,
    n: QNode,
) -> Option<QDomain> {
    let m = &inp.module(n.inst);
    let d = match n.node {
        SchedNode::Sched(e) | SchedNode::Exec(e) => match e {
            SchedEntity::Rule(r) => m.rules[r.idx()].clock_domain,
            SchedEntity::Method(mr) => m.methods[mr.idx()].clock_domain,
        },
    };
    if !m.clock_domains.iter().any(|cd| cd.id == d) {
        return None;
    }
    resolved(up, QDomain::of_module(n.inst, d))
}

/// One dynamic-scheduling fact, as the merge accumulates it.
///
/// A fact records an ordering the static schedule cannot pin across a
/// submodule, together with the edges each of its per-cycle states
/// makes vacuous -- which can only be worked out where the child's own
/// graph is still to hand, so they are filled in as the fold passes.
pub struct DynFact {
    /// the instance whose module stated it
    pub inst: u32,
    pub sched: crate::schedule::DynSched,
    /// a self-call's fused edges: the late call's, then the early one's
    pub drops_l: Vec<(QNode, QNode)>,
    pub drops_e: Vec<(QNode, QNode)>,
}

/// The edges fusing `r`'s use of `meth` creates, with the multiplicity
/// `combine_sched_map` gives them: everything before the method comes
/// before the rule, and the rule comes before everything after it.
fn fused_edges(
    child: &BTreeMap<QNode, Vec<QNode>>,
    rev: &BTreeMap<QNode, Vec<QNode>>,
    c: u32,
    caller: QNode,
    meth: crate::MethodRef,
) -> Vec<(QNode, QNode)> {
    let e = SchedEntity::Method(meth);
    let nodes = [q(c, SchedNode::Sched(e)), q(c, SchedNode::Exec(e))];
    let mut out = Vec::new();
    for u in nodes {
        for p in rev.get(&u).into_iter().flatten() {
            if !is_method(&p.node) {
                out.push((*p, caller));
            }
        }
    }
    for u in nodes {
        for s in child.get(&u).into_iter().flatten() {
            if !is_method(&s.node) {
                out.push((caller, *s));
            }
        }
    }
    out
}

/// Which edges one per-cycle state of a fact makes vacuous.
enum Vacuous {
    /// a rule that cannot fire: its execution's cross-instance edges go,
    /// in both directions.  Its module-local edges stay, which keeps
    /// the flat order as close to the base as it can be.
    Idle(Vec<QNode>),
    /// a call that cannot happen: the edges fusing it drop, one
    /// occurrence each, so an edge another live use also justifies
    /// keeps its other occurrences.
    Edges(Vec<(QNode, QNode)>),
}

/// The states one fact can be in on a given cycle, each with what it
/// makes vacuous, guardless default last.
fn fact_states(f: &DynFact) -> Vec<(Option<Expr>, Vacuous)> {
    use crate::schedule::DynSched as D;
    match &f.sched {
        D::Pair { rule_e, guard_e, rule_l, guard_l, .. } => {
            let e = q(f.inst, SchedNode::Exec(SchedEntity::Rule(*rule_e)));
            let l = q(f.inst, SchedNode::Exec(SchedEntity::Rule(*rule_l)));
            match guard_l {
                None => vec![
                    (Some(guard_e.clone()), Vacuous::Idle(vec![l])),
                    (None, Vacuous::Idle(vec![e])),
                ],
                Some(gl) => vec![
                    (Some(guard_e.clone()), Vacuous::Idle(vec![l])),
                    (Some(gl.clone()), Vacuous::Idle(vec![e])),
                    (None, Vacuous::Idle(vec![e, l])),
                ],
            }
        }
        D::SelfCall { guard, .. } => vec![
            (Some(guard.clone()), Vacuous::Edges(f.drops_l.clone())),
            (None, Vacuous::Edges(f.drops_e.clone())),
        ],
    }
}

/// Drop a node's cross-instance edges, both ways.
fn drop_idle(g: &mut BTreeMap<QNode, Vec<QNode>>, n: QNode) {
    if let Some(tos) = g.get_mut(&n) {
        tos.retain(|t| t.inst == n.inst);
    }
    for (k, tos) in g.iter_mut() {
        if k.inst != n.inst {
            if let Some(i) = tos.iter().position(|t| *t == n) {
                tos.remove(i);
            }
        }
    }
}

/// Drop one occurrence of an edge.
fn drop_edge(g: &mut BTreeMap<QNode, Vec<QNode>>, (a, b): (QNode, QNode)) {
    if let Some(tos) = g.get_mut(&a) {
        if let Some(i) = tos.iter().position(|t| *t == b) {
            tos.remove(i);
        }
    }
}

fn apply(g: &mut BTreeMap<QNode, Vec<QNode>>, v: &Vacuous) {
    match v {
        Vacuous::Idle(ns) => {
            for n in ns {
                drop_idle(g, *n);
            }
        }
        Vacuous::Edges(es) => {
            for e in es {
                drop_edge(g, *e);
            }
        }
    }
}

/// The conjunction of some guards, spelled as bsc spells it (`aAnds`).
fn all_of(gs: &[Expr]) -> Expr {
    let is_const = |e: &Expr, v: u32| {
        matches!(e, Expr::Const { limbs, .. }
                 if limbs.first().copied().unwrap_or(0) == v
                    && limbs.iter().skip(1).all(|&w| w == 0))
    };
    let konst = |v: u32| Expr::Const { width: 1, limbs: vec![v] };
    if gs.len() == 1 {
        return gs[0].clone();
    }
    if gs.iter().any(|g| is_const(g, 0)) {
        return konst(0);
    }
    let mut args: Vec<Expr> = Vec::new();
    for g in gs.iter().filter(|g| !is_const(g, 1)) {
        if !args.contains(g) {
            args.push(g.clone());
        }
    }
    match args.len() {
        0 => konst(1),
        1 => args.pop().expect("just checked"),
        _ => Expr::Prim { op: crate::PrimOp::And, width: 1, args },
    }
}

/// One domain's base schedule and its guarded alternatives.
///
/// Each fact is in one of a few states on any given cycle, and in each
/// state some of the orderings it imposes are vacuous -- the rule
/// cannot fire, or the call cannot happen.  Every combination of the
/// facts' states gives one interleaving; the combination that needs no
/// guard is the base, and the rest become alternatives the runtime
/// selects by testing guards in order.  Most-active first, so a
/// combination is reached only when every more-active one failed and
/// its guard needs no negations.
fn dyn_alternatives(
    graph: &BTreeMap<QNode, Vec<QNode>>,
    facts: &[&DynFact],
) -> Result<(BTreeMap<QNode, Vec<QNode>>, Vec<(u32, Expr, BTreeMap<QNode, Vec<QNode>>)>), String>
{
    if facts.is_empty() {
        return Ok((graph.clone(), Vec::new()));
    }
    // one alternative's guard is a conjunction read in a single
    // instance, so every fact has to live in the same one
    let inst = facts[0].inst;
    if facts.iter().any(|f| f.inst != inst) {
        return Err("dynamic scheduling across more than one module".to_string());
    }

    let per: Vec<Vec<(Option<Expr>, Vacuous)>> = facts.iter().map(|f| fact_states(f)).collect();
    let mut combos: Vec<Vec<usize>> = vec![Vec::new()];
    for states in &per {
        combos = combos
            .into_iter()
            .flat_map(|c| {
                (0..states.len()).map(move |k| {
                    let mut c = c.clone();
                    c.push(k);
                    c
                })
            })
            .collect();
    }

    let guards_of = |c: &[usize]| -> Vec<Expr> {
        c.iter()
            .enumerate()
            .filter_map(|(i, &k)| per[i][k].0.clone())
            .collect()
    };
    let map_of = |c: &[usize]| -> BTreeMap<QNode, Vec<QNode>> {
        let mut g = graph.clone();
        for (i, &k) in c.iter().enumerate() {
            apply(&mut g, &per[i][k].1);
        }
        g
    };

    let (defaults, mut others): (Vec<_>, Vec<_>) =
        combos.into_iter().partition(|c| guards_of(c).is_empty());
    let [base] = defaults.as_slice() else {
        return Err(format!(
            "dynamic scheduling: {} guardless combinations, expected one",
            defaults.len()
        ));
    };
    others.sort_by_key(|c| std::cmp::Reverse(guards_of(c).len()));
    if others.len() > 15 {
        return Err(format!(
            "dynamic scheduling: {} order combinations, at most 16 are selectable",
            others.len() + 1
        ));
    }
    let alts = others
        .iter()
        .map(|c| (inst, all_of(&guards_of(c)), map_of(c)))
        .collect();
    Ok((map_of(base), alts))
}

/// The design's execution order, one per clock domain
/// (`splitCSIByClock`, `SimExpand.hs`).
///
/// Each domain's nodes are sorted on their own: the merged graph never
/// orders a node against one in another domain, so splitting first and
/// sorting after is the same answer for less work.
pub fn domain_orders(
    inp: &Inputs,
    up: &BTreeMap<QDomain, Fate>,
    facts: &[DynFact],
    graph: &BTreeMap<QNode, Vec<QNode>>,
) -> Result<Vec<DomainSched>, String> {
    // every domain, including those no rule is in: bsc splits by the
    // domain map, not by what the graph happens to mention
    let mut per: BTreeMap<QDomain, BTreeMap<QNode, Vec<QNode>>> =
        merged_domains(inp, up).into_iter().map(|d| (d, BTreeMap::new())).collect();
    for (n, tos) in graph {
        let Some(d) = node_domain(inp, up, *n) else { continue };
        let g = per.entry(d).or_default();
        let kept: Vec<QNode> =
            tos.iter().filter(|t| node_domain(inp, up, **t).is_some()).copied().collect();
        g.insert(*n, kept);
    }

    let stuck = |what: &str, nodes: Vec<QNode>| {
        format!(
            "cycle in the {what}: {} nodes unplaced, first {}",
            nodes.len(),
            nodes.first().map(|n| qname(inp, *n)).unwrap_or_default()
        )
    };

    let mut out = Vec::new();
    for (domain, g) in per {
        // only the facts about rules of this domain bear on it
        let mine: Vec<&DynFact> = facts
            .iter()
            .filter(|f| {
                let r = match &f.sched {
                    crate::schedule::DynSched::Pair { rule_e, .. } => *rule_e,
                    crate::schedule::DynSched::SelfCall { rule, .. } => *rule,
                };
                let n = q(f.inst, SchedNode::Exec(SchedEntity::Rule(r)));
                g.contains_key(&n)
            })
            .collect();

        let (base, alt_specs) = dyn_alternatives(&g, &mine)?;
        let (order, graph) =
            flatten(inp, &base).map_err(|ns| stuck("merged graph", ns))?;
        let mut alts = Vec::new();
        for (inst, guard, amap) in alt_specs {
            let (aorder, agraph) = flatten(inp, &amap)
                .map_err(|ns| stuck("merged graph of an alternative", ns))?;
            alts.push(DomainAlt { inst, guard, graph: agraph, order: aorder });
        }
        out.push(DomainSched { domain, graph, order, alts });
    }
    Ok(out)
}

/// How deep a clock expression's name sits, counting `ch` -- bsc's
/// `numLevels`, which reads the character out of the identifier's own
/// spelling.
///
/// `path` is the instance path the expression belongs to, since a
/// domain that joined from below has its clocks qualified by the
/// instance they came through.
fn num_levels(inp: &Inputs, path: &str, e: &Expr, ch: char) -> usize {
    let count = |s: &str| s.matches(ch).count();
    // a name below the top is spelled "<path>.<name>"
    let qual = if path.is_empty() { 0 } else { count(path) + usize::from(ch == '.') };
    match e {
        Expr::Port(n) => 1 + qual + count(inp.name(*n)),
        // a submodule's output clock is one wire, named "<inst>$<port>"
        Expr::ClockOut { instance, clock } => {
            1 + qual
                + count(inp.name(*instance))
                + count(inp.name(*clock))
                + usize::from(ch == '$')
        }
        Expr::Gate { instance, clock } => {
            1 + qual + count(inp.name(*instance)) + count(inp.name(*clock))
        }
        _ => 0,
    }
}

/// The path a domain's clocks are named from.
fn domain_path(inp: &Inputs, d: QDomain) -> String {
    let (path, mir) = &inp.hier.insts[d.inst as usize];
    match d.prim {
        None => path.clone(),
        Some(pos) => {
            let leaf = inp.name(inp.design.modules[*mir].instances[pos as usize].name);
            if path.is_empty() { leaf.to_string() } else { format!("{path}.{leaf}") }
        }
    }
}

/// The clocks a domain holds, as its owner spells them.
fn domain_clocks<'a>(
    inp: &'a Inputs,
    d: QDomain,
) -> Option<&'a [(Expr, Expr)]> {
    let m = &inp.module(d.inst);
    let domains = match d.prim {
        None => &m.clock_domains,
        Some(pos) => &m.instances[pos as usize].prim_clocks.as_ref()?.domains,
    };
    domains.iter().find(|cd| cd.id == d.id).map(|cd| cd.clocks.as_slice())
}

/// A domain's clock: the one bsc would pick (`findBestClock`).
///
/// Fewest levels in the gate first, so an ungated clock beats a gated
/// one; then fewest levels in the oscillator, then fewest `$`.  What it
/// comes to is the clock named from as high in the design as it can be.
///
/// Only the owner's own clocks are considered.  A domain accumulates
/// the clocks of every domain that joined it, but those joined from
/// below and are named through the instance they came through, so they
/// are strictly deeper and can never win.
fn canonical_clock<'a>(inp: &'a Inputs, d: QDomain) -> Option<&'a Expr> {
    let clocks = domain_clocks(inp, d)?;
    let path = domain_path(inp, d);
    clocks
        .iter()
        .map(|(osc, gate)| {
            (
                (
                    num_levels(inp, &path, gate, '.'),
                    num_levels(inp, &path, osc, '.'),
                    num_levels(inp, &path, osc, '$'),
                ),
                osc,
            )
        })
        .min_by_key(|(k, _)| *k)
        .map(|(_, osc)| osc)
}

/// The name the design records a clock under (bsc's `oscName`): a port
/// by its own name, a submodule's output clock by the wire it arrives
/// on.
fn osc_name(inp: &Inputs, osc: &Expr) -> Option<String> {
    match osc {
        Expr::Port(n) => Some(inp.name(*n).to_string()),
        Expr::ClockOut { instance, clock } => {
            Some(format!("{}${}", inp.name(*instance), inp.name(*clock)))
        }
        _ => None,
    }
}

/// The rules of one domain that run after the edge rather than in it
/// (`di_clock_crossing_rules`): a rule marked `clock_crossing_rule`
/// reads a signal from another domain, so it must see the state the
/// edge leaves behind rather than the state it started from.
fn early_rules(
    inp: &Inputs,
    up: &BTreeMap<QDomain, Fate>,
    d: QDomain,
) -> Vec<crate::schedule::QualRule> {
    let mut out = Vec::new();
    for (i, (_, mir)) in inp.hier.insts.iter().enumerate() {
        let instance = inst_id(inp, i as u32);
        for (ri, r) in inp.design.modules[*mir].rules.iter().enumerate() {
            if !r.crossing {
                continue;
            }
            if resolved(up, QDomain::of_module(i as u32, r.clock_domain)) != Some(d) {
                continue;
            }
            out.push(crate::schedule::QualRule { instance, rule: RuleRef(ri as u32) });
        }
    }
    out
}

/// A clock as the outermost module that still names it sees it
/// (`substInputClockInDomainInfo`, applied all the way up).
///
/// A clock arriving on one of a module's input ports came from its
/// parent, and what a primitive's tick has to read is the parent's
/// gate, not the port it arrived on -- so the clock is followed up the
/// hierarchy until it stops being an input.
fn effective_clock(inp: &Inputs, inst: u32, clk: &Expr) -> (u32, Expr) {
    let mut inst = inst;
    let mut clk = clk.clone();
    loop {
        let Expr::Clock { osc, .. } = &clk else { return (inst, clk) };
        let Expr::Port(p) = &**osc else { return (inst, clk) };
        let m = &inp.module(inst);
        let Some(ic) = m.input_clocks.iter().find(|c| c.osc == *p) else {
            return (inst, clk);
        };
        let Some((pi, pos)) = inp.hier.parents[inst as usize] else { return (inst, clk) };
        let pm = &inp.module(pi as u32);
        let Some(x) = pm.instances.get(pos as usize) else { return (inst, clk) };
        let Some(ca) = x.clock_args.iter().find(|a| a.name == ic.name) else {
            return (inst, clk);
        };
        let Some(arg) = x.args.get(ca.arg as usize) else { return (inst, clk) };
        if !matches!(arg, Expr::Clock { .. }) {
            return (inst, clk);
        }
        inst = pi as u32;
        clk = arg.clone();
    }
}

/// The gate an oscillator is really driven through.
///
/// A clock argument can be written ungated where the thing generating
/// the clock knows better: a clock a primitive hands back is gated by
/// that primitive's own gate output, and the merge substitutes the
/// generator's spelling for the ungated one the instantiation used
/// (`substOutputClockInDomainInfo`).
fn domain_gate(inp: &Inputs, at: u32, osc: &Expr) -> Option<Expr> {
    let m = &inp.module(at);

    // the generator's own spelling, where the clock came out of one
    if let Expr::ClockOut { instance, clock } = osc {
        let own = m
            .instances
            .iter()
            .find(|x| x.name == *instance)
            .and_then(|x| x.prim_clocks.as_ref())
            .and_then(|pc| {
                pc.domains.iter().rev().find_map(|cd| {
                    cd.clocks
                        .iter()
                        .find(|(o, _)| *o == Expr::Port(*clock))
                        .map(|(_, g)| g)
                })
            });
        if let Some(g) = own {
            return Some(match g {
                // a port of the generator, as the module outside names it
                Expr::Port(p) => Expr::Gate { instance: *instance, clock: *p },
                other => other.clone(),
            });
        }
    }

    m.clock_domains
        .iter()
        .rev()
        .find_map(|cd| cd.clocks.iter().find(|(o, _)| o == osc).map(|(_, g)| g.clone()))
}

/// One primitive that has to be ticked: where it is, what it is called,
/// which clock port, and the clock driving it.
#[derive(Clone)]
struct TickPrim {
    /// index into `Hier::insts` of the module holding the primitive
    inst: u32,
    /// the primitive's name
    name: StrId,
    /// the clock argument, and whether it also carries a reset
    arg: ClockArg,
    /// the clock as the outermost module that names it sees it, and
    /// where that is
    clk_inst: u32,
    clk: Expr,
}

/// Every primitive of one domain that needs ticking (`di_prims`), in
/// the order the merge accumulates them: a module's children before
/// the module's own primitives (`joinDomainInfo` puts the child's list
/// first), and within a module in elaboration order.
fn domain_prims(
    inp: &Inputs,
    up: &BTreeMap<QDomain, Fate>,
    d: QDomain,
) -> Vec<TickPrim> {
    fn walk(
        inp: &Inputs,
        up: &BTreeMap<QDomain, Fate>,
        d: QDomain,
        i: usize,
        out: &mut Vec<TickPrim>,
    ) {
        let m = inp.module(i as u32);

        let mut kids = inp.hier.kids[i].clone();
        kids.sort_by_key(|&(_, pos)| m.instances[pos as usize].elab_order);
        for (c, _) in kids {
            walk(inp, up, d, c, out);
        }

        let mdom = inp.doms(i as u32);
        let mut own: Vec<&crate::Instance> = m
            .instances
            .iter()
            .filter(|x| matches!(x.kind, crate::InstanceKind::Prim(_)))
            .collect();
        own.sort_by_key(|x| x.elab_order);
        for x in own {
            // An instance's clock arguments land in the domain in the
            // reverse of the order it declares them; the grouping below
            // reverses again, which is how a dual-port memory ends up
            // ticking its first port first.
            let mut mine = Vec::new();
            for ca in &x.clock_args {
                let Some(arg) = x.args.get(ca.arg as usize) else { continue };
                let Some(osc) = clock_osc(arg) else { continue };
                let Some(dom) = mdom.domain_of(osc) else { continue };
                if resolved(up, QDomain::of_module(i as u32, dom)) != Some(d) {
                    continue;
                }
                let (clk_inst, mut clk) = effective_clock(inp, i as u32, arg);
                if let Expr::Clock { osc, gate } = &mut clk {
                    if let Some(g) = domain_gate(inp, clk_inst, osc) {
                        *gate = Box::new(g);
                    }
                }
                mine.push(TickPrim {
                    inst: i as u32,
                    name: x.name,
                    arg: *ca,
                    clk_inst,
                    clk,
                });
            }
            mine.reverse();
            out.append(&mut mine);
        }
    }

    let mut out = Vec::new();
    walk(inp, up, d, 0, &mut out);
    out
}

/// The gate of a tick's clock, as the tick call reads it: `None` where
/// the clock is ungated.
fn tick_gate(clk: &Expr) -> Option<Expr> {
    let Expr::Clock { gate, .. } = clk else { return None };
    match &**gate {
        Expr::Const { limbs, .. } if limbs.iter().any(|&w| w != 0) => None,
        g => Some(g.clone()),
    }
}

/// Every tick one domain's edge makes, and the ticks the opposite edge
/// of the same clock owns (`doTickCall`'s direction filter, in
/// `sortTickCalls`' order).
fn domain_ticks(
    inp: &Inputs,
    up: &BTreeMap<QDomain, Fate>,
    d: QDomain,
) -> (Vec<QualifiedTick>, Vec<QualifiedTick>) {
    let prims = domain_prims(inp, up, d);

    // Group by the clock driving them.  Within a group the order is
    // the reverse of the order the prims were collected in, which is
    // the order the backend emits its grouped tick calls -- dual-port
    // memory write-write semantics depend on it.
    let mut groups: Vec<(Expr, Vec<TickPrim>)> = Vec::new();
    for p in &prims {
        match groups.iter_mut().find(|(c, _)| *c == p.clk) {
            Some((_, v)) => v.push(p.clone()),
            None => groups.push((p.clk.clone(), vec![p.clone()])),
        }
    }
    for (_, v) in &mut groups {
        v.reverse();
    }

    let path_of = |i: u32, name: StrId| -> String {
        let base = inp.path(i);
        let leaf = inp.name(name);
        if base.is_empty() { leaf.to_string() } else { format!("{base}.{leaf}") }
    };

    // A group whose primitive drives another group's clock gate ticks
    // first, so the second group's gate argument reads the value the
    // first just wrote.
    let gate_src = |clk: &Expr, at: u32| -> Option<String> {
        let Expr::Clock { gate, .. } = clk else { return None };
        match &**gate {
            Expr::Gate { instance, .. } => Some(path_of(at, *instance)),
            _ => None,
        }
    };
    let mut before: Vec<Vec<usize>> = vec![Vec::new(); groups.len()];
    for (h, (clk, prs)) in groups.iter().enumerate() {
        let at = prs.first().map(|p| p.clk_inst).unwrap_or(0);
        let Some(src) = gate_src(clk, at) else { continue };
        for (g, (_, gprs)) in groups.iter().enumerate() {
            if g != h && gprs.iter().any(|p| path_of(p.inst, p.name) == src) {
                before[h].push(g);
            }
        }
    }
    let mut order: Vec<usize> = Vec::with_capacity(groups.len());
    let mut placed = vec![false; groups.len()];
    while order.len() < groups.len() {
        let Some(next) = (0..groups.len())
            .find(|&g| !placed[g] && before[g].iter().all(|&p| placed[p]))
        else {
            // a cycle: no order satisfies it, so keep the rest as they
            // came rather than dropping them
            for g in 0..groups.len() {
                if !placed[g] {
                    order.push(g);
                    placed[g] = true;
                }
            }
            break;
        };
        order.push(next);
        placed[next] = true;
    }

    // the top's own input-clock gates are driven from outside, and a
    // reset tick reads them as always open
    let top_gates: Vec<StrId> = inp
        .design
        .modules
        .iter()
        .find(|m| m.name == inp.design.top)
        .map(|m| m.input_clocks.iter().filter_map(|c| c.gate).collect())
        .unwrap_or_default();

    let tick = |p: &TickPrim, reset: bool| {
        let mut gate = tick_gate(&p.clk);
        if reset {
            if let Some(Expr::Port(g)) = &gate {
                if top_gates.contains(g) {
                    gate = None;
                }
            }
        }
        QualifiedTick {
            instance: inst_id(inp, p.inst),
            prim: p.name,
            port: p.arg.name,
            reset,
            gate,
        }
    };

    let mut pos = Vec::new();
    let mut neg = Vec::new();
    for g in order {
        for p in &groups[g].1 {
            if p.arg.ticks.on_posedge() {
                pos.push(tick(p, false));
            }
            if p.arg.ticks.on_negedge() {
                neg.push(tick(p, false));
            }
        }
    }
    // reset ticks come after the rest, in the order the primitives were
    // collected rather than the order they tick in
    for p in &prims {
        if p.arg.has_reset {
            pos.push(tick(p, true));
        }
    }
    (pos, neg)
}

/// One clock domain's share of the merged schedule.
pub struct DomainAlt {
    /// the instance the guard is read in
    pub inst: u32,
    pub guard: Expr,
    pub graph: BTreeMap<QNode, Vec<QNode>>,
    pub order: Vec<QNode>,
}

pub struct DomainSched {
    pub domain: QDomain,
    /// The graph the order was taken from.  A foreign-function edge
    /// dropped to break a cycle is gone from here too, so nothing
    /// downstream re-imposes an ordering the sort had to give up.
    pub graph: BTreeMap<QNode, Vec<QNode>>,
    pub order: Vec<QNode>,
    /// interleavings the runtime picks between by testing guards
    pub alts: Vec<DomainAlt>,
}

/// Whether the merged graph holds together, independent of whether it
/// is the *right* graph: no method node survives the merge, and every
/// edge lands on a node the graph knows.  Cheap invariants, but they
/// are checkable before anything downstream can compare against the
/// oracle.
pub fn graph_anomalies(inp: &Inputs) -> Vec<String> {
    let (g, _) = merged_graph(inp);
    let mut out = Vec::new();
    // a submodule's methods are the seam the merge joins along and do
    // not survive it; the top's have no caller to be fused into
    if g.keys().any(|k| is_method(&k.node) && k.inst != 0) {
        out.push("merged graph keeps a submodule's method node".to_string());
    }
    if g.values().flatten().any(|n| is_method(&n.node) && n.inst != 0) {
        out.push("merged graph has an edge to a submodule's method node".to_string());
    }
    let dangling = g
        .values()
        .flatten()
        .filter(|n| !g.contains_key(n))
        .count();
    if dangling != 0 {
        out.push(format!("merged graph has {dangling} edges to unknown nodes"));
    }
    if g.is_empty() && !inp.design.modules.iter().all(|m| m.rules.is_empty()) {
        out.push("merged graph is empty but the design has rules".to_string());
    }
    // fusion is the whole point of the merge: a design whose parent
    // rules call submodule methods must end up with edges that cross
    // between instances.  Without this the invariants above would pass
    // on a graph that merely concatenated the modules.
    // ...but only where there is something for the caller to inherit.
    // Fusion gives a caller the non-method neighbours of the methods it
    // calls, so a child whose methods are ordered only against each
    // other -- which bsc does not chain through either -- correctly
    // yields no crossing edge, however many rules it has.
    let calls_a_child = inp.hier.kids.iter().enumerate().any(|(p, kids)| {
        let u = &inp.uses[inp.hier.insts[p].1];
        kids.iter().any(|&(c, cpos)| {
            let cm = &inp.module(c as u32);
            let called: Vec<SchedNode> = u
                .pred
                .iter()
                .chain(u.body.iter())
                .filter(|(&(_, i), _)| i == cpos)
                .flat_map(|(_, ms)| ms.iter().flat_map(|m| use_to_nodes(cm, *m)))
                .collect();
            if called.is_empty() {
                return false;
            }
            let cg = &inp.csi[inp.hier.insts[c].1].sched;
            let mut rev: BTreeMap<SchedNode, Vec<SchedNode>> = BTreeMap::new();
            for (from, tos) in cg {
                for to in tos {
                    rev.entry(*to).or_default().push(*from);
                }
            }
            called.iter().any(|n| {
                let nbrs = cg.get(n).into_iter().flatten();
                let preds = rev.get(n).into_iter().flatten();
                nbrs.chain(preds).any(|x| !is_method(x))
            })
        })
    });
    let crosses = g
        .iter()
        .any(|(from, tos)| tos.iter().any(|t| t.inst != from.inst));
    if calls_a_child && !crosses {
        out.push("no edge crosses an instance, yet a parent calls a child".to_string());
    }

    // no edge may cross a domain: bsc splits the merged graph by
    // domain and treats an edge that survives the split as an error
    // ("schedMap not disjoint"), so an edge that crosses means the
    // domains were read wrong
    {
        let up = unified_domains(inp);
        for (n, tos) in &g {
            let Some(dn) = node_domain(inp, &up, *n) else { continue };
            for t in tos {
                let Some(dt) = node_domain(inp, &up, *t) else { continue };
                if dn != dt {
                    out.push(format!(
                        "edge {:?} -> {:?} crosses from domain {dn:?} to {dt:?}",
                        qname(inp, *n),
                        qname(inp, *t)
                    ));
                }
            }
        }
    }

    // domain unification must land on a domain that is itself final,
    // or a later reader following one hop gets a different answer than
    // one following two
    let u = unified_domains(inp);
    for (from, fate) in &u {
        let Fate::Joins(to) = fate else { continue };
        if u.contains_key(to) {
            out.push(format!(
                "domain {from:?} unifies onto {to:?}, which is itself unified"
            ));
        }
        if from == to {
            out.push(format!("domain {from:?} unifies onto itself"));
        }
    }
    out
}

/// Whether the per-module domain read holds together, independent of
/// the merge: every rule lands in exactly one domain, and every clock a
/// primitive is wired with resolves to one.  A violation means the
/// domains were misread, and nothing downstream of them can be right.
pub fn domain_anomalies(inp: &Inputs) -> Vec<String> {
    let mut out = Vec::new();
    for (k, m) in inp.design.modules.iter().enumerate() {
        let d = &inp.domains[k];
        let placed: usize = d.info.values().map(|i| i.rules.len()).sum();
        if placed != m.rules.len() {
            out.push(format!(
                "module {}: {} rules, {} placed in domains",
                inp.name(m.name),
                m.rules.len(),
                placed
            ));
        }
        for inst in &m.instances {
            if !matches!(inst.kind, crate::InstanceKind::Prim(_)) {
                continue;
            }
            for ca in &inst.clock_args {
                let Some(osc) = inst.args.get(ca.arg as usize).and_then(clock_osc)
                else {
                    out.push(format!(
                        "module {}: instance {} clock arg {} is not a clock",
                        inp.name(m.name),
                        inp.name(inst.name),
                        inp.name(ca.name)
                    ));
                    continue;
                };
                if !is_no_clock(osc) && d.domain_of(osc).is_none() {
                    out.push(format!(
                        "module {}: instance {} clock {} is in no domain",
                        inp.name(m.name),
                        inp.name(inst.name),
                        inp.name(ca.name)
                    ));
                }
            }
        }
    }
    out
}

/// Where the computed compositions differ from the exported ones.
///
/// The comparison is over the whole structure's `Debug` rendering
/// rather than a hand-written field-by-field check.  That is
/// deliberate: a hand-written comparison silently stops covering a
/// field the day someone adds one, and this harness is the only thing
/// standing behind the port.  Exhaustive by construction beats
/// exhaustive by inspection.
/// How one computed composition differs from the exported one, or
/// `None` if they agree.  Reports the first difference with its
/// position, so a regression names a place rather than a volume of
/// output.
fn composition_diff(
    inp: &Inputs,
    i: usize,
    got: &Composition,
    want: &Composition,
) -> Option<String> {
    if got.clock != want.clock {
        return Some(format!(
            "comp {i} clock: computed {:?}, exported {:?}",
            inp.name(got.clock),
            inp.name(want.clock)
        ));
    }
    let shown_early = |c: &Composition| {
        c.early
            .iter()
            .map(|q| (inp.name(q.instance).to_string(), q.rule.idx()))
            .collect::<Vec<_>>()
    };
    if shown_early(got) != shown_early(want) {
        return Some(format!(
            "comp {i} early rules: computed {:?}, exported {:?}",
            shown_early(got),
            shown_early(want)
        ));
    }
    if got.posedge != want.posedge {
        return Some(format!(
            "comp {i} edge: computed {}, exported {}",
            got.posedge, want.posedge
        ));
    }
    if got.alts.len() != want.alts.len() {
        return Some(format!(
            "comp {i} alternatives: computed {}, exported {}",
            got.alts.len(),
            want.alts.len()
        ));
    }
    for (k, (a, b)) in got.alts.iter().zip(want.alts.iter()).enumerate() {
        let shown = |x: &crate::schedule::SchedAlt| {
            (
                inp.name(x.guard_inst).to_string(),
                format!("{:?}", x.guard),
                x.entries
                    .iter()
                    .map(|e| (inp.name(e.instance).to_string(), e.domain, e.segment))
                    .collect::<Vec<_>>(),
                x.cross_inhibits
                    .iter()
                    .map(|(p, q)| {
                        (inp.name(p.instance).to_string(), p.rule.idx(),
                         inp.name(q.instance).to_string(), q.rule.idx())
                    })
                    .collect::<Vec<_>>(),
            )
        };
        let (ga, gb) = (shown(a), shown(b));
        if ga.0 != gb.0 || ga.1 != gb.1 {
            return Some(format!(
                "comp {i} alternative {k} guard: computed {:?} in {:?}, exported {:?} in {:?}",
                ga.1, ga.0, gb.1, gb.0
            ));
        }
        if ga.2 != gb.2 {
            return Some(format!(
                "comp {i} alternative {k} entries: computed {:?}, exported {:?}",
                ga.2, gb.2
            ));
        }
        if ga.3 != gb.3 {
            return Some(format!(
                "comp {i} alternative {k} inhibitors: computed {:?}, exported {:?}",
                ga.3, gb.3
            ));
        }
    }
    let shown_pairs = |c: &Composition| {
        c.cross_inhibits
            .iter()
            .map(|(a, b)| {
                (
                    inp.name(a.instance).to_string(),
                    a.rule.idx(),
                    inp.name(b.instance).to_string(),
                    b.rule.idx(),
                )
            })
            .collect::<Vec<_>>()
    };
    if shown_pairs(got) != shown_pairs(want) {
        let (g, w) = (shown_pairs(got), shown_pairs(want));
        let at = g.iter().zip(w.iter()).position(|(a, b)| a != b);
        return Some(match at {
            Some(k) => format!(
                "comp {i} inhibitor {k}: computed {:?}, exported {:?}",
                g[k], w[k]
            ),
            None => format!(
                "comp {i} inhibitors: computed {} of them, exported {}",
                g.len(),
                w.len()
            ),
        });
    }
    let shown_ticks = |c: &Composition| {
        c.ticks
            .iter()
            .map(|t| {
                (
                    inp.name(t.instance).to_string(),
                    inp.name(t.prim).to_string(),
                    inp.name(t.port).to_string(),
                    t.reset,
                    t.gate.as_ref().map(|g| format!("{g:?}")),
                )
            })
            .collect::<Vec<_>>()
    };
    if shown_ticks(got) != shown_ticks(want) {
        let (g, w) = (shown_ticks(got), shown_ticks(want));
        let at = g.iter().zip(w.iter()).position(|(a, b)| a != b);
        return Some(match at {
            Some(k) => format!(
                "comp {i} tick {k}: computed {:?}, exported {:?}",
                g[k], w[k]
            ),
            None => format!(
                "comp {i} ticks: computed {} of them, exported {}",
                g.len(),
                w.len()
            ),
        });
    }
    let (got, want) = (&got.entries, &want.entries);
    if got.len() != want.len() {
        return Some(format!(
            "comp {i}: computed {} entries, exported {}",
            got.len(),
            want.len()
        ));
    }
    let shown =
        |e: &CompositionEntry| (inp.name(e.instance).to_string(), e.domain, e.segment);
    got.iter().zip(want.iter()).enumerate().find_map(|(k, (a, b))| {
        (shown(a) != shown(b)).then(|| {
            format!("comp {i} entry {k}: computed {:?}, exported {:?}", shown(a), shown(b))
        })
    })
}

/// An expression with its names in place of its string ids.
///
/// The recorded schedules have to survive the exporter interning
/// strings in a different order, so nothing in them may be an id.
fn show(design: &Design, e: &Expr) -> String {
    let n = |s: StrId| design.name(s).to_string();
    let list = |es: &[Expr]| {
        es.iter().map(|x| show(design, x)).collect::<Vec<_>>().join(", ")
    };
    match e {
        Expr::Const { width, limbs } => {
            let v = limbs.first().copied().unwrap_or(0);
            if limbs.iter().skip(1).all(|&w| w == 0) {
                format!("{width}'d{v}")
            } else {
                format!("{width}'{limbs:?}")
            }
        }
        Expr::Def(x) => n(*x),
        Expr::Port(x) => n(*x),
        Expr::Param(x) => format!("param {}", n(*x)),
        Expr::Str(x) => format!("{:?}", design.name(*x)),
        Expr::Real(v) => format!("{v}"),
        Expr::MethCall { instance, method, port, args, .. } => {
            format!("{}.{}#{port}({})", n(*instance), n(*method), list(args))
        }
        Expr::MethValue { instance, method, .. } => {
            format!("{}.{} value", n(*instance), n(*method))
        }
        Expr::TaskValue { cookie, .. } => format!("task value {cookie}"),
        Expr::ForeignCall { func, args, .. } => format!("{}({})", n(*func), list(args)),
        Expr::Clock { osc, gate } => {
            format!("clock {} gated {}", show(design, osc), show(design, gate))
        }
        Expr::Reset { wire } => format!("reset {}", show(design, wire)),
        Expr::Gate { instance, clock } => format!("{}.{} gate", n(*instance), n(*clock)),
        Expr::ClockOut { instance, clock } => format!("{}.{} out", n(*instance), n(*clock)),
        Expr::Prim { op, args, .. } => format!("{op:?}({})", list(args)),
        Expr::If { cond, then_, else_, .. } => format!(
            "if {} then {} else {}",
            show(design, cond),
            show(design, then_),
            show(design, else_)
        ),
        Expr::Case { scrutinee, arms, default, .. } => format!(
            "case {} of [{}] else {}",
            show(design, scrutinee),
            arms.iter()
                .map(|(k, v)| format!("{k:?} => {}", show(design, v)))
                .collect::<Vec<_>>()
                .join(", "),
            show(design, default)
        ),
    }
}

/// What the merge computes, rendered so it can be checked in and
/// compared later.
///
/// The differential check against the exporter is scaffolding and goes
/// when the export does; recording its answer while it is still
/// vouched for is what keeps the evidence afterwards.  A frozen answer
/// only catches *change*, not error -- its authority is entirely that
/// it agreed with bsc on the day it was written.
///
/// Names rather than ids throughout, so a change to the string table
/// does not read as a change to the schedule.
pub fn render(design: &Design) -> String {
    use std::fmt::Write;
    let mut out = String::new();
    let Some(inp) = Inputs::of(design) else {
        return "no top module\n".to_string();
    };
    let comps = match compositions(&inp) {
        Ok(c) => c,
        Err(why) => return format!("{why}\n"),
    };
    let rule = |r: &crate::schedule::QualRule| {
        let path = design.name(r.instance);
        let m = &design.modules[inp.hier.insts[
            inp.hier.insts.iter().position(|(p, _)| p == path).unwrap_or(0)
        ].1];
        let name = m.rules.get(r.rule.idx()).map(|x| design.name(x.name)).unwrap_or("?");
        if path.is_empty() { name.to_string() } else { format!("{path}.{name}") }
    };
    for c in &comps {
        let _ = writeln!(
            out,
            "clock {} {}",
            design.name(c.clock),
            if c.posedge { "posedge" } else { "negedge" }
        );
        for e in &c.entries {
            let _ = writeln!(
                out,
                "  run {:?} domain {} segment {}",
                design.name(e.instance),
                e.domain,
                e.segment
            );
        }
        for t in &c.ticks {
            let _ = writeln!(
                out,
                "  tick {:?} {} port {}{}{}",
                design.name(t.instance),
                design.name(t.prim),
                design.name(t.port),
                if t.reset { " reset" } else { "" },
                match &t.gate {
                    Some(g) => format!(" gated {}", show(design, g)),
                    None => String::new(),
                }
            );
        }
        for e in &c.early {
            let _ = writeln!(out, "  early {}", rule(e));
        }
        for (a, b) in &c.cross_inhibits {
            let _ = writeln!(out, "  inhibit {} -> {}", rule(a), rule(b));
        }
        for a in &c.alts {
            let _ = writeln!(
                out,
                "  alternative in {:?} when {}",
                design.name(a.guard_inst),
                show(design, &a.guard)
            );
            for e in &a.entries {
                let _ = writeln!(
                    out,
                    "    run {:?} domain {} segment {}",
                    design.name(e.instance),
                    e.domain,
                    e.segment
                );
            }
            for (x, y) in &a.cross_inhibits {
                let _ = writeln!(out, "    inhibit {} -> {}", rule(x), rule(y));
            }
        }
    }
    out
}

pub fn diff(design: &Design) -> Vec<String> {
    let Some(inp) = Inputs::of(design) else {
        return vec!["no top module".to_string()];
    };
    // a misread of the per-module domains makes everything downstream
    // meaningless, so it is reported alone rather than alongside
    let mut anomalies = domain_anomalies(&inp);
    anomalies.extend(graph_anomalies(&inp));
    if !anomalies.is_empty() {
        return anomalies;
    }

    let want: Vec<&Composition> = design.compositions.iter().collect();
    let mut out = Vec::new();
    match compositions(&inp) {
        Err(why) => out.push(why),
        Ok(got) if got.len() != want.len() => out.push(format!(
            "composition count: computed {}, exported {}",
            got.len(),
            want.len()
        )),
        Ok(got) => {
            for (i, (g, w)) in got.iter().zip(want.iter()).enumerate() {
                out.extend(composition_diff(&inp, i, g, w));
            }
        }
    }
    if out.is_empty() {
        // say how much agreed, so a design with nothing to compare is
        // not recorded the same as one that matched
        out.push(if want.is_empty() {
            "ok vacuous".to_string()
        } else {
            format!("ok {}", want.len())
        });
        return out;
    }

    // The composed entries are a projection of the flat merged order,
    // so a disagreement is almost always an order that came out wrong
    // several steps earlier.  bsc prints its own order under
    // `-trace-mergesched`; printing ours beside the disagreement makes
    // the two directly comparable.
    {
        let (g, facts) = merged_graph(&inp);
        let up = unified_domains(&inp);
        if let Ok(orders) = domain_orders(&inp, &up, &facts, &g) {
            for d in orders {
                out.push(format!("  order {:?}:", d.domain));
                for n in d.order {
                    let kind = match n.node {
                        SchedNode::Sched(_) => "Sched",
                        SchedNode::Exec(_) => "Exec",
                    };
                    out.push(format!("    {kind} {}", qname(&inp, n)));
                }
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::schedule::{Composition, CompositionEntry};

    fn comp(entries: Vec<(u32, u32, u32)>) -> Composition {
        Composition {
            clock: 0,
            posedge: true,
            entries: entries
                .into_iter()
                .map(|(i, d, s)| CompositionEntry { instance: i, domain: d, segment: s })
                .collect(),
            ticks: vec![],
            early: vec![],
            cross_inhibits: vec![],
            alts: vec![],
        }
    }

    /// A domain names itself by the clock highest up that reaches it:
    /// ungated before gated, then fewest levels, then fewest `$`.  What
    /// this picks is what the composition is recorded under, so a
    /// different choice renames a clock the runtime then looks up.
    #[test]
    fn a_domain_takes_the_name_of_its_shallowest_ungated_clock() {
        use crate::ClockDomain;

        let one = || Expr::Const { width: 1, limbs: vec![1] };
        let mut d = crate::tests::tiny_design();
        // 0 mkTop, 1 CLK, 2 gc, 3 CLK_OUT, 4 new_clk, 5 gc$CLK_OUT
        d.strings = ["mkTop", "CLK", "gc", "CLK_OUT", "new_clk", "gc$CLK_OUT"]
            .iter()
            .map(|s| (*s).to_string())
            .collect();
        let gated = Expr::ClockOut { instance: 2, clock: 3 };
        // the generated clock is listed first, so a reader that simply
        // took the first would take it
        d.modules[0].clock_domains = vec![ClockDomain {
            id: 0,
            clocks: vec![
                (gated.clone(), Expr::Gate { instance: 2, clock: 4 }),
                (Expr::Port(1), one()),
            ],
        }];
        d.index_strings();
        let inp = inputs(&d);
        let dom = QDomain::of_module(0, 0);

        assert_eq!(
            canonical_clock(&inp, dom).and_then(|o| osc_name(&inp, o)),
            Some("CLK".to_string()),
            "the ungated clock names the domain"
        );

        // with the ungated one gone, the generated clock names it, and
        // by the wire it arrives on
        let mut only_gated = d.clone();
        only_gated.modules[0].clock_domains[0].clocks.pop();
        let inp2 = inputs(&only_gated);
        assert_eq!(
            canonical_clock(&inp2, dom).and_then(|o| osc_name(&inp2, o)),
            Some("gc$CLK_OUT".to_string()),
            "a domain with only a gated clock still has a name"
        );
    }

    /// A rule is inhibited by every rule disjoint from it that has
    /// already executed when its own guard is computed -- and by no
    /// others.  Which those are depends on the composed order, so the
    /// same pair inhibits the other way round when the order flips.
    #[test]
    fn an_inhibitor_is_a_disjoint_rule_that_already_ran() {
        let d = two_rule_design();
        let inp = inputs(&d);
        let (a, b) = (SchedEntity::Rule(RuleRef(0)), SchedEntity::Rule(RuleRef(1)));
        let disjoint: BTreeMap<QEntity, BTreeSet<QEntity>> =
            [((0, a), [(0, b)].into_iter().collect()),
             ((0, b), [(0, a)].into_iter().collect())]
            .into_iter()
            .collect();

        // a's segment first: a executes before b's guard is computed
        let a_first = [(0, 0, 0), (0, 0, 1), (0, 0, 2), (0, 0, 3)];
        let pairs = cross_inhibits(&inp, &a_first, &disjoint);
        assert_eq!(
            pairs.iter().map(|(p, q)| (p.rule.idx(), q.rule.idx())).collect::<Vec<_>>(),
            vec![(0, 1)],
            "the rule that ran inhibits the one still to be guarded"
        );

        // the other order, and the same pair points the other way
        let b_first = [(0, 0, 2), (0, 0, 3), (0, 0, 0), (0, 0, 1)];
        let pairs = cross_inhibits(&inp, &b_first, &disjoint);
        assert_eq!(
            pairs.iter().map(|(p, q)| (p.rule.idx(), q.rule.idx())).collect::<Vec<_>>(),
            vec![(1, 0)],
            "order decides which way the inhibitor runs"
        );

        // rules that are not disjoint inhibit nothing, whatever the order
        assert!(
            cross_inhibits(&inp, &a_first, &BTreeMap::new()).is_empty(),
            "only a disjoint pair can inhibit"
        );
    }

    /// Which edge a primitive ticks on comes off its clock argument,
    /// reset ticks come after the rest, and a group's members come out
    /// in the reverse of the order they were collected -- which is how
    /// a dual-port memory ticks its first port first.
    #[test]
    fn ticks_split_by_edge_and_reset() {
        use crate::{ClockArg, ClockDomain, Instance, InstanceKind, Primitive, Ticks};

        let one = || Expr::Const { width: 1, limbs: vec![1] };
        let mut d = crate::tests::tiny_design();
        // 0 mkTop, 1 CLK, 2 p_pos, 3 p_neg, 4 p_rst, 5 p_pos2, 6 clk,
        // 7 RegN, 8 ""
        d.strings = ["mkTop", "CLK", "p_pos", "p_neg", "p_rst", "p_pos2", "clk", "RegN", ""]
            .iter()
            .map(|s| (*s).to_string())
            .collect();
        d.modules[0].clock_domains =
            vec![ClockDomain { id: 0, clocks: vec![(Expr::Port(1), one())] }];
        let prim = |name, order, ticks, has_reset| Instance {
            name,
            kind: InstanceKind::Prim(Primitive::Other { name: 7 }),
            clock_args: vec![ClockArg { name: 6, arg: 0, has_reset, ticks }],
            elab_order: order,
            prim_clocks: None,
            args: vec![Expr::Clock { osc: Box::new(Expr::Port(1)), gate: Box::new(one()) }],
            method_order: vec![],
            port_counts: vec![],
        };
        d.modules[0].instances = vec![
            prim(2, 0, Ticks::Pos, false),
            prim(3, 1, Ticks::Neg, false),
            prim(4, 2, Ticks::Never, true),
            prim(5, 3, Ticks::Pos, false),
        ];
        d.index_strings();

        let inp = inputs(&d);
        let up = unified_domains(&inp);
        let (pos, neg) = domain_ticks(&inp, &up, QDomain::of_module(0, 0));
        let names = |ts: &[QualifiedTick]| {
            ts.iter()
                .map(|t| (inp.name(t.prim).to_string(), t.reset, t.gate.is_some()))
                .collect::<Vec<_>>()
        };

        assert_eq!(
            names(&pos),
            vec![
                ("p_pos2".to_string(), false, false),
                ("p_pos".to_string(), false, false),
                ("p_rst".to_string(), true, false),
            ],
            "rising-edge ticks in reverse collection order, then the reset tick"
        );
        assert_eq!(
            names(&neg),
            vec![("p_neg".to_string(), false, false)],
            "the falling edge ticks only what asked for it"
        );

        // gating the domain's clock reaches every tick on it
        let mut gated = d.clone();
        let g = Expr::Gate { instance: 2, clock: 6 };
        gated.modules[0].clock_domains[0].clocks[0].1 = g;
        let inp2 = inputs(&gated);
        let up2 = unified_domains(&inp2);
        let (pos2, _) = domain_ticks(&inp2, &up2, QDomain::of_module(0, 0));
        assert!(
            pos2.iter().all(|t| t.gate.is_some()),
            "a gated clock gates the ticks it drives"
        );
    }

    /// Every combination of the facts' states is one interleaving.  The
    /// combination needing no guard is the base; the rest are tried
    /// most-active first, so reaching one implies the more-active ones
    /// failed and its guard needs no negations.
    #[test]
    fn alternatives_cover_every_combination_most_active_first() {
        use crate::schedule::DynSched;

        let g: BTreeMap<QNode, Vec<QNode>> = BTreeMap::new();
        let guard = |k: u32| Expr::Port(k);
        let pair = |e: u32, l: u32, ge: u32, gl: Option<Expr>| DynFact {
            inst: 0,
            sched: DynSched::Pair {
                rule_e: RuleRef(e),
                guard_e: guard(ge),
                rule_l: RuleRef(l),
                guard_l: gl,
                meths: vec![],
                between: vec![],
            },
            drops_l: vec![],
            drops_e: vec![],
        };

        // one fact, guarded on one side: two states, so one alternative
        let f = pair(0, 1, 0, None);
        let (_, alts) = dyn_alternatives(&g, &[&f]).unwrap();
        assert_eq!(alts.len(), 1, "two states leave one guarded alternative");
        assert_eq!(alts[0].1, guard(0), "and it is guarded by the early rule");

        // Three facts, because with two the combinations come out in
        // descending order anyway and the sort proves nothing.  Their
        // guards are all distinct: the sort keys on how many guards a
        // combination needs, which the conjunction then deduplicates,
        // so a shared guard would make the two disagree for reasons
        // that have nothing to do with the ordering.
        let facts =
            [pair(0, 1, 10, None), pair(1, 0, 11, None), pair(0, 1, 12, Some(guard(13)))];
        let refs: Vec<&DynFact> = facts.iter().collect();
        let (_, alts) = dyn_alternatives(&g, &refs).unwrap();
        let widths: Vec<usize> = alts
            .iter()
            .map(|(_, guard, _)| match guard {
                Expr::Prim { args, .. } => args.len(),
                _ => 1,
            })
            .collect();
        assert!(
            widths.windows(2).all(|w| w[0] >= w[1]),
            "the most-active combination is tried first, got {widths:?}"
        );
        assert_eq!(widths[0], 3, "and the most active needs every guard");
        assert!(
            matches!(&alts[0].1, Expr::Prim { op: crate::PrimOp::And, args, .. }
                     if args.len() == 3),
            "a combination of live facts is guarded by their conjunction"
        );
        assert!(
            alts.iter().any(|(_, g, _)| !matches!(g, Expr::Prim { .. })),
            "and a combination needing one guard is guarded by it alone"
        );

        // facts in different instances cannot share one guard
        let mut elsewhere = pair(0, 1, 0, None);
        elsewhere.inst = 1;
        assert!(
            dyn_alternatives(&g, &[&facts[0], &elsewhere])
                .is_err_and(|e| e.contains("more than one module")),
            "a guard is read in one instance, so the facts must live in one"
        );
    }

    fn sched(r: u32) -> SchedNode {
        SchedNode::Sched(SchedEntity::Rule(RuleRef(r)))
    }
    fn exec(r: u32) -> SchedNode {
        SchedNode::Exec(SchedEntity::Rule(RuleRef(r)))
    }

    /// The composed entries are the flat order projected onto whole
    /// segments, and where the graph says nothing the flat order
    /// decides -- but where it does say something, it wins.
    #[test]
    fn entries_follow_the_graph_before_the_flat_order() {
        let d = two_rule_design();
        let inp = inputs(&d);
        let n = |x: SchedNode| q(0, x);
        let acyclic = |extra: Vec<(SchedNode, SchedNode)>| {
            let mut g: BTreeMap<QNode, Vec<QNode>> = BTreeMap::new();
            for r in 0..2 {
                g.insert(n(sched(r)), vec![n(exec(r))]);
                g.entry(n(exec(r))).or_default();
            }
            for (a, b) in extra {
                g.entry(n(a)).or_default().push(n(b));
            }
            g
        };
        let order: Vec<QNode> =
            [sched(0), exec(0), sched(1), exec(1)].into_iter().map(n).collect();

        // nothing orders the two rules against each other, so the flat
        // order stands
        assert_eq!(
            derive_entries(&inp, &order, &acyclic(vec![]), &BTreeMap::new()).unwrap(),
            vec![(0, 0, 0), (0, 0, 1), (0, 0, 2), (0, 0, 3)],
            "with nothing to say, the graph leaves the flat order alone"
        );

        // an edge from b's execution back to a's guard: b's segment has
        // to come first, though a appears first in the flat order
        let pushed = derive_entries(
            &inp,
            &order,
            &acyclic(vec![(exec(1), sched(0))]),
            &BTreeMap::new(),
        )
        .unwrap();
        assert_eq!(
            pushed,
            vec![(0, 0, 2), (0, 0, 3), (0, 0, 0), (0, 0, 1)],
            "an edge outranks first appearance"
        );

        // and a graph that cannot be satisfied is reported, not
        // silently linearised
        let cyclic = acyclic(vec![(exec(1), sched(0)), (exec(0), sched(1))]);
        assert!(
            derive_entries(&inp, &order, &cyclic, &BTreeMap::new())
                .is_err_and(|e| e.contains("cyclic")),
            "a cycle among segments has no composition"
        );
    }

    /// A disjoint pair carries no graph edge, but the flat order still
    /// fixes which state each guard sees: if one rule's guard is
    /// computed after the other has executed, that has to stay true of
    /// the composed order too.
    ///
    /// With one node per segment the pin can only ever agree with the
    /// flat order it was derived from, so what this establishes is
    /// consistency, not that the pin decides anything.  It would decide
    /// something only where a segment holds several nodes and the pin
    /// reaches between two of them -- the interlock the exporter
    /// refuses.
    #[test]
    fn a_disjoint_pair_pins_the_segments_it_straddles() {
        let d = two_rule_design();
        let inp = inputs(&d);
        let n = |x: SchedNode| q(0, x);
        let mut g: BTreeMap<QNode, Vec<QNode>> = BTreeMap::new();
        for r in 0..2 {
            g.insert(n(sched(r)), vec![n(exec(r))]);
            g.entry(n(exec(r))).or_default();
        }
        // b's guard is computed after a has executed
        let order: Vec<QNode> =
            [sched(0), exec(0), sched(1), exec(1)].into_iter().map(n).collect();
        let (a, b) = (SchedEntity::Rule(RuleRef(0)), SchedEntity::Rule(RuleRef(1)));
        let disjoint: BTreeMap<QEntity, BTreeSet<QEntity>> =
            [((0, b), [(0, a)].into_iter().collect())].into_iter().collect();

        let with = derive_entries(&inp, &order, &g, &disjoint).unwrap();
        let without = derive_entries(&inp, &order, &g, &BTreeMap::new()).unwrap();
        assert_eq!(with, without, "the pin agrees with the flat order here");

        // the pin is real: reverse only the disjointness direction that
        // the order justifies and the edge disappears
        let other: BTreeMap<QEntity, BTreeSet<QEntity>> =
            [((0, a), [(0, b)].into_iter().collect())].into_iter().collect();
        assert_eq!(
            derive_entries(&inp, &order, &g, &other).unwrap(),
            without,
            "a pair the order does not straddle adds nothing"
        );
    }

    /// A top module with two rules and a segment per schedule node.
    fn two_rule_design() -> Design {
        use crate::schedule::{ModuleSchedule, Segment};
        use crate::Rule;

        let mut d = crate::tests::tiny_design();
        d.strings = ["mkTop", "a", "b", ""].iter().map(|s| (*s).to_string()).collect();
        let rule = |name| Rule {
            name,
            can_fire: name,
            will_fire: name,
            body: crate::Lazy::new(vec![]),
            clock_domain: 0,
            crossing: false,
            me_inhibits: vec![],
        };
        d.modules[0].rules = vec![rule(1), rule(2)];
        d.modules[0].clock_domains = vec![crate::ClockDomain {
            id: 0,
            clocks: vec![(Expr::Port(1), Expr::Const { width: 1, limbs: vec![1] })],
        }];
        // one segment per node, which is what the exporter cuts: a
        // method call can land between a rule's Sched and its Exec, so
        // the two have to be independently placeable
        let seg = |n: SchedNode| Segment { nodes: vec![n], cut: vec![] };
        let (sa, ea) = (sched(0), exec(0));
        let (sb, eb) = (sched(1), exec(1));
        d.modules[0].schedule.domains = vec![ModuleSchedule {
            domain: 0,
            posedge: true,
            segments: vec![seg(sa), seg(ea), seg(sb), seg(eb)],
            ticks: vec![],
        }];
        d.index_strings();
        d
    }

    /// The harness is the only thing standing behind the port, so it
    /// has to be shown to fail.  Each mutation is a way the composed
    /// order could plausibly go wrong; a comparison that misses any of
    /// them is not evidence of anything.
    #[test]
    fn diff_catches_every_way_the_entries_can_be_wrong() {
        let mut d = crate::tests::tiny_design();
        d.strings = vec!["mkTop".into(), "u".into()];
        let base = comp(vec![(0, 0, 0), (0, 0, 1), (0, 0, 2)]);
        let mutants: Vec<(&str, Composition)> = vec![
            ("reordered entries", comp(vec![(0, 0, 1), (0, 0, 0), (0, 0, 2)])),
            ("dropped an entry", comp(vec![(0, 0, 0), (0, 0, 1)])),
            ("extra entry", comp(vec![(0, 0, 0), (0, 0, 1), (0, 0, 2), (0, 0, 3)])),
            ("wrong segment", comp(vec![(0, 0, 0), (0, 0, 1), (0, 0, 5)])),
            ("wrong instance", comp(vec![(0, 0, 0), (1, 0, 1), (0, 0, 2)])),
            ("wrong domain", comp(vec![(0, 0, 0), (0, 1, 1), (0, 0, 2)])),
            ("wrong clock", {
                let mut c = comp(vec![(0, 0, 0), (0, 0, 1), (0, 0, 2)]);
                c.clock = 1;
                c
            }),
        ];
        for (what, m) in mutants {
            assert!(
                composition_diff(&inputs(&d), 0, &base, &m).is_some(),
                "the comparison does not notice: {what}"
            );
        }
        assert!(
            composition_diff(&inputs(&d), 0, &base, &base).is_none(),
            "and it must not cry wolf on an identical pair"
        );
    }

    /// The domain check reports nothing across the whole corpus, which
    /// is only worth believing if it can report something.  Each of
    /// these is a way the domain read could be wrong.
    #[test]
    fn domain_check_catches_a_misread() {
        use crate::{ClockArg, ClockDomain, Instance, InstanceKind, Primitive, Ticks};

        let mut d = crate::tests::tiny_design();
        let osc = Expr::Const { width: 1, limbs: vec![1] };
        d.modules[0].clock_domains = vec![ClockDomain {
            id: 0,
            clocks: vec![(osc.clone(), Expr::Const { width: 1, limbs: vec![1] })],
        }];
        assert!(domain_anomalies(&inputs(&d)).is_empty(), "a consistent module must be quiet");

        // a primitive wired with a clock that belongs to no domain
        let stray = Expr::Const { width: 1, limbs: vec![9] };
        let mut bad = d.clone();
        bad.modules[0].instances = vec![Instance {
            name: 0,
            kind: InstanceKind::Prim(Primitive::Other { name: 0 }),
            clock_args: vec![ClockArg { name: 0, arg: 0, has_reset: false, ticks: Ticks::Pos }],
            prim_clocks: None,
            elab_order: 0,
            args: vec![Expr::Clock {
                osc: Box::new(stray),
                gate: Box::new(Expr::Const { width: 1, limbs: vec![1] }),
            }],
            method_order: vec![],
            port_counts: vec![],
        }];
        assert!(
            domain_anomalies(&inputs(&bad)).iter().any(|l| l.contains("in no domain")),
            "an unresolvable prim clock must be reported"
        );

        // a clock argument pointing at something that is not a clock
        let mut bad2 = bad.clone();
        bad2.modules[0].instances[0].args = vec![Expr::Const { width: 1, limbs: vec![0] }];
        assert!(
            domain_anomalies(&inputs(&bad2)).iter().any(|l| l.contains("is not a clock")),
            "a non-clock clock argument must be reported"
        );
    }

    /// Nodes are ranked by name, not by position: bsc keys its graph
    /// on `SchedNode`, whose Ord puts every `Sched` before every `Exec`
    /// and then compares names.  A port that ranked by rule index would
    /// hand the queue a different graph and get a different run --
    /// which is observable, through task output and unguarded
    /// primitives.  (What the queue does with equal ranks is its own
    /// business; `psq` is where that is pinned down.)
    #[test]
    fn nodes_are_ranked_by_name_not_position() {
        use crate::Rule;

        let mut d = crate::tests::tiny_design();
        d.strings = ["mkTop", "b", "a"].iter().map(|s| (*s).to_string()).collect();
        let rule = |name| Rule {
            name,
            can_fire: name,
            will_fire: name,
            body: crate::Lazy::new(vec![]),
            clock_domain: 0,
            crossing: false,
            me_inhibits: vec![],
        };
        // declared b, then a: position order is the opposite of name order
        d.modules[0].rules = vec![rule(1), rule(2)];

        let sched = |r| q(0, SchedNode::Sched(SchedEntity::Rule(RuleRef(r))));
        let exec = |r| q(0, SchedNode::Exec(SchedEntity::Rule(RuleRef(r))));
        let mut g: BTreeMap<QNode, Vec<QNode>> = BTreeMap::new();
        for r in 0..2 {
            g.insert(sched(r), vec![exec(r)]);
            g.insert(exec(r), vec![]);
        }

        let (order, _) = flatten(&inputs(&d), &g).expect("the graph is acyclic");
        let names: Vec<String> = order.iter().map(|n| {
            let k = if matches!(n.node, SchedNode::Exec(_)) { "exec" } else { "sched" };
            format!("{k} {}", qname(&inputs(&d), *n))
        }).collect();
        assert_eq!(
            names,
            ["sched a", "sched b", "exec a", "exec b"],
            "every Sched before every Exec, and each group in name order"
        );
    }

    /// A cycle made only of foreign-function edges is not a cycle the
    /// merge has to accept: those edges record an arbitrary choice, and
    /// dropping one leaves a real order.
    #[test]
    fn a_foreign_function_cycle_is_broken() {
        use crate::Rule;

        let mut d = crate::tests::tiny_design();
        d.strings = ["mkTop", "a", "b"].iter().map(|s| (*s).to_string()).collect();
        let rule = |name| Rule {
            name,
            can_fire: name,
            will_fire: name,
            body: crate::Lazy::new(vec![]),
            clock_domain: 0,
            crossing: false,
            me_inhibits: vec![],
        };
        d.modules[0].rules = vec![rule(1), rule(2)];

        let ea = SchedEntity::Rule(RuleRef(0));
        let eb = SchedEntity::Rule(RuleRef(1));
        let exec = |e| q(0, SchedNode::Exec(e));
        let mut g: BTreeMap<QNode, Vec<QNode>> = BTreeMap::new();
        g.insert(exec(ea), vec![exec(eb)]);
        g.insert(exec(eb), vec![exec(ea)]);

        assert!(
            flatten(&inputs(&d), &g).is_err(),
            "a cycle with nothing droppable in it must stay a cycle"
        );

        d.modules[0].schedule.ffunc_edges = vec![(eb, ea)];
        let (order, cut) =
            flatten(&inputs(&d), &g).expect("the ffunc edge breaks the cycle");
        assert_eq!(order, vec![exec(ea), exec(eb)], "the surviving edge decides");
        assert!(
            !cut.get(&exec(eb)).is_some_and(|v| v.contains(&exec(ea))),
            "the dropped edge must be gone from the graph the order came from, \
             or whatever reads it next re-imposes the ordering the sort gave up"
        );
    }

    fn inputs(d: &Design) -> Inputs<'_> {
        Inputs::of(d).expect("the design has a top")
    }

    /// Two instances sharing one clock are one domain, not two.  The
    /// child names that clock by the port its oscillator arrives on,
    /// the parent by the expression it wired to the argument, and only
    /// the instance's clock arguments connect the two names.
    #[test]
    fn a_wired_clock_makes_parent_and_child_one_domain() {
        use crate::{
            ClockArg, ClockDomain, Extern, InputClock, Instance, InstanceKind, Ticks,
        };

        let one = || Expr::Const { width: 1, limbs: vec![1] };
        let mut d = crate::tests::tiny_design();
        // 0 mkTop, 1 mkChild, 2 u, 3 CLK, 4 CLK_child, 5 default_clock
        d.strings = ["mkTop", "mkChild", "u", "CLK", "CLK_child", "default_clock"]
            .iter()
            .map(|s| (*s).to_string())
            .collect();

        let mut child = d.modules[0].clone();
        child.name = 1;
        child.input_clocks = vec![InputClock { name: 5, osc: 4, gate: None }];
        child.clock_domains = vec![ClockDomain { id: 0, clocks: vec![(Expr::Port(4), one())] }];

        let parent = &mut d.modules[0];
        parent.externs = vec![Extern { module: 1 }];
        parent.clock_domains = vec![ClockDomain { id: 0, clocks: vec![(Expr::Port(3), one())] }];
        parent.instances = vec![Instance {
            name: 2,
            kind: InstanceKind::Module(crate::ExternRef(0)),
            clock_args: vec![ClockArg { name: 5, arg: 0, has_reset: false, ticks: Ticks::Pos }],
            prim_clocks: None,
            elab_order: 0,
            args: vec![Expr::Clock { osc: Box::new(Expr::Port(3)), gate: Box::new(one()) }],
            method_order: vec![],
            port_counts: vec![],
        }];
        d.modules.push(child);

        assert_eq!(inputs(&d).hier.insts.len(), 2, "the child must be reached");
        assert_eq!(
            unified_domains(&inputs(&d)).get(&QDomain::of_module(1, 0)),
            Some(&Fate::Joins(QDomain::of_module(0, 0))),
            "the child's domain must be the parent's"
        );

        // the connection runs through the clock argument: without it
        // the two domains are unrelated, and a merge that unified them
        // anyway would be unifying on nothing
        let mut cut = d.clone();
        cut.modules[0].instances[0].clock_args.clear();
        assert!(
            unified_domains(&inputs(&cut)).is_empty(),
            "an unwired child keeps its own domain"
        );

        // and it runs through the child's own name for the clock: if
        // the input clock arrives on a port the child's domains never
        // mention, there is nothing to unify
        let mut renamed = d.clone();
        renamed.modules[1].input_clocks[0].osc = 3;
        assert!(
            unified_domains(&inputs(&renamed)).is_empty(),
            "a clock the child does not use stays unmapped"
        );

        // wiring noClock into the input takes the child's domain out of
        // the design altogether: nothing ticks it, so it is not a
        // domain the schedule splits by
        let mut none = d.clone();
        none.modules[0].instances[0].args = vec![Expr::Clock {
            osc: Box::new(Expr::Const { width: 1, limbs: vec![0] }),
            gate: Box::new(Expr::Const { width: 1, limbs: vec![0] }),
        }];
        assert_eq!(
            unified_domains(&inputs(&none)).get(&QDomain::of_module(1, 0)),
            Some(&Fate::Dropped),
            "a child clocked by noClock has no domain"
        );
        assert_eq!(
            merged_domains(&inputs(&none), &unified_domains(&inputs(&none))),
            vec![QDomain::of_module(0, 0)],
            "and the design is left with the parent's domain alone"
        );
    }

    /// A gated clock wired to an input the child declares ungated does
    /// not unify their domains.  The two sides are matched on the whole
    /// clock, gate included, and a mismatch leaves a domain of its own
    /// standing -- which is a domain of the design, not an oversight.
    #[test]
    fn a_gate_mismatch_leaves_the_domain_standing() {
        use crate::{ClockArg, ClockDomain, Extern, InputClock, Instance, InstanceKind, Ticks};

        let one = || Expr::Const { width: 1, limbs: vec![1] };
        let mut d = crate::tests::tiny_design();
        // 0 mkTop, 1 mkChild, 2 u, 3 CLK, 4 CLK_IN, 5 default_clock,
        // 6 CLK_GATE_IN
        d.strings = ["mkTop", "mkChild", "u", "CLK", "CLK_IN", "default_clock", "CLK_GATE_IN"]
            .iter()
            .map(|s| (*s).to_string())
            .collect();

        // the child takes a gated clock in, but its domain is declared
        // around the ungated one
        let mut child = d.modules[0].clone();
        child.name = 1;
        child.input_clocks = vec![InputClock { name: 5, osc: 4, gate: Some(6) }];
        child.clock_domains = vec![ClockDomain { id: 0, clocks: vec![(Expr::Port(4), one())] }];

        let parent = &mut d.modules[0];
        parent.externs = vec![Extern { module: 1 }];
        parent.clock_domains = vec![ClockDomain { id: 0, clocks: vec![(Expr::Port(3), one())] }];
        parent.instances = vec![Instance {
            name: 2,
            kind: InstanceKind::Module(crate::ExternRef(0)),
            clock_args: vec![ClockArg { name: 5, arg: 0, has_reset: false, ticks: Ticks::Pos }],
            prim_clocks: None,
            elab_order: 0,
            args: vec![Expr::Clock { osc: Box::new(Expr::Port(3)), gate: Box::new(one()) }],
            method_order: vec![],
            port_counts: vec![],
        }];
        d.modules.push(child);

        assert!(
            unified_domains(&inputs(&d)).is_empty(),
            "the gates differ, so the two domains are not the same domain"
        );
        assert_eq!(merged_domains(&inputs(&d), &unified_domains(&inputs(&d))).len(), 2, "and the design has both");

        // declare the domain around the gated clock and they meet
        let mut matched = d.clone();
        matched.modules[1].clock_domains[0].clocks = vec![(Expr::Port(4), Expr::Port(6))];
        assert_eq!(
            unified_domains(&inputs(&matched)).get(&QDomain::of_module(1, 0)),
            Some(&Fate::Joins(QDomain::of_module(0, 0))),
            "the same clock on both sides is one domain"
        );
    }

    /// A clock-crossing rule runs after the edge, and belongs to the
    /// composition of its own domain -- not to whichever composition
    /// happens to come first.  No corpus design has one, so this is
    /// what stands behind that part of the merge.
    #[test]
    fn a_crossing_rule_belongs_to_its_own_domain() {
        use crate::{ClockDomain, Rule};

        let one = || Expr::Const { width: 1, limbs: vec![1] };
        let mut d = crate::tests::tiny_design();
        // 0 mkTop, 1 CLK, 2 CLK2, 3 cross_a, 4 plain, 5 cross_b,
        // 6 the top instance's own (empty) path
        d.strings = ["mkTop", "CLK", "CLK2", "cross_a", "plain", "cross_b", ""]
            .iter()
            .map(|s| (*s).to_string())
            .collect();
        let rule = |name, dom, crossing| Rule {
            name,
            can_fire: name,
            will_fire: name,
            body: crate::Lazy::new(vec![]),
            clock_domain: dom,
            crossing,
            me_inhibits: vec![],
        };
        let top = &mut d.modules[0];
        top.clock_domains = vec![
            ClockDomain { id: 0, clocks: vec![(Expr::Port(1), one())] },
            ClockDomain { id: 1, clocks: vec![(Expr::Port(2), one())] },
        ];
        top.rules = vec![rule(3, 0, true), rule(4, 0, false), rule(5, 1, true)];
        d.index_strings();

        let inp = inputs(&d);
        let up = unified_domains(&inp);
        let of = |dom| {
            early_rules(&inp, &up, QDomain::of_module(0, dom))
                .iter()
                .map(|q| q.rule.idx())
                .collect::<Vec<_>>()
        };
        assert_eq!(of(0), vec![0], "only the crossing rule, and only this domain's");
        assert_eq!(of(1), vec![2], "the other domain has its own");
    }

    /// A primitive has clock domains of its own -- a divider's slow
    /// output is one -- and they take part in the design's domains
    /// exactly as a submodule's do, out of what the instantiating
    /// module carries for it.
    #[test]
    fn a_primitive_brings_its_own_domains() {
        use crate::{
            ClockArg, ClockDomain, InputClock, Instance, InstanceKind, PrimClocks,
            Primitive, Ticks,
        };

        let one = || Expr::Const { width: 1, limbs: vec![1] };
        let mut d = crate::tests::tiny_design();
        // 0 mkTop, 1 div, 2 CLK, 3 CLK_IN, 4 CLK_OUT, 5 clk, 6 ClockDiv
        d.strings = ["mkTop", "div", "CLK", "CLK_IN", "CLK_OUT", "clk", "ClockDiv"]
            .iter()
            .map(|s| (*s).to_string())
            .collect();

        let top = &mut d.modules[0];
        top.clock_domains = vec![
            ClockDomain { id: 0, clocks: vec![(Expr::Port(2), one())] },
            ClockDomain {
                id: 1,
                clocks: vec![(Expr::ClockOut { instance: 1, clock: 4 }, one())],
            },
        ];
        top.instances = vec![Instance {
            name: 1,
            kind: InstanceKind::Prim(Primitive::Other { name: 6 }),
            clock_args: vec![ClockArg { name: 5, arg: 0, has_reset: false, ticks: Ticks::Pos }],
            elab_order: 0,
            prim_clocks: Some(PrimClocks {
                inputs: vec![InputClock { name: 5, osc: 3, gate: None }],
                domains: vec![
                    ClockDomain { id: 0, clocks: vec![(Expr::Port(4), one())] },
                    ClockDomain { id: 1, clocks: vec![(Expr::Port(3), one())] },
                ],
                outputs: vec![(4, Expr::Port(4))],
            }),
            args: vec![Expr::Clock { osc: Box::new(Expr::Port(2)), gate: Box::new(one()) }],
            method_order: vec![],
            port_counts: vec![],
        }];

        assert_eq!(
            inputs(&d).hier.insts.len(),
            1,
            "a primitive is not an instance of the hierarchy"
        );
        let up = unified_domains(&inputs(&d));
        assert_eq!(
            up.get(&QDomain { inst: 0, prim: Some(0), id: 1 }),
            Some(&Fate::Joins(QDomain::of_module(0, 0))),
            "what feeds the divider is the clock the parent wired in"
        );
        assert_eq!(
            up.get(&QDomain { inst: 0, prim: Some(0), id: 0 }),
            Some(&Fate::Joins(QDomain::of_module(0, 1))),
            "what the divider hands back is the domain the parent uses it as"
        );
        assert_eq!(merged_domains(&inputs(&d), &unified_domains(&inputs(&d))).len(), 2, "two domains, not four");

        // a divider whose output nothing uses keeps that domain to
        // itself: it still ticks, so it is still a domain
        let mut unused = d.clone();
        unused.modules[0].clock_domains.pop();
        assert_eq!(merged_domains(&inputs(&unused), &unified_domains(&inputs(&unused))).len(), 2, "the parent's, and the divider's");
    }

    /// A clock can cross the other way too: the child exports one and
    /// the parent uses it.  The parent names such a clock by the
    /// instance that exports it, and the two sides meet on the port
    /// name the child gave it.
    #[test]
    fn an_exported_clock_makes_parent_and_child_one_domain() {
        use crate::{ClockDomain, Extern, Instance, InstanceKind};

        let one = || Expr::Const { width: 1, limbs: vec![1] };
        let mut d = crate::tests::tiny_design();
        // 0 mkTop, 1 mkDiv, 2 u, 3 CLK, 4 CLK_slow, 5 slow_osc
        d.strings = ["mkTop", "mkDiv", "u", "CLK", "CLK_slow", "slow_osc"]
            .iter()
            .map(|s| (*s).to_string())
            .collect();

        // the child divides its own clock and exports the result
        let mut child = d.modules[0].clone();
        child.name = 1;
        child.clock_domains = vec![ClockDomain { id: 7, clocks: vec![(Expr::Port(5), one())] }];
        child.ifc_clocks = vec![(4, Expr::Port(5))];

        // the parent has a domain for the clock coming back out
        let parent = &mut d.modules[0];
        parent.externs = vec![Extern { module: 1 }];
        parent.clock_domains = vec![ClockDomain {
            id: 0,
            clocks: vec![(Expr::ClockOut { instance: 2, clock: 4 }, one())],
        }];
        parent.instances = vec![Instance {
            name: 2,
            kind: InstanceKind::Module(crate::ExternRef(0)),
            clock_args: vec![],
            prim_clocks: None,
            elab_order: 0,
            args: vec![],
            method_order: vec![],
            port_counts: vec![],
        }];
        d.modules.push(child);

        assert_eq!(
            unified_domains(&inputs(&d)).get(&QDomain::of_module(1, 7)),
            Some(&Fate::Joins(QDomain::of_module(0, 0))),
            "the domain driving the exported clock is the parent's"
        );
        assert_eq!(
            merged_domains(&inputs(&d), &unified_domains(&inputs(&d))),
            vec![QDomain::of_module(0, 0)],
            "one domain, not two"
        );

        // the parent has to actually be in that clock's domain: if it
        // never uses the exported clock, the child keeps its own
        let mut unused = d.clone();
        unused.modules[0].clock_domains[0].clocks = vec![(Expr::Port(3), one())];
        assert!(
            unified_domains(&inputs(&unused)).is_empty(),
            "an exported clock nothing uses unifies nothing"
        );
        assert_eq!(merged_domains(&inputs(&unused), &unified_domains(&inputs(&unused))).len(), 2, "so there are two domains");
    }

    /// Fusing a child into its parent: the child's method node is the
    /// seam, and it does not survive.  Whatever came before the method
    /// now comes before the calling node, and whatever followed the
    /// method now follows it.
    #[test]
    fn a_called_method_is_replaced_by_its_caller() {
        let meth = SchedNode::Sched(SchedEntity::Method(crate::MethodRef(0)));
        let before = SchedNode::Exec(SchedEntity::Rule(RuleRef(0)));
        let after = SchedNode::Exec(SchedEntity::Rule(RuleRef(1)));
        let caller = SchedNode::Exec(SchedEntity::Rule(RuleRef(5)));

        let (before, meth, after) = (q(1, before), q(1, meth), q(1, after));
        let caller = q(0, caller);
        let mut child = BTreeMap::new();
        child.insert(before, vec![meth]);
        child.insert(meth, vec![after]);

        let mut out = BTreeMap::new();
        combine_sched_map(&mut out, 1, &child, &[(caller, vec![meth])]);

        let pq = caller;
        assert_eq!(
            out.get(&pq).map(|v| v.as_slice()),
            Some(&[after][..]),
            "the caller must inherit what followed the method"
        );
        assert!(
            out.get(&before).is_some_and(|v| v.contains(&pq)),
            "what preceded the method must now precede the caller"
        );
        assert!(
            !out.keys().any(|k| is_method(&k.node)),
            "no method node may survive the merge"
        );
        assert!(
            !out.values().flatten().any(|n| is_method(&n.node)),
            "no edge may point at a method node"
        );
    }

    /// A call two boundaries deep still fuses.  The top calls a method
    /// of the middle module, whose own schedule was ordered against a
    /// rule of the bottom one -- so after the merge the top's rule is
    /// ordered against that bottom rule, with neither method left.
    /// This only works if each instance absorbs its children before its
    /// parent absorbs it.
    #[test]
    fn a_call_fuses_through_two_boundaries() {
        use crate::{Extern, Instance, InstanceKind, Method, MethodKind, Rule, Stmt};

        // 0 mkTop, 1 mkMid, 2 mkBot, 3 m (mid inst), 4 b (bot inst),
        // 5 go (mid's method), 6 tick (bot's method), 7 RL_top, 8 RL_bot
        let mut d = crate::tests::tiny_design();
        d.strings = ["mkTop", "mkMid", "mkBot", "m", "b", "go", "tick", "RL_top", "RL_bot"]
            .iter()
            .map(|s| (*s).to_string())
            .collect();

        let rule = |name, body: Vec<Stmt>| Rule {
            name,
            can_fire: name,
            will_fire: name,
            body: crate::Lazy::new(body),
            clock_domain: 0,
            crossing: false,
            me_inhibits: vec![],
        };
        let meth = |name, body: Vec<Stmt>| Method {
            name,
            kind: MethodKind::Action,
            args: vec![],
            ready: None,
            rdy: None,
            body,
            result: None,
            clock_domain: 0,
            always_enabled: false,
            will_fire: None,
            en: None,
        };
        let call = |instance, method| {
            Stmt::Action(crate::Action::MethCall {
                instance,
                method,
                port: 0,
                cond: Expr::Const { width: 1, limbs: vec![1] },
                args: vec![],
            })
        };
        let inst = |name, x| Instance {
            name,
            kind: InstanceKind::Module(crate::ExternRef(x)),
            clock_args: vec![],
            prim_clocks: None,
            elab_order: 0,
            args: vec![],
            method_order: vec![],
            port_counts: vec![],
        };

        let base = d.modules[0].clone();
        // the bottom: one rule, and a method its rule must precede
        let mut bot = base.clone();
        bot.name = 2;
        bot.rules = vec![rule(8, vec![])];
        bot.methods = vec![meth(6, vec![])];
        let (bot_rule, bot_meth) = (
            SchedNode::Exec(SchedEntity::Rule(RuleRef(0))),
            SchedNode::Exec(SchedEntity::Method(crate::MethodRef(0))),
        );
        bot.schedule.sched_graph = vec![(bot_meth, vec![bot_rule]), (bot_rule, vec![])];

        // the middle: a method that calls the bottom's
        let mut mid = base.clone();
        mid.name = 1;
        mid.externs = vec![Extern { module: 2 }];
        mid.instances = vec![inst(4, 0)];
        mid.methods = vec![meth(5, vec![call(4, 6)])];
        mid.schedule.sched_graph = vec![(
            SchedNode::Exec(SchedEntity::Method(crate::MethodRef(0))),
            vec![],
        )];

        // the top: a rule that calls the middle's method
        let top = &mut d.modules[0];
        top.externs = vec![Extern { module: 1 }];
        top.instances = vec![inst(3, 0)];
        top.rules = vec![rule(7, vec![call(3, 5)])];
        top.schedule.sched_graph =
            vec![(SchedNode::Exec(SchedEntity::Rule(RuleRef(0))), vec![])];
        d.modules.push(mid);
        d.modules.push(bot);

        assert_eq!(inputs(&d).hier.insts.len(), 3, "top, middle and bottom");
        let (g, _) = merged_graph(&inputs(&d));

        let top_exec = q(0, SchedNode::Exec(SchedEntity::Rule(RuleRef(0))));
        let bot_exec = q(2, bot_rule);
        assert!(
            g.get(&bot_exec).is_some_and(|v| v.contains(&top_exec)),
            "the bottom rule must precede the top rule that reaches it, got {g:#?}"
        );
        assert!(
            !g.keys().chain(g.values().flatten()).any(|n| is_method(&n.node)),
            "no method may survive: they are all below the top"
        );
    }

    /// Conflicts fuse one way only: a rule the called method blocked is
    /// afterwards blocked by the caller.  The other direction is empty
    /// in bsc too -- rules cannot block methods -- so a test that
    /// expected it would be testing a fiction.
    #[test]
    fn a_blocked_rule_becomes_blocked_by_the_caller() {
        let meth = SchedEntity::Method(crate::MethodRef(0));
        let blocked = SchedEntity::Rule(RuleRef(1));
        let caller = SchedEntity::Rule(RuleRef(5));

        // in the child, `meth` blocks `blocked`
        let mut child: BTreeMap<QEntity, Vec<QEntity>> = BTreeMap::new();
        child.insert((1, blocked), vec![(1, meth)]);

        let mut out = BTreeMap::new();
        let uses = [(SchedEntity::Rule(RuleRef(5)), vec![crate::MethodRef(0)])];
        combine_conflicts(&mut out, 0, 1, &child, &uses);

        assert!(
            out.get(&(1, blocked)).is_some_and(|v| v.contains(&(0, caller))),
            "the blocked rule must now be blocked by the caller"
        );
        assert!(
            !out.keys().any(|(_, e)| is_method_entity(e)),
            "no method entity may survive"
        );
        assert!(
            !out.values().flatten().any(|(_, e)| is_method_entity(e)),
            "nothing may still be blocked by a method"
        );
    }

    /// Disjointness is symmetric, so fusing it has to record both
    /// directions or a later reader gets a different answer depending
    /// on which side it asks from.
    #[test]
    fn disjointness_fuses_both_ways() {
        let m0 = crate::MethodRef(0);
        let meth = SchedEntity::Method(m0);
        let other = SchedEntity::Rule(RuleRef(1));
        let caller = SchedEntity::Rule(RuleRef(5));

        let mut child: BTreeMap<QEntity, BTreeSet<QEntity>> = BTreeMap::new();
        child.insert((1, meth), [(1, other)].into_iter().collect());

        let flat = [(caller, vec![m0])].into_iter().collect();
        let mut out = BTreeMap::new();
        combine_disjoint(&mut out, 0, 1, &child, &flat);

        let cq = (0, caller);
        assert!(
            out.get(&cq).is_some_and(|v| v.contains(&(1, other))),
            "the caller must be disjoint from what the method was"
        );
        assert!(
            out.get(&(1, other)).is_some_and(|v| v.contains(&cq)),
            "and the relation must read the same from the other side"
        );
    }

    /// Two child methods disjoint from each other make their callers
    /// disjoint: nothing in either module says so on its own, and the
    /// merge is the only place the fact exists.
    #[test]
    fn disjoint_methods_make_their_callers_disjoint() {
        let (m0, m1) = (crate::MethodRef(0), crate::MethodRef(1));
        let (a, b) = (SchedEntity::Rule(RuleRef(3)), SchedEntity::Rule(RuleRef(4)));

        let mut child: BTreeMap<QEntity, BTreeSet<QEntity>> = BTreeMap::new();
        child.insert((1, SchedEntity::Method(m0)), [(1, SchedEntity::Method(m1))].into());
        child.insert((1, SchedEntity::Method(m1)), [(1, SchedEntity::Method(m0))].into());

        let flat = [(a, vec![m0]), (b, vec![m1])].into_iter().collect();
        let mut out = BTreeMap::new();
        combine_disjoint(&mut out, 0, 1, &child, &flat);

        let (aq, bq) = ((0, a), (0, b));
        assert!(
            out.get(&aq).is_some_and(|v| v.contains(&bq)),
            "the caller of one must be disjoint from the caller of the other"
        );
        assert!(out.get(&bq).is_some_and(|v| v.contains(&aq)), "and back");
        assert!(
            !out.keys().any(|(_, e)| is_method_entity(e))
                && !out.values().flatten().any(|(_, e)| is_method_entity(e)),
            "no method entity may survive the merge"
        );
    }

    /// A ready-signal read is not a call, so it carries no
    /// disjointness: `flat_uses` keeps only what reaches a method's
    /// Exec node.
    #[test]
    fn a_ready_read_is_not_a_call() {
        use crate::{Method, MethodKind};

        let mut d = crate::tests::tiny_design();
        d.strings = ["mkTop", "go", "RDY_go", "r"].iter().map(|s| (*s).to_string()).collect();
        let child = crate::Module {
            methods: vec![Method {
                name: 1,
                kind: MethodKind::Action,
                args: vec![],
                ready: None,
                rdy: Some(2),
                body: vec![],
                result: None,
                clock_domain: 0,
                always_enabled: false,
                will_fire: None,
                en: None,
            }],
            ..d.modules[0].clone()
        };
        d.modules.push(child);
        let cmod = &d.modules[1];

        let mut uses = Uses::default();
        uses.pred.insert((SchedEntity::Rule(RuleRef(0)), 0), vec![2]);
        assert!(
            flat_uses(cmod, &uses, 0).is_empty(),
            "reading RDY_go is not calling go"
        );

        let mut uses = Uses::default();
        uses.pred.insert((SchedEntity::Rule(RuleRef(0)), 0), vec![1]);
        assert_eq!(
            flat_uses(cmod, &uses, 0)
                .get(&SchedEntity::Rule(RuleRef(0)))
                .map(|v| v.as_slice()),
            Some(&[crate::MethodRef(0)][..]),
            "calling go in a predicate is still a call"
        );
    }

    /// The per-module read is the merge's ground floor: whatever the
    /// fragment says about itself has to survive being read.
    #[test]
    fn mod_csi_reads_what_the_fragment_says() {
        let d = crate::tests::tiny_design();
        for m in &d.modules {
            let c = ModCsi::of(m);
            assert_eq!(c.sched.len(), m.schedule.sched_graph.len());
            assert_eq!(c.conflicts.len(), m.schedule.conflicts.len());
            assert_eq!(c.disjoint.len(), m.schedule.disjoint_rules.len());
            assert_eq!(c.ffunc.len(), m.schedule.ffunc_edges.len());
        }
    }
}
