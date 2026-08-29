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

use crate::schedule::{Composition, SchedNode};
use crate::{ClockArg, Design, Expr, RuleRef, SchedEntity, StrId};

/// The instance hierarchy, as the merge walks it: for each instance, its
/// path, the module it is of, and its children.
pub struct Hier {
    /// (path, module index), parent before child.
    pub insts: Vec<(String, usize)>,
    /// children of `insts[i]`, as indices into `insts`
    pub kids: Vec<Vec<usize>>,
}

impl Hier {
    /// Walk the design from `top`, following each module's instances.
    pub fn of(design: &Design) -> Option<Hier> {
        let top = design.modules.iter().position(|m| m.name == design.top)?;
        let mut h = Hier { insts: vec![(String::new(), top)], kids: vec![Vec::new()] };
        let mut queue = vec![0usize];
        while let Some(i) = queue.pop() {
            let (path, mir) = (h.insts[i].0.clone(), h.insts[i].1);
            for inst in &design.modules[mir].instances {
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
                h.kids[i].push(k);
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
        let sched = m.schedule.sched_graph.iter().map(|(n, ns)| (*n, ns.clone())).collect();
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
        self.by_osc.iter().find(|(o, _)| o == osc).map(|(_, d)| *d)
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

/// The design's compositions, computed from the fragments.
///
/// Empty while the port is in progress; `diff` measures the distance.
pub fn compositions(_design: &Design) -> Vec<Composition> {
    Vec::new()
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

    /// The harness is the only thing standing behind the port, so it
    /// has to be shown to fail.  Each mutation is a way the merge could
    /// plausibly go wrong; a comparison that misses any of them is not
    /// evidence of anything.
    #[test]
    fn diff_catches_every_way_a_composition_can_be_wrong() {
        let base = comp(vec![(0, 0, 0), (1, 0, 0), (1, 0, 1)]);
        let mutants: Vec<(&str, Composition)> = vec![
            ("reordered entries", comp(vec![(1, 0, 0), (0, 0, 0), (1, 0, 1)])),
            ("dropped an entry", comp(vec![(0, 0, 0), (1, 0, 0)])),
            ("extra entry", comp(vec![(0, 0, 0), (1, 0, 0), (1, 0, 1), (2, 0, 0)])),
            ("wrong segment", comp(vec![(0, 0, 0), (1, 0, 0), (1, 0, 2)])),
            ("wrong instance", comp(vec![(0, 0, 0), (2, 0, 0), (1, 0, 1)])),
            ("wrong domain", comp(vec![(0, 0, 0), (1, 1, 0), (1, 0, 1)])),
            ("wrong edge", {
                let mut c = comp(vec![(0, 0, 0), (1, 0, 0), (1, 0, 1)]);
                c.posedge = false;
                c
            }),
            ("wrong clock", {
                let mut c = comp(vec![(0, 0, 0), (1, 0, 0), (1, 0, 1)]);
                c.clock = 7;
                c
            }),
        ];
        for (what, m) in mutants {
            let a = format!("{base:#?}");
            let b = format!("{m:#?}");
            assert_ne!(a, b, "the comparison does not notice: {what}");
        }
        // and it must not cry wolf on an identical pair
        assert_eq!(format!("{base:#?}"), format!("{:#?}", comp(vec![(0, 0, 0), (1, 0, 0), (1, 0, 1)])));
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
        assert!(domain_anomalies(&d).is_empty(), "a consistent module must be quiet");

        // a primitive wired with a clock that belongs to no domain
        let stray = Expr::Const { width: 1, limbs: vec![9] };
        let mut bad = d.clone();
        bad.modules[0].instances = vec![Instance {
            name: 0,
            kind: InstanceKind::Prim(Primitive::Other { name: 0 }),
            clock_args: vec![ClockArg { name: 0, arg: 0, has_reset: false, ticks: Ticks::Pos }],
            args: vec![Expr::Clock {
                osc: Box::new(stray),
                gate: Box::new(Expr::Const { width: 1, limbs: vec![1] }),
            }],
            method_order: vec![],
            port_counts: vec![],
        }];
        assert!(
            domain_anomalies(&bad).iter().any(|l| l.contains("in no domain")),
            "an unresolvable prim clock must be reported"
        );

        // a clock argument pointing at something that is not a clock
        let mut bad2 = bad.clone();
        bad2.modules[0].instances[0].args = vec![Expr::Const { width: 1, limbs: vec![0] }];
        assert!(
            domain_anomalies(&bad2).iter().any(|l| l.contains("is not a clock")),
            "a non-clock clock argument must be reported"
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

/// Whether the per-module domain read holds together, independent of
/// the merge: every rule lands in exactly one domain, and every clock a
/// primitive is wired with resolves to one.  A violation means the
/// domains were misread, and nothing downstream of them can be right.
pub fn domain_anomalies(design: &Design) -> Vec<String> {
    let mut out = Vec::new();
    for m in &design.modules {
        let d = ModDomains::of(m);
        let placed: usize = d.info.values().map(|i| i.rules.len()).sum();
        if placed != m.rules.len() {
            out.push(format!(
                "module {}: {} rules, {} placed in domains",
                design.name(m.name),
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
                        design.name(m.name),
                        design.name(inst.name),
                        design.name(ca.name)
                    ));
                    continue;
                };
                if !is_no_clock(osc) && d.domain_of(osc).is_none() {
                    out.push(format!(
                        "module {}: instance {} clock {} is in no domain",
                        design.name(m.name),
                        design.name(inst.name),
                        design.name(ca.name)
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
pub fn diff(design: &Design) -> Vec<String> {
    let mut out = domain_anomalies(design);
    if !out.is_empty() {
        return out;
    }
    let got = compositions(design);
    let want = &design.compositions;
    if got.len() != want.len() {
        return vec![format!(
            "composition count: computed {}, exported {}",
            got.len(),
            want.len()
        )];
    }
    let mut out = Vec::new();
    for (i, (g, w)) in got.iter().zip(want.iter()).enumerate() {
        let (gs, ws) = (format!("{g:#?}"), format!("{w:#?}"));
        if gs == ws {
            continue;
        }
        // report the first line that differs, with its position, so a
        // regression names a place rather than a volume of output
        let (mut gl, mut wl) = (gs.lines(), ws.lines());
        let mut n = 0usize;
        loop {
            match (gl.next(), wl.next()) {
                (Some(a), Some(b)) if a == b => n += 1,
                (a, b) => {
                    out.push(format!(
                        "comp {i} line {n}: computed {:?}, exported {:?}",
                        a.unwrap_or("<end>").trim(),
                        b.unwrap_or("<end>").trim()
                    ));
                    break;
                }
            }
        }
    }
    out
}
