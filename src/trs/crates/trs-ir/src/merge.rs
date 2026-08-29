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
use crate::{Design, SchedEntity};

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

/// Where the computed compositions differ from the exported ones.
///
/// The comparison is over the whole structure's `Debug` rendering
/// rather than a hand-written field-by-field check.  That is
/// deliberate: a hand-written comparison silently stops covering a
/// field the day someone adds one, and this harness is the only thing
/// standing behind the port.  Exhaustive by construction beats
/// exhaustive by inspection.
pub fn diff(design: &Design) -> Vec<String> {
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
