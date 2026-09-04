//! Deriving a module's debug symbols.
//!
//! `DefProps.sym` marks the defs a bluetcl session can name. bsc arrives
//! at the set by building Bluesim C++ blocks and reading back which defs
//! survived as class members, which is why the exporter runs
//! `simMakeCBlocks` and `simCOpt` and throws away everything but the
//! answer. The set is derivable from the module itself, and this derives
//! it.
//!
//! bsc's rule, in two parts. `SimMakeCBlocks` seeds the public set with
//! "all defs needed to compute CAN_FIRE and WILL_FIRE signals": the
//! transitive cone of the module's fire defs. `SimCOpt` then takes two
//! classes back out -- a def no generated function references at all,
//! and a def exactly one references that is also cheap enough to sink
//! into that function's stack frame.
//!
//! Which functions reference a def is not a question a module can
//! answer alone: bsc counts the design's schedule functions alongside
//! the module's own, and whether a module is the design's top decides
//! whether its methods have a reader outside it.  That is why this
//! runs over an assembled design rather than in the exporter.
//!
//! One part is not derivable at all.  Whether bsc will show a name is
//! three Id properties its front end sets, so the exporter records
//! that as `DefProps.nameable` and this filters by it.

use crate::expr::{Action, Expr, Stmt};
use crate::{Design, Module, StrId};
use std::collections::{BTreeSet, HashMap, HashSet};

/// A generated function, as bsc counts them when deciding whether a def
/// has more than one reader.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Site {
    Rule(u32),
    Method(u32),
    Reset(u32),
    /// the design schedule, which reads every rule's will-fire
    Schedule,
}

/// The transitive cone of a module's fire defs: the defs bsc would make
/// public before any of them are taken back out.
fn cone(m: &Module, by_name: &HashMap<StrId, usize>) -> BTreeSet<StrId> {
    let mut out = BTreeSet::new();
    let mut queue: Vec<StrId> = m
        .defs
        .iter()
        .filter(|d| d.props.can_fire || d.props.will_fire)
        .map(|d| d.name)
        .collect();
    while let Some(n) = queue.pop() {
        if !out.insert(n) {
            continue;
        }
        if let Some(&i) = by_name.get(&n) {
            each_def_ref(&m.defs[i].expr, &mut |r| queue.push(r));
        }
    }
    out
}

/// Every def this expression names, without following them.
fn each_def_ref(e: &Expr, f: &mut impl FnMut(StrId)) {
    match e {
        Expr::Def(n) => f(*n),
        Expr::MethCall { args, .. } | Expr::ForeignCall { args, .. } | Expr::Prim { args, .. } => {
            for a in args {
                each_def_ref(a, f);
            }
        }
        Expr::Clock { osc, gate } => {
            each_def_ref(osc, f);
            each_def_ref(gate, f);
        }
        Expr::Reset { wire } => each_def_ref(wire, f),
        Expr::If {
            cond, then_, else_, ..
        } => {
            each_def_ref(cond, f);
            each_def_ref(then_, f);
            each_def_ref(else_, f);
        }
        Expr::Case {
            scrutinee,
            arms,
            default,
            ..
        } => {
            each_def_ref(scrutinee, f);
            for (_, a) in arms {
                each_def_ref(a, f);
            }
            each_def_ref(default, f);
        }
        Expr::Const { .. }
        | Expr::Port(_)
        | Expr::Param(_)
        | Expr::Str(_)
        | Expr::Real(_)
        | Expr::TaskValue { .. }
        | Expr::MethValue { .. }
        | Expr::Gate { .. }
        | Expr::ClockOut { .. } => {}
    }
}

/// Attribute every def an expression reaches to the function reading it.
///
/// A def that stays a class member is computed in whichever function
/// needs it, so a def its body names is read by that same function --
/// the walk follows through def bodies, once each per site.
fn mark(
    e: &Expr,
    site: Site,
    m: &Module,
    by_name: &HashMap<StrId, usize>,
    refs: &mut HashMap<StrId, HashSet<Site>>,
    seen: &mut HashSet<StrId>,
) {
    let _ = (m, by_name, seen);
    each_def_ref(e, &mut |n| {
        refs.entry(n).or_default().insert(site);
    });
}

fn mark_action(
    a: &Action,
    site: Site,
    m: &Module,
    by_name: &HashMap<StrId, usize>,
    refs: &mut HashMap<StrId, HashSet<Site>>,
    seen: &mut HashSet<StrId>,
) {
    match a {
        Action::MethCall { cond, args, .. } => {
            mark(cond, site, m, by_name, refs, seen);
            for x in args {
                mark(x, site, m, by_name, refs, seen);
            }
        }
        Action::Foreign { cond, args, .. } | Action::Task { cond, args, .. } => {
            mark(cond, site, m, by_name, refs, seen);
            for x in args {
                mark(x, site, m, by_name, refs, seen);
            }
        }
    }
}

fn mark_stmts(
    body: &[Stmt],
    site: Site,
    m: &Module,
    by_name: &HashMap<StrId, usize>,
    refs: &mut HashMap<StrId, HashSet<Site>>,
    seen: &mut HashSet<StrId>,
) {
    for s in body {
        match s {
            Stmt::Def { expr, .. } => mark(expr, site, m, by_name, refs, seen),
            Stmt::Action(a) => mark_action(a, site, m, by_name, refs, seen),
            Stmt::AvAction { action, .. } => mark_action(action, site, m, by_name, refs, seen),
            Stmt::Cond { cond, then_, else_ } => {
                mark(cond, site, m, by_name, refs, seen);
                mark_stmts(then_, site, m, by_name, refs, seen);
                mark_stmts(else_, site, m, by_name, refs, seen);
            }
        }
    }
}

/// Whether bsc would sink this def into the one function that reads
/// it, leaving it with no name of its own.
///
/// Only a fire signal is ever sunk.  Everything else in the cone is
/// there because a fire signal is computed from it, and bsc keeps
/// those as members -- so of the defs with a single reader, the fire
/// ones go and the rest stay.  `-keep-fires` pins even those.
fn sinkable(m: &Module, i: usize) -> bool {
    let d = &m.defs[i];
    (d.props.can_fire || d.props.will_fire) && !m.keep_fires
}

/// Which functions read each def of a module.
fn references(
    m: &Module,
    by_name: &HashMap<StrId, usize>,
    is_top: bool,
) -> HashMap<StrId, HashSet<Site>> {
    let mut refs: HashMap<StrId, HashSet<Site>> = HashMap::new();
    for (ri, r) in m.rules.iter().enumerate() {
        let mut seen = HashSet::new();
        mark_stmts(
            &r.body,
            Site::Rule(ri as u32),
            m,
            by_name,
            &mut refs,
            &mut seen,
        );
    }
    for (mi, x) in m.methods.iter().enumerate() {
        let site = Site::Method(mi as u32);
        let mut seen = HashSet::new();
        // `ready` does not make the method a reader of the condition
        // itself -- that reaches generated code as the companion RDY_
        // value method, its own entry in this list, and counting the
        // field too would give it two readers where bsc's block has
        // one.  What the condition is computed FROM is read here,
        // though: the method's function evaluates it.
        if let Some(e) = &x.ready {
            each_def_ref(e, &mut |n| {
                if let Some(&i) = by_name.get(&n) {
                    each_def_ref(&m.defs[i].expr, &mut |o| {
                        refs.entry(o).or_default().insert(site);
                    });
                }
            });
        }
        mark_stmts(&x.body, site, m, by_name, &mut refs, &mut seen);
        if let Some(e) = &x.result {
            mark(e, site, m, by_name, &mut refs, &mut seen);
        }
        // A method's own will-fire is read where the method is
        // decided.  For the TOP module that is outside the design --
        // whatever drives the enable is a reader this module cannot
        // see, so the will-fire has one more than it looks.  For a
        // submodule the driver is its parent's code, which is a
        // reference in the parent's block and not in this one.
        if let Some(wf) = x.will_fire {
            refs.entry(wf).or_default().insert(site);
            if is_top {
                refs.entry(wf).or_default().insert(Site::Schedule);
            }
        }
    }
    // The schedule is what decides RULES, so it computes their fire
    // signals and reads whatever those are computed from.  It reaches
    // no further: each of those is a def with an assignment of its
    // own somewhere else.  A method's fire signals are not its
    // business -- the method's own function computes those.
    for r in &m.rules {
        refs.entry(r.will_fire).or_default().insert(Site::Schedule);
        if let Some(&i) = by_name.get(&r.will_fire) {
            each_def_ref(&m.defs[i].expr, &mut |n| {
                refs.entry(n).or_default().insert(Site::Schedule);
            });
        }
    }
    for (i, r) in m.resets.iter().enumerate() {
        let mut seen = HashSet::new();
        mark(
            &r.wire,
            Site::Reset(i as u32),
            m,
            by_name,
            &mut refs,
            &mut seen,
        );
    }
    refs
}

/// One module's symbols.
fn one(m: &Module, is_top: bool) -> BTreeSet<StrId> {
    let by_name: HashMap<StrId, usize> = m
        .defs
        .iter()
        .enumerate()
        .map(|(i, d)| (d.name, i))
        .collect();
    let public = cone(m, &by_name);
    let refs = references(m, &by_name, is_top);
    let syms = public
        .into_iter()
        // a name bsc will not show is not a symbol whatever the
        // analysis says
        .filter(|n| by_name.get(n).is_some_and(|&i| m.defs[i].props.nameable))
        .filter(|n| match refs.get(n).map(|s| s.len()).unwrap_or(0) {
            // nothing reads it: bsc deletes it outright
            0 => false,
            // one reader: kept only if it cannot be sunk into that reader
            1 => by_name.get(n).is_none_or(|&i| !sinkable(m, i)),
            _ => true,
        })
        .collect();
    syms
}

/// The symbol set of each module of an assembled design, indexed as
/// `design.modules` is.
pub fn derive(design: &Design) -> Vec<BTreeSet<StrId>> {
    design
        .modules
        .iter()
        .map(|m| one(m, m.name == design.top))
        .collect()
}
