//! Assembling a whole design out of the fragments of its modules.
//!
//! A fragment is one module carrying its own string table and no
//! cross-module data -- what `trs-bir` writes, one per module.
//!
//! "Module" here is bsc's synthesis boundary: one `(* synthesize *)`,
//! one `.ba`, one `Module` in the IR.  A BSV module that is not a
//! boundary never reaches this far -- elaboration inlines it into the
//! boundary that instantiates it, leaving its primitives behind under
//! prefixed names.  So the set handed to a link is the design's
//! boundaries, and how a design divides into fragments was decided by
//! where `(* synthesize *)` was written, not here.
//!
//! Linking a set of them means two things: translating every string id
//! into one combined table, and deriving the design-level facts --
//! which module is the top, and the merged schedule -- from the
//! modules themselves.
//!
//! The translation is written as exhaustive destructuring: no `..` in a
//! struct pattern, no `_` arm in a match.  A field that grows a string
//! id then fails to compile here, instead of silently carrying an id
//! that means a different string in the combined table.

use crate::expr::{Action, Expr, Stmt};
use crate::schedule::{
    Composition, CompositionEntry, DynSched, ModuleSchedule, QualRule,
    QualifiedTick, SchedAlt, Schedule, Segment, SubMethod, TickCall,
};
use crate::{
    Bir, BirBody, ClockArg, ClockDomain, DecodeError, Def, DefProps, Design,
    Extern, ForeignFunc, InputClock, Instance, InstanceKind, Lazy, Method,
    MethodRef, Module, Port, PrimClocks, Primitive, Reset, Rule, StrId,
};
use std::collections::{HashMap, HashSet};

/// One fragment's string ids translated into the combined table.
struct Remap(Vec<StrId>);

impl Remap {
    fn s(&self, id: &mut StrId) {
        *id = self.0[*id as usize];
    }
    fn opt(&self, id: &mut Option<StrId>) {
        if let Some(i) = id {
            self.s(i);
        }
    }
}

fn expr(r: &Remap, e: &mut Expr) {
    match e {
        Expr::Const { width: _, limbs: _ } => {}
        Expr::Def(id) | Expr::Port(id) | Expr::Param(id) | Expr::Str(id) => {
            r.s(id)
        }
        Expr::MethCall { width: _, instance, method, port: _, args } => {
            r.s(instance);
            r.s(method);
            for a in args {
                expr(r, a);
            }
        }
        Expr::MethValue { width: _, instance, method } => {
            r.s(instance);
            r.s(method);
        }
        Expr::TaskValue { width: _, cookie: _ } => {}
        Expr::ForeignCall { width: _, func, args } => {
            r.s(func);
            for a in args {
                expr(r, a);
            }
        }
        Expr::Clock { osc, gate } => {
            expr(r, osc);
            expr(r, gate);
        }
        Expr::Real(_) => {}
        Expr::Reset { wire } => expr(r, wire),
        Expr::Gate { instance, clock } | Expr::ClockOut { instance, clock } => {
            r.s(instance);
            r.s(clock);
        }
        Expr::Prim { op: _, width: _, args } => {
            for a in args {
                expr(r, a);
            }
        }
        Expr::If { width: _, cond, then_, else_ } => {
            expr(r, cond);
            expr(r, then_);
            expr(r, else_);
        }
        Expr::Case { width: _, scrutinee, arms, default } => {
            expr(r, scrutinee);
            for (_, a) in arms {
                expr(r, a);
            }
            expr(r, default);
        }
    }
}

fn action(r: &Remap, a: &mut Action) {
    match a {
        Action::MethCall { instance, method, port: _, cond, args } => {
            r.s(instance);
            r.s(method);
            expr(r, cond);
            for x in args {
                expr(r, x);
            }
        }
        Action::Foreign { func, cond, args, signed: _, assumption: _ } => {
            r.s(func);
            expr(r, cond);
            for x in args {
                expr(r, x);
            }
        }
        Action::Task {
            func,
            cookie: _,
            temp,
            width: _,
            cond,
            args,
            signed: _,
            assumption: _,
        } => {
            r.s(func);
            r.opt(temp);
            expr(r, cond);
            for x in args {
                expr(r, x);
            }
        }
    }
}

fn stmts(r: &Remap, v: &mut Vec<Stmt>) {
    for s in v {
        match s {
            Stmt::Def { name, expr: e } => {
                r.s(name);
                expr(r, e);
            }
            Stmt::Action(a) => action(r, a),
            Stmt::AvAction { def, action: a } => {
                r.s(def);
                action(r, a);
            }
            Stmt::Cond { cond, then_, else_ } => {
                expr(r, cond);
                stmts(r, then_);
                stmts(r, else_);
            }
        }
    }
}

fn port(r: &Remap, p: &mut Port) {
    let Port { name, width: _, kind: _, base } = p;
    r.s(name);
    r.opt(base);
}

fn input_clock(r: &Remap, c: &mut InputClock) {
    let InputClock { name, osc, gate } = c;
    r.s(name);
    r.s(osc);
    r.opt(gate);
}

fn clock_domain(r: &Remap, d: &mut ClockDomain) {
    let ClockDomain { id: _, clocks } = d;
    for (osc, gate) in clocks {
        expr(r, osc);
        expr(r, gate);
    }
}

fn prim_clocks(r: &Remap, p: &mut PrimClocks) {
    let PrimClocks { inputs, domains, outputs } = p;
    for c in inputs {
        input_clock(r, c);
    }
    for d in domains {
        clock_domain(r, d);
    }
    for (name, e) in outputs {
        r.s(name);
        expr(r, e);
    }
}

fn primitive(r: &Remap, p: &mut Primitive) {
    match p {
        Primitive::Reg { width: _, reset: _ }
        | Primitive::ConfigReg { width: _, reset: _ }
        | Primitive::CReg { width: _, ports: _, reset: _ }
        | Primitive::Wire { width: _ }
        | Primitive::Fifo {
            width: _,
            depth: _,
            guarded: _,
            loopy: _,
            bypass: _,
        }
        | Primitive::Bram {
            width: _,
            addr_width: _,
            ports: _,
            byte_enables: _,
        }
        | Primitive::ClockGen { params: _ }
        | Primitive::GatedClock
        | Primitive::ClockDivider { divisor: _ }
        | Primitive::SyncReg { width: _, stages: _ }
        | Primitive::SyncFifo { width: _, depth: _ } => {}
        Primitive::RegFile { width: _, addr_width: _, binary_init } => {
            r.opt(binary_init)
        }
        Primitive::Other { name } => r.s(name),
    }
}

fn instance(r: &Remap, i: &mut Instance) {
    let Instance {
        name,
        kind,
        clock_args,
        elab_order: _,
        prim_clocks: pc,
        args,
        method_order,
        port_counts,
    } = i;
    r.s(name);
    match kind {
        // an ExternRef indexes the module's own extern list, which
        // moves with it
        InstanceKind::Module(_) => {}
        InstanceKind::Prim(p) => primitive(r, p),
    }
    for c in clock_args {
        let ClockArg { name, arg: _, has_reset: _, ticks: _ } = c;
        r.s(name);
    }
    if let Some(p) = pc {
        prim_clocks(r, p);
    }
    for a in args {
        expr(r, a);
    }
    for (a, b) in method_order {
        r.s(a);
        r.s(b);
    }
    for (n, _) in port_counts {
        r.s(n);
    }
}

fn def(r: &Remap, d: &mut Def) {
    let Def { name, width: _, expr: e, props } = d;
    let DefProps { can_fire: _, will_fire: _, signed: _, sym: _, nameable: _ } =
        props;
    r.s(name);
    let mut body = (**e).clone();
    expr(r, &mut body);
    *e = Lazy::new(body);
}

fn rule(r: &Remap, x: &mut Rule) {
    let Rule {
        name,
        can_fire,
        will_fire,
        body,
        clock_domain: _,
        crossing: _,
        me_inhibits: _,
    } = x;
    r.s(name);
    r.s(can_fire);
    r.s(will_fire);
    let mut b = (**body).clone();
    stmts(r, &mut b);
    *body = Lazy::new(b);
}

fn method(r: &Remap, m: &mut Method) {
    let Method {
        name,
        kind: _,
        args,
        ready,
        body,
        result,
        clock_domain: _,
        always_enabled: _,
        rdy,
        will_fire,
        en,
    } = m;
    r.s(name);
    for a in args {
        port(r, a);
    }
    if let Some(e) = ready {
        expr(r, e);
    }
    stmts(r, body);
    if let Some(e) = result {
        expr(r, e);
    }
    r.opt(rdy);
    r.opt(will_fire);
    r.opt(en);
}

fn schedule(r: &Remap, s: &mut Schedule) {
    // every other field of a Schedule names rules, methods and
    // segments by position, and positions move with the module
    let Schedule {
        domains,
        conflicts: _,
        task_rules: _,
        finish_rules: _,
        sched_graph: _,
        disjoint_rules: _,
        ffunc_edges: _,
        dyn_scheds,
    } = s;
    for d in domains {
        let ModuleSchedule { domain: _, posedge: _, segments, ticks } = d;
        for g in segments {
            let Segment { nodes: _, cut } = g;
            for c in cut {
                r.s(c);
            }
        }
        for t in ticks {
            let TickCall { instance, port } = t;
            r.s(instance);
            r.s(port);
        }
    }
    for d in dyn_scheds {
        match d {
            DynSched::Pair {
                rule_e: _,
                guard_e,
                rule_l: _,
                guard_l,
                meths,
                between,
            } => {
                for (a, b) in meths {
                    sub_method(r, a);
                    sub_method(r, b);
                }
                expr(r, guard_e);
                if let Some(g) = guard_l {
                    expr(r, g);
                }
                for b in between {
                    r.s(b);
                }
            }
            DynSched::SelfCall {
                rule: _,
                guard,
                early,
                late,
                between,
            } => {
                sub_method(r, early);
                sub_method(r, late);
                expr(r, guard);
                for b in between {
                    r.s(b);
                }
            }
        }
    }
}

fn module(r: &Remap, m: &mut Module) {
    let Module {
        name,
        externs,
        foreign_calls,
        def_ix,
        method_ix,
        content_hash: _,
        keep_fires: _,
        default_clock,
        default_reset,
        clock_domains,
        resets,
        inputs,
        input_clocks,
        ifc_clocks,
        ifc_clock_gates,
        ifc_resets,
        instances,
        defs,
        rules,
        methods,
        schedule: sched,
    } = m;
    r.s(name);
    for e in externs {
        let Extern { module } = e;
        r.s(module);
    }
    for c in foreign_calls {
        r.s(c);
    }
    // derived indices, rebuilt wholesale once the table is final
    def_ix.clear();
    method_ix.clear();
    r.opt(default_clock);
    r.opt(default_reset);
    for d in clock_domains {
        clock_domain(r, d);
    }
    for x in resets {
        let Reset { id: _, wire } = x;
        expr(r, wire);
    }
    for p in inputs {
        port(r, p);
    }
    for c in input_clocks {
        input_clock(r, c);
    }
    for (n, e) in ifc_clocks.iter_mut().chain(ifc_clock_gates.iter_mut()) {
        r.s(n);
        expr(r, e);
    }
    for (a, b) in ifc_resets {
        r.s(a);
        r.s(b);
    }
    for i in instances {
        instance(r, i);
    }
    for d in defs {
        def(r, d);
    }
    for x in rules {
        rule(r, x);
    }
    for x in methods {
        method(r, x);
    }
    schedule(r, sched);
}

fn sub_method(r: &Remap, m: &mut SubMethod) {
    let SubMethod { instance: _, name, method: _ } = m;
    r.s(name);
}

fn qual_rule(r: &Remap, q: &mut QualRule) {
    let QualRule { instance, rule: _ } = q;
    r.s(instance);
}

fn entries(r: &Remap, es: &mut Vec<CompositionEntry>) {
    for e in es {
        let CompositionEntry { instance, domain: _, segment: _ } = e;
        r.s(instance);
    }
}

fn inhibits(r: &Remap, ps: &mut Vec<(QualRule, QualRule)>) {
    for (a, b) in ps {
        qual_rule(r, a);
        qual_rule(r, b);
    }
}

/// A composition the file already carried.
///
/// The merge recomputes these, so the translated copy is what the
/// oracle compares against rather than what the design runs on.
fn composition(r: &Remap, c: &mut Composition) {
    let Composition {
        clock,
        posedge: _,
        entries: es,
        ticks,
        early,
        cross_inhibits,
        alts,
    } = c;
    r.s(clock);
    entries(r, es);
    for t in ticks {
        let QualifiedTick { instance, prim, port, reset: _, gate } = t;
        r.s(instance);
        r.s(prim);
        r.s(port);
        if let Some(g) = gate {
            expr(r, g);
        }
    }
    for q in early {
        qual_rule(r, q);
    }
    inhibits(r, cross_inhibits);
    for a in alts {
        let SchedAlt { guard_inst, guard, entries: es, cross_inhibits } = a;
        r.s(guard_inst);
        expr(r, guard);
        entries(r, es);
        inhibits(r, cross_inhibits);
    }
}

fn foreign_func(r: &Remap, f: &mut ForeignFunc) {
    let ForeignFunc { name, c_name, ret: _, args: _ } = f;
    r.s(name);
    r.s(c_name);
}

/// Turn each flagged submodule call's method NAME into its position.
///
/// A module is exported without reading the modules it instantiates,
/// so it cannot know where a method sits in a child's list -- it
/// records the name.  Here the child is present, so the name resolves,
/// and everything downstream sees the position it expects.
pub(crate) fn resolve_sub_methods(design: &mut Design) -> Result<(), DecodeError> {
    let by_name: HashMap<StrId, usize> =
        design.modules.iter().enumerate().map(|(i, m)| (m.name, i)).collect();

    // (module, instance, method name) -> position, worked out against
    // the immutable design before anything is written back
    let mut fixups: Vec<(usize, usize, usize, MethodRef)> = Vec::new();
    for (mi, m) in design.modules.iter().enumerate() {
        for (di, d) in m.schedule.dyn_scheds.iter().enumerate() {
            let subs: Vec<&SubMethod> = match d {
                DynSched::Pair { meths, .. } => {
                    meths.iter().flat_map(|(a, b)| [a, b]).collect()
                }
                DynSched::SelfCall { early, late, .. } => vec![early, late],
            };
            for (si, sub) in subs.iter().enumerate() {
                let inst = m.instances.get(sub.instance as usize).ok_or_else(|| {
                    DecodeError::Link(format!(
                        "{}: a flagged call names instance {}, which it does \
                         not have",
                        design.name(m.name),
                        sub.instance
                    ))
                })?;
                let InstanceKind::Module(x) = inst.kind else {
                    return Err(DecodeError::Link(format!(
                        "{}: a flagged call is on `{}', which is not a \
                         synthesized module",
                        design.name(m.name),
                        design.name(inst.name)
                    )));
                };
                let child_name = m.externs[x.idx()].module;
                let child = by_name.get(&child_name).ok_or_else(|| {
                    DecodeError::Link(format!(
                        "no fragment for `{}', instantiated by `{}'",
                        design.name(child_name),
                        design.name(m.name)
                    ))
                })?;
                let k = design.modules[*child]
                    .method_idx(sub.name)
                    .ok_or_else(|| {
                        DecodeError::Link(format!(
                            "`{}' has no method `{}', which `{}' calls",
                            design.name(child_name),
                            design.name(sub.name),
                            design.name(m.name)
                        ))
                    })?;
                fixups.push((mi, di, si, MethodRef(k as u32)));
            }
        }
    }
    for (mi, di, si, k) in fixups {
        let d = &mut design.modules[mi].schedule.dyn_scheds[di];
        let subs: Vec<&mut SubMethod> = match d {
            DynSched::Pair { meths, .. } => {
                meths.iter_mut().flat_map(|(a, b)| [a, b]).collect()
            }
            DynSched::SelfCall { early, late, .. } => vec![early, late],
        };
        if let Some(sub) = subs.into_iter().nth(si) {
            sub.method = k;
        }
    }
    Ok(())
}

/// Combine the contents of a set of .bir files into one design.
///
/// Each file contributes its modules and its own string table; the
/// combined table is built here and every id is translated into it.
/// What no fragment carries is then established: the top is the one
/// module no other module instantiates, its own pragmas supply the
/// default clock and reset, and the schedule is the merge over the
/// assembled hierarchy.  The structural check and the merge are the
/// ones every design runs, so an unchecked design cannot come out of
/// here.
///
/// A whole-design file is the one-input case rather than a path of its
/// own -- its stated top is cross-checked against the derived one, and
/// the schedule it carries is recomputed.  Mixing one with fragments
/// is refused: it is already linked.
pub fn assemble(birs: Vec<Bir>) -> Result<Design, DecodeError> {
    let err = |m: String| DecodeError::Link(m);
    if birs.is_empty() {
        return Err(err("no .bir files to link".to_string()));
    }
    let linked = birs
        .iter()
        .filter(|b| matches!(b.body, BirBody::Design(_)))
        .count();
    if linked > 0 && birs.len() > 1 {
        return Err(err(
            "one of these files is a linked design already; link \
             fragments, or that file on its own"
                .to_string(),
        ));
    }

    // the combined table, grown through Design::intern as each file's
    // strings are folded in; the modules are attached at the end
    let mut design = Design {
        strings: Vec::new(),
        str_ids: HashMap::new(),
        uses_wave_tasks: false,
        top: 0,
        modules: Vec::new(),
        compositions: vec![],
        foreign_funcs: Vec::new(),
        default_clock: None,
        default_reset: None,
    };
    let mut modules: Vec<Module> = Vec::new();
    let mut seen_ffunc: HashSet<StrId> = HashSet::new();
    let mut stated_top: Option<StrId> = None;

    for bir in birs {
        let r =
            Remap(bir.strings.iter().map(|s| design.intern(s)).collect());
        design.uses_wave_tasks |= bir.uses_wave_tasks;
        let mut take_ffunc = |design: &mut Design, mut f: ForeignFunc| {
            foreign_func(&r, &mut f);
            if seen_ffunc.insert(f.name) {
                design.foreign_funcs.push(f);
            }
        };
        match bir.body {
            BirBody::Fragment(mut m) => {
                module(&r, &mut m);
                modules.push(m);
            }
            BirBody::Foreign(f) => {
                take_ffunc(&mut design, f);
            }
            BirBody::Design(d) => {
                stated_top = Some(r.0[d.top as usize]);
                for f in d.foreign_funcs {
                    take_ffunc(&mut design, f);
                }
                for mut m in d.modules {
                    module(&r, &mut m);
                    modules.push(m);
                }
                // carried across, not used: the merge below recomputes
                // the schedule.  What this is for is the oracle, which
                // compares the two while the exporter still writes one.
                design.compositions = d.compositions;
                for c in &mut design.compositions {
                    composition(&r, c);
                }
            }
        }
    }

    let mut by_name: HashSet<StrId> = HashSet::new();
    for m in &modules {
        if !by_name.insert(m.name) {
            return Err(err(format!(
                "module `{}' comes from more than one file",
                design.name(m.name)
            )));
        }
    }
    let mut referenced: HashSet<StrId> = HashSet::new();
    for m in &modules {
        for e in &m.externs {
            if !by_name.contains(&e.module) {
                return Err(err(format!(
                    "no fragment for `{}', instantiated by `{}'",
                    design.name(e.module),
                    design.name(m.name)
                )));
            }
            referenced.insert(e.module);
        }
    }
    // A called import with no signature is a missing file, not a
    // missing implementation: the implementation is supplied to the
    // link (--bdpi) and its absence traps by name at run time, but
    // without the signature there is nothing to marshal a call into.
    for m in &modules {
        for c in &m.foreign_calls {
            if !seen_ffunc.contains(c) {
                return Err(err(format!(
                    "no .bir for the BDPI import `{}', called by `{}'",
                    design.name(*c),
                    design.name(m.name)
                )));
            }
        }
    }
    if modules.is_empty() {
        return Err(err(
            "no module in any of these files: a design needs at least the \
             one it is topped by"
                .to_string(),
        ));
    }
    let roots: Vec<StrId> = modules
        .iter()
        .map(|m| m.name)
        .filter(|n| !referenced.contains(n))
        .collect();
    let top = match roots.as_slice() {
        [t] => *t,
        [] => {
            return Err(err(
                "no top module: every one of these is instantiated by \
                 another"
                    .to_string(),
            ))
        }
        many => {
            let names: Vec<&str> =
                many.iter().map(|n| design.name(*n)).collect();
            return Err(err(format!(
                "more than one top module: {} -- link the fragments of \
                 one design",
                names.join(", ")
            )));
        }
    };
    if let Some(stated) = stated_top {
        if stated != top {
            return Err(err(format!(
                "this design says its top is `{}', but `{}' is the module \
                 nothing instantiates",
                design.name(stated),
                design.name(top)
            )));
        }
    }

    design.top = top;
    // the top's own pragmas, which bsc derived when it was the root of
    // an export -- for a fragment set, of its own export
    let top_module = modules
        .iter()
        .find(|m| m.name == top)
        .expect("the top is one of these modules");
    design.default_clock = top_module.default_clock;
    design.default_reset = top_module.default_reset;
    design.modules = modules;
    design.index_strings();
    design.finish()?;
    Ok(design)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::Expr;
    use crate::schedule::{ModuleSchedule, Schedule, Segment, TickCall};
    use crate::{
        ClockArg, ClockDomain, DefProps, ForeignType, MethodKind, PortKind,
        Primitive, RuleRef, Ticks,
    };
    use crate::schedule::SchedNode;
    use crate::SchedEntity;

    fn one() -> Expr {
        Expr::Const { width: 1, limbs: vec![1] }
    }

    /// A two-module design with a distinct string in every field the
    /// link has to translate.  Names are what the assertions check, so
    /// no two of them are the same.
    fn parent_and_child() -> Design {
        let mut d = crate::tests::tiny_design();
        d.strings = [
            "mkTop",    // 0  module name
            "mkKid",    // 1  submodule name (also the extern)
            "kid",      // 2  instance name
            "CLK",      // 3  clock port
            "clk_in",   // 4  input clock name
            "GATE",     // 5  clock gate port
            "a_def",    // 6  def
            "a_rule",   // 7  rule
            "CAN_a",    // 8  rule can-fire
            "WILL_a",   // 9  rule will-fire
            "get",      // 10 method
            "RDY_get",  // 11 method ready signal
            "arg0",     // 12 method argument port
            "cut_sig",  // 13 a segment cut
            "TICK",     // 14 tick port
            "ffunc",    // 15 foreign function
            "c_ffunc",  // 16 its C name
            "memfile",  // 17 a RegFile init file
            "reg",      // 18 the regfile instance
            "",         // 19 the top's own path
        ]
        .iter()
        .map(|s| (*s).to_string())
        .collect();

        let port = |name, kind| crate::Port { name, width: 1, kind, base: None };
        let kid = crate::Module {
            name: 1,
            externs: vec![],
            foreign_calls: vec![],
            def_ix: HashMap::new(),
            method_ix: HashMap::new(),
            content_hash: [0; 32],
            keep_fires: false,
            default_clock: None,
            default_reset: None,
            clock_domains: vec![ClockDomain {
                id: 0,
                clocks: vec![(Expr::Port(3), one())],
            }],
            resets: vec![],
            inputs: vec![port(3, PortKind::Clock), port(12, PortKind::MethodArg)],
            input_clocks: vec![crate::InputClock {
                name: 4,
                osc: 3,
                gate: Some(5),
            }],
            ifc_clocks: vec![],
            ifc_clock_gates: vec![],
            ifc_resets: vec![],
            instances: vec![crate::Instance {
                name: 18,
                kind: InstanceKind::Prim(Primitive::RegFile {
                    width: 8,
                    addr_width: 4,
                    binary_init: Some(17),
                }),
                clock_args: vec![ClockArg {
                    name: 4,
                    arg: 0,
                    has_reset: false,
                    ticks: Ticks::Pos,
                }],
                elab_order: 0,
                prim_clocks: None,
                args: vec![],
                method_order: vec![],
                port_counts: vec![],
            }],
            defs: vec![crate::Def {
                name: 6,
                width: 1,
                expr: Lazy::new(Expr::Port(12)),
                props: DefProps {
                    can_fire: false,
                    will_fire: false,
                    signed: false,
                    sym: false,
                    nameable: true,
                },
            }],
            rules: vec![],
            methods: vec![crate::Method {
                name: 10,
                kind: MethodKind::Value,
                args: vec![port(12, PortKind::MethodArg)],
                ready: Some(one()),
                body: vec![],
                result: Some(Expr::Def(6)),
                clock_domain: 0,
                always_enabled: false,
                rdy: Some(11),
                will_fire: None,
                en: None,
            }],
            schedule: Schedule::default(),
        };

        let top = crate::Module {
            name: 0,
            externs: vec![Extern { module: 1 }],
            foreign_calls: vec![15],
            def_ix: HashMap::new(),
            method_ix: HashMap::new(),
            content_hash: [0; 32],
            keep_fires: false,
            default_clock: None,
            default_reset: None,
            clock_domains: vec![ClockDomain {
                id: 0,
                clocks: vec![(Expr::Port(3), one())],
            }],
            resets: vec![],
            inputs: vec![port(3, PortKind::Clock)],
            input_clocks: vec![crate::InputClock {
                name: 4,
                osc: 3,
                gate: None,
            }],
            ifc_clocks: vec![],
            ifc_clock_gates: vec![],
            ifc_resets: vec![],
            instances: vec![crate::Instance {
                name: 2,
                kind: InstanceKind::Module(crate::ExternRef(0)),
                clock_args: vec![ClockArg {
                    name: 4,
                    arg: 0,
                    has_reset: false,
                    ticks: Ticks::Pos,
                }],
                elab_order: 0,
                prim_clocks: None,
                args: vec![],
                method_order: vec![(10, 10)],
                port_counts: vec![(12, 1)],
            }],
            defs: vec![],
            rules: vec![crate::Rule {
                name: 7,
                can_fire: 8,
                will_fire: 9,
                body: Lazy::new(vec![Stmt::Action(Action::Foreign {
                    func: 15,
                    cond: one(),
                    args: vec![Expr::MethCall {
                        width: 1,
                        instance: 2,
                        method: 10,
                        port: 0,
                        args: vec![one()],
                    }],
                    signed: vec![false],
                    assumption: false,
                })]),
                clock_domain: 0,
                crossing: false,
                me_inhibits: vec![],
            }],
            methods: vec![],
            schedule: Schedule {
                domains: vec![ModuleSchedule {
                    domain: 0,
                    posedge: true,
                    segments: vec![
                        Segment {
                            nodes: vec![SchedNode::Sched(SchedEntity::Rule(
                                RuleRef(0),
                            ))],
                            cut: vec![13],
                        },
                        Segment {
                            nodes: vec![SchedNode::Exec(SchedEntity::Rule(
                                RuleRef(0),
                            ))],
                            cut: vec![],
                        },
                    ],
                    ticks: vec![TickCall { instance: 2, port: 14 }],
                }],
                ..Schedule::default()
            },
        };

        d.modules = vec![top, kid];
        d.modules[0].default_clock = Some(3);
        d.top = 0;
        d.default_clock = Some(3);
        d.foreign_funcs = vec![ForeignFunc {
            name: 15,
            c_name: 16,
            ret: ForeignType::Void,
            args: vec![],
        }];
        d.index_strings();
        d
    }

    /// Split a design into one fragment per module, each with the whole
    /// table ROTATED by a different amount.  Rotating is what makes the
    /// round trip a real check: every id in a fragment means a
    /// different string than it did, so a field the link forgets to
    /// translate comes out holding the wrong name rather than the
    /// right one by luck.
    fn scatter(d: &Design) -> Vec<Bir> {
        let n = d.strings.len();
        // a table rotated by `shift`, and the remap back out of it
        let rot = |shift: usize| {
            let strings: Vec<String> =
                (0..n).map(|i| d.strings[(i + shift) % n].clone()).collect();
            let back = Remap(
                (0..n).map(|i| ((i + n - shift) % n) as StrId).collect(),
            );
            (strings, back)
        };
        // one file per foreign signature, as an export writes them
        let mut out: Vec<Bir> = d
            .foreign_funcs
            .iter()
            .enumerate()
            .map(|(k, f)| {
                let (strings, back) = rot((k % (n - 1)) + 1);
                let mut f = f.clone();
                foreign_func(&back, &mut f);
                Bir {
                    strings,
                    uses_wave_tasks: false,
                    body: BirBody::Foreign(f),
                }
            })
            .collect();
        // one per module, the top last as a link expects
        out.extend(d.modules.iter().rev().map(|m| {
            let (strings, back) = rot((m.name as usize % (n - 1)) + 1);
            let mut m = m.clone();
            module(&back, &mut m);
            Bir {
                strings,
                uses_wave_tasks: d.uses_wave_tasks,
                body: BirBody::Fragment(m),
            }
        }));
        out
    }

    /// A scatter split into its foreign files and its module fragments,
    /// the latter in the order a link is given them, top last.
    fn scatter_parts(d: &Design) -> (Vec<Bir>, Vec<Bir>) {
        scatter(d)
            .into_iter()
            .partition(|b| matches!(b.body, BirBody::Foreign(_)))
    }

    /// Every string a module reaches through the table survives the
    /// link, whatever ids the fragment it came in used.
    #[test]
    fn a_link_translates_every_name() {
        let d = parent_and_child();
        let out = assemble(scatter(&d)).expect("links");
        let n = |id: StrId| out.name(id).to_string();

        let kid = out.modules.iter().find(|m| n(m.name) == "mkKid").unwrap();
        let top = out.modules.iter().find(|m| n(m.name) == "mkTop").unwrap();

        assert_eq!(n(out.top), "mkTop", "the top is the module nothing instantiates");
        assert_eq!(n(top.externs[0].module), "mkKid", "extern");
        assert_eq!(out.default_clock.map(n), Some("CLK".to_string()), "default clock");

        let i = &top.instances[0];
        assert_eq!(n(i.name), "kid", "instance name");
        assert_eq!(n(i.clock_args[0].name), "clk_in", "clock argument");
        assert_eq!(i.method_order.iter().map(|(a, b)| (n(*a), n(*b))).collect::<Vec<_>>(),
                   vec![("get".to_string(), "get".to_string())], "method order");
        assert_eq!(n(i.port_counts[0].0), "arg0", "port counts");

        assert_eq!(n(top.inputs[0].name), "CLK", "port");
        assert_eq!(n(top.input_clocks[0].name), "clk_in", "input clock");
        assert_eq!(kid.input_clocks[0].gate.map(n), Some("GATE".to_string()), "clock gate");
        assert_eq!(kid.clock_domains[0].clocks[0].0, Expr::Port(top.inputs[0].name), "domain osc");

        assert_eq!(n(kid.defs[0].name), "a_def", "def");
        assert_eq!(*kid.defs[0].expr, Expr::Port(kid.inputs[1].name), "def body");
        assert_eq!(n(kid.methods[0].name), "get", "method");
        assert_eq!(kid.methods[0].rdy.map(n), Some("RDY_get".to_string()), "method ready");
        assert_eq!(n(kid.methods[0].args[0].name), "arg0", "method argument");

        let r = &top.rules[0];
        assert_eq!((n(r.name), n(r.can_fire), n(r.will_fire)),
                   ("a_rule".into(), "CAN_a".into(), "WILL_a".into()), "rule");
        let Stmt::Action(Action::Foreign { func, args, .. }) = &r.body[0] else {
            panic!("the rule's body is a foreign call")
        };
        assert_eq!(n(*func), "ffunc", "foreign call in a rule body");
        assert_eq!(top.foreign_calls.iter().map(|c| n(*c)).collect::<Vec<_>>(),
                   vec!["ffunc".to_string()], "declared foreign call");
        let Expr::MethCall { instance, method, .. } = &args[0] else {
            panic!("its argument is a method call")
        };
        assert_eq!((n(*instance), n(*method)), ("kid".into(), "get".into()), "nested call");

        let sd = &top.schedule.domains[0];
        assert_eq!(n(sd.segments[0].cut[0]), "cut_sig", "segment cut");
        assert_eq!((n(sd.ticks[0].instance), n(sd.ticks[0].port)),
                   ("kid".into(), "TICK".into()), "tick call");

        let InstanceKind::Prim(Primitive::RegFile { binary_init, .. }) =
            &kid.instances[0].kind
        else {
            panic!("the child holds a regfile")
        };
        assert_eq!(binary_init.map(n), Some("memfile".to_string()), "primitive string");

        assert_eq!((n(out.foreign_funcs[0].name), n(out.foreign_funcs[0].c_name)),
                   ("ffunc".into(), "c_ffunc".into()), "foreign function");
        assert_eq!(out.foreign_funcs.len(), 1,
                   "a foreign function every fragment carries is linked once");
    }

    /// The design-level facts no fragment carries.
    #[test]
    fn a_link_derives_what_a_fragment_leaves_out() {
        let d = parent_and_child();
        let out = assemble(scatter(&d)).expect("links");
        assert!(!out.compositions.is_empty(), "the schedule is merged, not read");
        assert_eq!(
            out.name(out.compositions[0].clock),
            "CLK",
            "the composition names its clock"
        );
        // a composition entry names its instance by path, so the paths
        // are in the table whether or not this design's schedule has
        // an entry that reaches for one
        assert_eq!(out.str_id("").is_some(), true, "the top's path");
        assert_eq!(out.str_id("kid").is_some(), true, "the child's path");
    }

    /// A composition's clock can be a submodule's output clock, whose
    /// name is a join no fragment has a reason to carry.
    #[test]
    fn a_composed_clock_name_is_interned() {
        let mut d = parent_and_child();
        // the top's domain now runs off the child's output clock
        let strings = d.strings.len() as StrId;
        d.strings.push("CLK_OUT".to_string());
        d.modules[1].ifc_clocks = vec![(strings, Expr::Port(3))];
        d.modules[0].clock_domains[0].clocks =
            vec![(Expr::ClockOut { instance: 2, clock: strings }, one())];
        d.index_strings();

        let out = assemble(scatter(&d)).expect("links");
        assert_eq!(
            out.name(out.compositions[0].clock),
            "kid$CLK_OUT",
            "the clock is named by joining the instance to its output clock"
        );
    }

    #[test]
    fn a_link_needs_one_top() {
        let d = parent_and_child();
        let (ffs, frags) = scatter_parts(&d);
        // the signatures are always there; what varies is the modules
        let with = |bs: Vec<Bir>| -> Vec<Bir> {
            let mut v = ffs.clone();
            v.extend(bs);
            v
        };

        let err = assemble(vec![]).unwrap_err().to_string();
        assert!(err.contains("no .bir files"), "{err}");

        // the top alone: the module it instantiates is absent
        let err =
            assemble(with(vec![frags[1].clone()])).unwrap_err().to_string();
        assert!(err.contains("no fragment for `mkKid'"), "{err}");

        // two unrelated tops
        let mut other = frags[1].clone();
        let id = other.strings.len() as StrId;
        other.strings.push("mkOther".to_string());
        let BirBody::Fragment(m) = &mut other.body else { panic!("a fragment") };
        m.name = id;
        let err =
            assemble(with(vec![frags[0].clone(), frags[1].clone(), other]))
                .unwrap_err()
                .to_string();
        assert!(err.contains("more than one top module"), "{err}");

        // no file for an import a module says it calls
        let err = assemble(frags.clone()).unwrap_err().to_string();
        assert!(err.contains("no .bir for the BDPI import `ffunc'"), "{err}");
    }

    #[test]
    fn a_link_rejects_an_inconsistent_set() {
        let d = parent_and_child();
        let (ffs, frags) = scatter_parts(&d);

        let mut dup = ffs;
        dup.extend([frags[0].clone(), frags[1].clone(), frags[1].clone()]);
        let err = assemble(dup).unwrap_err().to_string();
        assert!(err.contains("more than one file"), "{err}");

        // fragments built with different -keep-fires settings link
        // fine: each boundary keeps the signals it was asked to
        let mut mixed = scatter(&d);
        let m = mixed
            .iter_mut()
            .find_map(|b| match &mut b.body {
                BirBody::Fragment(m) => Some(m),
                _ => None,
            })
            .expect("a fragment");
        m.keep_fires = true;
        let out = assemble(mixed).expect("links");
        assert_eq!(
            out.modules.iter().filter(|m| m.keep_fires).count(),
            1,
            "the setting travels with the module it was made for"
        );
    }
}
