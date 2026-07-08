//! Hybrid JIT (feature `jit`, runtime-gated by TRS_JIT=1): eligible
//! rules run as LLVM-compiled functions inside the interpreter's event
//! loop, over a shared u64 arena (see trs-codegen::lower).
//!
//! v1 scope — all-or-nothing: the whole design runs compiled or the
//! whole design stays interpreted.  A composition is compilable when it
//! has no early (clock-crossing) rules and every schedule node is a
//! rule (not a method) whose CF/WF cone and body lower successfully:
//! plain ≤64-bit sync registers, reset-port reads, the scalar PrimOps,
//! and $display-family statements whose arguments re-evaluate safely at
//! callback time.  VCD tracing disables the JIT (def-value recording
//! and per-prim dump hooks want the interpreted paths).

use super::*;
use trs_codegen::lower::{
    compile_rules, CompiledRule, JitEngine, PlanEnv, RuleSpec,
};

fn words_for(w: u32) -> u32 {
    w.div_ceil(64)
}

/// Zero-divisor trap for compiled Quot/Rem: raise SIGFPE like the
/// interpreter (Value::quot) and native division.
pub(crate) unsafe extern "C" fn jit_sigfpe_cb() {
    libc::raise(libc::SIGFPE);
}

/// One dispatch step of a compiled composition, in entries order.
pub(crate) enum JitNode {
    Sched(unsafe extern "C" fn(*mut u64)),
    Exec(unsafe extern "C" fn(*mut u64, *mut core::ffi::c_void) -> i32),
}

/// Compiled state carried by the Stepper.
pub(crate) struct JitPlans {
    /// the shared state arena; register prims and Interp::jit_arena_ptr
    /// hold raw pointers into this allocation (heap address is stable)
    _arena: Box<[u64]>,
    arena_ptr: *mut u64,
    /// per-composition dispatch lists (parallel to Stepper::rcomps)
    pub(crate) comp_nodes: Vec<Option<Vec<JitNode>>>,
    _engines: Vec<JitEngine>,
}

impl JitPlans {
    pub(crate) fn arena_ptr(&self) -> *mut u64 {
        self.arena_ptr
    }
}

/// The callback compiled code uses for $display-family statements.
/// `env` is the owning Interp; the token encodes (rule ordinal << 16 |
/// local statement index) resolved through Interp::jit_tokens.
pub(crate) unsafe extern "C" fn jit_foreign_cb(
    env: *mut core::ffi::c_void,
    token: u64,
) -> i32 {
    let interp = &mut *(env as *mut Interp);
    let ordinal = (token >> 16) as usize;
    let local = (token & 0xffff) as usize;
    let (inst, rule_idx, ref paths) = interp.jit_tokens[ordinal];
    let path = paths[local].clone();
    interp.jit_run_foreign(inst, rule_idx, &path);
    interp.finished.is_some() as i32
}

impl Interp {
    /// Execute one body statement located by its index path (Cond arms
    /// are path elements: stmt index, then 0/1 for then/else, ...).
    fn jit_run_foreign(&mut self, inst: usize, rule_idx: usize, path: &[u32]) {
        let module = self.module_of(inst);
        let mir = self.mods[module].ir;
        let mut stmts = self.d.modules[mir].rules[rule_idx].body.clone();
        let mut it = path.iter();
        loop {
            let Some(&i) = it.next() else { return };
            let st = stmts[i as usize].clone();
            match (it.next(), st) {
                (None, st) => {
                    // the compiled branch already evaluated the action's
                    // condition (against pre-store state); re-evaluating
                    // it here would see mid-body mutations — force true
                    let st = match st {
                        Stmt::Action(Action::Foreign { func, args, signed, .. }) => {
                            Stmt::Action(Action::Foreign {
                                func,
                                cond: Expr::Const { width: 1, limbs: vec![1] },
                                args,
                                signed,
                            })
                        }
                        other => other,
                    };
                    let mut ctx = Ctx::default();
                    self.exec_stmt(inst, &mut ctx, &st);
                    return;
                }
                (Some(0), Stmt::Cond { then_, .. }) => stmts = then_,
                (Some(1), Stmt::Cond { else_, .. }) => stmts = else_,
                _ => panic!("trs jit: bad foreign statement path"),
            }
        }
    }

    /// Build the JIT plan for the resolved compositions, or None to run
    /// fully interpreted.  Called once from prime().
    pub(crate) fn jit_plan(&mut self, rcomps: &[RComp]) -> Option<JitPlans> {
        if std::env::var_os("TRS_JIT").is_none() {
            return None;
        }
        let trace = std::env::var_os("TRS_JIT_TRACE").is_some();
        if self.vcd_trace || self.vcd_file_pending.is_some() {
            if trace {
                eprintln!("trs jit: off (VCD tracing)");
            }
            return None;
        }

        let mut nslots: u32 = 0;
        let mut alloc = |n: &mut u32, words: u32| {
            let s = *n;
            *n += words;
            s
        };

        // plain-register slots, per owning user instance
        let mut reg_slot_by_inst: HashMap<usize, HashMap<StrId, (u32, u32)>> =
            HashMap::new();
        let mut attach: Vec<(usize, u32)> = Vec::new(); // (prim inst, slot)
        for i in 0..self.insts.len() {
            let InstKind::User { children, .. } = &self.insts[i].kind else {
                continue;
            };
            let kids: Vec<(StrId, usize)> =
                children.iter().map(|(k, v)| (*k, *v)).collect();
            for (name, ci) in kids {
                let InstKind::Prim(p) = &self.insts[ci].kind else { continue };
                if let Some(w) = p.arena_width() {
                    let s = alloc(&mut nslots, words_for(w));
                    reg_slot_by_inst.entry(i).or_default().insert(name, (s, w));
                    attach.push((ci, s));
                }
            }
        }

        // reset-node slots holding the port LEVEL (1 = deasserted)
        let reset_node_slot: Vec<u32> =
            (0..self.rst_asserted.len()).map(|_| alloc(&mut nslots, 1)).collect();

        // CF/WF slots for every scheduled rule, plus rule specs skeleton
        struct RuleInfo {
            inst: usize,
            rule_idx: usize,
            ordinal: usize,
            cf_slot: u32,
            wf_slot: u32,
            eager: Vec<StrId>,
        }
        let mut rules: Vec<RuleInfo> = Vec::new();
        let mut rule_ord: HashMap<(usize, StrId), usize> = HashMap::new();
        let mut cfwf_by_inst: HashMap<usize, HashMap<StrId, u32>> = HashMap::new();
        let mut eager_by_inst: HashMap<usize, HashMap<StrId, (u32, u32)>> = HashMap::new();
        for rc in rcomps {
            if !rc.early.is_empty() {
                if trace {
                    eprintln!("trs jit: off (early rules)");
                }
                return None;
            }
            for en in &rc.entries {
                for &node in &en.nodes {
                    let SchedNode::Sched(r) = node else { continue };
                    if rule_ord.contains_key(&(en.inst, r)) {
                        continue;
                    }
                    let module = self.module_of(en.inst);
                    let mir = self.mods[module].ir;
                    let Some(&ri) = self.mods[module].rules.get(&r) else {
                        if trace {
                            eprintln!("trs jit: off (method node in schedule)");
                        }
                        return None;
                    };
                    let rr = &self.d.modules[mir].rules[ri];
                    let cf_slot = alloc(&mut nslots, 1);
                    let wf_slot = alloc(&mut nslots, 1);
                    let by = cfwf_by_inst.entry(en.inst).or_default();
                    by.insert(rr.can_fire, cf_slot);
                    by.insert(rr.will_fire, wf_slot);
                    let eb = eager_by_inst.entry(en.inst).or_default();
                    for &e in &en.eager {
                        if eb.contains_key(&e) {
                            continue;
                        }
                        let Some(ed) =
                            self.d.modules[mir].defs.iter().find(|d| d.name == e)
                        else {
                            if trace {
                                eprintln!("trs jit: off (eager def unknown)");
                            }
                            return None;
                        };
                        let ew = ed.width.max(1);
                        let base = alloc(&mut nslots, words_for(ew));
                        eb.insert(e, (base, ew));
                    }
                    rule_ord.insert((en.inst, r), rules.len());
                    rules.push(RuleInfo {
                        inst: en.inst,
                        rule_idx: ri,
                        ordinal: rules.len(),
                        cf_slot,
                        wf_slot,
                        eager: en.eager.clone(),
                    });
                }
            }
        }

        // any Exec node must belong to a scheduled rule above; method
        // Exec nodes or multi-comp oddities fall back
        for rc in rcomps {
            for en in &rc.entries {
                for &node in &en.nodes {
                    let SchedNode::Exec(r) = node else { continue };
                    if !rule_ord.contains_key(&(en.inst, r)) {
                        if trace {
                            eprintln!("trs jit: off (exec without sched)");
                        }
                        return None;
                    }
                }
            }
        }

        // compile per instance, batching that instance's rules
        let mut by_inst: HashMap<usize, Vec<usize>> = HashMap::new();
        for (k, ri) in rules.iter().enumerate() {
            by_inst.entry(ri.inst).or_default().push(k);
        }
        let mut compiled: Vec<Option<CompiledRule>> = Vec::new();
        compiled.resize_with(rules.len(), || None);
        let mut engines = Vec::new();
        let mut inst_list: Vec<usize> = by_inst.keys().copied().collect();
        inst_list.sort_unstable();
        for inst in inst_list {
            let module = self.module_of(inst);
            let mir = self.mods[module].ir;
            // reset ports of this instance -> node slots
            let mut reset_slot = HashMap::new();
            if let InstKind::User { resets, .. } = &self.insts[inst].kind {
                for (port, node) in resets {
                    reset_slot.insert(*port, reset_node_slot[*node]);
                }
            }
            let env = PlanEnv {
                d: &self.d,
                mir,
                reg_slot: reg_slot_by_inst.get(&inst).cloned().unwrap_or_default(),
                reset_slot,
                cfwf_slot: cfwf_by_inst.get(&inst).cloned().unwrap_or_default(),
                eager_slot: eager_by_inst.get(&inst).cloned().unwrap_or_default(),
            };
            let idxs = &by_inst[&inst];
            let mut specs = Vec::new();
            for &k in idxs {
                let ri = &rules[k];
                let rr = &self.d.modules[mir].rules[ri.rule_idx];
                // inhibitors: earlier same-module MEs + cross-module
                let mut inhibit_slots = Vec::new();
                for other in &rr.me_inhibits {
                    let other_ri = self.mods[module].rules[other];
                    let other_cf = self.d.modules[mir].rules[other_ri].can_fire;
                    match cfwf_by_inst.get(&inst).and_then(|m| m.get(&other_cf)) {
                        Some(&s) => inhibit_slots.push(s),
                        None => {
                            if trace {
                                eprintln!("trs jit: off (unslotted ME inhibitor)");
                            }
                            return None;
                        }
                    }
                }
                for rc in rcomps {
                    if let Some(cs) = rc.cross.get(&(inst, rr.name)) {
                        for (oi, ocf) in cs {
                            match cfwf_by_inst.get(oi).and_then(|m| m.get(ocf)) {
                                Some(&s) => inhibit_slots.push(s),
                                None => {
                                    if trace {
                                        eprintln!(
                                            "trs jit: off (unslotted cross inhibitor)"
                                        );
                                    }
                                    return None;
                                }
                            }
                        }
                    }
                }
                specs.push(RuleSpec {
                    rule_idx: ri.rule_idx,
                    inhibit_slots,
                    cf_slot: ri.cf_slot,
                    wf_slot: ri.wf_slot,
                    eager: ri.eager.clone(),
                    label: format!("i{}_{}", inst, ri.ordinal),
                    token_base: (ri.ordinal as u64) << 16,
                });
            }
            match compile_rules(&env, &specs, jit_foreign_cb, jit_sigfpe_cb) {
                Ok((engine, mut fns)) => {
                    engines.push(engine);
                    for (&k, cr) in idxs.iter().zip(fns.drain(..)) {
                        compiled[k] = Some(cr);
                    }
                }
                Err(e) => {
                    if trace {
                        eprintln!("trs jit: off ({e})");
                    }
                    return None;
                }
            }
        }

        // token table + dispatch lists
        self.jit_tokens = rules
            .iter()
            .map(|ri| {
                let cr = compiled[ri.ordinal].as_ref().unwrap();
                (ri.inst, ri.rule_idx, cr.foreign_stmts.clone())
            })
            .collect();
        let comp_nodes: Vec<Option<Vec<JitNode>>> = rcomps
            .iter()
            .map(|rc| {
                let mut nodes = Vec::new();
                for en in &rc.entries {
                    for &node in &en.nodes {
                        let (r, is_sched) = match node {
                            SchedNode::Sched(r) => (r, true),
                            SchedNode::Exec(r) => (r, false),
                        };
                        let ord = rule_ord[&(en.inst, r)];
                        let cr = compiled[ord].as_ref().unwrap();
                        nodes.push(if is_sched {
                            JitNode::Sched(cr.sched)
                        } else {
                            JitNode::Exec(cr.exec)
                        });
                    }
                }
                Some(nodes)
            })
            .collect();

        // allocate + wire the arena
        let mut arena = vec![0u64; nslots as usize].into_boxed_slice();
        let arena_ptr = arena.as_mut_ptr();
        for (ci, slot) in attach {
            if let InstKind::Prim(p) = &mut self.insts[ci].kind {
                p.arena_attach(unsafe { arena_ptr.add(slot as usize) });
            }
        }
        for (node, &slot) in reset_node_slot.iter().enumerate() {
            unsafe { *arena_ptr.add(slot as usize) = (!self.rst_asserted[node]) as u64 };
        }
        self.jit_arena_ptr = arena_ptr;
        self.jit_reset_slots = reset_node_slot;
        if trace {
            eprintln!(
                "trs jit: on ({} rules, {} slots, {} compositions)",
                rules.len(),
                nslots,
                comp_nodes.len()
            );
        }
        Some(JitPlans { _arena: arena, arena_ptr, comp_nodes, _engines: engines })
    }
}
