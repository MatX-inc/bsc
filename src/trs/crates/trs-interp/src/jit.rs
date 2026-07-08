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
use std::sync::atomic::Ordering;
use std::sync::{Arc, OnceLock};

use trs_codegen::lower::{
    compile_execs, compile_scheds, trial_lower, CompiledExec, CompiledSched, FArgSpec,
    InstEnv, PlanEnv, RuleSpec, TOKEN_KIND_EXEC,
};
use prim::ArenaKind;

/// Zero-divisor trap for compiled Quot/Rem: raise SIGFPE like the
/// interpreter (Value::quot) and native division.
pub(crate) unsafe extern "C" fn jit_sigfpe_cb() {
    libc::raise(libc::SIGFPE);
}

/// Prim-method trampoline: unmarshal per the call-site table, invoke
/// the boxed prim through the interpreter, marshal the result back.
pub(crate) unsafe extern "C" fn jit_prim_cb(
    env: *mut core::ffi::c_void,
    token: u64,
    args: *const u64,
    out: *mut u64,
) {
    let interp = &mut *(env as *mut Interp);
    let ordinal = (token >> 17) as usize;
    let is_exec = token & TOKEN_KIND_EXEC != 0;
    let local = (token & 0xffff) as usize;
    let lz = interp.jit_shared.as_ref().expect("jit prim cb without plan").clone();
    let pc = if is_exec {
        &lz.cells[ordinal].get().expect("prim cb from uncompiled body").prim_calls[local]
    } else {
        &lz.scheds[ordinal].prim_calls[local]
    };
    let (inst, method, ret_width, is_action) =
        (pc.inst, pc.method, pc.ret_width, pc.is_action);
    let arg_widths = pc.arg_widths.clone();
    let mut argv = Vec::with_capacity(arg_widths.len());
    let mut off = 0usize;
    for &w in &arg_widths {
        let words = ((w as usize) + 63) / 64;
        let limbs =
            std::slice::from_raw_parts(args.add(off), words.max(1)).to_vec();
        argv.push(Value::from_limbs64(w.max(1), limbs));
        off += words;
    }
    if is_action {
        interp.call_action(inst, method, &argv);
    } else {
        let v = interp.call_value(inst, method, &argv, ret_width);
        let words = ((ret_width.max(1) as usize) + 63) / 64;
        let dst = std::slice::from_raw_parts_mut(out, words);
        for (i, d) in dst.iter_mut().enumerate() {
            *d = v.limbs64().get(i).copied().unwrap_or(0);
        }
    }
}

/// One dispatch step of a compiled composition, in entries order (rule
/// ordinals resolved through LazyJit at dispatch time).
pub(crate) enum JitNode {
    Sched(u32),
    Exec(u32),
}

/// Shared compilation state: eligibility was proven by a synchronous
/// trial lowering at prime(); SCHED functions compile eagerly (they
/// run on every edge — sudoku's scheds are 4% of the IR thanks to
/// eager-slot cone sharing), and EXEC bodies fill per-rule cells on
/// background workers.  An Exec node whose cell is cold interprets the
/// body over the same arena-backed state (the interpreter's Def
/// evaluation falls through to the arena slots), so no global mode
/// flip exists.
pub(crate) struct LazyJit {
    /// owned snapshot the compile threads read (decouples lifetimes
    /// from the Interp)
    design: Design,
    insts: HashMap<usize, InstEnv>,
    specs: Vec<RuleSpec>,
    /// eagerly compiled sched fns, one per rule ordinal
    pub(crate) scheds: Vec<CompiledSched>,
    /// batch index counter for body workers
    next_batch: std::sync::atomic::AtomicUsize,
    batch_size: usize,
    /// bodies not yet compiled (0 = fully warm: dispatch skips the
    /// latch/bridge machinery entirely)
    cold: std::sync::atomic::AtomicUsize,
    cells: Vec<OnceLock<CompiledExec>>,
}

impl LazyJit {
    pub(crate) fn exec(&self, ord: usize) -> Option<&CompiledExec> {
        self.cells[ord].get()
    }

    pub(crate) fn any_cold(&self) -> bool {
        self.cold.load(Ordering::Acquire) != 0
    }

    /// Worker loop: claim body batches, compile, fill cells.
    fn work(&self) {
        loop {
            let b = self.next_batch.fetch_add(1, Ordering::AcqRel);
            let lo = b * self.batch_size;
            if lo >= self.specs.len() {
                return;
            }
            let hi = (lo + self.batch_size).min(self.specs.len());
            let env = PlanEnv { d: &self.design, insts: &self.insts };
            let compiled = compile_execs(
                &env,
                &self.specs[lo..hi],
                jit_foreign_cb,
                jit_sigfpe_cb,
                jit_prim_cb,
            )
            .unwrap_or_else(|e| {
                // trial_lower proved eligibility at prime; only an
                // LLVM-level failure can land here
                panic!("trs jit: compile of proven-eligible bodies failed: {e}")
            });
            for (k, cr) in compiled.into_iter().enumerate() {
                let _ = self.cells[lo + k].set(cr);
            }
            self.cold.fetch_sub(hi - lo, Ordering::AcqRel);
        }
    }
}

/// Compiled state carried by the Stepper.
pub(crate) struct JitPlans {
    /// the shared state arena; register prims and Interp::jit_arena_ptr
    /// hold raw pointers into this allocation (heap address is stable)
    _arena: Box<[u64]>,
    arena_ptr: *mut u64,
    /// per-composition dispatch lists (parallel to Stepper::rcomps)
    pub(crate) comp_nodes: Vec<Option<Vec<JitNode>>>,
    /// EN slots to zero before dispatching a composition (the C++
    /// schedule zeroes every enable at the top of the pass)
    pub(crate) en_slots: Vec<u32>,
    /// lazy compile cells (also reachable from Interp::jit_shared for
    /// the callbacks)
    pub(crate) lazy: Arc<LazyJit>,
    /// rule ordinal -> (instance, rule name, WF slot) for the
    /// interpreted-body fallback while its cell is cold
    pub(crate) exec_fallback: Vec<(usize, StrId, u32)>,
}

impl JitPlans {
    pub(crate) fn arena_ptr(&self) -> *mut u64 {
        self.arena_ptr
    }
}

/// The callback compiled code uses for foreign statements: rebuild
/// the Arg list from the call-site spec (numeric args arrive as word
/// runs, strings ride the table), dispatch through the interpreter's
/// foreign machinery, and marshal a task's result back.  Returns
/// nonzero when $finish was called.
pub(crate) unsafe extern "C" fn jit_foreign_cb(
    env: *mut core::ffi::c_void,
    token: u64,
    args: *const u64,
    out: *mut u64,
) -> i32 {
    let interp = &mut *(env as *mut Interp);
    let ordinal = (token >> 17) as usize;
    let is_exec = token & TOKEN_KIND_EXEC != 0;
    let local = (token & 0xffff) as usize;
    let lz = interp.jit_shared.as_ref().expect("jit foreign cb without plan").clone();
    let fs = if is_exec {
        &lz.cells[ordinal].get().expect("foreign cb from uncompiled body").foreign_stmts
            [local]
    } else {
        &lz.scheds[ordinal].foreign_stmts[local]
    };
    let (inst, func, ret_width) = (fs.inst, fs.func, fs.ret_width);
    let mut argv = Vec::with_capacity(fs.args.len());
    let mut off = 0usize;
    for a in &fs.args {
        match *a {
            FArgSpec::Str(sid) => {
                argv.push(Arg::Str(interp.s(sid).to_string()));
            }
            FArgSpec::Num { width, signed } => {
                let w = width;
                let words = ((w.max(1) as usize) + 63) / 64;
                let limbs = std::slice::from_raw_parts(args.add(off), words).to_vec();
                argv.push(Arg::Val(Value::from_limbs64(w.max(1), limbs), signed));
                off += words;
            }
        }
    }
    let fname = interp.s(func).to_string();
    let loc = interp.loc_of(inst);
    if ret_width == 0 {
        interp.foreign_action(&fname, &argv, &loc);
    } else {
        let v = interp.foreign_value(&fname, &argv, ret_width, &loc);
        let words = ((ret_width.max(1) as usize) + 63) / 64;
        let dst = std::slice::from_raw_parts_mut(out, words);
        for (i, d) in dst.iter_mut().enumerate() {
            *d = v.limbs64().get(i).copied().unwrap_or(0);
        }
    }
    interp.finished.is_some() as i32
}

impl Interp {
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
        let alloc = |n: &mut u32, words: u32| {
            let s = *n;
            *n += words;
            s
        };

        // per-instance environments: children, prim slots, resets, ENs
        let mut inst_envs: HashMap<usize, InstEnv> = HashMap::new();
        let mut attach: Vec<(usize, u32)> = Vec::new(); // (prim inst, base)
        let reset_node_slot: Vec<u32> =
            (0..self.rst_asserted.len()).map(|_| alloc(&mut nslots, 1)).collect();
        for i in 0..self.insts.len() {
            let InstKind::User { module, children, resets, .. } = &self.insts[i].kind
            else {
                continue;
            };
            let module = *module;
            let mir = self.mods[module].ir;
            let children: HashMap<StrId, usize> =
                children.iter().map(|(k, v)| (*k, *v)).collect();
            let mut reg_slot = HashMap::new();
            let mut wire_slot = HashMap::new();
            for (&name, &ci) in &children {
                let InstKind::Prim(p) = &self.insts[ci].kind else { continue };
                match p.arena_kind() {
                    Some(ArenaKind::Reg { width }) => {
                        let base = alloc(&mut nslots, width.div_ceil(64).max(1));
                        reg_slot.insert(name, (base, width));
                        attach.push((ci, base));
                    }
                    Some(ArenaKind::Wire { width }) => {
                        let base = alloc(&mut nslots, 1 + width.max(1).div_ceil(64));
                        wire_slot.insert(name, (base, width));
                        attach.push((ci, base));
                    }
                    None => {}
                }
            }
            let reset_slot: HashMap<StrId, u32> = resets
                .iter()
                .map(|(port, node)| (*port, reset_node_slot[*node]))
                .collect();
            // every EN_* port gets a slot (zeroed per dispatch, stored
            // by compiled call sites; method WF cones read them)
            let mut en_slot = HashMap::new();
            for (&pname, &(_w, kind)) in &self.mods[module].ports {
                if kind == ir::PortKind::MethodEnable {
                    en_slot.insert(pname, alloc(&mut nslots, 1));
                }
            }
            inst_envs.insert(
                i,
                InstEnv {
                    mir,
                    children,
                    reg_slot,
                    wire_slot,
                    reset_slot,
                    en_slot,
                    cfwf_slot: HashMap::new(),
                    eager_slot: HashMap::new(),
                },
            );
        }

        // CF/WF and eager-def slots for every scheduled rule
        struct RuleInfo {
            inst: usize,
            rule_idx: usize,
            ordinal: usize,
            cf_slot: u32,
            wf_slot: u32,
            eager: Vec<StrId>,
            shared: Vec<StrId>,
        }
        let mut rules: Vec<RuleInfo> = Vec::new();
        let mut rule_ord: HashMap<(usize, StrId), usize> = HashMap::new();
        for rc in rcomps {
            if !rc.early.is_empty() {
                if trace {
                    eprintln!("trs jit: off (early rules)");
                }
                return None;
            }
            // eager defs owned by entries already walked in THIS comp,
            // per instance: later rules of the same instance may load
            // their slots instead of re-expanding the cone
            let mut owned_so_far: HashMap<usize, Vec<StrId>> = HashMap::new();
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
                    let (can_fire, will_fire) = (rr.can_fire, rr.will_fire);
                    let mut eager_adds: Vec<(StrId, u32, u32)> = Vec::new();
                    {
                        let ie = inst_envs.get(&en.inst)?;
                        for &e in &en.eager {
                            if ie.eager_slot.contains_key(&e)
                                || eager_adds.iter().any(|(n, _, _)| *n == e)
                            {
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
                            let base = alloc(&mut nslots, ew.div_ceil(64));
                            eager_adds.push((e, base, ew));
                        }
                    }
                    let ie = inst_envs.get_mut(&en.inst)?;
                    ie.cfwf_slot.insert(can_fire, cf_slot);
                    ie.cfwf_slot.insert(will_fire, wf_slot);
                    for (e, base, ew) in eager_adds {
                        ie.eager_slot.insert(e, (base, ew));
                    }
                    let shared =
                        owned_so_far.get(&en.inst).cloned().unwrap_or_default();
                    owned_so_far
                        .entry(en.inst)
                        .or_default()
                        .extend(en.eager.iter().copied());
                    rule_ord.insert((en.inst, r), rules.len());
                    rules.push(RuleInfo {
                        inst: en.inst,
                        rule_idx: ri,
                        ordinal: rules.len(),
                        cf_slot,
                        wf_slot,
                        eager: en.eager.clone(),
                        shared,
                    });
                }
            }
        }

        // any Exec node must belong to a scheduled rule above
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

        // one design-wide spec list (ordinal order)
        let mut specs = Vec::new();
        for ri in &rules {
            let mir = inst_envs[&ri.inst].mir;
            let rr = &self.d.modules[mir].rules[ri.rule_idx];
            let module = self.module_of(ri.inst);
            let mut inhibit_slots = Vec::new();
            for other in &rr.me_inhibits {
                let other_ri = self.mods[module].rules[other];
                let other_cf = self.d.modules[mir].rules[other_ri].can_fire;
                match inst_envs[&ri.inst].cfwf_slot.get(&other_cf) {
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
                if let Some(cs) = rc.cross.get(&(ri.inst, rr.name)) {
                    for (oi, ocf) in cs {
                        match inst_envs.get(oi).and_then(|e| e.cfwf_slot.get(ocf)) {
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
                inst: ri.inst,
                rule_idx: ri.rule_idx,
                inhibit_slots,
                cf_slot: ri.cf_slot,
                wf_slot: ri.wf_slot,
                eager: ri.eager.clone(),
                shared: ri.shared.clone(),
                label: format!("i{}_{}", ri.inst, ri.ordinal),
                token_base: (ri.ordinal as u64) << 17,
            });
        }
        // eligibility decided NOW, synchronously, by lowering into a
        // throwaway context (~ms/rule, no LLVM codegen); the expensive
        // engine work is deferred to per-rule cells
        {
            let env = PlanEnv { d: &self.d, insts: &inst_envs };
            let t0 = std::time::Instant::now();
            if let Err(e) = trial_lower(&env, &specs) {
                if trace {
                    eprintln!("trs jit: off ({e})");
                }
                return None;
            }
            if std::env::var_os("TRS_JIT_TIME").is_some() {
                eprintln!("trs jit: trial lower {:?}", t0.elapsed());
            }
        }

        let n = specs.len();
        let nworkers = std::env::var("TRS_JIT_THREADS")
            .ok()
            .and_then(|v| v.parse::<usize>().ok())
            .unwrap_or_else(|| {
                std::thread::available_parallelism().map(|x| x.get()).unwrap_or(8)
            })
            .clamp(1, 64)
            .min(n.max(1));

        // SCHED functions compile eagerly (blocking, parallel): they
        // run on every edge and the cone-sharing keeps them small
        trs_codegen::lower::llvm_init_once();
        let t0 = std::time::Instant::now();
        let chunk = n.div_ceil(nworkers).max(1);
        let sched_results: Vec<_> = std::thread::scope(|sc| {
            let d = &self.d;
            let ie = &inst_envs;
            specs
                .chunks(chunk)
                .map(|c| {
                    sc.spawn(move || {
                        let env = PlanEnv { d, insts: ie };
                        compile_scheds(&env, c, jit_foreign_cb, jit_sigfpe_cb, jit_prim_cb)
                    })
                })
                .collect::<Vec<_>>()
                .into_iter()
                .map(|h| h.join().expect("sched compile thread"))
                .collect()
        });
        let mut scheds = Vec::with_capacity(n);
        for r in sched_results {
            match r {
                Ok(mut v) => scheds.append(&mut v),
                Err(e) => {
                    if trace {
                        eprintln!("trs jit: off (sched compile: {e})");
                    }
                    return None;
                }
            }
        }
        if std::env::var_os("TRS_JIT_TIME").is_some() {
            eprintln!("trs jit: sched compile {:?}", t0.elapsed());
        }

        let lazy = Arc::new(LazyJit {
            design: self.d.clone(),
            insts: inst_envs,
            specs,
            scheds,
            next_batch: std::sync::atomic::AtomicUsize::new(0),
            batch_size: chunk,
            cold: std::sync::atomic::AtomicUsize::new(n),
            cells: (0..n).map(|_| OnceLock::new()).collect(),
        });
        self.jit_shared = Some(lazy.clone());

        // bodies compile in the background; cold bodies interpret
        for _ in 0..nworkers {
            let lz = lazy.clone();
            std::thread::spawn(move || lz.work());
        }
        if std::env::var_os("TRS_JIT_SYNC").is_some() {
            let t0 = std::time::Instant::now();
            while (0..n).any(|i| lazy.cells[i].get().is_none()) {
                std::thread::yield_now();
            }
            if std::env::var_os("TRS_JIT_TIME").is_some() {
                eprintln!("trs jit: sync body compile {:?}", t0.elapsed());
            }
        }

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
                        let ord = rule_ord[&(en.inst, r)] as u32;
                        nodes.push(if is_sched {
                            JitNode::Sched(ord)
                        } else {
                            JitNode::Exec(ord)
                        });
                    }
                }
                Some(nodes)
            })
            .collect();
        let en_slots: Vec<u32> =
            lazy.insts.values().flat_map(|e| e.en_slot.values().copied()).collect();

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
        let exec_fallback: Vec<(usize, StrId, u32)> = {
            let mut v = Vec::with_capacity(rules.len());
            for ri in &rules {
                let mir = lazy.insts[&ri.inst].mir;
                v.push((ri.inst, self.d.modules[mir].rules[ri.rule_idx].name, ri.wf_slot));
            }
            v
        };
        // interpreted bodies resolve fire signals and schedule-position
        // defs straight from the arena (same values the native scheds
        // stored; matches the proven full-interpreter eager semantics)
        self.jit_eager_slots = lazy
            .insts
            .iter()
            .flat_map(|(&i, e)| {
                e.cfwf_slot
                    .iter()
                    .map(move |(&d, &s)| ((i, d), (s, 1u32)))
                    .chain(e.eager_slot.iter().map(move |(&d, &(b, w))| ((i, d), (b, w))))
            })
            .collect();
        // interp method calls during body fallback must write EN slots
        // through so native scheds see them
        self.jit_en_slots = lazy
            .insts
            .iter()
            .flat_map(|(&i, e)| e.en_slot.iter().map(move |(&p, &s)| ((i, p), s)))
            .collect();
        Some(JitPlans {
            _arena: arena,
            arena_ptr,
            comp_nodes,
            en_slots,
            lazy,
            exec_fallback,
        })
    }
}
