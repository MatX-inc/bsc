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
    compile_execs, compile_helpers, compile_helpers_object, compile_scheds, decode_protos,
    encode_protos, trial_lower,
    CompiledExec, CompiledSched, FArgSpec, FnProtos, ForeignCb, HelperMap, HelperRef,
    HelperSpec, InstEnv, PlanEnv, PrimCb, RuleSpec, SigfpeCb, AOT_LAYOUT_REV,
    TOKEN_KIND_EXEC,
};
use prim::ArenaKind;

/// TRS_PROF=1: cheap wall-time accounting of where a JIT/AOT run
/// spends its time (trampoline vs dispatch vs ticks).  Off = one
/// cached-bool branch per site.
pub(crate) mod prof {
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::sync::OnceLock;
    pub static PRIM_NS: AtomicU64 = AtomicU64::new(0);
    pub static PRIM_CALLS: AtomicU64 = AtomicU64::new(0);
    pub static FOREIGN_NS: AtomicU64 = AtomicU64::new(0);
    pub static FOREIGN_CALLS: AtomicU64 = AtomicU64::new(0);
    pub static DISPATCH_NS: AtomicU64 = AtomicU64::new(0);
    pub static TICK_NS: AtomicU64 = AtomicU64::new(0);
    /// per prim-method call counts (TRS_PROF=1)
    pub static PRIM_HIST: std::sync::Mutex<
        Option<std::collections::HashMap<String, u64>>,
    > = std::sync::Mutex::new(None);
    pub fn on() -> bool {
        static P: OnceLock<bool> = OnceLock::new();
        *P.get_or_init(|| std::env::var_os("TRS_PROF").is_some())
    }
    pub fn add(cell: &AtomicU64, t0: std::time::Instant) {
        cell.fetch_add(t0.elapsed().as_nanos() as u64, Ordering::Relaxed);
    }
    pub fn dump(total: std::time::Duration) {
        if let Some(h) = PRIM_HIST.lock().unwrap().as_ref() {
            let mut v: Vec<_> = h.iter().collect();
            v.sort_by_key(|(_, &n)| std::cmp::Reverse(n));
            for (meth, n) in v.into_iter().take(12) {
                eprintln!("trs prof:   {n:>9}  .{meth}");
            }
        }
        let g = |c: &AtomicU64| c.load(Ordering::Relaxed);
        eprintln!(
            "trs prof: total {:.3}s | dispatch {:.3}s | ticks {:.3}s | \
             prim cb {:.3}s ({} calls) | foreign cb {:.3}s ({} calls)",
            total.as_secs_f64(),
            g(&DISPATCH_NS) as f64 / 1e9,
            g(&TICK_NS) as f64 / 1e9,
            g(&PRIM_NS) as f64 / 1e9,
            g(&PRIM_CALLS),
            g(&FOREIGN_NS) as f64 / 1e9,
            g(&FOREIGN_CALLS),
        );
    }
}

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
    let _t0 = prof::on().then(std::time::Instant::now);
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
    if let Some(t0) = _t0 {
        prof::add(&prof::PRIM_NS, t0);
        prof::PRIM_CALLS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let meth = interp.s(method).to_string();
        prof::PRIM_HIST
            .lock()
            .unwrap()
            .get_or_insert_with(Default::default)
            .entry(meth)
            .and_modify(|n| *n += 1)
            .or_insert(1);
    }
}

/// One dispatch step of a compiled composition, in entries order (rule
/// ordinals resolved through LazyJit at dispatch time).
pub(crate) enum JitNode {
    Sched(u32),
    Exec(u32),
}

/// What prime()'s planning pass should do with the compiled form:
/// JIT in-process (default), emit a persistent artifact .so (trs
/// link), or load one (trs run --code).
#[derive(Default)]
pub(crate) enum JitRequest {
    #[default]
    Run,
    Emit {
        so: std::path::PathBuf,
    },
    Load {
        so: std::path::PathBuf,
    },
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
    now_slot: u32,
    /// per-ordinal exec args: (region base index, token base) — the
    /// compiled body is shared across instances of a module type
    pub(crate) exec_args: Vec<(u64, u64)>,
    /// per-ordinal call-site tables (from trial_lower; per-ordinal even
    /// when the compiled body is shared, because prim targets differ)
    protos: Vec<FnProtos>,
    /// exec dedup classes: (representative ordinal, member ordinals)
    classes: Vec<(usize, Vec<usize>)>,
    /// outlined def-piece helpers (baked addresses; shared JIT/AOT
    /// lowering — AOT uses symbol refs at emit time instead)
    helpers: Arc<HelperMap>,
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

    /// Worker loop: claim CLASS batches, compile one representative
    /// per class, fill every member's cell with the shared body and
    /// its own call-site tables.
    fn work(&self) {
        loop {
            let b = self.next_batch.fetch_add(1, Ordering::AcqRel);
            let lo = b * self.batch_size;
            if lo >= self.classes.len() {
                return;
            }
            let hi = (lo + self.batch_size).min(self.classes.len());
            let env = PlanEnv {
                d: &self.design,
                insts: &self.insts,
                now_slot: self.now_slot,
            };
            let reps: Vec<RuleSpec> =
                (lo..hi).map(|c| self.specs[self.classes[c].0].clone()).collect();
            let compiled = compile_execs(
                &env,
                &reps,
                Some(&self.helpers),
                jit_foreign_cb,
                jit_sigfpe_cb,
                jit_prim_cb,
            )
            .unwrap_or_else(|e| {
                // trial_lower proved eligibility at prime; only an
                // LLVM-level failure can land here
                panic!("trs jit: compile of proven-eligible bodies failed: {e}")
            });
            for (c, cr) in (lo..hi).zip(compiled) {
                for &m in &self.classes[c].1 {
                    let _ = self.cells[m].set(CompiledExec {
                        exec: cr.exec,
                        foreign_stmts: self.protos[m].exec_foreign.clone(),
                        prim_calls: self.protos[m].exec_prims.clone(),
                    });
                }
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
    /// slot stamped with the current instant at every edge
    pub(crate) now_slot: u32,
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
    let _t0 = prof::on().then(std::time::Instant::now);
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
    if let Some(t0) = _t0 {
        prof::add(&prof::FOREIGN_NS, t0);
        prof::FOREIGN_CALLS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    }
    interp.finished.is_some() as i32
}

/// Body-splitting cone analysis: child classification for one module
/// type (uniform across its instances).
#[derive(Clone, Copy, PartialEq)]
pub(crate) enum ChildClass {
    Reg,
    CfgReg,
    Wire,
    Fifo,
    Other,
}

/// Child resolution for the cone analyzer: an arena-backed prim, a
/// user submodule (recurse into its method cones), or opaque.
pub(crate) enum ChildRef {
    Prim(ChildClass),
    User(usize),
    Opaque,
}

/// Per-def piece statistics (see select_outlined).
#[derive(Clone)]
pub(crate) struct PieceInfo {
    pub eff: u32,
    pub outlinable: bool,
    pub stable: bool,
    pub outlined: bool,
    /// unbound data-port reads in the piece: helper parameters (v2);
    /// nonempty => no per-instant memo (value is per-call)
    pub ports: Vec<StrId>,
}

/// Bottom-up outline selection over a module's def DAG, recursing
/// through user-child value-method cones (callee Method.result/ready;
/// callee Port reads must be bound method args).  eff counts each
/// transitive non-outlined def once (SSA-memoized lowering cost);
/// outlined children are unit-cost calls.
pub(crate) struct ConeAnalyzer<'a> {
    pub d: &'a Design,
    /// (mir, child instance name) -> resolution via exemplar instances
    pub kind: &'a dyn Fn(usize, StrId) -> ChildRef,
    pub thresh: u32,
    memo: HashMap<(usize, StrId), PieceInfo>,
    reach: HashMap<(usize, StrId), std::collections::HashSet<(usize, StrId)>>,
    own: HashMap<(usize, StrId), u32>,
    seen: Vec<(usize, StrId)>,
}

impl<'a> ConeAnalyzer<'a> {
    pub fn new(d: &'a Design, kind: &'a dyn Fn(usize, StrId) -> ChildRef, thresh: u32) -> Self {
        ConeAnalyzer { d, kind, thresh, memo: HashMap::new(), reach: HashMap::new(), own: HashMap::new(), seen: Vec::new() }
    }

    pub fn module(&mut self, mir: usize) -> HashMap<StrId, PieceInfo> {
        let names: Vec<StrId> = self.d.modules[mir].defs.iter().map(|dd| dd.name).collect();
        names.iter().map(|&n| (n, self.def_piece(mir, n))).collect()
    }

    fn def_piece(&mut self, mir: usize, n: StrId) -> PieceInfo {
        if let Some(r) = self.memo.get(&(mir, n)) {
            return r.clone();
        }
        if self.seen.contains(&(mir, n)) {
            return PieceInfo { eff: 0, outlinable: false, stable: false, outlined: false, ports: Vec::new() };
        }
        let Some(di) = self.d.modules[mir].defs.iter().position(|dd| dd.name == n) else {
            return PieceInfo { eff: 0, outlinable: false, stable: false, outlined: false, ports: Vec::new() };
        };
        self.seen.push((mir, n));
        let e = self.d.modules[mir].defs[di].expr.clone();
        let mut rs = std::collections::HashSet::new();
        let mut ports = std::collections::BTreeSet::new();
        let (nodes, outl, stab) = self.walk(mir, &e, None, &mut rs, &mut ports);
        self.seen.pop();
        let mut eff = nodes;
        for k in &rs {
            eff = eff.saturating_add(*self.own.get(k).unwrap_or(&0));
        }
        self.own.insert((mir, n), nodes);
        // cap the parameter count: huge signatures cost more than the
        // split saves
        let outl = outl && ports.len() <= 8;
        let outlined = outl && eff >= self.thresh;
        if !outlined {
            rs.insert((mir, n));
            self.reach.insert((mir, n), rs);
        }
        let r = PieceInfo {
            eff,
            outlinable: outl,
            stable: stab,
            outlined,
            ports: ports.into_iter().collect(),
        };
        self.memo.insert((mir, n), r.clone());
        r
    }

    /// returns (own nodes, outlinable, stable); accumulates reached
    /// non-outlined defs into rs
    fn walk(
        &mut self,
        mir: usize,
        e: &trs_ir::Expr,
        margs: Option<&std::collections::HashSet<StrId>>,
        rs: &mut std::collections::HashSet<(usize, StrId)>,
        ports: &mut std::collections::BTreeSet<StrId>,
    ) -> (u32, bool, bool) {
        use trs_ir::Expr as E;
        let (mut nodes, mut outl, mut stab) = (1u32, true, true);
        macro_rules! sub {
            ($x:expr) => {{
                let (c, o, sb) = self.walk(mir, $x, margs, rs, ports);
                nodes = nodes.saturating_add(c);
                outl &= o;
                stab &= sb;
            }};
        }
        match e {
            E::Const { .. } | E::Str(_) | E::Real(_) => {}
            E::Port(pn) => {
                if margs.map(|a| a.contains(pn)).unwrap_or(false) {
                    // bound method arg: accounted at the call site
                } else {
                    let m = &self.d.modules[mir];
                    let is_en = m.inputs.iter().any(|q| {
                        q.name == *pn && q.kind == trs_ir::PortKind::MethodEnable
                    });
                    // data ports live in Module.inputs; METHOD ARG
                    // ports live in Method.args — both parameterize
                    let is_data = m.inputs.iter().any(|q| {
                        q.name == *pn && q.kind != trs_ir::PortKind::MethodEnable
                    }) || m
                        .methods
                        .iter()
                        .any(|me| me.args.iter().any(|q| q.name == *pn));
                    let is_reset = m
                        .resets
                        .iter()
                        .any(|_| false) // reset PORT names resolve via InstEnv; conservative below
                        ;
                    let _ = is_reset;
                    if is_en {
                        // EN slots change during dispatch
                        stab = false;
                    } else if is_data {
                        // data/method-arg port: helper parameter (v2)
                        ports.insert(*pn);
                        stab = false;
                    } else {
                        // unknown port kind (reset wires etc.): the
                        // lowering may not have a binding — taint
                        outl = false;
                        stab = false;
                    }
                }
            }
            E::Def(dn) => {
                let r = self.def_piece(mir, *dn);
                outl &= r.outlinable || r.outlined;
                stab &= r.stable;
                // a piece's port params propagate to its callers
                // (outlined callees receive them as call arguments)
                ports.extend(r.ports.iter().copied());
                if r.outlined {
                    nodes = nodes.saturating_add(1);
                } else {
                    if let Some(rr) = self.reach.get(&(mir, *dn)) {
                        rs.extend(rr.iter().cloned());
                    }
                    outl &= r.outlinable;
                }
            }
            E::MethCall { instance, method, args, .. } => {
                for a in args {
                    sub!(a);
                }
                let mname = self.d.strings[*method as usize].clone();
                match (self.kind)(mir, *instance) {
                    ChildRef::Prim(c) => {
                        let (ok, st) = match c {
                            ChildClass::Reg | ChildClass::CfgReg => {
                                (matches!(mname.as_str(), "read" | "get" | "_read"), true)
                            }
                            ChildClass::Wire => {
                                // schedule certification pending: not stable
                                (matches!(mname.as_str(), "whas" | "wget"), false)
                            }
                            ChildClass::Fifo => match mname.as_str() {
                                "i_notFull" | "i_notEmpty" => (true, true),
                                "first" | "notFull" | "notEmpty" => (true, false),
                                _ => (false, false),
                            },
                            ChildClass::Other => (false, false),
                        };
                        outl &= ok;
                        stab &= st;
                    }
                    ChildRef::User(cmir) => {
                        let mm = self.d.modules[cmir]
                            .methods
                            .iter()
                            .find(|m| m.name == *method);
                        match mm {
                            Some(m) if m.body.is_empty() && m.result.is_some() => {
                                let aset: std::collections::HashSet<StrId> =
                                    m.args.iter().map(|p| p.name).collect();
                                let res = m.result.clone().unwrap();
                                // callee ports beyond its bound args
                                // are the CALLEE module's — v1 cannot
                                // parameterize across modules: taint
                                let mut cports = Default::default();
                                let (c, o, sb) =
                                    self.walk(cmir, &res, Some(&aset), rs, &mut cports);
                                nodes = nodes.saturating_add(c);
                                outl &= o && cports.is_empty();
                                stab &= sb;
                            }
                            Some(m) => {
                                if std::env::var_os("TRS_JIT_SPLIT_WHY").is_some() {
                                    eprintln!("why: method-with-body args={} res={}", m.body.len(), m.result.is_some());
                                }
                                outl = false;
                                stab = false;
                            }
                            None => {
                                if std::env::var_os("TRS_JIT_SPLIT_WHY").is_some() {
                                    eprintln!("why: method-not-found {}", self.d.strings[*method as usize]);
                                }
                                outl = false;
                                stab = false;
                            }
                        }
                    }
                    ChildRef::Opaque => {
                        if std::env::var_os("TRS_JIT_SPLIT_WHY").is_some() {
                            eprintln!("why: opaque-child {}", self.d.strings[*instance as usize]);
                        }
                        outl = false;
                        stab = false;
                    }
                }
            }
            E::Prim { args, .. } => {
                for a in args {
                    sub!(a);
                }
            }
            E::If { cond, then_, else_, .. } => {
                sub!(cond);
                sub!(then_);
                sub!(else_);
            }
            E::Case { scrutinee, arms, default, .. } => {
                sub!(scrutinee);
                for (_, a) in arms {
                    sub!(a);
                }
                sub!(default);
            }
            other => {
                if std::env::var_os("TRS_JIT_SPLIT_WHY").is_some() {
                    eprintln!("why: expr-kind {:?}", std::mem::discriminant(other));
                }
                outl = false;
                stab = false;
            }
        }
        (nodes, outl, stab)
    }
}

/// Eager parallel sched compile (in-process JIT path).
fn aot_or_jit_scheds(
    interp: &Interp,
    inst_envs: &HashMap<usize, InstEnv>,
    specs: &[RuleSpec],
    now_slot: u32,
    helpers: Option<&HelperMap>,
    nworkers: usize,
    trace: bool,
) -> Option<Vec<CompiledSched>> {
    trs_codegen::lower::llvm_init_once();
    let t0 = std::time::Instant::now();
    let n = specs.len();
    let chunk = n.div_ceil(nworkers).max(1);
    let sched_results: Vec<_> = std::thread::scope(|sc| {
        let d = &interp.d;
        specs
            .chunks(chunk)
            .map(|c| {
                sc.spawn(move || {
                    let env = PlanEnv { d, insts: inst_envs, now_slot };
                    compile_scheds(&env, c, helpers, jit_foreign_cb, jit_sigfpe_cb, jit_prim_cb)
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
    Some(scheds)
}

/// trs link: compile every rule (sched + exec) into PIC objects in
/// parallel, add the fingerprint object, and cc -shared them into the
/// artifact .so.
#[allow(clippy::too_many_arguments)]
#[allow(clippy::too_many_arguments)]
fn aot_emit(
    d: &Design,
    inst_envs: &HashMap<usize, InstEnv>,
    specs: &[RuleSpec],
    now_slot: u32,
    classes: &[(usize, Vec<usize>)],
    helper_specs: &[HelperSpec],
    refs_sym: &HelperMap,
    split_thresh: u32,
    protos: &[FnProtos],
    so: &std::path::Path,
    bir_hash: u64,
) -> Result<(), String> {
    use trs_codegen::lower::{compile_meta_object, compile_object_chunk};
    trs_codegen::lower::llvm_init_once();
    let t0 = std::time::Instant::now();
    let nworkers = jit_workers(specs.len());
    let chunk = specs.len().div_ceil(nworkers).max(1);
    // sched functions per ordinal; exec bodies once per dedup class
    let reps: Vec<RuleSpec> =
        classes.iter().map(|(rep, _)| specs[*rep].clone()).collect();
    let rchunk = reps.len().div_ceil(nworkers).max(1);
    // helpers are best-effort in AOT exactly as in JIT: if their
    // object fails to compile, drop them and link the design unsplit
    // rather than failing the artifact
    let mut helpers_on = !helper_specs.is_empty();
    let mut helper_obj: Option<Vec<u8>> = None;
    if helpers_on {
        let _g = trs_codegen::lower::AotModeGuard::set();
        let env = PlanEnv { d, insts: inst_envs, now_slot };
        let pseudo = specs[0].clone();
        match compile_helpers_object(&env, helper_specs, refs_sym, &pseudo) {
            Ok(o) => helper_obj = Some(o),
            Err(e) => {
                eprintln!(
                    "trs link: note: split helpers disabled for this design ({e})"
                );
                helpers_on = false;
            }
        }
    }
    let objs: Vec<Result<Vec<u8>, _>> = std::thread::scope(|sc| {
        let mut handles = Vec::new();
        for c in specs.chunks(chunk) {
            handles.push(sc.spawn(move || {
                let _g = trs_codegen::lower::AotModeGuard::set();
                let env = PlanEnv { d, insts: inst_envs, now_slot };
                compile_object_chunk(&env, c, helpers_on.then_some(refs_sym), true, false)
            }));
        }
        for c in reps.chunks(rchunk) {
            handles.push(sc.spawn(move || {
                let _g = trs_codegen::lower::AotModeGuard::set();
                let env = PlanEnv { d, insts: inst_envs, now_slot };
                compile_object_chunk(&env, c, helpers_on.then_some(refs_sym), false, true)
            }));
        }
        handles
            .into_iter()
            .map(|h| h.join().expect("aot compile thread"))
            .collect()
    });
    let helper_obj = helpers_on.then_some(helper_obj).flatten();
    let tmp = std::env::temp_dir().join(format!("trs-link-{}", std::process::id()));
    std::fs::create_dir_all(&tmp).map_err(|e| e.to_string())?;
    let mut files = Vec::new();
    for (i, o) in objs.into_iter().enumerate() {
        let bytes = o.map_err(|e| format!("object compile: {e}"))?;
        let f = tmp.join(format!("chunk{i}.o"));
        std::fs::write(&f, bytes).map_err(|e| e.to_string())?;
        files.push(f);
    }
    if let Some(o) = helper_obj {
        let f = tmp.join("helpers.o");
        std::fs::write(&f, o).map_err(|e| e.to_string())?;
        files.push(f);
    }
    let meta =
        compile_meta_object(bir_hash, split_thresh as u64, &encode_protos(protos))
            .map_err(|e| format!("meta object: {e}"))?;
    let mf = tmp.join("meta.o");
    std::fs::write(&mf, meta).map_err(|e| e.to_string())?;
    files.push(mf);
    let st = std::process::Command::new("cc")
        .args(["-shared", "-o"])
        .arg(so)
        .args(&files)
        .status()
        .map_err(|e| format!("cc: {e}"))?;
    std::fs::remove_dir_all(&tmp).ok();
    if !st.success() {
        return Err("cc -shared failed".into());
    }
    if std::env::var_os("TRS_JIT_TIME").is_some() {
        eprintln!("trs aot: emit + link {:?}", t0.elapsed());
    }
    Ok(())
}

/// trs run --code: dlopen the artifact, verify its fingerprint, fill
/// the callback pointer-globals, and resolve every rule's sched/exec
/// function.  Any failure falls back to in-process compilation.
fn aot_load(
    so: &std::path::Path,
    bir_hash: u64,
    specs: &[RuleSpec],
    classes: &[(usize, Vec<usize>)],
    split_thresh: u32,
) -> Result<(Vec<CompiledSched>, Vec<CompiledExec>, Vec<FnProtos>), String> {
    unsafe {
        let lib = libloading::Library::new(so).map_err(|e| e.to_string())?;
        let h: libloading::Symbol<*const u64> =
            lib.get(b"trs_bir_hash").map_err(|e| e.to_string())?;
        if **h != bir_hash {
            return Err("BIR fingerprint mismatch (stale artifact)".into());
        }
        let r: libloading::Symbol<*const u64> =
            lib.get(b"trs_layout_rev").map_err(|e| e.to_string())?;
        if **r != AOT_LAYOUT_REV {
            return Err(format!(
                "layout revision {} (this trs expects {AOT_LAYOUT_REV})",
                **r
            ));
        }
        let t: libloading::Symbol<*const u64> =
            lib.get(b"trs_split_thresh").map_err(|e| e.to_string())?;
        if **t != split_thresh as u64 {
            return Err(format!(
                "split threshold {} but this run plans with {split_thresh} \
                 (arena layouts differ)",
                **t
            ));
        }
        for (name, addr) in [
            (&b"trs_cb_foreign"[..], jit_foreign_cb as ForeignCb as usize),
            (&b"trs_cb_sigfpe"[..], jit_sigfpe_cb as SigfpeCb as usize),
            (&b"trs_cb_prim"[..], jit_prim_cb as PrimCb as usize),
        ] {
            let g: libloading::Symbol<*mut usize> =
                lib.get(name).map_err(|e| e.to_string())?;
            **g = addr;
        }
        let pl: libloading::Symbol<*const u64> =
            lib.get(b"trs_protos_len").map_err(|e| e.to_string())?;
        let pg: libloading::Symbol<*const u8> =
            lib.get(b"trs_protos").map_err(|e| e.to_string())?;
        let pbytes = std::slice::from_raw_parts(*pg, **pl as usize);
        let protos = decode_protos(pbytes)
            .ok_or("corrupt trs_protos table")?;
        if protos.len() != specs.len() {
            return Err("protos count mismatch".into());
        }
        let mut scheds = Vec::with_capacity(specs.len());
        for (spec, proto) in specs.iter().zip(protos.iter()) {
            let sf: libloading::Symbol<
                unsafe extern "C" fn(*mut u64, *mut core::ffi::c_void),
            > = lib
                .get(format!("sched_{}\0", spec.label).as_bytes())
                .map_err(|e| e.to_string())?;
            scheds.push(CompiledSched {
                sched: *sf,
                foreign_stmts: proto.sched_foreign.clone(),
                prim_calls: proto.sched_prims.clone(),
            });
        }
        // exec bodies: one symbol per dedup class, shared by members
        let mut execs: Vec<Option<CompiledExec>> =
            (0..specs.len()).map(|_| None).collect();
        for (rep, members) in classes {
            let ef: libloading::Symbol<
                unsafe extern "C" fn(*mut u64, *mut core::ffi::c_void, u64, u64) -> i32,
            > = lib
                .get(format!("exec_{}\0", specs[*rep].label).as_bytes())
                .map_err(|e| e.to_string())?;
            for &m in members {
                execs[m] = Some(CompiledExec {
                    exec: *ef,
                    foreign_stmts: protos[m].exec_foreign.clone(),
                    prim_calls: protos[m].exec_prims.clone(),
                });
            }
        }
        let execs: Vec<CompiledExec> = execs
            .into_iter()
            .map(|o| o.expect("every ordinal belongs to a class"))
            .collect();
        // the artifact stays mapped for the process lifetime
        std::mem::forget(lib);
        Ok((scheds, execs, protos))
    }
}

/// Worker-thread count for compile fan-out (TRS_JIT_THREADS caps).
fn jit_workers(n: usize) -> usize {
    std::env::var("TRS_JIT_THREADS")
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .unwrap_or_else(|| {
            std::thread::available_parallelism().map(|x| x.get()).unwrap_or(8)
        })
        .clamp(1, 64)
        .min(n.max(1))
}

impl Interp {
    /// Build the JIT plan for the resolved compositions, or None to run
    /// fully interpreted.  Called once from prime().
    pub(crate) fn jit_plan(&mut self, rcomps: &[RComp]) -> Option<JitPlans> {
        let request = std::mem::take(&mut self.jit_request);
        if matches!(request, JitRequest::Run)
            && std::env::var_os("TRS_JIT").is_none()
        {
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

        // ---- pass A: collect scheduled rules (NO allocation) ----
        // Schedule order defines ordinals and shared-cone ownership;
        // slots are handed out in pass B per instance in
        // module-canonical order, so twin instances of one module type
        // get identical region-relative layouts (code dedup).
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
                        cf_slot: 0,
                        wf_slot: 0,
                        eager: en.eager.clone(),
                        shared,
                    });
                    let _ = mir;
                }
            }
        }
        let mut per_inst_rules: HashMap<usize, Vec<usize>> = HashMap::new();
        for (k, ri) in rules.iter().enumerate() {
            per_inst_rules.entry(ri.inst).or_default().push(k);
        }
        for v in per_inst_rules.values_mut() {
            v.sort_by_key(|&k| rules[k].rule_idx);
        }

        // ---- outline selection (TRS_JIT_SPLIT=<thresh> opt-in) ----
        // per module type: which def pieces become helper fns, and
        // which of those are per-instant memoizable.  Eager-set defs
        // are excluded: a helper body hitting the eager-slot fast path
        // could read slots whose owners have not run yet.
        let split_thresh: Option<u32> = std::env::var("TRS_JIT_SPLIT")
            .ok()
            .and_then(|v| v.parse().ok())
            .filter(|&t| t > 0);
        let outlined_sel: HashMap<(usize, StrId), (u32, bool, Vec<StrId>)> = if let Some(th) =
            split_thresh
        {
            let mut exemplar: HashMap<usize, usize> = HashMap::new();
            for i in 0..self.insts.len() {
                if let InstKind::User { module, .. } = &self.insts[i].kind {
                    exemplar.entry(self.mods[*module].ir).or_insert(i);
                }
            }
            let mut eager_excl: std::collections::HashSet<(usize, StrId)> =
                Default::default();
            for ri in &rules {
                let mir = self.mods[self.module_of(ri.inst)].ir;
                for &e in &ri.eager {
                    eager_excl.insert((mir, e));
                }
            }
            let insts = &self.insts;
            let mods = &self.mods;
            let ex2 = exemplar.clone();
            let kind = move |m: usize, name: StrId| -> ChildRef {
                let Some(&ex) = ex2.get(&m) else { return ChildRef::Opaque };
                let InstKind::User { children, .. } = &insts[ex].kind else {
                    return ChildRef::Opaque;
                };
                let Some(&ci) =
                    children.iter().find(|(k, _)| **k == name).map(|(_, v)| v)
                else {
                    return ChildRef::Opaque;
                };
                match &insts[ci].kind {
                    InstKind::Prim(p) => ChildRef::Prim(match p.arena_kind() {
                        Some(ArenaKind::Reg { .. }) => ChildClass::Reg,
                        Some(ArenaKind::ConfigReg { .. }) => ChildClass::CfgReg,
                        Some(ArenaKind::Wire { .. }) => ChildClass::Wire,
                        Some(ArenaKind::Fifo { .. }) => ChildClass::Fifo,
                        None => ChildClass::Other,
                    }),
                    InstKind::User { module, .. } => ChildRef::User(mods[*module].ir),
                }
            };
            let mut an = ConeAnalyzer::new(&self.d, &kind, th);
            let mut sel = HashMap::new();
            let mut mirs: Vec<usize> = exemplar.keys().copied().collect();
            mirs.sort_unstable();
            for mir in mirs {
                for (name, pi) in an.module(mir) {
                    if pi.outlined && !eager_excl.contains(&(mir, name)) {
                        let w = self.d.modules[mir]
                            .defs
                            .iter()
                            .find(|dd| dd.name == name)
                            .map(|dd| dd.width.max(1))
                            .unwrap_or(1);
                        let stable = pi.stable && pi.ports.is_empty();
                        sel.insert((mir, name), (w, stable, pi.ports.clone()));
                    }
                }
            }
            if trace {
                eprintln!(
                    "trs jit: split thresh={th}: {} pieces ({} memoized)",
                    sel.len(),
                    sel.values().filter(|(_, st, _)| *st).count()
                );
            }
            sel
        } else {
            HashMap::new()
        };

        // ---- pass B: DFS subtree-contiguous allocation ----
        // Every slot an instance's compiled code touches (its prims,
        // ENs, rule cf/wf/eager, and everything in its submodule
        // subtree) lands in one contiguous region, at offsets that are
        // uniform across instances of the same module type.
        let mut inst_envs: HashMap<usize, InstEnv> = HashMap::new();
        let mut attach: Vec<(usize, u32)> = Vec::new(); // (prim inst, base)
        let reset_node_slot: Vec<u32> =
            (0..self.rst_asserted.len()).map(|_| alloc(&mut nslots, 1)).collect();
        // the dispatcher stamps the current instant here at every edge
        let now_slot = alloc(&mut nslots, 1);
        // memo stamp slots initialize to u64::MAX (0 == instant 0)
        let mut memo_stamp_slots: Vec<u32> = Vec::new();
        let mut is_child = vec![false; self.insts.len()];
        for i in 0..self.insts.len() {
            if let InstKind::User { children, .. } = &self.insts[i].kind {
                for (_, &c) in children.iter() {
                    is_child[c] = true;
                }
            }
        }
        enum Walk {
            Enter(usize),
            Exit(usize),
        }
        let mut stack: Vec<Walk> = (0..self.insts.len())
            .rev()
            .filter(|&i| {
                !is_child[i] && matches!(self.insts[i].kind, InstKind::User { .. })
            })
            .map(Walk::Enter)
            .collect();
        let mut subtree: HashMap<usize, (u32, u32)> = HashMap::new();
        let mut dfs_order: Vec<usize> = Vec::new();
        while let Some(w) = stack.pop() {
            let i = match w {
                Walk::Exit(i) => {
                    subtree.get_mut(&i).expect("exit before enter").1 = nslots;
                    continue;
                }
                Walk::Enter(i) => i,
            };
            let InstKind::User { module, children, resets, .. } = &self.insts[i].kind
            else {
                continue;
            };
            dfs_order.push(i);
            let region_start = nslots;
            let module = *module;
            let mir = self.mods[module].ir;
            let children: HashMap<StrId, usize> =
                children.iter().map(|(k, v)| (*k, *v)).collect();
            let mut reg_slot = HashMap::new();
            let mut wire_slot = HashMap::new();
            let mut creg_slot = HashMap::new();
            let mut fifo_slot = HashMap::new();
            // sorted iteration: slot assignment must be deterministic
            // across processes so an AOT artifact's baked slot numbers
            // match a fresh planning walk at load time
            let mut kids: Vec<(StrId, usize)> =
                children.iter().map(|(&k, &v)| (k, v)).collect();
            kids.sort_unstable();
            for &(name, ci) in &kids {
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
                    Some(ArenaKind::ConfigReg { width }) => {
                        let words = width.max(1).div_ceil(64);
                        let base = alloc(&mut nslots, 2 * words + 1);
                        creg_slot.insert(name, (base, width));
                        attach.push((ci, base));
                    }
                    Some(ArenaKind::Fifo { width, size, guard }) => {
                        let words = width.max(1).div_ceil(64);
                        let base = alloc(&mut nslots, 6 + size * words);
                        fifo_slot.insert(name, (base, width, size, guard));
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
            let mut enps: Vec<StrId> = self.mods[module]
                .ports
                .iter()
                .filter(|&(_, &(_w, kind))| kind == ir::PortKind::MethodEnable)
                .map(|(&pn, _)| pn)
                .collect();
            enps.sort_unstable();
            for pname in enps {
                en_slot.insert(pname, alloc(&mut nslots, 1));
            }
            // per-rule cf/wf slots in module-canonical rule order, then
            // the instance's eager-def UNION in sorted order: eager
            // attachment (first-Sched-node) can split differently
            // between twin instances, but the union and this layout
            // stay type-uniform (dedup depends on it)
            let mut cfwf_slot = HashMap::new();
            let mut eager_slot: HashMap<StrId, (u32, u32)> = HashMap::new();
            if let Some(rks) = per_inst_rules.get(&i) {
                for &k in rks {
                    let cf_slot = alloc(&mut nslots, 1);
                    let wf_slot = alloc(&mut nslots, 1);
                    let rr = &self.d.modules[mir].rules[rules[k].rule_idx];
                    cfwf_slot.insert(rr.can_fire, cf_slot);
                    cfwf_slot.insert(rr.will_fire, wf_slot);
                    rules[k].cf_slot = cf_slot;
                    rules[k].wf_slot = wf_slot;
                }
                let mut union: Vec<StrId> = Vec::new();
                for &k in rks {
                    for &e in &rules[k].eager {
                        if !union.contains(&e) {
                            union.push(e);
                        }
                    }
                }
                union.sort_unstable();
                for e in union {
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
                    eager_slot.insert(e, (base, ew));
                }
            }
            // memo slots for outlined stable defs of this module type
            // (sorted: type-uniform offsets, part of the dedup sig)
            let mut memo_slot: HashMap<StrId, (u32, u32)> = HashMap::new();
            {
                let mut ms: Vec<(StrId, u32)> = outlined_sel
                    .iter()
                    .filter(|((m, _), (_, st, _))| *m == mir && *st)
                    .map(|((_, dn), (w, _, _))| (*dn, *w))
                    .collect();
                ms.sort_unstable();
                for (dn, w) in ms {
                    let base = alloc(&mut nslots, 1 + w.div_ceil(64));
                    memo_slot.insert(dn, (base, w));
                    memo_stamp_slots.push(base);
                }
            }
            subtree.insert(i, (region_start, 0));
            stack.push(Walk::Exit(i));
            for &(_, c) in kids.iter().rev() {
                if matches!(self.insts[c].kind, InstKind::User { .. }) {
                    stack.push(Walk::Enter(c));
                }
            }
            inst_envs.insert(
                i,
                InstEnv {
                    mir,
                    children,
                    reg_slot,
                    wire_slot,
                    creg_slot,
                    fifo_slot,
                    reset_slot,
                    en_slot,
                    cfwf_slot,
                    eager_slot,
                    memo_slot,
                    region: (region_start, 0),
                },
            );
        }
        // subtree extents (known only after the whole subtree walked)
        for (i, &(s0, s1)) in &subtree {
            if let Some(e) = inst_envs.get_mut(i) {
                e.region = (s0, s1);
            }
        }

        // ---- per-instance subtree signatures (exec dedup classes) ----
        // Two instances share compiled exec bodies iff their signatures
        // match.  The sig must cover EVERY input the exec lowering
        // reads: module IR id, region-relative slot layout (all maps),
        // absolute reset-node slots, and the user children recursively.
        // (Stage-2a made twin IR raw-identical; the sweep + twin test
        // referee this invariant.)
        let inst_sig: HashMap<usize, u64> = {
            use std::hash::{Hash, Hasher};
            let mut sigs: HashMap<usize, u64> = HashMap::new();
            for &i in dfs_order.iter().rev() {
                let e = &inst_envs[&i];
                let mut h = std::collections::hash_map::DefaultHasher::new();
                e.mir.hash(&mut h);
                (e.region.1 - e.region.0).hash(&mut h);
                let r0 = e.region.0;
                let mut m1: Vec<_> =
                    e.reg_slot.iter().map(|(&k, &(b, w))| (k, b - r0, w)).collect();
                m1.sort_unstable();
                m1.hash(&mut h);
                let mut m2: Vec<_> =
                    e.wire_slot.iter().map(|(&k, &(b, w))| (k, b - r0, w)).collect();
                m2.sort_unstable();
                m2.hash(&mut h);
                let mut m3: Vec<_> =
                    e.creg_slot.iter().map(|(&k, &(b, w))| (k, b - r0, w)).collect();
                m3.sort_unstable();
                m3.hash(&mut h);
                let mut m4: Vec<_> = e
                    .fifo_slot
                    .iter()
                    .map(|(&k, &(b, w, sz, g))| (k, b - r0, w, sz, g))
                    .collect();
                m4.sort_unstable();
                m4.hash(&mut h);
                let mut m5: Vec<_> =
                    e.en_slot.iter().map(|(&k, &b)| (k, b - r0)).collect();
                m5.sort_unstable();
                m5.hash(&mut h);
                let mut m6: Vec<_> =
                    e.cfwf_slot.iter().map(|(&k, &b)| (k, b - r0)).collect();
                m6.sort_unstable();
                m6.hash(&mut h);
                let mut m7: Vec<_> =
                    e.eager_slot.iter().map(|(&k, &(b, w))| (k, b - r0, w)).collect();
                m7.sort_unstable();
                m7.hash(&mut h);
                // reset nodes are design-global: absolute slots baked
                let mut m8: Vec<_> =
                    e.reset_slot.iter().map(|(&k, &b)| (k, b)).collect();
                m8.sort_unstable();
                m8.hash(&mut h);
                let mut m9: Vec<_> =
                    e.memo_slot.iter().map(|(&k, &(b, w))| (k, b - r0, w)).collect();
                m9.sort_unstable();
                m9.hash(&mut h);
                let mut kids: Vec<_> = e
                    .children
                    .iter()
                    .filter_map(|(&n, &c)| sigs.get(&c).map(|&sg| (n, sg)))
                    .collect();
                kids.sort_unstable();
                kids.hash(&mut h);
                sigs.insert(i, h.finish());
            }
            sigs
        };

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
        // ---- exec dedup classes: one compiled body per class ----
        let mut classes: Vec<(usize, Vec<usize>)> = Vec::new();
        {
            let mut key_to_class: HashMap<(u64, usize), usize> = HashMap::new();
            for (o, sp) in specs.iter().enumerate() {
                let key = (inst_sig[&sp.inst], sp.rule_idx);
                let c = *key_to_class.entry(key).or_insert_with(|| {
                    classes.push((o, Vec::new()));
                    classes.len() - 1
                });
                classes[c].1.push(o);
            }
        }
        if trace {
            eprintln!(
                "trs jit: {} exec bodies in {} classes",
                specs.len(),
                classes.len()
            );
        }

        // ---- helper fns for outlined pieces (split opt-in) ----
        // v1: only module types whose instances all share one subtree
        // sig (helper symbols are sig-keyed); shared JIT/AOT lowering,
        // resolution differs (baked addresses vs .so symbols)
        let mut helper_specs: Vec<HelperSpec> = Vec::new();
        if !outlined_sel.is_empty() && !specs.is_empty() {
            let mut mir_sigs: HashMap<usize, std::collections::HashSet<u64>> =
                HashMap::new();
            let mut exemplar: HashMap<usize, usize> = HashMap::new();
            let mut iis: Vec<usize> = inst_envs.keys().copied().collect();
            iis.sort_unstable();
            for i in iis {
                let e = &inst_envs[&i];
                mir_sigs.entry(e.mir).or_default().insert(inst_sig[&i]);
                exemplar.entry(e.mir).or_insert(i);
            }
            let mut keys: Vec<(usize, StrId)> = outlined_sel.keys().copied().collect();
            keys.sort_unstable();
            for (mir, dn) in keys {
                if mir_sigs.get(&mir).map(|x| x.len()) != Some(1) {
                    continue;
                }
                let ex = exemplar[&mir];
                let (w, st, ref pnames) = outlined_sel[&(mir, dn)];
                let mut ports: Vec<(StrId, u32)> = Vec::new();
                let mut ok = true;
                for &pn in pnames {
                    let m = &self.d.modules[mir];
                    let w = m
                        .inputs
                        .iter()
                        .find(|q| q.name == pn)
                        .map(|q| q.width)
                        .or_else(|| {
                            m.methods.iter().find_map(|me| {
                                me.args.iter().find(|q| q.name == pn).map(|q| q.width)
                            })
                        });
                    match w {
                        Some(w) => ports.push((pn, w.max(1))),
                        None => ok = false,
                    }
                }
                if !ok {
                    continue;
                }
                helper_specs.push(HelperSpec {
                    mir,
                    def: dn,
                    width: w,
                    sym: format!("hlp_{:016x}_{}", inst_sig[&ex], dn),
                    inst: ex,
                    memo_slot: if st {
                        Some(inst_envs[&ex].memo_slot[&dn].0)
                    } else {
                        None
                    },
                    ports,
                });
            }
        }
        let refs_sym: HelperMap = helper_specs
            .iter()
            .map(|h| {
                ((h.mir, h.def), (HelperRef::Sym(h.sym.clone()), h.width, h.ports.clone()))
            })
            .collect();
        // deferred: Load requests only need addresses if the artifact
        // fails to load (in-process fallback) — never compile helpers
        // just to throw them away at every artifact startup
        let compile_helpers_now = |inst_envs: &HashMap<usize, InstEnv>| -> HelperMap {
            if helper_specs.is_empty() {
                return HelperMap::new();
            }
            trs_codegen::lower::llvm_init_once();
            let env = PlanEnv { d: &self.d, insts: inst_envs, now_slot };
            let pseudo = specs[0].clone();
            let t0 = std::time::Instant::now();
            match compile_helpers(&env, &helper_specs, &refs_sym, &pseudo) {
                Ok(addrs) => {
                    if std::env::var_os("TRS_JIT_TIME").is_some() {
                        eprintln!(
                            "trs jit: {} helpers compiled {:?}",
                            helper_specs.len(),
                            t0.elapsed()
                        );
                    }
                    let am: HashMap<String, usize> = addrs.into_iter().collect();
                    helper_specs
                        .iter()
                        .map(|h| {
                            (
                                (h.mir, h.def),
                                (HelperRef::Addr(am[&h.sym]), h.width, h.ports.clone()),
                            )
                        })
                        .collect()
                }
                Err(e) => {
                    if trace {
                        eprintln!("trs jit: helpers off ({e})");
                    }
                    HelperMap::new()
                }
            }
        };

        // Load attempt FIRST: an artifact carrying protos skips
        // trial_lower entirely (0.32s of sudoku startup); any failure
        // falls back to in-process compilation (which trials below)
        let mut preloaded: Option<(Vec<CompiledSched>, Vec<CompiledExec>)> = None;
        let mut protos_opt: Option<Vec<FnProtos>> = None;
        if let JitRequest::Load { so } = &request {
            match aot_load(so, self.bir_hash, &specs, &classes, split_thresh.unwrap_or(0))
            {
                Ok((sch, exe, pr)) => {
                    preloaded = Some((sch, exe));
                    protos_opt = Some(pr);
                }
                Err(e) => {
                    eprintln!(
                        "trs: artifact {}: {e}; compiling in-process instead",
                        so.display()
                    );
                }
            }
        }
        // eligibility + call-site tables via trial lowering (link, run,
        // and artifact-fallback paths; skipped on successful loads)
        let protos: Vec<FnProtos> = match protos_opt {
            Some(p) => p,
            None => {
                let env = PlanEnv { d: &self.d, insts: &inst_envs, now_slot };
                let t0 = std::time::Instant::now();
                match trial_lower(&env, &specs) {
                    Ok(p) => {
                        if std::env::var_os("TRS_JIT_TIME").is_some() {
                            eprintln!("trs jit: trial lower {:?}", t0.elapsed());
                        }
                        p
                    }
                    Err(e) => {
                        if let JitRequest::Emit { .. } = &request {
                            self.jit_emit_result =
                                Some(crate::AotEmit::Ineligible(e.to_string()));
                        }
                        if trace {
                            eprintln!("trs jit: off ({e})");
                        }
                        return None;
                    }
                }
            }
        };

        // trs link: emit the artifact .so and stop (nothing runs)
        if let JitRequest::Emit { so } = &request {
            self.jit_emit_result = Some(
                match aot_emit(
                    &self.d,
                    &inst_envs,
                    &specs,
                    now_slot,
                    &classes,
                    &helper_specs,
                    &refs_sym,
                    split_thresh.unwrap_or(0),
                    &protos,
                    so,
                    self.bir_hash,
                ) {
                    Ok(()) => crate::AotEmit::Compiled,
                    Err(e) => crate::AotEmit::Failed(e),
                },
            );
            return None;
        }



        let n = specs.len();
        let nworkers = jit_workers(n);

        // SCHED functions compile eagerly (blocking, parallel): they
        // run on every edge and the cone-sharing keeps them small
        let chunk = n.div_ceil(nworkers).max(1);
        let helpers_addr: HelperMap = if preloaded.is_some() {
            HelperMap::new()
        } else {
            compile_helpers_now(&inst_envs)
        };
        let jit_helpers: Option<&HelperMap> =
            (!helpers_addr.is_empty()).then_some(&helpers_addr);
        let (scheds, preexecs) = if let Some((s, e)) = preloaded {
            (s, Some(e))
        } else {
            (
                aot_or_jit_scheds(
                    self, &inst_envs, &specs, now_slot, jit_helpers, nworkers, trace,
                )?,
                None,
            )
        };

        let exec_args: Vec<(u64, u64)> = specs
            .iter()
            .map(|sp| {
                let r0 = inst_envs[&sp.inst].region.0 as u64;
                (r0, sp.token_base)
            })
            .collect();
        let nclasses = classes.len();
        let cchunk = nclasses.div_ceil(nworkers).max(1);
        let lazy = Arc::new(LazyJit {
            design: self.d.clone(),
            insts: inst_envs,
            specs,
            now_slot,
            exec_args,
            protos,
            classes,
            helpers: Arc::new(helpers_addr),
            scheds,
            next_batch: std::sync::atomic::AtomicUsize::new(0),
            batch_size: cchunk,
            cold: std::sync::atomic::AtomicUsize::new(if preexecs.is_some() {
                0
            } else {
                nclasses
            }),
            cells: (0..n).map(|_| OnceLock::new()).collect(),
        });
        self.jit_shared = Some(lazy.clone());

        match preexecs {
            Some(execs) => {
                // artifact bodies: every cell warm from the start
                for (i, ce) in execs.into_iter().enumerate() {
                    let _ = lazy.cells[i].set(ce);
                }
            }
            None => {
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
        for &slot in &memo_stamp_slots {
            unsafe { *arena_ptr.add(slot as usize) = u64::MAX };
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
            now_slot,
            lazy,
            exec_fallback,
        })
    }
}
