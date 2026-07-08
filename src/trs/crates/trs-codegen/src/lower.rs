//! BIR -> LLVM IR lowering (feature `llvm`).
//!
//! Hybrid P2 slice (DESIGN.md §10): per-rule native functions running
//! inside the interpreter's event loop, over a shared u64 state arena
//! (plain sync registers of any width, reset-port levels, per-rule
//! CF/WF, and schedule-position "eager" defs; wide state takes
//! ceil(width/64) consecutive slots).  Each eligible rule compiles to
//!
//!   sched_<label>(arena: *mut u64)
//!     — evaluates the CAN_FIRE/WILL_FIRE cone (expanding defs as SSA),
//!       applies inhibitor slots, stores CF/WF and the entry's eager
//!       defs to their slots (the C++ schedule_posedge position);
//!   exec_<label>(arena: *mut u64, env: *mut c_void) -> i32
//!     — loads WF, executes the body: SSA defs, conditional register
//!       stores, Cond control flow; $display-family statements call
//!       back into the interpreter (`ForeignCb`), and a nonzero return
//!       (=$finish) unwinds immediately.  Returns nonzero iff stopped.
//!
//! Values are native LLVM iN integers of their exact BSV width — LLVM
//! legalizes arbitrary widths — so no masking and no 64-bit cap.
//! Shift semantics mirror Value::shl/lshr/ashr (overflow to zero /
//! sign-fill; LLVM's shift-amount poison is guarded); Quot/Rem raise
//! SIGFPE on a zero divisor like the interpreter and native division.
//! Ineligibility is an Err from the trial lowering — the caller falls
//! back to the interpreter per design.

use std::collections::HashMap;

use trs_ir::{Action, Design, Expr, PrimOp, Stmt, StrId};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::IntType;
use inkwell::values::{FunctionValue, IntValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};

/// Callback for foreign statements inside compiled bodies (the
/// $display family and value/ActionValue tasks): compiled code
/// evaluates the arguments natively at the statement position and
/// passes their words in `args` (string literals occupy no words —
/// the call-site table carries them); a task's result words land in
/// `out`.  Returns nonzero to stop the simulation ($finish).
pub type ForeignCb = unsafe extern "C" fn(
    env: *mut core::ffi::c_void,
    token: u64,
    args: *const u64,
    out: *mut u64,
) -> i32;

/// Called on a zero divisor: must raise SIGFPE (never returns normally).
pub type SigfpeCb = unsafe extern "C" fn();

/// Trampoline for prim method calls the arena does not model (FIFOs,
/// ConfigRegs, RegFiles, ...): the interpreter unmarshals `args` per
/// the call-site table, invokes the boxed prim, and writes the result
/// words to `out`.  Token = rule ordinal << 16 | local call index.
pub type PrimCb = unsafe extern "C" fn(
    env: *mut core::ffi::c_void,
    token: u64,
    args: *const u64,
    out: *mut u64,
);

/// One compiled prim call site (resolved by the trampoline).
pub struct PrimCallSpec {
    /// global instance index of the prim
    pub inst: usize,
    pub method: StrId,
    /// argument widths, in order (marshaled as consecutive word runs)
    pub arg_widths: Vec<u32>,
    /// result width (0 = action, no result)
    pub ret_width: u32,
    /// action (mutates) vs pure value read
    pub is_action: bool,
}

/// Per-instance name resolution: arena slots and child links assigned
/// by the interpreter.
pub struct InstEnv {
    /// module index in `d.modules`
    pub mir: usize,
    /// local child instance name -> global instance index
    pub children: HashMap<StrId, usize>,
    /// local register instance name -> (arena base slot, width); plain
    /// sync/no-reset regs only, ceil(width/64) consecutive slots
    pub reg_slot: HashMap<StrId, (u32, u32)>,
    /// local RWire/PulseWire instance name -> (base slot, width): valid
    /// word at base, value words after it
    pub wire_slot: HashMap<StrId, (u32, u32)>,
    /// module reset input port name -> arena slot holding the PORT level
    /// (1 = deasserted, matching the interpreter's Port read)
    pub reset_slot: HashMap<StrId, u32>,
    /// EN_<m> port name -> arena slot; zeroed at composition dispatch,
    /// stored by compiled call sites (the C++ enable protocol)
    pub en_slot: HashMap<StrId, u32>,
    /// any rule's CAN_FIRE/WILL_FIRE def name -> arena slot (this
    /// instance); reads of other rules' fire signals become slot loads
    pub cfwf_slot: HashMap<StrId, u32>,
    /// schedule-position def name -> (arena base slot, width): stored by
    /// the sched fn that owns the def, reloaded by exec bodies (the C++
    /// `DEF_x = DEF_x;` reuse semantics)
    pub eager_slot: HashMap<StrId, (u32, u32)>,
}

/// Design-wide plan: one InstEnv per user instance the compiled code
/// can touch.
pub struct PlanEnv<'a> {
    pub d: &'a Design,
    pub insts: &'a HashMap<usize, InstEnv>,
}

/// One rule to compile.
pub struct RuleSpec {
    /// owning instance (key into PlanEnv::insts)
    pub inst: usize,
    pub rule_idx: usize,
    /// arena slots of earlier CAN_FIREs negated into this rule's CF
    /// (intra-module ME inhibitors + cross-module inhibitors)
    pub inhibit_slots: Vec<u32>,
    pub cf_slot: u32,
    pub wf_slot: u32,
    /// defs this rule's Sched entry evaluates at its schedule position
    /// (REntry::eager); each must have an `eager_slot`
    pub eager: Vec<StrId>,
    /// eager defs of the SAME instance owned by entries that run
    /// strictly earlier in this rule's composition: the sched fn may
    /// load their slots instead of re-expanding the cone (the owner has
    /// already stored them this edge)
    pub shared: Vec<StrId>,
    /// unique function-name label (instance path + rule name)
    pub label: String,
    /// baked into callback tokens: token = base + local foreign-stmt
    /// index (callers use e.g. global_rule_ordinal << 16 so one shared
    /// callback can resolve the rule and the statement)
    pub token_base: u64,
}

/// One compiled foreign call site: everything the interpreter needs
/// to rebuild the Arg list and dispatch ($display family, value tasks).
pub struct ForeignSpec {
    /// instance for $display location reporting
    pub inst: usize,
    pub func: StrId,
    /// result width (0 = plain action, no result)
    pub ret_width: u32,
    pub args: Vec<FArgSpec>,
}

/// One foreign argument: a string literal (no marshaled words) or a
/// numeric value of the given width with its signed-display flag.
pub enum FArgSpec {
    Str(StrId),
    Num { width: u32, signed: bool },
}

/// A compiled rule sched function (kept alive by the leaked engine).
pub struct CompiledSched {
    pub sched: unsafe extern "C" fn(*mut u64, *mut core::ffi::c_void),
    /// token -> foreign call-site spec (cones can reach foreign value
    /// paths only through prim calls today, but keep both tables)
    pub foreign_stmts: Vec<ForeignSpec>,
    /// token -> prim call site
    pub prim_calls: Vec<PrimCallSpec>,
}

/// A compiled rule body.
pub struct CompiledExec {
    pub exec: unsafe extern "C" fn(*mut u64, *mut core::ffi::c_void) -> i32,
    pub foreign_stmts: Vec<ForeignSpec>,
    pub prim_calls: Vec<PrimCallSpec>,
}

/// Which half of a rule a callback token belongs to (bit 16; the rule
/// ordinal sits at bit 17+, the site index in the low 16 bits).
pub const TOKEN_KIND_EXEC: u64 = 1 << 16;

/// Why a rule cannot be compiled; the caller falls back to the
/// interpreter (this is expected and silent — coverage grows over time).
#[derive(Debug)]
pub struct Ineligible(pub String);

impl std::fmt::Display for Ineligible {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

fn nope<T>(why: impl Into<String>) -> Result<T, Ineligible> {
    Err(Ineligible(why.into()))
}

fn words_for(w: u32) -> u32 {
    w.div_ceil(64)
}

/// Compile a batch of rules for one (module type, instance) pair.
/// All-or-nothing per call: any ineligible rule fails the whole batch.
/// LLVM global state (target registry, MCJIT linkage) must initialize
/// exactly once before engines are created on worker threads — the
/// per-call init inside create_jit_execution_engine races otherwise.
pub fn llvm_init_once() {
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(|| {
        inkwell::targets::Target::initialize_native(
            &inkwell::targets::InitializationConfig::default(),
        )
        .expect("LLVM native target init");
        // force MCJIT linkage and any lazy registry state on one thread
        let ctx = Context::create();
        let m = ctx.create_module("trs_init");
        let _ = m.create_jit_execution_engine(OptimizationLevel::None);
    });
}

/// Eligibility check: run the full lowering into a throwaway context
/// (no engine, no LLVM codegen — ~ms per rule) so ineligibility is
/// decided synchronously before any compiled dispatch is planned.
pub fn trial_lower(env: &PlanEnv, specs: &[RuleSpec]) -> Result<(), Ineligible> {
    let ctx = Context::create();
    let module = ctx.create_module("trs_trial");
    let i64t = ctx.i64_type();
    let i32t = ctx.i32_type();
    let ptrt = ctx.ptr_type(AddressSpace::default());
    let cb_ty =
        i32t.fn_type(&[ptrt.into(), i64t.into(), ptrt.into(), ptrt.into()], false);
    let cb_fn = module.add_function("trs_jit_foreign", cb_ty, None);
    let fpe_ty = ctx.void_type().fn_type(&[], false);
    let fpe_fn = module.add_function("trs_jit_sigfpe", fpe_ty, None);
    let prim_ty = ctx
        .void_type()
        .fn_type(&[ptrt.into(), i64t.into(), ptrt.into(), ptrt.into()], false);
    let prim_fn = module.add_function("trs_jit_prim", prim_ty, None);
    for spec in specs {
        let mut lc = Lower {
            env,
            ctx: &ctx,
            module: &module,
            builder: ctx.create_builder(),
            cb_fn,
            fpe_fn,
            prim_fn,
            spec,
            token_kind: 0,
            foreign_stmts: Vec::new(),
            prim_calls: Vec::new(),
        };
        lc.lower_sched()?;
        lc.token_kind = TOKEN_KIND_EXEC;
        lc.lower_exec()?;
    }
    Ok(())
}

fn make_module<'ctx>(
    ctx: &'ctx Context,
) -> (Module<'ctx>, FunctionValue<'ctx>, FunctionValue<'ctx>, FunctionValue<'ctx>) {
    let module = ctx.create_module("trs_rules");
    let i64t = ctx.i64_type();
    let i32t = ctx.i32_type();
    let ptrt = ctx.ptr_type(AddressSpace::default());
    let cb_ty =
        i32t.fn_type(&[ptrt.into(), i64t.into(), ptrt.into(), ptrt.into()], false);
    let cb_fn = module.add_function("trs_jit_foreign", cb_ty, None);
    let fpe_ty = ctx.void_type().fn_type(&[], false);
    let fpe_fn = module.add_function("trs_jit_sigfpe", fpe_ty, None);
    let prim_ty = ctx
        .void_type()
        .fn_type(&[ptrt.into(), i64t.into(), ptrt.into(), ptrt.into()], false);
    let prim_fn = module.add_function("trs_jit_prim", prim_ty, None);
    (module, cb_fn, fpe_fn, prim_fn)
}

fn finish_engine(
    module: Module<'static>,
    foreign_cb: ForeignCb,
    sigfpe_cb: SigfpeCb,
    prim_cb: PrimCb,
) -> Result<inkwell::execution_engine::ExecutionEngine<'static>, Ineligible> {
    if std::env::var_os("TRS_JIT_DUMP").is_some() {
        eprintln!("{}", module.print_to_string().to_string());
    }
    // JIT default is -O0 (DESIGN.md §6: iterate-run starts fast; -O0
    // halves LLVM time and costs ~4% sim speed on compute-bound loops)
    let opt = match std::env::var("TRS_JIT_OPT").as_deref() {
        Ok("1") => OptimizationLevel::Less,
        Ok("2") => OptimizationLevel::Default,
        Ok("3") => OptimizationLevel::Aggressive,
        _ => OptimizationLevel::None,
    };
    let cb = module.get_function("trs_jit_foreign").unwrap();
    let fpe = module.get_function("trs_jit_sigfpe").unwrap();
    let prim = module.get_function("trs_jit_prim").unwrap();
    let ee = module
        .create_jit_execution_engine(opt)
        .map_err(|e| Ineligible(format!("LLVM JIT engine: {e}")))?;
    ee.add_global_mapping(&cb, foreign_cb as usize);
    ee.add_global_mapping(&fpe, sigfpe_cb as usize);
    ee.add_global_mapping(&prim, prim_cb as usize);
    Ok(ee)
}

/// Compile the SCHED functions for a batch of rules (eager: they run
/// on every edge).  All-or-nothing per call.
pub fn compile_scheds(
    env: &PlanEnv,
    specs: &[RuleSpec],
    foreign_cb: ForeignCb,
    sigfpe_cb: SigfpeCb,
    prim_cb: PrimCb,
) -> Result<Vec<CompiledSched>, Ineligible> {
    let ctx: &'static Context = Box::leak(Box::new(Context::create()));
    let (module, cb_fn, fpe_fn, prim_fn) = make_module(ctx);
    let mut protos = Vec::new();
    for spec in specs {
        let mut lc = Lower {
            env,
            ctx,
            module: &module,
            builder: ctx.create_builder(),
            cb_fn,
            fpe_fn,
            prim_fn,
            spec,
            token_kind: 0,
            foreign_stmts: Vec::new(),
            prim_calls: Vec::new(),
        };
        lc.lower_sched()?;
        protos.push((lc.foreign_stmts, lc.prim_calls));
    }
    let ee = finish_engine(module, foreign_cb, sigfpe_cb, prim_cb)?;
    let mut out = Vec::new();
    for (spec, (foreign_stmts, prim_calls)) in specs.iter().zip(protos) {
        let addr = ee
            .get_function_address(&format!("sched_{}", spec.label))
            .map_err(|e| Ineligible(format!("sched fn address: {e}")))?;
        out.push(CompiledSched {
            sched: unsafe { std::mem::transmute::<usize, _>(addr as usize) },
            foreign_stmts,
            prim_calls,
        });
    }
    std::mem::forget(ee);
    Ok(out)
}

/// Compile the EXEC (body) functions for a batch of rules (lazy: they
/// run only when the rule fires; an uncompiled body interprets).
pub fn compile_execs(
    env: &PlanEnv,
    specs: &[RuleSpec],
    foreign_cb: ForeignCb,
    sigfpe_cb: SigfpeCb,
    prim_cb: PrimCb,
) -> Result<Vec<CompiledExec>, Ineligible> {
    let ctx: &'static Context = Box::leak(Box::new(Context::create()));
    let (module, cb_fn, fpe_fn, prim_fn) = make_module(ctx);
    let mut protos = Vec::new();
    for spec in specs {
        let mut lc = Lower {
            env,
            ctx,
            module: &module,
            builder: ctx.create_builder(),
            cb_fn,
            fpe_fn,
            prim_fn,
            spec,
            token_kind: TOKEN_KIND_EXEC,
            foreign_stmts: Vec::new(),
            prim_calls: Vec::new(),
        };
        lc.lower_exec()?;
        protos.push((lc.foreign_stmts, lc.prim_calls));
    }
    let ee = finish_engine(module, foreign_cb, sigfpe_cb, prim_cb)?;
    let mut out = Vec::new();
    for (spec, (foreign_stmts, prim_calls)) in specs.iter().zip(protos) {
        let addr = ee
            .get_function_address(&format!("exec_{}", spec.label))
            .map_err(|e| Ineligible(format!("exec fn address: {e}")))?;
        out.push(CompiledExec {
            exec: unsafe { std::mem::transmute::<usize, _>(addr as usize) },
            foreign_stmts,
            prim_calls,
        });
    }
    std::mem::forget(ee);
    Ok(out)
}

struct Lower<'a, 'ctx> {
    env: &'a PlanEnv<'a>,
    ctx: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: Builder<'ctx>,
    cb_fn: FunctionValue<'ctx>,
    fpe_fn: FunctionValue<'ctx>,
    prim_fn: FunctionValue<'ctx>,
    spec: &'a RuleSpec,
    /// OR'd into callback tokens (TOKEN_KIND_EXEC for body passes)
    token_kind: u64,
    foreign_stmts: Vec<ForeignSpec>,
    prim_calls: Vec<PrimCallSpec>,
}

/// Per-scope lowering state: the arena pointer, the instance whose
/// names resolve here, and the SSA maps.  Method inlining opens a
/// fresh child Frame (its defs may depend on the call's arguments, so
/// the memo cannot be shared).
struct Frame<'ctx> {
    arena: PointerValue<'ctx>,
    /// env pointer (exec functions only)
    envp: Option<PointerValue<'ctx>>,
    /// the instance names resolve against
    inst: usize,
    /// Some(mi) while inlining method mi of `inst` (foreign-statement
    /// tokens must name the container body)
    method_idx: Option<usize>,
    /// method argument port name -> (value, width) for the current call
    args: HashMap<StrId, (IntValue<'ctx>, u32)>,
    /// def name -> computed value (cone memo or body locals)
    ssa: HashMap<StrId, IntValue<'ctx>>,
    /// defs currently being expanded (cycle guard)
    expanding: Vec<StrId>,
    /// ActionValue task results by cookie (Expr::TaskValue reads):
    /// (value, width)
    tasks: HashMap<u32, (IntValue<'ctx>, u32)>,
    /// exec-body scope (reloads eager slots) vs sched scope (stores them)
    is_exec: bool,
    /// inline depth (method-call recursion guard)
    depth: u32,
}

impl<'a, 'ctx> Lower<'a, 'ctx> {
    fn ie(&self, inst: usize) -> Result<&'a InstEnv, Ineligible> {
        match self.env.insts.get(&inst) {
            Some(e) => Ok(e),
            None => nope("instance outside the plan"),
        }
    }

    fn rule(&self) -> &trs_ir::Rule {
        let mir = self.env.insts[&self.spec.inst].mir;
        &self.env.d.modules[mir].rules[self.spec.rule_idx]
    }

    fn ity(&self, w: u32) -> IntType<'ctx> {
        // callers guarantee w >= 1 (zero widths are Ineligible earlier)
        self.ctx
            .custom_width_int_type(std::num::NonZeroU32::new(w.max(1)).unwrap())
            .unwrap_or_else(|e| panic!("trs jit: int type i{w}: {e}"))
    }

    fn def_width(&self, inst: usize, name: StrId) -> Result<u32, Ineligible> {
        let ie = self.ie(inst)?;
        if ie.cfwf_slot.contains_key(&name) {
            return Ok(1);
        }
        let m = &self.env.d.modules[ie.mir];
        match m.defs.iter().find(|d| d.name == name) {
            Some(d) if d.width >= 1 => Ok(d.width),
            Some(_) => nope("zero-width def"),
            None => nope("unknown def"),
        }
    }

    fn expr_width(&self, f: &Frame<'ctx>, e: &Expr) -> Result<u32, Ineligible> {
        match e {
            Expr::Def(n) => self.def_width(f.inst, *n),
            Expr::Port(p) => match f.args.get(p) {
                Some(&(_, w)) => Ok(w),
                None => Ok(1), // reset/EN ports
            },
            Expr::Const { width, .. }
            | Expr::MethCall { width, .. }
            | Expr::Prim { width, .. }
            | Expr::If { width, .. }
            | Expr::Case { width, .. } => {
                if *width >= 1 {
                    Ok(*width)
                } else {
                    nope("zero-width expression")
                }
            }
            _ => nope("expression kind not compilable"),
        }
    }

    /// Resize `v` (of width `from`) to width `to`.
    fn to_w(&self, v: IntValue<'ctx>, from: u32, to: u32, signed: bool) -> IntValue<'ctx> {
        use std::cmp::Ordering::*;
        match from.cmp(&to) {
            Equal => v,
            Greater => self.builder.build_int_truncate(v, self.ity(to), "tr").unwrap(),
            Less => {
                if signed {
                    self.builder.build_int_s_extend(v, self.ity(to), "sx").unwrap()
                } else {
                    self.builder.build_int_z_extend(v, self.ity(to), "zx").unwrap()
                }
            }
        }
    }

    /// i1 truthiness of a width-w value.
    fn nonzero(&self, v: IntValue<'ctx>, w: u32) -> IntValue<'ctx> {
        self.builder
            .build_int_compare(IntPredicate::NE, v, self.ity(w).const_zero(), "nz")
            .unwrap()
    }

    fn slot_ptr(&self, f: &Frame<'ctx>, slot: u32) -> PointerValue<'ctx> {
        let i64t = self.ctx.i64_type();
        unsafe {
            self.builder
                .build_gep(i64t, f.arena, &[i64t.const_int(slot as u64, false)], "sp")
                .unwrap()
        }
    }

    /// Load one raw arena word.
    fn load_word(&self, f: &Frame<'ctx>, slot: u32) -> IntValue<'ctx> {
        let p = self.slot_ptr(f, slot);
        self.builder
            .build_load(self.ctx.i64_type(), p, "ld")
            .unwrap()
            .into_int_value()
    }

    fn store_word(&self, f: &Frame<'ctx>, slot: u32, v: IntValue<'ctx>) {
        let p = self.slot_ptr(f, slot);
        self.builder.build_store(p, v).unwrap();
    }

    /// Load a width-w value from ceil(w/64) consecutive slots.
    fn load_val(&self, f: &Frame<'ctx>, base: u32, w: u32) -> IntValue<'ctx> {
        if w <= 64 {
            let word = self.load_word(f, base);
            return self.to_w(word, 64, w, false);
        }
        let t = self.ity(w);
        let mut acc = t.const_zero();
        for k in 0..words_for(w) {
            let word = self.load_word(f, base + k);
            let wide = self.builder.build_int_z_extend(word, t, "wz").unwrap();
            let sh = t.const_int((64 * k) as u64, false);
            let pos = self.builder.build_left_shift(wide, sh, "wsh").unwrap();
            acc = self.builder.build_or(acc, pos, "wor").unwrap();
        }
        acc
    }

    /// Store a width-w value into ceil(w/64) consecutive slots.
    fn store_val(&self, f: &Frame<'ctx>, base: u32, w: u32, v: IntValue<'ctx>) {
        if w <= 64 {
            let word = self.to_w(v, w, 64, false);
            self.store_word(f, base, word);
            return;
        }
        let t = self.ity(w);
        for k in 0..words_for(w) {
            let sh = t.const_int((64 * k) as u64, false);
            let piece = self.builder.build_right_shift(v, sh, false, "psh").unwrap();
            let word =
                self.builder.build_int_truncate(piece, self.ctx.i64_type(), "ptr").unwrap();
            self.store_word(f, base + k, word);
        }
    }

    /// Constant of width w from the BIR's LE 32-bit limbs.
    fn cval(&self, w: u32, limbs32: &[u32]) -> IntValue<'ctx> {
        let mut words = vec![0u64; words_for(w) as usize];
        for (i, &l) in limbs32.iter().enumerate() {
            if i / 2 < words.len() {
                words[i / 2] |= (l as u64) << (32 * (i % 2));
            }
        }
        self.ity(w).const_int_arbitrary_precision(&words)
    }

    /// Lower an expression to an iN value of its BSV width.
    fn expr(&mut self, f: &mut Frame<'ctx>, e: &Expr) -> Result<IntValue<'ctx>, Ineligible> {
        match e {
            Expr::Const { width, limbs } => {
                if *width == 0 {
                    return nope("zero-width constant");
                }
                Ok(self.cval(*width, limbs))
            }
            Expr::Def(n) => self.def(f, *n),
            Expr::Port(p) => {
                if let Some(&(v, _)) = f.args.get(p) {
                    return Ok(v);
                }
                let ie = self.ie(f.inst)?;
                if let Some(&slot) = ie.reset_slot.get(p) {
                    let word = self.load_word(f, slot);
                    return Ok(self.to_w(word, 64, 1, false));
                }
                if let Some(&slot) = ie.en_slot.get(p) {
                    let word = self.load_word(f, slot);
                    return Ok(self.to_w(word, 64, 1, false));
                }
                nope("port read outside args/reset/EN")
            }
            Expr::MethCall { width, instance, method, port, args } => {
                self.value_call(f, *width, *instance, *method, *port, args)
            }
            Expr::If { width, cond, then_, else_ } => {
                // real control flow, matching the interpreter's lazy arm
                // evaluation (prim calls in arms can have side effects);
                // LLVM if-converts pure arms back to selects
                let wc = self.expr_width(f, cond)?;
                let c = self.expr(f, cond)?;
                let cz = self.nonzero(c, wc);
                self.lazy_mux(f, *width, cz, then_, else_)
            }
            Expr::Case { width, scrutinee, arms, default } => {
                let ws = self.expr_width(f, scrutinee)?;
                let sv = self.expr(f, scrutinee)?;
                // right-fold into nested lazy muxes: eq(k) ? arm : rest
                fn build<'a, 'ctx>(
                    lc: &mut Lower<'a, 'ctx>,
                    f: &mut Frame<'ctx>,
                    width: u32,
                    ws: u32,
                    sv: IntValue<'ctx>,
                    arms: &[(u64, Expr)],
                    default: &Expr,
                ) -> Result<IntValue<'ctx>, Ineligible> {
                    match arms.split_first() {
                        None => {
                            let wd = lc.expr_width(f, default)?;
                            let v = lc.expr(f, default)?;
                            Ok(lc.to_w(v, wd, width, false))
                        }
                        Some(((k, arm), rest)) => {
                            let kc = lc.ity(ws).const_int_arbitrary_precision(&[*k]);
                            let hit = lc
                                .builder
                                .build_int_compare(IntPredicate::EQ, sv, kc, "k")
                                .unwrap();
                            lc.lazy_mux_fn(f, width, hit, arm, &|lc, f| {
                                build(lc, f, width, ws, sv, rest, default)
                            })
                        }
                    }
                }
                build(self, f, *width, ws, sv, arms, default)
            }
            Expr::TaskValue { width, cookie } => match f.tasks.get(cookie) {
                Some(&(v, vw)) => Ok(self.to_w(v, vw, (*width).max(1), false)),
                None => nope("task value before its task"),
            },
            Expr::Prim { op, width, args } => self.prim(f, *op, *width, args),
            _ => nope("expression kind not compilable"),
        }
    }

    /// A value-method call in an expression: arena register/wire reads
    /// on prim children, inlined result cones on user-module children.
    fn value_call(
        &mut self,
        f: &mut Frame<'ctx>,
        width: u32,
        instance: StrId,
        method: StrId,
        port: u32,
        args: &[Expr],
    ) -> Result<IntValue<'ctx>, Ineligible> {
        let ie = self.ie(f.inst)?;
        let mname = self.env.d.strings[method as usize].clone();
        if let Some(&(base, rw)) = ie.reg_slot.get(&instance) {
            if !matches!(mname.as_str(), "read" | "get" | "_read") || !args.is_empty() {
                return nope("non-read register method in expression");
            }
            if rw != width {
                return nope("register read width mismatch");
            }
            return Ok(self.load_val(f, base, rw));
        }
        if let Some(&(base, ww)) = ie.wire_slot.get(&instance) {
            return match mname.as_str() {
                "whas" => {
                    let word = self.load_word(f, base);
                    Ok(self.to_w(word, 64, 1, false))
                }
                "wget" if ww >= 1 && ww == width => Ok(self.load_val(f, base + 1, ww)),
                _ => nope("wire read mismatch"),
            };
        }
        // other prim children: trampoline into the interpreter's prim
        let Some(&child) = ie.children.get(&instance) else {
            return nope("call on unknown child");
        };
        if !self.env.insts.contains_key(&child) {
            let v = self
                .emit_prim_call(f, child, method, args, width, false)?
                .expect("value prim call returns");
            return Ok(v);
        }
        // user-module child: inline the method's result cone
        if port != 0 {
            return nope("multi-ported user method");
        }
        let cie = self.ie(child)?;
        let cmod = &self.env.d.modules[cie.mir];
        let Some((mi, m)) = cmod
            .methods
            .iter()
            .enumerate()
            .find(|(_, m)| m.name == method)
        else {
            return nope("unknown method on child");
        };
        if m.kind != trs_ir::MethodKind::Value {
            return nope("non-value method in expression");
        }
        let Some(res) = m.result.clone() else {
            return nope("value method without result");
        };
        if args.len() != m.args.len() {
            return nope("method arg count mismatch");
        }
        let margs = m.args.clone();
        let mut cf = self.child_frame(f, child, Some(mi))?;
        for (a, p) in args.iter().zip(&margs) {
            let wa = self.expr_width(f, a)?;
            let v0 = self.expr(f, a)?;
            let v = self.to_w(v0, wa, p.width, false);
            cf.args.insert(p.name, (v, p.width));
        }
        let rw = self.expr_width(&cf, &res)?;
        let v = self.expr(&mut cf, &res)?;
        // call_value zero-extends the result to the caller's width
        Ok(self.to_w(v, rw, width, false))
    }

    /// Branch-based mux: evaluate exactly one arm (interpreter If
    /// semantics), joining with a phi.  Defs memoized inside an arm are
    /// discarded after it — their SSA values would not dominate uses
    /// outside the arm.
    fn lazy_mux(
        &mut self,
        f: &mut Frame<'ctx>,
        width: u32,
        cz: IntValue<'ctx>,
        then_: &Expr,
        else_: &Expr,
    ) -> Result<IntValue<'ctx>, Ineligible> {
        self.lazy_mux_fn(f, width, cz, then_, &|lc, f| {
            let wx = lc.expr_width(f, else_)?;
            let v = lc.expr(f, else_)?;
            Ok(lc.to_w(v, wx, width, false))
        })
    }

    fn lazy_mux_fn(
        &mut self,
        f: &mut Frame<'ctx>,
        width: u32,
        cz: IntValue<'ctx>,
        then_: &Expr,
        else_gen: &dyn Fn(
            &mut Lower<'a, 'ctx>,
            &mut Frame<'ctx>,
        ) -> Result<IntValue<'ctx>, Ineligible>,
    ) -> Result<IntValue<'ctx>, Ineligible> {
        let func = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let then_bb = self.ctx.append_basic_block(func, "mt");
        let else_bb = self.ctx.append_basic_block(func, "me");
        let join_bb = self.ctx.append_basic_block(func, "mj");
        self.builder.build_conditional_branch(cz, then_bb, else_bb).unwrap();

        self.builder.position_at_end(then_bb);
        let saved: HashMap<StrId, IntValue<'ctx>> = f.ssa.clone();
        let wt = self.expr_width(f, then_)?;
        let tv0 = self.expr(f, then_)?;
        let tv = self.to_w(tv0, wt, width, false);
        f.ssa = saved.clone();
        let t_end = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(join_bb).unwrap();

        self.builder.position_at_end(else_bb);
        let ev = else_gen(self, f)?;
        f.ssa = saved;
        let e_end = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(join_bb).unwrap();

        self.builder.position_at_end(join_bb);
        let phi = self.builder.build_phi(self.ity(width), "mphi").unwrap();
        phi.add_incoming(&[(&tv, t_end), (&ev, e_end)]);
        Ok(phi.as_basic_value().into_int_value())
    }

    /// Compile a call-site into the prim trampoline: marshal argument
    /// words to a stack buffer, call, read result words back.  Needs
    /// the env pointer — sched functions carry it too.
    fn emit_prim_call(
        &mut self,
        f: &mut Frame<'ctx>,
        prim_inst: usize,
        method: StrId,
        args: &[Expr],
        ret_width: u32,
        is_action: bool,
    ) -> Result<Option<IntValue<'ctx>>, Ineligible> {
        let Some(envp) = f.envp else {
            return nope("prim call without env pointer");
        };
        let mut arg_widths = Vec::new();
        let mut vals = Vec::new();
        for a in args {
            let wa = self.expr_width(f, a)?;
            let v = self.expr(f, a)?;
            arg_widths.push(wa);
            vals.push((v, wa));
        }
        let total_words: u32 =
            arg_widths.iter().map(|&w| words_for(w)).sum::<u32>().max(1);
        let out_words = words_for(ret_width.max(1));
        let i64t = self.ctx.i64_type();
        let abuf = self
            .builder
            .build_array_alloca(i64t, i64t.const_int(total_words as u64, false), "pa")
            .unwrap();
        let obuf = self
            .builder
            .build_array_alloca(i64t, i64t.const_int(out_words as u64, false), "po")
            .unwrap();
        let mut off = 0u32;
        for (v, wa) in vals {
            let words = words_for(wa);
            let t = self.ity(wa.max(64 * words.min(1)).max(wa));
            let _ = t;
            for k in 0..words {
                let sh = self.ity(wa).const_int((64 * k) as u64, false);
                let piece = if k == 0 {
                    v
                } else {
                    self.builder.build_right_shift(v, sh, false, "pp").unwrap()
                };
                let word = self.to_w(piece, wa, 64, false);
                let idx = i64t.const_int((off + k) as u64, false);
                let p = unsafe {
                    self.builder.build_gep(i64t, abuf, &[idx], "pap").unwrap()
                };
                self.builder.build_store(p, word).unwrap();
            }
            off += words;
        }
        let token = self.spec.token_base | self.token_kind | self.prim_calls.len() as u64;
        self.prim_calls.push(PrimCallSpec {
            inst: prim_inst,
            method,
            arg_widths,
            ret_width: if is_action { 0 } else { ret_width },
            is_action,
        });
        self.builder
            .build_call(
                self.prim_fn,
                &[
                    envp.into(),
                    i64t.const_int(token, false).into(),
                    abuf.into(),
                    obuf.into(),
                ],
                "pc",
            )
            .unwrap();
        if is_action && ret_width == 0 {
            return Ok(None);
        }
        // reassemble the result from out words
        let w = ret_width;
        let t = self.ity(w);
        let mut acc = t.const_zero();
        for k in 0..words_for(w) {
            let idx = i64t.const_int(k as u64, false);
            let p = unsafe { self.builder.build_gep(i64t, obuf, &[idx], "pop").unwrap() };
            let word = self
                .builder
                .build_load(i64t, p, "pol")
                .unwrap()
                .into_int_value();
            if w <= 64 {
                acc = self.to_w(word, 64, w, false);
            } else {
                let wide = self.builder.build_int_z_extend(word, t, "pwz").unwrap();
                let sh = t.const_int((64 * k) as u64, false);
                let pos = self.builder.build_left_shift(wide, sh, "pws").unwrap();
                acc = self.builder.build_or(acc, pos, "pwo").unwrap();
            }
        }
        Ok(Some(acc))
    }

    fn child_frame(
        &self,
        f: &Frame<'ctx>,
        child: usize,
        method_idx: Option<usize>,
    ) -> Result<Frame<'ctx>, Ineligible> {
        if f.depth >= 32 {
            return nope("method inline depth");
        }
        Ok(Frame {
            arena: f.arena,
            envp: f.envp,
            inst: child,
            method_idx,
            args: HashMap::new(),
            ssa: HashMap::new(),
            expanding: Vec::new(),
            tasks: HashMap::new(),
            is_exec: f.is_exec,
            depth: f.depth + 1,
        })
    }

    /// Lower a def reference: body locals / cone memo, then this
    /// instance's fire-signal slots, then eager-def slots (exec bodies
    /// reload the schedule-time value), then table expansion.
    fn def(&mut self, f: &mut Frame<'ctx>, n: StrId) -> Result<IntValue<'ctx>, Ineligible> {
        if let Some(v) = f.ssa.get(&n) {
            return Ok(*v);
        }
        let ie = self.ie(f.inst)?;
        // other rules' fire signals read their (already computed) slots;
        // this rule's own CF/WF must expand its cone instead — the sched
        // fn is what computes those slots
        let own = f.inst == self.spec.inst && {
            let r = self.rule();
            n == r.can_fire || n == r.will_fire
        };
        if !own {
            if let Some(&slot) = ie.cfwf_slot.get(&n) {
                let word = self.load_word(f, slot);
                return Ok(self.to_w(word, 64, 1, false));
            }
        }
        // schedule-position (eager) defs live in arena slots, but ONLY
        // the rule's own frame may touch them: inlined callee frames
        // must recompute (their instances' owning entries may not have
        // run yet, and C++ method bodies recompute at call time).
        // Within the own frame: exec bodies reload (bsc's def tsort
        // guarantees the owner's schedule position precedes any body
        // alias), and sched fns reload defs owned by strictly earlier
        // entries (spec.shared) — the cone-dedup that keeps shared
        // solver cones from expanding into every rule's IR.
        if f.inst == self.spec.inst {
            if let Some(&(base, w)) = ie.eager_slot.get(&n) {
                let own_eager = self.spec.eager.contains(&n);
                if f.is_exec || (!own_eager && self.spec.shared.contains(&n)) {
                    return Ok(self.load_val(f, base, w));
                }
            }
        }
        if f.expanding.contains(&n) {
            return nope("cyclic def");
        }
        let m = &self.env.d.modules[ie.mir];
        let Some(d) = m.defs.iter().find(|d| d.name == n) else {
            return nope("unknown def");
        };
        let dex = d.expr.clone();
        f.expanding.push(n);
        let v = self.expr(f, &dex)?;
        f.expanding.pop();
        f.ssa.insert(n, v);
        // schedule-position defs are visible to exec bodies via the
        // arena — stored only by the OWNING rule's sched fn (an inlined
        // frame writing call-time values would corrupt them)
        if !f.is_exec && f.inst == self.spec.inst && self.spec.eager.contains(&n) {
            if let Some(&(base, w)) = self.ie(f.inst)?.eager_slot.get(&n) {
                self.store_val(f, base, w, v);
            }
        }
        Ok(v)
    }

    fn prim(
        &mut self,
        f: &mut Frame<'ctx>,
        op: PrimOp,
        width: u32,
        args: &[Expr],
    ) -> Result<IntValue<'ctx>, Ineligible> {
        match op {
            PrimOp::And | PrimOp::Or | PrimOp::Xor | PrimOp::Add | PrimOp::Sub | PrimOp::Mul => {
                let mut it = args.iter();
                let first = it.next().ok_or_else(|| Ineligible("no args".into()))?;
                let w0 = self.expr_width(f, first)?;
                let a0 = self.expr(f, first)?;
                let mut acc = self.to_w(a0, w0, width, false);
                for a in it {
                    let wa = self.expr_width(f, a)?;
                    let v0 = self.expr(f, a)?;
                    let v = self.to_w(v0, wa, width, false);
                    acc = match op {
                        PrimOp::And => self.builder.build_and(acc, v, "and").unwrap(),
                        PrimOp::Or => self.builder.build_or(acc, v, "or").unwrap(),
                        PrimOp::Xor => self.builder.build_xor(acc, v, "xor").unwrap(),
                        PrimOp::Add => self.builder.build_int_add(acc, v, "add").unwrap(),
                        PrimOp::Sub => self.builder.build_int_sub(acc, v, "sub").unwrap(),
                        PrimOp::Mul => self.builder.build_int_mul(acc, v, "mul").unwrap(),
                        _ => unreachable!(),
                    };
                }
                Ok(acc)
            }
            PrimOp::Not => {
                let w0 = self.expr_width(f, &args[0])?;
                let v0 = self.expr(f, &args[0])?;
                let v = self.to_w(v0, w0, width, false);
                Ok(self.builder.build_not(v, "not").unwrap())
            }
            PrimOp::Neg => {
                let w0 = self.expr_width(f, &args[0])?;
                let v0 = self.expr(f, &args[0])?;
                let v = self.to_w(v0, w0, width, false);
                Ok(self.builder.build_int_neg(v, "neg").unwrap())
            }
            PrimOp::Eq | PrimOp::Ult | PrimOp::Ule => {
                let wx = self.expr_width(f, &args[0])?;
                let wy = self.expr_width(f, &args[1])?;
                let wm = wx.max(wy);
                let x0 = self.expr(f, &args[0])?;
                let y0 = self.expr(f, &args[1])?;
                let x = self.to_w(x0, wx, wm, false);
                let y = self.to_w(y0, wy, wm, false);
                let p = match op {
                    PrimOp::Eq => IntPredicate::EQ,
                    PrimOp::Ult => IntPredicate::ULT,
                    _ => IntPredicate::ULE,
                };
                Ok(self.builder.build_int_compare(p, x, y, "uc").unwrap())
            }
            PrimOp::Slt | PrimOp::Sle => {
                let wx = self.expr_width(f, &args[0])?;
                let wy = self.expr_width(f, &args[1])?;
                let wm = wx.max(wy);
                let x0 = self.expr(f, &args[0])?;
                let y0 = self.expr(f, &args[1])?;
                let x = self.to_w(x0, wx, wm, true);
                let y = self.to_w(y0, wy, wm, true);
                let p = if op == PrimOp::Slt { IntPredicate::SLT } else { IntPredicate::SLE };
                Ok(self.builder.build_int_compare(p, x, y, "sc").unwrap())
            }
            PrimOp::Shl | PrimOp::Lshr | PrimOp::Ashr => {
                let ws = self.expr_width(f, &args[0])?;
                if ws != width {
                    return nope("shift result width differs from source");
                }
                let x = self.expr(f, &args[0])?;
                let wa = self.expr_width(f, &args[1])?;
                let s0 = self.expr(f, &args[1])?;
                // compare/clamp the amount in 64 bits, then bring to iW
                let s64 = self.to_w(s0, wa, 64, false);
                let wc = self.ctx.i64_type().const_int(width as u64, false);
                let big = self
                    .builder
                    .build_int_compare(IntPredicate::UGE, s64, wc, "sb")
                    .unwrap();
                match op {
                    PrimOp::Shl | PrimOp::Lshr => {
                        let zero64 = self.ctx.i64_type().const_zero();
                        let samt64 = self
                            .builder
                            .build_select(big, zero64, s64, "sa")
                            .unwrap()
                            .into_int_value();
                        let samt = self.to_w(samt64, 64, width, false);
                        let r = if op == PrimOp::Shl {
                            self.builder.build_left_shift(x, samt, "shl").unwrap()
                        } else {
                            self.builder.build_right_shift(x, samt, false, "lshr").unwrap()
                        };
                        let zero = self.ity(width).const_zero();
                        Ok(self
                            .builder
                            .build_select(big, zero, r, "shz")
                            .unwrap()
                            .into_int_value())
                    }
                    _ => {
                        // ashr: clamp to width-1 — sign-fill for any
                        // amount >= width, matching Value::ashr
                        let maxs = self.ctx.i64_type().const_int((width - 1) as u64, false);
                        let samt64 = self
                            .builder
                            .build_select(big, maxs, s64, "aa")
                            .unwrap()
                            .into_int_value();
                        let samt = self.to_w(samt64, 64, width, false);
                        Ok(self.builder.build_right_shift(x, samt, true, "ashr").unwrap())
                    }
                }
            }
            PrimOp::Extract => {
                // args: [val, hi, lo]; the result width is static, so
                // only lo matters (hi = lo + width - 1); bits beyond the
                // source read as zero (Value::extract)
                let ws = self.expr_width(f, &args[0])?;
                if let (Expr::Const { limbs: hi, .. }, Expr::Const { limbs: lo, .. }) =
                    (&args[1], &args[2])
                {
                    let (hi, lo) =
                        (*hi.first().unwrap_or(&0) as u64, *lo.first().unwrap_or(&0) as u64);
                    if hi < lo || hi - lo + 1 != width as u64 {
                        return nope("extract bounds/width mismatch");
                    }
                    if lo >= ws as u64 {
                        return Ok(self.ity(width).const_zero());
                    }
                    let x = self.expr(f, &args[0])?;
                    let sh = self.ity(ws).const_int(lo, false);
                    let r = self.builder.build_right_shift(x, sh, false, "ex").unwrap();
                    return Ok(self.to_w(r, ws, width, false));
                }
                // dynamic bounds: Value::extract takes bits lo..=hi
                // (source-clamped), i.e. min(hi-lo+1, width) result bits
                let i64t = self.ctx.i64_type();
                let x = self.expr(f, &args[0])?;
                let wh = self.expr_width(f, &args[1])?;
                let hi0 = self.expr(f, &args[1])?;
                let hi64 = self.to_w(hi0, wh, 64, false);
                let wl = self.expr_width(f, &args[2])?;
                let lo0 = self.expr(f, &args[2])?;
                let lo64 = self.to_w(lo0, wl, 64, false);
                // shifted = lo >= ws ? 0 : x >> lo, widened to the result
                let wsc = i64t.const_int(ws as u64, false);
                let big = self
                    .builder
                    .build_int_compare(IntPredicate::UGE, lo64, wsc, "exb")
                    .unwrap();
                let zero64 = i64t.const_zero();
                let samt64 = self
                    .builder
                    .build_select(big, zero64, lo64, "exa")
                    .unwrap()
                    .into_int_value();
                let samt = self.to_w(samt64, 64, ws, false);
                let sh = self.builder.build_right_shift(x, samt, false, "exd").unwrap();
                let zerows = self.ity(ws).const_zero();
                let sh = self
                    .builder
                    .build_select(big, zerows, sh, "exz")
                    .unwrap()
                    .into_int_value();
                let shifted = self.to_w(sh, ws, width, false);
                // mask to min(hi-lo+1, width) bits; hi < lo reads as zero
                let hlt = self
                    .builder
                    .build_int_compare(IntPredicate::ULT, hi64, lo64, "exh")
                    .unwrap();
                let n = self.builder.build_int_sub(hi64, lo64, "exn").unwrap();
                let n = self
                    .builder
                    .build_int_add(n, i64t.const_int(1, false), "exn1")
                    .unwrap();
                let wc = i64t.const_int(width as u64, false);
                let bign = self
                    .builder
                    .build_int_compare(IntPredicate::UGE, n, wc, "exbn")
                    .unwrap();
                let count = self.builder.build_select(bign, wc, n, "exc").unwrap().into_int_value();
                // mask = allones >> (width - count); count >= 1 here
                // (hi >= lo), so the shift amount is < width
                let msh64 = self.builder.build_int_sub(wc, count, "exms").unwrap();
                let msh = self.to_w(msh64, 64, width, false);
                let allones = self.ity(width).const_all_ones();
                let mask = self.builder.build_right_shift(allones, msh, false, "exmk").unwrap();
                let r = self.builder.build_and(shifted, mask, "exr").unwrap();
                let zerow = self.ity(width).const_zero();
                Ok(self
                    .builder
                    .build_select(hlt, zerow, r, "exf")
                    .unwrap()
                    .into_int_value())
            }
            PrimOp::Concat => {
                // left-to-right, first arg highest
                let t = self.ity(width);
                let mut acc = t.const_zero();
                let mut total = 0u32;
                for a in args {
                    let wa = self.expr_width(f, a)?;
                    let v0 = self.expr(f, a)?;
                    let v = self.to_w(v0, wa, width, false);
                    total += wa;
                    if total > width {
                        return nope("concat width overflow");
                    }
                    let sh = t.const_int(wa as u64, false);
                    let shifted = self.builder.build_left_shift(acc, sh, "cc").unwrap();
                    acc = self.builder.build_or(shifted, v, "co").unwrap();
                }
                if total != width {
                    return nope("concat width mismatch");
                }
                Ok(acc)
            }
            PrimOp::ZeroExt => {
                let ws = self.expr_width(f, &args[0])?;
                let v = self.expr(f, &args[0])?;
                Ok(self.to_w(v, ws, width, false))
            }
            PrimOp::SignExt => {
                let ws = self.expr_width(f, &args[0])?;
                let v = self.expr(f, &args[0])?;
                Ok(self.to_w(v, ws, width, true))
            }
            PrimOp::Quot | PrimOp::Rem => {
                // unsigned; zero divisor raises SIGFPE like the
                // interpreter (Value::quot) and native division
                let wx = self.expr_width(f, &args[0])?;
                let wy = self.expr_width(f, &args[1])?;
                let x0 = self.expr(f, &args[0])?;
                let y0 = self.expr(f, &args[1])?;
                let wm = wx.max(wy).max(width);
                let x = self.to_w(x0, wx, wm, false);
                let y = self.to_w(y0, wy, wm, false);
                let z = self
                    .builder
                    .build_int_compare(IntPredicate::EQ, y, self.ity(wm).const_zero(), "dz")
                    .unwrap();
                let func = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let trap_bb = self.ctx.append_basic_block(func, "divz");
                let ok_bb = self.ctx.append_basic_block(func, "divok");
                self.builder.build_conditional_branch(z, trap_bb, ok_bb).unwrap();
                self.builder.position_at_end(trap_bb);
                self.builder.build_call(self.fpe_fn, &[], "fpe").unwrap();
                self.builder.build_unreachable().unwrap();
                self.builder.position_at_end(ok_bb);
                let r = if op == PrimOp::Quot {
                    self.builder.build_int_unsigned_div(x, y, "quot").unwrap()
                } else {
                    self.builder.build_int_unsigned_rem(x, y, "rem").unwrap()
                };
                Ok(self.to_w(r, wm, width, false))
            }
            _ => nope(format!("prim op {op:?} not compilable")),
        }
    }

    /// sched_<label>(arena): cone eval, inhibitors, CF/WF + eager stores.
    fn lower_sched(&mut self) -> Result<(), Ineligible> {
        let r = self.rule().clone();
        let ptrt = self.ctx.ptr_type(AddressSpace::default());
        let fnty = self.ctx.void_type().fn_type(&[ptrt.into(), ptrt.into()], false);
        let func = self.module.add_function(&format!("sched_{}", self.spec.label), fnty, None);
        let bb = self.ctx.append_basic_block(func, "entry");
        self.builder.position_at_end(bb);
        let mut f = Frame {
            arena: func.get_nth_param(0).unwrap().into_pointer_value(),
            envp: Some(func.get_nth_param(1).unwrap().into_pointer_value()),
            inst: self.spec.inst,
            method_idx: None,
            args: HashMap::new(),
            ssa: HashMap::new(),
            expanding: Vec::new(),
            tasks: HashMap::new(),
            is_exec: false,
            depth: 0,
        };

        let mut cf = self.def(&mut f, r.can_fire)?; // i1
        for &slot in &self.spec.inhibit_slots {
            let other = self.load_word(&f, slot);
            let nz = self.nonzero(other, 64);
            let zero = self.ctx.bool_type().const_zero();
            cf = self.builder.build_select(nz, zero, cf, "inh").unwrap().into_int_value();
        }
        let cf64 = self.to_w(cf, 1, 64, false);
        self.store_word(&f, self.spec.cf_slot, cf64);
        // the WF cone reads the (inhibited) latched CF, not the raw cone
        f.ssa.insert(r.can_fire, cf);
        let wf = self.def(&mut f, r.will_fire)?;
        let wf64 = self.to_w(wf, 1, 64, false);
        self.store_word(&f, self.spec.wf_slot, wf64);
        // eager defs the cones did not reach still need their slots
        // stored (later rules' cones or bodies may reload them)
        for &e in &self.spec.eager {
            if !self.ie(self.spec.inst)?.eager_slot.contains_key(&e) {
                return nope("eager def without slot");
            }
            self.def(&mut f, e)?; // def() stores to the slot on compute
        }
        self.builder.build_return(None).unwrap();
        Ok(())
    }

    /// exec_<label>(arena, env) -> i32: WF-gated body execution.
    fn lower_exec(&mut self) -> Result<(), Ineligible> {
        let r = self.rule().clone();
        let ptrt = self.ctx.ptr_type(AddressSpace::default());
        let i32t = self.ctx.i32_type();
        let fnty = i32t.fn_type(&[ptrt.into(), ptrt.into()], false);
        let func = self.module.add_function(&format!("exec_{}", self.spec.label), fnty, None);
        let entry = self.ctx.append_basic_block(func, "entry");
        let body_bb = self.ctx.append_basic_block(func, "body");
        let done_bb = self.ctx.append_basic_block(func, "done");
        let stop_bb = self.ctx.append_basic_block(func, "stop");

        self.builder.position_at_end(entry);
        let mut f = Frame {
            arena: func.get_nth_param(0).unwrap().into_pointer_value(),
            envp: Some(func.get_nth_param(1).unwrap().into_pointer_value()),
            inst: self.spec.inst,
            method_idx: None,
            args: HashMap::new(),
            ssa: HashMap::new(),
            expanding: Vec::new(),
            tasks: HashMap::new(),
            is_exec: true,
            depth: 0,
        };
        let wf = self.load_word(&f, self.spec.wf_slot);
        let fire = self.nonzero(wf, 64);
        self.builder.build_conditional_branch(fire, body_bb, done_bb).unwrap();

        self.builder.position_at_end(body_bb);
        self.stmts(&mut f, func, &r.body, stop_bb)?;
        self.builder.build_unconditional_branch(done_bb).unwrap();

        self.builder.position_at_end(done_bb);
        self.builder.build_return(Some(&i32t.const_int(0, false))).unwrap();
        self.builder.position_at_end(stop_bb);
        self.builder.build_return(Some(&i32t.const_int(1, false))).unwrap();
        Ok(())
    }

    /// Marshal a foreign call site: numeric args as word runs (strings
    /// ride the spec table), call, optionally read back result words.
    /// Returns the result value for tasks (ret_width > 0).
    fn emit_foreign(
        &mut self,
        f: &mut Frame<'ctx>,
        func_id: StrId,
        args: &[Expr],
        signed: &[bool],
        ret_width: u32,
        stop_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> Result<Option<IntValue<'ctx>>, Ineligible> {
        let Some(envp) = f.envp else {
            return nope("foreign call without env pointer");
        };
        let i64t = self.ctx.i64_type();
        let mut spec_args = Vec::new();
        let mut vals = Vec::new();
        for (i, a) in args.iter().enumerate() {
            if let Expr::Str(sid) = a {
                spec_args.push(FArgSpec::Str(*sid));
                continue;
            }
            let wa = self.expr_width(f, a)?;
            let v = self.expr(f, a)?;
            spec_args.push(FArgSpec::Num {
                width: wa,
                signed: signed.get(i).copied().unwrap_or(false),
            });
            vals.push((v, wa));
        }
        let total_words: u32 =
            vals.iter().map(|&(_, w)| words_for(w)).sum::<u32>().max(1);
        let out_words = words_for(ret_width.max(1));
        let abuf = self
            .builder
            .build_array_alloca(i64t, i64t.const_int(total_words as u64, false), "fa")
            .unwrap();
        let obuf = self
            .builder
            .build_array_alloca(i64t, i64t.const_int(out_words as u64, false), "fo")
            .unwrap();
        let mut off = 0u32;
        for (v, wa) in vals {
            for k in 0..words_for(wa) {
                let sh = self.ity(wa).const_int((64 * k) as u64, false);
                let piece = if k == 0 {
                    v
                } else {
                    self.builder.build_right_shift(v, sh, false, "fp").unwrap()
                };
                let word = self.to_w(piece, wa, 64, false);
                let idx = i64t.const_int((off + k) as u64, false);
                let p =
                    unsafe { self.builder.build_gep(i64t, abuf, &[idx], "fap").unwrap() };
                self.builder.build_store(p, word).unwrap();
            }
            off += words_for(wa);
        }
        let token =
            self.spec.token_base | self.token_kind | self.foreign_stmts.len() as u64;
        self.foreign_stmts.push(ForeignSpec {
            inst: f.inst,
            func: func_id,
            ret_width,
            args: spec_args,
        });
        let call = self
            .builder
            .build_call(
                self.cb_fn,
                &[
                    envp.into(),
                    i64t.const_int(token, false).into(),
                    abuf.into(),
                    obuf.into(),
                ],
                "fcb",
            )
            .unwrap();
        let inkwell::values::ValueKind::Basic(rv) = call.try_as_basic_value() else {
            return nope("callback returned void");
        };
        let stop = self
            .builder
            .build_int_compare(
                IntPredicate::NE,
                rv.into_int_value(),
                self.ctx.i32_type().const_int(0, false),
                "fst",
            )
            .unwrap();
        let func = self.builder.get_insert_block().unwrap().get_parent().unwrap();
        let cont_bb = self.ctx.append_basic_block(func, "fcont");
        self.builder.build_conditional_branch(stop, stop_bb, cont_bb).unwrap();
        self.builder.position_at_end(cont_bb);
        if ret_width == 0 {
            return Ok(None);
        }
        let t = self.ity(ret_width);
        let mut acc = t.const_zero();
        for k in 0..words_for(ret_width) {
            let idx = i64t.const_int(k as u64, false);
            let p = unsafe { self.builder.build_gep(i64t, obuf, &[idx], "fop").unwrap() };
            let word = self.builder.build_load(i64t, p, "fol").unwrap().into_int_value();
            if ret_width <= 64 {
                acc = self.to_w(word, 64, ret_width, false);
            } else {
                let wide = self.builder.build_int_z_extend(word, t, "fwz").unwrap();
                let sh = t.const_int((64 * k) as u64, false);
                let pos = self.builder.build_left_shift(wide, sh, "fws").unwrap();
                acc = self.builder.build_or(acc, pos, "fwo").unwrap();
            }
        }
        Ok(Some(acc))
    }

    /// Lower a statement list (rule body or Cond arm); `stop_bb`
    /// receives control when a callback requests stop.
    fn stmts(
        &mut self,
        f: &mut Frame<'ctx>,
        func: FunctionValue<'ctx>,
        list: &[Stmt],
        stop_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> Result<(), Ineligible> {
        for st in list.iter() {
            match st {
                Stmt::Def { name, expr } => {
                    let v = self.expr(f, expr)?;
                    f.ssa.insert(*name, v);
                }
                Stmt::Action(a) => self.action(f, func, a, stop_bb)?,
                Stmt::AvAction { def, action } => match action {
                    Action::Task { func: tf, cookie, temp, width, cond, args, signed } => {
                        let v = self.task_call(
                            f, func, *tf, *cookie, *temp, *width, cond, args, signed,
                            stop_bb,
                        )?;
                        f.ssa.insert(*def, v);
                    }
                    Action::MethCall { instance, method, cond, args, .. } => {
                        // ActionValue method on a prim child (trampoline)
                        let ie = self.ie(f.inst)?;
                        let Some(&child) = ie.children.get(instance) else {
                            return nope("avaction on unknown child");
                        };
                        if self.env.insts.contains_key(&child)
                            || ie.reg_slot.contains_key(instance)
                            || ie.wire_slot.contains_key(instance)
                        {
                            return nope("avaction on non-trampoline instance");
                        }
                        let wd = self.def_width(f.inst, *def).unwrap_or(1);
                        let wc = self.expr_width(f, cond)?;
                        let c = self.expr(f, cond)?;
                        let cz = self.nonzero(c, wc);
                        let go_bb = self.ctx.append_basic_block(func, "avgo");
                        let sk_bb = self.ctx.append_basic_block(func, "avsk");
                        let jn_bb = self.ctx.append_basic_block(func, "avjn");
                        self.builder.build_conditional_branch(cz, go_bb, sk_bb).unwrap();
                        self.builder.position_at_end(go_bb);
                        let v = self
                            .emit_prim_call(f, child, *method, args, wd, true)?
                            .expect("av prim call returns");
                        let g_end = self.builder.get_insert_block().unwrap();
                        self.builder.build_unconditional_branch(jn_bb).unwrap();
                        self.builder.position_at_end(sk_bb);
                        let undet = self.ity(wd).const_zero();
                        let s_end = self.builder.get_insert_block().unwrap();
                        self.builder.build_unconditional_branch(jn_bb).unwrap();
                        self.builder.position_at_end(jn_bb);
                        let phi = self.builder.build_phi(self.ity(wd), "avphi").unwrap();
                        phi.add_incoming(&[(&v, g_end), (&undet, s_end)]);
                        f.ssa.insert(*def, phi.as_basic_value().into_int_value());
                    }
                    _ => return nope("actionvalue kind in body"),
                },
                Stmt::Cond { cond, then_, else_ } => {
                    let wc = self.expr_width(f, cond)?;
                    let c = self.expr(f, cond)?;
                    let cz = self.nonzero(c, wc);
                    let then_bb = self.ctx.append_basic_block(func, "then");
                    let else_bb = self.ctx.append_basic_block(func, "else");
                    let join_bb = self.ctx.append_basic_block(func, "join");
                    self.builder.build_conditional_branch(cz, then_bb, else_bb).unwrap();
                    self.builder.position_at_end(then_bb);
                    self.cond_arm(f, func, then_, stop_bb)?;
                    self.builder.build_unconditional_branch(join_bb).unwrap();
                    self.builder.position_at_end(else_bb);
                    self.cond_arm(f, func, else_, stop_bb)?;
                    self.builder.build_unconditional_branch(join_bb).unwrap();
                    self.builder.position_at_end(join_bb);
                }
            }
        }
        Ok(())
    }

    /// A conditional foreign ActionValue task: call under cond, bind the
    /// cookie/temp (undet-zero when skipped, like a fresh ctx read).
    #[allow(clippy::too_many_arguments)]
    fn task_call(
        &mut self,
        f: &mut Frame<'ctx>,
        func: FunctionValue<'ctx>,
        tf: StrId,
        cookie: u32,
        temp: Option<StrId>,
        width: u32,
        cond: &Expr,
        args: &[Expr],
        signed: &[bool],
        stop_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> Result<IntValue<'ctx>, Ineligible> {
        let w = width.max(1);
        let wc = self.expr_width(f, cond)?;
        let c = self.expr(f, cond)?;
        let cz = self.nonzero(c, wc);
        let go_bb = self.ctx.append_basic_block(func, "tgo");
        let sk_bb = self.ctx.append_basic_block(func, "tsk");
        let jn_bb = self.ctx.append_basic_block(func, "tjn");
        self.builder.build_conditional_branch(cz, go_bb, sk_bb).unwrap();
        self.builder.position_at_end(go_bb);
        let v = self
            .emit_foreign(f, tf, args, signed, w, stop_bb)?
            .expect("task returns");
        let g_end = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(jn_bb).unwrap();
        self.builder.position_at_end(sk_bb);
        let z = self.ity(w).const_zero();
        let s_end = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(jn_bb).unwrap();
        self.builder.position_at_end(jn_bb);
        let phi = self.builder.build_phi(self.ity(w), "tphi").unwrap();
        phi.add_incoming(&[(&v, g_end), (&z, s_end)]);
        let out = phi.as_basic_value().into_int_value();
        f.tasks.insert(cookie, (out, w));
        if let Some(t) = temp {
            f.ssa.insert(t, out);
        }
        Ok(out)
    }

    /// A Cond arm: defs inside arms would leak SSA across basic blocks
    /// where the interpreter would not have computed them — reject (v1).
    fn cond_arm(
        &mut self,
        f: &mut Frame<'ctx>,
        func: FunctionValue<'ctx>,
        list: &[Stmt],
        stop_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> Result<(), Ineligible> {
        for st in list {
            if matches!(st, Stmt::Def { .. } | Stmt::AvAction { .. }) {
                return nope("def inside conditional arm");
            }
        }
        // table defs expanded inside the arm must not leak (dominance)
        let saved = f.ssa.clone();
        let r = self.stmts(f, func, list, stop_bb);
        f.ssa = saved;
        r
    }

    fn action(
        &mut self,
        f: &mut Frame<'ctx>,
        func: FunctionValue<'ctx>,
        a: &Action,
        stop_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> Result<(), Ineligible> {
        match a {
            Action::MethCall { instance, method, port, cond, args } => {
                let ie = self.ie(f.inst)?;
                let mname = self.env.d.strings[*method as usize].clone();
                let wc = self.expr_width(f, cond)?;
                let c = self.expr(f, cond)?;
                let cz = self.nonzero(c, wc);

                if let Some(&(base, rw)) = ie.reg_slot.get(instance) {
                    if !matches!(mname.as_str(), "write" | "set" | "put" | "_write")
                        || args.len() != 1
                    {
                        return nope("non-write register action");
                    }
                    let wv = self.expr_width(f, &args[0])?;
                    let v0 = self.expr(f, &args[0])?;
                    let v = self.to_w(v0, wv, rw, false);
                    let wr_bb = self.ctx.append_basic_block(func, "wr");
                    let sk_bb = self.ctx.append_basic_block(func, "sk");
                    self.builder.build_conditional_branch(cz, wr_bb, sk_bb).unwrap();
                    self.builder.position_at_end(wr_bb);
                    self.store_val(f, base, rw, v);
                    self.builder.build_unconditional_branch(sk_bb).unwrap();
                    self.builder.position_at_end(sk_bb);
                    return Ok(());
                }
                if let Some(&(base, ww)) = ie.wire_slot.get(instance) {
                    if !matches!(mname.as_str(), "wset" | "send") {
                        return nope("non-wset wire action");
                    }
                    let v = if ww >= 1 && !args.is_empty() {
                        let wv = self.expr_width(f, &args[0])?;
                        let v0 = self.expr(f, &args[0])?;
                        Some(self.to_w(v0, wv, ww, false))
                    } else {
                        None
                    };
                    let wr_bb = self.ctx.append_basic_block(func, "wr");
                    let sk_bb = self.ctx.append_basic_block(func, "sk");
                    self.builder.build_conditional_branch(cz, wr_bb, sk_bb).unwrap();
                    self.builder.position_at_end(wr_bb);
                    let one = self.ctx.i64_type().const_int(1, false);
                    self.store_word(f, base, one);
                    if let Some(v) = v {
                        self.store_val(f, base + 1, ww, v);
                    }
                    self.builder.build_unconditional_branch(sk_bb).unwrap();
                    self.builder.position_at_end(sk_bb);
                    return Ok(());
                }

                let Some(&child) = ie.children.get(instance) else {
                    return nope("action on unknown child");
                };
                // other prim children: trampoline under the condition
                if !self.env.insts.contains_key(&child) {
                    let go_bb = self.ctx.append_basic_block(func, "pgo");
                    let sk_bb = self.ctx.append_basic_block(func, "psk");
                    self.builder.build_conditional_branch(cz, go_bb, sk_bb).unwrap();
                    self.builder.position_at_end(go_bb);
                    self.emit_prim_call(f, child, *method, args, 0, true)?;
                    self.builder.build_unconditional_branch(sk_bb).unwrap();
                    self.builder.position_at_end(sk_bb);
                    return Ok(());
                }
                // user-module child: inline the action method body under
                // the call condition, storing EN first (the C++ enable
                // protocol — conflicting rules' WFs read it later)
                if *port != 0 {
                    return nope("multi-ported user action method");
                }
                let cie = self.ie(child)?;
                let cmod = &self.env.d.modules[cie.mir];
                let Some((mi, m)) = cmod
                    .methods
                    .iter()
                    .enumerate()
                    .find(|(_, m)| m.name == *method)
                else {
                    return nope("unknown action method on child");
                };
                if m.kind != trs_ir::MethodKind::Action {
                    return nope("actionvalue method call");
                }
                if m.always_enabled {
                    return nope("always_enabled method (RDY-gated body)");
                }
                if args.len() != m.args.len() {
                    return nope("method arg count mismatch");
                }
                let margs = m.args.clone();
                let body = m.body.clone();
                let en_name = format!("EN_{}", self.env.d.strings[*method as usize]);
                let en_slot = self
                    .env
                    .d
                    .strings
                    .iter()
                    .position(|x| x == &en_name)
                    .and_then(|id| cie.en_slot.get(&(id as StrId)).copied());

                let mut cf = self.child_frame(f, child, Some(mi))?;
                for (a, pa) in args.iter().zip(&margs) {
                    let wa = self.expr_width(f, a)?;
                    let v0 = self.expr(f, a)?;
                    let v = self.to_w(v0, wa, pa.width, false);
                    cf.args.insert(pa.name, (v, pa.width));
                }
                let go_bb = self.ctx.append_basic_block(func, "mgo");
                let sk_bb = self.ctx.append_basic_block(func, "msk");
                self.builder.build_conditional_branch(cz, go_bb, sk_bb).unwrap();
                self.builder.position_at_end(go_bb);
                if let Some(slot) = en_slot {
                    let one = self.ctx.i64_type().const_int(1, false);
                    self.store_word(&cf, slot, one);
                }
                // the inlined body executes inside a conditional block:
                // caller-frame defs expanded here must not leak either —
                // cf is fresh, so only its own scope is at stake
                self.stmts(&mut cf, func, &body, stop_bb)?;
                self.builder.build_unconditional_branch(sk_bb).unwrap();
                self.builder.position_at_end(sk_bb);
                Ok(())
            }
            Action::Foreign { func: ff, cond, args, signed } => {
                let wc = self.expr_width(f, cond)?;
                let c = self.expr(f, cond)?;
                let cz = self.nonzero(c, wc);
                let go_bb = self.ctx.append_basic_block(func, "fgo");
                let sk_bb = self.ctx.append_basic_block(func, "fsk");
                self.builder.build_conditional_branch(cz, go_bb, sk_bb).unwrap();
                self.builder.position_at_end(go_bb);
                self.emit_foreign(f, *ff, args, signed, 0, stop_bb)?;
                self.builder.build_unconditional_branch(sk_bb).unwrap();
                self.builder.position_at_end(sk_bb);
                Ok(())
            }
            Action::Task { func: tf, cookie, temp, width, cond, args, signed } => {
                self.task_call(
                    f, func, *tf, *cookie, *temp, *width, cond, args, signed, stop_bb,
                )?;
                Ok(())
            }
            _ => nope("action kind not compilable"),
        }
    }

}

/// Smoke-level check that LLVM is usable: build `i64 add(i64,i64)`, JIT it,
/// call it.  Exercised by `cargo test -p trs-codegen --features llvm`.
pub fn llvm_smoke_test() -> Result<u64, String> {
    let ctx = Context::create();
    let module = ctx.create_module("trs_smoke");
    let builder = ctx.create_builder();
    let i64t = ctx.i64_type();
    let fnt = i64t.fn_type(&[i64t.into(), i64t.into()], false);
    let f = module.add_function("add", fnt, None);
    let bb = ctx.append_basic_block(f, "entry");
    builder.position_at_end(bb);
    let a = f.get_nth_param(0).unwrap().into_int_value();
    let b = f.get_nth_param(1).unwrap().into_int_value();
    let sum = builder.build_int_add(a, b, "sum").map_err(|e| e.to_string())?;
    builder.build_return(Some(&sum)).map_err(|e| e.to_string())?;
    let ee = module
        .create_jit_execution_engine(OptimizationLevel::Aggressive)
        .map_err(|e| e.to_string())?;
    let add = unsafe { ee.get_function::<unsafe extern "C" fn(u64, u64) -> u64>("add") }
        .map_err(|e| e.to_string())?;
    Ok(unsafe { add.call(40, 2) })
}

#[cfg(test)]
mod tests {
    #[test]
    fn jit_round_trip() {
        assert_eq!(super::llvm_smoke_test().unwrap(), 42);
    }
}
