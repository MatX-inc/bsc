//! BIR -> LLVM IR lowering (feature `llvm`).
//!
//! Hybrid P2 slice (DESIGN.md §10): per-rule native functions running
//! inside the interpreter's event loop, over a shared u64 state arena
//! (plain ≤64-bit registers, reset-port levels, per-rule CF/WF, and
//! schedule-position "eager" defs).  Each eligible rule compiles to
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
//! Values are modeled as i64 with bits above the BSV width kept zero
//! (sign-extended on demand for signed ops); width > 64 anywhere makes
//! the rule ineligible and it stays interpreted.  Ineligibility is an
//! Err from the trial lowering — the caller falls back per composition.

use std::collections::HashMap;

use trs_ir::{Action, Design, Expr, PrimOp, Stmt, StrId};
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::values::{FunctionValue, IntValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};

/// Callback for interpreted statements inside compiled bodies (the
/// $display family): `env` is the interpreter, `token` identifies the
/// statement (see [`CompiledRule::foreign_stmts`]).  Returns nonzero to
/// stop the simulation ($finish was called).
pub type ForeignCb = unsafe extern "C" fn(env: *mut core::ffi::c_void, token: u64) -> i32;

/// Everything the lowering needs to resolve names in one module type,
/// for one instance: arena slots assigned by the interpreter.
pub struct PlanEnv<'a> {
    pub d: &'a Design,
    /// module index in `d.modules`
    pub mir: usize,
    /// local register instance name -> (arena slot, width); only plain
    /// sync/no-reset regs of width ≤ 64 appear here
    pub reg_slot: HashMap<StrId, (u32, u32)>,
    /// module reset input port name -> arena slot holding the PORT level
    /// (1 = deasserted, matching the interpreter's Port read)
    pub reset_slot: HashMap<StrId, u32>,
    /// any rule's CAN_FIRE/WILL_FIRE def name -> arena slot (this
    /// instance); reads of other rules' fire signals become slot loads
    pub cfwf_slot: HashMap<StrId, u32>,
    /// schedule-position def name -> arena slot (this instance): stored
    /// by the sched fn that owns the def, reloaded by exec bodies (the
    /// C++ `DEF_x = DEF_x;` reuse semantics)
    pub eager_slot: HashMap<StrId, u32>,
}

/// One rule to compile.
pub struct RuleSpec {
    pub rule_idx: usize,
    /// arena slots of earlier CAN_FIREs negated into this rule's CF
    /// (intra-module ME inhibitors + cross-module inhibitors)
    pub inhibit_slots: Vec<u32>,
    pub cf_slot: u32,
    pub wf_slot: u32,
    /// defs this rule's Sched entry evaluates at its schedule position
    /// (REntry::eager); each must have an `eager_slot`
    pub eager: Vec<StrId>,
    /// unique function-name label (instance path + rule name)
    pub label: String,
    /// baked into callback tokens: token = base + local foreign-stmt
    /// index (callers use e.g. global_rule_ordinal << 16 so one shared
    /// callback can resolve the rule and the statement)
    pub token_base: u64,
}

/// A compiled rule: raw function pointers into the JIT (kept alive by
/// the owning [`JitEngine`]).
pub struct CompiledRule {
    pub sched: unsafe extern "C" fn(*mut u64),
    pub exec: unsafe extern "C" fn(*mut u64, *mut core::ffi::c_void) -> i32,
    /// token -> statement index path into `rule.body` (Cond arms are
    /// path elements too) for the foreign-statement callback
    pub foreign_stmts: Vec<Vec<u32>>,
}

/// Owns the LLVM context/module/engine the compiled functions live in.
pub struct JitEngine {
    // leaked so the ExecutionEngine (and the returned fn pointers) are
    // 'static; one per loaded design, lives for the process
    _ctx: &'static Context,
    _ee: ExecutionEngine<'static>,
}

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

/// Compile a batch of rules for one (module type, instance) pair.
/// All-or-nothing per call: any ineligible rule fails the whole batch.
pub fn compile_rules(
    env: &PlanEnv,
    specs: &[RuleSpec],
    foreign_cb: ForeignCb,
) -> Result<(JitEngine, Vec<CompiledRule>), Ineligible> {
    let ctx: &'static Context = Box::leak(Box::new(Context::create()));
    let module = ctx.create_module("trs_rules");
    // the interpreter callback for $display-family statements
    let i64t = ctx.i64_type();
    let i32t = ctx.i32_type();
    let ptrt = ctx.ptr_type(AddressSpace::default());
    let cb_ty = i32t.fn_type(&[ptrt.into(), i64t.into()], false);
    let cb_fn = module.add_function("trs_jit_foreign", cb_ty, None);

    let mut protos = Vec::new();
    for spec in specs {
        let mut lc = Lower {
            env,
            ctx,
            module: &module,
            builder: ctx.create_builder(),
            cb_fn,
            spec,
            foreign_stmts: Vec::new(),
        };
        lc.lower_sched()?;
        lc.lower_exec()?;
        protos.push(lc.foreign_stmts);
    }

    if std::env::var_os("TRS_JIT_DUMP").is_some() {
        eprintln!("{}", module.print_to_string().to_string());
    }
    let ee = module
        .create_jit_execution_engine(OptimizationLevel::Less)
        .map_err(|e| Ineligible(format!("LLVM JIT engine: {e}")))?;
    ee.add_global_mapping(&cb_fn, foreign_cb as usize);
    // SAFETY: 'static via the leaked context; the module moved into the EE
    let ee: ExecutionEngine<'static> = unsafe { std::mem::transmute(ee) };

    let mut out = Vec::new();
    for (spec, foreign_stmts) in specs.iter().zip(protos) {
        let sched_addr = ee
            .get_function_address(&format!("sched_{}", spec.label))
            .map_err(|e| Ineligible(format!("sched fn address: {e}")))?;
        let exec_addr = ee
            .get_function_address(&format!("exec_{}", spec.label))
            .map_err(|e| Ineligible(format!("exec fn address: {e}")))?;
        out.push(CompiledRule {
            sched: unsafe { std::mem::transmute::<usize, _>(sched_addr as usize) },
            exec: unsafe { std::mem::transmute::<usize, _>(exec_addr as usize) },
            foreign_stmts,
        });
    }
    Ok((JitEngine { _ctx: ctx, _ee: ee }, out))
}

struct Lower<'a, 'ctx> {
    env: &'a PlanEnv<'a>,
    ctx: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: Builder<'ctx>,
    cb_fn: FunctionValue<'ctx>,
    spec: &'a RuleSpec,
    foreign_stmts: Vec<Vec<u32>>,
}

/// Per-function lowering state: the arena pointer and the SSA maps.
struct Frame<'ctx> {
    arena: PointerValue<'ctx>,
    /// env pointer (exec functions only)
    envp: Option<PointerValue<'ctx>>,
    /// def name -> computed value (cone memo or body locals)
    ssa: HashMap<StrId, IntValue<'ctx>>,
    /// defs currently being expanded (cycle guard)
    expanding: Vec<StrId>,
    /// a compiled register store has executed in this body (body-local
    /// defs are no longer safe to recompute at callback time)
    wrote_reg: bool,
}

impl<'a, 'ctx> Lower<'a, 'ctx> {
    fn mask(&self, w: u32) -> u64 {
        if w >= 64 {
            u64::MAX
        } else {
            (1u64 << w) - 1
        }
    }

    fn rule(&self) -> &trs_ir::Rule {
        &self.env.d.modules[self.env.mir].rules[self.spec.rule_idx]
    }

    fn def_width(&self, name: StrId) -> Result<u32, Ineligible> {
        if self.env.cfwf_slot.contains_key(&name) {
            return Ok(1);
        }
        let m = &self.env.d.modules[self.env.mir];
        match m.defs.iter().find(|d| d.name == name) {
            Some(d) if d.width >= 1 && d.width <= 64 => Ok(d.width),
            Some(d) => nope(format!("def width {} > 64", d.width)),
            None => nope("unknown def"),
        }
    }

    fn expr_width(&self, e: &Expr) -> Result<u32, Ineligible> {
        match e {
            Expr::Def(n) => self.def_width(*n),
            Expr::Port(_) => Ok(1), // only reset ports are eligible
            Expr::Const { width, .. }
            | Expr::MethCall { width, .. }
            | Expr::Prim { width, .. }
            | Expr::If { width, .. }
            | Expr::Case { width, .. } => {
                if *width >= 1 && *width <= 64 {
                    Ok(*width)
                } else {
                    nope(format!("width {width} out of range"))
                }
            }
            _ => nope("expression kind not compilable"),
        }
    }

    fn slot_ptr(&self, f: &Frame<'ctx>, slot: u32) -> PointerValue<'ctx> {
        let i64t = self.ctx.i64_type();
        unsafe {
            self.builder
                .build_gep(i64t, f.arena, &[i64t.const_int(slot as u64, false)], "sp")
                .unwrap()
        }
    }

    fn load_slot(&self, f: &Frame<'ctx>, slot: u32) -> IntValue<'ctx> {
        let p = self.slot_ptr(f, slot);
        self.builder
            .build_load(self.ctx.i64_type(), p, "ld")
            .unwrap()
            .into_int_value()
    }

    fn store_slot(&self, f: &Frame<'ctx>, slot: u32, v: IntValue<'ctx>) {
        let p = self.slot_ptr(f, slot);
        self.builder.build_store(p, v).unwrap();
    }

    fn c64(&self, x: u64) -> IntValue<'ctx> {
        self.ctx.i64_type().const_int(x, false)
    }

    /// Sign-extend a width-w zero-masked i64 value to full i64.
    fn sext(&self, v: IntValue<'ctx>, w: u32) -> IntValue<'ctx> {
        if w >= 64 {
            return v;
        }
        let sh = self.c64(64 - w as u64);
        let l = self.builder.build_left_shift(v, sh, "sxl").unwrap();
        self.builder.build_right_shift(l, sh, true, "sxr").unwrap()
    }

    fn masked(&self, v: IntValue<'ctx>, w: u32) -> IntValue<'ctx> {
        if w >= 64 {
            return v;
        }
        self.builder.build_and(v, self.c64(self.mask(w)), "mk").unwrap()
    }

    /// Lower an expression to a zero-masked i64 value of its BSV width.
    fn expr(&mut self, f: &mut Frame<'ctx>, e: &Expr) -> Result<IntValue<'ctx>, Ineligible> {
        match e {
            Expr::Const { width, limbs } => {
                if *width > 64 {
                    return nope("wide constant");
                }
                let lo = *limbs.first().unwrap_or(&0) as u64;
                let hi = *limbs.get(1).unwrap_or(&0) as u64;
                Ok(self.c64((hi << 32 | lo) & self.mask(*width)))
            }
            Expr::Def(n) => self.def(f, *n),
            Expr::Port(p) => match self.env.reset_slot.get(p) {
                Some(&slot) => Ok(self.load_slot(f, slot)),
                None => nope("non-reset port read"),
            },
            Expr::MethCall { width, instance, method, args, .. } => {
                let (slot, rw) = match self.env.reg_slot.get(instance) {
                    Some(&s) => s,
                    None => return nope("method call on non-arena instance"),
                };
                let mname = &self.env.d.strings[*method as usize];
                if !matches!(mname.as_str(), "read" | "get" | "_read") || !args.is_empty() {
                    return nope("non-read method in expression");
                }
                if rw != *width {
                    return nope("register read width mismatch");
                }
                Ok(self.load_slot(f, slot))
            }
            Expr::If { width, cond, then_, else_ } => {
                // value exprs are pure here (only reg loads): select is safe
                let c = self.expr(f, cond)?;
                let cz = self
                    .builder
                    .build_int_compare(IntPredicate::NE, c, self.c64(0), "c")
                    .unwrap();
                let t = self.expr(f, then_)?;
                let x = self.expr(f, else_)?;
                let r = self.builder.build_select(cz, t, x, "if").unwrap().into_int_value();
                Ok(self.masked(r, *width))
            }
            Expr::Case { width, scrutinee, arms, default } => {
                let s = self.expr(f, scrutinee)?;
                let mut acc = self.expr(f, default)?;
                for (k, arm) in arms.iter().rev() {
                    let hit = self
                        .builder
                        .build_int_compare(IntPredicate::EQ, s, self.c64(*k), "k")
                        .unwrap();
                    let av = self.expr(f, arm)?;
                    acc =
                        self.builder.build_select(hit, av, acc, "cs").unwrap().into_int_value();
                }
                Ok(self.masked(acc, *width))
            }
            Expr::Prim { op, width, args } => self.prim(f, *op, *width, args),
            _ => nope("expression kind not compilable"),
        }
    }

    /// Lower a def reference: body locals / cone memo, then this
    /// instance's fire-signal slots, then eager-def slots (exec bodies
    /// reload the schedule-time value), then table expansion.
    fn def(&mut self, f: &mut Frame<'ctx>, n: StrId) -> Result<IntValue<'ctx>, Ineligible> {
        if let Some(v) = f.ssa.get(&n) {
            return Ok(*v);
        }
        // other rules' fire signals read their (already computed) slots;
        // this rule's own CF/WF must expand its cone instead — the sched
        // fn is what computes those slots
        let own = {
            let r = self.rule();
            n == r.can_fire || n == r.will_fire
        };
        if !own {
            if let Some(&slot) = self.env.cfwf_slot.get(&n) {
                return Ok(self.load_slot(f, slot));
            }
        }
        // exec bodies reuse schedule-time eager values; the sched fn
        // computes them itself (they're its own cone) and stores them
        if f.envp.is_some() {
            if let Some(&slot) = self.env.eager_slot.get(&n) {
                return Ok(self.load_slot(f, slot));
            }
        }
        if f.expanding.contains(&n) {
            return nope("cyclic def");
        }
        let m = &self.env.d.modules[self.env.mir];
        let Some(d) = m.defs.iter().find(|d| d.name == n) else {
            return nope("unknown def");
        };
        let dex = d.expr.clone();
        f.expanding.push(n);
        let v = self.expr(f, &dex)?;
        f.expanding.pop();
        f.ssa.insert(n, v);
        // schedule-position defs are visible to exec bodies via the arena
        if f.envp.is_none() {
            if let Some(&slot) = self.env.eager_slot.get(&n) {
                self.store_slot(f, slot, v);
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
                let mut acc = self.expr(f, first)?;
                for a in it {
                    let v = self.expr(f, a)?;
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
                Ok(self.masked(acc, width))
            }
            PrimOp::Not => {
                let v = self.expr(f, &args[0])?;
                let r = self.builder.build_xor(v, self.c64(self.mask(width)), "not").unwrap();
                Ok(self.masked(r, width))
            }
            PrimOp::Neg => {
                let v = self.expr(f, &args[0])?;
                let r = self.builder.build_int_sub(self.c64(0), v, "neg").unwrap();
                Ok(self.masked(r, width))
            }
            PrimOp::Eq => {
                let x = self.expr(f, &args[0])?;
                let y = self.expr(f, &args[1])?;
                let c = self.builder.build_int_compare(IntPredicate::EQ, x, y, "eq").unwrap();
                Ok(self.builder.build_int_z_extend(c, self.ctx.i64_type(), "z").unwrap())
            }
            PrimOp::Ult | PrimOp::Ule => {
                let x = self.expr(f, &args[0])?;
                let y = self.expr(f, &args[1])?;
                let p = if op == PrimOp::Ult { IntPredicate::ULT } else { IntPredicate::ULE };
                let c = self.builder.build_int_compare(p, x, y, "uc").unwrap();
                Ok(self.builder.build_int_z_extend(c, self.ctx.i64_type(), "z").unwrap())
            }
            PrimOp::Slt | PrimOp::Sle => {
                let wx = self.expr_width(&args[0])?;
                let wy = self.expr_width(&args[1])?;
                let x0 = self.expr(f, &args[0])?;
                let y0 = self.expr(f, &args[1])?;
                let x = self.sext(x0, wx);
                let y = self.sext(y0, wy);
                let p = if op == PrimOp::Slt { IntPredicate::SLT } else { IntPredicate::SLE };
                let c = self.builder.build_int_compare(p, x, y, "sc").unwrap();
                Ok(self.builder.build_int_z_extend(c, self.ctx.i64_type(), "z").unwrap())
            }
            PrimOp::Shl => {
                // Value::shl: shift >= result width -> 0
                let x = self.expr(f, &args[0])?;
                let s = self.expr(f, &args[1])?;
                let big = self
                    .builder
                    .build_int_compare(IntPredicate::UGE, s, self.c64(width as u64), "sb")
                    .unwrap();
                let sh =
                    self.builder.build_select(big, self.c64(0), s, "sa").unwrap().into_int_value();
                let r = self.builder.build_left_shift(x, sh, "shl").unwrap();
                let r = self.masked(r, width);
                Ok(self.builder.build_select(big, self.c64(0), r, "shr").unwrap().into_int_value())
            }
            PrimOp::Lshr => {
                // Value::lshr: bits beyond the source width read as 0
                let ws = self.expr_width(&args[0])?;
                let x = self.expr(f, &args[0])?;
                let s = self.expr(f, &args[1])?;
                let big = self
                    .builder
                    .build_int_compare(IntPredicate::UGE, s, self.c64(ws as u64), "lb")
                    .unwrap();
                let sh =
                    self.builder.build_select(big, self.c64(0), s, "la").unwrap().into_int_value();
                let r = self.builder.build_right_shift(x, sh, false, "lshr").unwrap();
                let r =
                    self.builder.build_select(big, self.c64(0), r, "lr").unwrap().into_int_value();
                Ok(self.masked(r, width))
            }
            PrimOp::Ashr => {
                // sign-fill from the source's sign bit; the clamp keeps
                // LLVM shift amounts < 64 (see Value::ashr)
                let ws = self.expr_width(&args[0])?;
                let x0 = self.expr(f, &args[0])?;
                let s = self.expr(f, &args[1])?;
                let x = self.sext(x0, ws);
                let big = self
                    .builder
                    .build_int_compare(IntPredicate::UGT, s, self.c64(63), "ab")
                    .unwrap();
                let sh =
                    self.builder.build_select(big, self.c64(63), s, "aa").unwrap().into_int_value();
                let r = self.builder.build_right_shift(x, sh, true, "ashr").unwrap();
                Ok(self.masked(r, width))
            }
            PrimOp::Extract => {
                // args: [val, hi, lo] with constant hi/lo
                let (Expr::Const { limbs: hi, .. }, Expr::Const { limbs: lo, .. }) =
                    (&args[1], &args[2])
                else {
                    return nope("dynamic extract");
                };
                let (hi, lo) =
                    (*hi.first().unwrap_or(&0) as u64, *lo.first().unwrap_or(&0) as u64);
                if hi < lo || hi - lo + 1 != width as u64 {
                    return nope("extract bounds/width mismatch");
                }
                let x = self.expr(f, &args[0])?;
                let r = self.builder.build_right_shift(x, self.c64(lo), false, "ex").unwrap();
                Ok(self.masked(r, width))
            }
            PrimOp::Concat => {
                // left-to-right, first arg highest
                let mut acc = self.c64(0);
                let mut total = 0u32;
                for a in args {
                    let w = self.expr_width(a)?;
                    let v = self.expr(f, a)?;
                    total += w;
                    if total > 64 {
                        return nope("wide concat");
                    }
                    let sh = self.builder.build_left_shift(acc, self.c64(w as u64), "cc").unwrap();
                    acc = self.builder.build_or(sh, v, "co").unwrap();
                }
                if total != width {
                    return nope("concat width mismatch");
                }
                Ok(acc)
            }
            PrimOp::ZeroExt => {
                let v = self.expr(f, &args[0])?;
                Ok(v) // already zero-masked
            }
            PrimOp::SignExt => {
                let ws = self.expr_width(&args[0])?;
                let v = self.expr(f, &args[0])?;
                let r = self.sext(v, ws);
                Ok(self.masked(r, width))
            }
            _ => nope(format!("prim op {op:?} not compilable")),
        }
    }

    /// sched_<label>(arena): cone eval, inhibitors, CF/WF + eager stores.
    fn lower_sched(&mut self) -> Result<(), Ineligible> {
        let r = self.rule().clone();
        let ptrt = self.ctx.ptr_type(AddressSpace::default());
        let fnty = self.ctx.void_type().fn_type(&[ptrt.into()], false);
        let func = self.module.add_function(&format!("sched_{}", self.spec.label), fnty, None);
        let bb = self.ctx.append_basic_block(func, "entry");
        self.builder.position_at_end(bb);
        let mut f = Frame {
            arena: func.get_nth_param(0).unwrap().into_pointer_value(),
            envp: None,
            ssa: HashMap::new(),
            expanding: Vec::new(),
            wrote_reg: false,
        };

        let mut cf = self.def(&mut f, r.can_fire)?;
        for &slot in &self.spec.inhibit_slots {
            let other = self.load_slot(&f, slot);
            let nz = self
                .builder
                .build_int_compare(IntPredicate::NE, other, self.c64(0), "inz")
                .unwrap();
            let zero = self.c64(0);
            cf = self.builder.build_select(nz, zero, cf, "inh").unwrap().into_int_value();
        }
        self.store_slot(&f, self.spec.cf_slot, cf);
        // the WF cone reads the (inhibited) latched CF, not the raw cone
        f.ssa.insert(r.can_fire, cf);
        let wf = self.def(&mut f, r.will_fire)?;
        self.store_slot(&f, self.spec.wf_slot, wf);
        // eager defs the cones did not reach still need their slots
        // stored (later rules' cones or bodies may reload them)
        for &e in &self.spec.eager {
            if !self.env.eager_slot.contains_key(&e) {
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
            ssa: HashMap::new(),
            expanding: Vec::new(),
            wrote_reg: false,
        };
        let wf = self.load_slot(&f, self.spec.wf_slot);
        let fire = self
            .builder
            .build_int_compare(IntPredicate::NE, wf, self.c64(0), "fire")
            .unwrap();
        self.builder.build_conditional_branch(fire, body_bb, done_bb).unwrap();

        self.builder.position_at_end(body_bb);
        self.stmts(&mut f, func, &r.body, &mut Vec::new(), stop_bb)?;
        self.builder.build_unconditional_branch(done_bb).unwrap();

        self.builder.position_at_end(done_bb);
        self.builder.build_return(Some(&i32t.const_int(0, false))).unwrap();
        self.builder.position_at_end(stop_bb);
        self.builder.build_return(Some(&i32t.const_int(1, false))).unwrap();
        Ok(())
    }

    /// Lower a statement list (rule body or Cond arm).  `path` is the
    /// index path for foreign-statement tokens; `stop_bb` receives
    /// control when a callback requests stop.
    fn stmts(
        &mut self,
        f: &mut Frame<'ctx>,
        func: FunctionValue<'ctx>,
        list: &[Stmt],
        path: &mut Vec<u32>,
        stop_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> Result<(), Ineligible> {
        for (i, st) in list.iter().enumerate() {
            path.push(i as u32);
            match st {
                Stmt::Def { name, expr } => {
                    let v = self.expr(f, expr)?;
                    f.ssa.insert(*name, v);
                }
                Stmt::Action(a) => self.action(f, func, a, path, stop_bb)?,
                Stmt::AvAction { .. } => return nope("actionvalue in body"),
                Stmt::Cond { cond, then_, else_ } => {
                    let c = self.expr(f, cond)?;
                    let cz = self
                        .builder
                        .build_int_compare(IntPredicate::NE, c, self.c64(0), "cc")
                        .unwrap();
                    let then_bb = self.ctx.append_basic_block(func, "then");
                    let else_bb = self.ctx.append_basic_block(func, "else");
                    let join_bb = self.ctx.append_basic_block(func, "join");
                    self.builder.build_conditional_branch(cz, then_bb, else_bb).unwrap();
                    self.builder.position_at_end(then_bb);
                    path.push(0);
                    self.cond_arm(f, func, then_, path, stop_bb)?;
                    path.pop();
                    self.builder.build_unconditional_branch(join_bb).unwrap();
                    self.builder.position_at_end(else_bb);
                    path.push(1);
                    self.cond_arm(f, func, else_, path, stop_bb)?;
                    path.pop();
                    self.builder.build_unconditional_branch(join_bb).unwrap();
                    self.builder.position_at_end(join_bb);
                }
            }
            path.pop();
        }
        Ok(())
    }

    /// A Cond arm: defs inside arms would leak SSA across basic blocks
    /// where the interpreter would not have computed them — reject (v1).
    fn cond_arm(
        &mut self,
        f: &mut Frame<'ctx>,
        func: FunctionValue<'ctx>,
        list: &[Stmt],
        path: &mut Vec<u32>,
        stop_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> Result<(), Ineligible> {
        for st in list {
            if matches!(st, Stmt::Def { .. } | Stmt::AvAction { .. }) {
                return nope("def inside conditional arm");
            }
        }
        self.stmts(f, func, list, path, stop_bb)
    }

    fn action(
        &mut self,
        f: &mut Frame<'ctx>,
        func: FunctionValue<'ctx>,
        a: &Action,
        path: &mut Vec<u32>,
        stop_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> Result<(), Ineligible> {
        match a {
            Action::MethCall { instance, method, cond, args, .. } => {
                let (slot, rw) = match self.env.reg_slot.get(instance) {
                    Some(&s) => s,
                    None => return nope("action on non-arena instance"),
                };
                let mname = &self.env.d.strings[*method as usize];
                if !matches!(mname.as_str(), "write" | "set" | "put" | "_write")
                    || args.len() != 1
                {
                    return nope("non-write action method");
                }
                let c = self.expr(f, cond)?;
                let v0 = self.expr(f, &args[0])?;
                let v = self.masked(v0, rw);
                let cz = self
                    .builder
                    .build_int_compare(IntPredicate::NE, c, self.c64(0), "wc")
                    .unwrap();
                let wr_bb = self.ctx.append_basic_block(func, "wr");
                let sk_bb = self.ctx.append_basic_block(func, "sk");
                self.builder.build_conditional_branch(cz, wr_bb, sk_bb).unwrap();
                self.builder.position_at_end(wr_bb);
                self.store_slot(f, slot, v);
                f.wrote_reg = true;
                self.builder.build_unconditional_branch(sk_bb).unwrap();
                self.builder.position_at_end(sk_bb);
                Ok(())
            }
            Action::Foreign { cond, args, .. } => {
                // interpreted via callback; the arguments must be
                // re-evaluable at callback time from non-local state
                for a in args {
                    self.arg_safe(f, a)?;
                }
                let c = self.expr(f, cond)?;
                let cz = self
                    .builder
                    .build_int_compare(IntPredicate::NE, c, self.c64(0), "fc")
                    .unwrap();
                let go_bb = self.ctx.append_basic_block(func, "fgo");
                let sk_bb = self.ctx.append_basic_block(func, "fsk");
                self.builder.build_conditional_branch(cz, go_bb, sk_bb).unwrap();
                self.builder.position_at_end(go_bb);
                let token = self.spec.token_base + self.foreign_stmts.len() as u64;
                self.foreign_stmts.push(path.clone());
                let call = self
                    .builder
                    .build_call(
                        self.cb_fn,
                        &[f.envp.unwrap().into(), self.c64(token).into()],
                        "cb",
                    )
                    .unwrap();
                let inkwell::values::ValueKind::Basic(rv) = call.try_as_basic_value() else {
                    return nope("callback returned void");
                };
                let ret = rv.into_int_value();
                let stop = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        ret,
                        self.ctx.i32_type().const_int(0, false),
                        "st",
                    )
                    .unwrap();
                self.builder.build_conditional_branch(stop, stop_bb, sk_bb).unwrap();
                self.builder.position_at_end(sk_bb);
                Ok(())
            }
            _ => nope("action kind not compilable"),
        }
    }

    /// A foreign-statement argument is safe iff the interpreter can
    /// recompute it at callback time: constants, strings, reset ports,
    /// arena register reads, and table defs built from those.  Body
    /// locals and schedule-position (eager) defs are not visible to the
    /// callback's fresh context.
    fn arg_safe(&self, f: &Frame<'ctx>, e: &Expr) -> Result<(), Ineligible> {
        match e {
            Expr::Const { .. } | Expr::Str(_) => Ok(()),
            Expr::Port(p) => {
                if self.env.reset_slot.contains_key(p) {
                    Ok(())
                } else {
                    nope("foreign arg reads non-reset port")
                }
            }
            Expr::MethCall { instance, args, .. } => {
                if !self.env.reg_slot.contains_key(instance) {
                    return nope("foreign arg calls non-arena instance");
                }
                for a in args {
                    self.arg_safe(f, a)?;
                }
                Ok(())
            }
            Expr::Def(n) => {
                // schedule-position values aren't reconstructible from a
                // fresh context; body locals are — via the def table —
                // until a compiled store has changed the state under them
                if self.env.eager_slot.contains_key(n) {
                    return nope("foreign arg reads eager def");
                }
                if f.ssa.contains_key(n) && f.wrote_reg {
                    return nope("foreign arg reads body-local after reg write");
                }
                let m = &self.env.d.modules[self.env.mir];
                match m.defs.iter().find(|d| d.name == *n) {
                    Some(d) => self.arg_safe(f, &d.expr),
                    None => nope("foreign arg reads unknown def"),
                }
            }
            Expr::Prim { args, .. } => {
                for a in args {
                    self.arg_safe(f, a)?;
                }
                Ok(())
            }
            Expr::If { cond, then_, else_, .. } => {
                self.arg_safe(f, cond)?;
                self.arg_safe(f, then_)?;
                self.arg_safe(f, else_)
            }
            Expr::Case { scrutinee, arms, default, .. } => {
                self.arg_safe(f, scrutinee)?;
                for (_, a) in arms {
                    self.arg_safe(f, a)?;
                }
                self.arg_safe(f, default)
            }
            _ => nope("foreign arg kind"),
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
