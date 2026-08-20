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
//!       back into the interpreter (`ForeignCb`).  A nonzero callback
//!       return unwinds immediately (reserved for genuine aborts —
//!       NEVER $finish/$stop, which complete the edge; the runtime
//!       loops stop at the slice boundary).  Returns nonzero iff
//!       aborted.
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
use inkwell::types::{FunctionType, IntType};
use inkwell::values::{FunctionValue, GlobalValue, IntValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};

pub use crate::abi::*;






fn nope<T>(why: impl Into<String>) -> Result<T, Ineligible> {
    Err(Ineligible(why.into()))
}

/// Variant name for ineligibility notes: the catch-alls must say WHICH
/// expression kind they refused, or the sweep's why= column lumps every
/// unlowered variant into one unactionable bucket.
fn expr_kind(e: &Expr) -> &'static str {
    match e {
        Expr::Const { .. } => "Const",
        Expr::Def(..) => "Def",
        Expr::Port(..) => "Port",
        Expr::Param(..) => "Param",
        Expr::MethCall { .. } => "MethCall",
        Expr::MethValue { .. } => "MethValue",
        Expr::TaskValue { .. } => "TaskValue",
        Expr::ForeignCall { .. } => "ForeignCall",
        Expr::Str(..) => "Str",
        Expr::Clock { .. } => "Clock",
        Expr::Real(..) => "Real",
        Expr::Reset { .. } => "Reset",
        Expr::Gate { .. } => "Gate",
        Expr::Prim { .. } => "Prim",
        Expr::If { .. } => "If",
        Expr::Case { .. } => "Case",
    }
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
/// Returns each rule's call-site tables.
pub fn trial_lower(env: &PlanEnv, specs: &[RuleSpec]) -> Result<Vec<FnProtos>, Ineligible> {
    let ctx = Context::create();
    let (module, cbs) = make_module(&ctx, None);
    let mut protos = Vec::with_capacity(specs.len());
    for spec in specs {
        let mut lc = Lower {
            env,
            ctx: &ctx,
            module: &module,
            builder: ctx.create_builder(),
            cbs,
            spec,
            token_kind: 0,
            outlined: None,
            helper_self: None,
            dedup: None,
            foreign_stmts: Vec::new(),
            prim_calls: Vec::new(),
            edge: None,
        };
        lc.lower_sched()?;
        let sched_foreign = std::mem::take(&mut lc.foreign_stmts);
        let sched_prims = std::mem::take(&mut lc.prim_calls);
        lc.token_kind = TOKEN_KIND_EXEC;
        lc.lower_exec()?;
        protos.push(FnProtos {
            sched_foreign,
            sched_prims,
            exec_foreign: lc.foreign_stmts,
            exec_prims: lc.prim_calls,
        });
    }
    Ok(protos)
}

/// How compiled code reaches the runtime callbacks: the JIT bakes the
/// addresses as constant pointers; AOT objects (and the trial
/// lowering) load them from named pointer-globals the loader fills
/// after dlopen — no --export-dynamic on the host binary.
#[derive(Clone, Copy)]
enum CbAddr<'ctx> {
    Baked(PointerValue<'ctx>),
    Global(GlobalValue<'ctx>),
}

#[derive(Clone, Copy)]
struct Callbacks<'ctx> {
    cb_ty: FunctionType<'ctx>,
    fpe_ty: FunctionType<'ctx>,
    prim_ty: FunctionType<'ctx>,
    cb: CbAddr<'ctx>,
    fpe: CbAddr<'ctx>,
    prim: CbAddr<'ctx>,
}

fn make_module<'ctx>(
    ctx: &'ctx Context,
    baked: Option<(ForeignCb, SigfpeCb, PrimCb)>,
) -> (Module<'ctx>, Callbacks<'ctx>) {
    let module = ctx.create_module("trs_rules");
    let i64t = ctx.i64_type();
    let i32t = ctx.i32_type();
    let ptrt = ctx.ptr_type(AddressSpace::default());
    let cb_ty =
        i32t.fn_type(&[ptrt.into(), i64t.into(), ptrt.into(), ptrt.into()], false);
    let fpe_ty = ctx.void_type().fn_type(&[], false);
    let prim_ty = ctx
        .void_type()
        .fn_type(&[ptrt.into(), i64t.into(), ptrt.into(), ptrt.into()], false);
    let (cb, fpe, prim) = match baked {
        Some((f, s, p)) => {
            let addr = |a: usize| {
                CbAddr::Baked(i64t.const_int(a as u64, false).const_to_pointer(ptrt))
            };
            (addr(f as usize), addr(s as usize), addr(p as usize))
        }
        None => {
            // declaration only (no initializer): every chunk object
            // references these; the meta object DEFINES them once
            let global = |name: &str| CbAddr::Global(module.add_global(ptrt, None, name));
            (
                global("trs_cb_foreign"),
                global("trs_cb_sigfpe"),
                global("trs_cb_prim"),
            )
        }
    };
    (module, Callbacks { cb_ty, fpe_ty, prim_ty, cb, fpe, prim })
}

/// Widest integer type the default middle-end pipeline accepts; wider
/// modules skip it (backend codegen still runs).  65536 is a measured
/// >90s wedge; 4096 keeps every realistic datapath optimized.
const IR_PASS_WIDTH_CAP: u32 = 4096;

/// Max integer bit-width appearing as an instruction result type.
/// Wide values only exist by being ASSEMBLED (zext/shl/or chains from
/// arena slots), so result types are a complete witness.
fn module_max_int_width(module: &Module) -> u32 {
    let mut w = 0;
    let mut f = module.get_first_function();
    while let Some(func) = f {
        for bb in func.get_basic_blocks() {
            let mut ins = bb.get_first_instruction();
            while let Some(i) = ins {
                if let inkwell::types::AnyTypeEnum::IntType(t) = i.get_type() {
                    w = w.max(t.get_bit_width());
                }
                ins = i.get_next_instruction();
            }
        }
        f = func.get_next_function();
    }
    w
}

/// Run the LLVM middle-end pipeline on a module when TRS_JIT_OPT
/// asks for optimization.  The engine/object paths only apply BACKEND
/// codegen opts; without this the IR pass pipeline (GVN, instcombine,
/// SimplifyCFG, jump threading) never runs at all.
fn run_ir_passes(module: &Module) -> Result<(), Ineligible> {
    // mirror opt_level(): the AOT default is O1 even when the env var
    // is unset (this silently skipping was why one-module emission
    // showed zero inlining)
    let lvl = match std::env::var("TRS_JIT_OPT").as_deref() {
        Ok("1") => 1,
        Ok("2") => 2,
        Ok("3") => 3,
        Ok(_) => return Ok(()),
        Err(_) if AOT_MODE.with(|m| m.get()) => {
            // width cap on the DEFAULT pipeline only: LLVM's known-bits
            // reasoning is quadratic in integer width, and one i65536
            // body wedges default<O1> for minutes (sysInit65536Bit AOT
            // link timeout).  An explicit TRS_JIT_OPT still forces
            // the pipeline.
            if module_max_int_width(module) > IR_PASS_WIDTH_CAP {
                return Ok(());
            }
            // O3 default (measured on the edge-SSA + outline-model
            // IR: ~22% run for +1s link vs O1; reference ships -O3)
            3
        }
        Err(_) => return Ok(()),
    };
    let tm = aot_target_machine()?;
    let opts = inkwell::passes::PassBuilderOptions::create();
    if std::env::var_os("TRS_JIT_NOVEC").is_some() {
        opts.set_loop_vectorization(false);
        opts.set_loop_slp_vectorization(false);
    }
    // debugging escape: run an arbitrary pipeline string instead
    // (miscompile bisection — e.g. "default<O1>,gvn")
    let pipeline = std::env::var("TRS_JIT_PIPELINE")
        .unwrap_or_else(|_| format!("default<O{lvl}>"));
    module
        .run_passes(&pipeline, &tm, opts)
        .map_err(|e| Ineligible(format!("IR passes: {e}")))
}

fn finish_engine(
    module: Module<'static>,
) -> Result<inkwell::execution_engine::ExecutionEngine<'static>, Ineligible> {
    if std::env::var_os("TRS_JIT_DUMP").is_some() {
        eprintln!("{}", module.print_to_string().to_string());
    }
    run_ir_passes(&module)?;
    let opt = opt_level();
    let ee = module
        .create_jit_execution_engine(opt)
        .map_err(|e| Ineligible(format!("LLVM JIT engine: {e}")))?;
    Ok(ee)
}

/// Compile the SCHED functions for a batch of rules (eager: they run
/// on every edge).  All-or-nothing per call.
pub fn compile_scheds(
    env: &PlanEnv,
    specs: &[RuleSpec],
    outlined: Option<&HelperMap>,
    foreign_cb: ForeignCb,
    sigfpe_cb: SigfpeCb,
    prim_cb: PrimCb,
) -> Result<Vec<CompiledSched>, Ineligible> {
    let ctx: &'static Context = Box::leak(Box::new(Context::create()));
    let (module, cbs) = make_module(ctx, Some((foreign_cb, sigfpe_cb, prim_cb)));
    let mut protos = Vec::new();
    for spec in specs {
        let mut lc = Lower {
            env,
            ctx,
            module: &module,
            builder: ctx.create_builder(),
            cbs,
            spec,
            token_kind: 0,
            outlined: None,
            helper_self: None,
            dedup: None,
            foreign_stmts: Vec::new(),
            prim_calls: Vec::new(),
            edge: None,
        };
        lc.lower_sched()?;
        protos.push((lc.foreign_stmts, lc.prim_calls));
    }
    let ee = finish_engine(module)?;
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
    outlined: Option<&HelperMap>,
    foreign_cb: ForeignCb,
    sigfpe_cb: SigfpeCb,
    prim_cb: PrimCb,
) -> Result<Vec<CompiledExec>, Ineligible> {
    let ctx: &'static Context = Box::leak(Box::new(Context::create()));
    let (module, cbs) = make_module(ctx, Some((foreign_cb, sigfpe_cb, prim_cb)));
    let mut protos = Vec::new();
    for spec in specs {
        let mut lc = Lower {
            env,
            ctx,
            module: &module,
            builder: ctx.create_builder(),
            cbs,
            spec,
            token_kind: TOKEN_KIND_EXEC,
            outlined: None,
            helper_self: None,
            dedup: None,
            foreign_stmts: Vec::new(),
            prim_calls: Vec::new(),
            edge: None,
        };
        lc.lower_exec()?;
        protos.push((lc.foreign_stmts, lc.prim_calls));
    }
    let ee = finish_engine(module)?;
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

/// Default is -O0 (DESIGN.md §6: iterate-run starts fast; -O0 halves
/// LLVM time and costs ~4% sim speed on compute-bound loops);
/// TRS_JIT_OPT=1/2/3 raises it for both JIT and AOT emission.
fn opt_level() -> OptimizationLevel {
    match std::env::var("TRS_JIT_OPT").as_deref() {
        Ok("0") => OptimizationLevel::None,
        Ok("1") => OptimizationLevel::Less,
        Ok("2") => OptimizationLevel::Default,
        Ok("3") => OptimizationLevel::Aggressive,
        // ARTIFACTS default to O1: the measured ladder (O0 2.74s /
        // O1 1.78s / O2 1.82s / O3 1.83s run; links 5.9/7.6/8.5/7.9)
        // shows O1 captures the whole win on this workload.  The JIT
        // keeps O0 for compile latency.
        _ if AOT_MODE.with(|m| m.get()) => OptimizationLevel::Less,
        _ => OptimizationLevel::None,
    }
}



/// A gate expression a compiled read may re-expand: static cones only
/// (constants, parameters, and pure combinationals over them).  Defs
/// are excluded because the interp's gate eval prefers their LATCHED
/// values; prim-state chases (Gate, method reads, flattened
/// $CLK_GATE_OUT ports) are excluded pending schedule-time gate slots.
fn gate_static(e: &Expr) -> bool {
    match e {
        Expr::Const { .. } | Expr::Param(_) => true,
        Expr::Prim { args, .. } => args.iter().all(gate_static),
        Expr::If { cond, then_, else_, .. } => {
            gate_static(cond) && gate_static(then_) && gate_static(else_)
        }
        Expr::Case { scrutinee, arms, default, .. } => {
            gate_static(scrutinee)
                && arms.iter().all(|(_, a)| gate_static(a))
                && gate_static(default)
        }
        _ => false,
    }
}


fn aot_target_machine() -> Result<inkwell::targets::TargetMachine, Ineligible> {
    use inkwell::targets::{CodeModel, RelocMode, Target, TargetMachine};
    llvm_init_once();
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple)
        .map_err(|e| Ineligible(format!("LLVM target: {e}")))?;
    target
        .create_target_machine(
            &triple,
            &TargetMachine::get_host_cpu_name().to_string(),
            &TargetMachine::get_host_cpu_features().to_string(),
            opt_level(),
            RelocMode::PIC,
            CodeModel::Default,
        )
        .ok_or_else(|| Ineligible("LLVM target machine creation failed".into()))
}

/// AOT: lower a batch (sched + exec per rule, callbacks through
/// pointer-globals) and emit one PIC object file for the artifact .so.
pub fn compile_object_chunk(
    env: &PlanEnv,
    specs: &[RuleSpec],
    outlined: Option<&HelperMap>,
    do_sched: bool,
    do_exec: bool,
) -> Result<Vec<u8>, Ineligible> {
    let ctx = Context::create();
    let (module, cbs) = make_module(&ctx, None);
    for spec in specs {
        let mut lc = Lower {
            env,
            ctx: &ctx,
            module: &module,
            builder: ctx.create_builder(),
            cbs,
            spec,
            token_kind: 0,
            outlined: None,
            helper_self: None,
            dedup: None,
            foreign_stmts: Vec::new(),
            prim_calls: Vec::new(),
            edge: None,
        };
        if do_sched {
            lc.lower_sched()?;
        }
        // reset the call-site tables between the two functions: token
        // local indices are per-function (must match trial_lower)
        lc.foreign_stmts = Vec::new();
        lc.prim_calls = Vec::new();
        lc.token_kind = TOKEN_KIND_EXEC;
        lc.dedup = None;
        if do_exec {
            lc.lower_exec()?;
        }
    }
    if std::env::var_os("TRS_JIT_DUMP").is_some() {
        eprintln!("{}", module.print_to_string().to_string());
    }
    run_ir_passes(&module)?;
    let tm = aot_target_machine()?;
    let buf = tm
        .write_to_memory_buffer(&module, inkwell::targets::FileType::Object)
        .map_err(|e| Ineligible(format!("object emit: {e}")))?;
    Ok(buf.as_slice().to_vec())
}

/// AOT: the fingerprint object.  The loader checks these globals before
/// trusting the artifact's baked slot numbers.
pub fn compile_meta_object(
    bir_hash: u64,
    bir_hash_raw: u64,
    split_thresh: u64,
    protos: &[u8],
    edge_wire_ticks: bool,
    bdpi_names: &[String],
    snap: &[u8],
    plan_a: &[u8],
    plan_b: &[u8],
) -> Result<Vec<u8>, Ineligible> {
    let ctx = Context::create();
    let module = ctx.create_module("trs_meta");
    let i64t = ctx.i64_type();
    let ptrt = ctx.ptr_type(AddressSpace::default());
    let h = module.add_global(i64t, None, "trs_bir_hash");
    h.set_initializer(&i64t.const_int(bir_hash, false));
    // the UNSALTED design identity: trace-independent consumers (the
    // embedded snap, the baked PlanA) gate on this — trs_bir_hash is
    // trace-salted and belongs to aot_load's mode check
    let hr = module.add_global(i64t, None, "trs_bir_hash_raw");
    hr.set_initializer(&i64t.const_int(bir_hash_raw, false));
    let r = module.add_global(i64t, None, "trs_layout_rev");
    r.set_initializer(&i64t.const_int(AOT_LAYOUT_REV, false));
    // split threshold changes the arena layout (memo slots): the
    // loader must plan with the SAME value or refuse the artifact
    let t = module.add_global(i64t, None, "trs_split_thresh");
    t.set_initializer(&i64t.const_int(split_thresh, false));
    // edge fns contain the compiled wire ticks: the loader skips the
    // interp tick loop's covered entries iff this is set (absent in
    // old artifacts -> loader reads 0)
    let wt = module.add_global(i64t, None, "trs_edge_wire_ticks");
    wt.set_initializer(&i64t.const_int(edge_wire_ticks as u64, false));
    // single definition of the callback pointer-globals every chunk
    // object references; the loader fills them after dlopen
    for name in ["trs_cb_foreign", "trs_cb_sigfpe", "trs_cb_prim", "trs_cb_stdio"] {
        let g = module.add_global(ptrt, None, name);
        g.set_initializer(&ptrt.const_null());
    }
    // direct-BDPI callee pointers, filled by the loader from the
    // artifact's companion .bdpi.so
    for n in bdpi_names {
        let g = module.add_global(ptrt, None, &format!("trs_bdpi_{n}"));
        g.set_initializer(&ptrt.const_null());
    }
    // per-ordinal call-site tables: loading decodes these instead of
    // re-running trial_lower
    let pl = module.add_global(i64t, None, "trs_protos_len");
    pl.set_initializer(&i64t.const_int(protos.len() as u64, false));
    let arr = ctx.const_string(protos, false);
    let pg = module.add_global(arr.get_type(), None, "trs_protos");
    pg.set_initializer(&arr);
    // the design snapshot rides INSIDE the artifact: a --code run
    // decodes the design from here and never opens the .bir (full-AOT
    // load doctrine: the .bir is the debug/link sidecar, not a
    // runtime dependency).  len 0 = not embedded (encode failed);
    // the loader falls back to the .bir path.
    let sl = module.add_global(i64t, None, "trs_snap_len");
    sl.set_initializer(&i64t.const_int(snap.len() as u64, false));
    let sarr = ctx.const_string(snap, false);
    let sg = module.add_global(sarr.get_type(), None, "trs_snap");
    sg.set_initializer(&sarr);
    // prime()'s derived schedule (see trs-interp PlanA): a --code run
    // decodes this instead of re-deriving
    let al = module.add_global(i64t, None, "trs_plan_a_len");
    al.set_initializer(&i64t.const_int(plan_a.len() as u64, false));
    let aarr = ctx.const_string(plan_a, false);
    let ag = module.add_global(aarr.get_type(), None, "trs_plan_a");
    ag.set_initializer(&aarr);
    // jit_plan's derived specs + dedup classes (see trs-interp PlanB):
    // slot numbers depend on trace mode, so the loader gates this on
    // the SALTED hash (trs_bir_hash), unlike the snap and PlanA
    let bl = module.add_global(i64t, None, "trs_plan_b_len");
    bl.set_initializer(&i64t.const_int(plan_b.len() as u64, false));
    let barr = ctx.const_string(plan_b, false);
    let bg = module.add_global(barr.get_type(), None, "trs_plan_b");
    bg.set_initializer(&barr);
    let tm = aot_target_machine()?;
    let buf = tm
        .write_to_memory_buffer(&module, inkwell::targets::FileType::Object)
        .map_err(|e| Ineligible(format!("meta object emit: {e}")))?;
    Ok(buf.as_slice().to_vec())
}


/// Lower a batch of helper functions into one module.  Same-batch
/// helpers call each other by symbol (module-local); the HelperMap may
/// also carry cross-references.  Returns nothing extra: callers either
/// finish a JIT engine or emit an object from the module.
fn lower_helpers<'ctx>(
    env: &PlanEnv,
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    cbs: Callbacks<'ctx>,
    specs: &[HelperSpec],
    refs: &HelperMap,
    pseudo: &RuleSpec,
) -> Result<(), Ineligible> {
    for hs in specs {
        let mut lc = Lower {
            env,
            ctx,
            module,
            builder: ctx.create_builder(),
            cbs,
            spec: pseudo,
            token_kind: TOKEN_KIND_EXEC,
            outlined: Some(refs),
            helper_self: Some((hs.mir, hs.def)),
            dedup: None,
            foreign_stmts: Vec::new(),
            prim_calls: Vec::new(),
            edge: None,
        };
        lc.lower_helper(hs).map_err(|e| {
            Ineligible(format!("{} (def {}): {e}", hs.sym, hs.def))
        })?;
        if !lc.foreign_stmts.is_empty() || !lc.prim_calls.is_empty() {
            return Err(Ineligible(format!(
                "helper piece has callback sites (analysis bug): {}",
                hs.sym
            )));
        }
    }
    Ok(())
}

/// AOT single-module emission (whole-edge inlining): lower the whole
/// design — helpers, scheds, exec class reps, fused edges — into ONE
/// module and run the pipeline, so the inliner can flatten cheap
/// calls into the fused edge (what g++'s single TU gives the C++
/// backend).  Larger bodies stay as calls by the inliner's own cost
/// model.
#[allow(clippy::too_many_arguments)]
pub fn compile_design_object(
    env: &PlanEnv,
    specs: &[RuleSpec],
    rep_ords: &[usize],
    helper_specs: &[HelperSpec],
    refs: &HelperMap,
    fused: &[FusedComp],
    edge_plan: Option<&EdgeSsaPlan>,
) -> Result<Vec<u8>, Ineligible> {
    let t_low = std::time::Instant::now();
    let ctx = Context::create();
    let (module, cbs) = make_module(&ctx, None);
    if !helper_specs.is_empty() {
        lower_helpers(env, &ctx, &module, cbs, helper_specs, refs, &specs[0])?;
    }
    let refs_opt = (!refs.is_empty()).then_some(refs);
    // rules covered by an SSA edge function need no standalone
    // sched/exec symbols (the loader stubs them): emitting them only
    // duplicated every body and doubled the LLVM mass
    // sched coverage: a rule whose SCHED node lowers inline in an edge
    // fn needs no standalone sched symbol (outlining is exec-only —
    // outlined rules' scheds still inline)
    let covered: std::collections::HashSet<usize> = edge_plan
        .map(|p| {
            p.nodes
                .iter()
                .flatten()
                .filter(|&&(is_exec, _)| !is_exec)
                .map(|&(_, o)| o)
                .collect()
        })
        .unwrap_or_default();
    for (o, spec) in specs.iter().enumerate() {
        if covered.contains(&o) {
            continue;
        }
        let mut lc = Lower {
            env,
            ctx: &ctx,
            module: &module,
            builder: ctx.create_builder(),
            cbs,
            spec,
            token_kind: 0,
            outlined: refs_opt,
            helper_self: None,
            dedup: None,
            foreign_stmts: Vec::new(),
            prim_calls: Vec::new(),
            edge: None,
        };
        lc.lower_sched()?;
    }
    // rep_ords arrives pre-filtered: a class rep is emitted iff some
    // member's composition is NOT edge-SSA covered (the caller owns
    // class membership; `covered` above only filters sched fns)
    for &o in rep_ords {
        let spec = &specs[o];
        let mut lc = Lower {
            env,
            ctx: &ctx,
            module: &module,
            builder: ctx.create_builder(),
            cbs,
            spec,
            token_kind: TOKEN_KIND_EXEC,
            outlined: refs_opt,
            helper_self: None,
            dedup: None,
            foreign_stmts: Vec::new(),
            prim_calls: Vec::new(),
            edge: None,
        };
        lc.lower_exec()?;
    }
    match edge_plan {
        Some(p) => {
            lower_edge_ssa(env, &ctx, &module, cbs, specs, refs_opt, p, fused)?
        }
        None => {
            let _ = lower_fused(&ctx, &module, fused);
        }
    }
    // ordinal-indexed fn tables: without them the loader dlsyms ~one
    // symbol per rule (ms-scale on rule-heavy designs).  Null entry =
    // symbol elided (edge-SSA covered scheds, non-rep exec members) —
    // the loader stubs or skips those.  Chunked artifacts carry no
    // tables (functions span objects) and keep the per-symbol path.
    {
        let ptrt = ctx.ptr_type(AddressSpace::default());
        let i64t = ctx.i64_type();
        let fnptr = |name: String| {
            module
                .get_function(&name)
                .map(|f| f.as_global_value().as_pointer_value())
                .unwrap_or_else(|| ptrt.const_null())
        };
        let scheds: Vec<_> = specs
            .iter()
            .map(|sp| fnptr(format!("sched_{}", sp.label)))
            .collect();
        let execs: Vec<_> = specs
            .iter()
            .map(|sp| fnptr(format!("exec_{}", sp.label)))
            .collect();
        let edges: Vec<_> =
            (0..fused.len()).map(|k| fnptr(format!("edge_c{k}"))).collect();
        for (name, vals) in [
            ("trs_sched_tab", scheds),
            ("trs_exec_tab", execs),
            ("trs_edge_tab", edges),
        ] {
            let arr = ptrt.const_array(&vals);
            let g = module.add_global(arr.get_type(), None, name);
            g.set_initializer(&arr);
            let l = module.add_global(i64t, None, &format!("{name}_len"));
            l.set_initializer(&i64t.const_int(vals.len() as u64, false));
        }
    }
    let timing = std::env::var_os("TRS_JIT_TIME").is_some();
    if timing {
        eprintln!("trs aot: lowering {:?}", t_low.elapsed());
        // per-function-group IR census: emitted size is the
        // load-immune proxy that predicts O3 cost; the exec/hlp vs
        // edge split is the per-type-precompilation ceiling
        let mut groups: [(&str, u64, u64); 5] = [
            ("edge", 0, 0),
            ("exec", 0, 0),
            ("hlp", 0, 0),
            ("sched", 0, 0),
            ("other", 0, 0),
        ];
        let mut top: Vec<(u64, String)> = Vec::new();
        let mut f = module.get_first_function();
        while let Some(func) = f {
            let name = func.get_name().to_string_lossy().into_owned();
            let mut blocks = 0u64;
            let mut insts = 0u64;
            for bb in func.get_basic_blocks() {
                blocks += 1;
                let mut cur = bb.get_first_instruction();
                while let Some(i) = cur {
                    insts += 1;
                    cur = i.get_next_instruction();
                }
            }
            let gi = if name.starts_with("edge_") {
                0
            } else if name.starts_with("exec_") {
                1
            } else if name.starts_with("hlp_") {
                2
            } else if name.starts_with("sched_") {
                3
            } else {
                4
            };
            groups[gi].1 += blocks;
            groups[gi].2 += insts;
            if insts > 0 {
                top.push((insts, name));
            }
            f = func.get_next_function();
        }
        let census: Vec<String> = groups
            .iter()
            .filter(|g| g.2 > 0)
            .map(|(n, b, i)| format!("{n}={i}insn/{b}bb"))
            .collect();
        eprintln!("trs aot: ir census {}", census.join(" "));
        top.sort_unstable_by(|a, b| b.0.cmp(&a.0).then(a.1.cmp(&b.1)));
        let tops: Vec<String> =
            top.iter().take(5).map(|(i, n)| format!("{n}={i}")).collect();
        eprintln!("trs aot: ir top {}", tops.join(" "));
    }
    let t0 = std::time::Instant::now();
    if std::env::var_os("TRS_JIT_DUMP_PRE").is_some() {
        eprintln!("{}", module.print_to_string().to_string());
    }
    run_ir_passes(&module)?;
    if std::env::var_os("TRS_JIT_DUMP_POST").is_some() {
        eprintln!("{}", module.print_to_string().to_string());
    }
    let t1 = std::time::Instant::now();
    if timing {
        eprintln!("trs aot: ir passes {:?}", t1 - t0);
    }
    let tm = aot_target_machine()?;
    let buf = tm
        .write_to_memory_buffer(&module, inkwell::targets::FileType::Object)
        .map_err(|e| Ineligible(format!("design object emit: {e}")))?;
    if timing {
        eprintln!("trs aot: backend emit {:?}", t1.elapsed());
    }
    Ok(buf.as_slice().to_vec())
}

/// JIT: compile a helper batch into one engine; returns (sym, addr).
/// Same-batch helpers call each other by module-local symbol.
pub fn compile_helpers(
    env: &PlanEnv,
    specs: &[HelperSpec],
    refs: &HelperMap,
    pseudo: &RuleSpec,
) -> Result<Vec<(String, usize)>, Ineligible> {
    let ctx: &'static Context = Box::leak(Box::new(Context::create()));
    let (module, cbs) = make_module(ctx, None);
    lower_helpers(env, ctx, &module, cbs, specs, refs, pseudo)?;
    if std::env::var_os("TRS_JIT_DUMP").is_some() {
        eprintln!("{}", module.print_to_string().to_string());
    }
    let ee = finish_engine(module)?;
    let mut out = Vec::with_capacity(specs.len());
    for hs in specs {
        let addr = ee
            .get_function_address(&hs.sym)
            .map_err(|e| Ineligible(format!("helper fn address: {e}")))?;
        out.push((hs.sym.clone(), addr as usize));
    }
    std::mem::forget(ee);
    Ok(out)
}

/// AOT: emit the helper batch as one PIC object (symbols resolve at
/// artifact link time).
pub fn compile_helpers_object(
    env: &PlanEnv,
    specs: &[HelperSpec],
    refs: &HelperMap,
    pseudo: &RuleSpec,
) -> Result<Vec<u8>, Ineligible> {
    let ctx = Context::create();
    let (module, cbs) = make_module(&ctx, None);
    lower_helpers(env, &ctx, &module, cbs, specs, refs, pseudo)?;
    run_ir_passes(&module)?;
    let tm = aot_target_machine()?;
    let buf = tm
        .write_to_memory_buffer(&module, inkwell::targets::FileType::Object)
        .map_err(|e| Ineligible(format!("helper object emit: {e}")))?;
    Ok(buf.as_slice().to_vec())
}


fn lower_fused<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    comps: &[FusedComp],
) -> Vec<String> {
    let i64t = ctx.i64_type();
    let i32t = ctx.i32_type();
    let ptrt = ctx.ptr_type(AddressSpace::default());
    let sched_ty = ctx.void_type().fn_type(&[ptrt.into(), ptrt.into()], false);
    let exec_ty = i32t
        .fn_type(&[ptrt.into(), ptrt.into(), i64t.into(), i64t.into()], false);
    let b = ctx.create_builder();
    let mut syms = Vec::with_capacity(comps.len());
    for (k, comp) in comps.iter().enumerate() {
        let sym = format!("edge_c{k}");
        let fnty = i32t.fn_type(&[ptrt.into(), ptrt.into(), i64t.into()], false);
        let func = module.add_function(&sym, fnty, None);
        let entry = ctx.append_basic_block(func, "entry");
        b.position_at_end(entry);
        let arena = func.get_nth_param(0).unwrap().into_pointer_value();
        let envp = func.get_nth_param(1).unwrap().into_pointer_value();
        let now = func.get_nth_param(2).unwrap().into_int_value();
        // now stamp + EN zeroing, inline
        let gep = |slot: u32| unsafe {
            b.build_gep(i64t, arena, &[i64t.const_int(slot as u64, false)], "s")
                .unwrap()
        };
        b.build_store(gep(comp.now_slot), now).unwrap();
        for &en in &comp.en_slots {
            b.build_store(gep(en), i64t.const_zero()).unwrap();
        }
        let callee = |r: &HelperRef, ty: inkwell::types::FunctionType<'ctx>| match r {
            HelperRef::Addr(a) => (
                None,
                Some(
                    i64t.const_int(*a as u64, false).const_to_pointer(ptrt),
                ),
                ty,
            ),
            HelperRef::Sym(name) => (
                Some(
                    module
                        .get_function(name)
                        .unwrap_or_else(|| module.add_function(name, ty, None)),
                ),
                None,
                ty,
            ),
        };
        let mut stop_bbs = Vec::new();
        for n in &comp.nodes {
            match n {
                FusedNode::Sched(r) => {
                    let (f_, p_, ty) = callee(r, sched_ty);
                    match (f_, p_) {
                        (Some(f_), _) => {
                            b.build_call(f_, &[arena.into(), envp.into()], "s")
                                .unwrap();
                        }
                        (_, Some(p_)) => {
                            b.build_indirect_call(
                                ty,
                                p_,
                                &[arena.into(), envp.into()],
                                "s",
                            )
                            .unwrap();
                        }
                        _ => unreachable!(),
                    }
                }
                FusedNode::Exec(r, base, tok) => {
                    let (f_, p_, ty) = callee(r, exec_ty);
                    let args: Vec<inkwell::values::BasicMetadataValueEnum> = vec![
                        arena.into(),
                        envp.into(),
                        i64t.const_int(*base, false).into(),
                        i64t.const_int(*tok, false).into(),
                    ];
                    let cs = match (f_, p_) {
                        (Some(f_), _) => b.build_call(f_, &args, "e").unwrap(),
                        (_, Some(p_)) => {
                            b.build_indirect_call(ty, p_, &args, "e").unwrap()
                        }
                        _ => unreachable!(),
                    };
                    let inkwell::values::ValueKind::Basic(rv) = cs.try_as_basic_value()
                    else {
                        unreachable!()
                    };
                    let stop = b
                        .build_int_compare(
                            IntPredicate::NE,
                            rv.into_int_value(),
                            i32t.const_zero(),
                            "st",
                        )
                        .unwrap();
                    let cont = ctx.append_basic_block(func, "c");
                    let halt = ctx.append_basic_block(func, "h");
                    b.build_conditional_branch(stop, halt, cont).unwrap();
                    stop_bbs.push(halt);
                    b.position_at_end(cont);
                }
            }
        }
        b.build_return(Some(&i32t.const_zero())).unwrap();
        for h in stop_bbs {
            b.position_at_end(h);
            b.build_return(Some(&i32t.const_int(1, false))).unwrap();
        }
        syms.push(sym);
    }
    syms
}

/// Whole-edge SSA emission (task #24 M2): one edge_c<k> per
/// composition with every sched/exec section lowered INLINE, sharing
/// an EdgeCtx value cache across sections — latched CF/WF/eager values
/// replace slot loads, and hoisted pure shared defs replace cross-rule
/// cone recomputation (evicted on intervening writes).  Same symbols
/// and signature as lower_fused, so the loader and runtime are
/// untouched.  Every existing slot STORE is preserved (interp/debug
/// contract).
#[allow(clippy::too_many_arguments)]
fn lower_edge_ssa<'ctx>(
    env: &PlanEnv,
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    cbs: Callbacks<'ctx>,
    specs: &[RuleSpec],
    outlined: Option<&HelperMap>,
    plan: &EdgeSsaPlan,
    fused: &[FusedComp],
) -> Result<(), Ineligible> {
    let i64t = ctx.i64_type();
    let i32t = ctx.i32_type();
    let ptrt = ctx.ptr_type(AddressSpace::default());
    let writes: Vec<std::collections::HashSet<usize>> = plan
        .exec_writes
        .iter()
        .map(|v| v.iter().copied().collect())
        .collect();
    for (k, comp) in fused.iter().enumerate() {
        let sym = format!("edge_c{k}");
        let fnty = i32t.fn_type(&[ptrt.into(), ptrt.into(), i64t.into()], false);
        let func = module.add_function(&sym, fnty, None);
        let entry = ctx.append_basic_block(func, "entry");
        let stop_bb = ctx.append_basic_block(func, "stop");
        let b = ctx.create_builder();
        b.position_at_end(entry);
        let arena = func.get_nth_param(0).unwrap().into_pointer_value();
        let envp = func.get_nth_param(1).unwrap().into_pointer_value();
        let now = func.get_nth_param(2).unwrap().into_int_value();
        let gep = |slot: u32| unsafe {
            b.build_gep(i64t, arena, &[i64t.const_int(slot as u64, false)], "s")
                .unwrap()
        };
        b.build_store(gep(comp.now_slot), now).unwrap();
        for &en in &comp.en_slots {
            b.build_store(gep(en), i64t.const_zero()).unwrap();
        }

        let mut edge_ctx = EdgeCtx {
            exports: plan.export_slots.clone(),
            ..Default::default()
        };
        let mut cur = entry;
        for (s, &(is_exec, o)) in plan.nodes[k].iter().enumerate() {
            let spec = &specs[o];
            let mut lc = Lower {
                env,
                ctx,
                module,
                builder: ctx.create_builder(),
                cbs,
                spec,
                token_kind: if is_exec { TOKEN_KIND_EXEC } else { 0 },
                outlined,
                helper_self: None,
                dedup: None,
                foreign_stmts: Vec::new(),
                prim_calls: Vec::new(),
                edge: Some(std::mem::take(&mut edge_ctx)),
            };
            lc.builder.position_at_end(cur);
            // hoist prelude: shared pure defs whose first consumer is
            // this section, computed unconditionally on the spine (so
            // the values dominate every later section)
            for &(hi, hd) in &plan.hoists[k][s] {
                let mut hf = Frame {
                    arena,
                    envp: Some(envp),
                    inst: hi,
                    method_idx: None,
                    args: HashMap::new(),
                    ssa: HashMap::new(),
                    expanding: Vec::new(),
                    thunks: HashMap::new(),
                    av_widths: HashMap::new(),
            dead_defs: Default::default(),
                    tasks: HashMap::new(),
                    av_slots: HashMap::new(),
                    av_args: HashMap::new(),
                    is_exec: true,
                    depth: 0,
                };
                let v = lc.def(&mut hf, hd)?;
                lc.edge.as_mut().unwrap().shared.insert((hi, hd), v);
            }
            let mut f = Frame {
                arena,
                envp: Some(envp),
                inst: spec.inst,
                method_idx: None,
                args: HashMap::new(),
                ssa: HashMap::new(),
                expanding: Vec::new(),
                thunks: HashMap::new(),
                av_widths: HashMap::new(),
            dead_defs: Default::default(),
                tasks: HashMap::new(),
                av_slots: HashMap::new(),
                av_args: HashMap::new(),
                is_exec,
                depth: 0,
            };
            if is_exec {
                // PRE-evict (review-fleet critical finding): a rule
                // whose own actions invalidate a shared def must not
                // consume the pre-body value — tsort body-position
                // semantics.  The plan already refuses to HOIST at
                // self-killing sections; this refuses CONSUMPTION of
                // earlier anchors too.
                {
                    let ws = &writes[o];
                    lc.edge.as_mut().unwrap().shared.retain(|key, _| {
                        plan.def_reads
                            .get(key)
                            .is_some_and(|rs| rs.iter().all(|gi| !ws.contains(gi)))
                    });
                }
                if plan.outlined_execs.contains(&o) {
                    // outline dial: call the standalone class body (it
                    // gates itself on the stored WF slot; stores are
                    // all kept) — bounds the mega-function while the
                    // body keeps its per-module-type dedup
                    let FusedNode::Exec(href, base, tok) = &fused[k].nodes[s] else {
                        return Err(Ineligible(
                            "outlined exec node mismatch".into(),
                        ));
                    };
                    let exec_ty = i32t.fn_type(
                        &[ptrt.into(), ptrt.into(), i64t.into(), i64t.into()],
                        false,
                    );
                    let args: Vec<inkwell::values::BasicMetadataValueEnum> = vec![
                        arena.into(),
                        envp.into(),
                        i64t.const_int(*base, false).into(),
                        i64t.const_int(*tok, false).into(),
                    ];
                    let cs = match href {
                        HelperRef::Sym(name) => {
                            let cf = module.get_function(name).unwrap_or_else(|| {
                                module.add_function(name, exec_ty, None)
                            });
                            lc.builder.build_call(cf, &args, "oe").unwrap()
                        }
                        HelperRef::Addr(a) => {
                            let fp = i64t
                                .const_int(*a as u64, false)
                                .const_to_pointer(ptrt);
                            lc.builder
                                .build_indirect_call(exec_ty, fp, &args, "oe")
                                .unwrap()
                        }
                    };
                    let inkwell::values::ValueKind::Basic(rv) = cs.try_as_basic_value()
                    else {
                        return Err(Ineligible("outlined exec returned void".into()));
                    };
                    let stop = lc
                        .builder
                        .build_int_compare(
                            IntPredicate::NE,
                            rv.into_int_value(),
                            i32t.const_zero(),
                            "st",
                        )
                        .unwrap();
                    let cont = ctx.append_basic_block(func, "oc");
                    lc.builder
                        .build_conditional_branch(stop, stop_bb, cont)
                        .unwrap();
                    lc.builder.position_at_end(cont);
                } else {
                    lc.exec_section(&mut f, func, stop_bb)?;
                }
                // evict shares whose cone the body may have invalidated
                let ws = &writes[o];
                lc.edge.as_mut().unwrap().shared.retain(|key, _| {
                    plan.def_reads
                        .get(key)
                        .is_some_and(|rs| rs.iter().all(|gi| !ws.contains(gi)))
                });
            } else {
                lc.sched_section(&mut f)?;
            }
            cur = lc.builder.get_insert_block().unwrap();
            edge_ctx = lc.edge.take().unwrap();
        }
        let bend = ctx.create_builder();
        bend.position_at_end(cur);
        // compiled wire ticks: end-of-edge valid-bit clears
        if let Some(clears) = plan.wire_clears.get(k) {
            for &slot in clears {
                let gepw = unsafe {
                    bend.build_gep(
                        i64t,
                        arena,
                        &[i64t.const_int(slot as u64, false)],
                        "wc",
                    )
                    .unwrap()
                };
                bend.build_store(gepw, i64t.const_zero()).unwrap();
            }
        }
        bend.build_return(Some(&i32t.const_int(0, false))).unwrap();
        bend.position_at_end(stop_bb);
        bend.build_return(Some(&i32t.const_int(1, false))).unwrap();
    }
    Ok(())
}

/// JIT: compile fused edge functions (baked callee addresses) into
/// one engine; returns per-comp fn addresses.
pub fn compile_fused(
    comps: &[FusedComp],
) -> Result<Vec<usize>, Ineligible> {
    llvm_init_once();
    let ctx: &'static Context = Box::leak(Box::new(Context::create()));
    let module = ctx.create_module("trs_fused");
    let syms = lower_fused(ctx, &module, comps);
    if std::env::var_os("TRS_JIT_DUMP").is_some() {
        eprintln!("{}", module.print_to_string().to_string());
    }
    let ee = finish_engine(module)?;
    let mut out = Vec::with_capacity(syms.len());
    for sym in &syms {
        let a = ee
            .get_function_address(sym)
            .map_err(|e| Ineligible(format!("fused fn address: {e}")))?;
        out.push(a as usize);
    }
    std::mem::forget(ee);
    Ok(out)
}

/// AOT: emit the fused edge functions as one PIC object (symbol
/// callees resolve at artifact link).
pub fn compile_fused_object(comps: &[FusedComp]) -> Result<Vec<u8>, Ineligible> {
    let ctx = Context::create();
    let module = ctx.create_module("trs_fused");
    let _ = lower_fused(&ctx, &module, comps);
    run_ir_passes(&module)?;
    let tm = aot_target_machine()?;
    let buf = tm
        .write_to_memory_buffer(&module, inkwell::targets::FileType::Object)
        .map_err(|e| Ineligible(format!("fused object emit: {e}")))?;
    Ok(buf.as_slice().to_vec())
}

/// Cross-section value cache for whole-edge SSA emission (task #24).
/// Lives for one composition's edge function; sections hand it forward.
/// Insertions happen ONLY at points that dominate every later section
/// (section top level / the driver's hoist prelude) — never from
/// inside def() recursion, so arm-local values can never leak.
#[derive(Default)]
struct EdgeCtx<'ctx> {
    /// slots whose stores must SURVIVE export elision: CF slots read
    /// by inhibitor loads, WF/eager slots read by outlined bodies.
    /// Everything else is dead weight in the specialized compile
    /// (Ravi: speed first — the slot-level debug contract is not part
    /// of the edge-SSA artifact surface).
    exports: std::collections::HashSet<u32>,
    /// position-latched values: CF/WF and eager defs at their compute
    /// position — what the arena slots hold.  NEVER evicted (eviction
    /// would change latched semantics, not just performance).
    latched: HashMap<(usize, StrId), IntValue<'ctx>>,
    /// speculative cross-rule shares (unslotted body defs, hoisted by
    /// the driver); the driver evicts after any exec section whose
    /// write-set intersects the def's unstable-read set.
    shared: HashMap<(usize, StrId), IntValue<'ctx>>,
}

struct Lower<'a, 'ctx> {
    env: &'a PlanEnv<'a>,
    ctx: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: Builder<'ctx>,
    cbs: Callbacks<'ctx>,
    spec: &'a RuleSpec,
    /// whole-edge SSA cache; None outside edge-function emission.
    /// Owned by the section's Lower and handed back to the driver.
    edge: Option<EdgeCtx<'ctx>>,
    /// OR'd into callback tokens (TOKEN_KIND_EXEC for body passes)
    token_kind: u64,
    /// outlined def pieces callable from this lowering (None while the
    /// helper set itself is being compiled bottom-up)
    outlined: Option<&'a HelperMap>,
    /// the piece being lowered right now (its own def must expand
    /// inline, not self-call)
    helper_self: Option<(usize, StrId)>,
    /// exec dedup mode: (subtree region of spec.inst, base param,
    /// token-base param).  In-region slots address as base + (slot -
    /// region.0); call-site tokens OR the runtime token base.  None =
    /// baked absolute addressing (sched fns, trial).
    dedup: Option<(u32, u32, IntValue<'ctx>, IntValue<'ctx>)>,
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
    /// defs whose positioned binding lived inside a conditional arm
    /// and died at the join: post-arm references must NOT re-expand
    /// the def table (the stmt's positioned expr may differ) — they
    /// make the design ineligible instead
    dead_defs: std::collections::HashSet<StrId>,
    /// ActionValue task results by cookie (Expr::TaskValue reads):
    /// (value, width)
    tasks: HashMap<u32, (IntValue<'ctx>, u32)>,
    /// method arg values latched at inline AV call sites, gated on the
    /// call condition (select(cond, v, 0) — the interp's per-edge
    /// latched-if-called-else-0), keyed by (child instance name, arg
    /// port name).  Expr::MethValue result cones read them.
    av_args: HashMap<(StrId, StrId), (IntValue<'ctx>, u32)>,
    /// join-safe ActionValue task results (value alloca, width), keyed
    /// by the bound def/temp name and by the cookie's synthetic key:
    /// the interp's ctx.locals persists past a Cond join, so a task
    /// executed inside an arm is readable after it — the alloca
    /// initializes to the undet pattern (what a never-run arm's read
    /// yields) and stores at task execution
    av_slots: HashMap<StrId, (PointerValue<'ctx>, u32)>,
    /// effectful-eval def memos (value alloca, valid-flag alloca):
    /// evaluate at FIRST dynamic reference this invocation, reuse
    /// after — even across Cond joins where the ssa binding dies
    thunks: HashMap<StrId, (PointerValue<'ctx>, PointerValue<'ctx>)>,
    /// widths of SYNTHETIC ActionValue result defs (in no def table
    /// — expr_width would otherwise fail on a direct reference)
    av_widths: HashMap<StrId, u32>,
    /// exec-body scope (reloads eager slots) vs sched scope (stores them)
    is_exec: bool,
    /// inline depth (method-call recursion guard)
    depth: u32,
}

impl<'a, 'ctx> Lower<'a, 'ctx> {
    /// Callable pointer for a runtime callback: the baked constant, or
    /// a load from the named global (AOT; filled by the loader).
    fn cb_callee(&self, a: CbAddr<'ctx>) -> PointerValue<'ctx> {
        match a {
            CbAddr::Baked(p) => p,
            CbAddr::Global(g) => self
                .builder
                .build_load(
                    self.ctx.ptr_type(AddressSpace::default()),
                    g.as_pointer_value(),
                    "cbp",
                )
                .unwrap()
                .into_pointer_value(),
        }
    }

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
            Some(_) => Ok(0), // zero-width def: the empty bit-vector
            None => Err(Ineligible(format!(
                "unknown def (width): {}",
                self.env.d.strings[name as usize]
            ))),
        }
    }

    fn expr_width(&self, f: &Frame<'ctx>, e: &Expr) -> Result<u32, Ineligible> {
        match e {
            Expr::Def(n) => match f.av_widths.get(n) {
                Some(&w) => Ok(w),
                None => self.def_width(f.inst, *n),
            },
            Expr::Port(p) => match f.args.get(p) {
                Some(&(_, w)) => Ok(w),
                None => {
                    let ie = self.ie(f.inst)?;
                    if ie.real_consts.contains_key(p) {
                        return Ok(64); // f64 bits carrier
                    }
                    if let Some(&(w, _)) = ie.wide_consts.get(p) {
                        return Ok(w);
                    }
                    Ok(ie.port_consts.get(p).map_or(1, |&(w, _)| w)) // reset/EN ports read 1 bit
                }
            },
            Expr::Param(p) => {
                let ie = self.ie(f.inst)?;
                if ie.real_consts.contains_key(p) {
                    return Ok(64); // f64 bits carrier
                }
                if let Some(&(w, _)) = ie.wide_consts.get(p) {
                    return Ok(w);
                }
                match ie.port_consts.get(p) {
                    Some(&(w, _)) => Ok(w),
                    None => nope("parameter not constant-lowerable"),
                }
            }
            Expr::Real(_) => Ok(64), // f64 bits carrier
            Expr::Gate { .. } => Ok(1),
            Expr::Const { width, .. }
            | Expr::MethCall { width, .. }
            | Expr::MethValue { width, .. }
            | Expr::TaskValue { width, .. }
            | Expr::ForeignCall { width, .. }
            | Expr::Prim { width, .. }
            | Expr::If { width, .. }
            | Expr::Case { width, .. } => Ok(*width), // 0 = empty bit-vector
            _ => nope(format!(
                "expression kind not compilable: {}",
                expr_kind(e)
            )),
        }
    }

    /// Resize `v` (of width `from`) to width `to`.
    fn to_w(&self, v: IntValue<'ctx>, from: u32, to: u32, signed: bool) -> IntValue<'ctx> {
        use std::cmp::Ordering::*;
        // zero-width values are the constant empty bit-vector: any
        // resize of one is 0, and a resize TO width 0 is the i1-carried
        // zero (ity(0) = i1).  from.max(1) is v's actual LLVM type.
        if from == 0 || to == 0 {
            return self.ity(to).const_zero();
        }
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

    /// i1 truthiness of a width-w value.  A zero-width value is the
    /// empty bit-vector — constant false, whatever bit its i1 carrier
    /// happens to hold.
    fn nonzero(&self, v: IntValue<'ctx>, w: u32) -> IntValue<'ctx> {
        if w == 0 {
            return self.ctx.bool_type().const_zero();
        }
        self.builder
            .build_int_compare(IntPredicate::NE, v, self.ity(w).const_zero(), "nz")
            .unwrap()
    }

    fn slot_ptr(&self, f: &Frame<'ctx>, slot: u32) -> PointerValue<'ctx> {
        let i64t = self.ctx.i64_type();
        let idx = self.slot_index(slot);
        unsafe { self.builder.build_gep(i64t, f.arena, &[idx], "sp").unwrap() }
    }

    /// Arena index for a slot: region-relative through the base param
    /// in exec dedup mode (globals like now/reset stay absolute).
    fn slot_index(&self, slot: u32) -> IntValue<'ctx> {
        let i64t = self.ctx.i64_type();
        if let Some((r0, r1, base, _)) = self.dedup {
            if slot >= r0 && slot < r1 {
                return self
                    .builder
                    .build_int_add(
                        base,
                        i64t.const_int((slot - r0) as u64, false),
                        "rsl",
                    )
                    .unwrap();
            }
        }
        i64t.const_int(slot as u64, false)
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

    /// Load a width-w value whose first slot is base + idx*ceil(w/64)
    /// with idx only known at run time (FIFO first: data[fst]).
    fn load_val_dyn(
        &self,
        f: &Frame<'ctx>,
        base: u32,
        idx: IntValue<'ctx>,
        w: u32,
    ) -> IntValue<'ctx> {
        let i64t = self.ctx.i64_type();
        let words = w.max(1).div_ceil(64);
        let scaled = self
            .builder
            .build_int_mul(idx, i64t.const_int(words as u64, false), "fsc")
            .unwrap();
        let bidx = self.slot_index(base);
        let off = self.builder.build_int_add(scaled, bidx, "foff").unwrap();
        if w <= 64 {
            let p = unsafe {
                self.builder.build_gep(i64t, f.arena, &[off], "fdp").unwrap()
            };
            let word =
                self.builder.build_load(i64t, p, "fdl").unwrap().into_int_value();
            return self.to_w(word, 64, w, false);
        }
        let t = self.ity(w);
        let mut acc = t.const_zero();
        for k in 0..words {
            let ok = self
                .builder
                .build_int_add(off, i64t.const_int(k as u64, false), "fok")
                .unwrap();
            let p = unsafe {
                self.builder.build_gep(i64t, f.arena, &[ok], "fdp").unwrap()
            };
            let word =
                self.builder.build_load(i64t, p, "fdl").unwrap().into_int_value();
            let wide = self.builder.build_int_z_extend(word, t, "wz").unwrap();
            let sh = t.const_int((64 * k) as u64, false);
            let pos = self.builder.build_left_shift(wide, sh, "wsh").unwrap();
            acc = self.builder.build_or(acc, pos, "wor").unwrap();
        }
        acc
    }

    /// Store a width-w value at base + idx*ceil(w/64) with idx only
    /// known at run time (FIFO enq: data[(fst+elems)%size]).
    #[allow(dead_code)]
    fn store_val_dyn(
        &self,
        f: &Frame<'ctx>,
        base: u32,
        idx: IntValue<'ctx>,
        w: u32,
        v: IntValue<'ctx>,
    ) {
        let i64t = self.ctx.i64_type();
        let words = w.max(1).div_ceil(64);
        let scaled = self
            .builder
            .build_int_mul(idx, i64t.const_int(words as u64, false), "fsc")
            .unwrap();
        let bidx = self.slot_index(base);
        let off = self.builder.build_int_add(scaled, bidx, "foff").unwrap();
        let t = self.ity(w.max(1));
        for k in 0..words {
            let ok = self
                .builder
                .build_int_add(off, i64t.const_int(k as u64, false), "fok")
                .unwrap();
            let p = unsafe {
                self.builder.build_gep(i64t, f.arena, &[ok], "fdp").unwrap()
            };
            let word = if w <= 64 {
                self.to_w(v, w, 64, false)
            } else {
                let sh = t.const_int((64 * k) as u64, false);
                let shifted =
                    self.builder.build_right_shift(v, sh, false, "fsh").unwrap();
                self.builder
                    .build_int_truncate(shifted, i64t, "ftr")
                    .unwrap()
            };
            self.builder.build_store(p, word).unwrap();
        }
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
        // string-typed expressions lower uniformly as i64 ids wherever
        // they appear (def bindings, cones, muxes) — the consumers
        // (StrDyn foreign args, string Eq, concat) understand the
        // carrier; concat inside a pure value cone stays ineligible
        // (no task context)
        if self.expr_is_str(f, e) {
            return self.str_expr(f, e, None);
        }
        match e {
            Expr::Const { width, limbs } => {
                if *width == 0 {
                    // the empty bit-vector: value is always 0, carried
                    // as i1 (ity(0)); the interp's Value width-0 masks
                    // every op to 0 identically
                    return Ok(self.ity(0).const_zero());
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
                if let Some(&(w, v)) = ie.port_consts.get(p) {
                    if w == 0 {
                        return Ok(self.ity(0).const_zero()); // empty bit-vector
                    }
                    return Ok(self.cval(w, &[v as u32, (v >> 32) as u32]));
                }
                if let Some(&bits) = ie.real_consts.get(p) {
                    return Ok(self.ctx.i64_type().const_int(bits, false));
                }
                if let Some((w, limbs)) = ie.wide_consts.get(p) {
                    let (w, limbs) = (*w, limbs.clone());
                    return Ok(self.cval(w, &limbs));
                }
                // input clock-gate port bound at instantiation: the
                // interp evaluates the recorded gate expr in the OWNER
                // instance's context, where LATCHED defs win over
                // recomputation — a compiled re-expansion only matches
                // when the cone is static (no defs, no prim-state
                // chases), so dynamic gates stay interp until gate
                // values get schedule-time slots (mcd_Rand measured a
                // cross-domain divergence with live re-expansion)
                if let Some((owner, g)) = ie.gates.get(p) {
                    // eligible dynamic cones: prim Gate chases are LIVE
                    // reads on both engines, and a Port link recurses
                    // through this same checked resolution in the owner
                    // (terminating at a const, a further chase, or nope)
                    if !gate_static(g)
                        && !matches!(g, Expr::Gate { .. } | Expr::Port(_))
                    {
                        return nope("dynamic input gate (latch semantics)");
                    }
                    let (owner, g) = (*owner, g.clone());
                    let mut of = self.child_frame(f, owner, None)?;
                    return self.expr(&mut of, &g);
                }
                nope("port read outside args/reset/EN/consts")
            }
            Expr::Param(p) => {
                let ie = self.ie(f.inst)?;
                if let Some(&bits) = ie.real_consts.get(p) {
                    return Ok(self.ctx.i64_type().const_int(bits, false));
                }
                if let Some((w, limbs)) = ie.wide_consts.get(p) {
                    let (w, limbs) = (*w, limbs.clone());
                    return Ok(self.cval(w, &limbs));
                }
                match ie.port_consts.get(p) {
                    Some(&(0, _)) => Ok(self.ity(0).const_zero()),
                    Some(&(w, v)) => {
                        Ok(self.cval(w, &[v as u32, (v >> 32) as u32]))
                    }
                    None => nope("parameter not constant-lowerable"),
                }
            }
            Expr::Real(r) => {
                // reals ride as i64 f64-bits; only Real-marked foreign
                // args (and real params) consume them, so the bits never
                // mix into integer arithmetic
                Ok(self.ctx.i64_type().const_int(r.to_bits(), false))
            }
            Expr::Gate { instance, clock } => {
                // a submodule's output clock gate (AMGate): prim
                // children answer gate_out() through the trampoline
                // (GATE_OUT_METHOD sentinel); user children evaluate
                // their recorded ifc gate expr in the child's frame; no
                // recorded gate = ungated (constant 1) — all three
                // mirror the interp's Expr::Gate
                let ie = self.ie(f.inst)?;
                let Some(&child) = ie.children.get(instance) else {
                    return nope("gate read on unknown child");
                };
                if self.env.insts.contains_key(&child) {
                    let cie = self.ie(child)?;
                    let cmod = &self.env.d.modules[cie.mir];
                    let g = cmod
                        .ifc_clock_gates
                        .iter()
                        .find(|(n, _)| n == clock)
                        .map(|(_, e)| e.clone());
                    return match g {
                        Some(e) => {
                            // same latch-semantics restriction as bound
                            // input gates, except a direct child prim
                            // gate chase (Expr::Gate) is a LIVE read on
                            // both engines and stays eligible
                            if !gate_static(&e)
                                && !matches!(e, Expr::Gate { .. })
                            {
                                return nope(
                                    "dynamic ifc gate (latch semantics)",
                                );
                            }
                            let mut cf = self.child_frame(f, child, None)?;
                            self.expr(&mut cf, &e)
                        }
                        None => Ok(self.ity(1).const_int(1, false)),
                    };
                }
                match self.emit_prim_call(
                    f,
                    child,
                    GATE_OUT_METHOD,
                    &[],
                    1,
                    false,
                )? {
                    Some(v) => Ok(v),
                    None => nope("gate_out returned no value"),
                }
            }
            Expr::MethCall { width, instance, method, port, args } => {
                self.value_call(f, *width, *instance, *method, *port, args)
            }
            Expr::MethValue { width, instance, method } => {
                // the returned value of an ActionValue method: the
                // interp calls call_value with no args — identical to a
                // no-arg value method read (port 0)
                self.value_call(f, *width, *instance, *method, 0, &[])
            }
            Expr::If { width, cond, then_, else_ } => {
                let wc = self.expr_width(f, cond)?;
                let c = self.expr(f, cond)?;
                let cz = self.nonzero(c, wc);
                // bsc LIFTS shared updates into mux dataflow; lowering
                // every If as a branch diamond re-manufactures control
                // flow LLVM's capped speculation cannot fully undo (the
                // monster bodies' 14k branches).  Pure, small arms keep
                // bsc's shape: evaluate both, one select.  Arms with
                // possible side effects (callbacks, unexpanded defs)
                // stay lazy, matching the interpreter.
                const SPEC_CAP: u32 = 64;
                let spec = self
                    .pure_size(f, then_, SPEC_CAP)
                    .zip(self.pure_size(f, else_, SPEC_CAP));
                if spec.is_some() {
                    let wt = self.expr_width(f, then_)?;
                    let tv0 = self.expr(f, then_)?;
                    let tv = self.to_w(tv0, wt, (*width).max(1), false);
                    let we = self.expr_width(f, else_)?;
                    let ev0 = self.expr(f, else_)?;
                    let ev = self.to_w(ev0, we, (*width).max(1), false);
                    return Ok(self
                        .builder
                        .build_select(cz, tv, ev, "sel")
                        .unwrap()
                        .into_int_value());
                }
                self.lazy_mux(f, *width, cz, then_, else_)
            }
            Expr::Case { width, scrutinee, arms, default } => {
                // one LLVM switch (backend lowers dense arms to a jump
                // table — the compare ladder was O(arms) per eval and
                // dominated the big decision-tree bodies); arms keep
                // the lazy_mux_fn discipline: scoped SSA, own blocks,
                // phi at the merge
                let w = (*width).max(1);
                let ws = self.expr_width(f, scrutinee)?;
                // interp semantics (lib.rs Case eval): a scrutinee wider
                // than 64 bits NEVER matches an arm (as_u64 compare is
                // gated on width <= 64) — lower the default only.  Keys
                // outside ws bits can't match either (the interp compares
                // the untruncated u64), and truncating them into the
                // switch could alias or duplicate cases.
                if ws > 64 {
                    let wd = self.expr_width(f, default)?;
                    let dv = self.expr(f, default)?;
                    return Ok(self.to_w(dv, wd, w, false));
                }
                let sv = self.expr(f, scrutinee)?;
                let func =
                    self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let def_bb = self.ctx.append_basic_block(func, "cd");
                let merge_bb = self.ctx.append_basic_block(func, "cj");
                let arm_bbs: Vec<_> = arms
                    .iter()
                    .map(|_| self.ctx.append_basic_block(func, "ca"))
                    .collect();
                let representable =
                    |k: u64| ws >= 64 || k < (1u64 << ws);
                let cases: Vec<_> = arms
                    .iter()
                    .zip(&arm_bbs)
                    .filter(|((k, _), _)| representable(*k))
                    .map(|((k, _), &bb)| {
                        (self.ity(ws).const_int_arbitrary_precision(&[*k]), bb)
                    })
                    .collect();
                self.builder.build_switch(sv, def_bb, &cases).unwrap();
                let saved: HashMap<StrId, IntValue<'ctx>> = f.ssa.clone();
                let mut incoming: Vec<(IntValue<'ctx>, _)> = Vec::new();
                for ((_, arm), &bb) in arms.iter().zip(&arm_bbs) {
                    self.builder.position_at_end(bb);
                    let wa = self.expr_width(f, arm)?;
                    let av0 = self.expr(f, arm)?;
                    let av = self.to_w(av0, wa, w, false);
                    f.ssa = saved.clone();
                    incoming.push((av, self.builder.get_insert_block().unwrap()));
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }
                self.builder.position_at_end(def_bb);
                let wd = self.expr_width(f, default)?;
                let dv0 = self.expr(f, default)?;
                let dv = self.to_w(dv0, wd, w, false);
                f.ssa = saved;
                incoming.push((dv, self.builder.get_insert_block().unwrap()));
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                self.builder.position_at_end(merge_bb);
                let phi = self.builder.build_phi(self.ity(w), "cv").unwrap();
                for (v, bb) in &incoming {
                    phi.add_incoming(&[(v, *bb)]);
                }
                Ok(phi.as_basic_value().into_int_value())
            }
            Expr::TaskValue { width, cookie } => {
                // load the join-safe slot when the task has one — valid
                // from any block, unlike the tphi value, which only
                // dominates its own arm
                if let Some(&(p, vw)) = f.av_slots.get(&(0x8000_0000u32 | *cookie)) {
                    let v = self
                        .builder
                        .build_load(self.ity(vw), p, "tval")
                        .unwrap()
                        .into_int_value();
                    return Ok(self.to_w(v, vw, (*width).max(1), false));
                }
                match f.tasks.get(cookie) {
                    Some(&(v, vw)) => Ok(self.to_w(v, vw, (*width).max(1), false)),
                    None => nope("task value before its task"),
                }
            }
            Expr::Prim { op, width, args } => self.prim(f, *op, *width, args),
            Expr::ForeignCall { width, func, args } => {
                self.bdpi_value_call(f, (*width).max(1), *func, args)
            }
            _ => nope(format!(
                "expression kind not compilable: {}",
                expr_kind(e)
            )),
        }
    }

    /// Direct BDPI value call (task #22): the interpreter's integer-
    /// slot ABI emitted inline — Bits by i64, Wide/Poly as i32-limb
    /// buffers, wide/poly returns via an out-pointer first arg; stdio
    /// flushes around the call keep printf/$display interleaving
    /// byte-exact.  System tasks and library-provided imports are not
    /// in the registry/globals and stay ineligible (v1 posture).
    fn bdpi_value_call(
        &mut self,
        f: &mut Frame<'ctx>,
        width: u32,
        func: StrId,
        args: &[Expr],
    ) -> Result<IntValue<'ctx>, Ineligible> {
        let Some(ff) = self.bdpi_import(func) else {
            return nope("foreign value call without BDPI import");
        };
        if Self::bdpi_lits_ok(&ff, args) {
            return self.bdpi_emit(f, width, &ff, args);
        }
        // dynamic CString arg: boxed trampoline call — the interp's
        // foreign_value falls through to bdpi_call, which marshals
        // Arg::Str for CString params
        match self.emit_foreign(f, func, args, &[], width, None)? {
            Some(v) => Ok(v),
            None => nope("BDPI value call returned no value"),
        }
    }

    /// Every FT::CString parameter is bound to a literal Expr::Str —
    /// the precondition for the direct bdpi_emit fast path.
    fn bdpi_lits_ok(ff: &trs_ir::ForeignFunc, args: &[Expr]) -> bool {
        ff.args.iter().zip(args).all(|(t, a)| {
            !matches!(t, trs_ir::ForeignType::CString) || matches!(a, Expr::Str(_))
        })
    }

    /// The design's BDPI import for `func`, if any (system tasks and
    /// unknown names return None).
    fn bdpi_import(&self, func: StrId) -> Option<trs_ir::ForeignFunc> {
        self.env.d.foreign_funcs.iter().find(|ff| ff.name == func).cloned()
    }

    /// Direct BDPI call at the current insertion point (no condition
    /// handling here — callers gate action calls themselves).
    fn bdpi_emit(
        &mut self,
        f: &mut Frame<'ctx>,
        width: u32,
        ff: &trs_ir::ForeignFunc,
        args: &[Expr],
    ) -> Result<IntValue<'ctx>, Ineligible> {
        use trs_ir::ForeignType as FT;
        let ff = ff.clone();
        let c_name = self.env.d.strings[ff.c_name as usize].clone();
        let i64t = self.ctx.i64_type();
        let i32t = self.ctx.i32_type();
        let ptrt = self.ctx.ptr_type(AddressSpace::default());
        let baked = matches!(self.cbs.cb, CbAddr::Baked(_));
        // callee pointer
        let callee: PointerValue<'ctx> = if baked {
            let Some(&addr) = BDPI_SYMS.get().and_then(|m| m.get(&c_name)) else {
                return nope("BDPI symbol not loaded");
            };
            i64t.const_int(addr as u64, false).const_to_pointer(ptrt)
        } else {
            let gname = format!("trs_bdpi_{c_name}");
            let g = self
                .module
                .get_global(&gname)
                .unwrap_or_else(|| self.module.add_global(ptrt, None, &gname));
            self.builder
                .build_load(ptrt, g.as_pointer_value(), "bdf")
                .unwrap()
                .into_pointer_value()
        };
        let stdio: PointerValue<'ctx> = if baked {
            let Some(&addr) = STDIO_CB.get() else {
                return nope("stdio cb not registered");
            };
            i64t.const_int(addr as u64, false).const_to_pointer(ptrt)
        } else {
            let g = self
                .module
                .get_global("trs_cb_stdio")
                .unwrap_or_else(|| self.module.add_global(ptrt, None, "trs_cb_stdio"));
            self.builder
                .build_load(ptrt, g.as_pointer_value(), "bds")
                .unwrap()
                .into_pointer_value()
        };
        let stdio_ty = self.ctx.void_type().fn_type(&[i64t.into()], false);
        let flush = |lc: &Self, phase: u64| {
            lc.builder
                .build_indirect_call(
                    stdio_ty,
                    stdio,
                    &[i64t.const_int(phase, false).into()],
                    "fl",
                )
                .unwrap();
        };

        // marshal
        let mut ptys: Vec<inkwell::types::BasicMetadataTypeEnum> = Vec::new();
        let mut slots: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
        let store_limbs = |lc: &Self, v: IntValue<'ctx>, w: u32| {
            let n32 = (w.max(1) as u64 + 31) / 32;
            let buf = lc.entry_alloca(i32t, n32, "bb");
            for k in 0..n32 {
                let piece = if k == 0 {
                    v
                } else {
                    let sh = lc.ity(w).const_int(32 * k, false);
                    lc.builder.build_right_shift(v, sh, false, "bs").unwrap()
                };
                let word = lc.to_w(piece, w, 32, false);
                let ip = unsafe {
                    lc.builder
                        .build_gep(i32t, buf, &[i64t.const_int(k, false)], "bi")
                        .unwrap()
                };
                // volatile: external C code reads this memory; an
                // in-process default<O2> deleted the (correct) buffer
                // traffic around the opaque call — repro preserved,
                // system opt on identical IR does NOT reproduce
                let st = lc.builder.build_store(ip, word).unwrap();
                st.set_volatile(true).unwrap();
            }
            buf
        };
        // wide/poly return: out-pointer first
        let ret_buf = match ff.ret {
            FT::Wide(n) => Some((n.max(1) as u64 + 31) / 32),
            FT::Poly => Some((width as u64 + 31) / 32),
            _ => None,
        }
        .map(|n32| {
            let buf = self.entry_alloca(i32t, n32, "bo");
            ptys.push(ptrt.into());
            slots.push(buf.into());
            (buf, n32)
        });
        if args.len() != ff.args.len() {
            return nope("BDPI arg count mismatch");
        }
        for (k, ft) in ff.args.iter().enumerate() {
            match ft {
                FT::Bits(_) => {
                    let wa = self.expr_width(f, &args[k])?;
                    let v = self.expr(f, &args[k])?;
                    ptys.push(i64t.into());
                    slots.push(self.to_w(v, wa, 64, false).into());
                }
                FT::Wide(m) => {
                    let wa = self.expr_width(f, &args[k])?;
                    let v0 = self.expr(f, &args[k])?;
                    let v = self.to_w(v0, wa, (*m).max(1), false);
                    ptys.push(ptrt.into());
                    slots.push(store_limbs(self, v, (*m).max(1)).into());
                }
                FT::Poly => {
                    let wa = self.expr_width(f, &args[k])?;
                    let v = self.expr(f, &args[k])?;
                    ptys.push(ptrt.into());
                    slots.push(store_limbs(self, v, wa.max(1)).into());
                }
                FT::CString => {
                    let Expr::Str(sid) = &args[k] else {
                        return nope("BDPI string arg not a literal");
                    };
                    let text = self.env.d.strings[*sid as usize].clone();
                    let gname = format!("trs_bdpistr_{sid}");
                    let g = self.module.get_global(&gname).unwrap_or_else(|| {
                        let arr = self.ctx.const_string(text.as_bytes(), true);
                        let g = self.module.add_global(arr.get_type(), None, &gname);
                        g.set_initializer(&arr);
                        g.set_constant(true);
                        g
                    });
                    ptys.push(ptrt.into());
                    slots.push(g.as_pointer_value().into());
                }
                FT::Void => return nope("void BDPI argument"),
            }
        }

        flush(self, 0);
        let fnty = i64t.fn_type(&ptys, false);
        let cs = self
            .builder
            .build_indirect_call(fnty, callee, &slots, "bdc")
            .unwrap();
        flush(self, 1);
        let raw = match cs.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(rv) => rv.into_int_value(),
            _ => i64t.const_zero(),
        };
        match ff.ret {
            FT::Void => Ok(self.ity(width).const_zero()),
            FT::Bits(n) => {
                let nn = n.clamp(1, 64);
                let masked = self.to_w(raw, 64, nn, false);
                Ok(self.to_w(masked, nn, width, false))
            }
            FT::Wide(_) | FT::Poly => {
                let (buf, n32) = ret_buf.unwrap();
                let mut acc = self.ity(width).const_zero();
                for k in 0..n32.min((width as u64 + 31) / 32) {
                    let ip = unsafe {
                        self.builder
                            .build_gep(i32t, buf, &[i64t.const_int(k, false)], "br")
                            .unwrap()
                    };
                    let w32 = self
                        .builder
                        .build_load(i32t, ip, "brl")
                        .unwrap()
                        .into_int_value();
                    w32.as_instruction()
                        .unwrap()
                        .set_volatile(true)
                        .unwrap();
                    let ext = self.to_w(w32, 32, width, false);
                    let sh = self.ity(width).const_int(32 * k, false);
                    let shifted =
                        self.builder.build_left_shift(ext, sh, "brs").unwrap();
                    acc = self.builder.build_or(acc, shifted, "bro").unwrap();
                }
                Ok(acc)
            }
            FT::CString => nope("BDPI string return"),
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
        if let Some(&(base, rw)) = ie.creg_slot.get(&instance) {
            if !matches!(mname.as_str(), "read" | "get" | "_read") || !args.is_empty() {
                return nope("non-read ConfigReg method in expression");
            }
            if rw != width {
                return nope("ConfigReg read width mismatch");
            }
            // read = (written_at == now) ? old : current — exactly the
            // interpreter's begin-of-instant rule
            let words = rw.max(1).div_ceil(64);
            let old = self.load_val(f, base, rw);
            let cur = self.load_val(f, base + words, rw);
            let wat = self.load_word(f, base + 2 * words);
            let now = self.load_word(f, self.env.now_slot);
            let wr = self
                .builder
                .build_int_compare(IntPredicate::EQ, wat, now, "cregwr")
                .unwrap();
            return Ok(self
                .builder
                .build_select(wr, old, cur, "cregv")
                .unwrap()
                .into_int_value());
        }
        if let Some(&(base, fw, _size, _g, loopy)) = ie.fifo_slot.get(&instance) {
            if !args.is_empty() {
                return nope("FIFO value method with args");
            }
            let i64t = self.ctx.i64_type();
            let load = |k: u32| self.load_word(f, base + k);
            // begin-of-instant element count for the i_ variants:
            // (enq_at==now || deq_at==now || clear_at==now)
            //   ? saved_elems : elems   (FifoType::Simple only)
            let inst_elems = |lc: &Self| -> IntValue<'ctx> {
                let now = lc.load_word(f, lc.env.now_slot);
                let mut any = lc
                    .builder
                    .build_int_compare(IntPredicate::EQ, load(3), now, "fe")
                    .unwrap();
                for k in [4u32, 5] {
                    let c = lc
                        .builder
                        .build_int_compare(IntPredicate::EQ, load(k), now, "fe")
                        .unwrap();
                    any = lc.builder.build_or(any, c, "feo").unwrap();
                }
                lc.builder
                    .build_select(any, load(1), load(0), "fsel")
                    .unwrap()
                    .into_int_value()
            };
            let cmp_w1 = |lc: &Self, pred, a: IntValue<'ctx>, b: IntValue<'ctx>| {
                let c = lc.builder.build_int_compare(pred, a, b, "fc").unwrap();
                lc.builder
                    .build_int_z_extend(c, lc.ity(1), "fb")
                    .unwrap()
            };
            return match mname.as_str() {
                "first" if fw == width => {
                    let fst = load(2);
                    Ok(self.load_val_dyn(f, base + 7, fst, fw))
                }
                "notFull" if width == 1 => {
                    Ok(cmp_w1(self, IntPredicate::ULT, load(0), i64t.const_int(_size as u64, false)))
                }
                "notEmpty" if width == 1 => {
                    Ok(cmp_w1(self, IntPredicate::NE, load(0), i64t.const_zero()))
                }
                "i_notFull" if width == 1 => {
                    // loopy i_* read LIVE elems: a same-instant deq
                    // reopens the fifo (the interp drops the
                    // begin-of-instant select for FifoType::Loopy)
                    let e = if loopy { load(0) } else { inst_elems(self) };
                    Ok(cmp_w1(self, IntPredicate::ULT, e, i64t.const_int(_size as u64, false)))
                }
                "i_notEmpty" if width == 1 => {
                    let e = if loopy { load(0) } else { inst_elems(self) };
                    Ok(cmp_w1(self, IntPredicate::NE, e, i64t.const_zero()))
                }
                _ => nope("FIFO value method mismatch"),
            };
        }
        if let Some(&(base, rw, lo, hi)) = ie.regfile_slot.get(&instance) {
            // RegFile sub: in-range reads inline over the dense image
            // with the ONE-DEEP same-instant bypass; out-of-range takes
            // the trampoline (bounds warning on stdout + undet) — the
            // B2 cold-path shape.
            if mname.as_str() != "sub" || args.len() != 1 {
                return nope("regfile method mismatch");
            }
            if rw != width {
                return nope("regfile read width mismatch");
            }
            let Some(&child) = ie.children.get(&instance) else {
                return nope("call on unknown child");
            };
            let i64t = self.ctx.i64_type();
            let aw = self.expr_width(f, &args[0])?;
            let a0 = self.expr(f, &args[0])?;
            let a = self.to_w(a0, aw, 64, false);
            let inlo = self
                .builder
                .build_int_compare(IntPredicate::UGE, a, i64t.const_int(lo, false), "rflo")
                .unwrap();
            let inhi = self
                .builder
                .build_int_compare(IntPredicate::ULE, a, i64t.const_int(hi, false), "rfhi")
                .unwrap();
            let inb = self.builder.build_and(inlo, inhi, "rfin").unwrap();
            let func =
                self.builder.get_insert_block().unwrap().get_parent().unwrap();
            let fast_bb = self.ctx.append_basic_block(func, "rff");
            let slow_bb = self.ctx.append_basic_block(func, "rfs");
            let join_bb = self.ctx.append_basic_block(func, "rfj");
            self.builder.build_conditional_branch(inb, fast_bb, slow_bb).unwrap();

            self.builder.position_at_end(fast_bb);
            let saved: HashMap<StrId, IntValue<'ctx>> = f.ssa.clone();
            let words = rw.max(1).div_ceil(64);
            let upd_at = self.load_word(f, base);
            let upd_addr = self.load_word(f, base + 1);
            let now = self.load_word(f, self.env.now_slot);
            let at_now = self
                .builder
                .build_int_compare(IntPredicate::EQ, upd_at, now, "rfan")
                .unwrap();
            let same_a = self
                .builder
                .build_int_compare(IntPredicate::EQ, upd_addr, a, "rfsa")
                .unwrap();
            let byp = self.builder.build_and(at_now, same_a, "rfb").unwrap();
            let pv = self.load_val(f, base + 2, rw);
            let idx = self
                .builder
                .build_int_sub(a, i64t.const_int(lo, false), "rfi")
                .unwrap();
            let dv = self.load_val_dyn(f, base + 2 + words, idx, rw);
            let fv = self
                .builder
                .build_select(byp, pv, dv, "rfv")
                .unwrap()
                .into_int_value();
            f.ssa = saved.clone();
            let f_end = self.builder.get_insert_block().unwrap();
            self.builder.build_unconditional_branch(join_bb).unwrap();

            self.builder.position_at_end(slow_bb);
            let sv = self
                .emit_prim_call(f, child, method, args, width, false)?
                .expect("value prim call returns");
            f.ssa = saved;
            let s_end = self.builder.get_insert_block().unwrap();
            self.builder.build_unconditional_branch(join_bb).unwrap();

            self.builder.position_at_end(join_bb);
            let phi = self.builder.build_phi(self.ity(rw), "rfp").unwrap();
            phi.add_incoming(&[(&fv, f_end), (&sv, s_end)]);
            return Ok(phi.as_basic_value().into_int_value());
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
        // the interp's call_value evaluates the method's RESULT expr
        // for Value and ActionValue methods alike; an Expr::MethValue
        // read arrives with NO args (the action side carried them).
        // The one asymmetry: the interp may see caller-latched arg
        // values in its Port fallthrough, which a fully-compiled design
        // never latches — so an AV result whose cone reads its own arg
        // ports stays ineligible.
        if m.kind != trs_ir::MethodKind::Value
            && !(m.kind == trs_ir::MethodKind::ActionValue && args.is_empty())
        {
            return nope("non-value method in expression");
        }
        let Some(res) = m.result.clone() else {
            return nope("value method without result");
        };
        if !args.is_empty() && args.len() != m.args.len() {
            return nope("method arg count mismatch");
        }
        let margs = m.args.clone();
        let mut cf = self.child_frame(f, child, Some(mi))?;
        if m.kind == trs_ir::MethodKind::ActionValue {
            // arg ports in the result cone read exactly what the interp
            // reads at THIS evaluation site: the per-edge latch when the
            // call was inlined earlier in this frame (select(cond, v, 0)
            // captured at the call site), else the uncalled-MethodArg
            // 0-fold via the Port fallthrough — sched-position hoists
            // evaluate BEFORE the call and legitimately read 0
            for pa in &margs {
                if let Some(&(lv, lw)) = f.av_args.get(&(instance, pa.name)) {
                    cf.args.insert(pa.name, (lv, lw));
                }
            }
        }
        for (a, p) in args.iter().zip(&margs) {
            let wa = self.expr_width(f, a)?;
            let v0 = self.expr(f, a)?;
            let v = self.to_w(v0, wa, p.width, false);
            cf.args.insert(p.name, (v, p.width));
        }
        let rw = self.expr_width(&cf, &res)?;
        let v = self.expr(&mut cf, &res)?;
        // call_value zero-extends the result to the caller's width
        let out = self.to_w(v, rw, width, false);
        self.rec_meth_result(&cf, child, method, out)?;
        Ok(out)
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

    /// Node count of a pure, speculation-safe expression: consts,
    /// ssa-resident defs (already computed in this function), inline
    /// arena-prim reads, arithmetic, and nested pure If/Case.  None =
    /// impure (possible side effects / unexpanded def) or over cap.
    fn pure_size(&self, f: &Frame<'ctx>, e: &Expr, cap: u32) -> Option<u32> {
        use trs_ir::Expr as E;
        if cap == 0 {
            return None;
        }
        let sub2 = |a: &Expr, b: &Expr| -> Option<u32> {
            let ca = self.pure_size(f, a, cap - 1)?;
            let cb = self.pure_size(f, b, cap.checked_sub(1 + ca)?)?;
            Some(1 + ca + cb)
        };
        match e {
            E::Const { .. } | E::Real(_) => Some(1),
            E::Def(n) => f.ssa.contains_key(n).then_some(1),
            E::Port(p) => {
                if f.args.contains_key(p) {
                    return Some(1);
                }
                let ie = self.ie(f.inst).ok()?;
                (ie.reset_slot.contains_key(p) || ie.en_slot.contains_key(p))
                    .then_some(1)
            }
            E::MethCall { instance, method, args, .. } => {
                if !args.is_empty() {
                    // dynamic-arg reads (RegFile.sub etc.) may warn
                    return None;
                }
                let ie = self.ie(f.inst).ok()?;
                let mname = &self.env.d.strings[*method as usize];
                let ok = (ie.reg_slot.contains_key(instance)
                    || ie.creg_slot.contains_key(instance))
                    && matches!(mname.as_str(), "read" | "get" | "_read")
                    || ie.wire_slot.contains_key(instance)
                        && matches!(mname.as_str(), "whas" | "wget")
                    || ie.fifo_slot.contains_key(instance)
                        && matches!(
                            mname.as_str(),
                            "first" | "notFull" | "notEmpty" | "i_notFull" | "i_notEmpty"
                        );
                ok.then_some(2)
            }
            E::Prim { op, args, .. } => {
                // Quot/Rem lower with an unconditional divisor-zero trap
                // check: speculating them in an unselected arm SIGFPEs
                // where the interp (evaluating only the taken arm) does
                // not — the canonical `b == 0 ? 0 : a % b` guard
                if matches!(op, PrimOp::Quot | PrimOp::Rem) {
                    return None;
                }
                let mut total = 1u32;
                for a in args {
                    total += self.pure_size(f, a, cap.checked_sub(total)?)?;
                }
                Some(total)
            }
            E::If { cond, then_, else_, .. } => {
                let cc = self.pure_size(f, cond, cap - 1)?;
                let rest = sub2(then_, else_)?;
                (cc + rest <= cap).then_some(cc + rest)
            }
            E::Case { scrutinee, arms, default, .. } => {
                let mut total = 1 + self.pure_size(f, scrutinee, cap - 1)?;
                for (_, a) in arms {
                    total += self.pure_size(f, a, cap.checked_sub(total)?)?;
                }
                total += self.pure_size(f, default, cap.checked_sub(total)?)?;
                (total <= cap).then_some(total)
            }
            _ => None,
        }
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
        // physical layout is w.max(1) words per argument, matching the
        // callback decode exactly — a zero-width argument occupies one
        // stored zero word (review finding: a 0-word argument
        // desynchronized the buffer walk into an out-of-bounds read)
        let total_words: u32 =
            arg_widths.iter().map(|&w| words_for(w.max(1))).sum::<u32>().max(1);
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
            let words = words_for(wa.max(1));
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
        let token_const =
            self.token_kind | self.prim_calls.len() as u64;
        if self.prim_calls.len() >= 1 << 16 {
            return nope("prim call-site count exceeds the 16-bit token field");
        }
        let token = self.spec.token_base | token_const;
        self.prim_calls.push(PrimCallSpec {
            inst: prim_inst,
            method,
            arg_widths,
            ret_width: if is_action { 0 } else { ret_width },
            is_action,
        });
        let tokv = match self.dedup {
            Some((_, _, _, tb)) => self
                .builder
                .build_or(tb, i64t.const_int(token_const, false), "tok")
                .unwrap(),
            None => i64t.const_int(token, false),
        };
        let prim_callee = self.cb_callee(self.cbs.prim);
        self.builder
            .build_indirect_call(
                self.cbs.prim_ty,
                prim_callee,
                &[envp.into(), tokv.into(), abuf.into(), obuf.into()],
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
            thunks: HashMap::new(),
            av_widths: HashMap::new(),
            dead_defs: Default::default(),
            tasks: HashMap::new(),
            av_slots: HashMap::new(),
            av_args: HashMap::new(),
            is_exec: f.is_exec,
            depth: f.depth + 1,
        })
    }

    /// Lower a def reference: body locals / cone memo, then this
    /// instance's fire-signal slots, then eager-def slots (exec bodies
    /// reload the schedule-time value), then table expansion.
    /// Static entry-block alloca: non-entry array allocas are DYNAMIC
    /// to LLVM (per-execution stack bumps, SROA-hostile — and the O2+
    /// pipeline miscompiled the BDPI buffer pattern built that way).
    fn entry_alloca(
        &self,
        elem: inkwell::types::IntType<'ctx>,
        count: u64,
        name: &str,
    ) -> PointerValue<'ctx> {
        let func = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let entry = func.get_first_basic_block().unwrap();
        let b = self.ctx.create_builder();
        match entry.get_first_instruction() {
            Some(i) => b.position_before(&i),
            None => b.position_at_end(entry),
        }
        b.build_array_alloca(elem, elem.const_int(count, false), name)
            .unwrap()
    }

    /// Whether evaluating `e` is OBSERVABLE: a dynamic-address prim
    /// read (RegFile.sub prints a bounds warning when the address is
    /// out of range) or a trapping op (Quot/Rem by a dynamic zero).
    /// Such cones must evaluate exactly as often as the interpreter
    /// evaluates them — see def_thunk.
    fn effectful_expr(
        &self,
        inst: usize,
        e: &Expr,
        seen: &mut std::collections::HashSet<(usize, StrId)>,
    ) -> bool {
        use trs_ir::Expr as E;
        match e {
            E::Def(n) => {
                if !seen.insert((inst, *n)) {
                    return false;
                }
                let Some(ie) = self.env.insts.get(&inst) else {
                    return true;
                };
                let m = &self.env.d.modules[ie.mir];
                m.defs
                    .iter()
                    .find(|d| d.name == *n)
                    .is_some_and(|d| self.effectful_expr(inst, &d.expr, seen))
            }
            E::MethCall { instance, method, args, .. } => {
                if args.iter().any(|a| self.effectful_expr(inst, a, seen)) {
                    return true;
                }
                let Some(ie) = self.env.insts.get(&inst) else {
                    return true;
                };
                let Some(&child) = ie.children.get(instance) else {
                    return true;
                };
                if let Some(ce) = self.env.insts.get(&child) {
                    // inlined module: a value call evaluates the
                    // result closure only
                    let cm = &self.env.d.modules[ce.mir];
                    return cm
                        .methods
                        .iter()
                        .find(|mm| mm.name == *method)
                        .and_then(|mm| mm.result.as_ref())
                        .is_some_and(|r| self.effectful_expr(child, r, seen));
                }
                // prim child: dynamic-arg value reads may warn —
                // except a FULL-RANGE RegFile (mkRegFileFull), where
                // every representable address is in [lo, hi] and the
                // bounds warning is unreachable (sudoku's hot LUTs:
                // thunking those cost 2.4x)
                if args.is_empty() {
                    return false;
                }
                if let Some(&(_, _, lo, hi)) = ie.regfile_slot.get(instance) {
                    // full coverage is judged against the ADDRESS
                    // WIDTH: every aw-bit address must land in
                    // [lo, hi] (a power-of-two SIZE with a wider
                    // address port still warns — sysMips ram_arr)
                    let aw = match &args[0] {
                        E::Def(dn) => self.def_width(inst, *dn).unwrap_or(0),
                        a => a.width(),
                    };
                    let full = (1..=64).contains(&aw)
                        && lo == 0
                        && hi >= if aw == 64 {
                            u64::MAX
                        } else {
                            (1u64 << aw) - 1
                        };
                    return !full;
                }
                true
            }
            E::Prim { op, args, .. } => {
                matches!(op, PrimOp::Quot | PrimOp::Rem)
                    || args.iter().any(|a| self.effectful_expr(inst, a, seen))
            }
            // BDPI value calls: nominally pure, but observable when the
            // C side prints or keeps state — the planner already
            // poisons ForeignCall cones; match it (review fleet)
            E::ForeignCall { args, .. } => {
                true || args.is_empty()
            }
            E::If { cond, then_, else_, .. } => {
                self.effectful_expr(inst, cond, seen)
                    || self.effectful_expr(inst, then_, seen)
                    || self.effectful_expr(inst, else_, seen)
            }
            E::Case { scrutinee, arms, default, .. } => {
                self.effectful_expr(inst, scrutinee, seen)
                    || arms.iter().any(|(_, a)| self.effectful_expr(inst, a, seen))
                    || self.effectful_expr(inst, default, seen)
            }
            E::Clock { osc, gate } => {
                self.effectful_expr(inst, osc, seen)
                    || self.effectful_expr(inst, gate, seen)
            }
            E::Reset { wire } => self.effectful_expr(inst, wire, seen),
            _ => false,
        }
    }

    /// Expand an effectful-eval def through a first-reference memo:
    /// entry allocas hold (value, valid); every reference checks the
    /// flag and only the first evaluates.  This is the interpreter's
    /// ctx.locals semantics in dominance-correct form — a plain ssa
    /// memo dies at Cond joins, and RE-expanding a def whose cone
    /// warns (RegFile bounds) re-fires the warning (sysMips: 116
    /// warnings vs the reference's 66).
    fn def_thunk(
        &mut self,
        f: &mut Frame<'ctx>,
        n: StrId,
        dex: &Expr,
    ) -> Result<IntValue<'ctx>, Ineligible> {
        let w = self.expr_width(f, dex)?.max(1);
        let ty = self.ity(w);
        let i8t = self.ctx.i8_type();
        let (vp, gp) = match f.thunks.get(&n) {
            Some(&t) => t,
            None => {
                let vp = self.entry_alloca(ty, 1, "thv");
                let gp = self.entry_alloca(i8t, 1, "thg");
                // the valid flag initializes FALSE at function entry
                // (the init must dominate every reference site)
                let b = self.ctx.create_builder();
                match gp.as_instruction().and_then(|i| i.get_next_instruction()) {
                    Some(next) => b.position_before(&next),
                    None => {
                        let entry = self
                            .builder
                            .get_insert_block()
                            .unwrap()
                            .get_parent()
                            .unwrap()
                            .get_first_basic_block()
                            .unwrap();
                        b.position_at_end(entry);
                    }
                }
                b.build_store(gp, i8t.const_zero()).unwrap();
                f.thunks.insert(n, (vp, gp));
                (vp, gp)
            }
        };
        let func = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let g = self
            .builder
            .build_load(i8t, gp, "thok")
            .unwrap()
            .into_int_value();
        let ok = self
            .builder
            .build_int_compare(IntPredicate::NE, g, i8t.const_zero(), "thok0")
            .unwrap();
        let ev_bb = self.ctx.append_basic_block(func, "thev");
        let jn_bb = self.ctx.append_basic_block(func, "thjn");
        self.builder
            .build_conditional_branch(ok, jn_bb, ev_bb)
            .unwrap();
        self.builder.position_at_end(ev_bb);
        // the eval block is CONDITIONAL: nested def expansions memoized
        // during the cone eval are defined only on this path and must
        // not leak past the join (review-fleet confirmed dominance
        // hazard — same save/restore discipline as lazy_mux arms)
        let saved: HashMap<StrId, IntValue<'ctx>> = f.ssa.clone();
        f.expanding.push(n);
        let v = self.expr(f, dex)?;
        f.expanding.pop();
        f.ssa = saved;
        // schedule-position slot store (mirrors the plain expansion
        // path): the owning sched fn publishes the eager slot at its
        // first — and only — evaluation
        if !f.is_exec && f.inst == self.spec.inst && self.spec.eager.contains(&n) {
            if let Some(&(base, w)) = self.ie(f.inst)?.eager_slot.get(&n) {
                if self
                    .edge
                    .as_ref()
                    .is_none_or(|e| e.exports.contains(&base))
                {
                    edge_ssa_count(3, 0);
                    self.store_val(f, base, w, v);
                }
            }
        }
        self.builder.build_store(vp, v).unwrap();
        self.builder
            .build_store(gp, i8t.const_int(1, false))
            .unwrap();
        self.builder.build_unconditional_branch(jn_bb).unwrap();
        self.builder.position_at_end(jn_bb);
        let out = self
            .builder
            .build_load(ty, vp, "thval")
            .unwrap()
            .into_int_value();
        f.ssa.insert(n, out);
        self.rec_def(f, n, out);
        Ok(out)
    }

    /// TRACED artifacts: mirror a def's computed value into its VCD
    /// recording slot (the interp's vcd_def_vals recording).  A no-op
    /// unless the instance declares the def (rec tables are empty in
    /// untraced artifacts — zero cost).
    fn rec_def(&mut self, f: &Frame<'ctx>, n: StrId, v: IntValue<'ctx>) {
        let Ok(ie) = self.ie(f.inst) else { return };
        let Some(&(base, w)) = ie.rec_defs.get(&n) else { return };
        let vw = v.get_type().get_bit_width();
        let vv = self.to_w(v, vw, w.max(1), false);
        self.store_val(f, base, w.max(1), vv);
    }

    /// TRACED artifacts: mirror an inlined user-child method call into
    /// its VCD recording slots (call time + argument ports), the
    /// interp's vcd_meth_calls.  Emit inside the taken branch only.
    fn rec_meth_call(
        &mut self,
        cf: &Frame<'ctx>,
        child: usize,
        method: StrId,
        argv: &[(IntValue<'ctx>, u32)],
    ) -> Result<(), Ineligible> {
        let rm = match self.ie(child)?.rec_meths.get(&method) {
            Some(rm) => rm.clone(),
            None => return Ok(()),
        };
        let now = self.load_word(cf, self.env.now_slot);
        self.store_word(cf, rm.t, now);
        for ((v, _), &(base, w)) in argv.iter().zip(&rm.args) {
            let vw = v.get_type().get_bit_width();
            let vv = self.to_w(*v, vw, w.max(1), false);
            self.store_val(cf, base, w.max(1), vv);
        }
        Ok(())
    }

    /// TRACED artifacts: record a value/AV method's returned value
    /// (the interp's vcd_meth_results).
    fn rec_meth_result(
        &mut self,
        cf: &Frame<'ctx>,
        child: usize,
        method: StrId,
        v: IntValue<'ctx>,
    ) -> Result<(), Ineligible> {
        let res = match self.ie(child)?.rec_meths.get(&method) {
            Some(rm) => rm.res,
            None => return Ok(()),
        };
        if let Some((base, w)) = res {
            let vw = v.get_type().get_bit_width();
            let vv = self.to_w(v, vw, w.max(1), false);
            self.store_val(cf, base, w.max(1), vv);
        }
        Ok(())
    }

    /// Join-safe slot for an ActionValue task binding: an entry alloca
    /// initialized to the undet pattern (Value::undet — what the interp
    /// yields when the binding arm never ran), stored at task execution.
    /// Post-join references load it, mirroring the interp's body-wide
    /// ctx.locals.
    /// The interp's Value::undet pattern (0xAA..., masked to width) as
    /// an LLVM constant.
    fn undet_const(&self, w: u32) -> IntValue<'ctx> {
        let mut words = vec![0xAAAA_AAAA_AAAA_AAAAu64; words_for(w.max(1)) as usize];
        let rem = w % 64;
        if w != 0 && rem != 0 {
            let last = words.len() - 1;
            words[last] &= (1u64 << rem) - 1;
        }
        if w == 0 {
            words = vec![0];
        }
        self.ity(w).const_int_arbitrary_precision(&words)
    }

    fn av_slot(&mut self, f: &mut Frame<'ctx>, n: StrId, w: u32) -> PointerValue<'ctx> {
        if let Some(&(p, _)) = f.av_slots.get(&n) {
            return p;
        }
        let ty = self.ity(w);
        let p = self.entry_alloca(ty, 1, "avsl");
        // undet init must dominate every reference site (same
        // discipline as the thunk valid-flag init)
        let mut words = vec![0xAAAA_AAAA_AAAA_AAAAu64; words_for(w) as usize];
        let rem = w % 64;
        if rem != 0 {
            let last = words.len() - 1;
            words[last] &= (1u64 << rem) - 1;
        }
        let undet = ty.const_int_arbitrary_precision(&words);
        let b = self.ctx.create_builder();
        match p.as_instruction().and_then(|i| i.get_next_instruction()) {
            Some(next) => b.position_before(&next),
            None => {
                let entry = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap()
                    .get_first_basic_block()
                    .unwrap();
                b.position_at_end(entry);
            }
        }
        b.build_store(p, undet).unwrap();
        f.av_slots.insert(n, (p, w));
        p
    }

    fn def(&mut self, f: &mut Frame<'ctx>, n: StrId) -> Result<IntValue<'ctx>, Ineligible> {
        if let Some(v) = f.ssa.get(&n) {
            return Ok(*v);
        }
        if f.dead_defs.contains(&n) {
            // an ActionValue task binding survives the join through its
            // slot (interp parity: ctx.locals is body-wide; undet when
            // the arm never ran)
            if let Some(&(p, w)) = f.av_slots.get(&n) {
                let out = self
                    .builder
                    .build_load(self.ity(w), p, "avesc")
                    .unwrap()
                    .into_int_value();
                return Ok(out);
            }
            return Err(Ineligible(format!(
                "def escaped conditional arm: {}",
                self.env.d.strings[n as usize]
            )));
        }
        // whole-edge SSA cache: a value latched (CF/WF/eager at its
        // schedule position) or legally shared by an earlier section of
        // this edge function replaces the slot load / cone re-expansion.
        // Frames with bound method args are excluded: their defs may be
        // call-site-specific.
        if f.args.is_empty() {
            if let Some(e) = &self.edge {
                if let Some(v) = e
                    .latched
                    .get(&(f.inst, n))
                    .or_else(|| e.shared.get(&(f.inst, n)))
                {
                    let v = *v;
                    f.ssa.insert(n, v);
                    return Ok(v);
                }
            }
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
                edge_ssa_count(0, 1);
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
                    edge_ssa_count(
                        if f.is_exec { 1 } else { 2 },
                        w.div_ceil(64) as usize,
                    );
                    return Ok(self.load_val(f, base, w));
                }
            }
        }
        // outlined piece: call its helper — compiled once per module
        // type, base-relative, with a per-instant memo inside when the
        // piece is stable.  The callee's region base is the caller's
        // base shifted by the (type-uniform) subtree offset.
        if let Some(out) = self.outlined {
            if self.helper_self == Some((ie.mir, n)) {
                // lowering this piece's own body: fall through to expand
            } else if let Some((href, w, hports)) = out.get(&(ie.mir, n)) {
                // parameterized pieces need every port bound in this
                // frame; otherwise expand inline as before
                let bound = hports.iter().all(|(p, _)| f.args.contains_key(p));
                if !bound {
                    // fall through to inline expansion below
                } else {
                let w = *w;
                let i64t = self.ctx.i64_type();
                let ptrt = self.ctx.ptr_type(AddressSpace::default());
                let mut ptys: Vec<inkwell::types::BasicMetadataTypeEnum> =
                    vec![ptrt.into(), ptrt.into(), i64t.into()];
                for (_, pw) in hports {
                    ptys.push(self.ity(*pw).into());
                }
                let hty = self.ity(w).fn_type(&ptys, false);
                let callee_base = self.slot_index(self.ie(f.inst)?.region.0);
                let envp = f.envp.ok_or_else(|| Ineligible("helper needs env".into()))?;
                let mut hargs: Vec<inkwell::values::BasicMetadataValueEnum> =
                    vec![f.arena.into(), envp.into(), callee_base.into()];
                for (pn, pw) in hports {
                    let (v, vw) = f.args[pn];
                    hargs.push(self.to_w(v, vw, *pw, false).into());
                }
                let cs = match href {
                    HelperRef::Addr(a) => {
                        let fp = i64t
                            .const_int(*a as u64, false)
                            .const_to_pointer(ptrt);
                        self.builder
                            .build_indirect_call(hty, fp, &hargs, "hlp")
                            .unwrap()
                    }
                    HelperRef::Sym(name) => {
                        let hf = self
                            .module
                            .get_function(name)
                            .unwrap_or_else(|| self.module.add_function(name, hty, None));
                        self.builder.build_call(hf, &hargs, "hlp").unwrap()
                    }
                };
                let inkwell::values::ValueKind::Basic(rv) = cs.try_as_basic_value() else {
                    return nope("helper returned void");
                };
                let v = rv.into_int_value();
                f.ssa.insert(n, v);
                self.rec_def(f, n, v);
                return Ok(v);
                }
            }
        }
        if f.expanding.contains(&n) {
            return nope("cyclic def");
        }
        let m = &self.env.d.modules[ie.mir];
        let Some(d) = m.defs.iter().find(|d| d.name == n) else {
            let chain: Vec<&str> = f
                .expanding
                .iter()
                .map(|&x| self.env.d.strings[x as usize].as_str())
                .collect();
            return Err(Ineligible(format!(
                "unknown def (expand): {} in {} (exec={} args={} chain={})",
                self.env.d.strings[n as usize],
                self.spec.label,
                f.is_exec,
                f.args.len(),
                chain.join(" <- "),
            )));
        };
        let dex = d.expr.clone();
        {
            let mut seen = std::collections::HashSet::new();
            if self.effectful_expr(f.inst, &dex, &mut seen) {
                return self.def_thunk(f, n, &dex);
            }
        }
        f.expanding.push(n);
        let v = self.expr(f, &dex)?;
        f.expanding.pop();
        f.ssa.insert(n, v);
        self.rec_def(f, n, v);
        // schedule-position defs are visible to exec bodies via the
        // arena — stored only by the OWNING rule's sched fn (an inlined
        // frame writing call-time values would corrupt them)
        if !f.is_exec && f.inst == self.spec.inst && self.spec.eager.contains(&n) {
            if let Some(&(base, w)) = self.ie(f.inst)?.eager_slot.get(&n) {
                if self
                    .edge
                    .as_ref()
                    .is_none_or(|e| e.exports.contains(&base))
                {
                    edge_ssa_count(3, 0);
                    self.store_val(f, base, w, v);
                }
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
        // a zero-width result is the empty bit-vector, always 0: the
        // interp masks every op back to zero at width 0.  A central
        // early return keeps the invariant where per-op lowering would
        // break it (Not of i1 0 is i1 1; Sra computes width-1, which
        // underflows) — prim exprs are pure, so skipping the args is
        // safe (review finding)
        if width == 0 {
            return Ok(self.ity(0).const_zero());
        }
        // string equality: the interp compares marker limbs, i.e. the
        // ids — an i64 id compare is the exact mirror (both engines
        // treat distinct ids as unequal regardless of text)
        if op == PrimOp::Eq
            && args.len() == 2
            && self.expr_is_str(f, &args[0])
        {
            let a = self.str_expr(f, &args[0], None)?;
            let b = self.str_expr(f, &args[1], None)?;
            return Ok(self
                .builder
                .build_int_compare(IntPredicate::EQ, a, b, "seq")
                .unwrap());
        }
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
                // left-to-right, first arg highest.  Zero-width members
                // contribute nothing and are skipped; the first real
                // member seeds acc UNSHIFTED — otherwise a full-width
                // member (its siblings all zero-width) would emit
                // `shl iW acc, W`, which is LLVM poison
                let t = self.ity(width);
                let mut acc: Option<IntValue<'ctx>> = None;
                let mut total = 0u32;
                for a in args {
                    let wa = self.expr_width(f, a)?;
                    let v0 = self.expr(f, a)?;
                    if wa == 0 {
                        continue;
                    }
                    let v = self.to_w(v0, wa, width, false);
                    total += wa;
                    if total > width {
                        return nope("concat width overflow");
                    }
                    acc = Some(match acc {
                        None => v,
                        Some(prev) => {
                            // prev holds >= 1 bit, so wa <= width - 1
                            let sh = t.const_int(wa as u64, false);
                            let shifted = self
                                .builder
                                .build_left_shift(prev, sh, "cc")
                                .unwrap();
                            self.builder.build_or(shifted, v, "co").unwrap()
                        }
                    });
                }
                if total != width {
                    return nope("concat width mismatch");
                }
                Ok(acc.unwrap_or_else(|| t.const_zero()))
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
                let fpe_callee = self.cb_callee(self.cbs.fpe);
                self.builder
                    .build_indirect_call(self.cbs.fpe_ty, fpe_callee, &[], "fpe")
                    .unwrap();
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
            thunks: HashMap::new(),
            av_widths: HashMap::new(),
            dead_defs: Default::default(),
            tasks: HashMap::new(),
            av_slots: HashMap::new(),
            av_args: HashMap::new(),
            is_exec: false,
            depth: 0,
        };
        self.sched_section(&mut f)?;
        self.builder.build_return(None).unwrap();
        Ok(())
    }

    /// The sched body at the builder's current position: cone eval,
    /// inhibitors, CF/WF + eager stores.  Reused by the whole-edge SSA
    /// emitter (one section per Sched node); every value computed here
    /// is at the section's top level, so recording it in the edge
    /// cache is dominance-safe.
    fn sched_section(&mut self, f: &mut Frame<'ctx>) -> Result<(), Ineligible> {
        let r = self.rule().clone();
        // EFFECTFUL eager defs latch FIRST, in list order — the
        // interpreter evaluates REntry::eager into the latch before
        // the entry's nodes, and for these evaluation order and count
        // are observable (RegFile bounds warnings, Quot/Rem traps).
        // Pure eager defs stay lazy: the cones reach them on demand
        // and the leftover loop below publishes unreached slots.
        for &e in &self.spec.eager {
            if !self.ie(self.spec.inst)?.eager_slot.contains_key(&e) {
                return nope("eager def without slot");
            }
            let m = &self.env.d.modules[self.ie(f.inst)?.mir];
            let Some(d) = m.defs.iter().find(|d| d.name == e) else {
                continue;
            };
            let dex = d.expr.clone();
            let mut seen = std::collections::HashSet::new();
            if self.effectful_expr(f.inst, &dex, &mut seen) {
                self.def(f, e)?; // stores the slot on compute
            }
        }
        let mut cf = self.def(f, r.can_fire)?; // i1
        for &slot in &self.spec.inhibit_slots {
            edge_ssa_count(0, 1);
            let other = self.load_word(f, slot);
            let nz = self.nonzero(other, 64);
            let zero = self.ctx.bool_type().const_zero();
            cf = self.builder.build_select(nz, zero, cf, "inh").unwrap().into_int_value();
        }
        let keep = |e: &Option<EdgeCtx>, slot: u32| {
            e.as_ref().is_none_or(|e| e.exports.contains(&slot))
        };
        if keep(&self.edge, self.spec.cf_slot) {
            let cf64 = self.to_w(cf, 1, 64, false);
            self.store_word(f, self.spec.cf_slot, cf64);
        }
        // the WF cone reads the (inhibited) latched CF, not the raw cone
        f.ssa.insert(r.can_fire, cf);
        let wf = self.def(f, r.will_fire)?;
        if keep(&self.edge, self.spec.wf_slot) {
            let wf64 = self.to_w(wf, 1, 64, false);
            self.store_word(f, self.spec.wf_slot, wf64);
        }
        // eager defs the cones did not reach still need their slots
        // stored (later rules' cones or bodies may reload them)
        let mut eager_vals = Vec::new();
        for &e in &self.spec.eager {
            let v = self.def(f, e)?; // def() stores to the slot on compute
            eager_vals.push((e, v));
        }
        // edge cache: CF/WF and eager defs are POSITION-LATCHED values
        // (what the slots hold); later sections read them in place of
        // slot loads.  Never evicted — eviction would change latched
        // semantics, not just performance.
        if self.edge.is_some() {
            let inst = self.spec.inst;
            let e = self.edge.as_mut().unwrap();
            e.latched.insert((inst, r.can_fire), cf);
            e.latched.insert((inst, r.will_fire), wf);
            for (n, v) in eager_vals {
                e.latched.insert((inst, n), v);
            }
        }
        Ok(())
    }

    /// exec_<label>(arena, env) -> i32: WF-gated body execution.
    fn lower_exec(&mut self) -> Result<(), Ineligible> {
        let ptrt = self.ctx.ptr_type(AddressSpace::default());
        let i32t = self.ctx.i32_type();
        let i64t = self.ctx.i64_type();
        // exec fns take (arena, env, region base index, token base):
        // all in-region state addresses relative to base and call-site
        // tokens OR the runtime token base, so ONE compiled body serves
        // every instance of the module type (per-module-type dedup)
        let fnty = i32t
            .fn_type(&[ptrt.into(), ptrt.into(), i64t.into(), i64t.into()], false);
        let func = self.module.add_function(&format!("exec_{}", self.spec.label), fnty, None);
        let entry = self.ctx.append_basic_block(func, "entry");
        let stop_bb = self.ctx.append_basic_block(func, "stop");

        self.builder.position_at_end(entry);
        let region = self.ie(self.spec.inst)?.region;
        self.dedup = Some((
            region.0,
            region.1,
            func.get_nth_param(2).unwrap().into_int_value(),
            func.get_nth_param(3).unwrap().into_int_value(),
        ));
        let mut f = Frame {
            arena: func.get_nth_param(0).unwrap().into_pointer_value(),
            envp: Some(func.get_nth_param(1).unwrap().into_pointer_value()),
            inst: self.spec.inst,
            method_idx: None,
            args: HashMap::new(),
            ssa: HashMap::new(),
            expanding: Vec::new(),
            thunks: HashMap::new(),
            av_widths: HashMap::new(),
            dead_defs: Default::default(),
            tasks: HashMap::new(),
            av_slots: HashMap::new(),
            av_args: HashMap::new(),
            is_exec: true,
            depth: 0,
        };
        self.exec_section(&mut f, func, stop_bb)?;
        self.builder.build_return(Some(&i32t.const_int(0, false))).unwrap();
        self.builder.position_at_end(stop_bb);
        self.builder.build_return(Some(&i32t.const_int(1, false))).unwrap();
        Ok(())
    }

    /// The WF gate + body at the builder's current position; leaves the
    /// builder at the section's continuation block.  $finish paths jump
    /// to `stop_bb` (owned by the caller).  Reused by the whole-edge
    /// SSA emitter, where the gate reads the sched's latched WF value
    /// instead of the slot.
    fn exec_section(
        &mut self,
        f: &mut Frame<'ctx>,
        func: FunctionValue<'ctx>,
        stop_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> Result<(), Ineligible> {
        let r = self.rule().clone();
        let body_bb = self.ctx.append_basic_block(func, "body");
        let cont_bb = self.ctx.append_basic_block(func, "cont");
        if self.spec.always_fire {
            // WILL_FIRE == const true: no gate (Ravi's static case)
            self.builder.build_unconditional_branch(body_bb).unwrap();
        } else {
            let latched = self
                .edge
                .as_ref()
                .and_then(|e| e.latched.get(&(self.spec.inst, r.will_fire)))
                .copied();
            let fire = match latched {
                Some(v) => self.nonzero(v, 1),
                None => {
                    edge_ssa_count(0, 1);
                    let wf = self.load_word(f, self.spec.wf_slot);
                    self.nonzero(wf, 64)
                }
            };
            self.builder.build_conditional_branch(fire, body_bb, cont_bb).unwrap();
        }

        self.builder.position_at_end(body_bb);
        self.stmts(f, func, &r.body, stop_bb)?;
        self.builder.build_unconditional_branch(cont_bb).unwrap();
        self.builder.position_at_end(cont_bb);
        Ok(())
    }

    /// One outlined def piece: iN hlp(arena, env, base).  Base-relative
    /// addressing throughout (shared across instances of the type);
    /// stable pieces get a per-instant memo prologue over [stamp,
    /// value] slots in the instance region.
    fn lower_helper(&mut self, hs: &HelperSpec) -> Result<(), Ineligible> {
        let ptrt = self.ctx.ptr_type(AddressSpace::default());
        let i64t = self.ctx.i64_type();
        let w = hs.width.max(1);
        let mut ptys: Vec<inkwell::types::BasicMetadataTypeEnum> =
            vec![ptrt.into(), ptrt.into(), i64t.into()];
        for (_, pw) in &hs.ports {
            ptys.push(self.ity(*pw).into());
        }
        let fnty = self.ity(w).fn_type(&ptys, false);
        // an earlier helper may have DECLARED this symbol at a call
        // site; adding a same-named function would silently rename the
        // definition (sym.1) and leave the declaration bodyless —
        // define into the existing declaration instead
        let func = self
            .module
            .get_function(&hs.sym)
            .unwrap_or_else(|| self.module.add_function(&hs.sym, fnty, None));
        let entry = self.ctx.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);
        let region = self.ie(hs.inst)?.region;
        self.dedup = Some((
            region.0,
            region.1,
            func.get_nth_param(2).unwrap().into_int_value(),
            // helpers carry no callback sites (v1): token base unused
            i64t.const_zero(),
        ));
        let mut args: HashMap<StrId, (IntValue<'ctx>, u32)> = HashMap::new();
        for (k, (pn, pw)) in hs.ports.iter().enumerate() {
            args.insert(
                *pn,
                (
                    func.get_nth_param(3 + k as u32).unwrap().into_int_value(),
                    *pw,
                ),
            );
        }
        let mut f = Frame {
            arena: func.get_nth_param(0).unwrap().into_pointer_value(),
            envp: Some(func.get_nth_param(1).unwrap().into_pointer_value()),
            inst: hs.inst,
            method_idx: None,
            args,
            ssa: HashMap::new(),
            expanding: Vec::new(),
            thunks: HashMap::new(),
            av_widths: HashMap::new(),
            dead_defs: Default::default(),
            tasks: HashMap::new(),
            av_slots: HashMap::new(),
            av_args: HashMap::new(),
            is_exec: true,
            depth: 0,
        };
        let (hit_bb, miss_bb) = if hs.memo_slot.is_some() {
            (
                Some(self.ctx.append_basic_block(func, "hit")),
                Some(self.ctx.append_basic_block(func, "miss")),
            )
        } else {
            (None, None)
        };
        if let (Some(ms), Some(hit), Some(miss)) = (hs.memo_slot, hit_bb, miss_bb) {
            let stamp = self.load_word(&f, ms);
            let now = self.load_word(&f, self.env.now_slot);
            let eq = self
                .builder
                .build_int_compare(IntPredicate::EQ, stamp, now, "mhit")
                .unwrap();
            self.builder.build_conditional_branch(eq, hit, miss).unwrap();
            self.builder.position_at_end(hit);
            let cached = self.load_val(&f, ms + 1, w);
            self.builder.build_return(Some(&cached)).unwrap();
            self.builder.position_at_end(miss);
        }
        let v = self.def(&mut f, hs.def)?;
        if let Some(ms) = hs.memo_slot {
            self.store_val(&f, ms + 1, w, v);
            let now = self.load_word(&f, self.env.now_slot);
            self.store_word(&f, ms, now);
        }
        self.builder.build_return(Some(&v)).unwrap();
        Ok(())
    }

    /// Marshal a foreign call site: numeric args as word runs (strings
    /// ride the spec table), call, optionally read back result words.
    /// Returns the result value for tasks (ret_width > 0).
    /// Statically real-typed expressions: the shapes whose interp
    /// evaluation yields a REAL_MARKER Value (val_arg -> Arg::Real).
    /// Reals reach simulation only as literals, parameters, and muxes/
    /// defs over those, so the classification is decidable at lowering.
    fn expr_is_real(&self, f: &Frame<'ctx>, e: &Expr) -> bool {
        self.expr_is_real_in(f, e, &mut Vec::new())
    }

    fn expr_is_real_in(
        &self,
        f: &Frame<'ctx>,
        e: &Expr,
        seen: &mut Vec<StrId>,
    ) -> bool {
        match e {
            Expr::Real(_) => true,
            Expr::Param(p) | Expr::Port(p) => self
                .ie(f.inst)
                .map(|ie| ie.real_consts.contains_key(p))
                .unwrap_or(false),
            Expr::If { then_, else_, .. } => {
                self.expr_is_real_in(f, then_, seen)
                    || self.expr_is_real_in(f, else_, seen)
            }
            Expr::Case { arms, default, .. } => {
                arms.iter().any(|(_, a)| self.expr_is_real_in(f, a, seen))
                    || self.expr_is_real_in(f, default, seen)
            }
            Expr::Def(n) => {
                if seen.contains(n) {
                    return false; // cycle guard
                }
                let Ok(ie) = self.ie(f.inst) else { return false };
                let m = &self.env.d.modules[ie.mir];
                let Some(d) = m.defs.iter().find(|d| d.name == *n) else {
                    return false;
                };
                seen.push(*n);
                let r = self.expr_is_real_in(f, &d.expr, seen);
                seen.pop();
                r
            }
            _ => false,
        }
    }

    /// Statically string-typed expressions: the shapes whose interp
    /// evaluation yields a STR_MARKER Value (val_arg -> Arg::Str).
    /// Strings are literals, string params, muxes/defs over those,
    /// marker-passthrough width adjustments, and StringConcat results.
    fn expr_is_str(&self, f: &Frame<'ctx>, e: &Expr) -> bool {
        self.expr_is_str_in(f, e, &mut Vec::new())
    }

    fn expr_is_str_in(
        &self,
        f: &Frame<'ctx>,
        e: &Expr,
        seen: &mut Vec<StrId>,
    ) -> bool {
        match e {
            Expr::Str(_) => true,
            Expr::Param(p) | Expr::Port(p) => self
                .ie(f.inst)
                .map(|ie| ie.str_consts.contains_key(p))
                .unwrap_or(false),
            Expr::If { then_, else_, .. } => {
                self.expr_is_str_in(f, then_, seen)
                    || self.expr_is_str_in(f, else_, seen)
            }
            Expr::Case { arms, default, .. } => {
                arms.iter().any(|(_, a)| self.expr_is_str_in(f, a, seen))
                    || self.expr_is_str_in(f, default, seen)
            }
            Expr::Def(n) => {
                if seen.contains(n) {
                    return false; // cycle guard
                }
                let Ok(ie) = self.ie(f.inst) else { return false };
                let m = &self.env.d.modules[ie.mir];
                let Some(d) = m.defs.iter().find(|d| d.name == *n) else {
                    return false;
                };
                seen.push(*n);
                let r = self.expr_is_str_in(f, &d.expr, seen);
                seen.pop();
                r
            }
            Expr::Prim { op: PrimOp::StringConcat, .. } => true,
            Expr::Prim { op: PrimOp::ZeroExt | PrimOp::SignExt, args, .. } => {
                args.first()
                    .is_some_and(|a| self.expr_is_str_in(f, a, seen))
            }
            _ => false,
        }
    }

    /// Does a string expression contain a StringConcat anywhere —
    /// including through Def table expansion?  Concat interns per
    /// evaluation, so it must never run on an unselected mux arm.
    fn contains_concat(&self, f: &Frame<'ctx>, e: &Expr) -> bool {
        self.contains_concat_in(f, e, &mut Vec::new())
    }

    fn contains_concat_in(
        &self,
        f: &Frame<'ctx>,
        e: &Expr,
        seen: &mut Vec<StrId>,
    ) -> bool {
        match e {
            Expr::Prim { op: PrimOp::StringConcat, .. } => true,
            Expr::Prim { args, .. } => {
                args.iter().any(|a| self.contains_concat_in(f, a, seen))
            }
            Expr::If { cond, then_, else_, .. } => {
                self.contains_concat_in(f, cond, seen)
                    || self.contains_concat_in(f, then_, seen)
                    || self.contains_concat_in(f, else_, seen)
            }
            Expr::Case { scrutinee, arms, default, .. } => {
                self.contains_concat_in(f, scrutinee, seen)
                    || arms
                        .iter()
                        .any(|(_, a)| self.contains_concat_in(f, a, seen))
                    || self.contains_concat_in(f, default, seen)
            }
            Expr::Def(n) => {
                // a positioned binding is already a pure id; only an
                // unbound def's table expansion could re-run a concat
                if f.ssa.contains_key(n) {
                    return false;
                }
                if seen.contains(n) {
                    return false; // cycle guard
                }
                let Ok(ie) = self.ie(f.inst) else { return false };
                let m = &self.env.d.modules[ie.mir];
                let Some(d) = m.defs.iter().find(|d| d.name == *n) else {
                    return false;
                };
                seen.push(*n);
                let r = self.contains_concat_in(f, &d.expr, seen);
                seen.pop();
                r
            }
            _ => false,
        }
    }

    /// Lower a string-typed expression to its i64 string id (the
    /// compiled carrier for the interp's str_ref marker Value).
    fn str_expr(
        &mut self,
        f: &mut Frame<'ctx>,
        e: &Expr,
        stop_bb: Option<inkwell::basic_block::BasicBlock<'ctx>>,
    ) -> Result<IntValue<'ctx>, Ineligible> {
        let i64t = self.ctx.i64_type();
        match e {
            Expr::Str(sid) => Ok(i64t.const_int(*sid as u64, false)),
            Expr::Param(p) | Expr::Port(p) => {
                match self.ie(f.inst)?.str_consts.get(p) {
                    Some(&sid) => Ok(i64t.const_int(sid as u64, false)),
                    None => nope("non-string port in string context"),
                }
            }
            Expr::If { cond, then_, else_, .. } => {
                // arms evaluate eagerly (select), which is only sound
                // for PURE ids — a StringConcat in an arm would intern
                // on the unselected path too, diverging from the
                // interp's selected-arm-only evaluation (review
                // finding); such shapes stay ineligible until arms get
                // real control flow
                if self.contains_concat(f, then_) || self.contains_concat(f, else_) {
                    return nope("string concat under a mux arm");
                }
                let wc = self.expr_width(f, cond)?;
                let c = self.expr(f, cond)?;
                let cz = self.nonzero(c, wc);
                let tv = self.str_expr(f, then_, stop_bb)?;
                let ev = self.str_expr(f, else_, stop_bb)?;
                Ok(self
                    .builder
                    .build_select(cz, tv, ev, "ssel")
                    .unwrap()
                    .into_int_value())
            }
            Expr::Case { scrutinee, arms, default, .. } => {
                if arms.iter().any(|(_, a)| self.contains_concat(f, a))
                    || self.contains_concat(f, default)
                {
                    return nope("string concat under a mux arm");
                }
                let ws = self.expr_width(f, scrutinee)?;
                // interp mirror: wide scrutinees never match an arm
                if ws > 64 {
                    return self.str_expr(f, default, stop_bb);
                }
                let sv = self.expr(f, scrutinee)?;
                let mut acc = self.str_expr(f, default, stop_bb)?;
                for (k, a) in arms {
                    if ws < 64 && *k >= (1u64 << ws) {
                        continue; // key not representable: never matches
                    }
                    let kv = self.ity(ws).const_int(*k, false);
                    let hit = self
                        .builder
                        .build_int_compare(IntPredicate::EQ, sv, kv, "scse")
                        .unwrap();
                    let av = self.str_expr(f, a, stop_bb)?;
                    acc = self
                        .builder
                        .build_select(hit, av, acc, "scsl")
                        .unwrap()
                        .into_int_value();
                }
                Ok(acc)
            }
            Expr::Def(n) => {
                // same resolution order as numeric def(): the POSITIONED
                // ssa binding wins (a Stmt::Def bound the id at its
                // statement position — table re-expansion would observe
                // later state mutations and re-run concat effects), then
                // dead-def refusal, then table expansion (review finding)
                if let Some(v) = f.ssa.get(n) {
                    return Ok(*v);
                }
                if f.dead_defs.contains(n) {
                    if let Some(&(p, w)) = f.av_slots.get(n) {
                        let out = self
                            .builder
                            .build_load(self.ity(w), p, "savesc")
                            .unwrap()
                            .into_int_value();
                        return Ok(out);
                    }
                    return nope("string def escaped conditional arm");
                }
                let ie = self.ie(f.inst)?;
                let m = &self.env.d.modules[ie.mir];
                let Some(d) = m.defs.iter().find(|d| d.name == *n) else {
                    return nope("unknown string def");
                };
                if f.expanding.contains(n) {
                    return nope("string def cycle");
                }
                let dex = d.expr.clone();
                f.expanding.push(*n);
                let r = self.str_expr(f, &dex, stop_bb);
                f.expanding.pop();
                let v = r?;
                f.ssa.insert(*n, v);
                Ok(v)
            }
            Expr::Prim { op: PrimOp::ZeroExt | PrimOp::SignExt, args, .. } => {
                // marker passthrough: width adjustments of string
                // values are identity in the interp
                self.str_expr(f, &args[0], stop_bb)
            }
            Expr::Prim { op: PrimOp::StringConcat, args, .. } => {
                // per-evaluation intern through the callback, exactly
                // the interp's intern_dyn; pure value cones (e.g.
                // string Eq) have no stop block, and the concat
                // callback never requests a stop
                match self.emit_foreign(
                    f,
                    STRING_CONCAT_FUNC,
                    args,
                    &[],
                    64,
                    stop_bb,
                )? {
                    Some(v) => Ok(v),
                    None => nope("string concat returned no value"),
                }
            }
            _ => nope("expression not string-lowerable"),
        }
    }

    fn emit_foreign(
        &mut self,
        f: &mut Frame<'ctx>,
        func_id: StrId,
        args: &[Expr],
        signed: &[bool],
        ret_width: u32,
        stop_bb: Option<inkwell::basic_block::BasicBlock<'ctx>>,
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
            if self.expr_is_real(f, a) {
                // i64 f64-bits value, one marshaled word; the spec's
                // Real tag makes the decode rebuild Arg::Real
                let v = self.expr(f, a)?;
                spec_args.push(FArgSpec::Real);
                vals.push((v, 64));
                continue;
            }
            if self.expr_is_str(f, a) {
                // i64 string id, one marshaled word; the spec's StrDyn
                // tag makes the decode resolve it to Arg::Str
                let v = self.str_expr(f, a, stop_bb)?;
                spec_args.push(FArgSpec::StrDyn);
                vals.push((v, 64));
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
        // the foreign decode reads w.max(1) words per numeric arg, so a
        // zero-width arg still occupies ONE (zero) word in the buffer —
        // its spec width stays 0 so the formatter sees the interp's
        // width-0 Value
        let total_words: u32 =
            vals.iter().map(|&(_, w)| words_for(w.max(1))).sum::<u32>().max(1);
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
            for k in 0..words_for(wa.max(1)) {
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
            off += words_for(wa.max(1));
        }
        let token =
            self.spec.token_base | self.token_kind | self.foreign_stmts.len() as u64;
        if self.foreign_stmts.len() >= 1 << 16 {
            return nope("foreign call-site count exceeds the 16-bit token field");
        }
        let token_const = self.token_kind | (self.foreign_stmts.len() as u64);
        self.foreign_stmts.push(ForeignSpec {
            inst: f.inst,
            func: func_id,
            ret_width,
            args: spec_args,
        });
        let tokv = match self.dedup {
            Some((_, _, _, tb)) => self
                .builder
                .build_or(tb, i64t.const_int(token_const, false), "tok")
                .unwrap(),
            None => i64t.const_int(token, false),
        };
        let cb_callee = self.cb_callee(self.cbs.cb);
        let call = self
            .builder
            .build_indirect_call(
                self.cbs.cb_ty,
                cb_callee,
                &[envp.into(), tokv.into(), abuf.into(), obuf.into()],
                "fcb",
            )
            .unwrap();
        let inkwell::values::ValueKind::Basic(rv) = call.try_as_basic_value() else {
            return nope("callback returned void");
        };
        // outside a task context there is no abort target; the callback
        // contract reserves the nonzero return for genuine aborts and
        // nothing requests one today, so the result is ignored there
        if let Some(sb) = stop_bb {
            let stop = self
                .builder
                .build_int_compare(
                    IntPredicate::NE,
                    rv.into_int_value(),
                    self.ctx.i32_type().const_int(0, false),
                    "fst",
                )
                .unwrap();
            let func =
                self.builder.get_insert_block().unwrap().get_parent().unwrap();
            let cont_bb = self.ctx.append_basic_block(func, "fcont");
            self.builder.build_conditional_branch(stop, sb, cont_bb).unwrap();
            self.builder.position_at_end(cont_bb);
        }
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
        let mut it = list.iter().peekable();
        while let Some(st) = it.next() {
            match st {
                Stmt::Def { name, expr } => {
                    let v = self.expr(f, expr)?;
                    f.dead_defs.remove(name);
                    f.ssa.insert(*name, v);
                    self.rec_def(f, *name, v);
                }
                Stmt::Action(a) => self.action(f, func, a, stop_bb)?,
                Stmt::AvAction { def, action } => match action {
                    Action::Task { func: tf, cookie, temp, width, cond, args, signed } => {
                        let v = self.task_call(
                            f, func, *tf, *cookie, *temp, *width, cond, args, signed,
                            stop_bb,
                        )?;
                        f.av_widths.insert(*def, (*width).max(1));
                        f.dead_defs.remove(def);
                        f.ssa.insert(*def, v);
                        self.rec_def(f, *def, v);
                        // join-safe binding for the AvAction def itself
                        let dp = self.av_slot(f, *def, (*width).max(1));
                        self.builder.build_store(dp, v).unwrap();
                    }
                    Action::MethCall { instance, method, port, cond, args } => {
                        // ActionValue method on a prim child (trampoline)
                        // or a user-module child (inline)
                        let ie = self.ie(f.inst)?;
                        let Some(&child) = ie.children.get(instance) else {
                            return nope("avaction on unknown child");
                        };
                        if ie.reg_slot.contains_key(instance)
                            || ie.wire_slot.contains_key(instance)
                        {
                            return nope("avaction on reg/wire instance");
                        }
                        if self.env.insts.contains_key(&child) {
                            // user-module child: inline the ActionValue
                            // method body under the call condition
                            // (action protocol: EN stored first), then
                            // evaluate the result expr in the child
                            // frame; the skip path binds undet zeros
                            // exactly like the interpreter
                            if *port != 0 {
                                return nope("multi-ported user actionvalue method");
                            }
                            let cie = self.ie(child)?;
                            let cmod = &self.env.d.modules[cie.mir];
                            let Some((mi, m)) = cmod
                                .methods
                                .iter()
                                .enumerate()
                                .find(|(_, m)| m.name == *method)
                            else {
                                return nope("unknown actionvalue method on child");
                            };
                            if m.kind != trs_ir::MethodKind::ActionValue {
                                return nope("non-actionvalue avaction method");
                            }
                            // (* always_enabled *): the body is gated on
                            // the sibling RDY_<m> method at call time —
                            // the RESULT still evaluates when the body
                            // is skipped (the C++ returns the stale
                            // port value).  No RDY exported = ready.
                            let rdy_id = if m.always_enabled {
                                let rdy_name = format!(
                                    "RDY_{}",
                                    self.env.d.strings[*method as usize]
                                );
                                self.env
                                    .d
                                    .strings
                                    .iter()
                                    .position(|x| x == &rdy_name)
                                    .map(|id| id as StrId)
                                    .filter(|id| {
                                        cmod.methods.iter().any(|mm| mm.name == *id)
                                    })
                            } else {
                                None
                            };
                            if args.len() != m.args.len() {
                                return nope("method arg count mismatch");
                            }
                            let Some(result) = m.result.clone() else {
                                return nope("actionvalue method without result");
                            };
                            let margs = m.args.clone();
                            let body = m.body.clone();
                            let en_name =
                                format!("EN_{}", self.env.d.strings[*method as usize]);
                            let en_slot = self
                                .env
                                .d
                                .strings
                                .iter()
                                .position(|x| x == &en_name)
                                .and_then(|id| cie.en_slot.get(&(id as StrId)).copied());
                            let mut cf = self.child_frame(f, child, Some(mi))?;
                            let wc = self.expr_width(f, cond)?;
                            let c = self.expr(f, cond)?;
                            let cz = self.nonzero(c, wc);
                            for (a, pa) in args.iter().zip(&margs) {
                                let wa = self.expr_width(f, a)?;
                                let v0 = self.expr(f, a)?;
                                let v = self.to_w(v0, wa, pa.width, false);
                                cf.args.insert(pa.name, (v, pa.width));
                                // per-edge arg latch for later
                                // Expr::MethValue reads: the interp's
                                // latched map holds the value iff the
                                // method was CALLED this edge, else the
                                // uncalled-MethodArg fallthrough reads 0
                                let z = self.ity(pa.width.max(1)).const_zero();
                                let lv = self
                                    .builder
                                    .build_select(cz, v, z, "avarg")
                                    .unwrap()
                                    .into_int_value();
                                f.av_args
                                    .insert((*instance, pa.name), (lv, pa.width));
                            }
                            let go_bb = self.ctx.append_basic_block(func, "avmgo");
                            let sk_bb = self.ctx.append_basic_block(func, "avmsk");
                            let jn_bb = self.ctx.append_basic_block(func, "avmjn");
                            self.builder.build_conditional_branch(cz, go_bb, sk_bb).unwrap();
                            self.builder.position_at_end(go_bb);
                            if let Some(slot) = en_slot {
                                let one = self.ctx.i64_type().const_int(1, false);
                                self.store_word(&cf, slot, one);
                            }
                            let rec_argv: Vec<_> = margs
                                .iter()
                                .filter_map(|pa| cf.args.get(&pa.name).copied())
                                .collect();
                            self.rec_meth_call(&cf, child, *method, &rec_argv)?;
                            if let Some(rid) = rdy_id {
                                // EN and recording landed; only the
                                // body waits on RDY — the result expr
                                // below evaluates on both paths.  SSA
                                // bindings from the gated body must not
                                // leak past the join (dominance); the
                                // result re-expands or reads undet-init
                                // AV slots, the interp's skip semantics
                                let saved_ssa = cf.ssa.clone();
                                let r =
                                    self.value_call(f, 1, *instance, rid, 0, &[])?;
                                let rz = self.nonzero(r, 1);
                                let bd_bb =
                                    self.ctx.append_basic_block(func, "avmrdy");
                                let rs_bb =
                                    self.ctx.append_basic_block(func, "avmres");
                                self.builder
                                    .build_conditional_branch(rz, bd_bb, rs_bb)
                                    .unwrap();
                                self.builder.position_at_end(bd_bb);
                                self.stmts(&mut cf, func, &body, stop_bb)?;
                                // escaped-conditional-arm protocol for
                                // every binding the gated body created:
                                // mirror it into an undet-init entry
                                // alloca (store HERE, where the value
                                // dominates) and mark it dead, so
                                // post-join reads reload the slot —
                                // undet when the body was skipped, the
                                // interp's empty-locals fallback
                                let newly: Vec<(StrId, IntValue<'ctx>)> = cf
                                    .ssa
                                    .iter()
                                    .filter(|(k, _)| !saved_ssa.contains_key(k))
                                    .map(|(k, v)| (*k, *v))
                                    .collect();
                                for (k, v) in newly {
                                    let w = v.get_type().get_bit_width();
                                    let p = self.av_slot(&mut cf, k, w);
                                    self.builder.build_store(p, v).unwrap();
                                    cf.dead_defs.insert(k);
                                }
                                self.builder
                                    .build_unconditional_branch(rs_bb)
                                    .unwrap();
                                self.builder.position_at_end(rs_bb);
                                cf.ssa = saved_ssa;
                            } else {
                                self.stmts(&mut cf, func, &body, stop_bb)?;
                            }
                            // the AvAction def is a SYNTHETIC temp — it
                            // is in no def table (def_width fails), so
                            // the binding width is the RESULT's width,
                            // like the interpreter ("the callee's result
                            // already has the declared width")
                            let rv = self.expr(&mut cf, &result)?;
                            self.rec_meth_result(&cf, child, *method, rv)?;
                            // the interp records the def only on the
                            // executed path (skip leaves prior value)
                            self.rec_def(f, *def, rv);
                            let wd = rv.get_type().get_bit_width();
                            let g_end = self.builder.get_insert_block().unwrap();
                            self.builder.build_unconditional_branch(jn_bb).unwrap();
                            self.builder.position_at_end(sk_bb);
                            // the interp binds Value::undet (the masked
                            // 0xAA... pattern) when the call condition is
                            // false — NOT zero (review finding: the old
                            // comment claimed parity while binding 0)
                            let undet = self.undet_const(wd);
                            let s_end = self.builder.get_insert_block().unwrap();
                            self.builder.build_unconditional_branch(jn_bb).unwrap();
                            self.builder.position_at_end(jn_bb);
                            let phi =
                                self.builder.build_phi(self.ity(wd), "avmphi").unwrap();
                            phi.add_incoming(&[(&rv, g_end), (&undet, s_end)]);
                            f.av_widths.insert(*def, wd);
                            f.dead_defs.remove(def);
                            let pv = phi.as_basic_value().into_int_value();
                            f.ssa.insert(*def, pv);
                            // join-safe mirror (like the Task arm): an
                            // always_enabled RDY gate restores the SSA
                            // map after the gated body, so later reads
                            // of this binding reload from the slot
                            let dp = self.av_slot(f, *def, wd);
                            self.builder.build_store(dp, pv).unwrap();
                            continue;
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
                        // the trampoline routes is_action to call_action,
                        // which never writes the out buffer, and no prim
                        // implements an AV method (the interp panics):
                        // refuse rather than read uninitialized words
                        return nope("prim actionvalue method (no trampoline AV path)");
                        #[allow(unreachable_code)]
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
                        f.av_widths.insert(*def, wd);
                        f.dead_defs.remove(def);
                        f.ssa.insert(*def, phi.as_basic_value().into_int_value());
                    }
                    Action::Foreign { func: ffn, cond, args, .. } => {
                        // BDPI ActionValue import: direct call under
                        // the condition, result phi-bound to the def
                        let Some(imp) = self.bdpi_import(*ffn) else {
                            return nope("foreign actionvalue without BDPI import");
                        };
                        let wd = self.def_width(f.inst, *def).unwrap_or(1);
                        let wc = self.expr_width(f, cond)?;
                        let c = self.expr(f, cond)?;
                        let cz = self.nonzero(c, wc);
                        let go_bb = self.ctx.append_basic_block(func, "bvgo");
                        let sk_bb = self.ctx.append_basic_block(func, "bvsk");
                        let jn_bb = self.ctx.append_basic_block(func, "bvjn");
                        self.builder
                            .build_conditional_branch(cz, go_bb, sk_bb)
                            .unwrap();
                        self.builder.position_at_end(go_bb);
                        let v = self.bdpi_emit(f, wd, &imp, args)?;
                        let g_end = self.builder.get_insert_block().unwrap();
                        self.builder.build_unconditional_branch(jn_bb).unwrap();
                        self.builder.position_at_end(sk_bb);
                        let z = self.ity(wd).const_zero();
                        let s_end = self.builder.get_insert_block().unwrap();
                        self.builder.build_unconditional_branch(jn_bb).unwrap();
                        self.builder.position_at_end(jn_bb);
                        let phi =
                            self.builder.build_phi(self.ity(wd), "bvphi").unwrap();
                        phi.add_incoming(&[(&v, g_end), (&z, s_end)]);
                        f.av_widths.insert(*def, wd);
                        f.dead_defs.remove(def);
                        f.ssa.insert(*def, phi.as_basic_value().into_int_value());
                    }
                    _ => return nope("actionvalue kind in body"),
                },
                Stmt::Cond { cond, then_, else_ } => {
                    // MERGE a run of consecutive Conds with the
                    // IDENTICAL port-pure condition: bsc emits one
                    // reset-guard Cond per task group, and defs bound
                    // in one arm are used by later same-guard arms —
                    // the interpreter executes them as one guarded
                    // sequence, so the lowering must too (otherwise
                    // the defs "escape" and the design stays
                    // interpreted)
                    let mut then_run: Vec<&[Stmt]> = vec![then_];
                    let mut else_run: Vec<&[Stmt]> = vec![else_];
                    if Self::port_pure(cond) {
                        while let Some(Stmt::Cond {
                            cond: c2,
                            then_: t2,
                            else_: e2,
                        }) = it.peek()
                        {
                            if c2 == cond {
                                then_run.push(t2);
                                else_run.push(e2);
                                it.next();
                            } else {
                                break;
                            }
                        }
                    }
                    let wc = self.expr_width(f, cond)?;
                    let c = self.expr(f, cond)?;
                    let cz = self.nonzero(c, wc);
                    let then_bb = self.ctx.append_basic_block(func, "then");
                    let else_bb = self.ctx.append_basic_block(func, "else");
                    let join_bb = self.ctx.append_basic_block(func, "join");
                    self.builder.build_conditional_branch(cz, then_bb, else_bb).unwrap();
                    self.builder.position_at_end(then_bb);
                    let st_t = self.cond_arm_run(f, func, &then_run, stop_bb)?;
                    self.builder.build_unconditional_branch(join_bb).unwrap();
                    self.builder.position_at_end(else_bb);
                    let st_e = self.cond_arm_run(f, func, &else_run, stop_bb)?;
                    self.builder.build_unconditional_branch(join_bb).unwrap();
                    self.builder.position_at_end(join_bb);
                    // idempotent-task arm bindings: re-materialize at
                    // the join (dominates all later uses)
                    let truth = Expr::Const { width: 1, limbs: vec![1] };
                    for (d, a) in st_t.into_iter().chain(st_e) {
                        let Action::Task {
                            func: tf5,
                            cookie,
                            temp,
                            width,
                            args,
                            signed,
                            ..
                        } = a
                        else {
                            continue;
                        };
                        let name = self.env.d.strings[tf5 as usize].clone();
                        let v = if matches!(name.as_str(), "$time" | "$stime") {
                            let now = self.load_word(f, self.env.now_slot);
                            self.to_w(now, 64, width.max(1), false)
                        } else {
                            // run-constant, side-effect-free: re-call
                            self.task_call(
                                f, func, tf5, cookie, temp, width, &truth,
                                &args, &signed, stop_bb,
                            )?
                        };
                        if let Some(t) = temp {
                            f.dead_defs.remove(&t);
                            f.ssa.insert(t, v);
                        }
                        f.dead_defs.remove(&d);
                        f.ssa.insert(d, v);
                    }
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
        // join-safe slots FIRST (undet-initialized at entry): the
        // interp binds nothing on a false condition, so a later read
        // sees undet (or a previous execution's value) — the stores
        // happen only on the TAKEN path (review finding: the old
        // unconditional store of the phi wrote 0 over undet on skip)
        let ck = 0x8000_0000u32 | cookie;
        let cp = self.av_slot(f, ck, w);
        let tp = temp.map(|t| self.av_slot(f, t, w));
        let wc = self.expr_width(f, cond)?;
        let c = self.expr(f, cond)?;
        let cz = self.nonzero(c, wc);
        let go_bb = self.ctx.append_basic_block(func, "tgo");
        let sk_bb = self.ctx.append_basic_block(func, "tsk");
        let jn_bb = self.ctx.append_basic_block(func, "tjn");
        self.builder.build_conditional_branch(cz, go_bb, sk_bb).unwrap();
        self.builder.position_at_end(go_bb);
        let v = self
            .emit_foreign(f, tf, args, signed, w, Some(stop_bb))?
            .expect("task returns");
        self.builder.build_store(cp, v).unwrap();
        if let Some(tp) = tp {
            self.builder.build_store(tp, v).unwrap();
        }
        self.builder.build_unconditional_branch(jn_bb).unwrap();
        self.builder.position_at_end(sk_bb);
        self.builder.build_unconditional_branch(jn_bb).unwrap();
        self.builder.position_at_end(jn_bb);
        // the slot is the single source of truth at the join: executed
        // value, or the preserved previous/undet contents on skip —
        // exactly the interp's locals-then-latched-then-undet order
        let out = self
            .builder
            .build_load(self.ity(w), cp, "tval")
            .unwrap()
            .into_int_value();
        f.tasks.insert(cookie, (out, w));
        if let Some(t) = temp {
            f.ssa.insert(t, out);
        }
        Ok(out)
    }

    /// Conditions safe to evaluate ONCE for a run of merged Conds:
    /// ports/consts/prims only — no Def refs, no method calls, so the
    /// value cannot change between the original per-Cond evaluations.
    fn port_pure(e: &Expr) -> bool {
        match e {
            Expr::Const { .. } | Expr::Port(_) => true,
            Expr::Prim { args, .. } => args.iter().all(Self::port_pure),
            Expr::If { cond, then_, else_, .. } => {
                Self::port_pure(cond) && Self::port_pure(then_) && Self::port_pure(else_)
            }
            _ => false,
        }
    }

    /// One merged arm region over a run of same-condition Conds.
    /// Returns the arm's instant-stable ($time-class) bindings for
    /// re-materialization at the caller's join block.
    fn cond_arm_run(
        &mut self,
        f: &mut Frame<'ctx>,
        func: FunctionValue<'ctx>,
        lists: &[&[Stmt]],
        stop_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> Result<Vec<(StrId, Action)>, Ineligible> {
        let merged: Vec<Stmt> =
            lists.iter().flat_map(|l| l.iter().cloned()).collect();
        self.cond_arm(f, func, &merged, stop_bb)
    }

    /// A Cond arm: positioned defs (Stmt::Def / AvAction) bind INSIDE
    /// the arm and die at the join (dominance).  bsc's tsort scoping
    /// keeps their uses arm-local in practice; a post-arm reference
    /// would silently re-expand the def TABLE, whose expr may differ
    /// from the positioned stmt — so dead bindings are tracked and
    /// referencing one makes the design ineligible (loud, not wrong).
    fn cond_arm(
        &mut self,
        f: &mut Frame<'ctx>,
        func: FunctionValue<'ctx>,
        list: &[Stmt],
        stop_bb: inkwell::basic_block::BasicBlock<'ctx>,
    ) -> Result<Vec<(StrId, Action)>, Ineligible> {
        // arm bindings split two ways.  IDEMPOTENT tasks ($time-class:
        // instant-stable; plusargs-class: run-constant, side-effect-
        // free) are RE-MATERIALIZED by the caller at the JOIN block
        // (dominates all later uses — cross-arm dataflow of these is
        // the common reset-guard shape).  Everything else dies at the
        // join and post-arm references make the design ineligible.
        fn idempotent(name: &str) -> bool {
            matches!(
                name,
                "$time" | "$stime" | "$test$plusargs" | "$value$plusargs"
            )
        }
        fn bound(
            strings: &[String],
            list: &[Stmt],
            dead: &mut Vec<StrId>,
            stable: &mut Vec<(StrId, Action)>,
        ) {
            for st in list {
                match st {
                    Stmt::Def { name, .. } => dead.push(*name),
                    Stmt::AvAction { def, action } => match action {
                        Action::Task { func, .. }
                            if idempotent(strings[*func as usize].as_str()) =>
                        {
                            stable.push((*def, action.clone()));
                        }
                        Action::Task { temp: Some(t), .. } => {
                            dead.push(*def);
                            dead.push(*t);
                        }
                        _ => dead.push(*def),
                    },
                    Stmt::Cond { then_, else_, .. } => {
                        bound(strings, then_, dead, stable);
                        bound(strings, else_, dead, stable);
                    }
                    _ => {}
                }
            }
        }
        // arm-local bindings + table defs expanded inside the arm must
        // not leak (dominance)
        let saved = f.ssa.clone();
        let r = self.stmts(f, func, list, stop_bb);
        f.ssa = saved;
        let mut dead = Vec::new();
        let mut stable = Vec::new();
        bound(&self.env.d.strings, list, &mut dead, &mut stable);
        for n in dead {
            f.dead_defs.insert(n);
        }
        r.map(|_| stable)
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
                    // the value is evaluated eagerly either way — the
                    // old wr/sk branch protected only the store.
                    // Branchless: store(select(cond, new, old)); the
                    // monsters' 14k branches are mostly these.
                    let old = self.load_val(f, base, rw);
                    let sel = self
                        .builder
                        .build_select(cz, v, old, "wsel")
                        .unwrap()
                        .into_int_value();
                    self.store_val(f, base, rw, sel);
                    return Ok(());
                }
                if let Some(&(base, rw)) = ie.creg_slot.get(instance) {
                    if !matches!(mname.as_str(), "write" | "set" | "put" | "_write")
                        || args.len() != 1
                    {
                        return nope("non-write ConfigReg action");
                    }
                    // inline write under the action-condition branch
                    // (write sites are arm-multiplied: branchless paid
                    // loads+stores at every site every firing, a
                    // measured regression; taken-path inline still
                    // eliminates the trampoline).  Boxed semantics: the
                    // instant's FIRST write snapshots old.  Layout:
                    // [old (w), cur (w), written_at].
                    let words = rw.max(1).div_ceil(64);
                    let wv = self.expr_width(f, &args[0])?;
                    let v0 = self.expr(f, &args[0])?;
                    let v = self.to_w(v0, wv, rw, false);
                    let wr_bb = self.ctx.append_basic_block(func, "cwr");
                    let sk_bb = self.ctx.append_basic_block(func, "csk");
                    self.builder.build_conditional_branch(cz, wr_bb, sk_bb).unwrap();
                    self.builder.position_at_end(wr_bb);
                    let cur = self.load_val(f, base + words, rw);
                    let wat = self.load_word(f, base + 2 * words);
                    let now = self.load_word(f, self.env.now_slot);
                    let first = self
                        .builder
                        .build_int_compare(IntPredicate::NE, wat, now, "cwf")
                        .unwrap();
                    let old = self.load_val(f, base, rw);
                    let old2 = self
                        .builder
                        .build_select(first, cur, old, "cwo")
                        .unwrap()
                        .into_int_value();
                    self.store_val(f, base, rw, old2);
                    self.store_val(f, base + words, rw, v);
                    self.store_word(f, base + 2 * words, now);
                    self.builder.build_unconditional_branch(sk_bb).unwrap();
                    self.builder.position_at_end(sk_bb);
                    return Ok(());
                }
                if let Some(&(base, rw, lo, hi)) = ie.regfile_slot.get(instance) {
                    if mname.as_str() != "upd" || args.len() != 2 {
                        return nope("non-upd RegFile action");
                    }
                    let Some(&child) = ie.children.get(instance) else {
                        return nope("call on unknown child");
                    };
                    // taken-path inline (ConfigReg rule); out-of-range
                    // bounces to the boxed prim (warning + drop), which
                    // is arena-authoritative already
                    let i64t = self.ctx.i64_type();
                    let words = rw.max(1).div_ceil(64);
                    let aw = self.expr_width(f, &args[0])?;
                    let a0 = self.expr(f, &args[0])?;
                    let a = self.to_w(a0, aw, 64, false);
                    let wv = self.expr_width(f, &args[1])?;
                    let v0 = self.expr(f, &args[1])?;
                    let vv = self.to_w(v0, wv, rw, false);
                    let wr_bb = self.ctx.append_basic_block(func, "rfw");
                    let fast_bb = self.ctx.append_basic_block(func, "rfwf");
                    let slow_bb = self.ctx.append_basic_block(func, "rfws");
                    let done_bb = self.ctx.append_basic_block(func, "rfwd");
                    self.builder.build_conditional_branch(cz, wr_bb, done_bb).unwrap();

                    self.builder.position_at_end(wr_bb);
                    let inlo = self
                        .builder
                        .build_int_compare(
                            IntPredicate::UGE,
                            a,
                            i64t.const_int(lo, false),
                            "rfwlo",
                        )
                        .unwrap();
                    let inhi = self
                        .builder
                        .build_int_compare(
                            IntPredicate::ULE,
                            a,
                            i64t.const_int(hi, false),
                            "rfwhi",
                        )
                        .unwrap();
                    let inb = self.builder.build_and(inlo, inhi, "rfwin").unwrap();
                    self.builder.build_conditional_branch(inb, fast_bb, slow_bb).unwrap();

                    self.builder.position_at_end(fast_bb);
                    // one-deep bypass bookkeeping, branchless within the
                    // taken path: snapshot prev on the instant's first
                    // upd (or on an address change); the header stores
                    // are idempotent when not-first
                    let upd_at = self.load_word(f, base);
                    let upd_addr = self.load_word(f, base + 1);
                    let now = self.load_word(f, self.env.now_slot);
                    let at_now = self
                        .builder
                        .build_int_compare(IntPredicate::EQ, upd_at, now, "rfwan")
                        .unwrap();
                    let same_a = self
                        .builder
                        .build_int_compare(IntPredicate::EQ, upd_addr, a, "rfwsa")
                        .unwrap();
                    let not_first = self.builder.build_and(at_now, same_a, "rfwnf").unwrap();
                    let idx = self
                        .builder
                        .build_int_sub(a, i64t.const_int(lo, false), "rfwi")
                        .unwrap();
                    let dcur = self.load_val_dyn(f, base + 2 + words, idx, rw);
                    let oldprev = self.load_val(f, base + 2, rw);
                    let newprev = self
                        .builder
                        .build_select(not_first, oldprev, dcur, "rfwp")
                        .unwrap()
                        .into_int_value();
                    self.store_val(f, base + 2, rw, newprev);
                    self.store_word(f, base, now);
                    self.store_word(f, base + 1, a);
                    self.store_val_dyn(f, base + 2 + words, idx, rw, vv);
                    self.builder.build_unconditional_branch(done_bb).unwrap();

                    self.builder.position_at_end(slow_bb);
                    let saved: HashMap<StrId, IntValue<'ctx>> = f.ssa.clone();
                    self.emit_prim_call(f, child, *method, args, 0, true)?;
                    f.ssa = saved;
                    self.builder.build_unconditional_branch(done_bb).unwrap();

                    self.builder.position_at_end(done_bb);
                    return Ok(());
                }
                if let Some(&(base, fw, size, guarded, loopy)) =
                    ie.fifo_slot.get(instance).copied().as_ref()
                {
                    let is_enq = mname.as_str() == "enq";
                    let is_deq = mname.as_str() == "deq";
                    if (is_enq || is_deq) && size > 0 {
                        // taken-path inline (ConfigReg rule: keep the
                        // action-condition branch); guard-warning slow
                        // path bounces to the boxed prim, which
                        // refresh()es from the arena first
                        let i64t = self.ctx.i64_type();
                        let words = fw.max(1).div_ceil(64);
                        let v = if is_enq && fw > 0 {
                            let wv = self.expr_width(f, &args[0])?;
                            let v0 = self.expr(f, &args[0])?;
                            Some(self.to_w(v0, wv, fw, false))
                        } else {
                            None
                        };
                        let go_bb = self.ctx.append_basic_block(func, "fgo");
                        let warn_bb = self.ctx.append_basic_block(func, "fwr");
                        let fast_bb = self.ctx.append_basic_block(func, "fft");
                        let sk_bb = self.ctx.append_basic_block(func, "fsk");
                        self.builder.build_conditional_branch(cz, go_bb, sk_bb).unwrap();
                        self.builder.position_at_end(go_bb);
                        let elems = self.load_word(f, base);
                        let saved = self.load_word(f, base + 1);
                        let other_at =
                            self.load_word(f, base + if is_enq { 4 } else { 3 });
                        let now = self.load_word(f, self.env.now_slot);
                        let szc = i64t.const_int(size as u64, false);
                        let zero = i64t.const_zero();
                        let (lim, slim) =
                            if is_enq { (szc, szc) } else { (zero, zero) };
                        let bad =
                            self.builder
                                .build_int_compare(IntPredicate::EQ, elems, lim, "fb")
                                .unwrap();
                        // loopy enq drops the begin-of-instant
                        // disqualifier: enq-when-begin-full succeeds
                        // if a same-instant deq freed a slot (deq
                        // keeps it — only Bypass drops the deq-side
                        // clause, and Bypass stays boxed)
                        let warn = if guarded && !(is_enq && loopy) {
                            let same = self
                                .builder
                                .build_int_compare(IntPredicate::EQ, other_at, now, "fs")
                                .unwrap();
                            let sbad = self
                                .builder
                                .build_int_compare(IntPredicate::EQ, saved, slim, "fsb")
                                .unwrap();
                            let g = self.builder.build_and(same, sbad, "fg").unwrap();
                            self.builder.build_or(bad, g, "fw").unwrap()
                        } else {
                            bad
                        };
                        // in-reset suppress (post-clear, until deassert):
                        // bounce to the boxed prim, whose action_method
                        // no-ops — the fast path must not stamp or write
                        let sup = self
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                self.load_word(f, base + 6),
                                i64t.const_zero(),
                                "fsp",
                            )
                            .unwrap();
                        let warn = self.builder.build_or(warn, sup, "fws").unwrap();
                        self.builder
                            .build_conditional_branch(warn, warn_bb, fast_bb)
                            .unwrap();
                        // slow path: boxed prim (bookkeeping + println)
                        self.builder.position_at_end(warn_bb);
                        let targs: Vec<Expr> =
                            if v.is_some() { vec![args[0].clone()] } else { vec![] };
                        let _ = self.emit_prim_call(
                            f,
                            *ie.children.get(instance).ok_or_else(|| {
                                Ineligible("fifo child missing".into())
                            })?,
                            *method,
                            &targs,
                            0,
                            true,
                        )?;
                        self.builder.build_unconditional_branch(sk_bb).unwrap();
                        // fast path: header bookkeeping + ring update
                        self.builder.position_at_end(fast_bb);
                        let osame = self
                            .builder
                            .build_int_compare(IntPredicate::NE, other_at, now, "fon")
                            .unwrap();
                        let saved2 = self
                            .builder
                            .build_select(osame, elems, saved, "fsv")
                            .unwrap()
                            .into_int_value();
                        self.store_word(f, base + 1, saved2);
                        self.store_word(f, base + if is_enq { 3 } else { 4 }, now);
                        let fst = self.load_word(f, base + 2);
                        if is_enq {
                            let idx0 =
                                self.builder.build_int_add(fst, elems, "fi").unwrap();
                            let idx = if size.is_power_of_two() {
                                self.builder
                                    .build_and(
                                        idx0,
                                        i64t.const_int((size - 1) as u64, false),
                                        "fim",
                                    )
                                    .unwrap()
                            } else {
                                self.builder
                                    .build_int_unsigned_rem(idx0, szc, "fim")
                                    .unwrap()
                            };
                            let dv = match v {
                                Some(v) => v,
                                None => self.ity(1).const_zero(),
                            };
                            self.store_val_dyn(f, base + 7, idx, fw.max(1), dv);
                            let e2 = self
                                .builder
                                .build_int_add(elems, i64t.const_int(1, false), "fe2")
                                .unwrap();
                            self.store_word(f, base, e2);
                        } else {
                            let f1 = self
                                .builder
                                .build_int_add(fst, i64t.const_int(1, false), "ff1")
                                .unwrap();
                            let f2 = if size.is_power_of_two() {
                                self.builder
                                    .build_and(
                                        f1,
                                        i64t.const_int((size - 1) as u64, false),
                                        "ffm",
                                    )
                                    .unwrap()
                            } else {
                                self.builder
                                    .build_int_unsigned_rem(f1, szc, "ffm")
                                    .unwrap()
                            };
                            self.store_word(f, base + 2, f2);
                            let e2 = self
                                .builder
                                .build_int_sub(elems, i64t.const_int(1, false), "fe2")
                                .unwrap();
                            self.store_word(f, base, e2);
                        }
                        self.builder.build_unconditional_branch(sk_bb).unwrap();
                        self.builder.position_at_end(sk_bb);
                        return Ok(());
                    }
                    // clear and anything else: boxed prim below
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
                    // branchless wset: valid |= cond; value = select
                    let ov = self.load_word(f, base);
                    let cz64 = self
                        .builder
                        .build_int_z_extend(cz, self.ctx.i64_type(), "wsz")
                        .unwrap();
                    let nv = self.builder.build_or(ov, cz64, "wsv").unwrap();
                    self.store_word(f, base, nv);
                    if let Some(v) = v {
                        let oldv = self.load_val(f, base + 1, ww);
                        let selv = self
                            .builder
                            .build_select(cz, v, oldv, "wvv")
                            .unwrap()
                            .into_int_value();
                        self.store_val(f, base + 1, ww, selv);
                    }
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
                // a plain Action call may target an ActionValue method
                // with its result discarded — the interp's call_action
                // executes the body either way
                if !matches!(
                    m.kind,
                    trs_ir::MethodKind::Action | trs_ir::MethodKind::ActionValue
                ) {
                    return nope("value method as action");
                }
                // (* always_enabled *): the caller-side RDY was dropped,
                // so the body is gated on the sibling RDY_<m> value
                // method at call time (the C++ check_rdy wrapper); EN
                // still lands when the caller fires. No RDY method
                // exported = constant ready.
                let rdy_id = if m.always_enabled {
                    let rdy_name =
                        format!("RDY_{}", self.env.d.strings[*method as usize]);
                    self.env
                        .d
                        .strings
                        .iter()
                        .position(|x| x == &rdy_name)
                        .map(|id| id as StrId)
                        .filter(|id| cmod.methods.iter().any(|mm| mm.name == *id))
                } else {
                    None
                };
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
                let rec_argv: Vec<_> = margs
                    .iter()
                    .filter_map(|pa| cf.args.get(&pa.name).copied())
                    .collect();
                self.rec_meth_call(&cf, child, *method, &rec_argv)?;
                if let Some(rid) = rdy_id {
                    // EN is already latched; only the body waits on RDY
                    let r = self.value_call(f, 1, *instance, rid, 0, &[])?;
                    let rz = self.nonzero(r, 1);
                    let bd_bb = self.ctx.append_basic_block(func, "mrdy");
                    self.builder.build_conditional_branch(rz, bd_bb, sk_bb).unwrap();
                    self.builder.position_at_end(bd_bb);
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
                match self.bdpi_import(*ff) {
                    // user BDPI action: direct call (task #22); dynamic
                    // CString args take the boxed trampoline instead
                    Some(imp) if Self::bdpi_lits_ok(&imp, args) => {
                        self.bdpi_emit(f, 1, &imp, args)?;
                    }
                    _ => {
                        self.emit_foreign(f, *ff, args, signed, 0, Some(stop_bb))?;
                    }
                }
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

    #[test]
    fn proto_tags_round_trip_and_fail_closed() {
        use super::{decode_protos, encode_protos, FArgSpec, FnProtos, ForeignSpec};
        let protos = vec![FnProtos {
            sched_foreign: vec![ForeignSpec {
                inst: 3,
                func: 7,
                ret_width: 64,
                args: vec![
                    FArgSpec::Str(11),
                    FArgSpec::Num { width: 0, signed: false },
                    FArgSpec::Num { width: 65, signed: true },
                    FArgSpec::Real,
                    FArgSpec::StrDyn,
                ],
            }],
            sched_prims: vec![],
            exec_foreign: vec![],
            exec_prims: vec![],
        }];
        let bytes = encode_protos(&protos);
        let back = decode_protos(&bytes).expect("round trip");
        assert_eq!(back.len(), 1);
        let args = &back[0].sched_foreign[0].args;
        assert!(matches!(args[0], FArgSpec::Str(11)));
        assert!(matches!(args[1], FArgSpec::Num { width: 0, signed: false }));
        assert!(matches!(args[2], FArgSpec::Num { width: 65, signed: true }));
        assert!(matches!(args[3], FArgSpec::Real));
        assert!(matches!(args[4], FArgSpec::StrDyn));
        // an unknown tag is a corrupted or future-format artifact:
        // decode must fail CLOSED, never fall open as Num
        let mut bad = encode_protos(&protos);
        // first arg record's tag word: protos_count(4) foreign_count(4)
        // inst(4) func(4) ret(4) argc(4) -> tag at byte offset 24
        let tag_off = 4 + 4 + 4 + 4 + 4 + 4;
        bad[tag_off] = 9;
        assert!(super::decode_protos(&bad).is_none());
    }
}
