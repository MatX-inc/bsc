//! The BIR reference interpreter — DESIGN.md P1, the differential oracle.
//!
//! Executes the exported design directly: per (clock, edge) composition,
//! walk each (instance, segment) entry; at `Sched r` latch the rule's
//! CAN_FIRE (with ME inhibitors) and WILL_FIRE; at `Exec r` run the rule
//! body if its latched WILL_FIRE is set.  Rule bodies mutate state in
//! place; primitives implement the begin-of-cycle-snapshot semantics.
//! Correctness and clarity over speed, everywhere.

pub mod format;
pub mod prim;
pub mod value;

use std::collections::HashMap;

use trs_ir as ir;
use trs_ir::{Action, Design, Expr, PrimOp, SchedNode, Stmt, StrId};

use format::Arg;
use prim::Prim;
use value::Value;

// ===============
// Indexed design

struct ModIx {
    ir: usize, // index into design.modules
    defs: HashMap<StrId, usize>,
    rules: HashMap<StrId, usize>,
    methods: HashMap<StrId, usize>,
}

pub struct Interp {
    d: Design,
    /// open files from $fopen, keyed by descriptor (bit 31 set, as
    /// Verilog file descriptors are; 0x8000_0000 reserved for stdout
    /// behavior of the MCD form is not modeled yet)
    files: HashMap<u64, std::fs::File>,
    next_fd: u64,
    mods: Vec<ModIx>,
    mod_by_name: HashMap<StrId, usize>,
    /// instance path -> instance state index
    inst_by_path: HashMap<String, usize>,
    insts: Vec<Inst>,
    finished: Option<i32>,
    cycle: u64,
    /// simulation time of the current posedge.  The default clock is
    /// low 5 / high 5 with initial delay 5, so posedges land at 10, 20, ...
    now: u64,
}

struct Inst {
    #[allow(dead_code)]
    path: String,
    kind: InstKind,
}

enum InstKind {
    User {
        module: usize, // ModIx index
        /// per-cycle latched defs (CAN_FIRE/WILL_FIRE at Sched, task temps)
        latched: HashMap<StrId, Value>,
        /// local child name -> instance index
        children: HashMap<StrId, usize>,
        /// module parameters, bound at instantiation (positional zip of
        /// the child's inputs with the parent's instantiation args)
        params: HashMap<StrId, Value>,
        /// string-valued parameters (only observable as task args)
        str_params: HashMap<StrId, String>,
    },
    Prim(Box<dyn Prim>),
}

/// Evaluation context: method-argument frame plus body-local defs.
/// `memo` caches every def computed on demand (used while latching fire
/// conditions, where no actions can intervene); body execution instead
/// computes defs at their statement positions, which is what preserves
/// read-before-mutate semantics.
#[derive(Default)]
struct Ctx {
    frame: HashMap<StrId, Value>,
    locals: HashMap<StrId, Value>,
    memo: bool,
}

impl Interp {
    pub fn new(d: Design) -> Interp {
        let mods: Vec<ModIx> = d
            .modules
            .iter()
            .enumerate()
            .map(|(i, m)| ModIx {
                ir: i,
                defs: m.defs.iter().enumerate().map(|(k, x)| (x.name, k)).collect(),
                rules: m.rules.iter().enumerate().map(|(k, x)| (x.name, k)).collect(),
                methods: m.methods.iter().enumerate().map(|(k, x)| (x.name, k)).collect(),
            })
            .collect();
        let mod_by_name: HashMap<StrId, usize> =
            d.modules.iter().enumerate().map(|(i, m)| (m.name, i)).collect();

        let mut it = Interp {
            d,
            mods,
            mod_by_name,
            inst_by_path: HashMap::new(),
            insts: Vec::new(),
            files: HashMap::new(),
            next_fd: 0x8000_0001,
            finished: None,
            cycle: 0,
            now: 10,
        };
        let top_mod = it.mod_by_name[&it.d.top];
        it.instantiate("".to_string(), top_mod, HashMap::new(), HashMap::new());
        it
    }

    fn s(&self, id: StrId) -> &str {
        &self.d.strings[id as usize]
    }

    fn instantiate(
        &mut self,
        path: String,
        module: usize,
        params: HashMap<StrId, Value>,
        str_params: HashMap<StrId, String>,
    ) -> usize {
        let slot = self.insts.len();
        self.insts.push(Inst {
            path: path.clone(),
            kind: InstKind::User {
                module,
                latched: HashMap::new(),
                children: HashMap::new(),
                params,
                str_params,
            },
        });
        self.inst_by_path.insert(path.clone(), slot);

        let mir = self.mods[module].ir;
        let child_specs: Vec<(StrId, ir::InstanceKind, Vec<Expr>)> = self.d.modules[mir]
            .instances
            .iter()
            .map(|i| (i.name, i.kind.clone(), i.args.clone()))
            .collect();

        for (name, kind, args) in child_specs {
            let cpath = if path.is_empty() {
                self.s(name).to_string()
            } else {
                format!("{}.{}", path, self.s(name))
            };
            let cidx = match kind {
                ir::InstanceKind::Module(mname) => {
                    let cmod = *self
                        .mod_by_name
                        .get(&mname)
                        .unwrap_or_else(|| panic!("unknown module {:?}", self.s(mname)));
                    // bind the child's parameters: its inputs align
                    // positionally with the instantiation args
                    let cmir = self.mods[cmod].ir;
                    let inputs: Vec<(StrId, ir::PortKind)> = self.d.modules[cmir]
                        .inputs
                        .iter()
                        .map(|p| (p.name, p.kind))
                        .collect();
                    let mut params = HashMap::new();
                    let mut str_params = HashMap::new();
                    for (k, ((pname_, kind_), arg)) in
                        inputs.iter().zip(args.iter()).enumerate()
                    {
                        let _ = k;
                        match kind_ {
                            ir::PortKind::Clock
                            | ir::PortKind::ClockGate
                            | ir::PortKind::Reset => {}
                            _ => match arg {
                                Expr::Str(sid) => {
                                    str_params.insert(*pname_, self.s(*sid).to_string());
                                }
                                _ => {
                                    let mut c = Ctx::default();
                                    let v = self.eval(slot, &mut c, arg);
                                    params.insert(*pname_, v);
                                }
                            },
                        }
                    }
                    self.instantiate(cpath.clone(), cmod, params, str_params)
                }
                ir::InstanceKind::Prim(p) => {
                    let pname = match &p {
                        ir::Primitive::Other { name } => self.s(*name).to_string(),
                        other => panic!("structured primitive kinds not exported yet: {other:?}"),
                    };
                    // evaluate instantiation args in the parent context
                    // (they may reference the parent's own parameters);
                    // clocks/resets are connection info, not values
                    let mut consts: Vec<Value> = Vec::new();
                    let mut strs: Vec<String> = Vec::new();
                    for a in &args {
                        match a {
                            Expr::Clock { .. } | Expr::Reset { .. } | Expr::Gate { .. } => {}
                            Expr::Str(sid) => strs.push(self.s(*sid).to_string()),
                            _ => {
                                let mut c = Ctx::default();
                                consts.push(self.eval(slot, &mut c, a));
                            }
                        }
                    }
                    let idx = self.insts.len();
                    self.insts.push(Inst {
                        path: cpath.clone(),
                        kind: InstKind::Prim(prim::make_prim(&pname, &consts, &strs)),
                    });
                    self.inst_by_path.insert(cpath.clone(), idx);
                    idx
                }
            };
            if let InstKind::User { children, .. } = &mut self.insts[slot].kind {
                children.insert(name, cidx);
            }
        }
        slot
    }

    // ===============
    // Evaluation

    fn module_of(&self, inst: usize) -> usize {
        match &self.insts[inst].kind {
            InstKind::User { module, .. } => *module,
            InstKind::Prim(_) => panic!("primitive treated as user module"),
        }
    }

    fn child_of(&self, inst: usize, name: StrId) -> usize {
        match &self.insts[inst].kind {
            InstKind::User { children, .. } => *children
                .get(&name)
                .unwrap_or_else(|| panic!("unknown child instance {:?}", self.s(name))),
            InstKind::Prim(_) => panic!("primitive has no children"),
        }
    }

    fn latched(&self, inst: usize, name: StrId) -> Option<Value> {
        match &self.insts[inst].kind {
            InstKind::User { latched, .. } => latched.get(&name).cloned(),
            InstKind::Prim(_) => None,
        }
    }

    fn set_latched(&mut self, inst: usize, name: StrId, v: Value) {
        if let InstKind::User { latched, .. } = &mut self.insts[inst].kind {
            latched.insert(name, v);
        }
    }

    /// Evaluate an expression in an instance context.  Body-local defs and
    /// per-cycle latched defs win over recomputation; in memo contexts,
    /// on-demand def values are cached.
    fn eval(&mut self, inst: usize, ctx: &mut Ctx, e: &Expr) -> Value {
        match e {
            Expr::Const { width, limbs } => Value::from_limbs32(*width, limbs),
            Expr::Str(_) => panic!("string used as value (only valid as task arg)"),
            Expr::Def(name) => {
                if let Some(v) = ctx.locals.get(name) {
                    return v.clone();
                }
                if let Some(v) = self.latched(inst, *name) {
                    return v;
                }
                let module = self.module_of(inst);
                let mir = self.mods[module].ir;
                let di = *self.mods[module]
                    .defs
                    .get(name)
                    .unwrap_or_else(|| panic!("unknown def {:?}", self.s(*name)));
                let d = self.d.modules[mir].defs[di].clone();
                let v = self.eval(inst, ctx, &d.expr);
                if ctx.memo {
                    ctx.locals.insert(*name, v.clone());
                }
                v
            }
            Expr::Port(name) | Expr::Param(name) => {
                if let Some(v) = ctx.frame.get(name) {
                    return v.clone();
                }
                if let InstKind::User { params, .. } = &self.insts[inst].kind {
                    if let Some(v) = params.get(name) {
                        return v.clone();
                    }
                }
                // module input ports outside a method frame: clock gates
                // and reset lines read as asserted-off (1)
                Value::from_u64(1, 1)
            }
            Expr::MethCall { width, instance, method, args, .. } => {
                let argv: Vec<Value> =
                    args.iter().map(|a| self.eval(inst, ctx, a)).collect();
                let child = self.child_of(inst, *instance);
                self.call_value(child, *method, &argv, *width)
            }
            Expr::MethValue { width, instance, method } => {
                let child = self.child_of(inst, *instance);
                self.call_value(child, *method, &[], *width)
            }
            Expr::TaskValue { width, cookie } => {
                // value produced by the paired Task action earlier in this
                // body, stored under a synthetic key
                ctx.locals
                    .get(&cookie_key(*cookie))
                    .cloned()
                    .or_else(|| self.latched(inst, cookie_key(*cookie)))
                    .unwrap_or_else(|| Value::undet(*width))
            }
            Expr::ForeignCall { width, func, args } => {
                let fname = self.s(*func).to_string();
                let argv: Vec<Arg> = args
                    .iter()
                    .map(|a| self.eval_arg(inst, ctx, a, false))
                    .collect();
                self.foreign_value(&fname, &argv, *width)
            }
            Expr::Gate { .. } => Value::from_u64(1, 1),
            Expr::Clock { .. } | Expr::Reset { .. } => Value::from_u64(1, 1),
            Expr::Prim { op, width, args } => self.eval_prim(inst, ctx, *op, *width, args),
            Expr::If { width, cond, then_, else_ } => {
                if self.eval(inst, ctx, cond).as_bool() {
                    self.eval(inst, ctx, then_).zext(*width)
                } else {
                    self.eval(inst, ctx, else_).zext(*width)
                }
            }
            Expr::Case { width, scrutinee, arms, default } => {
                let s = self.eval(inst, ctx, scrutinee);
                for (k, v) in arms {
                    if s.as_u64() == *k && s.width <= 64 {
                        return self.eval(inst, ctx, v).zext(*width);
                    }
                }
                self.eval(inst, ctx, default).zext(*width)
            }
        }
    }

    fn eval_prim(
        &mut self,
        inst: usize,
        ctx: &mut Ctx,
        op: PrimOp,
        w: u32,
        args: &[Expr],
    ) -> Value {
        match op {
            PrimOp::And => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                a.and(&b, w)
            }
            PrimOp::Or => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                a.or(&b, w)
            }
            PrimOp::Xor => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                a.xor(&b, w)
            }
            PrimOp::Not => self.eval(inst, ctx, &args[0]).not(w),
            PrimOp::Add => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                a.add(&b, w)
            }
            PrimOp::Sub => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                a.sub(&b, w)
            }
            PrimOp::Neg => self.eval(inst, ctx, &args[0]).neg(w),
            PrimOp::Mul => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                a.mul(&b, w)
            }
            PrimOp::Quot => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                a.quot(&b, w)
            }
            PrimOp::Rem => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                a.rem(&b, w)
            }
            PrimOp::Eq => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                Value::from_u64(1, a.eq(&b) as u64)
            }
            PrimOp::Ult => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                Value::from_u64(1, a.ult(&b) as u64)
            }
            PrimOp::Ule => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                Value::from_u64(1, a.ule(&b) as u64)
            }
            PrimOp::Slt => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                Value::from_u64(1, a.slt(&b) as u64)
            }
            PrimOp::Sle => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                Value::from_u64(1, a.sle(&b) as u64)
            }
            PrimOp::Shl => {
                let a = self.eval(inst, ctx, &args[0]);
                let sh = self.eval(inst, ctx, &args[1]).as_u64();
                a.shl(sh, w)
            }
            PrimOp::Lshr => {
                let a = self.eval(inst, ctx, &args[0]);
                let sh = self.eval(inst, ctx, &args[1]).as_u64();
                a.lshr(sh, w)
            }
            PrimOp::Ashr => {
                let a = self.eval(inst, ctx, &args[0]);
                let sh = self.eval(inst, ctx, &args[1]).as_u64();
                a.ashr(sh, w)
            }
            PrimOp::Extract => {
                // PrimExtract e hi lo
                let a = self.eval(inst, ctx, &args[0]);
                let hi = self.eval(inst, ctx, &args[1]).as_u64();
                let lo = self.eval(inst, ctx, &args[2]).as_u64();
                a.extract(hi, lo, w)
            }
            PrimOp::Concat => {
                // left-to-right, first is most significant
                let mut acc = self.eval(inst, ctx, &args[0]);
                let mut accw = acc.width;
                for k in 1..args.len() {
                    let nxt = self.eval(inst, ctx, &args[k]);
                    accw += nxt.width;
                    acc = acc.concat(&nxt, accw);
                }
                acc.zext(w)
            }
            PrimOp::ZeroExt => self.eval(inst, ctx, &args[0]).zext(w),
            PrimOp::SignExt => self.eval(inst, ctx, &args[0]).sext(w),
            PrimOp::Select => {
                panic!("PrimArrayDynSelect should be expanded by simPackageOpt")
            }
        }
    }

    fn eval_arg(&mut self, inst: usize, ctx: &mut Ctx, e: &Expr, signed: bool) -> Arg {
        match e {
            Expr::Str(s) => Arg::Str(self.s(*s).to_string()),
            Expr::Port(name) | Expr::Param(name) => {
                if let InstKind::User { str_params, .. } = &self.insts[inst].kind {
                    if let Some(sv) = str_params.get(name) {
                        return Arg::Str(sv.clone());
                    }
                }
                Arg::Val(self.eval(inst, ctx, e), signed)
            }
            _ => Arg::Val(self.eval(inst, ctx, e), signed),
        }
    }

    // ===============
    // Method calls

    fn method_ctx(&mut self, callee_mod: usize, mi: usize, argv: &[Value], memo: bool) -> Ctx {
        let mir = self.mods[callee_mod].ir;
        let m = &self.d.modules[mir].methods[mi];
        Ctx {
            frame: m.args.iter().zip(argv.iter()).map(|(p, v)| (p.name, v.clone())).collect(),
            locals: HashMap::new(),
            memo,
        }
    }

    fn call_value(&mut self, callee: usize, method: StrId, argv: &[Value], w: u32) -> Value {
        match &mut self.insts[callee].kind {
            InstKind::Prim(p) => {
                let mname = self.d.strings[method as usize].clone();
                p.value_method(&mname, argv, self.cycle)
            }
            InstKind::User { module, .. } => {
                let module = *module;
                let mi = *self.mods[module]
                    .methods
                    .get(&method)
                    .unwrap_or_else(|| panic!("unknown method {:?}", self.s(method)));
                let mir = self.mods[module].ir;
                let result = self.d.modules[mir].methods[mi].result.clone();
                let mut ctx = self.method_ctx(module, mi, argv, true);
                match result {
                    Some(r) => self.eval(callee, &mut ctx, &r).zext(w),
                    None => panic!("value call to method without result"),
                }
            }
        }
    }

    fn call_action(&mut self, callee: usize, method: StrId, argv: &[Value]) {
        match &mut self.insts[callee].kind {
            InstKind::Prim(p) => {
                let mname = self.d.strings[method as usize].clone();
                p.action_method(&mname, argv, self.cycle);
            }
            InstKind::User { module, .. } => {
                let module = *module;
                let mi = *self.mods[module]
                    .methods
                    .get(&method)
                    .unwrap_or_else(|| panic!("unknown method {:?}", self.s(method)));
                let mir = self.mods[module].ir;
                let body: Vec<Stmt> = self.d.modules[mir].methods[mi].body.clone();
                let mut ctx = self.method_ctx(module, mi, argv, false);
                for st in &body {
                    self.exec_stmt(callee, &mut ctx, st);
                }
            }
        }
    }

    fn call_actionvalue(&mut self, callee: usize, method: StrId, argv: &[Value]) -> Value {
        match &mut self.insts[callee].kind {
            InstKind::Prim(p) => {
                let mname = self.d.strings[method as usize].clone();
                p.actionvalue_method(&mname, argv, self.cycle)
            }
            InstKind::User { module, .. } => {
                let module = *module;
                let mi = *self.mods[module]
                    .methods
                    .get(&method)
                    .unwrap_or_else(|| panic!("unknown method {:?}", self.s(method)));
                let mir = self.mods[module].ir;
                let body: Vec<Stmt> = self.d.modules[mir].methods[mi].body.clone();
                let result = self.d.modules[mir].methods[mi].result.clone();
                let mut ctx = self.method_ctx(module, mi, argv, false);
                for st in &body {
                    self.exec_stmt(callee, &mut ctx, st);
                }
                match result {
                    Some(r) => self.eval(callee, &mut ctx, &r),
                    None => panic!("actionvalue method without result"),
                }
            }
        }
    }

    // ===============
    // Statements and actions

    /// Execute one body statement: defs are computed at their exact
    /// position (a later action must not affect them).
    fn exec_stmt(&mut self, inst: usize, ctx: &mut Ctx, st: &Stmt) {
        if self.finished.is_some() {
            return;
        }
        match st {
            Stmt::Def(name) => {
                let v = self.eval(inst, ctx, &Expr::Def(*name));
                ctx.locals.insert(*name, v);
            }
            Stmt::Action(a) => self.exec_action(inst, ctx, a),
            Stmt::AvAction { def, action } => match action {
                Action::MethCall { instance, method, cond, args, .. } => {
                    let dw = self.def_width(inst, *def);
                    if !self.eval(inst, ctx, cond).as_bool() {
                        ctx.locals.insert(*def, Value::undet(dw));
                        return;
                    }
                    let argv: Vec<Value> =
                        args.iter().map(|x| self.eval(inst, ctx, x)).collect();
                    let child = self.child_of(inst, *instance);
                    let v = self.call_actionvalue(child, *method, &argv);
                    ctx.locals.insert(*def, v.zext(dw));
                }
                a @ Action::Task { temp, width, .. } => {
                    self.exec_action(inst, ctx, a);
                    let v = temp
                        .and_then(|t| ctx.locals.get(&t).cloned())
                        .unwrap_or_else(|| Value::undet((*width).max(1)));
                    ctx.locals.insert(*def, v);
                }
                other => panic!("AvAction with non-method action: {other:?}"),
            },
            Stmt::Cond { cond, then_, else_ } => {
                let branch = if self.eval(inst, ctx, cond).as_bool() {
                    then_
                } else {
                    else_
                };
                for st in branch.clone() {
                    self.exec_stmt(inst, ctx, &st);
                }
            }
        }
    }

    fn def_width(&self, inst: usize, name: StrId) -> u32 {
        let module = self.module_of(inst);
        let mir = self.mods[module].ir;
        match self.mods[module].defs.get(&name) {
            Some(di) => self.d.modules[mir].defs[*di].width,
            None => 64,
        }
    }

    fn exec_action(&mut self, inst: usize, ctx: &mut Ctx, a: &Action) {
        if self.finished.is_some() {
            return;
        }
        match a {
            Action::MethCall { instance, method, cond, args, .. } => {
                if !self.eval(inst, ctx, cond).as_bool() {
                    return;
                }
                let argv: Vec<Value> =
                    args.iter().map(|x| self.eval(inst, ctx, x)).collect();
                let child = self.child_of(inst, *instance);
                self.call_action(child, *method, &argv);
            }
            Action::Foreign { func, cond, args, signed } => {
                if !self.eval(inst, ctx, cond).as_bool() {
                    return;
                }
                let fname = self.s(*func).to_string();
                let argv: Vec<Arg> = args
                    .iter()
                    .zip(signed.iter().chain(std::iter::repeat(&false)))
                    .map(|(x, sg)| self.eval_arg(inst, ctx, x, *sg))
                    .collect();
                self.foreign_action(&fname, &argv);
            }
            Action::Task { func, cookie, temp, width, cond, args, signed } => {
                if !self.eval(inst, ctx, cond).as_bool() {
                    return;
                }
                let fname = self.s(*func).to_string();
                let argv: Vec<Arg> = args
                    .iter()
                    .zip(signed.iter().chain(std::iter::repeat(&false)))
                    .map(|(x, sg)| self.eval_arg(inst, ctx, x, *sg))
                    .collect();
                let v = self.foreign_value(&fname, &argv, *width);
                ctx.locals.insert(cookie_key(*cookie), v.clone());
                if let Some(t) = temp {
                    ctx.locals.insert(*t, v);
                }
            }
        }
    }

    // ===============
    // System tasks

    fn write_fd(&mut self, fd: u64, text: &str) {
        use std::io::Write;
        if fd == 0x8000_0000 {
            print!("{text}");
        } else if let Some(f) = self.files.get_mut(&fd) {
            let _ = f.write_all(text.as_bytes());
        }
    }

    fn foreign_action(&mut self, name: &str, args: &[Arg]) {
        match name {
            "$fdisplay" | "$fwrite" | "$fdisplayh" | "$fwriteh"
            | "$fdisplayb" | "$fwriteb" | "$fdisplayo" | "$fwriteo" => {
                let base = match name.chars().last() {
                    Some('h') => 16,
                    Some('b') => 2,
                    Some('o') => 8,
                    _ => 10,
                };
                let fd = match args.first() {
                    Some(Arg::Val(v, _)) => v.as_u64(),
                    _ => 0x8000_0000,
                };
                let mut text = format::format_args(&args[1..], base, self.now);
                if name.starts_with("$fdisplay") {
                    text.push('\n');
                }
                self.write_fd(fd, &text);
            }
            "$fclose" => {
                if let Some(Arg::Val(v, _)) = args.first() {
                    self.files.remove(&v.as_u64());
                }
            }
            "$fflush" => {
                use std::io::Write;
                if let Some(Arg::Val(v, _)) = args.first() {
                    if let Some(f) = self.files.get_mut(&v.as_u64()) {
                        let _ = f.flush();
                    }
                } else {
                    let _ = std::io::stdout().flush();
                }
            }
            "$display" => println!("{}", format::format_args(args, 10, self.now)),
            "$displayh" => println!("{}", format::format_args(args, 16, self.now)),
            "$displayb" => println!("{}", format::format_args(args, 2, self.now)),
            "$displayo" => println!("{}", format::format_args(args, 8, self.now)),
            "$write" => print!("{}", format::format_args(args, 10, self.now)),
            "$writeh" => print!("{}", format::format_args(args, 16, self.now)),
            "$writeb" => print!("{}", format::format_args(args, 2, self.now)),
            "$writeo" => print!("{}", format::format_args(args, 8, self.now)),
            "$error" | "$warning" | "$info" => {
                // severity prefix, then the message (dollar_display.cxx)
                let sev = &name[1..];
                let mut t = sev[..1].to_uppercase();
                t.push_str(&sev[1..]);
                println!("{}: {}", t, format::format_args(args, 10, self.now));
            }
            "$fatal" => {
                println!("Fatal: {}", format::format_args(args, 10, self.now));
                self.finished = Some(1);
            }
            "$finish" => {
                let code = match args.first() {
                    Some(Arg::Val(v, _)) => v.as_u64() as i32,
                    _ => 0,
                };
                self.finished = Some(code);
            }
            "$stop" => self.finished = Some(0),
            "$dumpvars" | "$dumpon" | "$dumpoff" => {} // waves: P2
            other => panic!("trs-interp: unimplemented system task {other:?}"),
        }
    }

    fn foreign_value(&mut self, name: &str, args: &[Arg], w: u32) -> Value {
        match name {
            "$time" | "$stime" => Value::from_u64(w.max(1), self.now),
            "$fopen" => {
                let path = match args.first() {
                    Some(Arg::Str(s)) => s.clone(),
                    _ => return Value::zero(w.max(1)),
                };
                let write_mode = !matches!(args.get(1), Some(Arg::Str(m)) if m.starts_with('r'));
                let f = if write_mode {
                    std::fs::File::create(&path)
                } else {
                    std::fs::File::open(&path)
                };
                match f {
                    Ok(f) => {
                        let fd = self.next_fd;
                        self.next_fd += 1;
                        self.files.insert(fd, f);
                        Value::from_u64(w.max(32), fd)
                    }
                    Err(_) => Value::zero(w.max(1)),
                }
            }
            "$test$plusargs" => Value::from_u64(1, 0), // no plusargs yet
            "$fgetc" => {
                use std::io::Read;
                let fd = match args.first() {
                    Some(Arg::Val(v, _)) => v.as_u64(),
                    _ => return Value::from_u64(w.max(32), u32::MAX as u64),
                };
                let mut byte = [0u8; 1];
                if let Some(f) = self.files.get_mut(&fd) {
                    if f.read_exact(&mut byte).is_ok() {
                        return Value::from_u64(w.max(32), byte[0] as u64);
                    }
                }
                // EOF / bad fd: -1
                Value::from_u64(w.max(32), 0xFFFF_FFFF)
            }
            "$fclose" => {
                if let Some(Arg::Val(v, _)) = args.first() {
                    self.files.remove(&v.as_u64());
                }
                Value::zero(w.max(1))
            }
            other => panic!("trs-interp: unimplemented value task {other:?} ({args:?})"),
        }
    }

    // ===============
    // Cycle execution

    fn latch_rule(&mut self, inst: usize, rule_name: StrId, cross_inh: &[(usize, StrId)]) {
        let module = self.module_of(inst);
        let mir = self.mods[module].ir;
        let ri = match self.mods[module].rules.get(&rule_name) {
            Some(ri) => *ri,
            None => return, // method node in a segment: nothing to latch
        };
        let r = self.d.modules[mir].rules[ri].clone();
        let mut ctx = Ctx { memo: true, ..Default::default() };

        let mut cf = self.eval(inst, &mut ctx, &Expr::Def(r.can_fire));
        // intra-module ME inhibitors (earlier disjoint rules' CFs)
        for other in &r.me_inhibits {
            let other_ri = self.mods[module].rules[other];
            let other_cf = self.d.modules[mir].rules[other_ri].can_fire;
            if let Some(v) = self.latched(inst, other_cf) {
                if v.as_bool() {
                    cf = Value::zero(1);
                }
            }
        }
        // cross-module inhibitors targeting this rule
        for (other_inst, other_cf) in cross_inh {
            if let Some(v) = self.latched(*other_inst, *other_cf) {
                if v.as_bool() {
                    cf = Value::zero(1);
                }
            }
        }
        self.set_latched(inst, r.can_fire, cf);
        // recompute the WILL_FIRE cone against the (possibly inhibited)
        // latched CAN_FIRE, not the memoized pre-inhibitor values
        let mut wf_ctx = Ctx { memo: true, ..Default::default() };
        let wf = self.eval(inst, &mut wf_ctx, &Expr::Def(r.will_fire));
        self.set_latched(inst, r.will_fire, wf);
    }

    fn exec_rule(&mut self, inst: usize, rule_name: StrId) {
        let module = self.module_of(inst);
        let mir = self.mods[module].ir;
        let ri = match self.mods[module].rules.get(&rule_name) {
            Some(ri) => *ri,
            None => return,
        };
        let r = self.d.modules[mir].rules[ri].clone();
        let fire = self
            .latched(inst, r.will_fire)
            .map(|v| v.as_bool())
            .unwrap_or(false);
        if !fire {
            return;
        }
        let mut ctx = Ctx::default();
        for st in &r.body {
            self.exec_stmt(inst, &mut ctx, st);
        }
    }

    /// Run until $finish or the cycle limit.  Returns the exit code.
    pub fn run(&mut self, max_cycles: u64) -> i32 {
        // pre-resolve composition structure
        let comps = self.d.compositions.clone();
        if comps.len() != 1 {
            panic!(
                "trs-interp: exactly one clock domain supported in P1 (got {})",
                comps.len()
            );
        }
        let comp = &comps[0];

        // (instance idx, module idx, segment idx)
        let entries: Vec<(usize, usize, usize)> = comp
            .entries
            .iter()
            .map(|e| {
                let path = self.s(e.instance).to_string();
                let ii = *self
                    .inst_by_path
                    .get(&path)
                    .unwrap_or_else(|| panic!("unknown instance path {path:?}"));
                (ii, self.module_of(ii), e.segment as usize)
            })
            .collect();

        // cross-inhibit lookup: (later inst, later rule) -> earlier CFs
        let mut cross: HashMap<(usize, StrId), Vec<(usize, StrId)>> = HashMap::new();
        for (earlier, later) in &comp.cross_inhibits {
            let (e_inst, e_rule) = self.split_qual(*earlier);
            let (l_inst, l_rule) = self.split_qual(*later);
            let e_mod = self.module_of(e_inst);
            let e_mir = self.mods[e_mod].ir;
            let e_ri = self.mods[e_mod].rules[&e_rule];
            let e_cf = self.d.modules[e_mir].rules[e_ri].can_fire;
            cross.entry((l_inst, l_rule)).or_default().push((e_inst, e_cf));
        }

        let ticks: Vec<(usize, StrId)> = comp
            .ticks
            .iter()
            .map(|t| {
                let ipath = self.s(t.instance).to_string();
                let ppath = if ipath.is_empty() {
                    self.s(t.prim).to_string()
                } else {
                    format!("{}.{}", ipath, self.s(t.prim))
                };
                let ii = *self
                    .inst_by_path
                    .get(&ppath)
                    .unwrap_or_else(|| panic!("unknown tick instance {ppath:?}"));
                (ii, t.port)
            })
            .collect();

        while self.finished.is_none() && self.cycle < max_cycles {
            // new cycle: clear latched state
            for i in 0..self.insts.len() {
                if let InstKind::User { latched, .. } = &mut self.insts[i].kind {
                    latched.clear();
                }
            }

            for &(inst, module, seg) in &entries {
                let mir = self.mods[module].ir;
                let sched = &self.d.modules[mir].schedule;
                let ms = &sched.domains[0];
                let nodes: Vec<SchedNode> = ms.segments[seg].nodes.clone();
                for node in nodes {
                    if self.finished.is_some() {
                        break;
                    }
                    match node {
                        SchedNode::Sched(r) => {
                            let ci = cross.get(&(inst, r)).cloned().unwrap_or_default();
                            self.latch_rule(inst, r, &ci);
                        }
                        SchedNode::Exec(r) => self.exec_rule(inst, r),
                    }
                }
            }

            // end-of-edge ticks
            for &(inst, port) in &ticks {
                if let InstKind::Prim(p) = &mut self.insts[inst].kind {
                    let pname = self.d.strings[port as usize].clone();
                    p.tick(&pname);
                }
            }

            self.cycle += 1;
            self.now += 10;
        }
        self.finished.unwrap_or(0)
    }

    /// "a.b.RL_r" -> (instance index of "a.b", rule StrId of "RL_r")
    fn split_qual(&mut self, q: StrId) -> (usize, StrId) {
        let s = self.s(q).to_string();
        let (ipath, rname) = match s.rfind('.') {
            Some(k) => (&s[..k], &s[k + 1..]),
            None => ("", s.as_str()),
        };
        let inst = *self
            .inst_by_path
            .get(ipath)
            .unwrap_or_else(|| panic!("unknown instance {ipath:?}"));
        // find the StrId for the local rule name
        let rid = self
            .d
            .strings
            .iter()
            .position(|x| x == rname)
            .unwrap_or_else(|| panic!("unknown rule name {rname:?}"))
            as StrId;
        (inst, rid)
    }
}

fn cookie_key(cookie: u32) -> StrId {
    // cookies live in a synthetic key space far above real string ids
    0x8000_0000 | cookie
}

pub fn run_file(path: &str, max_cycles: u64) -> Result<i32, String> {
    let bytes = std::fs::read(path).map_err(|e| format!("{path}: {e}"))?;
    let design = Design::decode(&bytes).map_err(|e| e.to_string())?;
    let mut interp = Interp::new(design);
    Ok(interp.run(max_cycles))
}
