//! Top-level module argument/parameter bindings and always_enabled
//! auto-fire arming — the trs side of the two Bluesim top-level
//! restrictions lifted for the -trs flow (bsc's SimExpand skips
//! EBSimTopLevelArgOrParam and EBSimEnablePragma under -trs).
//!
//! Contract (v1, deliberately narrow — every unsupported shape is a
//! loud, specific refusal):
//!
//! - A top module's Bit-typed arguments and parameters bind to
//!   constants: `+NAME=value` (or `--bind NAME=value`) at `trs run`
//!   and `trs link`.  Widths come from the declaration; a value that
//!   does not fit, an unknown explicit name, or a missing binding is
//!   an error.  Bindings supplied at link are BAKED into compiled
//!   bodies (port_consts folding) — the artifact records them in its
//!   .opts and re-supplies them per run; different values require a
//!   relink.  `trs run` takes them per-run.
//! - Consumed `+NAME=value` arguments are bindings, not plusargs; a
//!   `+` argument matching no top-level argument stays an ordinary
//!   plusarg (existing designs keep their behavior — a design with no
//!   top-level arguments never enters this code).
//! - always_enabled Action methods of the top auto-fire in batch mode
//!   on every edge of their clock, at their schedule position (the
//!   method's cut position in the composition), with EN constant true
//!   (the Verilog always_enabled contract: EN is tied high, so reads
//!   of the EN port see 1 for the whole edge, not latch-at-call).
//!   Method arguments bind to constants as `+<method>.<arg>=value`.
//!   always_enabled implies RDY constant true: asserted at arm time
//!   (a sibling RDY_<m> method must be absent or a non-zero
//!   constant), and call_action's check_rdy still guards each fire.
//! - Auto-fire designs run INTERPRETED: the jit plan declines with
//!   "top always_enabled autofire" (the sweep's why= column).
//!   Binding-only designs compile normally (params fold into
//!   port_consts / wide_consts).
//! - Refused loudly: ActionValue or enabled_when_ready methods,
//!   zero-width/non-Bit arguments (also refused by bsc), input clocks
//!   or resets beyond the defaults on a top with bindable arguments,
//!   and `trs link --interactive`/`--exe` for designs that bind or
//!   auto-fire (the interactive bk_* surface cannot drive either).

use crate::value::Value;
use trs_ir as ir;

/// One NAME=value binding from the CLI.  `explicit` = `--bind` or a
/// link-time `+` (unknown names are errors); a run-time `+` is
/// opportunistic (unmatched names stay plusargs).
#[derive(Clone, Debug)]
pub struct TopBind {
    pub name: String,
    pub value: String,
    pub explicit: bool,
}

/// Parse "NAME=value" (the CLI's spelling after `+` / `--bind`).
pub fn parse_bind(s: &str, explicit: bool) -> Result<TopBind, String> {
    match s.split_once('=') {
        Some((n, v)) if !n.is_empty() && !v.is_empty() => Ok(TopBind {
            name: n.to_string(),
            value: v.to_string(),
            explicit,
        }),
        _ => Err(format!("malformed binding `{s}' (expected NAME=value)")),
    }
}

pub(crate) struct ResolvedBinds {
    /// Constants for the top instance's params map: bound arguments
    /// and parameters, auto-fire method arguments, and EN_<m> = 1 for
    /// each auto-fired method.
    pub params: Vec<(ir::StrId, Value)>,
    /// always_enabled Action methods to auto-fire, in interface
    /// order, each with its constant argument values.
    pub autofire: Vec<(ir::StrId, Vec<Value>)>,
    /// (composition index, entry index) -> autofire indices to invoke
    /// after that entry's nodes: each method's EXEC schedule position.
    /// A method's Exec node cuts the top's own schedule at the LAST
    /// segment cut naming it (Sched(m) always precedes Exec(m), and a
    /// method has exactly those two nodes, so at most two cuts name
    /// it); the anchor is the latest node-bearing top segment at or
    /// before that cut, mapped to its composition entry.  Cut-only
    /// segments never enter the composition (the exporter skips
    /// top-method nodes), hence this indirection.
    pub autofire_at: std::collections::HashMap<(usize, usize), Vec<usize>>,
    /// composition index -> autofire indices to invoke BEFORE the
    /// entry walk (methods whose Exec cut precedes every node-bearing
    /// top segment).
    pub autofire_pre: std::collections::HashMap<usize, Vec<usize>>,
    /// Identity salt folded into bir_hash: same design + different
    /// bindings must never match a compiled artifact's stamp (the
    /// baked port_consts differ).  0 when nothing is bound.
    pub salt: u64,
    /// Raw "NAME=value" strings consumed from the `+` namespace —
    /// the loader filters these OUT of the design-visible plusargs.
    pub consumed_plus: Vec<String>,
}

fn str_id(d: &ir::Design, s: &str) -> Option<ir::StrId> {
    d.strings.iter().position(|x| x == s).map(|i| i as ir::StrId)
}

/// Resolve an expression to a constant u64 by chasing def references
/// (bounded depth).  None = not (provably) constant.
fn resolve_const(m: &ir::Module, e: &ir::Expr, depth: u32) -> Option<u64> {
    if depth == 0 {
        return None;
    }
    match e {
        ir::Expr::Const { limbs, .. } => {
            let mut v = 0u64;
            for (i, &l) in limbs.iter().enumerate().take(2) {
                v |= (l as u64) << (32 * i);
            }
            // any set bit above 64 still means "non-zero"
            if limbs.iter().skip(2).any(|&l| l != 0) {
                v |= 1 << 63;
            }
            Some(v)
        }
        ir::Expr::Def(n) => {
            let d = m.defs.iter().find(|d| d.name == *n)?;
            resolve_const(m, &d.expr, depth - 1)
        }
        _ => None,
    }
}

/// Parse an unsigned integer of arbitrary width: decimal, 0x hex, or
/// 0b binary, with optional `_` separators.  Returns LE 64-bit limbs.
fn parse_uint(text: &str) -> Result<Vec<u64>, String> {
    let (digits, radix) = if let Some(h) = text.strip_prefix("0x").or_else(|| text.strip_prefix("0X")) {
        (h, 16u32)
    } else if let Some(b) = text.strip_prefix("0b").or_else(|| text.strip_prefix("0B")) {
        (b, 2u32)
    } else {
        (text, 10u32)
    };
    if digits.is_empty() {
        return Err("empty value".into());
    }
    let mut limbs: Vec<u64> = vec![0];
    for c in digits.chars() {
        if c == '_' {
            continue;
        }
        let dv = c.to_digit(radix).ok_or_else(|| {
            format!("invalid digit `{c}' for base {radix}")
        })?;
        // limbs = limbs * radix + dv
        let mut carry = dv as u128;
        for l in limbs.iter_mut() {
            let t = (*l as u128) * (radix as u128) + carry;
            *l = t as u64;
            carry = t >> 64;
        }
        if carry != 0 {
            limbs.push(carry as u64);
        }
    }
    Ok(limbs)
}

fn bit_len(limbs: &[u64]) -> u32 {
    for (i, &l) in limbs.iter().enumerate().rev() {
        if l != 0 {
            return (i as u32) * 64 + (64 - l.leading_zeros());
        }
    }
    0
}

/// FNV-1a over the canonical binding list (sorted by name, values as
/// LE limb hex) — the bind identity salt.
fn bind_salt(bound: &[(String, Vec<u64>)]) -> u64 {
    let mut names: Vec<&(String, Vec<u64>)> = bound.iter().collect();
    names.sort_by(|a, b| a.0.cmp(&b.0));
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    let mut eat = |bytes: &[u8]| {
        for &b in bytes {
            h ^= b as u64;
            h = h.wrapping_mul(0x1000_0000_01b3);
        }
    };
    for (n, ls) in names {
        eat(n.as_bytes());
        eat(&[b'=']);
        for l in ls {
            eat(&l.to_le_bytes());
        }
        eat(&[b';']);
    }
    h
}

pub(crate) fn resolve(
    d: &ir::Design,
    binds: &[TopBind],
) -> Result<ResolvedBinds, String> {
    let top = d
        .modules
        .iter()
        .find(|m| m.name == d.top)
        .ok_or_else(|| "top module not found".to_string())?;
    let s = |id: ir::StrId| d.strings[id as usize].as_str();

    // ---- bindable surface ----
    // top-level arguments/parameters: the module's MethodArg inputs
    let arg_ports: Vec<(ir::StrId, u32)> = top
        .inputs
        .iter()
        .filter(|p| p.kind == ir::PortKind::MethodArg)
        .map(|p| (p.name, p.width))
        .collect();
    // always_enabled methods (arm even with no bindings on the CLI)
    let mut autofire: Vec<(ir::StrId, Vec<(ir::StrId, u32)>)> = Vec::new();
    for m in &top.methods {
        if !m.always_enabled {
            continue;
        }
        match m.kind {
            ir::MethodKind::Action => {}
            ir::MethodKind::Value => continue, // EN is meaningless
            ir::MethodKind::ActionValue => {
                return Err(format!(
                    "top-level always_enabled method `{}' is an \
                     ActionValue method; auto-firing supports Action \
                     methods only (v1)",
                    s(m.name)
                ));
            }
        }
        // always_enabled implies RDY constant true: assert it at arm
        // time via the sibling RDY_<m> method (what check_rdy
        // evaluates; absent = constant ready).  The result is
        // typically a def reference to a constant def — chase the
        // chain rather than pattern-matching one shape.
        if let Some(rdy_id) = str_id(d, &format!("RDY_{}", s(m.name))) {
            if let Some(rm) = top.methods.iter().find(|x| x.name == rdy_id) {
                let const_true = match &rm.result {
                    None => true,
                    Some(e) => {
                        matches!(resolve_const(top, e, 16), Some(v) if v != 0)
                    }
                };
                if !const_true {
                    return Err(format!(
                        "top-level always_enabled method `{}' has a \
                         non-constant-true RDY; auto-firing requires \
                         RDY constant true",
                        s(m.name)
                    ));
                }
            }
        }
        autofire.push((
            m.name,
            m.args.iter().map(|a| (a.name, a.width)).collect(),
        ));
    }

    if arg_ports.is_empty() && autofire.is_empty() {
        // no bindable surface at all: explicit binds are errors, `+`
        // candidates stay plusargs, and nothing else changes
        if let Some(b) = binds.iter().find(|b| b.explicit) {
            return Err(format!(
                "unknown top-level binding `{}': the top module has no \
                 arguments, parameters, or always_enabled methods",
                b.name
            ));
        }
        return Ok(ResolvedBinds {
            params: Vec::new(),
            autofire: Vec::new(),
            autofire_at: Default::default(),
            autofire_pre: Default::default(),
            salt: 0,
            consumed_plus: Vec::new(),
        });
    }

    // with bindable arguments in play, refuse input clocks or resets
    // beyond the defaults: bindings supply constants, never waveforms
    // (mirrors bsc's -trs check; this one also guards stale .birs)
    // structural rule (bsc's -trs check does the precise name-based
    // one; Design::default_reset carries a legacy string, not a port
    // name): the runtime drives exactly one input clock (the default
    // waveform) and one reset (the kernel reset node) — a second of
    // either alongside bindable arguments is refused, never silently
    // left unticking
    if !arg_ports.is_empty() {
        for kind in [ir::PortKind::Clock, ir::PortKind::Reset] {
            let ins: Vec<&str> = top
                .inputs
                .iter()
                .filter(|p| p.kind == kind)
                .map(|p| s(p.name))
                .collect();
            if ins.len() > 1 {
                return Err(format!(
                    "top-level module arguments bind to constants; \
                     additional input {}s are not supported \
                     (found: {})",
                    if kind == ir::PortKind::Clock { "clock" } else { "reset" },
                    ins.join(", ")
                ));
            }
        }
    }
    // zero-width (or string-typed: both export width 0) arguments
    // cannot bind — bsc refuses these too; this guards stale .birs
    if let Some((n, _)) = arg_ports.iter().find(|(_, w)| *w == 0) {
        return Err(format!(
            "top-level argument `{}' has width 0 (zero-width or \
             non-Bit); it cannot be bound",
            s(*n)
        ));
    }

    // ---- match bindings against the surface ----
    // name -> (port StrId, width); method args are "<method>.<arg>"
    let mut surface: Vec<(String, ir::StrId, u32)> = arg_ports
        .iter()
        .map(|&(n, w)| (s(n).to_string(), n, w))
        .collect();
    for (mname, args) in &autofire {
        for &(an, aw) in args {
            // arg ports export as "<method>_<arg>"; the binding key
            // is the user-facing "<method>.<arg>"
            let disp = s(an)
                .strip_prefix(&format!("{}_", s(*mname)))
                .unwrap_or(s(an));
            surface.push((format!("{}.{}", s(*mname), disp), an, aw));
        }
    }

    let mut bound: Vec<(String, Vec<u64>)> = Vec::new();
    let mut params: Vec<(ir::StrId, Value)> = Vec::new();
    let mut consumed_plus: Vec<String> = Vec::new();
    for b in binds {
        let Some((port, width)) = surface
            .iter()
            .find(|(n, _, _)| *n == b.name)
            .map(|t| (t.1, t.2))
        else {
            if b.explicit {
                let names: Vec<&str> =
                    surface.iter().map(|(n, _, _)| n.as_str()).collect();
                return Err(format!(
                    "unknown top-level binding `{}' (bindable: {})",
                    b.name,
                    names.join(", ")
                ));
            }
            continue; // run-time `+`: stays a plusarg
        };
        let limbs = parse_uint(&b.value).map_err(|e| {
            format!("binding `{}={}': {e}", b.name, b.value)
        })?;
        if bit_len(&limbs) > width {
            return Err(format!(
                "binding `{}={}' does not fit in the declared width \
                 ({} bits)",
                b.name, b.value, width
            ));
        }
        if let Some((_, prev)) = bound.iter().find(|(n, _)| *n == b.name) {
            if *prev != limbs {
                return Err(format!(
                    "conflicting bindings for `{}' (a linked artifact \
                     bakes its bindings; relink to change them)",
                    b.name
                ));
            }
        } else {
            params.push((port, Value::from_limbs64(width, limbs.clone())));
            bound.push((b.name.clone(), limbs));
        }
        if !b.explicit {
            consumed_plus.push(format!("{}={}", b.name, b.value));
        }
    }

    // ---- completeness ----
    let missing: Vec<String> = surface
        .iter()
        .filter(|(n, _, _)| !bound.iter().any(|(bn, _)| bn == n))
        .map(|(n, _, w)| format!("{n} ({w} bits)"))
        .collect();
    if !missing.is_empty() {
        return Err(format!(
            "top-level module `{}' requires bindings for: {} \
             (supply +NAME=value, or --bind NAME=value)",
            s(d.top),
            missing.join(", ")
        ));
    }

    // EN_<m> reads constant 1 for auto-fired methods (tied high)
    let mut af: Vec<(ir::StrId, Vec<Value>)> = Vec::new();
    for (mname, args) in &autofire {
        if let Some(en) = str_id(d, &format!("EN_{}", s(*mname))) {
            params.push((en, Value::from_u64(1, 1)));
        }
        let argv: Vec<Value> = args
            .iter()
            .map(|&(an, _)| {
                params
                    .iter()
                    .find(|(p, _)| *p == an)
                    .map(|(_, v)| v.clone())
                    .expect("auto-fire arg bound above")
            })
            .collect();
        af.push((*mname, argv));
    }

    let salt = if bound.is_empty() && af.is_empty() {
        0
    } else {
        // auto-fire arming is design-driven (always_enabled methods),
        // not binding-driven, so it does NOT enter the salt: the same
        // design auto-fires identically on every run.  Only bound
        // values differentiate compiled artifacts.
        bind_salt(&bound)
    };

    // ---- auto-fire schedule positions (see ResolvedBinds docs) ----
    let mut autofire_at: std::collections::HashMap<(usize, usize), Vec<usize>> =
        Default::default();
    let mut autofire_pre: std::collections::HashMap<usize, Vec<usize>> =
        Default::default();
    if !af.is_empty() {
        // v1 safety: an auto-fired body may touch primitives only.  A
        // call into a USER submodule's method would need the fused
        // cross-boundary ordering the exporter attaches to method
        // nodes — which the composition dropped for the (uncalled)
        // top methods — so the placement below could interleave the
        // child's segments wrongly.  Refuse rather than guess.
        let user_children: Vec<ir::StrId> = top
            .instances
            .iter()
            .filter(|i| matches!(i.kind, ir::InstanceKind::Module(_)))
            .map(|i| i.name)
            .collect();
        for (mi, _) in af.iter().enumerate() {
            let m = top
                .methods
                .iter()
                .find(|x| x.name == af[mi].0)
                .expect("autofire method exists");
            if body_calls_user_child(top, &m.body, &user_children) {
                return Err(format!(
                    "top-level always_enabled method `{}' calls a \
                     submodule method; auto-fired methods may touch \
                     primitives only (v1)",
                    s(m.name)
                ));
            }
        }
        let mut anchored = vec![false; af.len()];
        for (rci, comp) in d.compositions.iter().enumerate() {
            // this composition's top entries: (entry idx, domain, segment)
            let tops: Vec<(usize, u32, u32)> = comp
                .entries
                .iter()
                .enumerate()
                .filter(|(_, e)| d.strings[e.instance as usize].is_empty())
                .map(|(ei, e)| (ei, e.domain, e.segment))
                .collect();
            if tops.is_empty() {
                continue; // rule-less (e.g. tick-only negedge) comps
            }
            // placement per method: (anchor, cut segment, pos in cut)
            let mut placed: Vec<(Option<usize>, u32, usize, usize)> = Vec::new();
            for (mi, (mname, _)) in af.iter().enumerate() {
                let meth = top.methods.iter().find(|x| x.name == *mname).unwrap();
                let dm = meth.clock_domain;
                let Some(ms) = top
                    .schedule
                    .domains
                    .iter()
                    .find(|ms| ms.domain == dm && ms.posedge == comp.posedge)
                    .or_else(|| {
                        top.schedule.domains.iter().find(|ms| ms.domain == dm)
                    })
                else {
                    continue;
                };
                // this comp must reference this domain's segments
                if !tops.iter().any(|&(_, ed, _)| ed == dm) {
                    continue;
                }
                // the method's Exec position: LAST cut naming it
                let Some((k_last, cut_pos)) = ms
                    .segments
                    .iter()
                    .enumerate()
                    .rev()
                    .find_map(|(k, seg)| {
                        seg.cut
                            .iter()
                            .position(|c| c == mname)
                            .map(|p| (k as u32, p))
                    })
                else {
                    continue;
                };
                // anchor: latest node-bearing top entry at/before the cut
                let anchor = tops
                    .iter()
                    .filter(|&&(_, ed, seg)| ed == dm && seg <= k_last)
                    .max_by_key(|&&(_, _, seg)| seg)
                    .map(|&(ei, _, _)| ei);
                placed.push((anchor, k_last, cut_pos, mi));
                anchored[mi] = true;
            }
            // schedule order within one anchor: (cut segment, position)
            placed.sort_by_key(|&(_, k, p, _)| (k, p));
            for (anchor, _, _, mi) in placed {
                match anchor {
                    Some(ei) => autofire_at
                        .entry((rci, ei))
                        .or_default()
                        .push(mi),
                    None => autofire_pre.entry(rci).or_default().push(mi),
                }
            }
        }
        if let Some(mi) = anchored.iter().position(|&a| !a) {
            return Err(format!(
                "top-level always_enabled method `{}' has no schedule \
                 anchor: v1 auto-firing requires a top-level rule in \
                 the method's clock domain",
                s(af[mi].0)
            ));
        }
    }

    Ok(ResolvedBinds {
        params,
        autofire: af,
        autofire_at,
        autofire_pre,
        salt,
        consumed_plus,
    })
}

/// Does a method body call into a user-submodule instance (Action or
/// value MethCall/MethValue whose instance is a Module child)?  Def
/// references chase into the module def table — hoisted child value
/// reads hide there and evaluate on demand at fire time.
fn body_calls_user_child(
    m: &ir::Module,
    body: &[ir::Stmt],
    kids: &[ir::StrId],
) -> bool {
    struct Scan<'a> {
        m: &'a ir::Module,
        kids: &'a [ir::StrId],
        seen: std::collections::HashSet<ir::StrId>,
    }
    impl Scan<'_> {
        fn expr(&mut self, e: &ir::Expr) -> bool {
            match e {
                ir::Expr::MethCall { instance, args, .. } => {
                    self.kids.contains(instance)
                        || args.iter().any(|a| self.expr(a))
                }
                ir::Expr::MethValue { instance, .. } => {
                    self.kids.contains(instance)
                }
                ir::Expr::Def(n) => {
                    if !self.seen.insert(*n) {
                        return false;
                    }
                    match self.m.defs.iter().find(|d| d.name == *n) {
                        Some(d) => {
                            let de = d.expr.clone();
                            self.expr(&de)
                        }
                        None => false,
                    }
                }
                ir::Expr::ForeignCall { args, .. }
                | ir::Expr::Prim { args, .. } => {
                    args.iter().any(|a| self.expr(a))
                }
                ir::Expr::Clock { osc, gate } => {
                    self.expr(osc) || self.expr(gate)
                }
                ir::Expr::Reset { wire } => self.expr(wire),
                ir::Expr::If { cond, then_, else_, .. } => {
                    self.expr(cond) || self.expr(then_) || self.expr(else_)
                }
                ir::Expr::Case { scrutinee, arms, default, .. } => {
                    self.expr(scrutinee)
                        || arms.iter().any(|(_, a)| self.expr(a))
                        || self.expr(default)
                }
                ir::Expr::Const { .. }
                | ir::Expr::Port(_)
                | ir::Expr::Param(_)
                | ir::Expr::TaskValue { .. }
                | ir::Expr::Str(_)
                | ir::Expr::Real(_)
                | ir::Expr::Gate { .. } => false,
            }
        }
        fn act(&mut self, a: &ir::Action) -> bool {
            match a {
                ir::Action::MethCall { instance, cond, args, .. } => {
                    self.kids.contains(instance)
                        || self.expr(cond)
                        || args.iter().any(|e| self.expr(e))
                }
                ir::Action::Foreign { cond, args, .. }
                | ir::Action::Task { cond, args, .. } => {
                    self.expr(cond) || args.iter().any(|e| self.expr(e))
                }
            }
        }
        fn stmt(&mut self, st: &ir::Stmt) -> bool {
            match st {
                ir::Stmt::Def { expr: e, .. } => self.expr(e),
                ir::Stmt::Action(a) => self.act(a),
                ir::Stmt::AvAction { action, .. } => self.act(action),
                ir::Stmt::Cond { cond, then_, else_ } => {
                    self.expr(cond)
                        || then_.iter().any(|x| self.stmt(x))
                        || else_.iter().any(|x| self.stmt(x))
                }
            }
        }
    }
    let mut sc = Scan { m, kids, seen: Default::default() };
    body.iter().any(|st| sc.stmt(st))
}
