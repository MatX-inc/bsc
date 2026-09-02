//! BIR-level algebraic folding.
//!
//! bsc lowers struct construction to `Concat` and field selection to
//! `Extract`, so a body that builds a wide value and later reads its fields
//! emits the whole pack/unpack round trip.  LLVM can undo that in
//! principle -- it is what InstCombine, EarlyCSE and GVN spend their time
//! on -- but only by proving `lshr(or(shl(zext x, n), y), n) == x` over a
//! legalised multi-word integer, with the definition and the use thousands
//! of instructions apart.  Here the bit ranges are literal constants and
//! the operand boundaries are known, so the same fact is a comparison.
//!
//! STATUS: this fires on ~3.5% of concats on the designs measured so far
//! and shows no wall-clock win.  It is off unless `TRS_BIR_FOLD` is set,
//! and is kept for the census it carries as much as for the rewrite: the
//! counters below are what say where the cost actually is (see
//! `FoldStats`).  Do not read the presence of this pass as a claim that it
//! pays for itself.
//!
//! Only the exactly-aligned case folds, and only when EVERY use of the
//! concat is such an extract.  Folding a subset would leave the composite
//! live alongside the operands it was built from -- two live copies of the
//! same bits where there was one -- so a partially-foldable concat is left
//! entirely alone.
//!
//! Scope is one body at a time.  A `Stmt::Def` latches a name for that body
//! only, and the def table may hold a DIFFERENT expression under the same
//! name (see `Stmt::Def`'s own doc), so a binding is dropped only once
//! nothing in the body refers to it -- otherwise a later reference would
//! resolve past the latch to the table entry.

use crate::expr::{Action, Expr, PrimOp, Stmt};
use crate::{Module, StrId};
use std::collections::HashMap;

/// What a fold pass did and what it saw, reported under `TRS_FOLD_STATS`.
///
/// The `kept_*` and study counters exist because the interesting output of
/// this pass is the census, not the rewrite: they are what identifies which
/// wide values dominate and how their fields sit relative to 64-bit words.
#[derive(Debug, Default, Clone, Copy)]
pub struct FoldStats {
    /// Concat-valued bindings considered.
    pub concats: usize,
    /// Concats all of whose uses were aligned extracts.
    pub folded: usize,
    /// Extract nodes replaced by the operand they select.
    pub extracts: usize,
    /// Bindings removed as dead after folding.
    pub dropped: usize,
    /// Concats left alone because some use was not an aligned extract.
    pub kept: usize,
    /// ...of those, disqualified by a whole-value reference.
    pub kept_whole_use: usize,
    /// ...of those, disqualified by an extract straddling operands.
    pub kept_misaligned: usize,

    // Where whole-value references occur.  A concat consumed only as a
    // value cannot be folded away; knowing what consumes it says whether
    // that use could itself be decomposed.
    pub whole_in_action_arg: usize,
    pub whole_in_action_cond: usize,
    pub whole_in_prim: usize,
    pub whole_in_nested_concat: usize,
    pub whole_in_if_case: usize,
    pub whole_in_def_rhs: usize,
    pub whole_in_other: usize,

    // Width study.  Only results wider than a machine word force LLVM onto
    // illegal integer types; narrower concats are already fine.
    pub concat_le_64: usize,
    pub concat_65_128: usize,
    pub concat_over_128: usize,

    // Chunk study.  If a wide value were carried as 64-bit chunks, an
    // extract within one chunk is a shift and mask on a legal type; one
    // straddling two needs both.  Nothing observed so far needs three.
    pub extract_1_chunk: usize,
    pub extract_2_chunks: usize,
    pub extract_3_plus_chunks: usize,
}

impl FoldStats {
    /// Fold `o` into `self`, field by field.
    pub fn merge(&mut self, o: FoldStats) {
        let Self {
            concats,
            folded,
            extracts,
            dropped,
            kept,
            kept_whole_use,
            kept_misaligned,
            whole_in_action_arg,
            whole_in_action_cond,
            whole_in_prim,
            whole_in_nested_concat,
            whole_in_if_case,
            whole_in_def_rhs,
            whole_in_other,
            concat_le_64,
            concat_65_128,
            concat_over_128,
            extract_1_chunk,
            extract_2_chunks,
            extract_3_plus_chunks,
        } = o;
        self.concats += concats;
        self.folded += folded;
        self.extracts += extracts;
        self.dropped += dropped;
        self.kept += kept;
        self.kept_whole_use += kept_whole_use;
        self.kept_misaligned += kept_misaligned;
        self.whole_in_action_arg += whole_in_action_arg;
        self.whole_in_action_cond += whole_in_action_cond;
        self.whole_in_prim += whole_in_prim;
        self.whole_in_nested_concat += whole_in_nested_concat;
        self.whole_in_if_case += whole_in_if_case;
        self.whole_in_def_rhs += whole_in_def_rhs;
        self.whole_in_other += whole_in_other;
        self.concat_le_64 += concat_le_64;
        self.concat_65_128 += concat_65_128;
        self.concat_over_128 += concat_over_128;
        self.extract_1_chunk += extract_1_chunk;
        self.extract_2_chunks += extract_2_chunks;
        self.extract_3_plus_chunks += extract_3_plus_chunks;
    }
}

impl std::fmt::Display for FoldStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "trs fold: concats={} folded={} extracts={} dropped={} kept={} \
             (whole-use={} misaligned={})",
            self.concats,
            self.folded,
            self.extracts,
            self.dropped,
            self.kept,
            self.kept_whole_use,
            self.kept_misaligned
        )?;
        writeln!(
            f,
            "trs fold: whole-value use in action-arg={} action-cond={} prim={} \
             nested-concat={} if/case={} def-rhs={} other={}",
            self.whole_in_action_arg,
            self.whole_in_action_cond,
            self.whole_in_prim,
            self.whole_in_nested_concat,
            self.whole_in_if_case,
            self.whole_in_def_rhs,
            self.whole_in_other
        )?;
        write!(
            f,
            "trs fold: concat width <=64={} 65-128={} >128={}; \
             extract chunks 1={} 2={} 3+={}",
            self.concat_le_64,
            self.concat_65_128,
            self.concat_over_128,
            self.extract_1_chunk,
            self.extract_2_chunks,
            self.extract_3_plus_chunks
        )
    }
}

/// Bit range `[lo, hi]` of one concat operand, and its index.
struct Field {
    lo: u64,
    hi: u64,
    idx: usize,
}

/// Candidate concats: binding name -> (operands, their bit ranges).
type Cands = HashMap<StrId, (Vec<Expr>, Vec<Field>)>;

/// Why each candidate was disqualified, if it was.
#[derive(Default)]
struct Census {
    bad: HashMap<StrId, bool>,
    whole: HashMap<StrId, bool>,
    misaligned: HashMap<StrId, bool>,
}

/// Where a whole-value reference was found.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Ctx {
    ActionArg,
    ActionCond,
    Prim,
    NestedConcat,
    IfCase,
    DefRhs,
    Other,
}

impl Ctx {
    fn bump(self, st: &mut FoldStats) {
        match self {
            Ctx::ActionArg => st.whole_in_action_arg += 1,
            Ctx::ActionCond => st.whole_in_action_cond += 1,
            Ctx::Prim => st.whole_in_prim += 1,
            Ctx::NestedConcat => st.whole_in_nested_concat += 1,
            Ctx::IfCase => st.whole_in_if_case += 1,
            Ctx::DefRhs => st.whole_in_def_rhs += 1,
            Ctx::Other => st.whole_in_other += 1,
        }
    }
}

/// Fold every rule and method body in the module.
pub fn fold_module(m: &mut Module) -> FoldStats {
    let mut tot = FoldStats::default();
    // Widths for names the def table declares; a body-local latch shadows
    // these, so the per-body pass layers its own map over the top.
    let table: HashMap<StrId, u32> = m.defs.iter().map(|d| (d.name, d.width)).collect();

    for i in 0..m.rules.len() {
        let mut body = (*m.rules[i].body).clone();
        tot.merge(fold_body(&mut body, &table));
        m.rules[i].body = crate::Lazy::new(body);
    }
    for i in 0..m.methods.len() {
        let mut body = std::mem::take(&mut m.methods[i].body);
        tot.merge(fold_body(&mut body, &table));
        m.methods[i].body = body;
    }
    tot
}

/// Fold one body.  `table` supplies widths for names the body does not
/// latch itself.
pub fn fold_body(body: &mut Vec<Stmt>, table: &HashMap<StrId, u32>) -> FoldStats {
    let mut st = FoldStats::default();

    let mut widths = table.clone();
    collect_widths(body, &mut widths);

    let mut cand = Cands::new();
    collect_concats(body, &widths, &mut cand, &mut st);
    st.concats = cand.len();
    if cand.is_empty() {
        return st;
    }

    let mut cen = Census::default();
    for s in body.iter() {
        census_stmt(s, &cand, &mut cen, &mut st);
    }
    cand.retain(|n, _| {
        if *cen.bad.get(n).unwrap_or(&false) {
            st.kept += 1;
            if *cen.whole.get(n).unwrap_or(&false) {
                st.kept_whole_use += 1;
            }
            if *cen.misaligned.get(n).unwrap_or(&false) {
                st.kept_misaligned += 1;
            }
            false
        } else {
            true
        }
    });
    if cand.is_empty() {
        return st;
    }
    st.folded = cand.len();

    for s in body.iter_mut() {
        rewrite_stmt(s, &cand, &mut st.extracts);
    }

    // Drop bindings nothing refers to any more.  Re-census rather than
    // trusting the rewrite: a name may appear in a construct the walker
    // does not fold through.
    let mut live: HashMap<StrId, bool> = HashMap::new();
    for s in body.iter() {
        census_live(s, &mut live);
    }
    let before = body.len();
    body.retain(|s| match s {
        Stmt::Def { name, .. } => {
            !(cand.contains_key(name) && !*live.get(name).unwrap_or(&false))
        }
        _ => true,
    });
    st.dropped = before - body.len();
    st
}

// ---- widths ----

fn collect_widths(body: &[Stmt], w: &mut HashMap<StrId, u32>) {
    for s in body {
        match s {
            Stmt::Def { name, expr } => {
                let width = expr.width();
                if width > 0 {
                    w.insert(*name, width);
                }
            }
            Stmt::Cond { then_, else_, .. } => {
                collect_widths(then_, w);
                collect_widths(else_, w);
            }
            Stmt::Action(_) | Stmt::AvAction { .. } => {}
        }
    }
}

/// Width of `e`, or None when it comes from a declaration we cannot see
/// (ports and parameters), which disqualifies the enclosing concat.
fn width_of(e: &Expr, w: &HashMap<StrId, u32>) -> Option<u32> {
    match e {
        Expr::Def(n) => w.get(n).copied(),
        Expr::Port(_) | Expr::Param(_) | Expr::Str(_) => None,
        other => match other.width() {
            0 => None,
            x => Some(x),
        },
    }
}

/// A concat's operands run left-to-right, first most significant.
fn fields_of(args: &[Expr], w: &HashMap<StrId, u32>) -> Option<Vec<Field>> {
    let mut ws = Vec::with_capacity(args.len());
    for a in args {
        ws.push(width_of(a, w)?);
    }
    let total: u64 = ws.iter().map(|&x| u64::from(x)).sum();
    let mut out = Vec::with_capacity(args.len());
    let mut hi = total;
    for (idx, &wa) in ws.iter().enumerate() {
        if wa == 0 {
            continue;
        }
        let lo = hi - u64::from(wa);
        out.push(Field { lo, hi: hi - 1, idx });
        hi = lo;
    }
    Some(out)
}

fn collect_concats(
    body: &[Stmt],
    w: &HashMap<StrId, u32>,
    out: &mut Cands,
    st: &mut FoldStats,
) {
    for s in body {
        match s {
            Stmt::Def { name, expr } => {
                if let Expr::Prim { op: PrimOp::Concat, args, .. } = expr {
                    if let Some(f) = fields_of(args, w) {
                        let total: u64 = f.iter().map(|x| x.hi - x.lo + 1).sum();
                        match total {
                            0..=64 => st.concat_le_64 += 1,
                            65..=128 => st.concat_65_128 += 1,
                            _ => st.concat_over_128 += 1,
                        }
                        out.insert(*name, (args.clone(), f));
                    }
                }
            }
            Stmt::Cond { then_, else_, .. } => {
                collect_concats(then_, w, out, st);
                collect_concats(else_, w, out, st);
            }
            Stmt::Action(_) | Stmt::AvAction { .. } => {}
        }
    }
}

/// The constant `[hi, lo]` of an extract, if both bounds are literal.
fn extract_bounds(args: &[Expr]) -> Option<(u64, u64)> {
    if args.len() != 3 {
        return None;
    }
    let (Expr::Const { limbs: hl, .. }, Expr::Const { limbs: ll, .. }) =
        (&args[1], &args[2])
    else {
        return None;
    };
    Some((u64::from(*hl.first().unwrap_or(&0)), u64::from(*ll.first().unwrap_or(&0))))
}

/// If `e` is `Extract(Def(n), hi, lo)` on a candidate with the range exactly
/// covering one operand, return that operand's index.
fn aligned_pick(e: &Expr, cand: &Cands) -> Option<(StrId, usize)> {
    let Expr::Prim { op: PrimOp::Extract, args, width } = e else { return None };
    let Expr::Def(n) = args.first()? else { return None };
    let (_, fields) = cand.get(n)?;
    let (hi, lo) = extract_bounds(args)?;
    let f = fields.iter().find(|f| f.lo == lo && f.hi == hi)?;
    // the extract's own declared width must match the operand it picks
    if hi.checked_sub(lo)? + 1 != u64::from(*width) {
        return None;
    }
    Some((*n, f.idx))
}

// ---- census ----

fn census_expr(e: &Expr, cand: &Cands, cen: &mut Census, st: &mut FoldStats, ctx: Ctx) {
    // An aligned extract consumes its Def operand; do not descend into it,
    // or the Def would count as a bare whole-value reference.
    if let Some((n, _)) = aligned_pick(e, cand) {
        cen.bad.entry(n).or_insert(false);
        return;
    }
    if let Expr::Prim { op: PrimOp::Extract, args, .. } = e {
        if let Some(Expr::Def(n)) = args.first() {
            if cand.contains_key(n) {
                if let Some((hi, lo)) = extract_bounds(args) {
                    match hi / 64 - lo / 64 {
                        0 => st.extract_1_chunk += 1,
                        1 => st.extract_2_chunks += 1,
                        _ => st.extract_3_plus_chunks += 1,
                    }
                }
                cen.bad.insert(*n, true);
                cen.misaligned.insert(*n, true);
                for c in args.iter().skip(1) {
                    census_expr(c, cand, cen, st, ctx);
                }
                return;
            }
        }
    }
    if let Expr::Def(n) = e {
        if cand.contains_key(n) {
            cen.bad.insert(*n, true);
            cen.whole.insert(*n, true);
            ctx.bump(st);
        }
        return;
    }
    let kid = match e {
        Expr::Prim { op: PrimOp::Concat, .. } => Ctx::NestedConcat,
        Expr::Prim { .. } => Ctx::Prim,
        Expr::If { .. } | Expr::Case { .. } => Ctx::IfCase,
        _ => ctx,
    };
    for c in children(e) {
        census_expr(c, cand, cen, st, kid);
    }
}

fn census_action(a: &Action, cand: &Cands, cen: &mut Census, st: &mut FoldStats) {
    match a {
        Action::MethCall { cond, args, .. }
        | Action::Foreign { cond, args, .. }
        | Action::Task { cond, args, .. } => {
            census_expr(cond, cand, cen, st, Ctx::ActionCond);
            for x in args {
                census_expr(x, cand, cen, st, Ctx::ActionArg);
            }
        }
    }
}

fn census_stmt(s: &Stmt, cand: &Cands, cen: &mut Census, st: &mut FoldStats) {
    match s {
        Stmt::Def { name, expr } => {
            // a concat's own definition is not a use of itself
            if cand.contains_key(name) {
                if let Expr::Prim { op: PrimOp::Concat, args, .. } = expr {
                    for a in args {
                        census_expr(a, cand, cen, st, Ctx::DefRhs);
                    }
                    return;
                }
            }
            census_expr(expr, cand, cen, st, Ctx::DefRhs);
        }
        Stmt::Action(a) => census_action(a, cand, cen, st),
        Stmt::AvAction { action, .. } => census_action(action, cand, cen, st),
        Stmt::Cond { cond, then_, else_ } => {
            census_expr(cond, cand, cen, st, Ctx::Other);
            for x in then_.iter().chain(else_.iter()) {
                census_stmt(x, cand, cen, st);
            }
        }
    }
}

/// Any reference at all, to decide whether a binding is now dead.  A
/// binding's own right-hand side does not keep it alive.
fn census_live(s: &Stmt, live: &mut HashMap<StrId, bool>) {
    fn ex(e: &Expr, live: &mut HashMap<StrId, bool>) {
        if let Expr::Def(n) = e {
            live.insert(*n, true);
            return;
        }
        for c in children(e) {
            ex(c, live);
        }
    }
    fn act(a: &Action, live: &mut HashMap<StrId, bool>) {
        match a {
            Action::MethCall { cond, args, .. }
            | Action::Foreign { cond, args, .. }
            | Action::Task { cond, args, .. } => {
                ex(cond, live);
                for x in args {
                    ex(x, live);
                }
            }
        }
    }
    match s {
        Stmt::Def { expr, .. } => ex(expr, live),
        Stmt::Action(a) => act(a, live),
        Stmt::AvAction { action, .. } => act(action, live),
        Stmt::Cond { cond, then_, else_ } => {
            ex(cond, live);
            for x in then_.iter().chain(else_.iter()) {
                census_live(x, live);
            }
        }
    }
}

// ---- rewrite ----

fn rewrite_expr(e: &mut Expr, cand: &Cands, n: &mut usize) {
    if let Some((name, idx)) = aligned_pick(e, cand) {
        *e = cand[&name].0[idx].clone();
        *n += 1;
        // the operand may itself be an aligned extract of another candidate
        rewrite_expr(e, cand, n);
        return;
    }
    for c in children_mut(e) {
        rewrite_expr(c, cand, n);
    }
}

fn rewrite_action(a: &mut Action, cand: &Cands, n: &mut usize) {
    match a {
        Action::MethCall { cond, args, .. }
        | Action::Foreign { cond, args, .. }
        | Action::Task { cond, args, .. } => {
            rewrite_expr(cond, cand, n);
            for x in args {
                rewrite_expr(x, cand, n);
            }
        }
    }
}

fn rewrite_stmt(s: &mut Stmt, cand: &Cands, n: &mut usize) {
    match s {
        Stmt::Def { name, expr } => {
            if cand.contains_key(name) {
                return; // leave the concat itself; deadness decides its fate
            }
            rewrite_expr(expr, cand, n);
        }
        Stmt::Action(a) => rewrite_action(a, cand, n),
        Stmt::AvAction { action, .. } => rewrite_action(action, cand, n),
        Stmt::Cond { cond, then_, else_ } => {
            rewrite_expr(cond, cand, n);
            for x in then_.iter_mut().chain(else_.iter_mut()) {
                rewrite_stmt(x, cand, n);
            }
        }
    }
}

// ---- structural child access ----

fn children(e: &Expr) -> Vec<&Expr> {
    match e {
        Expr::Prim { args, .. }
        | Expr::ForeignCall { args, .. }
        | Expr::MethCall { args, .. } => args.iter().collect(),
        Expr::If { cond, then_, else_, .. } => vec![cond, then_, else_],
        Expr::Case { scrutinee, arms, default, .. } => {
            let mut v = vec![&**scrutinee];
            v.extend(arms.iter().map(|(_, e)| e));
            v.push(default);
            v
        }
        Expr::Clock { osc, gate } => vec![osc, gate],
        Expr::Reset { wire } => vec![wire],
        _ => Vec::new(),
    }
}

fn children_mut(e: &mut Expr) -> Vec<&mut Expr> {
    match e {
        Expr::Prim { args, .. }
        | Expr::ForeignCall { args, .. }
        | Expr::MethCall { args, .. } => args.iter_mut().collect(),
        Expr::If { cond, then_, else_, .. } => vec![cond, then_, else_],
        Expr::Case { scrutinee, arms, default, .. } => {
            let mut v = vec![&mut **scrutinee];
            v.extend(arms.iter_mut().map(|(_, e)| e));
            v.push(default);
            v
        }
        Expr::Clock { osc, gate } => vec![osc, gate],
        Expr::Reset { wire } => vec![wire],
        _ => Vec::new(),
    }
}
