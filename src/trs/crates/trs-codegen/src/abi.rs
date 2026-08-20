//! The compiled-artifact ABI: the plain-Rust types, constants, and
//! wire codecs shared between the LLVM lowering (feature `llvm`) and
//! the artifact load path.  No inkwell here — this module is what a
//! slim, LLVM-free runtime (artifact loading + trampolines) builds
//! against.  `lower` glob-re-exports it, so `lower::X` paths keep
//! working in llvm builds.

use std::collections::HashMap;

use trs_ir::{Design, Expr, StrId};

/// Callback for foreign statements inside compiled bodies (the
/// $display family and value/ActionValue tasks): compiled code
/// evaluates the arguments natively at the statement position and
/// passes their words in `args` (string literals occupy no words —
/// the call-site table carries them); a task's result words land in
/// `out`.  A nonzero return aborts the compiled edge — reserved for
/// genuine aborts, never $finish/$stop (edge-completion contract).
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

thread_local! {
    /// Edge-SSA site census (task #24 M1): static counts of the slot
    /// round-trips an SSA edge lowering would eliminate.  Indices:
    /// [0] other-rule CF/WF slot loads (incl. exec WF gates and sched
    /// inhibitor reads), [1] eager reloads in exec bodies, [2]
    /// shared-eager reloads in sched fns, [3] eager owner stores (kept
    /// as exports), [4] words moved by the promotable loads
    /// (ceil(w/64) per site).  Thread-local like AOT_MODE: the
    /// one-module link path lowers the whole design on one thread,
    /// which is the path the census exists for.  Read via
    /// edge_ssa_sites() under TRS_EDGE_SSA_STATS=1.
    pub static EDGE_SSA_SITES: std::cell::Cell<[usize; 5]> =
        const { std::cell::Cell::new([0; 5]) };
}

pub(crate) fn edge_ssa_count(idx: usize, words: usize) {
    EDGE_SSA_SITES.with(|c| {
        let mut v = c.get();
        v[idx] += 1;
        v[4] += words;
        c.set(v);
    });
}

/// Snapshot the census counters (this thread).
pub fn edge_ssa_sites() -> [usize; 5] {
    EDGE_SSA_SITES.with(|c| c.get())
}

/// Direct-BDPI support (task #22): c_name -> function address for
/// baked-mode (JIT) call emission, set once by the interpreter after
/// dlopening the user .so.  AOT artifacts use per-function pointer
/// globals filled by the loader instead.
pub static BDPI_SYMS: std::sync::OnceLock<HashMap<String, usize>> =
    std::sync::OnceLock::new();
/// Address of the stdio-flush callback (phase 0 = flush Rust stdout
/// before the C call, 1 = fflush(NULL) after) — preserves the byte
/// interleaving of user printf with $display output, exactly like the
/// interpreter's BDPI dispatch.
pub static STDIO_CB: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
/// One compiled prim call site (resolved by the trampoline).
#[derive(Clone)]
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
    /// local ConfigReg instance name -> (base slot, width): old value,
    /// current value, written_at instant (see ArenaKind::CReg)
    pub creg_slot: HashMap<StrId, (u32, u32)>,
    /// local RegFile instance name -> (base slot, width, lo, hi):
    /// header [upd_at, upd_addr, upd_prev(w)] then dense data
    /// (see ArenaKind::RegFile)
    pub regfile_slot: HashMap<StrId, (u32, u32, u64, u64)>,
    /// local BRAM instance name -> (base slot, width, size, chunk_size,
    /// num_wens, dual, pipelined): per-port headers then dense data
    /// (see ArenaKind::Bram)
    pub bram_slot: HashMap<StrId, (u32, u32, u64, u32, u32, bool, bool)>,
    /// local CReg (CRegN5) instance name -> (base slot, width): live
    /// value then registered value, w words each (see ArenaKind::CReg5)
    pub creg5_slot: HashMap<StrId, (u32, u32)>,
    /// local FIFO instance name -> (base slot, width, size, guarded):
    /// header (elems, saved_elems, fst, enq_at, deq_at, clear_at) then
    /// data (see ArenaKind::Fifo)
    pub fifo_slot: HashMap<StrId, (u32, u32, u32, bool, bool)>,
    /// module reset input port name -> arena slot holding the PORT level
    /// (1 = deasserted, matching the interpreter's Port read)
    pub reset_slot: HashMap<StrId, u32>,
    /// outlined stable def -> (memo slot base: stamp word then value
    /// words, width); type-uniform offsets (part of the dedup sig)
    pub memo_slot: HashMap<StrId, (u32, u32)>,
    /// subtree arena region [start, end): every slot this instance's
    /// compiled code can touch (own state + descendants); the basis
    /// for per-module-type code dedup (base-relative addressing)
    pub region: (u32, u32),
    /// EN_<m> port name -> arena slot; zeroed at composition dispatch,
    /// stored by compiled call sites (the C++ enable protocol)
    pub en_slot: HashMap<StrId, u32>,
    /// constant-valued module input ports and instantiation
    /// parameters: the compiled mirror of the interpreter's
    /// Port/Param fallthrough — an uncalled method's arg reads 0,
    /// unbound clock/gate/reset-kind input ports read 1, numeric
    /// params read their bound value.  Dynamic bindings never land
    /// here (bound gates evaluate in the parent; EN and reset ports
    /// have arena slots; string params are marker values).  Part of
    /// the exec dedup signature.
    pub port_consts: HashMap<StrId, (u32, u64)>,
    /// Real-valued instantiation parameters, as f64 bits: reals reach
    /// simulation only as task arguments and module parameters, so the
    /// compiled carrier is an i64 of the double's bits and the foreign
    /// spec marks the argument Real (decode rebuilds Arg::Real).  Part
    /// of the exec dedup signature.
    pub real_consts: HashMap<StrId, u64>,
    /// Input clock-gate ports bound at instantiation: port name ->
    /// (owner instance, gate expr) — reads lower the expr in the
    /// OWNER's frame, mirroring the interp's parent-context gate
    /// evaluation.  Part of the exec dedup signature (owner slots are
    /// absolute in deduped bodies, so gate wiring must pin the sig).
    pub gates: HashMap<StrId, (usize, Expr)>,
    /// String-valued instantiation parameters: name -> string id.  The
    /// compiled carrier for strings is an i64 of the id (the interp's
    /// str_ref marker value), consumed by StrDyn foreign args, string
    /// Eq, and the StringConcat intern callback.  Part of the exec
    /// dedup signature.
    pub str_consts: HashMap<StrId, StrId>,
    /// Instantiation values wider than 64 bits: name -> (width, LE
    /// 32-bit limbs), lowered as wide constants (cval).  Part of the
    /// exec dedup signature.
    pub wide_consts: HashMap<StrId, (u32, Vec<u32>)>,
    /// any rule's CAN_FIRE/WILL_FIRE def name -> arena slot (this
    /// instance); reads of other rules' fire signals become slot loads
    pub cfwf_slot: HashMap<StrId, u32>,
    /// schedule-position def name -> (arena base slot, width): stored by
    /// the sched fn that owns the def, reloaded by exec bodies (the C++
    /// `DEF_x = DEF_x;` reuse semantics)
    pub eager_slot: HashMap<StrId, (u32, u32)>,
    /// TRACED artifacts only (empty otherwise): VCD-declared member
    /// def -> (recording slot base, width).  Def bindings store their
    /// value here so the VCD writer sees the interp's last-evaluated
    /// semantics.  Part of the exec dedup signature.
    pub rec_defs: HashMap<StrId, (u32, u32)>,
    /// TRACED artifacts only: method name -> recording slots for its
    /// VCD ports (EN time / args / result), stored by inlined call
    /// sites.  Part of the exec dedup signature.
    pub rec_meths: HashMap<StrId, RecMeth>,
}

/// Arena recording slots for one user-module method's VCD ports
/// (traced artifacts).
#[derive(Clone)]
pub struct RecMeth {
    /// last-call time slot (init u64::MAX; the writer's EN test is
    /// time == the clock's last posedge)
    pub t: u32,
    /// per-argument (base, port width), in method arg order (init 0)
    pub args: Vec<(u32, u32)>,
    /// result (base, width) for value/AV methods (init 0)
    pub res: Option<(u32, u32)>,
}

/// Design-wide plan: one InstEnv per user instance the compiled code
/// can touch.
pub struct PlanEnv<'a> {
    /// arena slot the dispatcher stamps with the current instant at
    /// every edge (ConfigReg reads compare written_at against it)
    pub now_slot: u32,
    pub d: &'a Design,
    pub insts: &'a HashMap<usize, InstEnv>,
}
/// One rule to compile.
#[derive(Clone, serde::Serialize, serde::Deserialize)]
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
    /// WILL_FIRE is provably constant-true (fire_when_enabled +
    /// no-conflict rules — the fully-static-schedule case): the exec
    /// body skips its WF gate entirely
    pub always_fire: bool,
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
#[derive(Clone)]
pub struct ForeignSpec {
    /// instance for $display location reporting
    pub inst: usize,
    pub func: StrId,
    /// result width (0 = plain action, no result)
    pub ret_width: u32,
    pub args: Vec<FArgSpec>,
}

/// One foreign argument: a string literal (no marshaled words), a
/// numeric value of the given width with its signed-display flag, or a
/// real value (one marshaled word carrying the f64 bits — the decode
/// rebuilds the interp's Arg::Real so formatting is identical).
#[derive(Clone)]
pub enum FArgSpec {
    Str(StrId),
    Num { width: u32, signed: bool },
    Real,
    /// A dynamically-selected string: one marshaled word carrying the
    /// string id (static table or runtime-interned) — the decode
    /// resolves it to the interp's Arg::Str.
    StrDyn,
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

/// A compiled rule body: (arena, env, region base index, token base).
/// One compiled body serves every instance of its module type.
pub struct CompiledExec {
    pub exec:
        unsafe extern "C" fn(*mut u64, *mut core::ffi::c_void, u64, u64) -> i32,
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
/// Call-site tables a lowering produces for one rule's sched and exec
/// functions.  Token `local` indices point into these; the AOT load
/// path rebuilds them by re-running trial_lower (deterministic).
pub struct FnProtos {
    pub sched_foreign: Vec<ForeignSpec>,
    pub sched_prims: Vec<PrimCallSpec>,
    pub exec_foreign: Vec<ForeignSpec>,
    pub exec_prims: Vec<PrimCallSpec>,
}

/// Wire format for per-ordinal call-site tables baked into artifacts
/// (trs_protos global): little-endian u32 stream.  Loading decoded
/// protos skips trial_lower entirely (0.32s of sudoku's startup);
/// validity is guaranteed by the bir_hash/layout/threshold checks.
pub fn encode_protos(protos: &[FnProtos]) -> Vec<u8> {
    let mut o: Vec<u8> = Vec::new();
    let w = |o: &mut Vec<u8>, v: u32| o.extend_from_slice(&v.to_le_bytes());
    let wf = |o: &mut Vec<u8>, v: &[ForeignSpec]| {
        w(o, v.len() as u32);
        for f in v {
            w(o, f.inst as u32);
            w(o, f.func);
            w(o, f.ret_width);
            w(o, f.args.len() as u32);
            for a in &f.args {
                match a {
                    FArgSpec::Str(sid) => {
                        w(o, 0);
                        w(o, *sid);
                        w(o, 0);
                    }
                    FArgSpec::Num { width, signed } => {
                        w(o, 1);
                        w(o, *width);
                        w(o, *signed as u32);
                    }
                    FArgSpec::Real => {
                        w(o, 2);
                        w(o, 0);
                        w(o, 0);
                    }
                    FArgSpec::StrDyn => {
                        w(o, 3);
                        w(o, 0);
                        w(o, 0);
                    }
                }
            }
        }
    };
    let wp = |o: &mut Vec<u8>, v: &[PrimCallSpec]| {
        w(o, v.len() as u32);
        for pc in v {
            w(o, pc.inst as u32);
            w(o, pc.method);
            w(o, pc.ret_width);
            w(o, pc.is_action as u32);
            w(o, pc.arg_widths.len() as u32);
            for &aw in &pc.arg_widths {
                w(o, aw);
            }
        }
    };
    w(&mut o, protos.len() as u32);
    for p in protos {
        wf(&mut o, &p.sched_foreign);
        wp(&mut o, &p.sched_prims);
        wf(&mut o, &p.exec_foreign);
        wp(&mut o, &p.exec_prims);
    }
    o
}

/// Inverse of encode_protos; None on truncation/garbage.
pub fn decode_protos(b: &[u8]) -> Option<Vec<FnProtos>> {
    let mut i = 0usize;
    // artifact-supplied counts must never drive an allocation larger
    // than the bytes backing them: every record is >= 4 bytes, so any
    // count above b.len()/4 is corruption — reject before reserving
    fn r(b: &[u8], i: &mut usize) -> Option<u32> {
        let v = u32::from_le_bytes(b.get(*i..*i + 4)?.try_into().ok()?);
        *i += 4;
        Some(v)
    }
    fn rf(b: &[u8], i: &mut usize) -> Option<Vec<ForeignSpec>> {
        let n = r(b, i)?;
        if n as usize > b.len() / 4 {
            return None;
        }
        let mut v = Vec::with_capacity(n as usize);
        for _ in 0..n {
            let inst = r(b, i)? as usize;
            let func = r(b, i)?;
            let ret_width = r(b, i)?;
            let argc = r(b, i)?;
            if argc as usize > b.len() / 4 {
                return None;
            }
            let mut args = Vec::with_capacity(argc as usize);
            for _ in 0..argc {
                let tag = r(b, i)?;
                let a = r(b, i)?;
                let sg = r(b, i)?;
                // exact tags only: an unknown tag is a corrupted or
                // future-format artifact — fail CLOSED, or the callback
                // buffer walk desynchronizes on a garbage width
                // (review finding: unknown tags fell open as Num)
                args.push(match tag {
                    0 => FArgSpec::Str(a),
                    1 => FArgSpec::Num { width: a, signed: sg != 0 },
                    2 => FArgSpec::Real,
                    3 => FArgSpec::StrDyn,
                    _ => return None,
                });
            }
            v.push(ForeignSpec { inst, func, ret_width, args });
        }
        Some(v)
    }
    fn rp(b: &[u8], i: &mut usize) -> Option<Vec<PrimCallSpec>> {
        let n = r(b, i)?;
        if n as usize > b.len() / 4 {
            return None;
        }
        let mut v = Vec::with_capacity(n as usize);
        for _ in 0..n {
            let inst = r(b, i)? as usize;
            let method = r(b, i)?;
            let ret_width = r(b, i)?;
            let is_action = r(b, i)? != 0;
            let argc = r(b, i)?;
            if argc as usize > b.len() / 4 {
                return None;
            }
            let mut arg_widths = Vec::with_capacity(argc as usize);
            for _ in 0..argc {
                arg_widths.push(r(b, i)?);
            }
            v.push(PrimCallSpec { inst, method, arg_widths, ret_width, is_action });
        }
        Some(v)
    }
    let n = r(b, &mut i)?;
    if n as usize > b.len() / 4 {
        return None;
    }
    let mut out = Vec::with_capacity(n as usize);
    for _ in 0..n {
        out.push(FnProtos {
            sched_foreign: rf(b, &mut i)?,
            sched_prims: rp(b, &mut i)?,
            exec_foreign: rf(b, &mut i)?,
            exec_prims: rp(b, &mut i)?,
        });
    }
    (i == b.len()).then_some(out)
}
thread_local! {
    /// set while emitting artifact objects (opt default differs)
    pub static AOT_MODE: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// RAII guard: artifact emission runs with the AOT opt default.
pub struct AotModeGuard;
impl AotModeGuard {
    pub fn set() -> AotModeGuard {
        AOT_MODE.with(|m| m.set(true));
        AotModeGuard
    }
}
impl Drop for AotModeGuard {
    fn drop(&mut self) {
        AOT_MODE.with(|m| m.set(false));
    }
}
/// Sentinel method id in a PrimCallSpec: not a method call — the
/// trampoline answers the prim's gate_out() (compiled Expr::Gate).
pub const GATE_OUT_METHOD: StrId = u32::MAX;

/// Sentinel func id in a ForeignSpec: not a foreign function — the
/// callback concatenates its (StrDyn) arguments' texts and interns the
/// result, returning the new string id (compiled PrimOp::StringConcat,
/// mirroring the interp's per-evaluation intern_dyn).
pub const STRING_CONCAT_FUNC: StrId = u32::MAX - 1;
/// AOT layout revision, baked into every artifact: bump whenever slot
/// allocation, token layout, or callback ABI changes so a stale .so is
/// refused at load instead of silently misreading the arena.
pub const AOT_LAYOUT_REV: u64 = 21;
/// How a caller reaches an outlined def-piece helper: a baked address
/// (JIT: the helper engine compiled first) or a named symbol (AOT: ld
/// resolves it inside the artifact .so).
pub enum HelperRef {
    Addr(usize),
    Sym(String),
}

/// Outlined pieces available to a lowering: (module ir, def) ->
/// (helper, result width, port params in signature order).
pub type HelperMap = HashMap<(usize, StrId), (HelperRef, u32, Vec<(StrId, u32)>)>;

/// One outlined def piece to compile as a helper function.
pub struct HelperSpec {
    /// module ir + def being outlined
    pub mir: usize,
    pub def: StrId,
    pub width: u32,
    /// symbol: hlp_<inst-sig hex>_<def id> (class-unique)
    pub sym: String,
    /// exemplar instance (frames, region context); the fn is shared by
    /// every instance whose subtree sig matches
    pub inst: usize,
    /// per-instant memo: region slot base (stamp word, then value
    /// words) — None for unstable pieces
    pub memo_slot: Option<u32>,
    /// unbound data-port reads: helper parameters, signature order
    pub ports: Vec<(StrId, u32)>,
}
/// Edge-SSA emission plan (task #24 M2): everything the whole-edge
/// inlining emitter needs beyond the FusedComp symbol lists.
/// `nodes` mirrors the per-comp FusedComp node order but carries SPEC
/// ORDINALS so sections lower inline; the read/write tables drive the
/// online eviction that enforces the sharing doctrine.
pub struct EdgeSsaPlan {
    /// per composition: (is_exec, spec ordinal) in schedule order
    pub nodes: Vec<Vec<(bool, usize)>>,
    /// exec ordinals whose bodies stay OUTLINED (called as the
    /// standalone exec_<class> symbol from the edge fn instead of
    /// inlining): the link-time dial — monster bodies bound the
    /// mega-function while small bodies keep full SSA sharing.
    /// Outlined ordinals keep their symbols (excluded from elision)
    /// and their class dedup.
    pub outlined_execs: std::collections::HashSet<usize>,
    /// per spec ordinal: prim instances its exec body writes
    pub exec_writes: Vec<Vec<usize>>,
    /// per (instance, def): prim instances its cone reads with NO
    /// stability contract; defs ABSENT from this table must never be
    /// cached across sections (conservative)
    pub def_reads: HashMap<(usize, StrId), Vec<usize>>,
    /// per composition, per section index: shared PURE defs to hoist
    /// (computed unconditionally before the section — first-consumer
    /// position; pure = no warning-emitting or callback reads, so the
    /// unconditional evaluation is output-invisible)
    pub hoists: Vec<Vec<Vec<(usize, StrId)>>>,
    /// slots whose stores survive export elision (see EdgeCtx::exports)
    pub export_slots: std::collections::HashSet<u32>,
    /// per composition: arena valid-slot numbers of ungated wire ticks
    /// to clear (store 0) at the END of the edge fn — the compiled form
    /// of RWire/PulseWire::tick (the boxed `written` latch only feeds
    /// VCD, where the interpreter runs ticks itself)
    pub wire_clears: Vec<Vec<u32>>,
    /// per composition: (value base slot, words) of ungated CReg ticks —
    /// the compiled form of CReg::tick is a copy of the live value into
    /// the registered value (arena words [base, base+w) -> [base+w,
    /// base+2w)); the boxed per-port history only feeds VCD
    pub creg_copies: Vec<Vec<(u32, u32)>>,
    /// per composition: packed trs_bram_tick argument triples of
    /// ungated BRAM port ticks — the edge fn calls the helper through
    /// the trs_bram_tick_cb pointer-global (filled at artifact load)
    pub bram_ticks: Vec<Vec<[u64; 3]>>,
}

/// Pack one BRAM port tick into trs_bram_tick's (a0, a1, a2) args.
pub fn bram_tick_args(
    base: u32,
    port_b: bool,
    width: u32,
    size: u64,
    chunk_size: u32,
    num_wens: u32,
    dual: bool,
) -> [u64; 3] {
    [
        base as u64 | (port_b as u64) << 32,
        width as u64 | (chunk_size as u64) << 32,
        size | (num_wens as u64) << 32 | (dual as u64) << 62,
    ]
}

/// Compiled BRAM end-of-edge tick (the arena form of Bram::clk): the
/// fused edge fn calls this through the trs_bram_tick_cb
/// pointer-global once per BRAM port tick.  Layout per
/// ArenaKind::Bram; args packed by bram_tick_args.  Must mirror the
/// interpreter's Bram::clk exactly: out2 <- out rotation, pending-put
/// latch, byte-enable lane merge, cross-port same-instant bypass,
/// out-of-range -> undet (the WARNING printed at put time on the
/// trampoline).
///
/// # Safety
/// `arena` must be the design arena; the packed args must describe a
/// BRAM block passB allocated inside it.
pub unsafe extern "C" fn trs_bram_tick(
    arena: *mut u64,
    now: u64,
    a0: u64,
    a1: u64,
    a2: u64,
) {
    let base = (a0 & 0xffff_ffff) as usize;
    let port_b = a0 >> 32 & 1 != 0;
    let width = (a1 & 0xffff_ffff) as u32;
    let chunk = (a1 >> 32) as u32;
    let size = a2 & 0xffff_ffff;
    let num_wens = ((a2 >> 32) & 0x3fff_ffff) as u32;
    let dual = a2 >> 62 & 1 != 0;
    let w = (width.max(1) as usize).div_ceil(64);
    let wenw = (num_wens.max(1) as usize).div_ceil(64);
    let pw = 3 + wenw + 4 * w;
    let me = base + if port_b { pw } else { 0 };
    let other = base + if port_b { 0 } else { pw };
    let (o_wens, o_val) = (3, 3 + wenw);
    let (o_prev, o_out, o_out2) =
        (3 + wenw + w, 3 + wenw + 2 * w, 3 + wenw + 3 * w);
    // out2 <- out (unconditional rotation, like the boxed clk)
    unsafe {
        std::ptr::copy_nonoverlapping(
            arena.add(me + o_out),
            arena.add(me + o_out2),
            w,
        );
        if *arena.add(me) != now {
            return;
        }
        let addr = *arena.add(me + 1);
        let wens_zero =
            (0..wenw).all(|i| *arena.add(me + o_wens + i) == 0);
        if addr >= size {
            // out-of-range: undet pattern, masked to width
            for i in 0..w {
                *arena.add(me + o_out + i) = 0xAAAA_AAAA_AAAA_AAAA;
            }
            mask_top(arena.add(me + o_out), width, w);
            return;
        }
        let daddr = base + pw * if dual { 2 } else { 1 } + addr as usize * w;
        // cross-port same-instant bypass: the other port wrote this
        // address at this instant -> its pre-write value
        let other_hit = dual
            && *arena.add(other + 2) == now
            && *arena.add(other + 1) == addr;
        if !wens_zero {
            // write: prev <- (bypass ? other.prev : data), then merge
            // the enabled lanes of upd_val into data, out <- merged
            *arena.add(me + 2) = now;
            if other_hit {
                std::ptr::copy_nonoverlapping(
                    arena.add(other + o_prev),
                    arena.add(me + o_prev),
                    w,
                );
            } else {
                std::ptr::copy_nonoverlapping(
                    arena.add(daddr),
                    arena.add(me + o_prev),
                    w,
                );
            }
            for n in 0..num_wens {
                let lane = *arena.add(me + o_wens + (n / 64) as usize)
                    >> (n % 64)
                    & 1;
                if lane == 0 {
                    continue;
                }
                if n * chunk >= width {
                    continue;
                }
                let lo = (n * chunk) as usize;
                let len = chunk.min(width - n * chunk) as usize;
                for b in lo..lo + len {
                    let bit =
                        *arena.add(me + o_val + b / 64) >> (b % 64) & 1;
                    let d = arena.add(daddr + b / 64);
                    *d = *d & !(1u64 << (b % 64)) | bit << (b % 64);
                }
            }
            std::ptr::copy_nonoverlapping(
                arena.add(daddr),
                arena.add(me + o_out),
                w,
            );
        } else {
            // read: bypassed pre-write value or the stored data
            let src = if other_hit { other + o_prev } else { daddr };
            std::ptr::copy_nonoverlapping(
                arena.add(src),
                arena.add(me + o_out),
                w,
            );
        }
    }
}

/// Mask the top word of a `words`-long little-endian value to `width`.
///
/// # Safety
/// `p` must point at `words` valid u64s.
unsafe fn mask_top(p: *mut u64, width: u32, words: usize) {
    let rem = width % 64;
    if width != 0 && rem != 0 {
        unsafe { *p.add(words - 1) &= (1u64 << rem) - 1 };
    }
}

/// One node of a fused per-composition edge function.
pub enum FusedNode {
    /// sched fn: baked address (JIT) or symbol (AOT)
    Sched(HelperRef),
    /// exec fn + its (region base, token base) args
    Exec(HelperRef, u64, u64),
}

/// A composition's fused edge: EN slots to zero, then the node
/// sequence as DIRECT calls — replaces the interpreter's per-node
/// walk (match + atomic cell load + indirect call, ~77M visits on
/// sudoku).  Returns nonzero when a body aborted (reserved path;
/// $finish/$stop complete the edge and return 0).
pub struct FusedComp {
    pub en_slots: Vec<u32>,
    pub now_slot: u32,
    pub nodes: Vec<FusedNode>,
}
