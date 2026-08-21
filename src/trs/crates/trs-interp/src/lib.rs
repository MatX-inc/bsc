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
pub mod fst;
mod vcd;
pub mod startup;
pub use vcd::WaveFormat;

use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet};

use trs_ir as ir;
use trs_ir::{Action, Design, Expr, PrimOp, SchedNode, Stmt, StrId};

mod bdpi;
mod foreign;
pub mod topbind;
pub use topbind::{parse_bind, TopBind};
#[cfg(feature = "aot")]
mod jit;
#[cfg(feature = "aot")]
mod runcore;

use foreign::ForeignEnv;
use format::Arg;
use prim::Prim;
use value::Value;

#[cfg(feature = "aot")]
use jit::JitPlans;
/// Placeholder so Stepper's field exists (always None) without the
/// `jit` feature.
#[cfg(not(feature = "aot"))]
type JitPlans = std::convert::Infallible;
#[cfg(feature = "aot")]
type JitShared = std::sync::Arc<jit::LazyJit>;
#[cfg(not(feature = "aot"))]
type JitShared = std::convert::Infallible;
#[cfg(feature = "aot")]
type JitRequestT = jit::JitRequest;
#[cfg(not(feature = "aot"))]
type JitRequestT = ();

/// central-loop bail diagnostics (task #21), dumped by finish() under
/// TRS_JIT_TRACE
static CENTRAL_BAIL: [std::sync::atomic::AtomicUsize; 16] =
    [const { std::sync::atomic::AtomicUsize::new(0) }; 16];

/// BDPI imports satisfied by the Bluesim library itself (libbsprim.a's
/// rand32.cxx), not by user C files.
fn is_lib_bdpi(c_name: &str) -> bool {
    matches!(c_name, "rand32" | "srand")
}

/// glibc random() (TYPE_3, trinomial x^31 + x^3 + 1), reimplemented so
/// every Interp owns its OWN stream.  The reference's rand32.cxx calls
/// libc random(), whose state is process-global — fine for one model
/// per process, but the lockstep selfcheck and the bluetcl
/// multi-engine oracle run several engines in one process, and a
/// shared stream interleaves: each engine sees a different
/// subsequence (witness: sysTest_mkNonPipelinedDivider, identical
/// stdout with divergent Randomize-fed state).  Verified word-exact
/// against glibc for 1000 draws under the default seed and srandom(N)
/// reseeding (scratch rngtest.c).
pub(crate) struct GlibcRandom {
    state: [u32; 31],
    f: usize,
    r: usize,
}

impl GlibcRandom {
    pub(crate) fn new() -> GlibcRandom {
        // glibc's initial state is as if srandom(1)
        let mut g = GlibcRandom { state: [0; 31], f: 3, r: 0 };
        g.srandom(1);
        g
    }
    pub(crate) fn srandom(&mut self, seed: u32) {
        let seed = if seed == 0 { 1 } else { seed };
        self.state[0] = seed;
        for i in 1..31 {
            // 16807 * prev % 2^31-1 via Schrage's method, exactly as
            // glibc computes it (signed intermediate)
            let prev = self.state[i - 1] as i32 as i64;
            let hi = prev / 127773;
            let lo = prev % 127773;
            let mut word = 16807 * lo - 2836 * hi;
            if word < 0 {
                word += 2147483647;
            }
            self.state[i] = word as u32;
        }
        self.f = 3;
        self.r = 0;
        for _ in 0..310 {
            self.next();
        }
    }
    pub(crate) fn next(&mut self) -> u32 {
        self.state[self.f] = self.state[self.f].wrapping_add(self.state[self.r]);
        let res = (self.state[self.f] >> 1) & 0x7fff_ffff;
        self.f = (self.f + 1) % 31;
        self.r = (self.r + 1) % 31;
        res
    }
}

// ===============
// Indexed design

struct ModIx {
    ir: usize, // index into design.modules
    ports: HashMap<StrId, (u32, ir::PortKind)>,
    defs: HashMap<StrId, usize>,
    rules: HashMap<StrId, usize>,
    methods: HashMap<StrId, usize>,
}

pub struct Interp {
    // owned, not Arc: the eval loop touches this on every expression,
    // and LazyJit's need for a copy is served by a one-shot clone only
    // when cold compilation is possible (jit.rs LazyJit.design)
    d: Design,
    /// console/file/finish/plusargs/timescale state for the foreign
    /// task family, split out (foreign.rs) so the compiled tier's
    /// foreign bounces can someday be serviced without the Interp
    fe: ForeignEnv,
    /// runtime-created strings (PrimStringConcat results); string ids at
    /// or past the design table's length index into this arena
    dyn_strs: Vec<String>,
    /// dlopened user BDPI code (from the companion .bdpi.so)
    bdpi: Option<bdpi::Bdpi>,
    /// capi Jit engine: arm the hybrid JIT without the TRS_JIT env
    /// (jit.rs run-mode gate honors this flag too)
    pub(crate) jit_armed: bool,
    mods: Vec<ModIx>,
    mod_by_name: HashMap<StrId, usize>,
    /// instance path -> instance state index
    inst_by_path: HashMap<String, usize>,
    insts: Vec<Inst>,
    cycle: u64,
    /// current simulation time (the time of the executing clock edge)
    now: u64,
    /// "<path>$CLK_OUT" -> waveform, captured from ClockGen instantiation
    /// args (bs_prim_mod_clockgen.h set_clk_0 -> bk_alter_clock)
    clockgen_waves: HashMap<String, Wave>,
    /// "<path>$CLK_OUT" -> initial level for dynamic clocks (MakeClock,
    /// ClockDiv, ClockInverter) whose edges are prim-triggered
    dynclk_init: HashMap<String, bool>,
    /// Reset network.  Node 0 is the top reset (kernel-driven: asserted
    /// at t=0, deasserted at t=2 after that instant's logic); other nodes
    /// are derived resets generated by reset primitives.  Rule bodies
    /// carry their own reset guards as exported Cond statements over the
    /// reset wire ports, so the interpreter only has to answer "is this
    /// reset wire asserted" and drive prim reset lines.
    rst_asserted: Vec<bool>,
    /// node -> (prim instance, reset-arg ordinal) subscriptions
    rst_subs: Vec<Vec<(usize, usize)>>,
    /// reset-generating prim instance -> the node its OUT_RST drives
    rstgen_out: HashMap<usize, usize>,
    /// number of currently-asserted reset nodes: reset ticks are pure
    /// no-ops while this is 0 (rst_tick acts only in_reset), so the
    /// per-edge tick loop skips them in steady state
    rst_active: usize,
    /// deferred (end-of-timeslice) reset transitions, mirroring
    /// reset_at_end_of_timeslice in bs_prim_mod_resets.h
    rst_pending: Vec<(usize, bool)>,
    /// reset nodes asserted from time 0 (InitialReset outputs), broadcast
    /// at run() start once instantiation is complete
    initial_asserts: Vec<usize>,
    /// VCD writer (vcd.rs, docs/VCD-CONTRACT.md)
    vcd: vcd::Vcd,
    /// record last-computed def values / method calls for VCD dumps (set
    /// when -V is given or the design contains a $dump* task)
    vcd_trace: bool,
    /// DEBUG-tier engine (bluetcl capi): exempt from the
    /// TRS_REQUIRE_AOT strict-execution refusal (see set_debug_tier)
    debug_tier: bool,
    /// TRS_TRACE / TRS_TRACE_CLK, read ONCE at construction: the
    /// per-event checks sat on getenv, and glibc getenv is a linear
    /// environ scan under the env lock — sampled at ~70% of
    /// sysFloatTest's wall (call_value/call_action/Def all checked
    /// per event)
    trace_events: bool,
    trace_clk: bool,
    /// foreign-call scratch (jit_foreign_cb): argv spine + task-name
    /// and %m-location buffers, reused across calls
    foreign_argv: Vec<Arg>,
    fname_buf: String,
    loc_buf: String,
    /// string id -> Arc'd text for Arg::Str: interned once, cloned as
    /// a refcount bump on every later call
    arg_strs: HashMap<u32, std::sync::Arc<str>>,
    /// Emit requests stash the serialized PlanA here for the meta
    /// object (prime derives it; the aot_emit call site reads it)
    #[cfg(feature = "aot")]
    plan_a_bytes: Option<Vec<u8>>,
    trace_wf: bool,
    /// batch waveform request (-V / +bscvcd / +bscfst), consumed at
    /// the stepper build: format + file (None = the format's default)
    wave_pending: Option<(WaveFormat, Option<String>)>,
    /// a batch waveform request was ARMED (survives the wave_pending
    /// take): jit_plan's wave-engine gate reads this — checking
    /// wave_pending there was dead code (prime consumes it first), so
    /// the hybrid JIT raced the dump and boxed-only VCD bookkeeping
    /// (FIFO D_IN) froze at a thread-timing-dependent cycle: the
    /// waveform was both WRONG (4,933 D_IN changes collapsed to ~1-7
    /// on TrafficBRAM) and nondeterministic run-to-run
    wave_engine: bool,
    /// last computed value of each def, per instance — the C++ member
    /// fields persist between edges, so dumps show the value from the
    /// last time the def was computed (write_undet pattern before that)
    vcd_def_vals: HashMap<(usize, StrId), Value>,
    /// last (edge time, args) of each user-module method call, for the
    /// EN_<m>/<m>_<arg> port values
    vcd_meth_calls: HashMap<(usize, StrId), (u64, Vec<Value>)>,
    /// last returned value of each user-module value-method call, for
    /// result-port values (C++ assigns PORT_<m> at call time)
    vcd_meth_results: HashMap<(usize, StrId), Value>,
    /// per-instance kernel clock index (from the composition that runs it)
    vcd_inst_clock: Vec<usize>,
    /// (instance, module clock domain) -> kernel clock index
    vcd_inst_domclock: HashMap<(usize, u32), usize>,
    /// kernel clock VCD state, in composition clock order
    vcd_clocks: Vec<VcdClock>,
    /// per-instance scope id block + change backing, built at header time
    vcd_layouts: HashMap<usize, VcdLayout>,
    /// cached member/port selection per module type
    vcd_mod_vars: HashMap<usize, std::rc::Rc<ModVars>>,
    /// resumable event-loop state, built once by prime(); run() =
    /// prime + advance + finish
    stepper: Option<Stepper>,
    /// lazy JIT compile cells; the compiled-code callbacks resolve
    /// their call-site specs through this (rule ordinal -> cell)
    jit_shared: Option<JitShared>,
    /// (instance, EN port) -> arena slot: interpreted method calls
    /// during body fallback write EN through so native scheds see them
    jit_en_slots: HashMap<(usize, StrId), u32>,
    /// (instance, def) -> (arena base, width) for fire signals and
    /// schedule-position defs: interpreted evaluation falls through to
    /// the slots the native scheds keep current
    jit_eager_slots: HashMap<(usize, StrId), (u32, u32)>,
    /// what prime()'s planning pass should do: JIT in-process (default),
    /// emit an AOT artifact, or load one (trs link / run --code)
    pub(crate) jit_request: JitRequestT,
    /// outcome of an Emit request (trs link reads this after prime)
    pub(crate) jit_emit_result: Option<AotEmit>,
    /// FNV-1a fingerprint of the loaded .bir bytes (artifact check)
    pub(crate) bir_hash: u64,
    /// raw view of the JIT arena for reset mirroring (null = JIT off);
    /// the owning allocation lives in Stepper::jit
    jit_arena_ptr: *mut u64,
    /// arena length in slots (0 = JIT off); with jit_arena_ptr this
    /// gives the RunCore image encoder a safe slice view
    pub(crate) jit_arena_len: usize,
    /// RunCore arena image encoded at plan tail on an Emit request —
    /// the linker CLI writes it beside the artifact (see
    /// jit::Interp::runcore_image_encode)
    pub(crate) runcore_pending: Option<Vec<u8>>,
    /// boot-descriptor stage A (tick coverage + warn rows), captured
    /// by the Emit arm for prime's runcore_desc_finish
    #[cfg(feature = "aot")]
    pub(crate) runcore_stage_a: Option<jit::RunCoreStageA>,
    /// link-time window bake armed (jit::runcore_bake_window): the
    /// central loop's engage point captures the post-window sections
    /// plus the WINDOW_EFFECTS reading AT capture (the bake advances
    /// one steady cycle past the boundary; its effects must not
    /// pollute the window-clean gate)
    #[cfg(feature = "aot")]
    pub(crate) runcore_bake: bool,
    #[cfg(feature = "aot")]
    pub(crate) runcore_window: Option<(Vec<u8>, u64)>,
    /// the central loop engaged at least once this run — the inverse
    /// half of the RunCore descriptor's eligibility witness
    central_engaged: bool,
    /// reset node -> arena slot holding the port level (1 = deasserted)
    jit_reset_slots: Vec<u32>,
    /// TRACED artifact only: (instance, VCD-declared def) -> recording
    /// slot (base, width).  When a key has a slot, the slot is the
    /// single authority — interp-side recording writes it (not the
    /// vcd_def_vals map) and the writer reads it first.
    jit_rec_defs: HashMap<(usize, StrId), (u32, u32)>,
    /// TRACED artifact only: (instance, method) -> recording slots for
    /// the method's VCD ports (EN time / args / result)
    jit_rec_meths: HashMap<(usize, StrId), RecSlots>,
    /// per-engine $random/$srandom stream (library rand32/srand BDPI)
    rng: GlibcRandom,
    /// identity salt of the top-level bindings (topbind); folded into
    /// bir_hash by the loaders so a compiled artifact never matches a
    /// run with different baked constants.  0 = nothing bound.
    top_binds_salt: u64,
    /// +NAME=value arguments consumed as bindings — filtered out of
    /// the design-visible plusargs by the loaders
    consumed_plus: Vec<String>,
    /// always_enabled Action methods of the top auto-fired in batch
    /// mode (interface order), each with its constant argument values
    autofire: Vec<(StrId, Vec<Value>)>,
    /// (composition index, entry index) -> autofire indices to invoke
    /// after that entry's nodes — the methods' Exec cut positions
    /// (resolved by topbind::resolve; keys line up with rcomps)
    autofire_at: HashMap<(usize, usize), Vec<usize>>,
    /// composition index -> autofire indices invoked before the entry
    /// walk (Exec cuts preceding every node-bearing top segment)
    autofire_pre: HashMap<usize, Vec<usize>>,
}

/// Arena recording slots for one user-module method's VCD ports
/// (traced artifacts): the runtime mirror of the codegen-side layout.
#[derive(Clone, Default)]
struct RecSlots {
    /// last-call time slot (init u64::MAX; PortEn = time == pos_at)
    t: u32,
    /// per-argument (base, port width), in method arg order (init 0)
    args: Vec<(u32, u32)>,
    /// result (base, width) for value/AV methods (init 0)
    res: Option<(u32, u32)>,
}

/// Kernel-side clock state mirrored for VCD (tClockInfo essentials).
struct VcdClock {
    name: String,
    vcd_id: u32,
    /// current level (bk_clock_val)
    cur: bool,
    /// bk_alter_clock has_initial_value
    has_init: bool,
    /// posedge count (bk_clock_cycle_count)
    pos_count: u64,
    /// negedge count (bk_clock_edge_count's other direction)
    neg_count: u64,
    pos_at: u64,
    neg_at: u64,
    /// value before the first edge (bk_clock_initial_value)
    init_val: bool,
    /// time of the first edge (bk_clock_first_edge)
    first_edge: Option<u64>,
    /// waveform durations (bk_clock_duration; 0 for derived clocks)
    low_dur: u64,
    high_dur: u64,
}

/// Method-port flavors for the debug-tier symbol tree (SYM_PORT).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MethPortKind {
    En,
    Arg(usize),
    Rdy,
    Result,
}

/// One kernel clock's state for the driver's `sim clock` (the
/// getClockInfo tuple bluetcl prints).
pub struct ClockInfo {
    pub name: String,
    pub initial_val: bool,
    pub first_edge: u64,
    pub low_dur: u64,
    pub high_dur: u64,
    pub cycles: u64,
    pub neg_edges: u64,
    pub cur_val: bool,
    pub last_edge: u64,
}

/// Where a module-scope VCD variable's value comes from.
#[derive(Clone)]
enum VcdSrc {
    /// reset wire (sb_resetDefs): dumps !asserted
    Reset(StrId),
    /// member def: last computed value (write_undet pattern initially)
    Def(StrId),
    /// EN_<m>: 1 when the method executed at the clock's last posedge
    PortEn(StrId),
    /// method argument port: value from the last call
    PortArg(StrId, usize),
    /// value/RDY method result port: evaluated on demand
    PortRes(StrId),
}

/// One module-scope $var: display name, value source, width, and whether
/// its changes back-date to the module's clock (vcd_set_clock).
#[derive(Clone)]
struct ModVar {
    name: String,
    src: VcdSrc,
    width: u32,
    clocked: bool,
    /// module clock domain whose composition clock backdates this var
    domain: Option<u32>,
}

/// Member/port var lists for one module type, in $var emission order.
struct ModVars {
    members: Vec<ModVar>,
    ports: Vec<ModVar>,
}

/// Per-instance id block (base id of members++ports) and change backing.
struct VcdLayout {
    base: u32,
    back: Vec<Option<Value>>,
}

/// A periodic clock waveform.  The default clock is LOW with first edge
/// at t=0, high 5 / low 5 (the generated model's bk_alter_clock call), so
/// posedges land at 0 (in reset), 10, 20, ...
#[derive(Clone, Copy)]
#[derive(serde::Serialize, serde::Deserialize)]
struct Wave {
    init_high: bool,
    delay: u64,
    hi: u64,
    lo: u64,
    /// bk_alter_clock has_initial_value: an extra one-shot edge at t=0 in
    /// the direction of the initial value, at PG_INITIAL priority (before
    /// regular t=0 edges).  True for ClockGen, false for the default CLK.
    has_init: bool,
}

/// How a clock's edges are produced: a periodic waveform, triggered at
/// runtime by a clock-generating primitive (bk_trigger_clock_edge), or
/// never (noClock and top-level input clocks with no waveform — the
/// kernel defines them with period 0 and they never fire).
#[derive(Clone, Copy)]
#[derive(serde::Serialize, serde::Deserialize)]
enum ClockSource {
    Wave(Wave),
    Triggered { init_high: bool, driver: usize },
    Never,
}

/// One composition pre-resolved against the instance tree: the kernel
/// clock index and edge it fires on, plus entries, cross-inhibits and
/// ticks in directly executable form.
/// One resolved schedule entry: a segment's nodes for one instance, plus
/// the defs the C++ schedule computes eagerly at this position.
#[derive(serde::Serialize, serde::Deserialize)]
struct REntry {
    inst: usize,
    /// module clock domain (used for VCD wiring at prime time)
    domain: u32,
    /// schedule segment nodes, resolved once at prime() so the per-edge
    /// pass never searches domain schedules or clones node lists
    nodes: Vec<SchedNode>,
    /// defs the C++ schedule_posedge_* computes at this position: the
    /// CAN_FIRE/WILL_FIRE cone of this entry's Sched rules (getExprIds
    /// True in SimMakeCBlocks), transitively through every mux arm,
    /// minus defs already attached to an earlier entry of the same
    /// composition, in module def-table (dependency) order.  Hoisted
    /// prim value-method calls live here, so their side effects (RegFile
    /// bounds warnings) fire once per edge even when no rule runs; rule
    /// bodies that alias these defs (C++ `DEF_x = DEF_x;`) reuse the
    /// schedule-time values via the latch instead of recomputing against
    /// later mid-cycle state.  CAN_FIRE/WILL_FIRE defs themselves are
    /// traversed but never latched: rule CF/WFs are computed (with
    /// inhibitors) by latch_rule, and method WFs follow the call-time
    /// EN protocol (the C++ schedule re-derives them from EN ports after
    /// the calling rules have run).
    eager: Vec<StrId>,
}

#[derive(serde::Serialize, serde::Deserialize)]
struct RComp {
    clk: usize,
    posedge: bool,
    entries: Vec<REntry>,
    cross: HashMap<(usize, StrId), Vec<(usize, StrId)>>,
    // (prim instance, resolved port name, is_reset_tick, owner instance
    // for gate evaluation, gate expr)
    ticks: Vec<(usize, String, bool, usize, Option<Expr>)>,
    // clock-crossing "early" rules: excluded from the edge pass,
    // run in the after-edge pass at end of timeslice (the C++
    // schedule_after_posedge_* at PG_FINAL)
    early: HashSet<(usize, StrId)>,
}

/// prime()'s derivation half, baked into AOT artifacts as trs_plan_a:
/// the pre-resolved schedule a --code run would otherwise re-derive
/// through string-keyed walks (split_qual, inst_by_path, eager cones).
/// Deterministic from the Design; trace-independent.  The blob is
/// versioned (bincode is positional — a silent field reorder must
/// fail the decode, not skew the schedule) and any gate failure falls
/// back to fresh derivation.
#[derive(serde::Serialize, serde::Deserialize)]
pub(crate) struct PlanA {
    version: u32,
    clocks: Vec<StrId>,
    sources: Vec<ClockSource>,
    driver_clock: Vec<(usize, usize)>,
    rcomps: Vec<RComp>,
}

pub(crate) const PLAN_A_VERSION: u32 = 1;

/// Collect the def names an expression references, descending into every
/// subexpression (both mux arms — the C++ schedule assigns eagerly).
fn collect_def_refs(e: &Expr, out: &mut Vec<StrId>) {
    match e {
        Expr::Def(d) => out.push(*d),
        Expr::MethCall { args, .. } | Expr::ForeignCall { args, .. } => {
            for a in args {
                collect_def_refs(a, out);
            }
        }
        Expr::Prim { args, .. } => {
            for a in args {
                collect_def_refs(a, out);
            }
        }
        Expr::If { cond, then_, else_, .. } => {
            collect_def_refs(cond, out);
            collect_def_refs(then_, out);
            collect_def_refs(else_, out);
        }
        Expr::Case { scrutinee, arms, default, .. } => {
            collect_def_refs(scrutinee, out);
            for (_, a) in arms {
                collect_def_refs(a, out);
            }
            collect_def_refs(default, out);
        }
        Expr::Clock { osc, gate } => {
            collect_def_refs(osc, out);
            collect_def_refs(gate, out);
        }
        Expr::Reset { wire } => collect_def_refs(wire, out),
        Expr::Const { .. }
        | Expr::Port(_)
        | Expr::Param(_)
        | Expr::MethValue { .. }
        | Expr::TaskValue { .. }
        | Expr::Str(_)
        | Expr::Real(_)
        | Expr::Gate { .. } => {}
    }
}

/// Resumable event-loop state.  Everything run() used to build locally
/// lives on the Interp so a driver (`sim step N`) or the JIT harness
/// (run-to-cycle, compare, continue) can advance in bounded steps.
/// Where `advance_until` stops, beyond $finish — the kernel's stop
/// machinery behind bk_advance (bk_quit_after_edge / bk_quit_at /
/// UI events).  Targets are ABSOLUTE (bluetcl passes current
/// count + N).
#[derive(Clone, Debug)]
pub struct StopCond {
    /// default-clock posedge budget (advance(max_cycles) legacy)
    pub max_cycles: u64,
    /// stop at the end of the timeslice in which edge #count of
    /// (kernel clock index, posedge?) completes
    pub edge_limits: Vec<(usize, bool, u64)>,
    /// stop at the end of timeslice t; time advances to t even when
    /// no design event lands there
    pub at_times: Vec<u64>,
    /// bk_abort_now: stop at the next slice boundary when set
    /// (checked between slices — "end of cycle" semantics)
    pub abort: Option<std::sync::Arc<std::sync::atomic::AtomicBool>>,
    /// async runs publish the current slice time here so bk_now can
    /// answer live from another thread
    pub progress: Option<std::sync::Arc<std::sync::atomic::AtomicU64>>,
}

impl Default for StopCond {
    fn default() -> Self {
        StopCond {
            max_cycles: u64::MAX,
            edge_limits: Vec::new(),
            at_times: Vec::new(),
            abort: None,
            progress: None,
        }
    }
}

impl StopCond {
    fn trivial(&self) -> bool {
        self.edge_limits.is_empty()
            && self.at_times.is_empty()
            && self.abort.is_none()
            && self.progress.is_none()
    }
}

struct Stepper {
    /// distinct clocks in first-appearance order, default clock first
    clocks: Vec<StrId>,
    sources: Vec<ClockSource>,
    /// clock-driver prim instance -> clock index, for routing triggered
    /// edges after ticks
    driver_clock: HashMap<usize, usize>,
    rcomps: Vec<RComp>,
    /// event heap over (time, priority, clock idx, is_posedge);
    /// priority 0 = one-shot initial edges (PG_INITIAL), 1 = regular
    /// waveform/triggered edges (PG_LOGIC) — initial edges run first
    /// within a timeslice and never period-reschedule
    heap: BinaryHeap<Reverse<(u64, u8, usize, bool)>>,
    /// (clock,edge) compositions that fired in the current timeslice,
    /// for the after-edge (PG_FINAL) pass
    fired_this_slice: Vec<usize>,
    /// the time the simulation is considered to have stopped at, for
    /// the final VCD flush (buffered changes at exactly this time are
    /// dropped, matching vcd_reset at bk_shutdown)
    final_now: u64,
    /// compiled dispatch state (feature `jit` + TRS_JIT=1); None runs
    /// the interpreted entries loop
    jit: Option<JitPlans>,
}

impl Interp {
    /// Identity salt of the top-level bindings, already folded into
    /// `bir_hash` by the loaders: a compiled artifact's stamp must
    /// not match a run with different baked constants.
    pub fn top_binds_salt(&self) -> u64 {
        self.top_binds_salt
    }

    /// `+NAME=value` arguments consumed as top-level bindings — the
    /// loaders filter these out of the design-visible plusargs.
    pub(crate) fn consumed_plus(&self) -> &[String] {
        &self.consumed_plus
    }

    /// True when batch mode auto-fires always_enabled top methods
    /// (the design runs interpreted; `trs link --interactive` and
    /// `--exe` refuse such designs).
    pub fn has_autofire(&self) -> bool {
        !self.autofire.is_empty()
    }
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
        /// string-valued parameters: port name -> design string id
        /// (kept as ids so dynamic muxes carry them as marker values)
        str_params: HashMap<StrId, StrId>,
        /// local reset wire name -> reset node (module reset inputs bound
        /// at instantiation; derived resets created by child reset prims)
        resets: HashMap<StrId, usize>,
        /// local clock-gate port name (e.g. CLK_GATE_gclk) -> the gate
        /// expression it was instantiated with, evaluated in the parent
        /// instance (mkGateSubstMap semantics)
        gates: HashMap<StrId, (usize, Expr)>,
        /// local input clock port name (e.g. CLK_gclk) -> the osc
        /// expression it was instantiated with, resolved in the parent
        /// instance (used to chase interface/laundered clocks to their
        /// driving oscillator)
        clk_binds: HashMap<StrId, (usize, Expr)>,
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
        // reachable only for designs with no bindable top surface
        // (every CLI/capi path goes through new_bound); a top that
        // needs bindings fails loudly rather than reading zeros
        Interp::new_bound(d, &[]).unwrap_or_else(|e| panic!("trs: {e}"))
    }

    /// Construct with top-level bindings (see `topbind`): resolves
    /// +NAME=value constants against the top module's arguments and
    /// always_enabled method arguments BEFORE instantiation — child
    /// instantiation arguments may reference the top's parameters, so
    /// the values must be present when the instance tree is built.
    pub fn new_bound(
        d: Design,
        binds: &[topbind::TopBind],
    ) -> Result<Interp, String> {
        let rb = topbind::resolve(&d, binds)?;
        let str_ids: HashMap<&str, StrId> = d
            .strings
            .iter()
            .enumerate()
            .map(|(i, s)| (s.as_str(), i as StrId))
            .collect();
        let mods: Vec<ModIx> = d
            .modules
            .iter()
            .enumerate()
            .map(|(i, m)| ModIx {
                ir: i,
                // module inputs, plus method argument ports and the EN_<meth>
                // enable ports their WILL_FIRE defs read — an uncalled
                // method's EN reads as 0 (see Expr::Port in eval)
                ports: m
                    .inputs
                    .iter()
                    .map(|p| (p.name, (p.width, p.kind)))
                    .chain(m.methods.iter().flat_map(|me| {
                        me.args
                            .iter()
                            .map(|a| (a.name, (a.width, a.kind)))
                            .chain(
                                str_ids
                                    .get(format!("EN_{}", d.strings[me.name as usize]).as_str())
                                    .map(|&en| (en, (1, ir::PortKind::MethodEnable))),
                            )
                            .collect::<Vec<_>>()
                    }))
                    .collect(),
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
            fe: ForeignEnv::new(),
            dyn_strs: Vec::new(),
            bdpi: None,
            jit_armed: false,
            cycle: 0,
            now: 0,
            clockgen_waves: HashMap::new(),
            dynclk_init: HashMap::new(),
            rst_asserted: vec![false],
            rst_subs: vec![Vec::new()],
            rstgen_out: HashMap::new(),
            rst_active: 0,
            rst_pending: Vec::new(),
            initial_asserts: Vec::new(),
            vcd: vcd::Vcd::new(),
            vcd_trace: false,
            debug_tier: false,
            trace_events: std::env::var_os("TRS_TRACE").is_some(),
            trace_clk: std::env::var_os("TRS_TRACE_CLK").is_some(),
            foreign_argv: Vec::new(),
            fname_buf: String::new(),
            loc_buf: String::new(),
            arg_strs: HashMap::new(),
            #[cfg(feature = "aot")]
            plan_a_bytes: None,
            trace_wf: std::env::var_os("TRS_TRACE_WF").is_some(),
            wave_pending: None,
            wave_engine: false,
            vcd_def_vals: HashMap::new(),
            vcd_meth_calls: HashMap::new(),
            vcd_meth_results: HashMap::new(),
            vcd_inst_clock: Vec::new(),
            vcd_inst_domclock: HashMap::new(),
            vcd_clocks: Vec::new(),
            vcd_layouts: HashMap::new(),
            vcd_mod_vars: HashMap::new(),
            stepper: None,
            jit_shared: None,
            jit_en_slots: HashMap::new(),
            jit_eager_slots: HashMap::new(),
            jit_request: Default::default(),
            jit_emit_result: None,
            bir_hash: 0,
            jit_arena_ptr: std::ptr::null_mut(),
            jit_arena_len: 0,
            runcore_pending: None,
            #[cfg(feature = "aot")]
            runcore_stage_a: None,
            #[cfg(feature = "aot")]
            runcore_bake: false,
            #[cfg(feature = "aot")]
            runcore_window: None,
            central_engaged: false,
            jit_reset_slots: Vec::new(),
            jit_rec_defs: HashMap::new(),
            jit_rec_meths: HashMap::new(),
            rng: GlibcRandom::new(),
            top_binds_salt: rb.salt,
            consumed_plus: rb.consumed_plus,
            autofire: rb.autofire,
            autofire_at: rb.autofire_at,
            autofire_pre: rb.autofire_pre,
        };
        // def/method-call recording must run from t=0 if the design can
        // ever start dumping ($dump* task present); -V sets it too
        it.vcd_trace = it.d.strings.iter().any(|s| s.starts_with("$dump"));
        let top_mod = it.mod_by_name[&it.d.top];
        // the top module's reset inputs are all the kernel-driven reset
        let top_binds: HashMap<StrId, usize> = it.d.modules[it.mods[top_mod].ir]
            .inputs
            .iter()
            .filter(|p| p.kind == ir::PortKind::Reset)
            .map(|p| (p.name, 0))
            .collect();
        // top-level bindings (arguments/parameters, auto-fire method
        // args, EN_<m>=1) enter as the top instance's params — the
        // same mechanism every child instance uses, so Port/Param
        // reads, the interp fallthroughs, and the compiled
        // port_consts/wide_consts folds all work unchanged
        let top_params: HashMap<StrId, Value> =
            rb.params.into_iter().collect();
        it.instantiate(
            "".to_string(),
            top_mod,
            top_params,
            HashMap::new(),
            top_binds,
            HashMap::new(),
            HashMap::new(),
        );
        Ok(it)
    }

    fn s(&self, id: StrId) -> &str {
        let n = self.d.strings.len();
        if (id as usize) < n {
            &self.d.strings[id as usize]
        } else {
            &self.dyn_strs[id as usize - n]
        }
    }

    /// Intern a runtime-created string (StringConcat) into the arena.
    pub(crate) fn intern_dyn(&mut self, text: String) -> StrId {
        // window-time interning shifts every later dyn string id and
        // leaves baked slots holding ids a RunCore boot cannot resolve
        // (adversarial-panel finding) — any bake-window intern makes
        // the window unskippable
        if prim::quiet_engine() {
            prim::note_window_effect();
        }
        self.dyn_strs.push(text);
        (self.d.strings.len() + self.dyn_strs.len() - 1) as StrId
    }

    /// The string id bound to a string-valued parameter of an instance,
    /// if any (used to forward string parameters down the hierarchy).
    fn str_param_of(&self, inst: usize, name: StrId) -> Option<StrId> {
        if let InstKind::User { str_params, .. } = &self.insts[inst].kind {
            str_params.get(&name).copied()
        } else {
            None
        }
    }

    fn instantiate(
        &mut self,
        path: String,
        module: usize,
        params: HashMap<StrId, Value>,
        str_params: HashMap<StrId, StrId>,
        reset_binds: HashMap<StrId, usize>,
        gate_binds: HashMap<StrId, (usize, Expr)>,
        clk_binds: HashMap<StrId, (usize, Expr)>,
    ) -> usize {
        let slot = self.insts.len();
        let mir = self.mods[module].ir;

        // resets generated inside this module (SyncReset/MakeReset/... 
        // children) appear in the module's reset list as fresh wire names;
        // give each its own node
        let mut resets = reset_binds;
        let wires: Vec<Expr> = self.d.modules[mir].resets.iter().map(|r| r.wire.clone()).collect();
        for w in wires {
            if let Expr::Port(name) = w {
                resets.entry(name).or_insert_with(|| {
                    self.rst_asserted.push(false);
                    self.rst_subs.push(Vec::new());
                    self.rst_asserted.len() - 1
                });
            }
        }
        let reset_map = resets.clone();

        self.insts.push(Inst {
            path: path.clone(),
            kind: InstKind::User {
                module,
                latched: HashMap::new(),
                children: HashMap::new(),
                params,
                str_params,
                resets,
                gates: gate_binds,
                clk_binds,
            },
        });
        self.inst_by_path.insert(path.clone(), slot);

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
                    let mut child_binds = HashMap::new();
                    let mut gate_binds = HashMap::new();
                    let mut clk_binds = HashMap::new();
                    // inputs align positionally with the instantiation
                    // args, except that a gated input clock occupies TWO
                    // input ports (Clock then ClockGate) bound from one
                    // Clock{osc,gate} arg
                    let mut pi = 0usize;
                    for arg in args.iter() {
                        if pi >= inputs.len() {
                            break;
                        }
                        let (pname_, kind_) = inputs[pi];
                        pi += 1;
                        match kind_ {
                            ir::PortKind::Clock => {
                                if let Expr::Clock { osc, .. } = arg {
                                    clk_binds.insert(
                                        pname_,
                                        (slot, osc.as_ref().clone()),
                                    );
                                }
                                if pi < inputs.len()
                                    && inputs[pi].1 == ir::PortKind::ClockGate
                                {
                                    let gname = inputs[pi].0;
                                    pi += 1;
                                    if let Expr::Clock { gate, .. } = arg {
                                        gate_binds.insert(
                                            gname,
                                            (slot, gate.as_ref().clone()),
                                        );
                                    }
                                }
                            }
                            ir::PortKind::ClockGate => {}
                            ir::PortKind::Reset => {
                                if let Expr::Reset { wire } = arg {
                                    if let Expr::Port(p) = wire.as_ref() {
                                        if let Some(&n) = reset_map.get(p) {
                                            child_binds.insert(pname_, n);
                                        }
                                    }
                                }
                            }
                            _ => match arg {
                                Expr::Str(sid) => {
                                    str_params.insert(pname_, *sid);
                                }
                                // a string parameter forwarded through an
                                // intermediate module level
                                Expr::Param(p) | Expr::Port(p)
                                    if self.str_param_of(slot, *p).is_some() =>
                                {
                                    let sid = self.str_param_of(slot, *p).unwrap();
                                    str_params.insert(pname_, sid);
                                }
                                _ => {
                                    let mut c = Ctx::default();
                                    let v = self.eval(slot, &mut c, arg);
                                    params.insert(pname_, v);
                                }
                            },
                        }
                    }
                    self.instantiate(
                        cpath.clone(),
                        cmod,
                        params,
                        str_params,
                        child_binds,
                        gate_binds,
                        clk_binds,
                    )
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
                            Expr::Param(p) | Expr::Port(p)
                                if self.str_param_of(slot, *p).is_some() =>
                            {
                                let sid = self.str_param_of(slot, *p).unwrap();
                                strs.push(self.s(sid).to_string());
                            }
                            _ => {
                                let mut c = Ctx::default();
                                let v = self.eval(slot, &mut c, a);
                                // computed strings (e.g. StringConcat of
                                // parameters) are string args, not consts
                                match v.as_str_id() {
                                    Some(id) => strs.push(self.s(id).to_string()),
                                    None => consts.push(v),
                                }
                            }
                        }
                    }
                    if pname == "ClockGen" {
                        // args: v1Width, v2Width, initDelay, initValue,
                        // otherValue; high phase = initValue ? v2 : v1
                        let v1 = consts[0].as_u64();
                        let v2 = consts[1].as_u64();
                        let delay = consts[2].as_u64();
                        let init_high = consts[3].as_u64() != 0;
                        let (hi, lo) = if init_high { (v2, v1) } else { (v1, v2) };
                        self.clockgen_waves.insert(
                            format!("{cpath}$CLK_OUT"),
                            Wave { init_high, delay, hi, lo, has_init: true },
                        );
                    }
                    // dynamic clocks: record the initial output level; the
                    // driving prim instance is found again by path at run()
                    // (bk_enqueue_initial_clock_edge semantics)
                    match pname.as_str() {
                        "MakeClock" => {
                            let init_high = consts[0].as_u64() != 0;
                            self.dynclk_init
                                .insert(format!("{cpath}$CLK_OUT"), init_high);
                        }
                        "ClockDiv" | "GatedClockDiv" => {
                            let width = consts[0].as_u64();
                            let upper = consts[2].as_u64();
                            let offset = consts[3].as_u64();
                            let init_high = (upper - offset) >= (1 << (width - 1));
                            self.dynclk_init
                                .insert(format!("{cpath}$CLK_OUT"), init_high);
                        }
                        "ClockInverter" | "GatedClockInverter" | "ClockMux"
                        | "UngatedClockMux" | "ClockSelect" | "UngatedClockSelect" => {
                            self.dynclk_init
                                .insert(format!("{cpath}$CLK_OUT"), false);
                        }
                        _ => {}
                    }
                    let idx = self.insts.len();
                    self.insts.push(Inst {
                        path: cpath.clone(),
                        kind: InstKind::Prim(prim::make_prim(&pname, &consts, &strs, &cpath)),
                    });
                    // reset-line subscriptions from Reset args wired to a
                    // live reset (a constant wire = noReset, never
                    // asserted); the ordinal distinguishes A_RST/B_RST on
                    // multi-input prims
                    let mut rst_ord = 0;
                    for a in &args {
                        if let Expr::Reset { wire } = a {
                            if let Expr::Port(p) = wire.as_ref() {
                                if let Some(&n) = reset_map.get(p) {
                                    self.rst_subs[n].push((idx, rst_ord));
                                }
                            }
                            rst_ord += 1;
                        }
                    }
                    // reset generators drive the node named <leaf>$OUT_RST
                    // in this module's reset list
                    if matches!(
                        pname.as_str(),
                        "SyncReset" | "SyncResetA" | "SyncReset0" | "InitialReset"
                            | "MakeReset" | "MakeResetA" | "MakeReset0"
                            | "ResetMux" | "ResetEither"
                            | "ClockSelect" | "UngatedClockSelect"
                    ) {
                        let t1 = format!("{}$OUT_RST", self.s(name));
                        let t2 = format!("{}$RST_OUT", self.s(name));
                        let out = reset_map
                            .iter()
                            .find(|(k, _)| self.s(**k) == t1 || self.s(**k) == t2)
                            .map(|(_, &n)| n);
                        if let Some(out) = out {
                            self.rstgen_out.insert(idx, out);
                            // InitialReset asserts its output from time 0
                            // (reset_init in set_reset_fn_gen_rst); the
                            // assert is broadcast at run() start once every
                            // subscriber exists
                            if pname == "InitialReset" {
                                self.initial_asserts.push(out);
                            }
                        }
                    }
                    self.inst_by_path.insert(cpath.clone(), idx);
                    idx
                }
            };
            if let InstKind::User { children, .. } = &mut self.insts[slot].kind {
                children.insert(name, cidx);
            }
        }

        // interface output resets: a reset-list wire "child$port" names a
        // node created inside the (module) child; merge our placeholder
        // node onto the child's real node so assertions reach every
        // subscriber and reset-wire reads see the right state
        let qual_wires: Vec<StrId> = self.d.modules[mir]
            .resets
            .iter()
            .filter_map(|r| match &r.wire {
                Expr::Port(n) if self.s(*n).contains('$') => Some(*n),
                _ => None,
            })
            .collect();
        for w in qual_wires {
            let name = self.s(w).to_string();
            let Some(k) = name.rfind('$') else { continue };
            let (cname, pname_) = (&name[..k], &name[k + 1..]);
            let cpath = if path.is_empty() {
                cname.to_string()
            } else {
                format!("{path}.{cname}")
            };
            let Some(&ci) = self.inst_by_path.get(&cpath) else { continue };
            let InstKind::User { module: cm, resets: crs, .. } = &self.insts[ci].kind
            else {
                continue; // prim reset generators are handled via rstgen_out
            };
            let cmir = self.mods[*cm].ir;
            let Some(&(_, wire)) = self.d.modules[cmir]
                .ifc_resets
                .iter()
                .find(|(p, _)| self.s(*p) == pname_)
            else {
                continue;
            };
            let Some(&new_node) = crs.get(&wire) else { continue };
            let old_node = match &self.insts[slot].kind {
                InstKind::User { resets, .. } => resets.get(&w).copied(),
                _ => None,
            };
            let Some(old_node) = old_node else { continue };
            if old_node == new_node {
                continue;
            }
            // migrate subscribers and rewrite every map that names the
            // placeholder (instantiation-time only)
            let moved = std::mem::take(&mut self.rst_subs[old_node]);
            self.rst_subs[new_node].extend(moved);
            for inst in &mut self.insts {
                if let InstKind::User { resets, .. } = &mut inst.kind {
                    for v in resets.values_mut() {
                        if *v == old_node {
                            *v = new_node;
                        }
                    }
                }
            }
            for v in self.rstgen_out.values_mut() {
                if *v == old_node {
                    *v = new_node;
                }
            }
            for v in &mut self.initial_asserts {
                if *v == old_node {
                    *v = new_node;
                }
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

    /// Write a value into arena recording slots (traced artifacts).
    fn rec_write(&self, base: u32, w: u32, v: &Value) {
        let words = (w.max(1) as usize).div_ceil(64);
        let limbs = v.limbs64();
        unsafe {
            for k in 0..words {
                *self.jit_arena_ptr.add(base as usize + k) =
                    limbs.get(k).copied().unwrap_or(0);
            }
        }
    }

    /// Read a value back from arena recording slots.
    fn rec_read(&self, base: u32, w: u32) -> Value {
        let words = (w.max(1) as usize).div_ceil(64);
        let limbs = unsafe {
            std::slice::from_raw_parts(self.jit_arena_ptr.add(base as usize), words)
        }
        .to_vec();
        Value::from_limbs64(w.max(1), limbs)
    }

    /// Record a def's evaluated value for the VCD writer: the arena
    /// recording slot when the traced artifact declares one (the slot is
    /// then the single authority — compiled bodies store it inline and
    /// interp-executed code lands here), else the vcd_def_vals map.
    fn vcd_rec_def(&mut self, inst: usize, name: StrId, v: &Value) {
        if !self.jit_arena_ptr.is_null() {
            if let Some(&(base, w)) = self.jit_rec_defs.get(&(inst, name)) {
                let vv = v.clone().zext(w.max(1));
                self.rec_write(base, w, &vv);
                return;
            }
        }
        self.vcd_def_vals.insert((inst, name), v.clone());
    }

    /// Record a method-fired timestamp (EN port), preserving prior args.
    fn vcd_rec_meth_time(&mut self, callee: usize, method: StrId) {
        let now = self.now;
        if !self.jit_arena_ptr.is_null() {
            if let Some(rs) = self.jit_rec_meths.get(&(callee, method)) {
                unsafe { *self.jit_arena_ptr.add(rs.t as usize) = now };
                return;
            }
        }
        self.vcd_meth_calls
            .entry((callee, method))
            .and_modify(|e| e.0 = now)
            .or_insert((now, Vec::new()));
    }

    /// Record a method call: timestamp + argument port values.
    fn vcd_rec_meth_call(&mut self, callee: usize, method: StrId, argv: &[Value]) {
        let now = self.now;
        if !self.jit_arena_ptr.is_null() {
            if let Some(rs) = self.jit_rec_meths.get(&(callee, method)).cloned() {
                unsafe { *self.jit_arena_ptr.add(rs.t as usize) = now };
                for (a, &(base, w)) in argv.iter().zip(&rs.args) {
                    let vv = a.clone().zext(w.max(1));
                    self.rec_write(base, w, &vv);
                }
                return;
            }
        }
        self.vcd_meth_calls.insert((callee, method), (now, argv.to_vec()));
    }

    /// Record a value/AV method's returned value (result port).
    fn vcd_rec_meth_result(&mut self, callee: usize, method: StrId, v: &Value) {
        if !self.jit_arena_ptr.is_null() {
            if let Some(rs) = self.jit_rec_meths.get(&(callee, method)) {
                if let Some((base, w)) = rs.res {
                    let vv = v.clone().zext(w.max(1));
                    self.rec_write(base, w, &vv);
                }
                return;
            }
        }
        self.vcd_meth_results.insert((callee, method), v.clone());
    }

    /// A latched def, or its arena-slot value when the def is kept
    /// current by compiled scheds (fire signals / schedule-position
    /// defs).  Inhibitor lookups must see compiled rules' CFs.
    fn latched_or_arena(&self, inst: usize, name: StrId) -> Option<Value> {
        if let Some(v) = self.latched(inst, name) {
            return Some(v);
        }
        if !self.jit_arena_ptr.is_null() {
            if let Some(&(base, w)) = self.jit_eager_slots.get(&(inst, name)) {
                let words = ((w.max(1) as usize) + 63) / 64;
                let limbs = unsafe {
                    std::slice::from_raw_parts(
                        self.jit_arena_ptr.add(base as usize),
                        words,
                    )
                }
                .to_vec();
                return Some(Value::from_limbs64(w.max(1), limbs));
            }
        }
        None
    }

    /// Evaluate an expression in an instance context.  Body-local defs and
    /// per-cycle latched defs win over recomputation; in memo contexts,
    /// on-demand def values are cached.
    fn eval(&mut self, inst: usize, ctx: &mut Ctx, e: &Expr) -> Value {
        match e {
            Expr::Const { width, limbs } => Value::from_limbs32(*width, limbs),
            Expr::Str(sid) => Value::str_ref(*sid),
            Expr::Def(name) => {
                if let Some(v) = ctx.locals.get(name) {
                    return v.clone();
                }
                if let Some(v) = self.latched(inst, *name) {
                    return v;
                }
                // JIT mode: fire signals and schedule-position defs live
                // in arena slots kept current by the native scheds
                if !self.jit_arena_ptr.is_null() {
                    if let Some(&(base, w)) = self.jit_eager_slots.get(&(inst, *name)) {
                        let words = ((w.max(1) as usize) + 63) / 64;
                        let limbs = unsafe {
                            std::slice::from_raw_parts(
                                self.jit_arena_ptr.add(base as usize),
                                words,
                            )
                        }
                        .to_vec();
                        return Value::from_limbs64(w.max(1), limbs);
                    }
                }
                let module = self.module_of(inst);
                let mir = self.mods[module].ir;
                let di = *self.mods[module]
                    .defs
                    .get(name)
                    .unwrap_or_else(|| panic!("unknown def {:?}", self.s(*name)));
                let d = self.d.modules[mir].defs[di].clone();
                let v = self.eval(inst, ctx, &d.expr);
                if self.vcd_trace {
                    self.vcd_rec_def(inst, *name, &v);
                }
                if ctx.memo {
                    ctx.locals.insert(*name, v.clone());
                }
                v
            }
            Expr::Port(name) | Expr::Param(name) => {
                if let Some(v) = ctx.frame.get(name) {
                    return v.clone();
                }
                if let InstKind::User { params, str_params, .. } = &self.insts[inst].kind {
                    if let Some(v) = params.get(name) {
                        return v.clone();
                    }
                    // string parameters flow as marker values so dynamic
                    // muxes over them still resolve at task boundaries
                    if let Some(&sid) = str_params.get(name) {
                        return Value::str_ref(sid);
                    }
                }
                // module input ports outside a method frame: clock gates and
                // reset lines read as asserted-off (1); method enables and
                // args read as not-driven (0) — an uncalled method's EN must
                // be 0 or WILL_FIRE inhibitors derived from it (rule vs.
                // conflicting method, e.g. sysSchedFixTb's rm vs. step) would
                // suppress the rule every cycle
                if let InstKind::User { resets, .. } = &self.insts[inst].kind {
                    if let Some(&n) = resets.get(name) {
                        // active-low reset wire
                        return Value::from_u64(1, (!self.rst_asserted[n]) as u64);
                    }
                }
                // input clock-gate ports bound at instantiation resolve in
                // the parent instance (top-level input gates fall through
                // and read as constant 1, mirroring top_gates -> aTrue)
                if let InstKind::User { gates, .. } = &self.insts[inst].kind {
                    if let Some((owner, g)) = gates.get(name) {
                        let (owner, g) = (*owner, g.clone());
                        let mut c = Ctx::default();
                        return self.eval(owner, &mut c, &g);
                    }
                }
                // flattened gate wires of derived clocks: the exporter
                // writes "<absolute.path>$CLK_GATE_OUT" for qualified
                // gate ports (tick gates); module-local Gate reads come
                // through Expr::Gate instead
                if let Some(path) = self.s(*name).strip_suffix("$CLK_GATE_OUT") {
                    let candidates = [path.to_string(), {
                        let base = &self.insts[inst].path;
                        if base.is_empty() {
                            path.to_string()
                        } else {
                            format!("{base}.{path}")
                        }
                    }];
                    for p in &candidates {
                        if let Some(&ci) = self.inst_by_path.get(p) {
                            if let InstKind::Prim(pr) = &self.insts[ci].kind {
                                return Value::from_u64(1, pr.gate_out() as u64);
                            }
                        }
                    }
                }
                let module = self.module_of(inst);
                match self.mods[module].ports.get(name) {
                    // EN_* is latched 1 for the rest of the pass when the
                    // method executes (urgency inhibitors read it); an
                    // uncalled method's EN reads 0
                    Some(&(w, ir::PortKind::MethodEnable)) => {
                        if let Some(v) = self.latched(inst, *name) {
                            return v;
                        }
                        // compiled call sites store EN only in the
                        // arena; interpreted cones (PG_FINAL early
                        // rules, cold bodies) must read it there
                        if !self.jit_arena_ptr.is_null() {
                            if let Some(&slot) =
                                self.jit_en_slots.get(&(inst, *name))
                            {
                                let word = unsafe {
                                    *self.jit_arena_ptr.add(slot as usize)
                                };
                                return Value::from_u64(w, word);
                            }
                        }
                        Value::from_u64(w, 0)
                    }
                    Some(&(w, ir::PortKind::MethodArg)) => Value::from_u64(w, 0),
                    Some(&(w, _)) => Value::from_u64(w, 1),
                    None => Value::from_u64(1, 1),
                }
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
                let loc = self.loc_of(inst);
                self.foreign_value(&fname, &argv, *width, &loc)
            }
            Expr::Gate { instance, clock } => {
                let child = self.child_of(inst, *instance);
                match &self.insts[child].kind {
                    InstKind::Prim(p) => Value::from_u64(1, p.gate_out() as u64),
                    InstKind::User { module, .. } => {
                        // a user module's exported gate: evaluate the
                        // child's recorded gate expr in the child's
                        // context (it may chase further Gates or a
                        // prim's $CLK_GATE_OUT port)
                        let mir = self.mods[*module].ir;
                        let g = self.d.modules[mir]
                            .ifc_clock_gates
                            .iter()
                            .find(|(n, _)| n == clock)
                            .map(|(_, e)| e.clone());
                        match g {
                            Some(e) => {
                                let mut c = Ctx::default();
                                self.eval(child, &mut c, &e)
                            }
                            // no gate recorded = ungated
                            None => Value::from_u64(1, 1),
                        }
                    }
                }
            }
            Expr::Clock { .. } => Value::from_u64(1, 1),
            Expr::Reset { wire } => self.eval(inst, ctx, wire),
            Expr::Real(r) => Value::real(*r),
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
            // And/Or/Xor/Add/Mul are associative and may appear n-ary in ASyntax.
            PrimOp::And => {
                let mut acc = self.eval(inst, ctx, &args[0]);
                for a in &args[1..] {
                    let b = self.eval(inst, ctx, a);
                    acc = acc.and(&b, w);
                }
                acc
            }
            PrimOp::Or => {
                let mut acc = self.eval(inst, ctx, &args[0]);
                for a in &args[1..] {
                    let b = self.eval(inst, ctx, a);
                    acc = acc.or(&b, w);
                }
                acc
            }
            PrimOp::Xor => {
                let mut acc = self.eval(inst, ctx, &args[0]);
                for a in &args[1..] {
                    let b = self.eval(inst, ctx, a);
                    acc = acc.xor(&b, w);
                }
                acc
            }
            PrimOp::Not => self.eval(inst, ctx, &args[0]).not(w),
            PrimOp::Add => {
                let mut acc = self.eval(inst, ctx, &args[0]);
                for a in &args[1..] {
                    let b = self.eval(inst, ctx, a);
                    acc = acc.add(&b, w);
                }
                acc
            }
            PrimOp::Sub => {
                let a = self.eval(inst, ctx, &args[0]);
                let b = self.eval(inst, ctx, &args[1]);
                a.sub(&b, w)
            }
            PrimOp::Neg => self.eval(inst, ctx, &args[0]).neg(w),
            PrimOp::Mul => {
                let mut acc = self.eval(inst, ctx, &args[0]);
                for a in &args[1..] {
                    let b = self.eval(inst, ctx, a);
                    acc = acc.mul(&b, w);
                }
                acc
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
            PrimOp::StringConcat => {
                let mut text = String::new();
                for a in args {
                    let v = self.eval(inst, ctx, a);
                    match v.as_str_id() {
                        Some(id) => text.push_str(self.s(id)),
                        None => panic!("StringConcat of a non-string value"),
                    }
                }
                let id = self.intern_dyn(text);
                Value::str_ref(id)
            }
        }
    }

    fn eval_arg(&mut self, inst: usize, ctx: &mut Ctx, e: &Expr, signed: bool) -> Arg {
        match e {
            Expr::Str(s) => Arg::Str(self.s(*s).into()),
            Expr::Port(name) | Expr::Param(name) => {
                if let InstKind::User { str_params, .. } = &self.insts[inst].kind {
                    if let Some(&sid) = str_params.get(name) {
                        return Arg::Str(self.s(sid).into());
                    }
                }
                let v = self.eval(inst, ctx, e);
                self.val_arg(v, signed)
            }
            _ => {
                let v = self.eval(inst, ctx, e);
                self.val_arg(v, signed)
            }
        }
    }

    /// Dynamically selected strings and reals surface as marker values.
    fn val_arg(&self, v: Value, signed: bool) -> Arg {
        if let Some(r) = v.as_real() {
            return Arg::Real(r);
        }
        match v.as_str_id() {
            Some(id) => Arg::Str(self.s(id).into()),
            None => Arg::Val(v, signed),
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

    /// Arg::Str text for a string id, Arc-interned on first use (the
    /// per-event-tax audit: an Arc clone per call replaces a String
    /// alloc per call on the $display path).
    pub(crate) fn arg_str(&mut self, id: u32) -> std::sync::Arc<str> {
        if let Some(a) = self.arg_strs.get(&id) {
            return a.clone();
        }
        let a: std::sync::Arc<str> = std::sync::Arc::from(self.s(id));
        self.arg_strs.insert(id, a.clone());
        a
    }

    fn call_value(&mut self, callee: usize, method: StrId, argv: &[Value], w: u32) -> Value {
        match &mut self.insts[callee].kind {
            InstKind::Prim(p) => {
                // borrow, don't clone: this is the per-event prim
                // value path, and a String alloc per call showed in
                // the FloatTest malloc profile (disjoint fields, so
                // the &mut prim borrow tolerates it)
                let mname: &str = &self.d.strings[method as usize];
                let r = p.value_method(mname, argv, self.now);
                if self.trace_events {
                    let path = self.insts[callee].path.clone();
                    eprintln!("[{}] {}.{} -> {}", self.cycle, path, mname,
                              r.to_hex_string());
                }
                return r;
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
                    Some(r) => {
                        let v = self.eval(callee, &mut ctx, &r).zext(w);
                        if self.vcd_trace {
                            self.vcd_rec_meth_result(callee, method, &v);
                        }
                        v
                    }
                    None => panic!("value call to method without result"),
                }
            }
        }
    }

    fn call_action(&mut self, callee: usize, method: StrId, argv: &[Value]) {
        if self.trace_events {
            if let InstKind::Prim(_) = &self.insts[callee].kind {
                let mname = self.d.strings[method as usize].clone();
                let args: Vec<String> = argv.iter().map(|v| v.to_hex_string()).collect();
                eprintln!("[{}] {}.{}({})", self.cycle, self.insts[callee].path,
                          mname, args.join(","));
            }
        }
        match &mut self.insts[callee].kind {
            InstKind::Prim(p) => {
                // borrow, don't clone (same disjoint-fields fix as
                // call_value; per-event-tax audit finding 3)
                let mname: &str = &self.d.strings[method as usize];
                p.action_method(mname, argv, self.now);
            }
            InstKind::User { module, .. } => {
                let module = *module;
                let mi = *self.mods[module]
                    .methods
                    .get(&method)
                    .unwrap_or_else(|| panic!("unknown method {:?}", self.s(method)));
                self.latch_method_en(callee, method);
                if self.vcd_trace {
                    self.vcd_rec_meth_call(callee, method, argv);
                }
                let mir = self.mods[module].ir;
                let body: Vec<Stmt> = self.d.modules[mir].methods[mi].body.clone();
                // an always_enabled method is invoked with its RDY
                // dropped from the caller's condition; the body itself
                // checks RDY at runtime (cvtIFace check_rdy) — EN and
                // args land above regardless
                if self.d.modules[mir].methods[mi].always_enabled
                    && !self.always_en_rdy(callee, module, method)
                {
                    return;
                }
                let mut ctx = self.method_ctx(module, mi, argv, false);
                for st in &body {
                    self.exec_stmt(callee, &mut ctx, st);
                }
            }
        }
    }

    /// check_rdy for an always_enabled method: the C++ wraps the body in
    /// `if (RDY_<m> port)` (cvtIFace), so evaluate the sibling RDY_<m>
    /// method — the always_enabled method's own `ready` expr can
    /// reference defs bsc dropped along with the caller-side condition.
    /// No RDY method exported = constant ready.
    fn always_en_rdy(&mut self, callee: usize, module: usize, method: StrId) -> bool {
        let rdy_name = format!("RDY_{}", self.s(method));
        let Some(id) = self.d.strings.iter().position(|x| x == &rdy_name) else {
            return true;
        };
        if !self.mods[module].methods.contains_key(&(id as StrId)) {
            return true;
        }
        self.call_value(callee, id as StrId, &[], 1).as_u64() & 1 == 1
    }

    /// Record that an action/actionvalue method fired this pass: the C++
    /// schedule zeroes every EN_* at the top of the pass and sets it when
    /// the method executes, and WILL_FIRE urgency inhibitors of
    /// conflicting rules read it later in the same pass.
    fn latch_method_en(&mut self, callee: usize, method: StrId) {
        if self.vcd_trace {
            self.vcd_rec_meth_time(callee, method);
        }
        let en = format!("EN_{}", self.s(method));
        if let Some(en_id) = self.d.strings.iter().position(|x| x == &en) {
            if let InstKind::User { latched, .. } = &mut self.insts[callee].kind {
                latched.insert(en_id as StrId, Value::from_u64(1, 1));
            }
            // JIT body fallback: native scheds read EN from the arena
            if !self.jit_arena_ptr.is_null() {
                if let Some(&slot) = self.jit_en_slots.get(&(callee, en_id as StrId)) {
                    unsafe { *self.jit_arena_ptr.add(slot as usize) = 1 };
                }
            }
        }
    }

    fn call_actionvalue(&mut self, callee: usize, method: StrId, argv: &[Value]) -> Value {
        match &mut self.insts[callee].kind {
            InstKind::Prim(p) => {
                let mname: &str = &self.d.strings[method as usize];
                p.actionvalue_method(mname, argv, self.now)
            }
            InstKind::User { module, .. } => {
                let module = *module;
                let mi = *self.mods[module]
                    .methods
                    .get(&method)
                    .unwrap_or_else(|| panic!("unknown method {:?}", self.s(method)));
                self.latch_method_en(callee, method);
                if self.vcd_trace {
                    self.vcd_rec_meth_call(callee, method, argv);
                }
                let mir = self.mods[module].ir;
                let body: Vec<Stmt> = self.d.modules[mir].methods[mi].body.clone();
                let result = self.d.modules[mir].methods[mi].result.clone();
                // check_rdy for always_enabled: skip the body when RDY
                // is off; the result is still evaluated (the C++ returns
                // the stale port value — "all bets are off")
                let skip_body = self.d.modules[mir].methods[mi].always_enabled
                    && !self.always_en_rdy(callee, module, method);
                let mut ctx = self.method_ctx(module, mi, argv, false);
                if !skip_body {
                    for st in &body {
                        self.exec_stmt(callee, &mut ctx, st);
                    }
                }
                match result {
                    Some(r) => {
                        if self.trace_events {
                            eprintln!("    av-result expr: {r:?}");
                        }
                        self.eval(callee, &mut ctx, &r)
                    }
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
        // NO finished check: $finish marks the stop but the reference
        // runs the in-flight edge (and the finishing rule's remaining
        // statements) TO COMPLETION — only console display tasks are
        // suppressed afterwards (dollar_display.cxx).  The transient
        // hunt's witness: gcd RL_flip after RL_exit's $finish.
        match st {
            Stmt::Def { name, expr } => {
                let v = self.eval(inst, ctx, expr);
                if self.trace_events {
                    eprintln!("    def {} := {}", self.s(*name), v.to_hex_string());
                }
                if self.vcd_trace {
                    self.vcd_rec_def(inst, *name, &v);
                }
                ctx.locals.insert(*name, v);
            }
            Stmt::Action(a) => self.exec_action(inst, ctx, a),
            Stmt::AvAction { def, action } => match action {
                Action::MethCall { instance, method, cond, args, .. } => {
                    if !self.eval(inst, ctx, cond).as_bool() {
                        let dw = self.def_width(inst, *def).unwrap_or(1);
                        ctx.locals.insert(*def, Value::undet(dw));
                        return;
                    }
                    let argv: Vec<Value> =
                        args.iter().map(|x| self.eval(inst, ctx, x)).collect();
                    let child = self.child_of(inst, *instance);
                    let v = self.call_actionvalue(child, *method, &argv);
                    if self.vcd_trace {
                        // Result-port peeks read the LAST-RETURNED
                        // value (review fleet: AV results were never
                        // recorded)
                        self.vcd_rec_meth_result(child, *method, &v);
                    }
                    // synthetic AV temps are not in the def table; the
                    // callee's result already has the declared width
                    let v = match self.def_width(inst, *def) {
                        Some(dw) => v.zext(dw),
                        None => v,
                    };
                    if self.vcd_trace {
                        self.vcd_rec_def(inst, *def, &v);
                    }
                    ctx.locals.insert(*def, v);
                }
                a @ Action::Task { temp, width, .. } => {
                    self.exec_action(inst, ctx, a);
                    let v = temp
                        .and_then(|t| ctx.locals.get(&t).cloned())
                        .unwrap_or_else(|| Value::undet((*width).max(1)));
                    if self.vcd_trace {
                        self.vcd_rec_def(inst, *def, &v);
                    }
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

    fn def_width(&self, inst: usize, name: StrId) -> Option<u32> {
        let module = self.module_of(inst);
        let mir = self.mods[module].ir;
        self.mods[module]
            .defs
            .get(&name)
            .map(|di| self.d.modules[mir].defs[*di].width)
    }

    fn exec_action(&mut self, inst: usize, ctx: &mut Ctx, a: &Action) {
        // no finished check — see exec_stmt
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
                let loc = self.loc_of(inst);
                self.foreign_action(&fname, &argv, &loc);
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
                let loc = self.loc_of(inst);
                let v = self.foreign_value(&fname, &argv, *width, &loc);
                ctx.locals.insert(cookie_key(*cookie), v.clone());
                if let Some(t) = temp {
                    ctx.locals.insert(*t, v);
                }
            }
        }
    }

    // ===============
    // System tasks

    /// Write to every channel a file key names (VLFiles::findFiles): keys
    /// with bit 31 index the fd table; smaller keys are MCD bitmasks
    /// fanning out to each set bit (bit 0 = stdout).
    /// dlopen the companion BDPI shared object and resolve the design's
    /// imported functions.  Library-provided imports (is_lib_bdpi) are
    /// not expected in the user .so and are excluded from the eager
    /// resolution.
    pub fn load_bdpi(&mut self, path: &str) -> Result<(), String> {
        let funcs: Vec<(String, String)> = self
            .d
            .foreign_funcs
            .iter()
            .filter(|f| !is_lib_bdpi(self.s(f.c_name)))
            .map(|f| (self.s(f.name).to_string(), self.s(f.c_name).to_string()))
            .collect();
        self.bdpi = Some(bdpi::Bdpi::load(std::path::Path::new(path), &funcs)?);
        Ok(())
    }

    /// True iff the design imports user BDPI functions (library-
    /// provided imports like rand32 excluded) — i.e. a companion
    /// .bdpi.so is REQUIRED, and running without it panics at the
    /// first call (across extern "C" in the capi = process abort).
    pub fn needs_user_bdpi(&self) -> bool {
        self.d
            .foreign_funcs
            .iter()
            .any(|f| !is_lib_bdpi(self.s(f.c_name)))
    }

    /// Dispatch a non-builtin task name as a BDPI import, if the design
    /// declares one.
    fn bdpi_call(&mut self, name: &str, args: &[Arg], w: u32) -> Option<Value> {
        let ff = self
            .d
            .foreign_funcs
            .iter()
            .find(|f| self.s(f.name) == name)?
            .clone();
        // library BDPI: reference Bluesim links libbsprim.a (with
        // rand32.cxx) into every executable, so these imports resolve
        // without any user C files; provide them natively with the same
        // glibc calls for bit-identical streams
        match self.s(ff.c_name) {
            "rand32" => {
                // rand32.cxx: return (unsigned int)random(); ours is
                // the same glibc stream, per-engine (see GlibcRandom)
                // — a window-time draw desyncs the stream a skipped
                // window leaves untouched
                if prim::quiet_engine() {
                    prim::note_window_effect();
                }
                let v = self.rng.next() as u64;
                return Some(Value::from_u64(w.max(1), v));
            }
            "srand" => {
                // glibc srand is an alias of srandom, seeding random()
                if prim::quiet_engine() {
                    prim::note_window_effect();
                }
                let seed = match args.first() {
                    Some(Arg::Val(v, _)) => v.as_u64() as u32,
                    _ => 0,
                };
                self.rng.srandom(seed);
                return Some(Value::from_u64(w.max(1), 0));
            }
            _ => {}
        }
        let b = self.bdpi.as_ref().unwrap_or_else(|| {
            panic!(
                "BDPI function {name:?} called but no .bdpi.so was found \
                 next to the .bir (link with the user's C files)"
            )
        });
        Some(b.call(&ff, name, args, w))
    }

    /// %m location string: the hierarchical name of the module executing
    /// the task (the C++ passes `this` and write_name prints it).
    fn loc_of(&self, inst: usize) -> String {
        let p = &self.insts[inst].path;
        if p.is_empty() {
            "top".to_string()
        } else {
            format!("top.{p}")
        }
    }

    fn foreign_action(&mut self, name: &str, args: &[Arg], loc: &str) {
        // console/file/finish core first (quiet and post-$finish
        // suppression live there too); what it declines is design-
        // coupled — the $dump* family (VCD writer) and BDPI imports
        if self.fe.action(name, args, self.now, loc) {
            return;
        }
        match name {
            // waves: dollar_dumpvars.cxx semantics
            "$dumpfile" => {
                let name = match args.first() {
                    Some(Arg::Str(s)) => s.to_string(),
                    Some(Arg::Val(v, _)) => format::unpack_str_pub(v),
                    _ => "dump.vcd".to_string(),
                };
                let _ = self.vcd.set_file(&name);
            }
            "$dumpvars" => {
                if let Some(Arg::Val(v, _)) = args.first() {
                    self.vcd.set_depth(v.as_u64() as u32);
                }
                self.vcd.set_state(true);
            }
            "$dumpon" => self.vcd.set_state(true),
            "$dumpoff" => {
                if self.vcd.enabled {
                    self.vcd.set_state(false);
                    self.vcd.dump_xs();
                }
            }
            "$dumpall" => self.vcd.request_checkpoint(),
            "$dumplimit" => {
                if let Some(Arg::Val(v, _)) = args.first() {
                    self.vcd.set_limit(v.as_u64());
                }
            }
            "$dumpflush" => self.vcd.flush(),
            other => {
                if self.bdpi_call(other, args, 1).is_none() {
                    panic!("trs-interp: unimplemented system task {other:?}");
                }
            }
        }
    }

    fn foreign_value(&mut self, name: &str, args: &[Arg], w: u32, loc: &str) -> Value {
        // console/file core first; None = a BDPI import (or unknown)
        if let Some(v) = self.fe.value(name, args, w, self.now, loc) {
            return v;
        }
        match self.bdpi_call(name, args, w) {
            Some(v) => v.zext(w.max(1)),
            None => {
                panic!("trs-interp: unimplemented value task {name:?} ({args:?})")
            }
        }
    }

    // ===============
    // Cycle execution

    // ===============
    // VCD dumping (docs/VCD-CONTRACT.md).
    //
    // The module-scope variable set replicates the C++ backend's
    // SimCOpt.moveDefsOntoStack: a def or method port survives as a
    // class member (and hence a VCD var) only if it is referenced by two
    // or more generated functions — the per-(domain,edge) schedule
    // function (CAN_FIRE/WILL_FIRE cones), each rule body function, and
    // each method function — or if it is >64 bits wide or an ATaskValue
    // def (never moved).  Unreferenced defs are deleted entirely.

    /// Compute the member/port var lists for one module type.
    fn vcd_mod_vars(&mut self, module: usize) -> std::rc::Rc<ModVars> {
        if let Some(mv) = self.vcd_mod_vars.get(&module) {
            return mv.clone();
        }
        let mir = self.mods[module].ir;
        let m = self.d.modules[mir].clone();
        // def table by name for cone recursion
        let defs_by_name: HashMap<StrId, usize> =
            m.defs.iter().enumerate().map(|(i, d)| (d.name, i)).collect();

        // usage[name] = set of function keys referencing the def/port
        type FnKey = (u8, u32, u32);
        let mut usage: HashMap<StrId, std::collections::HashSet<FnKey>> = HashMap::new();

        fn mark_expr(
            e: &Expr,
            fk: (u8, u32, u32),
            m: &ir::Module,
            defs_by_name: &HashMap<StrId, usize>,
            usage: &mut HashMap<StrId, std::collections::HashSet<(u8, u32, u32)>>,
            seen: &mut std::collections::HashSet<StrId>,
        ) {
            match e {
                Expr::Def(n) => {
                    usage.entry(*n).or_default().insert(fk);
                    if seen.insert(*n) {
                        if let Some(&di) = defs_by_name.get(n) {
                            let inner = m.defs[di].expr.clone();
                            mark_expr(&inner, fk, m, defs_by_name, usage, seen);
                        }
                    }
                }
                Expr::Port(n) | Expr::Param(n) => {
                    usage.entry(*n).or_default().insert(fk);
                }
                Expr::MethCall { args, .. } | Expr::ForeignCall { args, .. } => {
                    for a in args {
                        mark_expr(a, fk, m, defs_by_name, usage, seen);
                    }
                }
                Expr::Prim { args, .. } => {
                    for a in args {
                        mark_expr(a, fk, m, defs_by_name, usage, seen);
                    }
                }
                Expr::Clock { osc, gate } => {
                    mark_expr(osc, fk, m, defs_by_name, usage, seen);
                    mark_expr(gate, fk, m, defs_by_name, usage, seen);
                }
                Expr::Reset { wire } => mark_expr(wire, fk, m, defs_by_name, usage, seen),
                Expr::If { cond, then_, else_, .. } => {
                    mark_expr(cond, fk, m, defs_by_name, usage, seen);
                    mark_expr(then_, fk, m, defs_by_name, usage, seen);
                    mark_expr(else_, fk, m, defs_by_name, usage, seen);
                }
                Expr::Case { scrutinee, arms, default, .. } => {
                    mark_expr(scrutinee, fk, m, defs_by_name, usage, seen);
                    for (_, a) in arms {
                        mark_expr(a, fk, m, defs_by_name, usage, seen);
                    }
                    mark_expr(default, fk, m, defs_by_name, usage, seen);
                }
                Expr::Const { .. }
                | Expr::Str(_)
                | Expr::Real(_)
                | Expr::TaskValue { .. }
                | Expr::MethValue { .. }
                | Expr::Gate { .. } => {}
            }
        }
        fn mark_action(
            a: &Action,
            fk: (u8, u32, u32),
            m: &ir::Module,
            dbn: &HashMap<StrId, usize>,
            usage: &mut HashMap<StrId, std::collections::HashSet<(u8, u32, u32)>>,
            seen: &mut std::collections::HashSet<StrId>,
        ) {
            match a {
                Action::MethCall { cond, args, .. }
                | Action::Foreign { cond, args, .. }
                | Action::Task { cond, args, .. } => {
                    mark_expr(cond, fk, m, dbn, usage, seen);
                    for x in args {
                        mark_expr(x, fk, m, dbn, usage, seen);
                    }
                }
            }
        }
        fn mark_stmts(
            sts: &[Stmt],
            fk: (u8, u32, u32),
            m: &ir::Module,
            dbn: &HashMap<StrId, usize>,
            usage: &mut HashMap<StrId, std::collections::HashSet<(u8, u32, u32)>>,
            seen: &mut std::collections::HashSet<StrId>,
        ) {
            for st in sts {
                match st {
                    Stmt::Def { name, expr } => {
                        usage.entry(*name).or_default().insert(fk);
                        mark_expr(expr, fk, m, dbn, usage, seen);
                    }
                    Stmt::Action(a) => mark_action(a, fk, m, dbn, usage, seen),
                    Stmt::AvAction { def, action } => {
                        usage.entry(*def).or_default().insert(fk);
                        mark_action(action, fk, m, dbn, usage, seen);
                    }
                    Stmt::Cond { cond, then_, else_ } => {
                        mark_expr(cond, fk, m, dbn, usage, seen);
                        mark_stmts(then_, fk, m, dbn, usage, seen);
                        mark_stmts(else_, fk, m, dbn, usage, seen);
                    }
                }
            }
        }

        // schedule functions: the CF/WF cones of every rule in each
        // (domain, edge) schedule
        let rules_by_name: HashMap<StrId, usize> =
            m.rules.iter().enumerate().map(|(i, r)| (r.name, i)).collect();
        for ms in m.schedule.domains.iter() {
            let fk = (0u8, ms.domain, ms.posedge as u32);
            let mut seen = std::collections::HashSet::new();
            for seg in &ms.segments {
                for node in &seg.nodes {
                    let rn = match node {
                        ir::SchedNode::Sched(r) | ir::SchedNode::Exec(r) => *r,
                    };
                    if let Some(&ri) = rules_by_name.get(&rn) {
                        let cf = m.rules[ri].can_fire;
                        let wf = m.rules[ri].will_fire;
                        mark_expr(&Expr::Def(cf), fk, &m, &defs_by_name, &mut usage, &mut seen);
                        mark_expr(&Expr::Def(wf), fk, &m, &defs_by_name, &mut usage, &mut seen);
                    }
                }
            }
        }
        // rule body functions
        for r in &m.rules {
            let fk = (1u8, r.name, 0u32);
            let mut seen = std::collections::HashSet::new();
            mark_stmts(&r.body, fk, &m, &defs_by_name, &mut usage, &mut seen);
        }
        // method functions (RDY_<m> methods are separate entries, so the
        // C++ METH_RDY_<m> function falls out naturally)
        for me in &m.methods {
            let fk = (2u8, me.name, 0u32);
            let mut seen = std::collections::HashSet::new();
            // NOTE: me.ready is NOT marked — the readiness cone lives in
            // the separate RDY_<m> method entry (its own C++ function)
            mark_stmts(&me.body, fk, &m, &defs_by_name, &mut usage, &mut seen);
            if let Some(res) = &me.result {
                mark_expr(res, fk, &m, &defs_by_name, &mut usage, &mut seen);
            }
            // the method function writes its own EN/arg/result ports
            let en = format!("EN_{}", self.s(me.name));
            if let Some(en_id) = self.d.strings.iter().position(|x| x == &en) {
                usage.entry(en_id as StrId).or_default().insert(fk);
            }
            for a in &me.args {
                usage.entry(a.name).or_default().insert(fk);
            }
            if me.result.is_some() {
                usage.entry(me.name).or_default().insert(fk);
            }
            // an action method's function writes its WILL_FIRE def
            // (cvtIFace wf_stmts); whether the schedule also reads it
            // (rule inhibitors) falls out of the rule-cone marking above
            if me.kind != ir::MethodKind::Value {
                let wf = format!("WILL_FIRE_{}", self.s(me.name));
                if let Some(id) = self.d.strings.iter().position(|x| x == &wf) {
                    let id = id as StrId;
                    if defs_by_name.contains_key(&id) {
                        usage.entry(id).or_default().insert(fk);
                    }
                }
            }
        }

        // member selection
        let mut members: Vec<ModVar> = Vec::new();
        for rst in &m.resets {
            if let Expr::Port(n) = &rst.wire {
                members.push(ModVar {
                    name: self.s(*n).to_string(),
                    src: VcdSrc::Reset(*n),
                    width: 1,
                    clocked: false,
                    domain: None,
                });
            }
        }
        for d in &m.defs {
            let is_string = d.width == 0
                || matches!(*d.expr, Expr::Str(_))
                || matches!(&*d.expr, Expr::Prim { op: ir::PrimOp::StringConcat, .. });
            if is_string {
                continue;
            }
            let n_fns = usage.get(&d.name).map(|s| s.len()).unwrap_or(0);
            let is_task = matches!(*d.expr, Expr::TaskValue { .. });
            // -keep-fires pins CAN_FIRE/WILL_FIRE defs (cfwfOkToMove)
            let pinned_fire =
                self.d.keep_fires && (d.props.can_fire || d.props.will_fire);
            let keep =
                n_fns >= 2 || (n_fns >= 1 && (d.width > 64 || is_task || pinned_fire));
            if keep {
                let dname = self.s(d.name).to_string();
                // an action method's WILL_FIRE follows the call, like EN
                // (the schedule zeroes it each edge, the call sets it)
                let src = dname
                    .strip_prefix("WILL_FIRE_")
                    .and_then(|rest| {
                        m.methods
                            .iter()
                            .find(|me| {
                                me.kind != ir::MethodKind::Value && self.s(me.name) == rest
                            })
                            .map(|me| VcdSrc::PortEn(me.name))
                    })
                    .unwrap_or(VcdSrc::Def(d.name));
                let domain = usage
                    .get(&d.name)
                    .and_then(|fs| fs.iter().find(|fk| fk.0 == 0).map(|fk| fk.1))
                    .or_else(|| m.rules.first().map(|r| r.clock_domain));
                members.push(ModVar {
                    name: dname,
                    src,
                    width: d.width,
                    clocked: true,
                    domain,
                });
            }
        }
        members.sort_by(|a, b| a.name.cmp(&b.name));

        // port selection
        let mut ports: Vec<ModVar> = Vec::new();
        for me in &m.methods {
            let is_action = me.kind != ir::MethodKind::Value;
            if is_action {
                let en = format!("EN_{}", self.s(me.name));
                if let Some(en_id) = self.d.strings.iter().position(|x| x == &en) {
                    let n_fns = usage.get(&(en_id as StrId)).map(|s| s.len()).unwrap_or(0);
                    if n_fns >= 2 || (self.d.keep_fires && n_fns >= 1) {
                        ports.push(ModVar {
                            name: en,
                            src: VcdSrc::PortEn(me.name),
                            width: 1,
                            clocked: true,
                            domain: Some(me.clock_domain),
                        });
                    }
                }
            }
            for (ai, a) in me.args.iter().enumerate() {
                let n_fns = usage.get(&a.name).map(|s| s.len()).unwrap_or(0);
                if n_fns >= 2 || a.width > 64 || (self.d.keep_fires && n_fns >= 1) {
                    ports.push(ModVar {
                        name: self.s(a.name).to_string(),
                        src: VcdSrc::PortArg(me.name, ai),
                        width: a.width,
                        clocked: true,
                        domain: Some(me.clock_domain),
                        });
                }
            }
            if let Some(res) = &me.result {
                let n_fns = usage.get(&me.name).map(|s| s.len()).unwrap_or(0);
                // Def/Port result exprs carry no intrinsic width
                // (expr.rs width() = 0) — resolve through the
                // declaration tables (TbGCD's 51-bit `result` port
                // was declared as 1 bit)
                let w = match res {
                    Expr::Def(n) => defs_by_name
                        .get(n)
                        .map(|&di| m.defs[di].width)
                        .unwrap_or(0),
                    Expr::Port(n) => m
                        .inputs
                        .iter()
                        .find(|p| p.name == *n)
                        .map(|p| p.width)
                        .unwrap_or(0),
                    e => e.width(),
                };
                if n_fns >= 2 || w > 64 || (self.d.keep_fires && n_fns >= 1) {
                    ports.push(ModVar {
                        name: self.s(me.name).to_string(),
                        src: VcdSrc::PortRes(me.name),
                        width: w.max(1),
                        clocked: true,
                        domain: Some(me.clock_domain),
                        });
                }
            }
        }
        ports.sort_by(|a, b| a.name.cmp(&b.name));

        let mv = std::rc::Rc::new(ModVars { members, ports });
        self.vcd_mod_vars.insert(module, mv.clone());
        mv
    }

    /// Ordered (name, child inst, is_prim) list for one instance, from
    /// the module's instance table (already cmpIdByName-sorted by the
    /// exporter).
    fn vcd_children(&self, inst: usize) -> Vec<(String, usize, bool)> {
        let module = self.module_of(inst);
        let mir = self.mods[module].ir;
        let mut out = Vec::new();
        if let InstKind::User { children, .. } = &self.insts[inst].kind {
            for i in &self.d.modules[mir].instances {
                if let Some(&ci) = children.get(&i.name) {
                    let is_prim = matches!(self.insts[ci].kind, InstKind::Prim(_));
                    out.push((self.s(i.name).to_string(), ci, is_prim));
                }
            }
        }
        out
    }

    /// dump_VCD_defs for one user module instance (SimCCBlock.hs
    /// simCCBlockToClassDefinition): scope, id block, clock defs and
    /// aliases, members, ports, primitives, submodules.
    fn vcd_scope_walk(&mut self, w: &mut vcd::Vcd, inst: usize, name: &str, levels: u32) {
        let module = self.module_of(inst);
        let mir = self.mods[module].ir;
        let mv = self.vcd_mod_vars(module);
        let kids = self.vcd_children(inst);
        let prims: Vec<_> = kids.iter().filter(|k| k.2).cloned().collect();
        let subs: Vec<_> = kids.iter().filter(|k| !k.2).cloned().collect();

        // FST records the scope's MODULE TYPE as its component field
        // (the fstscopes correlation surface); VCD ignores it
        let mtype = self.s(self.d.modules[mir].name).to_string();
        w.scope_start(name, Some(&mtype));
        let base = w.reserve_ids((mv.members.len() + mv.ports.len() + prims.len()) as u32);

        // clock-def loop (vcd_add_clock_def + match_hierarchy): an
        // undotted clock name is emitted only in the root scope; a dotted
        // one where the prefix matches this instance's path
        let path = self.insts[inst].path.clone();
        for c in 0..self.vcd_clocks.len() {
            let cname = self.vcd_clocks[c].name.clone();
            let cid = self.vcd_clocks[c].vcd_id;
            match cname.rfind('.') {
                None => {
                    if path.is_empty() {
                        w.write_def(cid, &cname, 1);
                    }
                }
                Some(k) => {
                    if path == cname[..k] {
                        w.write_def(cid, &cname[k + 1..], 1);
                    }
                }
            }
        }
        // input clock port aliases, reusing the bound kernel clock's id
        let my_clk = self.vcd_inst_clock.get(inst).copied().unwrap_or(0);
        let in_clks: Vec<StrId> = self.d.modules[mir]
            .inputs
            .iter()
            .filter(|p| p.kind == ir::PortKind::Clock)
            .map(|p| p.name)
            .collect();
        for pn in in_clks {
            let ci = self.vcd_clock_index(inst, pn).unwrap_or(my_clk);
            let cid = self.vcd_clocks[ci].vcd_id;
            w.write_def(cid, self.s(pn), 1);
        }

        // members then ports (ids base..); each var backdates to the
        // kernel clock of its own domain's composition
        let mut n = base;
        for v in mv.members.iter().chain(mv.ports.iter()) {
            if v.clocked {
                let ci = v
                    .domain
                    .and_then(|d| self.vcd_inst_domclock.get(&(inst, d)).copied())
                    .unwrap_or(my_clk);
                w.set_clock(n, ci);
            }
            w.write_def(n, &v.name, v.width);
            n += 1;
        }
        // primitives (self-reserving; the block slot stays unused)
        for (pname, pinst, _) in &prims {
            let pclk = self.vcd_inst_clock.get(*pinst).copied().unwrap_or(my_clk);
            let pcid = self.vcd_clocks[pclk].vcd_id;
            if let InstKind::Prim(p) = &mut self.insts[*pinst].kind {
                p.vcd_defs(w, pname, pclk, pcid);
            }
        }
        // submodules, depth-limited
        if levels != 1 {
            let l = if levels == 0 { 0 } else { levels - 1 };
            for (sname, sinst, _) in &subs {
                self.vcd_scope_walk(w, *sinst, sname, l);
            }
        }
        w.scope_end();
        self.vcd_layouts.insert(
            inst,
            VcdLayout { base, back: vec![None; mv.members.len() + mv.ports.len()] },
        );
    }

    /// Which kernel clock a local clock wire aliases: chase instantiation
    /// bindings (like resolve_clock_at) and match clock names.
    fn vcd_clock_index(&self, inst: usize, port: StrId) -> Option<usize> {
        let wire = self.s(port).to_string();
        self.vcd_clock_index_wire(inst, &wire, 0)
    }
    fn vcd_clock_index_wire(&self, inst: usize, wire: &str, depth: u32) -> Option<usize> {
        if depth > 16 {
            return None;
        }
        // absolute prim-driven names ("a.b$CLK_OUT") appear verbatim
        let abs = {
            let base = &self.insts[inst].path;
            if base.is_empty() {
                wire.to_string()
            } else {
                format!("{base}.{wire}")
            }
        };
        if let Some(k) = self.vcd_clocks.iter().position(|c| c.name == abs) {
            return Some(k);
        }
        // child-qualified wire: resolve inside the child
        if let Some(kk) = wire.rfind('$') {
            let (qual, base) = (&wire[..kk], &wire[kk + 1..]);
            let cpath = if self.insts[inst].path.is_empty() {
                qual.to_string()
            } else {
                format!("{}.{}", self.insts[inst].path, qual)
            };
            if let Some(&ci) = self.inst_by_path.get(&cpath) {
                return self.vcd_clock_index_wire(ci, base, depth + 1);
            }
        }
        // top-level ports match kernel clock names directly
        if self.insts[inst].path.is_empty() {
            return self.vcd_clocks.iter().position(|c| c.name == wire);
        }
        // input clock port: chase the parent's binding expression
        if let InstKind::User { clk_binds, .. } = &self.insts[inst].kind {
            let pid = self.d.strings.iter().position(|x| x == wire);
            if let Some(pid) = pid {
                if let Some((owner, e)) = clk_binds.get(&(pid as StrId)) {
                    let (owner, e) = (*owner, e.clone());
                    let osc = match &e {
                        Expr::Clock { osc, .. } => osc.as_ref().clone(),
                        other => other.clone(),
                    };
                    if let Expr::Port(n) = osc {
                        let name = self.s(n).to_string();
                        return self.vcd_clock_index_wire(owner, &name, depth + 1);
                    }
                }
            }
        }
        None
    }

    /// Current value of one module-scope var.
    fn vcd_var_value(&mut self, inst: usize, v: &ModVar) -> Value {
        match &v.src {
            VcdSrc::Reset(n) => {
                let node = if let InstKind::User { resets, .. } = &self.insts[inst].kind {
                    resets.get(n).copied()
                } else {
                    None
                };
                let asserted = node.map(|nn| self.rst_asserted[nn]).unwrap_or(false);
                Value::from_u64(1, (!asserted) as u64)
            }
            VcdSrc::Def(n) => {
                // traced artifacts: the recording slot is the authority
                if !self.jit_arena_ptr.is_null() {
                    if let Some(&(base, w)) = self.jit_rec_defs.get(&(inst, *n)) {
                        return self.rec_read(base, w);
                    }
                }
                self.vcd_def_vals
                    .get(&(inst, *n))
                    .cloned()
                    .unwrap_or_else(|| Value::undet(v.width.max(1)))
            }
            VcdSrc::PortEn(mth) => {
                let clk = self.vcd_inst_clock.get(inst).copied().unwrap_or(0);
                let at = self.vcd_clocks[clk].pos_at;
                let en = if !self.jit_arena_ptr.is_null()
                    && self.jit_rec_meths.contains_key(&(inst, *mth))
                {
                    let rs = &self.jit_rec_meths[&(inst, *mth)];
                    let t = unsafe { *self.jit_arena_ptr.add(rs.t as usize) };
                    t == at
                } else {
                    self.vcd_meth_calls
                        .get(&(inst, *mth))
                        .map(|(t, _)| *t == at)
                        .unwrap_or(false)
                };
                Value::from_u64(1, en as u64)
            }
            // all method ports are zero-initialized in the C++ ctor
            // (mkPortInit) and only updated when the method is called
            VcdSrc::PortArg(mth, ai) => {
                if !self.jit_arena_ptr.is_null() {
                    if let Some(rs) = self.jit_rec_meths.get(&(inst, *mth)) {
                        if let Some(&(base, w)) = rs.args.get(*ai) {
                            return self.rec_read(base, w).zext(v.width.max(1));
                        }
                    }
                }
                self.vcd_meth_calls
                    .get(&(inst, *mth))
                    .and_then(|(_, args)| args.get(*ai).cloned())
                    .map(|x| x.zext(v.width.max(1)))
                    .unwrap_or_else(|| Value::zero(v.width.max(1)))
            }
            VcdSrc::PortRes(mth) => {
                if !self.jit_arena_ptr.is_null() {
                    if let Some(rs) = self.jit_rec_meths.get(&(inst, *mth)) {
                        if let Some((base, w)) = rs.res {
                            return self.rec_read(base, w).zext(v.width.max(1));
                        }
                    }
                }
                self.vcd_meth_results
                    .get(&(inst, *mth))
                    .map(|r| r.clone().zext(v.width.max(1)))
                    .unwrap_or_else(|| Value::zero(v.width.max(1)))
            }
        }
    }

    /// dump_VCD for one user module instance: members+ports (vcd_defs),
    /// primitives, submodules — same order and ids as the defs walk.
    fn vcd_dump_walk(
        &mut self,
        w: &mut vcd::Vcd,
        inst: usize,
        dt: vcd::DumpType,
        now: u64,
        levels: u32,
    ) {
        use vcd::DumpType as D;
        let module = self.module_of(inst);
        let mv = self.vcd_mod_vars(module);
        let kids = self.vcd_children(inst);
        let Some(mut layout) = self.vcd_layouts.remove(&inst) else { return };
        let mut n = layout.base;
        for (k, v) in mv.members.iter().chain(mv.ports.iter()).enumerate() {
            match dt {
                D::Xs => w.write_x(n, v.width, now),
                D::Changes => {
                    let cur = self.vcd_var_value(inst, v);
                    if layout.back[k].as_ref() != Some(&cur) {
                        w.write_val(n, &cur, now);
                        layout.back[k] = Some(cur);
                    }
                }
                _ => {
                    let cur = self.vcd_var_value(inst, v);
                    w.write_val(n, &cur, now);
                    layout.back[k] = Some(cur);
                }
            }
            n += 1;
        }
        self.vcd_layouts.insert(inst, layout);
        for (_, pinst, is_prim) in &kids {
            if !*is_prim {
                continue;
            }
            let pclk = self.vcd_inst_clock.get(*pinst).copied().unwrap_or(0);
            let edge_now = {
                let c = &self.vcd_clocks[pclk];
                c.cur && c.pos_at == now
            };
            if let InstKind::Prim(p) = &mut self.insts[*pinst].kind {
                p.vcd_dump(w, dt, now, edge_now);
            }
        }
        if levels != 1 {
            let l = if levels == 0 { 0 } else { levels - 1 };
            for (_, sinst, is_prim) in &kids {
                if !*is_prim {
                    self.vcd_dump_walk(w, *sinst, dt, now, l);
                }
            }
        }
    }

    /// kernel.cxx vcd_event: once per timeslice at PG_AFTER_LOGIC, after
    /// all same-time edges (and reset flushing), before the PG_FINAL
    /// early-rule pass.
    fn vcd_event(&mut self, now: u64) {
        let mut w = std::mem::replace(&mut self.vcd, vcd::Vcd::new());
        let top = *self.inst_by_path.get("").unwrap_or(&0);
        if w.write_header() {
            self.vcd_layouts.clear();
            w.scope_start("main", None);
            let levels = w.depth;
            self.vcd_scope_walk(&mut w, top, "top", levels);
            w.scope_end();
            w.enddefinitions();
        }
        w.advance(now, false);
        let dt = w.dump_type();
        use vcd::DumpType as D;
        let bit = |b: bool| Value::from_u64(1, b as u64);
        match dt {
            D::None => {}
            D::Xs => {
                w.task(now, "$dumpoff");
                for c in &self.vcd_clocks {
                    w.write_x(c.vcd_id, 1, now);
                }
                let levels = w.depth;
                self.vcd_dump_walk(&mut w, top, dt, now, levels);
            }
            D::Initial => {
                w.task(now, "$dumpvars");
                for c in &self.vcd_clocks {
                    if c.has_init || c.pos_count != 0 {
                        w.write_val(c.vcd_id, &bit(c.cur), now);
                    }
                }
                let levels = w.depth;
                self.vcd_dump_walk(&mut w, top, dt, now, levels);
            }
            D::Changes => {
                for c in &self.vcd_clocks {
                    if c.pos_at == now || c.neg_at == now {
                        w.write_val(c.vcd_id, &bit(c.cur), now);
                    }
                }
                let levels = w.depth;
                self.vcd_dump_walk(&mut w, top, dt, now, levels);
            }
            D::Checkpoint | D::Restart => {
                w.task(now, if dt == D::Checkpoint { "$dumpall" } else { "$dumpon" });
                for c in &self.vcd_clocks {
                    w.write_val(c.vcd_id, &bit(c.cur), now);
                }
                let levels = w.depth;
                self.vcd_dump_walk(&mut w, top, dt, now, levels);
            }
        }
        w.check_file_size(now);
        self.vcd = w;
    }

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
        // intra-module ME inhibitors (earlier disjoint rules' CFs).
        // A compiled rule's CF lives in its arena slot, not in latched
        // (the native scheds keep it current) — an early rule inhibited
        // by a compiled rule must read the slot when no interpreter
        // latch exists (review finding: latched-only lookup missed
        // every compiled inhibitor)
        for other in &r.me_inhibits {
            let other_ri = self.mods[module].rules[other];
            let other_cf = self.d.modules[mir].rules[other_ri].can_fire;
            if let Some(v) = self.latched_or_arena(inst, other_cf) {
                if v.as_bool() {
                    cf = Value::zero(1);
                }
            }
        }
        // cross-module inhibitors targeting this rule
        for (other_inst, other_cf) in cross_inh {
            if let Some(v) = self.latched_or_arena(*other_inst, *other_cf) {
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
        if self.trace_wf && wf.as_bool() {
            eprintln!("[{}] FIRE {}.{}", self.cycle, self.insts[inst].path,
                      self.s(rule_name));
        }
        self.set_latched(inst, r.will_fire, wf);
    }

    fn exec_rule(&mut self, inst: usize, rule_name: StrId) {
        let module = self.module_of(inst);
        let mir = self.mods[module].ir;
        let ri = match self.mods[module].rules.get(&rule_name) {
            Some(ri) => *ri,
            None => return,
        };
        let wf = self.d.modules[mir].rules[ri].will_fire;
        let fire = self.latched(inst, wf).map(|v| v.as_bool()).unwrap_or(false);
        if !fire {
            return;
        }
        self.exec_rule_forced(inst, rule_name);
    }

    /// Execute a rule body unconditionally (the caller has already
    /// established WILL_FIRE — the JIT dispatch reads it from the slot).
    fn exec_rule_forced(&mut self, inst: usize, rule_name: StrId) {
        let module = self.module_of(inst);
        let mir = self.mods[module].ir;
        let ri = match self.mods[module].rules.get(&rule_name) {
            Some(ri) => *ri,
            None => return,
        };
        let r = self.d.modules[mir].rules[ri].clone();
        let mut ctx = Ctx::default();
        for st in r.body.iter() {
            self.exec_stmt(inst, &mut ctx, st);
        }
    }

    /// Apply a reset-node transition: broadcast to subscribed prims, then
    /// poll reset generators among them — asynchronous assertion cascades
    /// immediately (reset_fn called inside reset_IN_RST), synchronous /
    /// deassert transitions defer to the end of the timeslice
    /// (reset_at_end_of_timeslice).
    fn apply_reset(&mut self, node: usize, asserted: bool) {
        let mut work = vec![(node, asserted)];
        while let Some((n, v)) = work.pop() {
            if self.rst_asserted[n] == v {
                continue;
            }
            self.rst_asserted[n] = v;
            if v {
                self.rst_active += 1;
            } else {
                self.rst_active -= 1;
            }
            // mirror the port LEVEL into the JIT arena (compiled reset
            // guards read it there)
            if !self.jit_arena_ptr.is_null() {
                unsafe {
                    *self.jit_arena_ptr.add(self.jit_reset_slots[n] as usize) = (!v) as u64;
                }
            }
            if self.trace_clk {
                eprintln!("[t={}] reset node {n} -> asserted={v}", self.now);
            }
            let subs = self.rst_subs[n].clone();
            for (idx, ord) in subs {
                if let InstKind::Prim(p) = &mut self.insts[idx].kind {
                    p.set_reset_input(ord, v);
                    if self.rstgen_out.contains_key(&idx) {
                        for (out_v, immediate) in p.take_reset_out() {
                            let out = self.rstgen_out[&idx];
                            if immediate {
                                work.push((out, out_v));
                            } else {
                                self.rst_pending.push((out, out_v));
                            }
                        }
                    }
                }
            }
        }
    }

    /// Poll one reset generator after its clock tick and route its
    /// pending output transitions.
    fn poll_rstgen(&mut self, idx: usize) {
        let Some(&out) = self.rstgen_out.get(&idx) else { return };
        let outs = if let InstKind::Prim(p) = &mut self.insts[idx].kind {
            p.take_reset_out()
        } else {
            return;
        };
        for (v, immediate) in outs {
            if immediate {
                self.apply_reset(out, v);
            } else {
                self.rst_pending.push((out, v));
            }
        }
    }

    /// End of timeslice: let generators move internally deferred
    /// transitions forward, then apply all deferred transitions (which
    /// may cascade into more, applied in the same instant).
    fn flush_reset_pending(&mut self) {
        let gens: Vec<usize> = self.rstgen_out.keys().copied().collect();
        for idx in gens {
            if let InstKind::Prim(p) = &mut self.insts[idx].kind {
                p.end_of_timeslice();
            }
            self.poll_rstgen_deferred(idx);
        }
        while let Some((n, v)) = self.rst_pending.pop() {
            self.apply_reset(n, v);
        }
    }

    /// Poll a generator, treating everything as applicable now (used at
    /// flush time, when deferred == immediate).
    fn poll_rstgen_deferred(&mut self, idx: usize) {
        let Some(&out) = self.rstgen_out.get(&idx) else { return };
        let outs = if let InstKind::Prim(p) = &mut self.insts[idx].kind {
            p.take_reset_out()
        } else {
            return;
        };
        for (v, _) in outs {
            self.rst_pending.push((out, v));
        }
    }

    fn default_wave() -> ClockSource {
        ClockSource::Wave(Wave { init_high: false, delay: 0, hi: 5, lo: 5, has_init: false })
    }

    /// Resolve a composition clock: the default clock is the fixed 5/5
    /// wave; "<path>$CLK_OUT" names a ClockGen (periodic) or a dynamic
    /// clock prim (MakeClock/ClockDiv/ClockInverter, prim-triggered
    /// edges); interface output clocks and input clock ports chase
    /// through ifc_clocks / instantiation bindings to their oscillator;
    /// noClock and non-default top input clocks never fire.
    fn resolve_source(&self, clock: StrId) -> ClockSource {
        if Some(clock) == self.d.default_clock {
            return Self::default_wave();
        }
        let name = self.s(clock).to_string();
        self.resolve_clock_at(0, &name)
    }

    /// Resolve a clock wire name relative to an instance.
    fn resolve_clock_at(&self, inst: usize, wire: &str) -> ClockSource {
        // prim-driven clocks register absolute "<abs.path>$CLK_OUT" keys
        let abs = {
            let base = &self.insts[inst].path;
            if base.is_empty() {
                wire.to_string()
            } else {
                format!("{base}.{wire}")
            }
        };
        if let Some(w) = self.clockgen_waves.get(&abs) {
            return ClockSource::Wave(*w);
        }
        if let Some(&init_high) = self.dynclk_init.get(&abs) {
            let path = abs.strip_suffix("$CLK_OUT").unwrap_or(&abs);
            let driver = *self
                .inst_by_path
                .get(path)
                .unwrap_or_else(|| panic!("unknown clock driver {path:?}"));
            return ClockSource::Triggered { init_high, driver };
        }
        // child-qualified wire ("i$CLK_outclk", "a.b$CLK_x"): resolve
        // inside the child instance
        if let Some(k) = wire.rfind('$') {
            let (qual, base) = (&wire[..k], &wire[k + 1..]);
            let cpath = if self.insts[inst].path.is_empty() {
                qual.to_string()
            } else {
                format!("{}.{}", self.insts[inst].path, qual)
            };
            if let Some(&ci) = self.inst_by_path.get(&cpath) {
                return self.resolve_clock_at(ci, base);
            }
            panic!("trs-interp: unimplemented clock source {wire:?} (P1 bring-up)");
        }
        // top-level wires: the default clock name resolves to the fixed
        // wave; other top input clocks have no waveform and never fire
        let module = self.module_of(inst);
        let mir = self.mods[module].ir;
        if self.insts[inst].path.is_empty() {
            if let Some(dc) = self.d.default_clock {
                if self.s(dc) == wire {
                    return Self::default_wave();
                }
            }
        }
        // interface output clock of this module: follow the internal wire
        if let Some((_, osc)) = self.d.modules[mir]
            .ifc_clocks
            .iter()
            .find(|(n, _)| self.s(*n) == wire)
        {
            return match osc {
                Expr::Port(p) => {
                    let inner = self.s(*p).to_string();
                    self.resolve_clock_at(inst, &inner)
                }
                _ => ClockSource::Never, // constant = noClock
            };
        }
        // input clock port: chase the parent's instantiation binding
        if let InstKind::User { clk_binds, .. } = &self.insts[inst].kind {
            if let Some((owner, osc)) =
                clk_binds.iter().find(|(k, _)| self.s(**k) == wire).map(|(_, v)| v)
            {
                return match osc {
                    Expr::Port(p) => {
                        let inner = self.s(*p).to_string();
                        self.resolve_clock_at(*owner, &inner)
                    }
                    _ => ClockSource::Never,
                };
            }
        }
        // a top input clock port that is not the default clock: defined
        // with period 0, never ticks
        if self.insts[inst].path.is_empty()
            && self.d.modules[mir]
                .inputs
                .iter()
                .any(|p| p.kind == ir::PortKind::Clock && self.s(p.name) == wire)
        {
            return ClockSource::Never;
        }
        panic!("trs-interp: unimplemented clock source {wire:?} (P1 bring-up)");
    }

    /// Current simulation time (bk_now), for the driver's `sim time`.
    pub fn now(&self) -> u64 {
        self.now
    }

    /// Kernel clock snapshots (getClockInfo), for `sim clock`.  Empty
    /// before the first run() call.
    pub fn clock_info(&self) -> Vec<ClockInfo> {
        self.vcd_clocks
            .iter()
            .map(|c| ClockInfo {
                name: c.name.clone(),
                initial_val: c.init_val,
                first_edge: c.first_edge.unwrap_or(0),
                low_dur: c.low_dur,
                high_dur: c.high_dur,
                cycles: c.pos_count,
                neg_edges: c.neg_count,
                cur_val: c.cur,
                last_edge: c.pos_at.max(c.neg_at),
            })
            .collect()
    }

    /// Run until $finish or the cycle limit.  Returns the exit code.
    fn dump_central_bails(&self) {
        if std::env::var_os("TRS_JIT_TRACE").is_none() {
            return;
        }
        let v: Vec<String> = CENTRAL_BAIL
            .iter()
            .enumerate()
            .filter(|(_, c)| c.load(std::sync::atomic::Ordering::Relaxed) > 0)
            .map(|(k, c)| format!("#{k}x{}", c.load(std::sync::atomic::Ordering::Relaxed)))
            .collect();
        if !v.is_empty() {
            eprintln!("trs jit: central bails: {}", v.join(" "));
        }
    }

    pub fn run(&mut self, max_cycles: u64) -> i32 {
        #[cfg(feature = "aot")]
        let t0 = jit::prof::on().then(std::time::Instant::now);
        self.advance(max_cycles);
        self.dump_central_bails();
        let rc = self.finish();
        #[cfg(feature = "aot")]
        if let Some(t0) = t0 {
            jit::prof::dump(t0.elapsed());
        }
        rc
    }

    /// Lockstep selfcheck (trs run --selfcheck): drive this engine —
    /// the PRIMARY, which owns stdout, waveforms, and the exit status —
    /// and one or more quiet shadow engines in bounded steps, comparing
    /// each shadow against the primary every `every` default-clock
    /// posedges (and at the end of the run): cycle/finish status,
    /// architectural prim state, and time where time is architecturally
    /// visible.  With an aot primary and interp+jit shadows, ONE run
    /// cross-checks all three execution tiers — no per-engine test-mode
    /// explosion — with no reference simulator anywhere.  A divergence
    /// reports on stderr (instant + the first mismatching state,
    /// primary-vs-shadow, shadow named by engine) and the run exits 87
    /// AT the divergence, the oracle doctrine's stop point.
    ///
    /// TRS_SELFCHECK_INJECT=<cycle> is the detector's negative
    /// witness: once the primary passes that cycle, the first shadow is
    /// advanced one extra posedge, which must trip the next compare.
    pub fn run_lockstep(
        &mut self,
        shadows: &mut [(&'static str, Interp)],
        max_cycles: u64,
        every: u64,
    ) -> i32 {
        let every = every.max(1);
        let inject = std::env::var("TRS_SELFCHECK_INJECT")
            .ok()
            .and_then(|v| v.parse::<u64>().ok());
        let mut injected = false;
        let trace = std::env::var_os("TRS_SELFCHECK_TRACE").is_some();
        loop {
            let target = self.cycles().saturating_add(every).min(max_cycles);
            self.advance(target);
            for (_, sh) in shadows.iter_mut() {
                sh.advance(target);
            }
            if trace {
                let mut line = format!(
                    "trs selfcheck: checkpoint target={target} primary \
                     (t={}, c={})",
                    self.now, self.cycle
                );
                for (kind, sh) in shadows.iter() {
                    line.push_str(&format!(
                        " {kind} (t={}, c={})",
                        sh.now, sh.cycle
                    ));
                }
                eprintln!("{line}");
            }
            if let Some(n) = inject {
                if !injected && self.cycles() >= n {
                    if let Some((_, sh)) = shadows.first_mut() {
                        sh.advance(sh.cycles().saturating_add(1));
                    }
                    injected = true;
                }
            }
            // a stop consumed by the cycle budget alone is an INTERNAL
            // point: the central player and the general loop credit the
            // last posedge's companion-negedge instant differently, so
            // `now` can sit half a period apart with identical
            // architectural history (no output can observe it — VCD
            // disables the central player, and interactive stops use
            // the heap loop).  Time compares only where time is
            // architecturally visible: $finish/$stop/heap-dry stops.
            let budget_stop = self.fe.finished.is_none()
                && !self.fe.stop_request
                && self.cycles() >= target;
            for si in 0..shadows.len() {
                let (kind, shadow) = &mut shadows[si];
                let kind = *kind;
                let mut diverged = Vec::new();
                if !budget_stop && shadow.now != self.now {
                    diverged.push(format!(
                        "time {} vs primary {}",
                        shadow.now, self.now
                    ));
                }
                if shadow.cycle != self.cycle {
                    diverged.push(format!(
                        "cycle {} vs primary {}",
                        shadow.cycle, self.cycle
                    ));
                }
                if shadow.fe.finished != self.fe.finished {
                    diverged.push(format!(
                        "finished {:?} vs primary {:?}",
                        shadow.fe.finished, self.fe.finished
                    ));
                }
                // shape first: state addressed at different times
                // compares apples to oranges (the capi oracle's
                // per-engine gate)
                if diverged.is_empty() {
                    diverged = self.state_divergence(shadow, 8);
                }
                if !diverged.is_empty() {
                    eprintln!(
                        "trs selfcheck: DIVERGENCE [{kind} shadow] at \
                         time {} (cycle {}):",
                        self.now, self.cycle
                    );
                    for d in &diverged {
                        eprintln!("trs selfcheck:   {d}");
                    }
                    self.dump_central_bails();
                    let _ = self.finish();
                    return 87;
                }
            }
            if self.fe.finished.is_some()
                || self.fe.stop_request
                || self.cycles() >= max_cycles
            {
                break;
            }
            if self.cycles() < target {
                // neither $finish, $stop, nor the cycle target ended
                // the step: the event heap is dry (run() would have
                // returned here after its single advance)
                break;
            }
        }
        self.dump_central_bails();
        let rc = self.finish();
        for (_, sh) in shadows.iter_mut() {
            let _ = sh.finish();
        }
        rc
    }

    /// Default-clock posedges processed so far (the `sim step` cursor).
    pub fn cycles(&self) -> u64 {
        self.cycle
    }

    /// True once $finish has been called (stepping past it is an error
    /// in the reference driver).
    pub fn is_finished(&self) -> bool {
        self.fe.finished.is_some()
    }

    /// Derive PlanA fresh (see PlanA): the string-keyed schedule
    /// resolution.  Emit requests always derive (the artifact writes
    /// what it derived); Load requests prefer the artifact's baked
    /// copy and fall back here.
    fn derive_plan_a(&mut self) -> PlanA {
        let comps = self.d.compositions.clone();

        // distinct clocks in first-appearance order, with the default
        // clock first: the kernel defines it before derived clocks, and
        // VCD clock ids follow definition order (CLK = id 0 = '!')
        let mut clocks: Vec<StrId> = Vec::new();
        if let Some(dc) = self.d.default_clock {
            if comps.iter().any(|c| c.clock == dc) {
                clocks.push(dc);
            }
        }
        for c in &comps {
            if !clocks.contains(&c.clock) {
                clocks.push(c.clock);
            }
        }
        let sources: Vec<ClockSource> = clocks
            .iter()
            .map(|&c| {
                // a phantom prim domain (getPrimDomainInfo's homeless
                // port clock, e.g. RegAligned's "clk_src"): all its
                // compositions are empty, and the name resolves nowhere.
                // Reference bluesim likewise defines a kernel clock that
                // never receives edges, so it must stay in the clock
                // list (VCD order) but never fire.
                let unused = Some(c) != self.d.default_clock
                    && comps.iter().filter(|k| k.clock == c).all(|k| {
                        k.entries.is_empty()
                            && k.ticks.is_empty()
                            && k.early.is_empty()
                            && k.cross_inhibits.is_empty()
                    });
                if unused {
                    ClockSource::Never
                } else {
                    self.resolve_source(c)
                }
            })
            .collect();
        if self.trace_clk {
            for (ci, &c) in clocks.iter().enumerate() {
                let k = match &sources[ci] {
                    ClockSource::Wave(w) => format!(
                        "wave init_high={} delay={} hi={} lo={}",
                        w.init_high, w.delay, w.hi, w.lo
                    ),
                    ClockSource::Triggered { driver, .. } => {
                        format!("triggered by inst {driver}")
                    }
                    ClockSource::Never => "never".to_string(),
                };
                eprintln!("clock[{ci}] {:?}: {k}", self.s(c));
            }
        }
        // clock-driver prim instance -> clock index, for routing triggered
        // edges after ticks
        let driver_clock: HashMap<usize, usize> = sources
            .iter()
            .enumerate()
            .filter_map(|(ci, s)| match s {
                ClockSource::Triggered { driver, .. } => Some((*driver, ci)),
                _ => None,
            })
            .collect();

        // pre-resolve each composition: (clock idx, entries, inhibitors,
        // ticks) — see RComp at module scope
        let rcomps: Vec<RComp> = comps
            .iter()
            .map(|comp| {
                let mut entries: Vec<REntry> = comp
                    .entries
                    .iter()
                    .map(|e| {
                        let path = self.s(e.instance).to_string();
                        let ii = *self
                            .inst_by_path
                            .get(&path)
                            .unwrap_or_else(|| panic!("unknown instance path {path:?}"));
                        let mir = self.mods[self.module_of(ii)].ir;
                        let sched = &self.d.modules[mir].schedule;
                        let ms = sched
                            .domains
                            .iter()
                            .find(|ms| ms.domain == e.domain && ms.posedge == comp.posedge)
                            .or_else(|| {
                                sched.domains.iter().find(|ms| ms.domain == e.domain)
                            })
                            .unwrap_or_else(|| {
                                panic!(
                                    "no schedule for domain {} in {:?}",
                                    e.domain,
                                    self.s(self.d.modules[mir].name)
                                )
                            });
                        REntry {
                            inst: ii,
                            domain: e.domain,
                            nodes: ms.segments[e.segment as usize].nodes.clone(),
                            eager: Vec::new(),
                        }
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

                let ticks = comp
                    .ticks
                    .iter()
                    .map(|tk| {
                        let ipath = self.s(tk.instance).to_string();
                        let ppath = if ipath.is_empty() {
                            self.s(tk.prim).to_string()
                        } else {
                            format!("{}.{}", ipath, self.s(tk.prim))
                        };
                        let ii = *self
                            .inst_by_path
                            .get(&ppath)
                            .unwrap_or_else(|| panic!("unknown tick instance {ppath:?}"));
                        let owner = *self.inst_by_path.get(&ipath).unwrap_or(&0);
                        let pname = self.d.strings[tk.port as usize].clone();
                        (ii, pname, tk.reset, owner, tk.gate.clone())
                    })
                    // no-op ticks (Reg/ConfigReg/FIFO clock ticks) cost a
                    // dynamic dispatch per prim per edge to do nothing;
                    // drop them unless the entry has side duties (reset
                    // ticks, reset generators, clock drivers)
                    .filter(|&(ii, _, is_rst, _, _)| {
                        if is_rst || self.rstgen_out.contains_key(&ii) {
                            return true;
                        }
                        match &self.insts[ii].kind {
                            InstKind::Prim(p) => !p.tick_is_noop(),
                            _ => true,
                        }
                    })
                    .collect();

                let early: HashSet<(usize, StrId)> = comp
                    .early
                    .iter()
                    .map(|q| self.split_qual(*q))
                    .collect();

                // per-entry eager schedule defs (see REntry::eager): walk
                // entries in merged order, attaching each cone def to the
                // first entry whose Sched rules reach it
                let mut attached: HashSet<(usize, StrId)> = HashSet::new();
                for en in &mut entries {
                    let ii = en.inst;
                    let module = self.module_of(ii);
                    let mir = self.mods[module].ir;
                    let mut stack: Vec<StrId> = Vec::new();
                    for &node in &en.nodes {
                        let SchedNode::Sched(r) = node else { continue };
                        if early.contains(&(ii, r)) {
                            continue;
                        }
                        let Some(&ri) = self.mods[module].rules.get(&r) else {
                            continue;
                        };
                        let rr = &self.d.modules[mir].rules[ri];
                        stack.push(rr.can_fire);
                        stack.push(rr.will_fire);
                    }
                    let mut visited: HashSet<StrId> = HashSet::new();
                    let mut wanted: HashSet<StrId> = HashSet::new();
                    while let Some(dn) = stack.pop() {
                        if !visited.insert(dn) {
                            continue;
                        }
                        let Some(&di) = self.mods[module].defs.get(&dn) else {
                            continue;
                        };
                        let d = &self.d.modules[mir].defs[di];
                        // CF/WF defs (rule or method) are never latched
                        // here; new cone defs attach to this entry
                        if !d.props.can_fire
                            && !d.props.will_fire
                            && !attached.contains(&(ii, dn))
                        {
                            wanted.insert(dn);
                        }
                        collect_def_refs(&d.expr, &mut stack);
                    }
                    for d in &self.d.modules[mir].defs {
                        if wanted.contains(&d.name) {
                            attached.insert((ii, d.name));
                            en.eager.push(d.name);
                        }
                    }
                }

                RComp {
                    clk: clocks.iter().position(|&c| c == comp.clock).unwrap(),
                    posedge: comp.posedge,
                    entries,
                    cross,
                    ticks,
                    early,
                }
            })
            .collect();
        PlanA {
            version: PLAN_A_VERSION,
            clocks,
            sources,
            driver_clock: driver_clock.into_iter().collect(),
            rcomps,
        }
    }

    /// One-time event-loop setup: resolve clocks and compositions, wire
    /// VCD clock state, seed the event heap, and run the kernel reset
    /// protocol.  Idempotent — later calls are no-ops.
    pub fn prime(&mut self) {
        if self.stepper.is_some() {
            return;
        }
        // baked plan first (Load requests): skip the derivation walks
        let mut psl = startup::StartupLap::new();
        #[cfg(feature = "aot")]
        let plan = match &self.jit_request {
            jit::JitRequest::Load { src } => {
                match jit::aot_plan_a(src, self.bir_hash) {
                    Some(p) => {
                        psl.lap("plan-a (baked decode)");
                        p
                    }
                    None => {
                        let p = self.derive_plan_a();
                        psl.lap("plan-a (derived: no valid bake)");
                        p
                    }
                }
            }
            _ => {
                let p = self.derive_plan_a();
                psl.lap("plan-a (derived)");
                p
            }
        };
        #[cfg(not(feature = "aot"))]
        let plan = {
            let p = self.derive_plan_a();
            psl.lap("plan-a (derived)");
            p
        };
        // Emit requests bake what they derived: stash the bytes for
        // the meta object (read at the aot_emit call site)
        #[cfg(feature = "aot")]
        if matches!(self.jit_request, jit::JitRequest::Emit { .. }) {
            self.plan_a_bytes = bincode::serialize(&plan).ok();
        }
        let PlanA { version: _, clocks, sources, driver_clock, rcomps } = plan;
        let driver_clock: HashMap<usize, usize> =
            driver_clock.into_iter().collect();
        // VCD clock state: reserve the kernel clock ids first (clock ids
        // are permanent across headers — vcd_keep_ids)
        if self.vcd_clocks.is_empty() {
            self.vcd_clocks = clocks
                .iter()
                .enumerate()
                .map(|(ci, &c)| VcdClock {
                    name: self.s(c).to_string(),
                    vcd_id: 0,
                    cur: match &sources[ci] {
                        ClockSource::Wave(w) => w.init_high,
                        ClockSource::Triggered { init_high, .. } => *init_high,
                        ClockSource::Never => false,
                    },
                    has_init: match &sources[ci] {
                        ClockSource::Wave(w) => w.has_init,
                        ClockSource::Triggered { .. } => true,
                        ClockSource::Never => false,
                    },
                    init_val: match &sources[ci] {
                        ClockSource::Wave(w) => w.init_high,
                        ClockSource::Triggered { init_high, .. } => *init_high,
                        ClockSource::Never => false,
                    },
                    // waveform clocks know their first edge STATICALLY
                    // (bk_clock_first_edge answers before any edge runs;
                    // the ClockGen initial one-shot at t=0 does NOT
                    // count).  Triggered clocks stay observational.
                    first_edge: match &sources[ci] {
                        ClockSource::Wave(w) => Some(w.delay),
                        _ => None,
                    },
                    low_dur: match &sources[ci] {
                        ClockSource::Wave(w) => w.lo,
                        _ => 0,
                    },
                    high_dur: match &sources[ci] {
                        ClockSource::Wave(w) => w.hi,
                        _ => 0,
                    },
                    pos_count: 0,
                    neg_count: 0,
                    pos_at: 0,
                    neg_at: 0,
                })
                .collect();
            for c in &mut self.vcd_clocks {
                c.vcd_id = self.vcd.reserve_ids(1);
            }
            self.vcd.keep_ids();
            self.vcd.clk_combinational = vec![0; clocks.len()];
        }
        // per-instance clock, for prim CLK aliases / vcd_set_clock /
        // posedge gating
        self.vcd_inst_clock = vec![0; self.insts.len()];
        self.vcd_inst_domclock.clear();
        for rc in &rcomps {
            for en in &rc.entries {
                self.vcd_inst_clock[en.inst] = rc.clk;
                self.vcd_inst_domclock.insert((en.inst, en.domain), rc.clk);
            }
            for (inst, pname, is_rst, owner, _) in &rc.ticks {
                self.vcd_inst_clock[*inst] = rc.clk;
                self.vcd_inst_clock[*owner] = rc.clk;
                if !*is_rst {
                    let cid = self.vcd_clocks[rc.clk].vcd_id;
                    if let InstKind::Prim(p) = &mut self.insts[*inst].kind {
                        p.vcd_port_clock(pname, rc.clk, cid);
                    }
                }
            }
        }
        if self.trace_clk {
            for (i, rc) in rcomps.iter().enumerate() {
                let tickinfo: Vec<String> = rc
                    .ticks
                    .iter()
                    .map(|(inst, pname, rst, _, _)| {
                        format!(
                            "{}:{}{}",
                            self.insts[*inst].path,
                            pname,
                            if *rst { "(rst)" } else { "" }
                        )
                    })
                    .collect();
                eprintln!(
                    "comp[{i}] clk={} pos={} entries={} ticks={:?}",
                    rc.clk,
                    rc.posedge,
                    rc.entries.len(),
                    tickinfo
                );
            }
        }
        // clock-source prims alias the clock they DRIVE (CLK_OUT)
        for (ci, &c) in clocks.iter().enumerate() {
            let cname = self.s(c).to_string();
            if let Some(cpath) = cname.strip_suffix("$CLK_OUT") {
                if let Some(&pi) = self.inst_by_path.get(cpath) {
                    self.vcd_inst_clock[pi] = ci;
                }
            }
        }
        // batch waveform selection (-V / +bscvcd / +bscfst): format
        // first, then the named file, or the format's default via
        // set_state (mirroring bluesim.tcl's `sim <fmt> on|<file>`)
        if let Some((fmt, file)) = self.wave_pending.take() {
            let now = self.now;
            // -dump-formats gate FIRST: a refused request must not
            // even create the file (the reference errors on stderr
            // and the simulation runs on, dump-less)
            if self.vcd.format_available(fmt) && self.vcd.set_format(fmt, now) {
                match file {
                    Some(f) => {
                        if self.vcd.set_file(&f).is_ok() {
                            self.vcd.set_state(true);
                        }
                    }
                    None => self.vcd.set_state(true),
                }
                self.vcd_trace = true;
                // armed: the wave engine runs interpreted (jit_plan
                // reads this AFTER the take above).  A REFUSED request
                // (-dump-formats gate) keeps the compile tier — the
                // run continues dump-less at full speed.
                self.wave_engine = true;
            }
        }

        // seed the event heap (see Stepper::heap for the ordering)
        let mut heap: BinaryHeap<Reverse<(u64, u8, usize, bool)>> = BinaryHeap::new();
        for (ci, src) in sources.iter().enumerate() {
            // all edges of every waveform clock are heaped (the kernel
            // schedules every clock's edges; VCD dumps clock waveforms
            // and reset deassertion timing depends on the first negedge)
            let need_pos = true;
            let need_neg = true;
            match src {
                ClockSource::Wave(w) => {
                    let first_pos = if w.init_high { w.delay + w.lo } else { w.delay };
                    let first_neg = if w.init_high { w.delay } else { w.delay + w.hi };
                    if need_pos {
                        heap.push(Reverse((first_pos, 1, ci, true)));
                    }
                    if need_neg {
                        heap.push(Reverse((first_neg, 1, ci, false)));
                    }
                    // has_initial_value: an extra t=0 edge toward the
                    // initial value, before regular t=0 edges
                    let dir = w.init_high;
                    if w.has_init && ((dir && need_pos) || (!dir && need_neg)) {
                        heap.push(Reverse((0, 0, ci, dir)));
                    }
                }
                ClockSource::Triggered { init_high, .. } => {
                    // bk_enqueue_initial_clock_edge: one edge at t=0 in the
                    // direction of the initial value; later edges come from
                    // the driving prim
                    let dir = *init_high;
                    if (dir && need_pos) || (!dir && need_neg) {
                        heap.push(Reverse((0, 0, ci, dir)));
                    }
                }
                ClockSource::Never => {}
            }
        }

        // kernel reset protocol: top reset asserted before time 0;
        // InitialReset outputs assert from time 0 as well (reset_init)
        self.apply_reset(0, true);
        let inits = std::mem::take(&mut self.initial_asserts);
        for n in inits {
            self.apply_reset(n, true);
        }
        self.flush_reset_pending();

        // capture BEFORE planning: jit_plan takes the request and the
        // field resets to the default (Run)
        #[cfg(feature = "aot")]
        let was_emit = matches!(self.jit_request, jit::JitRequest::Emit { .. });
        #[cfg(feature = "aot")]
        let jit = self.jit_plan(&rcomps);
        #[cfg(not(feature = "aot"))]
        let jit = None;

        // RunCore sidecar v2: an Emit plan stashed the arena image and
        // stage A; append the boot-descriptor sections (clock, comp
        // order, eligibility — needs the clock state built above)
        #[cfg(feature = "aot")]
        if was_emit && self.runcore_pending.is_some() {
            self.runcore_desc_finish(&rcomps, &sources, &clocks, &driver_clock);
        }

        // TRS_REQUIRE_AOT: strict-execution contract for validation
        // runs — a design about to RUN interpreted is a hard failure,
        // never a silent degrade.  Emit requests are exempt (the link
        // caller enforces its own strictness on the emit result).
        #[cfg(feature = "aot")]
        if jit.is_none()
            && !was_emit
            && !self.debug_tier
            && std::env::var_os("TRS_REQUIRE_AOT").is_some()
        {
            eprintln!(
                "trs: TRS_REQUIRE_AOT is set but this design would run                  interpreted (TRS_JIT_TRACE=1 shows why); refusing"
            );
            std::process::exit(86);
        }

        self.stepper = Some(Stepper {
            clocks,
            sources,
            driver_clock,
            rcomps,
            heap,
            fired_this_slice: Vec::new(),
            final_now: 0,
            jit,
        });
    }

    /// Drive the event loop until $finish, the cycle limit, or event
    /// exhaustion.  Resumable: a later call with a higher limit picks up
    /// exactly where this one stopped.
    ///
    /// Multi-clock event loop: each composition fires on one (clock, edge);
    /// clock waveforms come from resolve_wave.  Same-time edges execute in
    /// clock definition order (the kernel breaks ties by clock handle
    /// index).  max_cycles counts default-clock posedges, including the
    /// in-reset edge at t=0.  Returns the exit code so far (1 iff $fatal).
    pub fn advance(&mut self, max_cycles: u64) -> i32 {
        self.advance_until(&StopCond { max_cycles, ..Default::default() })
    }

    /// advance() with the kernel's full stop machinery — the capi's
    /// bk_advance (docs/TCL-CAPI.md).  bluetcl computes ABSOLUTE
    /// targets (bk_clock_edge_count + N) for edge limits.
    pub fn advance_until(&mut self, cond: &StopCond) -> i32 {
        let max_cycles = cond.max_cycles;
        // prim-level diagnostics (fifo guard warnings, readmem
        // errors) check this thread-local — engines run sequentially
        // on one thread, so stamping per advance scopes it correctly
        prim::QUIET_ENGINE.with(|c| c.set(self.fe.quiet));
        // a $stop yield is one-shot: the next advance resumes
        self.fe.stop_request = false;
        self.prime();
        let Stepper {
            clocks,
            sources,
            driver_clock,
            rcomps,
            mut heap,
            mut fired_this_slice,
            mut final_now,
            jit,
        } = self.stepper.take().unwrap();

        #[cfg(feature = "aot")]
        let mut central_tried = false;
        // ---- central loop (task #21): tight steady-state player ----
        // A single Wave clock with fused, tick-free posedge comps and a
        // quiet aperiodic world degenerates into a plain loop: no heap,
        // no per-edge machinery.  Any irregularity bails back to the
        // general event loop below (one shared semantics).
        macro_rules! try_central {
            () => {
                #[cfg(feature = "aot")]
                'central: {
                    // hot path: no diagnostics on the already-tried check
                    if central_tried {
                        break 'central;
                    }
                    // interactive stop machinery is heap-loop-only:
                    // the central player has no per-edge bookkeeping
                    if !cond.trivial() {
                        central_tried = true;
                        break 'central;
                    }

            let Some(j) = jit.as_ref() else {
                central_tried = true;
                { CENTRAL_BAIL[2].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; }
            };
            // fusion may not exist yet (JIT compiles it once bodies
            // warm): don't burn the attempt until it does
            let Some(fused) = j.fused.get() else { { CENTRAL_BAIL[3].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; } };
            central_tried = true;
            if self.vcd.is_active()
                || !driver_clock.is_empty()
                || !self.rstgen_out.is_empty()
            {
                { CENTRAL_BAIL[4].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; }
            }
            // the initial reset pulse is TRANSIENT: AOT artifacts have
            // fused edges at t=0 while it is still in flight, and
            // burning the attempt here left the central loop
            // permanently off for every artifact run.  Un-burn and let
            // the deassert boundary retry; without generators (bailed
            // above) reset state cannot reassert, so the re-probe is
            // bounded to the few reset slices.
            if self.rst_asserted.iter().any(|&a| a) || !self.rst_pending.is_empty() {
                central_tried = false;
                { CENTRAL_BAIL[15].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; }
            }
            // exactly one periodic Wave clock
            let mut wave = None;
            for (ci2, src) in sources.iter().enumerate() {
                if let ClockSource::Wave(w) = src {
                    if wave.is_some() {
                        { CENTRAL_BAIL[5].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; }
                    }
                    wave = Some((ci2, w.hi, w.lo));
                }
            }
            let Some((wci, hi, lo)) = wave else { { CENTRAL_BAIL[6].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; } };
            if Some(clocks[wci]) != self.d.default_clock {
                { CENTRAL_BAIL[7].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; }
            }
            // posedge comps: fused, no early rules, no residual ticks;
            // negedge comps: none
            let mut pos_rcis: Vec<usize> = Vec::new();
            for (rci, rc) in rcomps.iter().enumerate() {
                if rc.clk != wci {
                    { CENTRAL_BAIL[8].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; }
                }
                // a non-rst tick disqualifies unless its work is
                // compiled into the loaded edge fns (wire clears);
                // reset ticks are no-ops here: the preconditions
                // guarantee reset stays deasserted (no generators,
                // no drivers), and rst_tick acts only in_reset
                let uncovered_tick = |rci: usize| {
                    rc.ticks.iter().enumerate().any(|(ti, (_, _, is_rst, _, _))| {
                        !*is_rst
                            && !j.covered_ticks
                                .get(rci)
                                .is_some_and(|c| c.contains(&ti))
                    })
                };
                if rc.posedge {
                    if !rc.early.is_empty()
                        || uncovered_tick(rci)
                        || fused[rci] == 0
                    {
                        // name the disqualifiers: an uncovered tick is
                        // the actionable one (which prim, which port)
                        if std::env::var_os("TRS_JIT_TRACE").is_some() {
                            for (ti, (ii, pname, is_rst, _, _)) in
                                rc.ticks.iter().enumerate()
                            {
                                if !*is_rst
                                    && !j.covered_ticks
                                        .get(rci)
                                        .is_some_and(|c| c.contains(&ti))
                                {
                                    eprintln!(
                                        "trs jit: central bail #9: uncovered \
                                         tick {} ({})",
                                        self.insts[*ii].path, pname
                                    );
                                }
                            }
                            if !rc.early.is_empty() {
                                eprintln!("trs jit: central bail #9: early rules");
                            }
                            if fused[rci] == 0 {
                                eprintln!("trs jit: central bail #9: comp {rci} not fused");
                            }
                        }
                        { CENTRAL_BAIL[9].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; }
                    }
                    pos_rcis.push(rci);
                } else if rc.entries.iter().any(|e| !e.nodes.is_empty())
                    || uncovered_tick(rci)
                {
                    // rule-less negedge comps with only covered wire
                    // clears are safe to skip entirely: wire ticks are
                    // edge-duplicated (Both ports) and nothing reads
                    // during a rule-less instant
                    { CENTRAL_BAIL[10].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; }
                }
            }
            if pos_rcis.is_empty() || j.lazy.any_cold() {
                { CENTRAL_BAIL[11].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; }
            }
            // heap must hold exactly this clock's periodic edges
            let mut t_pos = None;
            let mut t_neg = None;
            for Reverse((t, prio, ci, pos)) in heap.iter() {
                if *ci != wci || *prio == 0 {
                    { CENTRAL_BAIL[12].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; }
                }
                if *pos {
                    t_pos = Some(*t);
                } else {
                    t_neg = Some(*t);
                }
            }
            let (Some(mut tp), Some(mut tn)) = (t_pos, t_neg) else {
                { CENTRAL_BAIL[13].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; }
            };
            if tp <= 2 {
                // top reset not yet deasserted: let the general loop
                // handle early instants
                { CENTRAL_BAIL[14].fetch_add(1, std::sync::atomic::Ordering::Relaxed); break 'central; }
            }
            heap.clear();
            self.central_engaged = true;
            // link-time window bake: this instant — reset just
            // deasserted, no steady edge processed — is the state a
            // RunCore boot starts from; capture it once
            if self.runcore_bake && self.runcore_window.is_none() {
                self.runcore_window = Some((
                    self.runcore_window_encode(tp, tn),
                    prim::WINDOW_EFFECTS
                        .load(std::sync::atomic::Ordering::Relaxed),
                ));
                // the bake wants the BOUNDARY, not simulation: stop
                // the advance here — no steady cycle executes at link
                // (stop_request is cleared at the next advance; the
                // bake interp is discarded anyway)
                self.fe.stop_request = true;
            }
            // RunCore descriptor witness (desc is parsed only under
            // TRS_RUNCORE_CHECK): the engage decision and shape must
            // match the baked claim
            if let Some(d) = j.runcore_desc.as_ref() {
                // negedge comps and the full wave, for the extended
                // shape compare (panel finding: neg/delay/init were
                // baked but witnessed by nothing)
                let live_neg: Vec<usize> = rcomps
                    .iter()
                    .enumerate()
                    .filter(|(_, rc)| rc.clk == wci && !rc.posedge)
                    .map(|(i, _)| i)
                    .collect();
                let wv = match &sources[wci] {
                    ClockSource::Wave(w) => Some(*w),
                    _ => None,
                };
                if !d.central {
                    eprintln!(
                        "trs runcore: MISMATCH: central loop engaged but \
                         descriptor says central-ineligible ({})",
                        d.reason
                    );
                } else if d.hi != hi
                    || d.lo != lo
                    || d.pos != pos_rcis
                    || d.neg != live_neg
                    || wv.is_none_or(|w| {
                        (d.delay, d.init_high, d.has_init)
                            != (w.delay, w.init_high, w.has_init)
                    })
                {
                    eprintln!(
                        "trs runcore: MISMATCH: engaged hi={hi} lo={lo} \
                         pos={pos_rcis:?} vs descriptor hi={} lo={} pos={:?}",
                        d.hi, d.lo, d.pos
                    );
                } else if let Some((wa, wtp, wtn, wcyc)) = d.window.as_ref() {
                    // post-window image witness: a classic run's state
                    // at this exact instant must equal what the link
                    // baked — this is the byte-level proof that a
                    // RunCore boot starting here is indistinguishable
                    let live = unsafe {
                        std::slice::from_raw_parts(
                            self.jit_arena_ptr,
                            self.jit_arena_len,
                        )
                    };
                    if (*wtp, *wtn, *wcyc) != (tp, tn, self.cycle) {
                        eprintln!(
                            "trs runcore: MISMATCH: window state tp/tn/cycle \
                             {wtp}/{wtn}/{wcyc} vs live {tp}/{tn}/{}",
                            self.cycle
                        );
                    } else if let Some(k) =
                        (0..live.len()).find(|&k| live[k] != wa[k])
                    {
                        eprintln!(
                            "trs runcore: MISMATCH: window slot {k}: baked \
                             {:#x}, live {:#x}",
                            wa[k], live[k]
                        );
                    } else if std::env::var_os("TRS_STARTUP_TIME").is_some() {
                        eprintln!(
                            "trs runcore: descriptor + window MATCH \
                             (central engage)"
                        );
                    }
                } else if std::env::var_os("TRS_STARTUP_TIME").is_some() {
                    eprintln!("trs runcore: descriptor MATCH (central engage)");
                }
            }
            if std::env::var_os("TRS_JIT_TRACE").is_some() {
                eprintln!("trs jit: central loop engaged (clock {wci})");
            }
            let period = hi + lo;
            let ap = j.arena_ptr();
            let envp = self as *mut Interp as *mut core::ffi::c_void;
            let cycles0 = self.cycle;
            let mut fin_break = false;
            let mut vcd_yield = false;
            while self.fe.finished.is_none()
                && !self.fe.stop_request
                && self.cycle < max_cycles
            {
                self.cycle += 1;
                self.now = tp;
                final_now = tp;
                for &rci in &pos_rcis {
                    let f: unsafe extern "C" fn(
                        *mut u64,
                        *mut core::ffi::c_void,
                        u64,
                    ) -> i32 = unsafe { std::mem::transmute(fused[rci]) };
                    unsafe { f(ap, envp, tp) };
                    // NO finished break here: $finish completes the
                    // instant's edge schedules
                }
                // $finish/$stop stop ON this posedge: break BEFORE
                // tp/tn advance so the exit bookkeeping and re-armed
                // heap see the companion negedge as PENDING, exactly
                // like the general loop's state at a yield (the
                // fleet: crediting it made oracle edge compares
                // diverge)
                if self.fe.finished.is_some() || self.fe.stop_request {
                    fin_break = true;
                    break;
                }
                // a compiled $dumpvars/$dumpon armed the dump mid-
                // slice: yield to the general loop, whose per-slice
                // vcd_event takes over (and whose is_active probe
                // blocks re-engagement).  The arming instant's own
                // event fires below AFTER the clock bookkeeping — the
                // reference writes the whole arming sequence
                // (unstamped initial values, the time marker, the
                // $dumpvars task, the checkpoint) in that one event.
                // Cost on the hot path: three flag loads per posedge
                // (the transition-tax budget).
                if self.vcd.is_active() {
                    vcd_yield = true;
                    break;
                }
                if !self.rst_pending.is_empty() || !self.rstgen_out.is_empty() {
                    break;
                }
                tp += period;
                tn += period;
            }
            // clock bookkeeping for queries + re-arm the heap so the
            // general loop (and later advance() calls) resume cleanly
            {
                let k = self.cycle - cycles0;
                let c = &mut self.vcd_clocks[wci];
                c.pos_at = self.now;
                c.pos_count = self.cycle;
                c.cur = true;
                // negedges pass silently inside the central player
                // (no negedge comps by precondition); keep the counts
                // coherent for later bk queries.  On a $finish exit
                // the finish posedge's companion negedge has NOT
                // retired — the general loop leaves it pending —
                // and on both exit shapes the last RETIRED negedge
                // is tn - period (tp/tn advance only on completed
                // iterations).
                // the vcd yield credits the lagging companion negedge
                // too (it was time-passed silently, like every negedge
                // inside the player): the general loop must resume at
                // its SUCCESSOR, or a stale past-time negedge event
                // writes a time-disordered clock line into the dump
                let done = if fin_break {
                    k.saturating_sub(1)
                } else {
                    k
                };
                if done > 0 {
                    c.neg_count += done;
                    c.neg_at = tn - period;
                }
                if vcd_yield {
                    c.neg_count += 1;
                    c.neg_at = tn;
                }
            }
            // on a yield exit tp still names the EXECUTED posedge —
            // re-arming it verbatim would re-run that edge on a
            // $stop resume; its successor is the pending one
            let tp_pend =
                if fin_break || vcd_yield { tp + period } else { tp };
            let tn_pend = if vcd_yield { tn + period } else { tn };
            heap.push(Reverse((tp_pend, 1, wci, true)));
            heap.push(Reverse((tn_pend, 1, wci, false)));
            if vcd_yield {
                // the arming slice's full dump sequence, with the
                // clock bookkeeping above already in place
                self.vcd_event(tp);
            }
        
                }
            };
        }

        while self.fe.finished.is_none() && !self.fe.stop_request {
            let Some(Reverse((t, prio, ci, pos))) = heap.pop() else { break };
            // top reset deasserts at t=2 after that instant's logic
            if t > 2 && self.rst_asserted[0] {
                self.apply_reset(0, false);
                self.flush_reset_pending();
                // steady state begins here: push the in-flight edge
                // back and try the central player once
                heap.push(Reverse((t, prio, ci, pos)));
                try_central!();
                continue;
            }
            if let Some(p) = &cond.progress {
                p.store(t, std::sync::atomic::Ordering::Relaxed);
            }
            // bk_abort_now: end-of-cycle stop — the finished slice is
            // complete, the popped edge waits for the next advance.
            // Same-instant events still run (t == now): stopping mid-
            // instant would leave a state no deterministic re-run can
            // reach (the oracle catch-up replays whole slices)
            if t != self.now
                && cond
                    .abort
                    .as_ref()
                    .is_some_and(|a| a.load(std::sync::atomic::Ordering::Relaxed))
            {
                heap.push(Reverse((t, prio, ci, pos)));
                break;
            }
            // bk_quit_at / UI events: every timeslice <= tq is done
            // once the next event lies beyond tq; the kernel's UI
            // callback is an event AT tq, so time advances to tq
            if let Some(&tq) = cond.at_times.iter().filter(|&&tq| t > tq).min()
            {
                self.now = self.now.max(tq);
                final_now = final_now.max(tq);
                heap.push(Reverse((t, prio, ci, pos)));
                break;
            }
            // bk_quit_after_edge: a reached limit refuses the next
            // same-direction edge (resume-safe, like the cycle limit)
            if cond.edge_limits.iter().any(|&(lci, ldir, lim)| {
                lci == ci
                    && ldir == pos
                    && (if pos {
                        self.vcd_clocks[ci].pos_count
                    } else {
                        self.vcd_clocks[ci].neg_count
                    }) >= lim
            }) {
                final_now = t;
                heap.push(Reverse((t, prio, ci, pos)));
                break;
            }
            if pos && Some(clocks[ci]) == self.d.default_clock {
                if self.cycle >= max_cycles {
                    final_now = t;
                    // put the unprocessed edge back for a later advance()
                    // with a higher cycle limit
                    heap.push(Reverse((t, prio, ci, pos)));
                    break;
                }
                self.cycle += 1;
            }
            self.now = t;
            final_now = t;
            // clock edge bookkeeping (run_edge_schedule_event):
            // combinational_at = previous same-direction edge time
            {
                let c = &mut self.vcd_clocks[ci];
                if pos {
                    self.vcd.clk_combinational[ci] = c.pos_at;
                    c.pos_at = t;
                    c.pos_count += 1;
                } else {
                    self.vcd.clk_combinational[ci] = c.neg_at;
                    c.neg_at = t;
                    c.neg_count += 1;
                }
                if c.first_edge.is_none() {
                    c.first_edge = Some(t);
                }
                c.cur = pos;
            }

            for (rci, rc) in rcomps
                .iter()
                .enumerate()
                .filter(|(_, r)| r.clk == ci && r.posedge == pos)
            {
                fired_this_slice.push(rci);

                // live clock-level updates before rules run (the kernel
                // flips a clock's value before executing its schedule;
                // GatedClock's transparent-low latch queries it)
                for (inst, pname, is_rst, _, _) in &rc.ticks {
                    if *is_rst {
                        continue;
                    }
                    if let InstKind::Prim(p) = &mut self.insts[*inst].kind {
                        p.clock_level(pname, pos);
                    }
                }

                // compiled dispatch: the whole composition runs as native
                // Sched/Exec functions over the arena (no latch space —
                // fire signals and schedule-position defs live in slots)
                #[cfg(feature = "aot")]
                let mut _ran_fused = false;
                #[cfg(feature = "aot")]
                let ran_jit = match &jit {
                    Some(j) => match &j.comp_nodes[rci] {
                        Some(nodes) => {
                            let ap = j.arena_ptr();
                            // latch space clears per edge UNCONDITIONALLY
                            // and for BOTH dispatch arms (fused and node
                            // walk): warming fallback bodies latch state as
                            // they run, and the PG_FINAL early-rule pass
                            // latches CF/WF — a surviving latch would shadow
                            // both the arena fall-through and recomputation
                            // on the NEXT timeslice (latched() wins in
                            // eval), freezing an early rule's first fire
                            // decision forever.  Review round 2 found the
                            // first fix covered only the node-walk arm while
                            // AOT runs fuse from the first slice.
                            for i in 0..self.insts.len() {
                                if let InstKind::User { latched, .. } =
                                    &mut self.insts[i].kind
                                {
                                    latched.clear();
                                }
                            }
                            // fused fast path (task #17): the whole
                            // edge as one compiled call — the schedule
                            // promoted from data to code.  The node
                            // walk remains for warm-up and fallback.
                            if j.fused.get().is_none() && !j.lazy.any_cold() {
                                j.try_fuse();
                            }
                            let fp = j.fused.get().map(|fs| fs[rci]).unwrap_or(0);
                            if fp != 0 {
                                let f: unsafe extern "C" fn(
                                    *mut u64,
                                    *mut core::ffi::c_void,
                                    u64,
                                ) -> i32 = unsafe { std::mem::transmute(fp) };
                                let envp =
                                    self as *mut Interp as *mut core::ffi::c_void;
                                unsafe { f(ap, envp, t) };
                                _ran_fused = true;
                                true
                            } else {
                            // ConfigReg reads compare written_at to now
                            unsafe { *ap.add(j.now_slot as usize) = t };
                            // the C++ schedule zeroes every enable at the
                            // top of the pass; compiled call sites set them
                            for &s in &j.en_slots {
                                unsafe { *ap.add(s as usize) = 0 };
                            }
                            let envp = self as *mut Interp as *mut core::ffi::c_void;
                            let _dt0 = jit::prof::on().then(std::time::Instant::now);
                            for n in nodes {
                                match *n {
                                    jit::JitNode::Sched(ord) => {
                                        let f = j.lazy.scheds[ord as usize].sched;
                                        unsafe { f(ap, envp) }
                                    }
                                    jit::JitNode::Exec(ord) => {
                                        match j.lazy.exec(ord as usize) {
                                            Some(ce) => {
                                                let f = ce.exec;
                                                let (b, tb) =
                                                    j.lazy.exec_args[ord as usize];
                                                unsafe {
                                                    f(ap, envp, b, tb);
                                                }
                                            }
                                            None => {
                                                // cold body: the native
                                                // sched wrote the WF slot;
                                                // interpret the body if set
                                                let (inst, rname, wf_slot) =
                                                    j.exec_fallback[ord as usize];
                                                let wf = unsafe {
                                                    *ap.add(wf_slot as usize)
                                                };
                                                if wf != 0 {
                                                    self.exec_rule_forced(inst, rname);
                                                }
                                            }
                                        }
                                    }
                                }
                                // NO finished break: $finish completes
                                // the in-flight edge schedule (see
                                // exec_stmt) — the walk runs every node
                            }
                            if let Some(t0) = _dt0 {
                                jit::prof::add(&jit::prof::DISPATCH_NS, t0);
                            }
                            true
                            }
                        }
                        None => false,
                    },
                    _ => false,
                };
                #[cfg(not(feature = "aot"))]
                let ran_jit = false;

                if !ran_jit {
                    // fresh latch space for this edge
                    for i in 0..self.insts.len() {
                        if let InstKind::User { latched, .. } = &mut self.insts[i].kind {
                            latched.clear();
                        }
                    }
                    // auto-fired methods whose Exec cut precedes every
                    // node-bearing top segment run first
                    if !self.autofire.is_empty() {
                        if let Some(idxs) = self.autofire_pre.get(&rci).cloned()
                        {
                            for mi in idxs {
                                let (m, argv) = self.autofire[mi].clone();
                                self.call_action(0, m, &argv);
                            }
                        }
                    }
                    for (ei, en) in rc.entries.iter().enumerate() {
                        let inst = en.inst;
                        // this entry's schedule-position cone defs: computed
                        // eagerly here like the C++ schedule function — side
                        // effects (hoisted prim value-method calls) included
                        for &dn in &en.eager {
                            let mut c = Ctx::default();
                            let v = self.eval(inst, &mut c, &Expr::Def(dn));
                            self.set_latched(inst, dn, v);
                        }
                        for &node in &en.nodes {
                            // clock-crossing rules run in the after-edge pass
                            let r0 = match node {
                                SchedNode::Sched(r) | SchedNode::Exec(r) => r,
                            };
                            if rc.early.contains(&(inst, r0)) {
                                continue;
                            }
                            match node {
                                SchedNode::Sched(r) => {
                                    let ci2 =
                                        rc.cross.get(&(inst, r)).cloned().unwrap_or_default();
                                    self.latch_rule(inst, r, &ci2);
                                }
                                SchedNode::Exec(r) => self.exec_rule(inst, r),
                            }
                        }
                        // batch auto-fire: always_enabled top methods
                        // execute at their cut position — after this
                        // entry's nodes (EN reads constant 1 via the
                        // top params; call_action's check_rdy guards
                        // each fire; $finish semantics as for rules)
                        if !self.autofire_at.is_empty() {
                            if let Some(idxs) =
                                self.autofire_at.get(&(rci, ei)).cloned()
                            {
                                for mi in idxs {
                                    let (m, argv) = self.autofire[mi].clone();
                                    self.call_action(0, m, &argv);
                                }
                            }
                        }
                    }
                }

                // end-of-edge ticks (reset ticks are conditional: the
                // prim itself checks its reset line)
                #[cfg(feature = "aot")]
                let _tt0 = jit::prof::on().then(std::time::Instant::now);
                for (_ti, (inst, pname, is_rst, owner, gexpr)) in
                    rc.ticks.iter().enumerate()
                {
                    let inst = *inst;
                    // steady state: a reset tick is a no-op unless some
                    // reset node is asserted (rst_tick acts only
                    // in_reset); generators/drivers keep side duties
                    if *is_rst
                        && self.rst_active == 0
                        && !self.rstgen_out.contains_key(&inst)
                        && !driver_clock.contains_key(&inst)
                    {
                        continue;
                    }
                    // wire ticks compiled INTO the fused edge fn that
                    // just ran (edge-SSA artifacts): already done
                    #[cfg(feature = "aot")]
                    if _ran_fused {
                        if let Some(j) = jit.as_ref() {
                            if j.covered_ticks
                                .get(rci)
                                .is_some_and(|c| c.contains(&_ti))
                            {
                                continue;
                            }
                        }
                    }
                    let gate = match gexpr {
                        None => true,
                        Some(g) => {
                            let mut c = Ctx::default();
                            self.eval(*owner, &mut c, g).as_bool()
                        }
                    };
                    if let InstKind::Prim(p) = &mut self.insts[inst].kind {
                        if *is_rst {
                            if gate {
                                p.rst_tick(t);
                            }
                        } else {
                            p.tick(pname, t, pos, gate);
                        }
                    }
                    if self.rstgen_out.contains_key(&inst) {
                        self.poll_rstgen(inst);
                    }
                    // clock-generating prims trigger output edges at the
                    // current instant
                    if let Some(&out_ci) = driver_clock.get(&inst) {
                        let edges = if let InstKind::Prim(p) = &mut self.insts[inst].kind {
                            p.take_clock_edges()
                        } else {
                            Vec::new()
                        };
                        for pos_edge in edges {
                            if self.trace_clk {
                                eprintln!("[t={t}] trigger clk={out_ci} pos={pos_edge}");
                            }
                            heap.push(Reverse((t, 1, out_ci, pos_edge)));
                        }
                    }
                }
                #[cfg(feature = "aot")]
                if let Some(t0) = _tt0 {
                    jit::prof::add(&jit::prof::TICK_NS, t0);
                }
            }

            // regular waveform edges self-reschedule one period out;
            // initial edges are one-shot (kernel: PG_INITIAL doesn't repeat)
            if prio != 0 {
                if let ClockSource::Wave(w) = &sources[ci] {
                    heap.push(Reverse((t + w.hi + w.lo, 1, ci, pos)));
                }
            }

            // end of timeslice: apply deferred reset transitions, then run
            // the after-edge pass (clock-crossing "early" rules sample
            // post-edge, post-reset state — kernel PG_FINAL, after
            // PG_AFTER_LOGIC reset flushing)
            let same_time = matches!(heap.peek(), Some(Reverse((nt, _, _, _))) if *nt == t);
            if !same_time {
                self.flush_reset_pending();
                // per-timeslice VCD event (PG_AFTER_LOGIC, before the
                // PG_FINAL early-rule pass)
                if self.vcd.is_active() {
                    self.vcd_event(t);
                }
                for rci in std::mem::take(&mut fired_this_slice) {
                    let rc = &rcomps[rci];
                    if rc.early.is_empty() {
                        continue;
                    }
                    for en in &rc.entries {
                        let inst = en.inst;
                        for &node in &en.nodes {
                            // $finish completes the EDGE SCHEDULE but
                            // the kernel's yield preempts the LATER
                            // same-instant events — the PG_FINAL
                            // early-rule pass does not run post-finish
                            // (sysFWrite3: 4 extra $fwrite lines when
                            // it did)
                            if self.fe.finished.is_some() {
                                break;
                            }
                            let r0 = match node {
                                SchedNode::Sched(r) | SchedNode::Exec(r) => r,
                            };
                            if !rc.early.contains(&(inst, r0)) {
                                continue;
                            }
                            match node {
                                SchedNode::Sched(r) => {
                                    let ci2 = rc
                                        .cross
                                        .get(&(inst, r))
                                        .cloned()
                                        .unwrap_or_default();
                                    self.latch_rule(inst, r, &ci2);
                                }
                                SchedNode::Exec(r) => self.exec_rule(inst, r),
                            }
                        }
                    }
                }
            }

            // steady state may only begin here (fusion compiles after
            // warm-up): retry the central player at slice boundaries
            if !same_time
                && self.fe.finished.is_none()
                && !self.fe.stop_request
                && self.cycle < max_cycles
            {
                try_central!();
            }
            // the cycle limit stops the simulation at the Nth default
            // posedge, but the kernel finishes the current timeslice
            // first — same-instant edges of other clocks still run;
            // events at later times do not
            let edge_hit = cond.edge_limits.iter().any(|&(lci, ldir, lim)| {
                (if ldir {
                    self.vcd_clocks[lci].pos_count
                } else {
                    self.vcd_clocks[lci].neg_count
                }) >= lim
            });
            if (self.cycle >= max_cycles || edge_hit) && !same_time {
                break;
            }
        }
        // RunCore descriptor witness, inverse half: a batch run that
        // FINISHED without the central loop ever engaging contradicts
        // a baked eligible=1 claim (desc is parsed only under
        // TRS_RUNCORE_CHECK; wave runs and interactive stop conditions
        // never engage by design, so they are out of scope)
        #[cfg(feature = "aot")]
        if cond.trivial()
            && self.fe.finished.is_some()
            && !self.central_engaged
            && !self.wave_engine
            && !self.vcd.is_active()
            // a run that finished inside the reset window (t <= 2)
            // never reaches the engage point — legitimately
            && self.now > 2
        {
            if let Some(d) =
                jit.as_ref().and_then(|j| j.runcore_desc.as_ref())
            {
                if d.central {
                    eprintln!(
                        "trs runcore: MISMATCH: descriptor says central-\
                         eligible but the central loop never engaged"
                    );
                }
            }
        }
        self.stepper = Some(Stepper {
            clocks,
            sources,
            driver_clock,
            rcomps,
            heap,
            fired_this_slice,
            final_now,
            jit,
        });
        if self.fe.fataled { 1 } else { 0 }
    }

    /// End-of-simulation epilogue (bk_shutdown's VCD side): finish an
    /// interrupted timeslice's VCD dump ($finish while same-time edges
    /// were still pending), then flush buffered changes strictly before
    /// the stop time (vcd_reset).  Separate from advance() so bounded
    /// stepping never emits the final flush early.  Returns the process
    /// exit code — bluesim.tcl exits 1 iff $fatal was called; $finish
    /// status codes do not surface as process exit codes.
    pub fn finish(&mut self) -> i32 {
        let (fired, final_now) = match &self.stepper {
            Some(st) => (!st.fired_this_slice.is_empty(), st.final_now),
            None => (false, 0),
        };
        if self.vcd.is_active() && fired {
            self.vcd_event(self.now);
        }
        self.vcd.set_final_min_pending(final_now);
        self.vcd.flush_all_pending();
        if self.fe.fataled { 1 } else { 0 }
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

/// dollar_display's Target collects errors with push_front and prints
/// them after the task output: "Output error: <msg>", newest first.
pub(crate) fn emit_output_errors(errs: &[String]) {
    // quiet oracle engines suppress these like every output sink
    // ($fdisplay-family arms reach here even when write_fd is gated)
    if prim::quiet_engine() {
        if !errs.is_empty() {
            prim::note_window_effect();
        }
        return;
    }
    for e in errs.iter().rev() {
        print!("Output error: {e}");
    }
}

fn cookie_key(cookie: u32) -> StrId {
    // cookies live in a synthetic key space far above real string ids
    0x8000_0000 | cookie
}

/// Outcome of a trs link Emit request.
pub enum AotEmit {
    /// artifact .so written; the wrapper should pass --code
    Compiled,
    /// design can't run in compiled mode (reason) — the artifact is
    /// still valid, it just runs interpreted (no --code)
    Ineligible(String),
    /// infrastructure failure (LLVM, cc, IO): link must fail
    Failed(String),
}

/// FNV-1a over the .bir bytes: the fingerprint baked into AOT
/// artifacts and checked at load (the impl lives in trs-ir so
/// snapshots can checksum their payload with the same function).
pub fn bir_fingerprint(bytes: &[u8]) -> u64 {
    ir::fnv1a(bytes)
}

#[cfg(feature = "aot")]
impl Interp {
    /// trs link: make prime() emit the artifact .so instead of
    /// setting up a run.
    pub fn aot_request_emit(&mut self, so: std::path::PathBuf) {
        self.jit_request = jit::JitRequest::Emit { so, exe: None };
    }

    /// trs link --exe: after the artifact .so, also link a
    /// self-contained executable (design objects + a main shim,
    /// --export-dynamic, against libtrs_capi.so in `libdir`).
    pub fn aot_request_emit_exe(
        &mut self,
        so: std::path::PathBuf,
        exe: std::path::PathBuf,
        libdir: std::path::PathBuf,
    ) {
        self.jit_request = jit::JitRequest::Emit { so, exe: Some((exe, libdir)) };
    }

    /// trs run --code: resolve compiled functions from the artifact.
    pub fn aot_request_code(&mut self, so: std::path::PathBuf) {
        self.jit_request =
            jit::JitRequest::Load { src: jit::ArtifactSource::Path(so) };
    }

    /// Take the RunCore arena image encoded by an Emit plan; the
    /// linker CLI writes it beside the artifact as `<base>.arena`
    /// (see jit — RunCore sidecar, validation form).
    pub fn take_runcore_image(&mut self) -> Option<Vec<u8>> {
        self.runcore_pending.take()
    }

    /// Artifact-as-executable: the design objects are linked into THIS
    /// process image — resolve compiled functions from ourselves.
    pub fn aot_request_code_self(&mut self) {
        self.jit_request =
            jit::JitRequest::Load { src: jit::ArtifactSource::This };
    }

    /// Outcome of an Emit request (valid after prime()).
    pub fn aot_take_emit_result(&mut self) -> Option<AotEmit> {
        self.jit_emit_result.take()
    }
}
#[cfg(not(feature = "aot"))]
impl Interp {
    pub fn aot_request_emit(&mut self, _so: std::path::PathBuf) {}
    pub fn aot_request_emit_exe(
        &mut self,
        _so: std::path::PathBuf,
        _exe: std::path::PathBuf,
        _libdir: std::path::PathBuf,
    ) {
    }
    pub fn aot_request_code_self(&mut self) {}
    pub fn take_runcore_image(&mut self) -> Option<Vec<u8>> {
        None
    }
    pub fn aot_request_code(&mut self, _so: std::path::PathBuf) {
        // the strict contract holds even without the aot feature: a
        // run that MUST be compiled cannot silently interpret
        if std::env::var_os("TRS_REQUIRE_AOT").is_some() {
            eprintln!(
                "trs: TRS_REQUIRE_AOT is set but this trs was built \
                 without artifact support (feature `aot`); refusing"
            );
            std::process::exit(86);
        }
        eprintln!(
            "trs: warning: built without JIT/AOT support; \
             --code ignored (running interpreted)"
        );
    }
    pub fn aot_take_emit_result(&mut self) -> Option<AotEmit> {
        // Ineligible, NOT Failed: a lean binary legitimately produces
        // interpreted artifacts ("only infrastructure failures fail
        // the link") — Failed hard-failed both `link` and
        // `link --interactive` on the lean product (review fleet)
        Some(AotEmit::Ineligible(
            "this trs was built without JIT/AOT support (feature `jit`)".into(),
        ))
    }
}

impl Interp {
    /// Debug-tier def recording: last-computed def values retained
    /// (Bluesim's C++ member semantics) so symbol peeks see what the
    /// simulation computed, not a fresh re-evaluation.
    pub fn set_sym_trace(&mut self) {
        self.vcd_trace = true;
    }

    /// Secondary oracle engine: suppress every output sink (console,
    /// design files, VCD) while state effects run normally.
    pub fn set_quiet(&mut self) {
        self.fe.quiet = true;
    }

    /// Mark this interp as a DEBUG-tier engine (the bluetcl capi).
    /// TRS_REQUIRE_AOT polices the fast artifact's execution contract;
    /// the interactive tier runs interp/jit BY DESIGN (introspection
    /// needs the interp engine's recording), so its engines are exempt
    /// from the strict-mode refusal.
    pub fn set_debug_tier(&mut self) {
        self.debug_tier = true;
    }

    /// Oracle divergence protocol (docs/TCL-CAPI.md): flip the fatal
    /// flag so scripts stop AT the divergence (bluesim.tcl exits 1
    /// via `sim isfatal`).  fatal implies finished in the reference
    /// ($fatal = message + bk_fatal_now), so latch both — a later
    /// `sim step` must refuse like any post-$finish step.
    pub fn mark_fatal(&mut self) {
        self.fe.fataled = true;
        self.fe.finished = Some(1);
    }

    // ===============
    // VCD-under-Tcl (trs-capi): the bk_* VCD controls route to the
    // same writer the $dump* tasks use.  Recording (vcd_trace) is
    // already on for capi interp engines (set_sym_trace at bk_init),
    // so mid-session enables see live values, like the reference.

    /// bk_set_VCD_file (vcd.cxx:36): None closes the file (success).
    pub fn vcd_set_file(&mut self, name: Option<&str>) -> Result<(), ()> {
        match name {
            Some(n) => self.vcd.set_file(n),
            None => {
                self.vcd.close_file();
                Ok(())
            }
        }
    }

    /// bk_enable_VCD_dumping: true iff dumping is now enabled.
    pub fn vcd_enable(&mut self) -> bool {
        self.vcd.enable()
    }

    /// bk_disable_VCD_dumping.
    pub fn vcd_disable(&mut self) {
        self.vcd.disable()
    }

    /// bk_get_VCD_file_name: the reference returns the C++ string's
    /// c_str() — "" when no file has been set, never NULL.
    pub fn vcd_file_name(&self) -> &str {
        self.vcd.file_name()
    }

    /// bk_set_waveform_format's engine half (the capi validates the
    /// string): a same-format set is a no-op, otherwise any dump in
    /// progress ends and the file closes.
    pub fn wave_set_format(&mut self, fmt: WaveFormat) -> bool {
        let now = self.now;
        if !self.vcd.set_format(fmt, now) {
            return false;
        }
        // FST recording needs the same def/method traces as VCD
        self.vcd_trace = true;
        true
    }

    /// bk_get_waveform_format.
    pub fn wave_format(&self) -> WaveFormat {
        self.vcd.format()
    }

    /// -dump-formats plumbing: which waveform writers this model
    /// carries.  `none` also turns recording off entirely — the model
    /// is the untraced fast artifact and can never start dumping.
    pub fn set_allowed_wave_formats(&mut self, vcd: bool, fst: bool) {
        self.vcd.set_allowed(vcd, fst);
        if !vcd && !fst {
            self.vcd_trace = false;
        }
    }

    /// Batch driver (+bscvcd / +bscfst): stage a waveform request
    /// consumed at the stepper build; file None = format default.
    pub fn wave_request(&mut self, fmt: WaveFormat, file: Option<String>) {
        self.wave_pending = Some((fmt, file));
    }

    /// Symbol-tree seed (trs-capi): per instance, (parent instance,
    /// local name, is-user-module).  Parents derive from paths; the
    /// root's local name is "" (the kernel top_symbol key).
    pub fn symbol_seed(&self) -> Vec<(Option<usize>, String, bool)> {
        let by_path: HashMap<&str, usize> = self
            .insts
            .iter()
            .enumerate()
            .map(|(i, n)| (n.path.as_str(), i))
            .collect();
        self.insts
            .iter()
            .enumerate()
            .map(|(i, n)| {
                let (parent, name) = match n.path.rfind('.') {
                    Some(k) => (
                        by_path.get(&n.path[..k]).copied(),
                        n.path[k + 1..].to_string(),
                    ),
                    // top-level children carry dot-less paths; only
                    // instance 0 is the true root (key "")
                    None if i == 0 => (None, String::new()),
                    None => (Some(0), n.path.clone()),
                };
                (parent, name, matches!(n.kind, InstKind::User { .. }))
            })
            .collect()
    }

    /// Instantiation parameters of a user-module instance
    /// (SYM_PARAM symbols): (name, bound value).
    pub fn inst_params(&self, i: usize) -> Vec<(String, Value)> {
        match &self.insts[i].kind {
            InstKind::User { params, .. } => {
                let mut v: Vec<(String, Value)> = params
                    .iter()
                    .map(|(n, val)| (self.s(*n).to_string(), val.clone()))
                    .collect();
                v.sort_by(|a, b| a.0.cmp(&b.0));
                v
            }
            _ => Vec::new(),
        }
    }

    /// Method-port symbols of a user-module instance (SYM_PORT):
    /// EN_<m> for action-kind methods, argument ports, RDY_<m>, and
    /// the result port named after value/AV methods.
    pub fn method_port_symbols(
        &self,
        i: usize,
    ) -> Vec<(String, u32, StrId, MethPortKind)> {
        let InstKind::User { module, .. } = &self.insts[i].kind else {
            return Vec::new();
        };
        let mir = self.mods[*module].ir;
        let mut out = Vec::new();
        for m in &self.d.modules[mir].methods {
            let mname = self.s(m.name).to_string();
            if m.kind != trs_ir::MethodKind::Value {
                out.push((
                    format!("EN_{mname}"),
                    1,
                    m.name,
                    MethPortKind::En,
                ));
            }
            for (k, a) in m.args.iter().enumerate() {
                out.push((
                    self.s(a.name).to_string(),
                    a.width.max(1),
                    m.name,
                    MethPortKind::Arg(k),
                ));
            }
            // const-true ready = always_ready: the reference has no
            // RDY port to register (interim until the exporter carries
            // the surviving methodPorts set)
            if !matches!(m.ready, Some(Expr::Const { .. }) | None) {
                out.push((format!("RDY_{mname}"), 1, m.name, MethPortKind::Rdy));
            }
            if m.result.is_some() {
                let w = match m.result.as_ref().unwrap() {
                    Expr::Def(dn) => self.d.modules[mir]
                        .defs
                        .iter()
                        .find(|d| d.name == *dn)
                        .map(|d| d.width)
                        .unwrap_or(1),
                    e => e.width().max(1),
                };
                out.push((mname, w.max(1), m.name, MethPortKind::Result));
            }
        }
        out
    }

    /// Peek a method port (member semantics: EN latched per pass,
    /// args persist from the last call, RDY/result evaluate against
    /// the settled state at the stop).
    pub fn method_port_peek(
        &mut self,
        i: usize,
        method: StrId,
        kind: MethPortKind,
        width: u32,
    ) -> Value {
        let InstKind::User { module, .. } = &self.insts[i].kind else {
            return Value::zero(width.max(1));
        };
        let module = *module;
        let mir = self.mods[module].ir;
        match kind {
            MethPortKind::En => {
                let en = format!("EN_{}", self.s(method));
                let id = self.d.strings.iter().position(|x| x == &en);
                let mut set = match (&self.insts[i].kind, id) {
                    (InstKind::User { latched, .. }, Some(id)) => {
                        latched.contains_key(&(id as StrId))
                    }
                    _ => false,
                };
                // compiled call sites store the arena EN word, not the
                // boxed latch map
                if !set && !self.jit_arena_ptr.is_null() {
                    if let Some(id) = id {
                        if let Some(&slot) = self.jit_en_slots.get(&(i, id as StrId))
                        {
                            set = unsafe { *self.jit_arena_ptr.add(slot as usize) }
                                != 0;
                        }
                    }
                }
                Value::from_u64(1, set as u64)
            }
            MethPortKind::Arg(k) => {
                if !self.jit_arena_ptr.is_null() {
                    if let Some(rs) = self.jit_rec_meths.get(&(i, method)) {
                        if let Some(&(base, w)) = rs.args.get(k) {
                            return self.rec_read(base, w);
                        }
                    }
                }
                self.vcd_meth_calls
                    .get(&(i, method))
                    .and_then(|(_, argv)| argv.get(k).cloned())
                    .map(|mut v| {
                        v.width = v.width.max(1);
                        v
                    })
                    .unwrap_or_else(|| Value::zero(width.max(1)))
            }
            MethPortKind::Result => {
                // PORT_<result> is a MEMBER: zero until the method's
                // first invocation, then the last returned value
                // (METH_result writes the port on call) — the
                // vcd_meth_results recording is exactly that
                if !self.jit_arena_ptr.is_null() {
                    if let Some(rs) = self.jit_rec_meths.get(&(i, method)) {
                        if let Some((base, w)) = rs.res {
                            return self.rec_read(base, w);
                        }
                    }
                }
                self.vcd_meth_results
                    .get(&(i, method))
                    .cloned()
                    .unwrap_or_else(|| Value::zero(width.max(1)))
            }
            MethPortKind::Rdy => {
                let mi = match self.mods[module].methods.get(&method) {
                    Some(&mi) => mi,
                    None => return Value::zero(width.max(1)),
                };
                let m = &self.d.modules[mir].methods[mi];
                let mut e = m.ready.clone();
                // the exported ready pred can reference the
                // PRE-block-conversion name (Def(RDY_<m>)) that no
                // def table carries; the reference resolves it to
                // the method's CAN_FIRE def (mkGCD.cxx:
                // PORT_RDY_result = DEF_CAN_FIRE_result)
                if let Some(Expr::Def(dn)) = &e {
                    if !self.mods[module].defs.contains_key(dn) {
                        let cf = format!("CAN_FIRE_{}", self.s(method));
                        if let Some(id) =
                            self.d.strings.iter().position(|x| x == &cf)
                        {
                            if self.mods[module].defs.contains_key(&(id as StrId))
                            {
                                e = Some(Expr::Def(id as StrId));
                            }
                        }
                    }
                }
                match e {
                    Some(e) => {
                        let mut ctx = Ctx::default();
                        self.eval(i, &mut ctx, &e)
                    }
                    None => Value::from_u64(1, 1),
                }
            }
        }
    }

    /// Rule names of a user-module instance (SYM_RULE symbols).
    pub fn inst_rules(&self, i: usize) -> Vec<String> {
        match &self.insts[i].kind {
            InstKind::User { module, .. } => {
                let mir = self.mods[*module].ir;
                self.d.modules[mir]
                    .rules
                    .iter()
                    .map(|r| self.s(r.name).to_string())
                    .collect()
            }
            _ => Vec::new(),
        }
    }

    /// Def symbols of a user-module instance: (name, width, id).
    pub fn def_symbols(&self, i: usize) -> Vec<(String, u32, StrId)> {
        match &self.insts[i].kind {
            InstKind::User { module, .. } => {
                let mir = self.mods[*module].ir;
                self.d.modules[mir]
                    .defs
                    .iter()
                    .filter(|d| d.props.sym)
                    .map(|d| (self.s(d.name).to_string(), d.width, d.name))
                    .collect()
            }
            _ => Vec::new(),
        }
    }

    /// Last-computed value of a def (set_sym_trace recording); zeros
    /// until first computed, like the reference's member fields.
    /// Traced-plan engines record into arena slots (the single
    /// authority when present — compiled bodies store them inline),
    /// so the slot is read first, like the VCD writer.
    pub fn def_peek(&self, i: usize, d: StrId) -> Option<Value> {
        if !self.jit_arena_ptr.is_null() {
            if let Some(&(base, w)) = self.jit_rec_defs.get(&(i, d)) {
                return Some(self.rec_read(base, w));
            }
        }
        self.vcd_def_vals.get(&(i, d)).cloned()
    }

    /// Debug-tier prim sub-symbols (the reference's per-prim
    /// init_symbols tables).
    pub fn prim_sym_children(&self, i: usize) -> Vec<prim::PrimSym> {
        match &self.insts[i].kind {
            InstKind::Prim(p) => p.sym_children(),
            _ => Vec::new(),
        }
    }

    /// Oracle-compare state surface: a SUPERSET of prim_sym_children
    /// (prims the reference leaves symbol-less — Counter, CReg —
    /// expose their architectural value here without touching the
    /// `sim ls`-parity bk tree).
    pub fn prim_state_children(&self, i: usize) -> Vec<prim::PrimSym> {
        match &self.insts[i].kind {
            InstKind::Prim(p) => p.state_children(),
            _ => Vec::new(),
        }
    }

    /// ORACLE architectural-state compare (trs-capi): walk every
    /// prim sub-symbol — scalars and range entries — and report
    /// values where `self` (the primary) and `other` (a secondary)
    /// disagree, at most `max` findings.  Engines share instance
    /// indexing (same BIR).  Prim state is live on every tier
    /// (arena-attached or boxed), unlike def recordings.
    pub fn state_divergence(
        &mut self,
        other: &mut Interp,
        max: usize,
    ) -> Vec<String> {
        let fmt = |v: &Option<Value>| match v {
            Some(v) => v.to_hex_string(),
            None => "NoValue".into(),
        };
        let mut out = Vec::new();
        for i in 0..self.insts.len() {
            if out.len() >= max {
                break;
            }
            // edge-transient prims (wires): stop-time value is a
            // clear-placement artifact, not architectural state
            if let InstKind::Prim(p) = &self.insts[i].kind {
                if p.sym_transient() {
                    continue;
                }
            }
            let path = self.insts[i].path.clone();
            for ps in self.prim_state_children(i) {
                if out.len() >= max {
                    break;
                }
                match ps.range {
                    None => {
                        let a = self.prim_sym_read(i, ps.key);
                        let b = other.prim_sym_read(i, ps.key);
                        if a != b {
                            out.push(format!(
                                "{path}.{}: {} vs primary {}",
                                ps.key,
                                fmt(&b),
                                fmt(&a)
                            ));
                        }
                    }
                    Some((lo, hi)) => {
                        // sparse ranges compare by OCCUPIED keys (the
                        // union of both engines' sets — a key written
                        // in one engine only reads undet in the other
                        // and flags); a dense lo..=hi walk over a
                        // RegFile#(UInt#(42)) is 4.4e12 reads per
                        // checkpoint (sysSparseRF hung the suite)
                        let ka = self.prim_sym_range_keys(i, ps.key);
                        let kb = other.prim_sym_range_keys(i, ps.key);
                        let keys: Option<Vec<u64>> = match (ka, kb) {
                            (None, None) => None,
                            (a, b) => {
                                let mut u: Vec<u64> = a
                                    .into_iter()
                                    .flatten()
                                    .chain(b.into_iter().flatten())
                                    .collect();
                                u.sort_unstable();
                                u.dedup();
                                Some(u)
                            }
                        };
                        let mut cmp = |addr: u64, out: &mut Vec<String>| {
                            let a = self.prim_sym_read_range(i, ps.key, addr);
                            let b = other.prim_sym_read_range(i, ps.key, addr);
                            if a != b {
                                out.push(format!(
                                    "{path}.{}[{addr}]: {} vs primary {}",
                                    ps.key,
                                    fmt(&b),
                                    fmt(&a)
                                ));
                            }
                        };
                        match keys {
                            Some(ks) => {
                                for addr in ks {
                                    cmp(addr, &mut out);
                                    if out.len() >= max {
                                        break;
                                    }
                                }
                            }
                            None => {
                                for addr in lo..=hi {
                                    cmp(addr, &mut out);
                                    if out.len() >= max {
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        out
    }

    pub fn prim_sym_read(&mut self, i: usize, key: &str) -> Option<Value> {
        let now = self.now;
        match &mut self.insts[i].kind {
            InstKind::Prim(p) => p.sym_read(key, now),
            _ => None,
        }
    }

    pub fn prim_sym_read_range(
        &mut self,
        i: usize,
        key: &str,
        addr: u64,
    ) -> Option<Value> {
        let now = self.now;
        match &mut self.insts[i].kind {
            InstKind::Prim(p) => p.sym_read_range(key, addr, now),
            _ => None,
        }
    }

    /// Occupied addresses of a sparse SYM_RANGE (None = dense; walk
    /// lo..=hi).  See Prim::sym_range_keys.
    pub fn prim_sym_range_keys(&mut self, i: usize, key: &str) -> Option<Vec<u64>> {
        match &mut self.insts[i].kind {
            InstKind::Prim(p) => p.sym_range_keys(key),
            _ => None,
        }
    }

    /// Construct from in-memory BIR bytes (the capi's embedded-BIR
    /// path — no file I/O at `sim load` time).
    pub fn from_bir_bytes(bytes: &[u8]) -> Result<Interp, String> {
        let design = Design::decode(bytes).map_err(|e| e.to_string())?;
        // no binding surface here: the interactive tier refuses
        // binding/auto-fire designs at `trs link --interactive`, and
        // a stale model of one fails loudly (missing binding) rather
        // than reading zeros
        let mut interp = Interp::new_bound(design, &[])?;
        interp.bir_hash = bir_fingerprint(bytes);
        Ok(interp)
    }

    /// capi EngineKind::Jit: enable the hybrid JIT for this engine
    /// regardless of the TRS_JIT environment (no-op without the
    /// jit feature — the lean build stays interp-only).
    pub fn arm_jit(&mut self) {
        self.jit_armed = true;
    }

    /// bk_set_timescale: scale factor applied to $time/%t values.
    pub fn set_timescale(&mut self, f: u64) {
        self.fe.timescale = f.max(1);
    }

    /// Top module name (the new_MODEL_<top> shim symbol).
    pub fn top_name(&self) -> &str {
        self.s(self.d.top)
    }

    /// Stage a +arg (without the '+') for $test$plusargs/$value$plusargs.
    pub fn append_plusarg(&mut self, a: &str) {
        self.fe.plusargs.push(a.to_string());
    }
}

/// Load a .bir (and its companion .bdpi.so, if any) into an Interp ready
/// to run — the driver's -c/-f scripting needs the handle between steps.
pub use startup::load_file;

/// Artifact-as-executable entry: the design objects are linked into
/// THIS process image (trs link --exe).  Load the embedded design,
/// resolve the compiled functions from ourselves, run.  BDPI rides as
/// a companion .so beside the executable.
#[cfg(feature = "aot")]
pub fn run_self(max_cycles: u64, plusargs: &[String]) -> Result<i32, String> {
    let mut sl = startup::StartupLap::new();
    let src = jit::ArtifactSource::This;
    let Some((hash, design)) = jit::aot_embedded_design(&src) else {
        return Err("no embedded design in this executable (trs_snap)".into());
    };
    sl.lap("design load (self-image snap)");
    // --exe artifacts refuse binding designs at link, so no binding
    // surface here either; a stale PIE of one fails loudly
    let mut interp = Interp::new_bound(design, &[])?;
    sl.lap("interp build (instantiate)");
    interp.bir_hash = hash;
    interp.fe.plusargs = plusargs.to_vec();
    if let Ok(exe) = std::env::current_exe() {
        let b = format!("{}.bdpi.so", exe.display());
        if std::path::Path::new(&b).exists() {
            interp.load_bdpi(&b)?;
        }
    }
    interp.aot_request_code_self();
    Ok(interp.run(max_cycles))
}

/// CLI teardown (tax-payment doctrine: exit pays nothing per run):
/// run to completion, finalize the one sink that needs a real close
/// (FST's tables are written by fstWriterClose in Drop; text waves
/// are fully flushed by finish() inside run(); $fopen files are raw
/// fds, closed by process exit), then LEAK the engine — the only
/// caller is `trs run`, which _exit()s immediately, so dropping the
/// whole design (measured 12.5M Ir on CrossingFIFOLoop, 16% of the
/// small-run gap) buys nothing.  TRS_TEARDOWN=drop restores full
/// drops (memcheck/leak hunting).  capi engines never come here —
/// bk_shutdown owns their teardown.
fn run_and_release(mut interp: Interp, max_cycles: u64) -> i32 {
    let rc = interp.run(max_cycles);
    if std::env::var_os("TRS_TEARDOWN").is_some_and(|v| v == "drop") {
        return rc;
    }
    let _ = interp.vcd_set_file(None);
    std::mem::forget(interp);
    rc
}

pub fn run_file(
    path: &str,
    max_cycles: u64,
    plusargs: &[String],
    binds: &[topbind::TopBind],
    vcd_file: Option<&str>,
    wave: Option<(WaveFormat, Option<String>)>,
    code: Option<&str>,
    formats: Option<(bool, bool)>,
    selfcheck: Option<(u64, bool)>,
) -> Result<i32, String> {
    // RunCore boot (TRS_RUNCORE=1, docs/RUNCORE.md): a wave-free,
    // selfcheck-free artifact run whose sidecar carries an eligible
    // descriptor + baked window boots here — no design decode, no
    // Interp, no plan.  Every gate failure falls through silently to
    // the classic boot below.  Top-level bindings boot classic: the
    // classic loader owns the baked-bind identity check (and the
    // recompile-on-mismatch fallback), which the boot cannot do.
    #[cfg(feature = "aot")]
    if selfcheck.is_none() && wave.is_none() && vcd_file.is_none() && binds.is_empty() {
        if let Some(so) = code {
            if let Some(rc) = runcore::try_boot(so, max_cycles, plusargs) {
                return Ok(rc);
            }
        }
    }
    let mut interp =
        startup::load_file_or_code(path, code, plusargs, binds, vcd_file)?;
    if let Some((vcd, fst)) = formats {
        interp.set_allowed_wave_formats(vcd, fst);
    }
    if let Some((f, file)) = wave {
        interp.wave_request(f, file);
    }
    if let Some(so) = code {
        interp.aot_request_code(so.into());
    }
    // announce: notes print only for an EXPLICIT --selfcheck (an
    // interactive user).  Env-armed runs (TRS_SELFCHECK=1 — the
    // corpus sweep and the DejaGnu suite) stay silent on skips: the
    // suite captures stderr into byte-compared output, and a note
    // would fail every BDPI test.  Divergence reports are NOT notes —
    // they always print (a diverging suite test failing with the
    // report in its log is the point).
    let Some((every, announce)) = selfcheck else {
        return Ok(run_and_release(interp, max_cycles));
    };
    if interp.needs_user_bdpi() {
        // dlopen of one path is one refcounted image: user C globals
        // are process-global, so a lockstep shadow DOUBLE-EXECUTES
        // stateful foreign functions and corrupts the primary's own
        // outputs (14 foreign-battery witnesses in the first selfcheck
        // sweep) — skip the shadow, run plain
        if announce {
            eprintln!(
                "trs selfcheck: note: design imports BDPI — user C \
                 state is process-global and a lockstep shadow would \
                 double-execute stateful foreign functions; selfcheck \
                 skipped"
            );
        }
        return Ok(run_and_release(interp, max_cycles));
    }
    // lockstep selfcheck: quiet shadow engines ride beside the primary
    // — no console/file/VCD output, debug tier (a shadow is an oracle,
    // not the artifact's execution engine, so TRS_REQUIRE_AOT does not
    // police it).  The default shadow set covers EVERY other execution
    // tier in one run: a pure interp always, plus a hybrid-jit shadow
    // when the primary is the aot artifact — interp, jit, and aot then
    // cross-check simultaneously, one mode instead of three.
    // TRS_SELFCHECK_ENGINES=interp[,jit] overrides.  Construction runs
    // under the quiet stamp too: elaboration-time prim diagnostics
    // ($readmem gap warnings) print at load, before any advance
    // re-stamps the thread-local (sysWarningTest leaked the shadow's
    // copy into stdout).
    let kinds: Vec<&'static str> = match std::env::var("TRS_SELFCHECK_ENGINES") {
        Ok(s) => s
            .split(',')
            .filter_map(|t| match t.trim() {
                "interp" => Some("interp"),
                "jit" => Some("jit"),
                _ => None,
            })
            .collect(),
        Err(_) => {
            if code.is_some() {
                vec!["interp", "jit"]
            } else {
                vec!["interp"]
            }
        }
    };
    let mut shadows: Vec<(&'static str, Interp)> = Vec::new();
    for kind in kinds {
        prim::QUIET_ENGINE.with(|c| c.set(true));
        let sh = startup::load_file_or_code(path, code, plusargs, binds, None);
        prim::QUIET_ENGINE.with(|c| c.set(false));
        let mut sh = sh?;
        sh.set_quiet();
        sh.set_debug_tier();
        if kind == "jit" {
            sh.arm_jit();
        }
        shadows.push((kind, sh));
    }
    Ok(interp.run_lockstep(&mut shadows, max_cycles, every))
}
