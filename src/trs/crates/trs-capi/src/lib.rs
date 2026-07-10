//! Bluesim kernel C API (`bk_*`) on the trs interpreter.
//!
//! bluetcl's `sim load <file>.so <top>` dlopens the model and dlsyms
//! `new_MODEL_<top>` plus ~47 `bk_*` functions (the exact set and the
//! call protocol are recorded in `docs/TCL-CAPI.md`, measured from
//! `src/comp/BluesimLoader.hs`).  This crate implements the generic
//! side; `trs link --interactive` emits a per-design shim object
//! that exports `new_MODEL_<top>` (returning a heap `Model` carrying
//! the embedded BIR) and links the two into `<out>.so`.
//!
//! Engine: the interpreter + resumable stepper (`prime`/`advance`/
//! `finish`) — the DEBUG compile mode's executor.  Nothing here may
//! depend on the fast compile's exports; full state visibility is the
//! interpreter's native property.
//!
//! Threading: the reference kernel runs the simulation on a separate
//! thread for `bk_advance(async)`. The interactive `.cmd` corpus uses
//! the sync path except `async.cmd`; the async story lands with a
//! dedicated driver thread once the sync surface is byte-clean.

use std::ffi::{c_char, c_void, CStr, CString};

use trs_interp::Interp;

/// What `new_MODEL_<top>` (the generated shim) returns: enough to
/// construct the interpreter at `bk_init`.
#[repr(C)]
pub struct Model {
    /// Embedded CBOR BIR (the shim links the design in; no file I/O
    /// at load time, mirroring the reference model's self-containment)
    pub bir_ptr: *const u8,
    pub bir_len: usize,
    /// NUL-terminated top module name (diagnostics only)
    pub top: *const c_char,
}

/// One executor: all three engines are Interp-rooted (plain interp;
/// hybrid JIT = the TRS_JIT machinery inside the interp; AOT = the
/// artifact's design .so loaded the artifact way).
pub struct Engine {
    pub interp: Interp,
    pub kind: EngineKind,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum EngineKind {
    Interp,
    Jit,
    Aot,
}

/// The `tSimStateHdl` behind every `bk_*` call.
///
/// engines[0] is PRIMARY: it owns stdout and answers queries.  More
/// than one engine = interactive ORACLE (docs/TCL-CAPI.md): run
/// control fans out to all engines in lockstep; time/status/peeks
/// are cross-checked and a divergence reports at the stop point.
pub struct SimState {
    engines: Vec<Engine>,
    /// plusargs staged before/after init (`bk_append_argument`)
    args: Vec<String>,
    /// interned CStrings handed out by `bk_*` name accessors (the C
    /// side treats them as borrowed; they must outlive the handle)
    names: Vec<CString>,
    /// exit protocol mirror (bk_finished / bk_exit_status / bk_fataled)
    exit_status: i32,
    /// bk_quit_after_edge limit slots: ABSOLUTE target per
    /// (clock handle, posedge?); overwrite on set, disarmed when at
    /// or below the current count
    edge_limits: std::collections::HashMap<(u32, bool), u64>,
    /// bk_schedule_ui_event times, consumed when reached
    ui_events: Vec<u64>,
    /// sim config interactive (edges with no logic still stop cleanly)
    interactive: bool,
    /// bk_abort_now (async sessions; sync records it only)
    aborted: bool,
    /// bk_set_timescale (VCD header)
    timescale: Option<(String, u64)>,
    /// async run in flight (bk_advance(async): engines move to the
    /// worker; bk_sync joins and moves them back)
    runner: Option<Runner>,
    /// the symbol tree (built once at bk_init; NEVER mutated after —
    /// tSymbol handles are raw pointers into this Vec)
    syms: Vec<Sym>,
    /// bk_peek_* word buffer (valid until the next peek, like the
    /// reference's internal storage)
    peek_buf: Vec<u32>,
}

/// The engines while they live on the async worker thread.  Interp
/// holds raw prim/arena pointers (not Send in general), but the
/// vector is moved WHOLE between threads with exclusive ownership —
/// never aliased across the boundary.
struct EngineBox(Vec<Engine>);
unsafe impl Send for EngineBox {}

struct Runner {
    join: std::thread::JoinHandle<(EngineBox, i32)>,
    abort: std::sync::Arc<std::sync::atomic::AtomicBool>,
    running: std::sync::Arc<std::sync::atomic::AtomicBool>,
    progress: std::sync::Arc<std::sync::atomic::AtomicU64>,
}

/// One node of the symbol tree.  Carries a back-pointer to its
/// SimState because the bk_* symbol accessors take only tSymbol.
pub struct Sym {
    st: *mut SimState,
    key: CString,
    width: u32,
    kind: SymKind,
    /// child indices into SimState.syms, symOrd-sorted
    /// (case-insensitive, then case-sensitive)
    children: Vec<usize>,
}

enum SymKind {
    /// user module or prim container ("module with value")
    Module,
    /// a value prim's sub-signal (module -> "" redirect target,
    /// isValid/value/level/... — the reference's per-prim tables)
    PrimValue { inst: usize, key: &'static str },
    /// a def signal; peeks read the LAST-COMPUTED value
    Def { inst: usize, id: trs_ir::StrId },
    /// an instantiation parameter (value bound at elaboration)
    Param { inst: usize, name: String },
    /// a method port (EN_/arg/RDY_/result — SYM_PORT semantics)
    MethPort {
        inst: usize,
        method: trs_ir::StrId,
        kind: trs_interp::MethPortKind,
    },
    Rule,
    /// an addressable range sub-symbol (RegFile/FIFO storage)
    Range { inst: usize, key: &'static str, lo: u64, hi: u64 },
}

impl SimState {
    fn primary(&mut self) -> &mut Interp {
        &mut self.engines[0].interp
    }
}

fn state<'a>(hdl: *mut c_void) -> &'a mut SimState {
    unsafe { &mut *(hdl as *mut SimState) }
}

/// `bk_init(model, master)`: construct the interpreter from the BIR,
/// run the one-time event-loop setup (kernel reset protocol included),
/// and return the handle (NULL on failure => `sim load` fails).
///
/// master=True (bluetcl always passes True) installs the default
/// clock waveform and default reset — `prime()` is that protocol.
#[no_mangle]
pub extern "C" fn bk_init(model: *mut c_void, _master: u8) -> *mut c_void {
    let m = unsafe { &*(model as *const Model) };
    let bir = unsafe { std::slice::from_raw_parts(m.bir_ptr, m.bir_len) };
    // engine set: link-time default (shim Model, TBD) overridden by
    // TRS_CAPI_ENGINES=interp[,jit][,aot] at load
    let sel = std::env::var("TRS_CAPI_ENGINES").unwrap_or_default();
    let kinds: Vec<EngineKind> = sel
        .split(',')
        .filter_map(|k| match k.trim() {
            "interp" => Some(EngineKind::Interp),
            "jit" => Some(EngineKind::Jit),
            "aot" => Some(EngineKind::Aot),
            _ => None,
        })
        .collect();
    let kinds = if kinds.is_empty() {
        // shipped default (Ravi): fast `sim run` via the hybrid JIT
        // when the library carries it; the lean build is interp-only.
        // Introspection tiers per docs/TCL-CAPI.md: def peeks need
        // the interp engine's recording.
        if cfg!(feature = "jit") {
            vec![EngineKind::Jit]
        } else {
            vec![EngineKind::Interp]
        }
    } else {
        kinds
    };
    // companions travel beside the model .so (trs link --interactive
    // puts them there); the Model statics live in the model's data
    // segment, so dladdr locates the .so.  <base>.bdpi.so = user BDPI
    // code; <base>.aot.so = the fast-artifact design .so the aot
    // engine loads (warm bodies from t=0).
    let model_base = {
        let mut info: libc::Dl_info = unsafe { std::mem::zeroed() };
        let found =
            unsafe { libc::dladdr(model as *const c_void, &mut info) } != 0
                && !info.dli_fname.is_null();
        found.then(|| {
            let p = unsafe { CStr::from_ptr(info.dli_fname) }
                .to_string_lossy()
                .into_owned();
            p.strip_suffix(".so").map(String::from).unwrap_or(p)
        })
    };
    let companion = |ext: &str| {
        model_base
            .as_ref()
            .map(|b| format!("{b}.{ext}"))
            .filter(|p| std::path::Path::new(p).exists())
    };
    let bdpi_so = companion("bdpi.so");
    let aot_so = companion("aot.so");
    let mut engines = Vec::new();
    for kind in kinds {
        match Interp::from_bir_bytes(bir) {
            Ok(mut interp) => {
                if let Some(so) = &bdpi_so {
                    if let Err(e) = interp.load_bdpi(so) {
                        eprintln!("trs capi: bk_init: {e}");
                        return std::ptr::null_mut();
                    }
                }
                if kind == EngineKind::Jit {
                    interp.arm_jit();
                }
                if kind == EngineKind::Aot {
                    // artifact-pair construction: the design .so beside
                    // the model, hash-checked at prime (a stale or
                    // missing artifact falls back with a stderr note —
                    // the degradation policy's downgrade)
                    match &aot_so {
                        Some(so) => interp.aot_request_code(so.into()),
                        None => eprintln!(
                            "trs capi: aot engine requested but no \
                             .aot.so beside the model (relink with a \
                             current trs); running interpreted"
                        ),
                    }
                }
                if !engines.is_empty() {
                    // secondary oracle engines: output suppressed,
                    // state runs — lockstep-compared at every stop
                    interp.set_quiet();
                }
                engines.push(Engine { interp, kind });
            }
            Err(e) => {
                eprintln!("trs capi: bk_init: {e}");
                return std::ptr::null_mut();
            }
        }
    }
    let mut st = Box::new(SimState {
        engines,
        args: Vec::new(),
        names: Vec::new(),
        exit_status: 0,
        edge_limits: std::collections::HashMap::new(),
        ui_events: Vec::new(),
        interactive: false,
        aborted: false,
        timescale: None,
        runner: None,
        syms: Vec::new(),
        peek_buf: Vec::new(),
    });
    // one-time event-loop setup: clocks resolved, kernel reset
    // protocol seeded — `sim clock` works right after `sim load`
    for e in &mut st.engines {
        e.interp.prime();
        // debug tier: the INTERP engine retains last-computed def
        // values for peeks; JIT engines skip the recording (their
        // def visibility degrades per the capability tiers — and
        // sym_trace would disable the hybrid entirely)
        if e.kind == EngineKind::Interp {
            e.interp.set_sym_trace();
        }
    }
    let raw = Box::into_raw(st);
    unsafe { build_symbols(raw) };
    raw as *mut c_void
}

/// Build the symbol tree (module/def/rule/value/range nodes per
/// docs/TCL-CAPI.md), sorted like the reference (case-insensitive,
/// then case-sensitive).  Runs once; the Vec is never touched again
/// so raw Sym pointers stay valid for the session.
unsafe fn build_symbols(stp: *mut SimState) {
    let st = &mut *stp;
    let seed = st.primary().symbol_seed();
    let mut syms: Vec<Sym> = Vec::new();
    let mut mod_sym: Vec<usize> = Vec::with_capacity(seed.len());
    let sym = |key: &str, width, kind| Sym {
        st: stp,
        key: CString::new(key).unwrap_or_default(),
        width,
        kind,
        children: Vec::new(),
    };
    // one module node per instance
    for (_, name, _) in &seed {
        mod_sym.push(syms.len());
        syms.push(sym(name, 0, SymKind::Module));
    }
    for (i, (parent, _, is_user)) in seed.iter().enumerate() {
        // wire into the parent
        if let Some(p) = parent {
            let child = mod_sym[i];
            syms[mod_sym[*p]].children.push(child);
        }
        if *is_user {
            let mut taken: std::collections::HashSet<String> =
                std::collections::HashSet::new();
            for (pn, w, method, kind) in st.primary().method_port_symbols(i) {
                taken.insert(pn.clone());
                let k = syms.len();
                syms.push(sym(&pn, w, SymKind::MethPort { inst: i, method, kind }));
                syms[mod_sym[i]].children.push(k);
            }
            for (pn, pv) in st.primary().inst_params(i) {
                let k = syms.len();
                syms.push(sym(
                    &pn,
                    pv.width,
                    SymKind::Param { inst: i, name: pn.clone() },
                ));
                syms[mod_sym[i]].children.push(k);
            }
            for r in st.primary().inst_rules(i) {
                let k = syms.len();
                syms.push(sym(&r, 0, SymKind::Rule));
                syms[mod_sym[i]].children.push(k);
            }
            for (name, width, id) in st.primary().def_symbols(i) {
                // method ports shadow same-named defs (RDY_<m> is
                // often both; the port evaluates FRESH like the
                // reference's per-pass member update)
                if taken.contains(&name) {
                    continue;
                }
                let k = syms.len();
                syms.push(sym(&name, width, SymKind::Def { inst: i, id }));
                syms[mod_sym[i]].children.push(k);
            }
        } else {
            for ps in st.primary().prim_sym_children(i) {
                let k = syms.len();
                let kind = match ps.range {
                    Some((lo, hi)) => {
                        SymKind::Range { inst: i, key: ps.key, lo, hi }
                    }
                    None => SymKind::PrimValue { inst: i, key: ps.key },
                };
                syms.push(sym(ps.key, ps.width, kind));
                syms[mod_sym[i]].children.push(k);
            }
        }
    }
    // symOrd: case-insensitive, ties case-sensitive
    let keys: Vec<String> = syms
        .iter()
        .map(|x| x.key.to_string_lossy().into_owned())
        .collect();
    for x in &mut syms {
        x.children
            .sort_by(|&a, &b| {
                let (ka, kb) = (&keys[a], &keys[b]);
                ka.to_lowercase()
                    .cmp(&kb.to_lowercase())
                    .then_with(|| ka.cmp(kb))
            });
    }
    st.syms = syms;
}

fn sym<'a>(p: *mut c_void) -> Option<&'a Sym> {
    unsafe { (p as *const Sym).as_ref() }
}

// =================================================================
// Symbol surface

#[no_mangle]
pub extern "C" fn bk_top_symbol(hdl: *mut c_void) -> *mut c_void {
    let st = state(hdl);
    match st.syms.first() {
        Some(s) => s as *const Sym as *mut c_void,
        None => std::ptr::null_mut(),
    }
}

/// Dotted-path resolution happens in the KERNEL (bluetcl passes
/// whole segments through).
#[no_mangle]
pub extern "C" fn bk_lookup_symbol(
    root: *mut c_void,
    name: *const c_char,
) -> *mut c_void {
    let Some(mut cur) = sym(root) else {
        return std::ptr::null_mut();
    };
    let path = unsafe { CStr::from_ptr(name) }.to_string_lossy().into_owned();
    let st = unsafe { &*cur.st };
    for seg in path.split('.') {
        let Some(&k) = cur.children.iter().find(|&&k| {
            st.syms[k].key.to_bytes() == seg.as_bytes()
        }) else {
            return std::ptr::null_mut();
        };
        cur = &st.syms[k];
    }
    cur as *const Sym as *mut c_void
}

#[no_mangle]
pub extern "C" fn bk_get_key(p: *mut c_void) -> *const c_char {
    sym(p).map(|s| s.key.as_ptr()).unwrap_or(std::ptr::null())
}

#[no_mangle]
pub extern "C" fn bk_get_size(p: *mut c_void) -> u32 {
    sym(p).map(|s| s.width).unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn bk_is_module(p: *mut c_void) -> u8 {
    matches!(sym(p).map(|s| &s.kind), Some(SymKind::Module)) as u8
}

#[no_mangle]
pub extern "C" fn bk_is_rule(p: *mut c_void) -> u8 {
    matches!(sym(p).map(|s| &s.kind), Some(SymKind::Rule)) as u8
}

#[no_mangle]
pub extern "C" fn bk_is_single_value(p: *mut c_void) -> u8 {
    matches!(
        sym(p).map(|s| &s.kind),
        Some(
            SymKind::Def { .. }
                | SymKind::PrimValue { .. }
                | SymKind::Param { .. }
                | SymKind::MethPort { .. }
        )
    ) as u8
}

#[no_mangle]
pub extern "C" fn bk_is_value_range(p: *mut c_void) -> u8 {
    matches!(sym(p).map(|s| &s.kind), Some(SymKind::Range { .. })) as u8
}

/// Fill the peek buffer with a Value's little-endian u32 words and
/// return it (valid until the next peek, like the reference).
fn peek_words(st: &mut SimState, v: &trs_interp::value::Value) -> *const u32 {
    let words = ((v.width as usize) + 31) / 32;
    st.peek_buf.clear();
    for l in v.limbs64() {
        st.peek_buf.push(*l as u32);
        st.peek_buf.push((*l >> 32) as u32);
    }
    st.peek_buf.truncate(words.max(1));
    while st.peek_buf.len() < words.max(1) {
        st.peek_buf.push(0);
    }
    st.peek_buf.as_ptr()
}

#[no_mangle]
pub extern "C" fn bk_peek_symbol_value(p: *mut c_void) -> *const u32 {
    // a peek must NEVER abort the session: resolution failures on
    // exotic shapes answer NoValue (the reference's own vocabulary)
    std::panic::catch_unwind(|| peek_symbol_value_inner(p))
        .unwrap_or(std::ptr::null())
}

fn peek_symbol_value_inner(p: *mut c_void) -> *const u32 {
    let Some(s) = sym(p) else {
        return std::ptr::null();
    };
    let st = unsafe { &mut *s.st };
    if st.engines.is_empty() {
        return std::ptr::null(); // async run in flight
    }
    // capability tiers: only the interp engine records defs/ports —
    // other engines degrade to NoValue rather than fabricate zeros
    let recording = st
        .engines
        .first()
        .map(|e| e.kind == EngineKind::Interp)
        .unwrap_or(false);
    match s.kind {
        SymKind::Def { .. } | SymKind::MethPort { .. } if !recording => {
            std::ptr::null()
        }
        SymKind::Def { inst, id } => {
            // last-computed value; zeros before first computation
            // (reference member fields start zeroed)
            let v = st
                .primary()
                .def_peek(inst, id)
                .unwrap_or_else(|| trs_interp::value::Value::zero(s.width));
            peek_words(st, &v)
        }
        SymKind::PrimValue { inst, key } => {
            match st.primary().prim_sym_read(inst, key) {
                Some(v) => peek_words(st, &v),
                None => std::ptr::null(),
            }
        }
        SymKind::MethPort { inst, method, kind } => {
            let w = s.width;
            let v = st.primary().method_port_peek(inst, method, kind, w);
            peek_words(st, &v)
        }
        SymKind::Param { inst, ref name } => {
            let v = st
                .primary()
                .inst_params(inst)
                .into_iter()
                .find(|(n, _)| n == name)
                .map(|(_, v)| v);
            match v {
                Some(v) => peek_words(st, &v),
                None => std::ptr::null(),
            }
        }
        _ => std::ptr::null(),
    }
}

#[no_mangle]
pub extern "C" fn bk_get_range_min_addr(p: *mut c_void) -> u64 {
    match sym(p).map(|s| &s.kind) {
        Some(&SymKind::Range { lo, .. }) => lo,
        _ => 0,
    }
}

#[no_mangle]
pub extern "C" fn bk_get_range_max_addr(p: *mut c_void) -> u64 {
    match sym(p).map(|s| &s.kind) {
        Some(&SymKind::Range { hi, .. }) => hi,
        _ => 0,
    }
}

#[no_mangle]
pub extern "C" fn bk_peek_range_value(p: *mut c_void, addr: u64) -> *const u32 {
    std::panic::catch_unwind(|| peek_range_value_inner(p, addr))
        .unwrap_or(std::ptr::null())
}

fn peek_range_value_inner(p: *mut c_void, addr: u64) -> *const u32 {
    let Some(s) = sym(p) else {
        return std::ptr::null();
    };
    let st = unsafe { &mut *s.st };
    if st.engines.is_empty() {
        return std::ptr::null(); // async run in flight
    }
    match s.kind {
        SymKind::Range { inst, key, lo, hi } if addr >= lo && addr <= hi => {
            match st.primary().prim_sym_read_range(inst, key, addr) {
                Some(v) => peek_words(st, &v),
                None => std::ptr::null(),
            }
        }
        _ => std::ptr::null(),
    }
}

#[no_mangle]
pub extern "C" fn bk_num_symbols(p: *mut c_void) -> u32 {
    sym(p).map(|s| s.children.len() as u32).unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn bk_get_nth_symbol(p: *mut c_void, n: u32) -> *mut c_void {
    let Some(s) = sym(p) else {
        return std::ptr::null_mut();
    };
    let st = unsafe { &*s.st };
    match s.children.get(n as usize) {
        Some(&k) => &st.syms[k] as *const Sym as *mut c_void,
        None => std::ptr::null_mut(),
    }
}



/// `bk_shutdown`: free everything.  bluetcl dlcloses afterwards.
#[no_mangle]
pub extern "C" fn bk_shutdown(hdl: *mut c_void) {
    if !hdl.is_null() {
        let mut st = unsafe { Box::from_raw(hdl as *mut SimState) };
        // an in-flight async run must be stopped and JOINED: bluetcl
        // dlcloses the .so immediately after, and a detached worker
        // would be executing unmapped code
        if let Some(r) = st.runner.take() {
            r.abort.store(true, std::sync::atomic::Ordering::SeqCst);
            if let Ok((engines, _)) = r.join.join() {
                st.engines = engines.0;
            }
        }
        // kernel.cxx:767 vcd_reset at shutdown: finish an interrupted
        // timeslice's VCD dump, then flush buffered changes strictly
        // before the stop time — without this, the final stanzas of a
        // Tcl-driven VCD (`sim vcd` + steps, then exit) never land
        if let Some(e) = st.engines.first_mut() {
            let _ = e.interp.finish();
        }
        drop(st);
    }
}

/// `bk_now`: current simulation time, SCALED by the timescale
/// factor (kernel.cxx: sim_timescale * sim_time).
#[no_mangle]
pub extern "C" fn bk_now(hdl: *mut c_void) -> u64 {
    let st = state(hdl);
    let f = st.timescale.as_ref().map(|(_, f)| *f).unwrap_or(1);
    if let Some(r) = &st.runner {
        // async run in flight: the worker publishes per-slice
        return r
            .progress
            .load(std::sync::atomic::Ordering::Relaxed)
            .wrapping_mul(f);
    }
    st.primary().now().wrapping_mul(f)
}

/// `bk_append_argument`: stage a plusarg.
#[no_mangle]
pub extern "C" fn bk_append_argument(hdl: *mut c_void, arg: *const c_char) {
    let s = unsafe { CStr::from_ptr(arg) }.to_string_lossy().into_owned();
    let st = state(hdl);
    for e in &mut st.engines {
        e.interp.append_plusarg(&s);
    }
    st.args.push(s);
}

/// `bk_finished`: has $finish been called.
#[no_mangle]
pub extern "C" fn bk_finished(hdl: *mut c_void) -> u8 {
    let st = state(hdl);
    if st.engines.is_empty() {
        return 0; // async run in flight
    }
    st.primary().is_finished() as u8
}

/// `bk_exit_status`: status of the last $stop/$finish.
#[no_mangle]
pub extern "C" fn bk_exit_status(hdl: *mut c_void) -> i32 {
    state(hdl).exit_status
}

// ---------------------------------------------------------------
// TODO (docs/TCL-CAPI.md acceptance ladder):
//   clocks:   bk_define_clock .. bk_clock_last_edge on prime()'s
//             kernel clock list
//   run ctl:  bk_quit_after_edge / bk_schedule_ui_event /
//             bk_remove_ui_event / bk_set_interactive / bk_advance /
//             bk_is_running / bk_sync / bk_abort_now / bk_fataled
//   symbols:  bk_top_symbol .. bk_get_nth_symbol (tree from BIR +
//             InstEnvs; case-insensitive symOrd; module -> ""
//             redirect; RegFile/BRAM ranges)
//   vcd:      bk_set_VCD_file / bk_enable_VCD_dumping /
//             bk_disable_VCD_dumping
//   misc:     bk_version / bk_set_timescale
// ---------------------------------------------------------------

// =================================================================
// Clock surface (bk_clock_*): reads over the interp's kernel clock
// list (VcdClock is the tClockInfo mirror; handles are indices).
// tClock = u32, BAD_CLOCK_HANDLE = !0u32, tEdgeDirection POSEDGE=1.

const BAD_CLOCK: u32 = !0u32;
const BK_SUCCESS: i32 = 0;
const BK_ERROR: i32 = -1;

impl SimState {
    fn cstr(&mut self, s: &str) -> *const c_char {
        let c = CString::new(s).unwrap_or_default();
        let p = c.as_ptr();
        self.names.push(c);
        p
    }
    fn clock(&mut self, h: u32) -> Option<trs_interp::ClockInfo> {
        if self.engines.is_empty() {
            return None; // async run in flight
        }
        self.primary().clock_info().into_iter().nth(h as usize)
    }
}

#[no_mangle]
pub extern "C" fn bk_num_clocks(hdl: *mut c_void) -> u32 {
    let st = state(hdl);
    if st.engines.is_empty() {
        return 0; // async run in flight
    }
    st.primary().clock_info().len() as u32
}

#[no_mangle]
pub extern "C" fn bk_get_nth_clock(hdl: *mut c_void, n: u32) -> u32 {
    let st = state(hdl);
    if st.engines.is_empty() {
        return BAD_CLOCK; // async run in flight
    }
    if (n as usize) < st.primary().clock_info().len() {
        n
    } else {
        BAD_CLOCK
    }
}

#[no_mangle]
pub extern "C" fn bk_get_clock_by_name(
    hdl: *mut c_void,
    name: *const c_char,
) -> u32 {
    let want = unsafe { CStr::from_ptr(name) }.to_string_lossy().into_owned();
    let st = state(hdl);
    if st.engines.is_empty() {
        return BAD_CLOCK; // async run in flight
    }
    st.primary()
        .clock_info()
        .iter()
        .position(|c| c.name == want)
        .map(|i| i as u32)
        .unwrap_or(BAD_CLOCK)
}

#[no_mangle]
pub extern "C" fn bk_clock_name(hdl: *mut c_void, h: u32) -> *const c_char {
    let st = state(hdl);
    match st.clock(h) {
        Some(c) => {
            let name = c.name.clone();
            st.cstr(&name)
        }
        None => std::ptr::null(),
    }
}

#[no_mangle]
pub extern "C" fn bk_clock_initial_value(hdl: *mut c_void, h: u32) -> u32 {
    state(hdl).clock(h).map(|c| c.initial_val as u32).unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn bk_clock_first_edge(hdl: *mut c_void, h: u32) -> u64 {
    state(hdl).clock(h).map(|c| c.first_edge).unwrap_or(0)
}

/// duration of the LOW (value=0) or HIGH (value=1) phase
#[no_mangle]
pub extern "C" fn bk_clock_duration(hdl: *mut c_void, h: u32, value: u32) -> u64 {
    state(hdl)
        .clock(h)
        .map(|c| if value != 0 { c.high_dur } else { c.low_dur })
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn bk_clock_val(hdl: *mut c_void, h: u32) -> u32 {
    state(hdl).clock(h).map(|c| c.cur_val as u32).unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn bk_clock_cycle_count(hdl: *mut c_void, h: u32) -> u64 {
    state(hdl).clock(h).map(|c| c.cycles).unwrap_or(0)
}

/// tEdgeDirection: NEGEDGE=0, POSEDGE=1
#[no_mangle]
pub extern "C" fn bk_clock_edge_count(hdl: *mut c_void, h: u32, dir: u32) -> u64 {
    state(hdl)
        .clock(h)
        .map(|c| if dir != 0 { c.cycles } else { c.neg_edges })
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn bk_clock_last_edge(hdl: *mut c_void, h: u32) -> u64 {
    state(hdl).clock(h).map(|c| c.last_edge).unwrap_or(0)
}

// =================================================================
// Run control: the bk stop machinery over advance_until(StopCond).
// Limit slots are ABSOLUTE per (clock, dir) and overwrite; a limit
// at or below the current count is DISARMED (this is how bluetcl's
// step "restores" a limit that was not reached).  Sync only for
// now: async lands with the driver thread (async.cmd).

#[no_mangle]
pub extern "C" fn bk_quit_after_edge(
    hdl: *mut c_void,
    h: u32,
    dir: u32,
    count: u64,
) -> i32 {
    let st = state(hdl);
    if st.clock(h).is_none() {
        return BK_ERROR;
    }
    st.edge_limits.insert((h, dir != 0), count);
    BK_SUCCESS
}

#[no_mangle]
pub extern "C" fn bk_schedule_ui_event(hdl: *mut c_void, at: u64) -> i32 {
    let st = state(hdl);
    if !st.ui_events.contains(&at) {
        st.ui_events.push(at);
    }
    BK_SUCCESS
}

#[no_mangle]
pub extern "C" fn bk_remove_ui_event(hdl: *mut c_void, at: u64) -> i32 {
    state(hdl).ui_events.retain(|&t| t != at);
    BK_SUCCESS
}

#[no_mangle]
pub extern "C" fn bk_set_interactive(hdl: *mut c_void) {
    state(hdl).interactive = true;
}

/// Oracle lockstep compare at a stop (docs/TCL-CAPI.md): every
/// secondary must agree with the primary on time, per-clock edge
/// counts, and finish state.  A divergence reports the instant and
/// the mismatching quantity to stderr, then flips the primary's
/// fatal flag so scripts stop AT the divergence.  Returns true iff
/// divergent.  (Architectural-state compare rides the symbol-tree
/// work queued with AOT engine construction.)
fn oracle_check(engines: &mut [Engine]) -> bool {
    if engines.len() < 2 {
        return false;
    }
    let (p, rest) = engines.split_first_mut().unwrap();
    let pt = p.interp.now();
    let pc = p.interp.clock_info();
    let pf = p.interp.is_finished();
    let mut msgs: Vec<String> = Vec::new();
    for (i, e) in rest.iter_mut().enumerate() {
        let n = i + 1;
        let t = e.interp.now();
        if t != pt {
            msgs.push(format!("engine {n}: time {t} vs primary {pt}"));
        }
        for (ci, (a, b)) in
            pc.iter().zip(e.interp.clock_info().iter()).enumerate()
        {
            if (a.cycles, a.neg_edges) != (b.cycles, b.neg_edges) {
                msgs.push(format!(
                    "engine {n}: clock {ci} edges {}+{} vs primary {}+{}",
                    b.cycles, b.neg_edges, a.cycles, a.neg_edges
                ));
            }
        }
        let f = e.interp.is_finished();
        if f != pf {
            msgs.push(format!("engine {n}: finished {f} vs primary {pf}"));
        }
    }
    if msgs.is_empty() {
        return false;
    }
    for m in &msgs {
        eprintln!("trs oracle: divergence at t={pt}: {m}");
    }
    p.interp.mark_fatal();
    true
}

#[no_mangle]
pub extern "C" fn bk_advance(hdl: *mut c_void, is_async: u8) -> i32 {
    let st = state(hdl);
    if st.runner.is_some() || st.engines.is_empty() {
        return BK_ERROR; // already running
    }
    // effective stop condition: armed limits only (above the current
    // per-clock count), plus outstanding UI events
    let clocks = st.primary().clock_info();
    let edge_limits: Vec<(usize, bool, u64)> = st
        .edge_limits
        .iter()
        .filter_map(|(&(h, dir), &lim)| {
            let c = clocks.get(h as usize)?;
            let count = if dir { c.cycles } else { c.neg_edges };
            (lim > count).then_some((h as usize, dir, lim))
        })
        .collect();
    let mut cond = trs_interp::StopCond {
        max_cycles: u64::MAX,
        edge_limits,
        at_times: st.ui_events.clone(),
        ..Default::default()
    };
    if is_async != 0 {
        use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
        use std::sync::Arc;
        let abort = Arc::new(AtomicBool::new(false));
        let running = Arc::new(AtomicBool::new(true));
        let progress = Arc::new(AtomicU64::new(st.primary().now()));
        cond.abort = Some(abort.clone());
        cond.progress = Some(progress.clone());
        let mut engines = EngineBox(std::mem::take(&mut st.engines));
        let running2 = running.clone();
        let join = std::thread::spawn(move || {
            let mut rc = BK_SUCCESS;
            let mut it = engines.0.iter_mut();
            if let Some(p) = it.next() {
                let r = p.interp.advance_until(&cond);
                if r != 0 {
                    rc = r;
                }
                // secondaries catch up to the primary's ACTUAL stop:
                // bk_abort_now may have fired mid-run, and the shared
                // flag would otherwise halt them at a random earlier
                // instant (false divergence).  Aborts are slice-
                // aligned, so "every timeslice <= primary.now" is
                // exactly the primary's state — the at_times contract.
                // (Edge-count targets are wrong here: the loop's
                // slice-end check breaks on ANY reached limit, and the
                // trailing direction's count is reached one slice
                // before the leading one's.)
                let target = trs_interp::StopCond {
                    max_cycles: u64::MAX,
                    at_times: vec![p.interp.now()],
                    ..Default::default()
                };
                for e in it {
                    let r = e.interp.advance_until(&target);
                    if r != 0 {
                        rc = r;
                    }
                }
            }
            if oracle_check(&mut engines.0) {
                rc = 1;
            }
            running2.store(false, Ordering::SeqCst);
            (engines, rc)
        });
        st.runner = Some(Runner { join, abort, running, progress });
        return BK_SUCCESS;
    }
    let mut rc = BK_SUCCESS;
    // primary first (owns stdout); quiet secondaries follow, then the
    // lockstep compare
    for e in &mut st.engines {
        let r = e.interp.advance_until(&cond);
        if r != 0 {
            rc = r;
        }
    }
    if oracle_check(&mut st.engines) {
        rc = 1;
    }
    st.exit_status = rc;
    // UI events at or before the stop time have fired
    let now = st.primary().now();
    st.ui_events.retain(|&t| t > now);
    BK_SUCCESS
}

#[no_mangle]
pub extern "C" fn bk_is_running(hdl: *mut c_void) -> u8 {
    state(hdl)
        .runner
        .as_ref()
        .map(|r| r.running.load(std::sync::atomic::Ordering::SeqCst))
        .unwrap_or(false) as u8
}

/// Block for an async run and move the engines back.
#[no_mangle]
pub extern "C" fn bk_sync(hdl: *mut c_void) -> u64 {
    let st = state(hdl);
    if let Some(r) = st.runner.take() {
        if let Ok((engines, rc)) = r.join.join() {
            st.engines = engines.0;
            st.exit_status = rc;
        }
        // a panicked worker leaves the engines LOST; answer inertly
        // rather than aborting the bluetcl session
        if st.engines.is_empty() {
            eprintln!("trs capi: async worker died; session is inert");
            return 0;
        }
        let now = st.primary().now();
        st.ui_events.retain(|&t| t > now);
    }
    // the kernel's bk_sync returns RAW sim_time (no timescale)
    let st = state(hdl);
    if st.engines.is_empty() {
        return 0;
    }
    st.primary().now()
}

/// External abort: the run stops at the next slice boundary
/// (bluetcl's `sim stop` = bk_abort_now + bk_sync).
#[no_mangle]
pub extern "C" fn bk_abort_now(hdl: *mut c_void) {
    let st = state(hdl);
    st.aborted = true;
    if let Some(r) = &st.runner {
        r.abort.store(true, std::sync::atomic::Ordering::SeqCst);
    }
}

#[no_mangle]
pub extern "C" fn bk_fataled(hdl: *mut c_void) -> u8 {
    (state(hdl).exit_status == 1) as u8
}

// =================================================================
// Misc

#[repr(C)]
pub struct BkVersionInfo {
    pub name: *const c_char,
    pub build: *const c_char,
    pub creation_time: i64,
}

#[no_mangle]
pub extern "C" fn bk_version(hdl: *mut c_void, out: *mut BkVersionInfo) {
    let st = state(hdl);
    let name = st.cstr("trs");
    let build = st.cstr(env!("CARGO_PKG_VERSION"));
    unsafe {
        (*out).name = name;
        (*out).build = build;
        (*out).creation_time = 0;
    }
}

#[no_mangle]
pub extern "C" fn bk_set_timescale(
    hdl: *mut c_void,
    scale_unit: *const c_char,
    scale_factor: u64,
) -> i32 {
    let unit = unsafe { CStr::from_ptr(scale_unit) }
        .to_string_lossy()
        .into_owned();
    let st = state(hdl);
    if st.engines.is_empty() || st.primary().now() > 0 {
        // the kernel rejects timescale changes mid-simulation
        return BK_ERROR;
    }
    st.timescale = Some((unit, scale_factor));
    for e in &mut st.engines {
        e.interp.set_timescale(scale_factor);
    }
    BK_SUCCESS
}

// =================================================================
// VCD control (`sim vcd [on|off|<file>]` -> these three): routed to
// the PRIMARY engine's writer — the same one the $dump* tasks drive.
// Secondary engines stay quiet (they'd clobber the same file).
// Capability tier: VCD needs the interp engine's def recording; a
// non-interp primary degrades honestly (stderr note + failure) —
// compiled bodies do not record the def values the dump walks read.

/// True iff the primary engine can serve VCD (interp tier); prints
/// the remedy note once per call site otherwise.
fn vcd_capable(st: &mut SimState) -> bool {
    match st.engines.first() {
        Some(e) if e.kind == EngineKind::Interp => true,
        Some(_) => {
            eprintln!(
                "trs: VCD dumping needs the interp engine \
                 (TRS_CAPI_ENGINES=interp); the current primary \
                 engine does not record signal values"
            );
            false
        }
        None => false, // async run in flight
    }
}

#[no_mangle]
pub extern "C" fn bk_set_VCD_file(hdl: *mut c_void, f: *const c_char) -> i32 {
    let st = state(hdl);
    if !vcd_capable(st) {
        return BK_ERROR;
    }
    let name = if f.is_null() {
        None
    } else {
        match unsafe { CStr::from_ptr(f) }.to_str() {
            Ok(s) => Some(s.to_string()),
            Err(_) => return BK_ERROR,
        }
    };
    match st.primary().vcd_set_file(name.as_deref()) {
        Ok(()) => BK_SUCCESS,
        Err(()) => BK_ERROR,
    }
}

#[no_mangle]
pub extern "C" fn bk_enable_VCD_dumping(hdl: *mut c_void) -> u8 {
    let st = state(hdl);
    if !vcd_capable(st) {
        return 0;
    }
    st.primary().vcd_enable() as u8
}

#[no_mangle]
pub extern "C" fn bk_disable_VCD_dumping(hdl: *mut c_void) {
    let st = state(hdl);
    if st.engines.is_empty() {
        return; // async run in flight
    }
    // disable is a no-op when dumping never started — no tier note
    st.primary().vcd_disable();
}

/// External clock definition: master-mode models (bluetcl always
/// passes master=True) own their clocks; the loader dlsyms this
/// regardless.  Answer with the existing handle if the name is
/// known, else BAD_CLOCK (we do not create externally-driven
/// clocks yet).
#[no_mangle]
pub extern "C" fn bk_define_clock(
    hdl: *mut c_void,
    name: *const c_char,
    _initial_value: u32,
    _has_initial_value: u8,
    _first_edge: u64,
    _low_duration: u64,
    _high_duration: u64,
) -> u32 {
    bk_get_clock_by_name(hdl, name)
}
