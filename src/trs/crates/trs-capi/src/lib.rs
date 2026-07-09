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
        vec![EngineKind::Interp]
    } else {
        kinds
    };
    let mut engines = Vec::new();
    for kind in kinds {
        match Interp::from_bir_bytes(bir) {
            Ok(interp) => engines.push(Engine { interp, kind }),
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
    });
    // one-time event-loop setup: clocks resolved, kernel reset
    // protocol seeded — `sim clock` works right after `sim load`
    for e in &mut st.engines {
        e.interp.prime();
    }
    Box::into_raw(st) as *mut c_void
}

/// `bk_shutdown`: free everything.  bluetcl dlcloses afterwards.
#[no_mangle]
pub extern "C" fn bk_shutdown(hdl: *mut c_void) {
    if !hdl.is_null() {
        drop(unsafe { Box::from_raw(hdl as *mut SimState) });
    }
}

/// `bk_now`: current simulation time.
#[no_mangle]
pub extern "C" fn bk_now(hdl: *mut c_void) -> u64 {
    state(hdl).primary().now()
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
    state(hdl).primary().is_finished() as u8
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
        self.primary().clock_info().into_iter().nth(h as usize)
    }
}

#[no_mangle]
pub extern "C" fn bk_num_clocks(hdl: *mut c_void) -> u32 {
    state(hdl).primary().clock_info().len() as u32
}

#[no_mangle]
pub extern "C" fn bk_get_nth_clock(hdl: *mut c_void, n: u32) -> u32 {
    if (n as usize) < state(hdl).primary().clock_info().len() {
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
    state(hdl)
        .primary()
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

#[no_mangle]
pub extern "C" fn bk_advance(hdl: *mut c_void, _async: u8) -> i32 {
    let st = state(hdl);
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
    let cond = trs_interp::StopCond {
        max_cycles: u64::MAX,
        edge_limits,
        at_times: st.ui_events.clone(),
    };
    let mut rc = BK_SUCCESS;
    // primary first (owns stdout); oracle comparison lands with the
    // quiet flag for secondaries
    for e in &mut st.engines {
        let r = e.interp.advance_until(&cond);
        if r != 0 {
            rc = r;
        }
    }
    st.exit_status = rc;
    // UI events at or before the stop time have fired
    let now = st.primary().now();
    st.ui_events.retain(|&t| t > now);
    BK_SUCCESS
}

#[no_mangle]
pub extern "C" fn bk_is_running(_hdl: *mut c_void) -> u8 {
    0
}

#[no_mangle]
pub extern "C" fn bk_sync(hdl: *mut c_void) -> u64 {
    state(hdl).primary().now()
}

#[no_mangle]
pub extern "C" fn bk_abort_now(hdl: *mut c_void) {
    state(hdl).aborted = true;
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
    st.timescale = Some((unit, scale_factor));
    BK_SUCCESS
}

// =================================================================
// Symbol tree — NOT YET BUILT (docs/TCL-CAPI.md rung 2).  The
// loader dlsyms every name up front, so these must exist; they
// answer in the reference API's absence vocabulary (NULL root,
// empty modules) until the tree lands.

#[no_mangle]
pub extern "C" fn bk_top_symbol(_hdl: *mut c_void) -> *mut c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn bk_lookup_symbol(
    _root: *mut c_void,
    _name: *const c_char,
) -> *mut c_void {
    std::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn bk_get_key(_sym: *mut c_void) -> *const c_char {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn bk_get_size(_sym: *mut c_void) -> u32 {
    0
}

#[no_mangle]
pub extern "C" fn bk_is_module(_sym: *mut c_void) -> u8 {
    0
}

#[no_mangle]
pub extern "C" fn bk_is_rule(_sym: *mut c_void) -> u8 {
    0
}

#[no_mangle]
pub extern "C" fn bk_is_single_value(_sym: *mut c_void) -> u8 {
    0
}

#[no_mangle]
pub extern "C" fn bk_is_value_range(_sym: *mut c_void) -> u8 {
    0
}

#[no_mangle]
pub extern "C" fn bk_peek_symbol_value(_sym: *mut c_void) -> *const u32 {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn bk_get_range_min_addr(_sym: *mut c_void) -> u64 {
    0
}

#[no_mangle]
pub extern "C" fn bk_get_range_max_addr(_sym: *mut c_void) -> u64 {
    0
}

#[no_mangle]
pub extern "C" fn bk_peek_range_value(
    _sym: *mut c_void,
    _addr: u64,
) -> *const u32 {
    std::ptr::null()
}

#[no_mangle]
pub extern "C" fn bk_num_symbols(_sym: *mut c_void) -> u32 {
    0
}

#[no_mangle]
pub extern "C" fn bk_get_nth_symbol(_sym: *mut c_void, _n: u32) -> *mut c_void {
    std::ptr::null_mut()
}

// =================================================================
// VCD control — wiring to the interp's writer lands with rung 4;
// stubs keep the dlsym set complete.

#[no_mangle]
pub extern "C" fn bk_set_VCD_file(_hdl: *mut c_void, _f: *const c_char) -> i32 {
    BK_ERROR
}

#[no_mangle]
pub extern "C" fn bk_enable_VCD_dumping(_hdl: *mut c_void) -> u8 {
    0
}

#[no_mangle]
pub extern "C" fn bk_disable_VCD_dumping(_hdl: *mut c_void) {}

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
