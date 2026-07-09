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

/// The `tSimStateHdl` behind every `bk_*` call.
pub struct SimState {
    interp: Interp,
    /// plusargs staged before/after init (`bk_append_argument`)
    args: Vec<String>,
    /// interned CStrings handed out by `bk_*` name accessors (the C
    /// side treats them as borrowed; they must outlive the handle)
    names: Vec<CString>,
    /// exit protocol mirror (bk_finished / bk_exit_status / bk_fataled)
    exit_status: i32,
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
    let interp = match Interp::from_bir_bytes(bir) {
        Ok(i) => i,
        Err(e) => {
            eprintln!("trs capi: bk_init: {e}");
            return std::ptr::null_mut();
        }
    };
    let st = Box::new(SimState {
        interp,
        args: Vec::new(),
        names: Vec::new(),
        exit_status: 0,
    });
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
    state(hdl).interp.now()
}

/// `bk_append_argument`: stage a plusarg.
#[no_mangle]
pub extern "C" fn bk_append_argument(hdl: *mut c_void, arg: *const c_char) {
    let s = unsafe { CStr::from_ptr(arg) }.to_string_lossy().into_owned();
    let st = state(hdl);
    st.interp.append_plusarg(&s);
    st.args.push(s);
}

/// `bk_finished`: has $finish been called.
#[no_mangle]
pub extern "C" fn bk_finished(hdl: *mut c_void) -> u8 {
    state(hdl).interp.is_finished() as u8
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
