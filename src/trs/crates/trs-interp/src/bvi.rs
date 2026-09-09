//! BviPrim: an imported BVI Verilog module behind the prim ABI, backed
//! by a Verilator-compiled model (design of record: the KB draft
//! "KB: BVI-via-Verilator design (trs)", v4 sec 4.1/5.3; protocol
//! validated end-to-end by the M0 spike, src/trs/spike/bvi-m0/).
//!
//! The shadow-vector execution model:
//! - Action/ActionValue calls update shadow inputs and ENs -- NO eval
//!   per call; no intermediate port transition reaches Verilator.
//! - Output observation (value method, RDY, AV result) is a FRONTIER:
//!   publish all dirty shadows, eval once, read.  Exactness on the
//!   accepted contract set follows from cone reasoning (every input in
//!   the output's declared cone was supplied by an operation the
//!   schedule ordered before the observation).
//! - Edge commit: batched once per timeslice at the interpreter's
//!   commit point (before flush_reset_pending / the PG_FINAL pass),
//!   three settled phases: (a) publish the final non-clock vector
//!   (args/ENs/gates/reset levels), eval; (b) ALL coincident clock
//!   levels in ONE eval (NBA semantics -- sequential per-edge evals
//!   shoot through two-flop crossings); (c) clear the fired ENs
//!   ((*inhigh*) has no port and stays logically true), eval.
//! - Startup: drive every input to 0 with resets DEASSERTED, one
//!   unconditional eval (initial blocks run); the kernel's t=0 reset
//!   assertion then arrives as a real transition.
//! - Time advances through vlt_advance: for --timing models (the model
//!   has delay constructs) it drains internal delayed events strictly
//!   before the target instant -- one eval per drained slot -- so
//!   delayed NBAs land between the kernel's timeslices exactly where
//!   the reference simulators put them; for delay-free models it is a
//!   plain time set.  A drain legitimately changes outputs with no
//!   input change, so it resets the observe-mode snapshots.
//!
//! TRS_BVI_CHECK=observe: epoch-tracked re-reads with declared-cone
//! attribution; every report is a sound witness of an undeclared
//! influence or a port-protocol violation.  A quiet run proves nothing.

use std::collections::HashMap;
use std::ffi::c_void;

use trs_ir::bvi::{BviContract, BviMethodKind, BviPortKind, BVI_PROP_INHIGH};

use crate::out;
use crate::value::Value;

type NewFn = unsafe extern "C" fn(
    *const std::ffi::c_char,
    i32,
    *const *const std::ffi::c_char,
) -> *mut c_void;
type FreeFn = unsafe extern "C" fn(*mut c_void) -> i32;
type SetFn = unsafe extern "C" fn(*mut c_void, u32, *const u64) -> i32;
type GetFn = unsafe extern "C" fn(*mut c_void, u32, *mut u64) -> i32;
type EvalFn = unsafe extern "C" fn(*mut c_void) -> i32;
type AdvFn = unsafe extern "C" fn(*mut c_void, u64, *mut u64) -> i32;
type FinFn = unsafe extern "C" fn(*mut c_void) -> i32;
type MsgFn = unsafe extern "C" fn() -> *const std::ffi::c_char;
type CbFn = unsafe extern "C" fn(Option<OutCb>, *mut c_void);
type OutCb = unsafe extern "C" fn(*mut c_void, *const std::ffi::c_char);

/// $display/$write from inside the model, routed through the sim's
/// stdout sink so interleaving with rule prints is exact.
unsafe extern "C" fn model_output_cb(_ctx: *mut c_void, text: *const std::ffi::c_char) {
    if text.is_null() {
        return;
    }
    let s = std::ffi::CStr::from_ptr(text).to_string_lossy();
    out::write_str(&s);
}

thread_local! {
    /// Raw +args staged BEFORE instantiation (BviPrim::new hands them
    /// to the model's VerilatedContext; the Verilog flow passes every
    /// command-line +arg to the simulator the same way).  Instantiation
    /// runs before the Interp's own fe.plusargs field is populated, so
    /// the loader stages them here first.
    static PLUSARGS: std::cell::RefCell<Vec<String>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Stage the raw +args for models constructed by the CURRENT thread's
/// upcoming instantiation (called by the loaders before Interp::new*).
pub fn stage_plusargs(args: &[String]) {
    PLUSARGS.with(|p| *p.borrow_mut() = args.to_vec());
}

struct MethodInfo {
    kind: BviMethodKind,
    args: Vec<usize>,
    results: Vec<usize>,
    enable: Option<usize>,
    rdy: Option<usize>,
}

pub struct BviPrim {
    path: String,
    top: String,
    // keep the library alive for the handle's lifetime
    _lib: libloading::Library,
    h: *mut c_void,
    f_free: FreeFn,
    f_set: SetFn,
    f_get: GetFn,
    f_eval: EvalFn,
    f_adv: AdvFn,
    f_finished: FinFn,
    f_msg: MsgFn,

    // per-port: width and limb count (inputs and outputs)
    widths: Vec<u32>,
    limbs: Vec<usize>,
    port_names: Vec<String>,
    is_input: Vec<bool>,
    /// (*inhigh*) enables have no physical port: never set/get them.
    phantom: Vec<bool>,

    methods: HashMap<String, MethodInfo>,
    /// "RDY_<m>" -> rdy port index of method m
    rdy_ports: HashMap<String, usize>,

    // shadow vector: pending (unpublished) input values
    shadow: Vec<Option<Vec<u64>>>,
    /// last published values (suppress no-op publishes)
    published: Vec<Option<Vec<u64>>>,
    dirty: bool, // born true: initial blocks must run at first settle
    /// ENs set by calls this timeslice, cleared at commit phase (c)
    en_group: Vec<usize>,
    /// per contract clock: pending (level, gate) from ticks this slice
    pending_edges: Vec<Option<(bool, bool)>>,
    /// osc / gate port per contract clock
    clk_osc: Vec<usize>,
    clk_gate: Vec<Option<usize>>,
    /// tick-port name -> contract clock indices
    tick_map: HashMap<String, Vec<usize>>,
    /// reset ordinal -> (port index, active_low)
    resets: Vec<(usize, bool)>,
    /// output reset ports (contract out_resets; at most one in v1.2 --
    /// the interpreter routes transitions into its derived-reset network)
    rst_out_ports: Vec<usize>,
    /// last sampled asserted-state per output reset (active-low level)
    rst_out_last: Vec<bool>,
    /// transitions observed since the last take_reset_out poll:
    /// (asserted, immediate)
    rst_out_pending: Vec<(bool, bool)>,
    /// output clock oscillator ports (contract out_clocks; edges route
    /// through the interpreter's dynamic-clock network)
    clk_out_ports: Vec<usize>,
    /// last sampled level per output clock
    clk_out_last: Vec<bool>,
    /// edges observed since the last take_clock_edges_multi poll:
    /// (out-clock ordinal, new level)
    clk_out_pending: Vec<(u32, bool)>,

    now: u64,
    finish_req: bool,
    /// TRS_BVI_TRACE: one line per protocol operation, to stderr.
    trace: bool,

    // -- TRS_BVI_CHECK=observe ------------------------------------
    check: bool,
    epoch: HashMap<usize, u64>,
    /// observed output -> (value, epoch snapshot at observation)
    obs: HashMap<usize, (Vec<u64>, HashMap<usize, u64>)>,
    /// declared cone per output port (+ every clock/reset port)
    cones: HashMap<usize, Vec<usize>>,
    struct_ports: Vec<usize>,
}

fn limbs_of(width: u32) -> usize {
    ((width as usize) + 63) / 64
}

impl BviPrim {
    /// Build (or reuse from cache) the verilated model and construct
    /// the prim.  Errors here are compiler/toolchain bugs or races --
    /// `trs link`/`trs run` already ran the verilate step with clean
    /// error reporting before instantiation -- so failure panics with
    /// the full diagnosis.
    pub fn new(
        c: &BviContract,
        strings: &[String],
        path: &str,
        resolved: Option<Vec<trs_vlt::ResolvedParam>>,
    ) -> BviPrim {
        let plusargs: Vec<String> = PLUSARGS.with(|p| p.borrow().clone());
        let s = |id: u32| strings.get(id as usize).map(String::as_str).unwrap_or("");
        let top = s(c.verilog_name).to_string();
        let opts = trs_vlt::BuildOptions::from_env();
        // Verilation is a BUILD step (ratified 2026-08-23): only the
        // build entry points (trs link, trs vlt build) set
        // TRS_VLT_BUILD and may verilate here; every other load --
        // trs run, artifact runs -- is LOAD-ONLY and a cache miss is a
        // rebuild instruction, never a silent runtime verilation.
        let build_mode = std::env::var_os("TRS_VLT_BUILD").is_some();
        let built = if build_mode {
            trs_vlt::build_model_resolved(c, strings, &opts, resolved.as_deref())
                .unwrap_or_else(|e| panic!("trs bvi: instance {path} ({top}): {e}"))
        } else {
            match trs_vlt::find_model_resolved(c, strings, &opts, resolved.as_deref()) {
                Ok(Some(b)) => b,
                Ok(None) => panic!(
                    "trs bvi: instance {path} ({top}): verilated model not \
                     found in cache {} -- verilation is a build step: \
                     link the design (`trs link <design>.bir`) or run \
                     `trs vlt build <design>.bir` first",
                    opts.cache_dir.display()
                ),
                Err(e) => panic!("trs bvi: instance {path} ({top}): {e}"),
            }
        };
        let lib = unsafe { libloading::Library::new(&built.so_path) }
            .unwrap_or_else(|e| panic!("trs bvi: {}: {e}", built.so_path.display()));

        macro_rules! sym {
            ($name:literal, $ty:ty) => {
                *unsafe { lib.get::<$ty>($name) }.unwrap_or_else(|e| {
                    panic!(
                        "trs bvi: {}: missing {}: {e}",
                        built.so_path.display(),
                        String::from_utf8_lossy($name)
                    )
                })
            };
        }
        let f_new: NewFn = sym!(b"vlt_new", NewFn);
        let f_free: FreeFn = sym!(b"vlt_free", FreeFn);
        let f_set: SetFn = sym!(b"vlt_set", SetFn);
        let f_get: GetFn = sym!(b"vlt_get", GetFn);
        let f_eval: EvalFn = sym!(b"vlt_eval", EvalFn);
        let f_adv: AdvFn = sym!(b"vlt_advance", AdvFn);
        let f_finished: FinFn = sym!(b"vlt_finished", FinFn);
        let f_msg: MsgFn = sym!(b"vlt_fatal_msg", MsgFn);
        let f_cb: CbFn = sym!(b"vlt_set_output_cb", CbFn);
        unsafe { f_cb(Some(model_output_cb), std::ptr::null_mut()) };

        let cpath = std::ffi::CString::new(path).unwrap_or_default();
        // sim plusargs reach the model through commandArgs (the shim
        // passes argv to the per-instance VerilatedContext), so
        // $test$plusargs/$value$plusargs see what the design sees
        let cargs: Vec<std::ffi::CString> = plusargs
            .iter()
            .map(|a| std::ffi::CString::new(format!("+{a}")).unwrap_or_default())
            .collect();
        let argv: Vec<*const std::ffi::c_char> = cargs.iter().map(|c| c.as_ptr()).collect();
        let h = unsafe {
            f_new(
                cpath.as_ptr(),
                argv.len() as i32,
                if argv.is_empty() {
                    std::ptr::null()
                } else {
                    argv.as_ptr()
                },
            )
        };
        assert!(!h.is_null(), "trs bvi: vlt_new failed for {path} ({top})");

        let n = c.ports.len();
        let mut widths = Vec::with_capacity(n);
        let mut limbs = Vec::with_capacity(n);
        let mut port_names = Vec::with_capacity(n);
        let mut is_input = Vec::with_capacity(n);
        let mut phantom = Vec::with_capacity(n);
        for p in &c.ports {
            widths.push(p.width);
            limbs.push(limbs_of(p.width));
            port_names.push(s(p.name).to_string());
            is_input.push(matches!(p.dir, trs_ir::bvi::BviDir::Input));
            phantom.push(p.kind == BviPortKind::Enable && (p.props & BVI_PROP_INHIGH) != 0);
        }

        let mut methods = HashMap::new();
        let mut rdy_ports = HashMap::new();
        for m in &c.methods {
            let name = s(m.name).to_string();
            if let Some(r) = m.rdy {
                rdy_ports.insert(format!("RDY_{name}"), r as usize);
            }
            methods.insert(
                name,
                MethodInfo {
                    kind: m.kind,
                    args: m.args.iter().map(|&a| a as usize).collect(),
                    results: m.results.iter().map(|&r| r as usize).collect(),
                    enable: m.enable.map(|e| e as usize),
                    rdy: m.rdy.map(|r| r as usize),
                },
            );
        }

        let mut clk_osc = Vec::new();
        let mut clk_gate = Vec::new();
        let mut tick_map: HashMap<String, Vec<usize>> = HashMap::new();
        for (ci, cl) in c.clocks.iter().enumerate() {
            clk_osc.push(cl.osc_port as usize);
            clk_gate.push(cl.gate_port.map(|g| g as usize));
            tick_map
                .entry(s(cl.tick_port).to_string())
                .or_default()
                .push(ci);
        }
        let resets: Vec<(usize, bool)> = c
            .resets
            .iter()
            .map(|r| (r.port as usize, r.active_low))
            .collect();
        let rst_out_ports: Vec<usize> = c.out_resets.iter().map(|r| r.port as usize).collect();
        let clk_out_ports: Vec<usize> = c.out_clocks.iter().map(|cl| cl.port as usize).collect();

        let check = std::env::var("TRS_BVI_CHECK")
            .map(|v| v == "observe")
            .unwrap_or(false);
        let trace = std::env::var_os("TRS_BVI_TRACE").is_some();
        let mut cones: HashMap<usize, Vec<usize>> = HashMap::new();
        if check {
            for (o, _) in c
                .ports
                .iter()
                .enumerate()
                .filter(|(_, p)| matches!(p.dir, trs_ir::bvi::BviDir::Output))
            {
                cones.insert(o, c.cone_of(o as u32).iter().map(|&x| x as usize).collect());
            }
        }
        let struct_ports: Vec<usize> = c
            .ports
            .iter()
            .enumerate()
            .filter(|(_, p)| {
                matches!(
                    p.kind,
                    BviPortKind::Clock | BviPortKind::ClockGate | BviPortKind::Reset
                )
            })
            .map(|(i, _)| i)
            .collect();

        let mut prim = BviPrim {
            path: path.to_string(),
            top,
            _lib: lib,
            h,
            f_free,
            f_set,
            f_get,
            f_eval,
            f_adv,
            f_finished,
            f_msg,
            widths,
            limbs,
            port_names,
            is_input,
            phantom,
            methods,
            rdy_ports,
            shadow: vec![None; n],
            published: vec![None; n],
            dirty: true,
            en_group: Vec::new(),
            pending_edges: vec![None; c.clocks.len()],
            clk_osc,
            clk_gate,
            tick_map,
            resets,
            rst_out_ports,
            rst_out_last: Vec::new(),
            rst_out_pending: Vec::new(),
            clk_out_ports,
            clk_out_last: Vec::new(),
            clk_out_pending: Vec::new(),
            now: 0,
            finish_req: false,
            trace,
            check,
            epoch: HashMap::new(),
            obs: HashMap::new(),
            cones,
            struct_ports,
        };

        // constant Port args: driven once, before the startup settle
        for &(pi, ref v) in &c.const_args {
            let w = prim.widths[pi as usize];
            let val = const_value(v, strings, w);
            prim.drive(pi as usize, val.limbs64().to_vec());
        }
        // startup: all inputs at 0 except resets DEASSERTED; one
        // unconditional settle so initial blocks run.  The kernel's t=0
        // reset assertion then arrives as a real transition.
        for i in 0..n {
            if prim.is_input[i] && !prim.phantom[i] && prim.shadow[i].is_none() {
                prim.drive(i, vec![0; prim.limbs[i].max(1)]);
            }
        }
        for &(port, active_low) in &prim.resets.clone() {
            let lv = if active_low { 1 } else { 0 };
            prim.drive(port, vec![lv; 1]);
        }
        prim.publish_and_settle();
        // initial output-reset state (an inverter's output is asserted
        // while its input is deasserted; the interpreter broadcasts
        // initially-asserted nodes at run() start)
        prim.rst_out_last = prim
            .rst_out_ports
            .clone()
            .iter()
            .map(|&p| prim.raw_get(p)[0] == 0)
            .collect();
        // initial output clock levels (dynclk_init registration)
        prim.clk_out_last = prim
            .clk_out_ports
            .clone()
            .iter()
            .map(|&p| prim.raw_get(p)[0] & 1 != 0)
            .collect();
        prim
    }

    /// Initial output clock levels after the startup settle (read by
    /// the interpreter's instantiation arm for dynclk registration).
    pub fn clk_out_initial(&self) -> &[bool] {
        &self.clk_out_last
    }

    /// Sample the output clock oscillator ports; changed levels queue
    /// edges for take_clock_edges_multi.
    fn sample_clk_outs(&mut self) {
        for i in 0..self.clk_out_ports.len() {
            let lvl = self.raw_get(self.clk_out_ports[i])[0] & 1 != 0;
            if lvl != self.clk_out_last[i] {
                self.clk_out_last[i] = lvl;
                self.clk_out_pending.push((i as u32, lvl));
                if self.trace {
                    eprintln!(
                        "bvi[{}] t={} clock out {} -> {}",
                        self.path, self.now, self.port_names[self.clk_out_ports[i]], lvl as u8
                    );
                }
            }
        }
    }

    /// Sample the output reset ports (asserted = level 0, the bsc
    /// active-low convention); changed levels queue transitions for
    /// take_reset_out.
    fn sample_rst_outs(&mut self, immediate: bool) {
        for i in 0..self.rst_out_ports.len() {
            let asserted = self.raw_get(self.rst_out_ports[i])[0] == 0;
            if asserted != self.rst_out_last[i] {
                self.rst_out_last[i] = asserted;
                self.rst_out_pending.push((asserted, immediate));
                if self.trace {
                    eprintln!(
                        "bvi[{}] t={} reset out {} -> asserted={}",
                        self.path, self.now, self.port_names[self.rst_out_ports[i]], asserted
                    );
                }
            }
        }
    }

    /// Bootstrap the output-reset initial condition: called once at run
    /// start AFTER the t=0 reset cascades have propagated into the
    /// model, this settles, samples the true initial levels, and clears
    /// any transitions the cascade staged (the settled state IS the
    /// initial condition, not a transition).  Returns the asserted
    /// state of the single output reset, if any.
    pub fn rst_out_bootstrap_impl(&mut self) -> Option<bool> {
        if self.rst_out_ports.is_empty() {
            return None;
        }
        self.publish_and_settle();
        for i in 0..self.rst_out_ports.len() {
            self.rst_out_last[i] = self.raw_get(self.rst_out_ports[i])[0] == 0;
        }
        self.rst_out_pending.clear();
        self.rst_out_last.first().copied()
    }

    fn drive(&mut self, port: usize, val: Vec<u64>) {
        if self.phantom[port] {
            return;
        }
        if self.published[port].as_ref() == Some(&val) {
            self.shadow[port] = None;
            return;
        }
        self.shadow[port] = Some(val);
    }

    fn drive_value(&mut self, port: usize, v: &Value) {
        let want = self.limbs[port].max(1);
        let mut l = v.limbs64().to_vec();
        l.resize(want, 0);
        self.drive(port, l);
    }

    fn raw_set(&mut self, port: usize, val: &[u64]) {
        let rc = unsafe { (self.f_set)(self.h, port as u32, val.as_ptr()) };
        if rc != 0 {
            self.die("vlt_set", rc);
        }
    }

    fn raw_get(&mut self, port: usize) -> Vec<u64> {
        let mut buf = vec![0u64; self.limbs[port].max(1)];
        let rc = unsafe { (self.f_get)(self.h, port as u32, buf.as_mut_ptr()) };
        if rc != 0 {
            self.die("vlt_get", rc);
        }
        // mask to width (the shim marshals raw member bytes)
        let w = self.widths[port] as usize;
        let top_bits = w % 64;
        if top_bits != 0 {
            let last = buf.len() - 1;
            buf[last] &= (1u64 << top_bits) - 1;
        }
        buf
    }

    fn eval(&mut self) {
        let rc = unsafe { (self.f_eval)(self.h) };
        if rc != 0 {
            self.die("eval", rc);
        }
        self.check_finished();
    }

    fn check_finished(&mut self) {
        let fin = unsafe { (self.f_finished)(self.h) };
        if fin & 2 != 0 {
            self.die("model fatal", -1);
        }
        if fin & 1 != 0 {
            self.finish_req = true;
        }
    }

    /// Bring the model's time to `to`.  On a --timing model this drains
    /// internal delayed events strictly before `to` (one eval per
    /// drained slot); a drain legitimately changes outputs with no
    /// input change, so the observe-mode snapshots reset.
    fn advance_to(&mut self, to: u64) {
        let mut drained: u64 = 0;
        let rc = unsafe { (self.f_adv)(self.h, to, &mut drained) };
        if rc != 0 {
            self.die("vlt_advance", rc);
        }
        if drained > 0 {
            if self.trace {
                eprintln!(
                    "bvi[{}] t={to} advance drained {drained} event slot(s)",
                    self.path
                );
            }
            self.check_finished();
            if self.check {
                self.obs.clear();
            }
        }
    }

    fn die(&self, what: &str, rc: i32) -> ! {
        let msg = unsafe {
            let p = (self.f_msg)();
            if p.is_null() {
                String::new()
            } else {
                std::ffi::CStr::from_ptr(p).to_string_lossy().to_string()
            }
        };
        panic!(
            "trs bvi: instance {} ({}): {what} failed (rc={rc}){}{}",
            self.path,
            self.top,
            if msg.is_empty() { "" } else { ": " },
            msg
        );
    }

    fn publish_and_settle(&mut self) {
        // drain internal delayed events up to `now` BEFORE publishing:
        // inputs staged in this timeslice must not be visible to events
        // at earlier instants
        self.advance_to(self.now);
        let mut any = false;
        for i in 0..self.shadow.len() {
            if let Some(v) = self.shadow[i].take() {
                self.raw_set(i, &v);
                self.published[i] = Some(v);
                if self.check {
                    *self.epoch.entry(i).or_insert(0) += 1;
                }
                any = true;
            }
        }
        if any || self.dirty {
            self.eval();
            self.dirty = false;
            if self.check {
                self.check_observed();
            }
        }
    }

    /// Observation frontier: publish + settle, read the port.
    fn observe(&mut self, port: usize) -> Value {
        self.publish_and_settle();
        let buf = self.raw_get(port);
        if self.check {
            self.obs.insert(port, (buf.clone(), self.epoch.clone()));
        }
        Value::from_limbs64(self.widths[port], buf)
    }

    /// TRS_BVI_CHECK=observe: a previously-observed output changed while
    /// nothing in its DECLARED cone (nor any clock/reset port) moved --
    /// a sound witness of an undeclared influence.
    fn check_observed(&mut self) {
        let keys: Vec<usize> = self.obs.keys().copied().collect();
        for port in keys {
            let (old, snap) = self.obs.get(&port).cloned().unwrap();
            let cur = self.raw_get(port);
            if cur == old {
                continue;
            }
            let mut cone: Vec<usize> = self.cones.get(&port).cloned().unwrap_or_default();
            cone.extend_from_slice(&self.struct_ports);
            let cone_moved = cone.iter().any(|p| {
                self.epoch.get(p).copied().unwrap_or(0) != snap.get(p).copied().unwrap_or(0)
            });
            if !cone_moved {
                let changed: Vec<&str> = self
                    .epoch
                    .iter()
                    .filter(|(p, e)| snap.get(p).copied().unwrap_or(0) != **e)
                    .map(|(p, _)| self.port_names[*p].as_str())
                    .collect();
                eprintln!(
                    "trs bvi observe: DYNAMIC_LIE witness: instance {} ({}) output \
                     '{}' changed ({:x?} -> {:x?}) with no declared-cone input \
                     change; inputs that DID change: [{}] -- undeclared influence \
                     or port-protocol violation",
                    self.path,
                    self.top,
                    self.port_names[port],
                    old,
                    cur,
                    changed.join(", ")
                );
            }
            self.obs.insert(port, (cur, self.epoch.clone()));
        }
    }

    fn method(&self, name: &str) -> &MethodInfo {
        self.methods.get(name).unwrap_or_else(|| {
            panic!(
                "trs bvi: instance {} ({}): no method '{name}' in contract",
                self.path, self.top
            )
        })
    }

    fn single_result(&self, name: &str) -> usize {
        let m = self.method(name);
        match m.results.as_slice() {
            [r] => *r,
            rs => panic!(
                "trs bvi: instance {} ({}): method '{name}' has {} result \
                 ports; multi-port results are not supported yet",
                self.path,
                self.top,
                rs.len()
            ),
        }
    }

    fn drive_call(&mut self, name: &str, args: &[Value]) {
        let m = self.method(name);
        let arg_ports: Vec<usize> = m.args.clone();
        let en = m.enable;
        assert_eq!(
            arg_ports.len(),
            args.len(),
            "trs bvi: instance {} ({}): method '{name}' called with {} args, \
             contract has {}",
            self.path,
            self.top,
            args.len(),
            arg_ports.len()
        );
        for (p, v) in arg_ports.iter().zip(args) {
            self.drive_value(*p, v);
        }
        if let Some(en) = en {
            if !self.phantom[en] {
                self.drive(en, vec![1]);
                if !self.en_group.contains(&en) {
                    self.en_group.push(en);
                }
            }
        }
    }
}

fn const_value(v: &trs_ir::bvi::BviParamValue, strings: &[String], width: u32) -> Value {
    use trs_ir::bvi::BviParamValue as P;
    match v {
        P::IntSigned { value, .. } => {
            // two's complement into the port width
            let mut l = vec![0u64; limbs_of(width).max(1)];
            let x = *value as u64;
            l[0] = x;
            if *value < 0 {
                for limb in l.iter_mut().skip(1) {
                    *limb = u64::MAX;
                }
            }
            let mut val = Value::from_limbs64(width, l);
            val = Value::from_limbs64(width, val.limbs64().to_vec());
            val
        }
        P::Bits { hex, .. } => {
            let h = strings
                .get(*hex as usize)
                .map(String::as_str)
                .unwrap_or("0");
            let mut l = vec![0u64; limbs_of(width).max(1)];
            for (i, c) in h.bytes().rev().enumerate() {
                let d = (c as char).to_digit(16).unwrap_or(0) as u64;
                let bit = i * 4;
                if bit / 64 < l.len() {
                    l[bit / 64] |= d << (bit % 64);
                }
            }
            Value::from_limbs64(width, l)
        }
        P::Str(_) | P::Real(_) => {
            panic!("trs bvi: string/real constant port arguments are not supported")
        }
        P::FromArg { .. } => {
            // const_args carry literal values only (forwarding applies
            // to -G parameters, not construction-time port drives)
            panic!("trs bvi: forwarded constant port arguments are not supported")
        }
    }
}

impl Drop for BviPrim {
    fn drop(&mut self) {
        unsafe {
            let _ = (self.f_free)(self.h);
        }
    }
}

impl crate::prim::Prim for BviPrim {
    fn value_method(&mut self, method: &str, port: u32, args: &[Value], now: u64) -> Value {
        self.now = now;
        // `port` selects among a method's several result ports.  A
        // multi-result BVI import is refused at export, so an accepted
        // contract only ever has port 0 -- assert rather than ignore,
        // since a nonzero one would mean the refusal has a hole.
        assert_eq!(
            port, 0,
            "trs bvi: instance {} ({}): '{method}' called at port {port}; \
             multi-result imports are refused at export",
            self.path, self.top
        );
        if self.trace {
            eprintln!("bvi[{}] t={now} value {method}", self.path);
        }
        if let Some(&r) = self.rdy_ports.get(method) {
            return self.observe(r);
        }
        let m = self.method(method);
        assert!(
            m.kind == BviMethodKind::Value,
            "trs bvi: instance {} ({}): '{method}' is not a value method",
            self.path,
            self.top
        );
        // value-method args are frontier inputs too (published with
        // everything else; cross-method influence from value args is
        // refused at export)
        let arg_ports = m.args.clone();
        for (p, v) in arg_ports.iter().zip(args) {
            self.drive_value(*p, v);
        }
        let r = self.single_result(method);
        let v = self.observe(r);
        if self.trace {
            eprintln!(
                "bvi[{}] t={} value {method} -> {:x?}",
                self.path,
                self.now,
                v.limbs64()
            );
        }
        v
    }

    fn action_method(&mut self, method: &str, port: u32, args: &[Value], now: u64) {
        self.now = now;
        assert_eq!(
            port, 0,
            "trs bvi: instance {} ({}): '{method}' called at port {port}; \
             multi-result imports are refused at export",
            self.path, self.top
        );
        if self.trace {
            eprintln!("bvi[{}] t={now} action {method}", self.path);
        }
        self.drive_call(method, args);
    }

    fn actionvalue_method(&mut self, method: &str, args: &[Value], now: u64) -> Value {
        self.now = now;
        self.drive_call(method, args);
        // the AV result read is an observation frontier at the call:
        // per-call semantics (a later self-SBR replacement call by a
        // later rule legitimately re-drives the args; the atomic-read
        // condition was enforced at export)
        let r = self.single_result(method);
        self.observe(r)
    }

    fn tick(&mut self, port: &str, now: u64, clk_val: bool, gate: bool) {
        self.now = now;
        if self.trace {
            eprintln!(
                "bvi[{}] t={now} tick {port} lvl={} gate={}",
                self.path, clk_val as u8, gate as u8
            );
        }
        let Some(cis) = self.tick_map.get(port) else {
            return;
        };
        for &ci in cis.clone().iter() {
            self.pending_edges[ci] = Some((clk_val, gate));
        }
    }

    fn set_reset_input(&mut self, input: usize, asserted: bool) {
        let Some(&(port, active_low)) = self.resets.get(input) else {
            return;
        };
        let lv = if asserted == active_low { 0 } else { 1 };
        // a LEVEL input: lands at the next publish (frontier or commit
        // phase a) -- the t=0 assertion after the deasserted startup
        // settle is a real transition
        self.drive(port, vec![lv]);
        // a model with an output reset may derive it combinationally
        // from this input (ResetInverter): settle now so the transition
        // is observed, but DEFER it to the end-of-timeslice flush --
        // Bluesim's reset network applies derived transitions at
        // reset_at_end_of_timeslice, so rules see them one slice later
        if !self.rst_out_ports.is_empty() {
            self.publish_and_settle();
            self.sample_rst_outs(false);
        }
    }

    fn set_in_reset(&mut self, asserted: bool) {
        self.set_reset_input(0, asserted);
    }

    fn tick_is_noop(&self) -> bool {
        false
    }

    /// The batched three-phase edge commit (per instance, once per
    /// timeslice, called by the interpreter's commit point).  Returns
    /// true if the model requested $finish.
    fn bvi_commit(&mut self, now: u64) -> bool {
        self.now = now;
        if self.trace {
            eprintln!(
                "bvi[{}] t={now} commit edges={:?} en={:?}",
                self.path, self.pending_edges, self.en_group
            );
        }
        // drain internal delayed events up to this instant even when the
        // slice stages nothing for this instance (a --timing model's
        // scheduled events fire between its edges regardless)
        self.advance_to(now);
        let any_edge = self.pending_edges.iter().any(|e| e.is_some());
        let any_input = self.shadow.iter().any(|s| s.is_some());
        if !any_edge && !any_input && self.en_group.is_empty() {
            // a --timing drain above may still have moved an output
            // reset (flush_reset_pending runs right after this point);
            // --timing + output clocks is refused at build
            self.sample_rst_outs(false);
            self.sample_clk_outs();
            return std::mem::take(&mut self.finish_req);
        }
        // (a) inputs: final selected non-clock vector -- args, ENs,
        // gates (levels settled pre-edge), reset levels
        for ci in 0..self.pending_edges.len() {
            if let Some((_, gate)) = self.pending_edges[ci] {
                if let Some(g) = self.clk_gate[ci] {
                    self.drive(g, vec![gate as u64]);
                }
            }
        }
        self.publish_and_settle();
        // (b) edges: all coincident clock levels, ONE eval (NBA)
        if any_edge {
            let mut moved = false;
            for ci in 0..self.pending_edges.len() {
                if let Some((lvl, _)) = self.pending_edges[ci].take() {
                    let osc = self.clk_osc[ci];
                    let v = vec![lvl as u64];
                    if self.published[osc].as_ref() != Some(&v) {
                        self.raw_set(osc, &v);
                        self.published[osc] = Some(v);
                        if self.check {
                            *self.epoch.entry(osc).or_insert(0) += 1;
                        }
                        moved = true;
                    }
                }
            }
            if moved {
                // time is already at `now` (the advance above; phase a
                // re-advances as a no-op)
                self.eval();
            }
        }
        // (c) post: clear the fired ENs ((*inhigh*) has no port), settle
        if !self.en_group.is_empty() {
            for en in std::mem::take(&mut self.en_group) {
                self.raw_set(en, &[0]);
                self.published[en] = Some(vec![0]);
                if self.check {
                    *self.epoch.entry(en).or_insert(0) += 1;
                }
            }
            self.eval();
        }
        self.dirty = false;
        // a new instant: edge commits legitimately change outputs
        self.obs.clear();
        // output resets move with the edge (deferred: applied by
        // flush_reset_pending at end of timeslice, the
        // reset_at_end_of_timeslice semantics); output clock edges are
        // collected by the commit point and heaped at this instant
        self.sample_rst_outs(false);
        self.sample_clk_outs();
        std::mem::take(&mut self.finish_req)
    }

    fn take_reset_out(&mut self) -> Vec<(bool, bool)> {
        std::mem::take(&mut self.rst_out_pending)
    }

    fn reset_out_bootstrap(&mut self) -> Option<bool> {
        self.rst_out_bootstrap_impl()
    }

    fn take_clock_edges_multi(&mut self) -> Vec<(u32, bool)> {
        std::mem::take(&mut self.clk_out_pending)
    }
}
