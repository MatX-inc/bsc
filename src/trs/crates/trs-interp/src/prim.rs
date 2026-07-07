//! Primitive state elements, dispatched by their BSV primitive-module
//! name (BIR currently exports all primitives as `Other { name }`).
//! Semantics reference: `src/bluesim/bs_prim_mod_*.h`; the load-bearing
//! pattern is in-place mutation plus begin-of-cycle snapshots guarded by
//! a cycle stamp (see trs-rt and DESIGN.md section 4).
//!
//! Unknown primitives and methods fail loudly — this is the oracle, and
//! silent wrong answers are the one unforgivable bug.

use crate::value::Value;

pub trait Prim {
    /// Value-method call (pure read).
    fn value_method(&mut self, method: &str, args: &[Value], now: u64) -> Value;
    /// Action-method call (mutates).
    fn action_method(&mut self, method: &str, args: &[Value], now: u64);
    /// ActionValue-method call.
    fn actionvalue_method(&mut self, method: &str, args: &[Value], now: u64) -> Value {
        let _ = (method, args, now);
        panic!("primitive has no actionvalue methods");
    }
    /// End-of-edge tick (RWire clear, CReg rotate, ...).
    fn tick(&mut self, port: &str);
}

/// Construct a primitive by BSV name.  `width` and other shape facts are
/// recovered from the constant instantiation args (clock/reset args are
/// filtered out by the caller; `consts` holds the remaining constants in
/// order).
pub fn make_prim(name: &str, consts: &[Value], strs: &[String]) -> Box<dyn Prim> {
    match name {
        // registers: args (after clock/reset) are [width, init] or [width]
        "RegN" | "RegA" => Box::new(Reg::new(consts, true, name == "RegA")),
        "RegUN" => Box::new(Reg::new(consts, false, false)),
        // a reverting virtual reg exists for scheduling; Bluesim models it
        // as a plain reg (primMap maps RevertReg to the Reg class, no tick)
        "RevertReg" => Box::new(Reg::new(consts, true, false)),
        "Probe" | "ProbeWire" => Box::new(Probe),
        // no reset modeling yet: reset outputs read as deasserted
        "ResetToBool" => Box::new(ResetToBool),
        "RegFile" => Box::new(RegFile::new(consts, None)),
        "RegFileLoad" => Box::new(RegFile::new(consts, strs.first().cloned())),
        "ConfigRegN" | "ConfigRegA" => Box::new(ConfigReg::new(consts, true)),
        "ConfigRegUN" => Box::new(ConfigReg::new(consts, false)),
        "RWire" => Box::new(RWire::new(consts, false)),
        "RWire0" => Box::new(RWire::new(consts, true)),
        "BypassWire" => Box::new(BypassWire::new(consts, false)),
        "BypassWire0" => Box::new(BypassWire::new(consts, true)),
        "CRegN5" | "CRegA5" | "CRegUN5" => Box::new(CReg::new(consts, !name.ends_with("UN5"))),
        "FIFO1" => Box::new(Fifo::new(consts, 1, false, false)),
        "FIFO2" => Box::new(Fifo::new(consts, 2, false, false)),
        "FIFO10" => Box::new(Fifo::new(consts, 1, false, true)),
        "FIFO20" => Box::new(Fifo::new(consts, 2, false, true)),
        "FIFOL1" => Box::new(Fifo::new(consts, 1, true, false)),
        "FIFOL10" => Box::new(Fifo::new(consts, 1, true, true)),
        "SizedFIFO" => Box::new(Fifo::new_sized(consts, false)),
        "SizedFIFO0" => Box::new(Fifo::new_sized(consts, true)),
        "SizedFIFOL" => Box::new(Fifo::new_sized_loopy(consts)),
        _ => panic!("trs-interp: unimplemented primitive {name:?} (P1 bring-up)"),
    }
}

// ===============

/// Probe: waveform-only sink.
struct Probe;

impl Prim for Probe {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("Probe: unknown value method {method:?}")
    }
    fn action_method(&mut self, _method: &str, _args: &[Value], _now: u64) {}
    fn tick(&mut self, _port: &str) {}
}

/// ResetToBool without reset modeling: reads as "not in reset".
struct ResetToBool;

impl Prim for ResetToBool {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "isAsserted" | "_read" | "read" => Value::from_u64(1, 0),
            m => panic!("ResetToBool: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, _args: &[Value], _now: u64) {
        panic!("ResetToBool: unknown action method {method:?}")
    }
    fn tick(&mut self, _port: &str) {}
}

// ===============

/// RegFile (bs_prim_mod_regfile.h): sparse storage, read-before-write with
/// one-entry write forwarding (a same-cycle earlier upd to the same
/// address reads the pre-write value).
struct RegFile {
    data: std::collections::HashMap<u64, Value>,
    width: u32,
    upd_at: u64,
    upd_addr: u64,
    upd_prev: Value,
}

impl RegFile {
    fn new(consts: &[Value], file: Option<String>) -> RegFile {
        // args: [addr_width, data_width, lo, hi] (file name separate)
        let width = carg(consts, 1) as u32;
        let mut rf = RegFile {
            data: Default::default(),
            width,
            upd_at: u64::MAX,
            upd_addr: 0,
            upd_prev: Value::undet(width),
        };
        if let Some(f) = file {
            rf.load_memfile(&f);
        }
        rf
    }

    /// Minimal $readmemh loader: @addr directives, hex words, //, /* */
    /// and # comments (bs_mem_file.h grammar subset).
    fn load_memfile(&mut self, path: &str) {
        let text = std::fs::read_to_string(path)
            .unwrap_or_else(|e| panic!("RegFileLoad: cannot read {path:?}: {e}"));
        let mut cleaned = String::new();
        let mut chars = text.chars().peekable();
        while let Some(c) = chars.next() {
            match c {
                '/' if chars.peek() == Some(&'/') => {
                    while let Some(&n) = chars.peek() {
                        if n == '\n' { break; }
                        chars.next();
                    }
                }
                '/' if chars.peek() == Some(&'*') => {
                    chars.next();
                    let mut prev = ' ';
                    for n in chars.by_ref() {
                        if prev == '*' && n == '/' { break; }
                        prev = n;
                    }
                }
                _ => cleaned.push(c),
            }
        }
        let mut addr: u64 = 0;
        for tok in cleaned.split_whitespace() {
            if let Some(a) = tok.strip_prefix('@') {
                addr = u64::from_str_radix(a, 16)
                    .unwrap_or_else(|_| panic!("RegFileLoad: bad address {tok:?}"));
            } else {
                let clean: String = tok.chars().filter(|c| *c != '_').collect();
                let mut v = Value::zero(self.width);
                for c in clean.chars() {
                    let d = c.to_digit(16)
                        .unwrap_or_else(|| panic!("RegFileLoad: bad datum {tok:?}"));
                    v = v.shl(4, self.width).or(&Value::from_u64(self.width, d as u64), self.width);
                }
                self.data.insert(addr, v);
                addr += 1;
            }
        }
    }
}

impl Prim for RegFile {
    fn value_method(&mut self, method: &str, args: &[Value], now: u64) -> Value {
        match method {
            "sub" => {
                let a = args[0].as_u64();
                if self.upd_at == now && self.upd_addr == a {
                    return self.upd_prev.clone();
                }
                self.data
                    .get(&a)
                    .cloned()
                    .unwrap_or_else(|| Value::undet(self.width))
            }
            m => panic!("RegFile: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], now: u64) {
        match method {
            "upd" => {
                let a = args[0].as_u64();
                if self.upd_at != now || self.upd_addr != a {
                    self.upd_prev = self
                        .data
                        .get(&a)
                        .cloned()
                        .unwrap_or_else(|| Value::undet(self.width));
                    self.upd_at = now;
                    self.upd_addr = a;
                }
                self.data.insert(a, args[1].clone());
            }
            m => panic!("RegFile: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, _port: &str) {}
}

fn carg(consts: &[Value], i: usize) -> u64 {
    consts.get(i).map(|v| v.as_u64()).unwrap_or(0)
}

// ===============

/// Reg / RegU (bs_prim_mod_reg.h): read returns current value; write is
/// immediate.  Registered semantics come from the static schedule order.
struct Reg {
    value: Value,
}

impl Reg {
    fn new(consts: &[Value], has_reset: bool, _async_rst: bool) -> Reg {
        // instantiation args: [width, init] for RegN/RegA, [width] for RegUN
        let width = carg(consts, 0) as u32;
        let value = if has_reset && consts.len() > 1 {
            consts[1].zext(width)
        } else {
            Value::undet(width)
        };
        Reg { value }
    }
}

impl Prim for Reg {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "read" | "get" => self.value.clone(),
            m => panic!("Reg: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], _now: u64) {
        match method {
            "write" | "set" | "put" => self.value = args[0].clone(),
            m => panic!("Reg: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, _port: &str) {}
}

// ===============

/// ConfigReg: reads always see the begin-of-cycle value regardless of
/// same-cycle writes (bs_prim_mod_reg.h:475).
struct ConfigReg {
    value: Value,
    old_value: Value,
    written_at: u64,
}

impl ConfigReg {
    fn new(consts: &[Value], has_reset: bool) -> ConfigReg {
        let width = carg(consts, 0) as u32;
        let value = if has_reset && consts.len() > 1 {
            consts[1].zext(width)
        } else {
            Value::undet(width)
        };
        ConfigReg { old_value: value.clone(), value, written_at: u64::MAX }
    }
}

impl Prim for ConfigReg {
    fn value_method(&mut self, method: &str, _args: &[Value], now: u64) -> Value {
        match method {
            "read" | "get" => {
                if self.written_at == now {
                    self.old_value.clone()
                } else {
                    self.value.clone()
                }
            }
            m => panic!("ConfigReg: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], now: u64) {
        match method {
            "write" | "set" | "put" => {
                if self.written_at != now {
                    self.old_value = self.value.clone();
                    self.written_at = now;
                }
                self.value = args[0].clone();
            }
            m => panic!("ConfigReg: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, _port: &str) {}
}

// ===============

/// RWire / PulseWire (bs_prim_mod_wire.h): valid only within the cycle
/// it is set; tick clears.
struct RWire {
    width: u32,
    value: Value,
    valid: bool,
}

impl RWire {
    fn new(consts: &[Value], zero_width: bool) -> RWire {
        let width = if zero_width { 0 } else { carg(consts, 0) as u32 };
        RWire { width, value: Value::zero(width.max(1)), valid: false }
    }
}

impl Prim for RWire {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "whas" => Value::from_u64(1, self.valid as u64),
            "wget" => self.value.clone(),
            m => panic!("RWire: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], _now: u64) {
        match method {
            "wset" | "send" => {
                if self.width > 0 {
                    self.value = args[0].clone();
                }
                self.valid = true;
            }
            m => panic!("RWire: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, _port: &str) {
        self.valid = false;
    }
}

// ===============

/// BypassWire: combinational wire, always written each cycle.
struct BypassWire {
    value: Value,
}

impl BypassWire {
    fn new(consts: &[Value], zero_width: bool) -> BypassWire {
        let width = if zero_width { 1 } else { carg(consts, 0) as u32 };
        BypassWire { value: Value::zero(width.max(1)) }
    }
}

impl Prim for BypassWire {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "wget" | "read" => self.value.clone(),
            "whas" => Value::from_u64(1, 1),
            m => panic!("BypassWire: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], _now: u64) {
        match method {
            "wset" | "write" => self.value = args[0].clone(),
            m => panic!("BypassWire: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, _port: &str) {}
}

// ===============

/// CReg with up to 5 ports (bs_prim_mod_reg.h:817): sequential port
/// writes are immediate; tick commits the registered view.
struct CReg {
    value: Value,       // live value, mutated by port writes
    value_reg: Value,   // value registered at the last edge
}

impl CReg {
    fn new(consts: &[Value], has_reset: bool) -> CReg {
        let width = carg(consts, 0) as u32;
        let init = if has_reset && consts.len() > 1 {
            consts[1].zext(width)
        } else {
            Value::undet(width)
        };
        CReg { value: init.clone(), value_reg: init }
    }
}

impl Prim for CReg {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        // portK__read returns the live value (port 0 sees the registered
        // value at cycle start because nothing has written yet)
        if method.starts_with("port") && method.ends_with("__read") {
            self.value.clone()
        } else {
            panic!("CReg: unknown value method {method:?}")
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], _now: u64) {
        if method.starts_with("port") && method.ends_with("__write") {
            self.value = args[0].clone();
        } else {
            panic!("CReg: unknown action method {method:?}")
        }
    }
    fn tick(&mut self, _port: &str) {
        self.value_reg = self.value.clone();
    }
}

// ===============

/// FIFO (bs_prim_mod_fifo.h): in-place mutation with a begin-of-cycle
/// snapshot for the conflict-free status methods.
struct Fifo {
    data: std::collections::VecDeque<Value>,
    depth: usize,
    loopy: bool,
    zero_width: bool,
    width: u32,
    stamp: u64,
    saved_len: usize,
}

impl Fifo {
    fn new(consts: &[Value], depth: usize, loopy: bool, zero_width: bool) -> Fifo {
        let width = if zero_width { 0 } else { carg(consts, 0) as u32 };
        Fifo {
            data: Default::default(),
            depth,
            loopy,
            zero_width,
            width,
            stamp: u64::MAX,
            saved_len: 0,
        }
    }

    fn new_sized(consts: &[Value], zero_width: bool) -> Fifo {
        // SizedFIFO args: [width, depth, cntr_width] (p1/p2/p3)
        let width = if zero_width { 0 } else { carg(consts, 0) as u32 };
        let depth = if zero_width { carg(consts, 0) } else { carg(consts, 1) } as usize;
        Fifo {
            data: Default::default(),
            depth: depth.max(1),
            loopy: false,
            zero_width,
            width,
            stamp: u64::MAX,
            saved_len: 0,
        }
    }

    fn new_sized_loopy(consts: &[Value]) -> Fifo {
        let mut f = Fifo::new_sized(consts, false);
        f.loopy = true;
        f
    }

    fn snapshot(&mut self, now: u64) {
        if self.stamp != now {
            self.stamp = now;
            self.saved_len = self.data.len();
        }
    }

    fn cycle_start_len(&self, now: u64) -> usize {
        if self.stamp == now {
            self.saved_len
        } else {
            self.data.len()
        }
    }
}

impl Prim for Fifo {
    fn value_method(&mut self, method: &str, _args: &[Value], now: u64) -> Value {
        match method {
            "first" => self
                .data
                .front()
                .cloned()
                .unwrap_or_else(|| Value::undet(self.width.max(1))),
            "notFull" => Value::from_u64(1, (self.data.len() < self.depth) as u64),
            "notEmpty" => Value::from_u64(1, (!self.data.is_empty()) as u64),
            "i_notFull" => Value::from_u64(1, (self.cycle_start_len(now) < self.depth) as u64),
            "i_notEmpty" => Value::from_u64(1, (self.cycle_start_len(now) > 0) as u64),
            m => panic!("FIFO: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], now: u64) {
        self.snapshot(now);
        match method {
            "enq" => {
                if self.data.len() >= self.depth {
                    panic!("FIFO overflow (schedule bug: guarded enq on full fifo)");
                }
                let v = if self.zero_width {
                    Value::zero(1)
                } else {
                    args[0].clone()
                };
                self.data.push_back(v);
            }
            "deq" => {
                if self.data.pop_front().is_none() {
                    panic!("FIFO underflow (schedule bug: guarded deq on empty fifo)");
                }
            }
            "clear" => self.data.clear(),
            m => panic!("FIFO: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, _port: &str) {}
}
