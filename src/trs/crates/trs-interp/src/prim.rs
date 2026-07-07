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
    /// End-of-edge tick (RWire clear, CReg rotate, synchronizer clock
    /// ports, ...).  `now` is the simulation time of the ticking edge.
    fn tick(&mut self, port: &str, now: u64);
    /// Reset line transition (assert = true).  Mirrors the `reset_RST`
    /// handlers in bs_prim_mod_*.h: while asserted, state-mutating methods
    /// are ignored and state is forced to the reset value.  Prims without
    /// a reset connection never see this.
    fn set_in_reset(&mut self, _asserted: bool) {}
    /// For reset-generating prims: drain pending output-reset transitions
    /// as (asserted, immediate) pairs.  Immediate transitions cascade in
    /// place (async reset_fn calls); deferred ones apply at the end of the
    /// timeslice (reset_at_end_of_timeslice).
    fn take_reset_out(&mut self) -> Vec<(bool, bool)> {
        Vec::new()
    }
    /// End of the current simulation instant: reset generators move
    /// internally deferred transitions forward (MakeReset's rst register
    /// reaching its internal SyncReset).
    fn end_of_timeslice(&mut self) {}
}

/// Construct a primitive by BSV name.  `width` and other shape facts are
/// recovered from the constant instantiation args (clock/reset args are
/// filtered out by the caller; `consts` holds the remaining constants in
/// order).
pub fn make_prim(name: &str, consts: &[Value], strs: &[String], path: &str) -> Box<dyn Prim> {
    match name {
        // registers: args (after clock/reset) are [width, init] or [width]
        "RegN" | "RegA" => Box::new(Reg::new(consts, true, name == "RegA")),
        "RegUN" => Box::new(Reg::new(consts, false, false)),
        // a reverting virtual reg exists for scheduling; Bluesim models it
        // as a plain reg (primMap maps RevertReg to the Reg class, no tick)
        "RevertReg" => Box::new(Reg::new(consts, true, false)),
        "Probe" | "ProbeWire" => Box::new(Probe),
        // no reset modeling yet: reset outputs read as deasserted
        "ResetToBool" => Box::new(ResetToBool { in_reset: false }),
        "Counter" => Box::new(Counter::new(consts)),
        "RegFile" => Box::new(RegFile::new(consts, None, path)),
        "RegFileLoad" => Box::new(RegFile::new(consts, strs.first().cloned(), path)),
        "ConfigRegN" | "ConfigRegA" => Box::new(ConfigReg::new(consts, true, name == "ConfigRegA")),
        "ConfigRegUN" => Box::new(ConfigReg::new(consts, false, false)),
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
        "ClockGen" => Box::new(ClockGen),
        // SyncBit = 2-flop; SyncBit15 = 2-flop ticked on both dst edges;
        // SyncBit05/SyncBit1 = 1-flop (negedge/posedge dst tick) -- edge
        // choice is carried by which compositions list the tick
        "SyncBit" | "SyncBit15" => Box::new(SyncBit::new(consts, true)),
        "SyncBit05" | "SyncBit1" => Box::new(SyncBit::new(consts, false)),
        "SyncPulse" => Box::new(SyncPulse::new()),
        "SyncHandshake" => Box::new(SyncHandshake { hs: Handshake::new(false, false) }),
        "SyncRegister" => Box::new(SyncReg::new(consts)),
        // reset generators: args are [cycles] / [cycles, init?] per
        // bs_prim_mod_resets.h ctors; A-variants assert asynchronously
        "SyncReset" => Box::new(SyncReset::new(carg(consts, 0) as u32, false)),
        "SyncResetA" => Box::new(SyncReset::new(carg(consts, 0) as u32, true)),
        "SyncReset0" => Box::new(SyncReset0::new()),
        "InitialReset" => Box::new(InitialReset::new(carg(consts, 0) as u32)),
        // MakeReset args: [cycles, init]; MakeReset0 args: [init]
        "MakeReset0" => Box::new(MakeReset::new(
            consts.first().map(|v| v.as_u64()).unwrap_or(1) as u8,
            None,
        )),
        "MakeReset" | "MakeResetA" => Box::new(MakeReset::new(
            consts.get(1).map(|v| v.as_u64()).unwrap_or(1) as u8,
            Some(SyncReset::new(carg(consts, 0) as u32, name == "MakeResetA")),
        )),
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
    fn tick(&mut self, _port: &str, _now: u64) {}
}

/// MOD_ResetToBool: reads 1 while its reset line is asserted.
struct ResetToBool {
    in_reset: bool,
}

impl Prim for ResetToBool {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "isAsserted" | "_read" | "read" => Value::from_u64(1, self.in_reset as u64),
            m => panic!("ResetToBool: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, _args: &[Value], _now: u64) {
        panic!("ResetToBool: unknown action method {method:?}")
    }
    fn tick(&mut self, _port: &str, _now: u64) {}
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
    }
}

// ===============

/// Counter (bs_prim_mod_counter.h): value() reads the begin-of-cycle
/// value once any write has happened this cycle; addA/addB accumulate;
/// setC overrides then re-applies same-cycle adds; setF force-overrides.
struct Counter {
    width: u32,
    val: Value,
    init: Value,
    saved_val: Value,
    saved_at: u64,
    a: Value,
    a_at: u64,
    b: Value,
    b_at: u64,
    in_reset: bool,
}

impl Counter {
    fn new(consts: &[Value]) -> Counter {
        let width = carg(consts, 0) as u32;
        let init = consts.get(1).cloned().unwrap_or_else(|| Value::undet(width));
        Counter {
            width,
            val: Value::undet(width),
            init: init.zext(width),
            saved_val: Value::zero(width),
            saved_at: u64::MAX,
            a: Value::zero(width),
            a_at: u64::MAX,
            b: Value::zero(width),
            b_at: u64::MAX,
            in_reset: false,
        }
    }
    fn save(&mut self, now: u64) {
        if self.saved_at != now {
            self.saved_at = now;
            self.saved_val = self.val.clone();
        }
    }
}

impl Prim for Counter {
    fn value_method(&mut self, method: &str, _args: &[Value], now: u64) -> Value {
        match method {
            "value" | "_read" => {
                if self.saved_at == now {
                    self.saved_val.clone()
                } else {
                    self.val.clone()
                }
            }
            m => panic!("Counter: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], now: u64) {
        if self.in_reset {
            return;
        }
        let w = self.width;
        match method {
            "addA" | "incrA" => {
                self.save(now);
                self.a_at = now;
                self.a = args[0].clone();
                self.val = self.val.add(&args[0], w);
            }
            "addB" | "incrB" => {
                self.save(now);
                self.b_at = now;
                self.b = args[0].clone();
                self.val = self.val.add(&args[0], w);
            }
            "setC" | "update" => {
                self.save(now);
                self.val = args[0].clone();
                if self.a_at == now {
                    self.val = self.val.add(&self.a.clone(), w);
                }
                if self.b_at == now {
                    self.val = self.val.add(&self.b.clone(), w);
                }
            }
            "setF" | "_write" => {
                self.save(now);
                self.val = args[0].clone();
            }
            m => panic!("Counter: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, _port: &str, _now: u64) {
        if self.in_reset {
            self.val = self.init.clone();
            self.saved_at = u64::MAX;
            self.a_at = u64::MAX;
            self.b_at = u64::MAX;
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
    }
}

// ===============

/// RegFile (bs_prim_mod_regfile.h): sparse storage over [lo, hi],
/// read-before-write with one-entry write forwarding, out-of-bounds
/// warnings, and the full mem_file.cxx loader (comments, @addr, x/z
/// digits, range-tracker gap/duplicate warnings).
struct RegFile {
    data: std::collections::HashMap<u64, Value>,
    /// leaf instance name (mem-file warnings)
    mem_name: String,
    /// hierarchical name rooted at "top" (out-of-bounds warnings)
    full_name: String,
    addr_bits: u32,
    width: u32,
    lo: u64,
    hi: u64,
    upd_at: u64,
    upd_addr: u64,
    upd_prev: Value,
}

/// bs_range_tracker.h: runs of loaded addresses; after loading, report
/// gaps and duplicates against [start, end].
struct RangeTracker {
    runs: Vec<(u64, u64)>,
}

impl RangeTracker {
    fn new() -> RangeTracker {
        RangeTracker { runs: Vec::new() }
    }
    fn set_addr(&mut self, addr: u64) {
        match self.runs.last_mut() {
            Some(r) if addr == r.1 + 1 => r.1 = addr,
            Some(r) if r.0 > 0 && addr == r.0 - 1 => r.0 = addr,
            _ => self.runs.push((addr, addr)),
        }
    }
    fn check_range(&mut self, filename: &str, memname: &str, start: u64, end: u64) {
        if self.runs.is_empty() {
            return;
        }
        self.runs.sort();
        let mut next_addr = start;
        let mut next_overlap_addr = start;
        let mut full = false;
        let mut overlap_full = false;
        for &(lo, hi) in &self.runs {
            if lo < next_addr || full {
                // overlap
                let mut overlap_low = lo;
                let overlap_high = if hi < next_addr || full { hi } else { next_addr - 1 };
                if !overlap_full && overlap_high >= next_overlap_addr {
                    if overlap_low < next_overlap_addr {
                        overlap_low = next_overlap_addr;
                    }
                    if overlap_low == overlap_high {
                        println!(
                            "Warning: file '{filename}' for memory '{memname}' has duplicate values for address {overlap_low}."
                        );
                    } else {
                        println!(
                            "Warning: file '{filename}' for memory '{memname}' has duplicate values for addresses {overlap_low} to {overlap_high}."
                        );
                    }
                    next_overlap_addr = overlap_high + 1;
                    if overlap_high == end {
                        overlap_full = true;
                    }
                }
            } else if lo > next_addr {
                // gap
                if next_addr == lo - 1 {
                    println!(
                        "Warning: file '{filename}' for memory '{memname}' has a gap at address {next_addr}."
                    );
                } else {
                    println!(
                        "Warning: file '{filename}' for memory '{memname}' has a gap at addresses {next_addr} to {}.",
                        lo - 1
                    );
                }
            }
            if hi >= next_addr {
                next_addr = hi + 1;
                if hi == end {
                    full = true;
                }
            }
        }
        if !full {
            if next_addr == end {
                println!(
                    "Warning: file '{filename}' for memory '{memname}' has a gap at address {next_addr}."
                );
            } else {
                println!(
                    "Warning: file '{filename}' for memory '{memname}' has a gap at addresses {next_addr} to {end}."
                );
            }
        }
        self.runs.clear();
    }
}

/// mem_file.cxx parse_hex: '_' ignored, x/z count as 0 nibbles; error if
/// the value extends beyond the last nibble or sets bits above `bits` in
/// a partial final nibble.
fn parse_mem_hex(s: &str, bits: u32) -> Option<Value> {
    // accumulate at full-nibble width so overflow into a partial final
    // nibble is observable, then truncate
    let nibbles = ((bits + 3) / 4).max(1);
    let w = nibbles * 4;
    let mut v = Value::zero(w);
    let mut nbits: u32 = 0;
    for c in s.chars() {
        let d = match c {
            '_' => continue,
            'x' | 'X' | 'z' | 'Z' => 0,
            c if c.is_ascii_hexdigit() => c.to_digit(16).unwrap(),
            _ => return None,
        };
        v = v.shl(4, w).or(&Value::from_u64(w, d as u64), w);
        nbits += 4;
        if nbits / 4 > nibbles
            || (nbits / 4 == nibbles && bits % 4 != 0 && !v.lshr(bits as u64, w).is_zero())
        {
            return None;
        }
    }
    Some(v.extract(bits.max(1) as u64 - 1, 0, bits.max(1)))
}

/// mem_file.cxx parse_bin: 0/1 plus x/z (as 0); error past `bits` digits.
fn parse_mem_bin(s: &str, bits: u32) -> Option<Value> {
    let mut v = Value::zero(bits.max(1));
    let mut nbits: u32 = 0;
    for c in s.chars() {
        let d = match c {
            '_' => continue,
            '0' | 'x' | 'X' | 'z' | 'Z' => 0,
            '1' => 1,
            _ => return None,
        };
        v = v.shl(1, bits.max(1)).or(&Value::from_u64(bits.max(1), d), bits.max(1));
        nbits += 1;
        if nbits > bits {
            return None;
        }
    }
    Some(v)
}

impl RegFile {
    fn new(consts: &[Value], file: Option<String>, path: &str) -> RegFile {
        // args (after clocks/resets/file): [addr_width, data_width, lo,
        // hi, binary_format]
        let addr_bits = carg(consts, 0) as u32;
        let width = carg(consts, 1) as u32;
        let lo = carg(consts, 2);
        let hi = carg(consts, 3);
        let bin = carg(consts, 4) != 0;
        let leaf = path.rsplit('.').next().unwrap_or(path).to_string();
        let full_name = if path.is_empty() {
            "top".to_string()
        } else {
            format!("top.{path}")
        };
        let mut rf = RegFile {
            data: Default::default(),
            mem_name: leaf,
            full_name,
            addr_bits,
            width,
            lo,
            hi,
            upd_at: u64::MAX,
            upd_addr: 0,
            upd_prev: Value::undet(width),
        };
        if let Some(f) = file {
            rf.load_memfile(&f, bin);
        }
        rf
    }

    fn in_range(&self, a: u64) -> bool {
        let (lo, hi) = (self.lo.min(self.hi), self.lo.max(self.hi));
        a >= lo && a <= hi
    }

    /// Port of mem_file.cxx read_mem_file + the {Hex,Bin}FormatHandler:
    /// same state machine, same messages (to stdout), same partial-load
    /// behavior on errors.
    fn load_memfile(&mut self, path: &str, bin: bool) {
        let text = match std::fs::read_to_string(path) {
            Ok(x) => x,
            Err(e) => {
                let mut msg = e.to_string();
                if let Some(i) = msg.find(" (os error") {
                    msg.truncate(i);
                }
                println!("Error: failed to open file '{path}' because {msg}");
                return;
            }
        };
        let decreasing = self.lo > self.hi;
        let mut addr = self.lo;
        let mut rt = RangeTracker::new();
        let mut set_entry = |rf: &mut RegFile, rt: &mut RangeTracker, s: &str,
                             addr: &mut u64|
         -> bool {
            if rf.in_range(*addr) {
                let parsed = if bin {
                    parse_mem_bin(s, rf.width)
                } else {
                    parse_mem_hex(s, rf.width)
                };
                match parsed {
                    Some(v) => {
                        rf.data.insert(*addr, v);
                        rt.set_addr(*addr);
                    }
                    None => return false,
                }
            }
            if decreasing {
                *addr = addr.wrapping_sub(1);
            } else {
                *addr += 1;
            }
            true
        };

        #[derive(PartialEq)]
        enum St {
            Start,
            BeginComment,
            CppComment,
            CComment,
            EndCComment,
            InAddr,
            InValue,
        }
        let mut state = St::Start;
        let mut line: u32 = 1;
        let mut start_line: u32 = 1;
        let mut comment_start_line: u32 = 0;
        let mut tok = String::new();
        for c in text.chars() {
            match state {
                St::Start => match c {
                    '/' => state = St::BeginComment,
                    '@' => {
                        state = St::InAddr;
                        tok.clear();
                        start_line = line;
                    }
                    c if c.is_ascii_hexdigit() => {
                        state = St::InValue;
                        tok.clear();
                        tok.push(c);
                        start_line = line;
                    }
                    '\n' => line += 1,
                    '\r' | ' ' | '\t' => {}
                    _ => {
                        println!("Error: syntax error at line {line} of file '{path}'");
                        println!("       Encountered '{c}' when expecting '/', '@', hex digit, end-of-line or whitespace.");
                        return;
                    }
                },
                St::BeginComment => match c {
                    '/' => state = St::CppComment,
                    '*' => {
                        state = St::CComment;
                        comment_start_line = line;
                    }
                    _ => {
                        println!("Error: syntax error at line {line} of file '{path}'");
                        println!("       Malformed comment start sequence.");
                        return;
                    }
                },
                St::CppComment => {
                    if c == '\n' {
                        line += 1;
                        state = St::Start;
                    }
                }
                St::CComment => {
                    if c == '\n' {
                        line += 1;
                    } else if c == '*' {
                        state = St::EndCComment;
                    }
                }
                St::EndCComment => {
                    state = if c == '/' { St::Start } else { St::CComment };
                }
                St::InAddr => {
                    let done = matches!(c, '\n' | '\r' | ' ' | '\t' | '/');
                    if done {
                        let err = match parse_mem_hex(&tok, self.addr_bits) {
                            None => Some("Malformed address".to_string()),
                            Some(v) => {
                                let a = v.as_u64();
                                if !self.in_range(a) {
                                    Some("Address is outside of the allowed range".to_string())
                                } else {
                                    addr = a;
                                    None
                                }
                            }
                        };
                        if let Some(e) = err {
                            println!("Error: address processing error at line {start_line} of file '{path}'");
                            println!("       {e}.");
                            return;
                        }
                        if c == '\n' {
                            line += 1;
                        }
                        state = if c == '/' { St::BeginComment } else { St::Start };
                    } else if c.is_ascii_hexdigit() || matches!(c, '_' | 'x' | 'X' | 'z' | 'Z') {
                        tok.push(c);
                    } else {
                        println!("Error: address processing error at line {start_line} of file '{path}'");
                        println!("       Encountered '{c}' when expecting '/', hex digit, end-of-line or whitespace.");
                        return;
                    }
                }
                St::InValue => {
                    let done = matches!(c, '\n' | '\r' | ' ' | '\t' | '/');
                    if done {
                        if !set_entry(self, &mut rt, &tok, &mut addr) {
                            println!("Error: value processing error at line {start_line} of file '{path}'");
                            println!("       Malformed value.");
                            return;
                        }
                        if c == '\n' {
                            line += 1;
                        }
                        state = if c == '/' { St::BeginComment } else { St::Start };
                    } else if c.is_ascii_hexdigit() || matches!(c, '_' | 'x' | 'X' | 'z' | 'Z') {
                        tok.push(c);
                    } else {
                        println!("Error: value processing error at line {start_line} of file '{path}'");
                        println!("       Encountered '{c}' when expecting '/', digit, end-of-line or whitespace.");
                        return;
                    }
                }
            }
        }
        match state {
            St::CComment | St::EndCComment => {
                println!("Error: syntax error at line {comment_start_line} of file '{path}'");
                println!("       Unterminated C-style comment.");
            }
            St::InValue => {
                if !set_entry(self, &mut rt, &tok, &mut addr) {
                    println!("Error: value processing error at line {line} of file '{path}'");
                    println!("       Malformed value.");
                }
            }
            _ => {}
        }
        rt.check_range(path, &self.mem_name, self.lo, self.hi);
    }

    fn addr_hex(&self, a: u64) -> String {
        let digits = ((self.addr_bits + 3) / 4).max(1) as usize;
        format!("{a:0digits$x}")
    }
}

impl Prim for RegFile {
    fn value_method(&mut self, method: &str, args: &[Value], now: u64) -> Value {
        match method {
            "sub" => {
                let a = args[0].as_u64();
                if !self.in_range(a) {
                    println!(
                        "Warning: RegFile '{}' -- Read address is out of bounds: {}",
                        self.full_name,
                        self.addr_hex(a)
                    );
                    return Value::undet(self.width);
                }
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
                if !self.in_range(a) {
                    println!(
                        "Warning: RegFile '{}' -- Write address is out of bounds: {}",
                        self.full_name,
                        self.addr_hex(a)
                    );
                    return;
                }
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
    fn tick(&mut self, _port: &str, _now: u64) {}
}

fn carg(consts: &[Value], i: usize) -> u64 {
    consts.get(i).map(|v| v.as_u64()).unwrap_or(0)
}

// ===============

/// Reg / RegU (bs_prim_mod_reg.h): read returns current value; write is
/// immediate.  Registered semantics come from the static schedule order.
struct Reg {
    value: Value,
    reset_value: Value,
    in_reset: bool,
    async_rst: bool,
}

impl Reg {
    fn new(consts: &[Value], has_reset: bool, async_rst: bool) -> Reg {
        // instantiation args: [width, init] for RegN/RegA, [width] for
        // RegUN.  The value starts undet even for resettable registers:
        // the reset value arrives via the reset tick at the first clock
        // edge with reset asserted (async regs take it at assert time) —
        // observable when a derived reset never asserts.
        let width = carg(consts, 0) as u32;
        let reset_value = if has_reset && consts.len() > 1 {
            consts[1].zext(width)
        } else {
            Value::undet(width)
        };
        Reg { reset_value, value: Value::undet(width), in_reset: false, async_rst }
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
            "write" | "set" | "put" => {
                if !self.in_reset {
                    self.value = args[0].clone();
                }
            }
            m => panic!("Reg: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, _port: &str, _now: u64) {
        // reset tick: while in reset, each clock edge loads the reset
        // value (rst_tick__clk__1)
        if self.in_reset {
            self.value = self.reset_value.clone();
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted && self.async_rst {
            self.value = self.reset_value.clone();
        }
    }
}

// ===============

/// ConfigReg: reads always see the begin-of-cycle value regardless of
/// same-cycle writes (bs_prim_mod_reg.h:475).
struct ConfigReg {
    value: Value,
    old_value: Value,
    written_at: u64,
    reset_value: Value,
    in_reset: bool,
    async_rst: bool,
}

impl ConfigReg {
    fn new(consts: &[Value], has_reset: bool, async_rst: bool) -> ConfigReg {
        let width = carg(consts, 0) as u32;
        let reset_value = if has_reset && consts.len() > 1 {
            consts[1].zext(width)
        } else {
            Value::undet(width)
        };
        ConfigReg {
            old_value: Value::undet(width),
            reset_value,
            value: Value::undet(width),
            written_at: u64::MAX,
            in_reset: false,
            async_rst,
        }
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
                if self.in_reset {
                    return;
                }
                if self.written_at != now {
                    self.old_value = self.value.clone();
                    self.written_at = now;
                }
                self.value = args[0].clone();
            }
            m => panic!("ConfigReg: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, _port: &str, _now: u64) {
        if self.in_reset {
            self.value = self.reset_value.clone();
            self.old_value = self.reset_value.clone();
            self.written_at = u64::MAX;
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted && self.async_rst {
            self.value = self.reset_value.clone();
            self.old_value = self.reset_value.clone();
            self.written_at = u64::MAX;
        }
    }
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
    fn tick(&mut self, _port: &str, _now: u64) {
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
    fn tick(&mut self, _port: &str, _now: u64) {}
}

// ===============

/// CReg with up to 5 ports (bs_prim_mod_reg.h:817): sequential port
/// writes are immediate; tick commits the registered view.
struct CReg {
    value: Value,       // live value, mutated by port writes
    value_reg: Value,   // value registered at the last edge
    reset_value: Value,
    in_reset: bool,
}

impl CReg {
    fn new(consts: &[Value], has_reset: bool) -> CReg {
        let width = carg(consts, 0) as u32;
        let init = if has_reset && consts.len() > 1 {
            consts[1].zext(width)
        } else {
            Value::undet(width)
        };
        CReg {
            value: Value::undet(width),
            value_reg: Value::undet(width),
            reset_value: init,
            in_reset: false,
        }
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
            if !self.in_reset {
                self.value = args[0].clone();
            }
        } else {
            panic!("CReg: unknown action method {method:?}")
        }
    }
    fn tick(&mut self, _port: &str, _now: u64) {
        if self.in_reset {
            self.value = self.reset_value.clone();
        }
        self.value_reg = self.value.clone();
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
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
    in_reset: bool,
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
            in_reset: false,
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
            in_reset: false,
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

    /// The count the CF status methods report (bs_prim_mod_fifo.h:181-203):
    /// loopy FIFOs reflect same-cycle mutations (deq < i_notFull < enq);
    /// all others report the begin-of-cycle count once anything mutated.
    fn cycle_start_len(&self, now: u64) -> usize {
        if !self.loopy && self.stamp == now {
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
        if self.in_reset {
            return;
        }
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
    fn tick(&mut self, _port: &str, _now: u64) {
        if self.in_reset {
            self.data.clear();
            self.stamp = u64::MAX;
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted {
            self.data.clear();
            self.stamp = u64::MAX;
        }
    }
}

// ===============
// Clock-domain crossing primitives (bs_prim_mod_synchronizers.h) and
// clock generators (bs_prim_mod_clockgen.h).

/// ClockGen: pure waveform source.  The waveform itself is consumed by the
/// interpreter's event loop (from the instantiation args); the primitive
/// instance has no methods and no state.
struct ClockGen;

impl Prim for ClockGen {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("ClockGen: unknown value method {method:?}")
    }
    fn action_method(&mut self, method: &str, _args: &[Value], _now: u64) {
        panic!("ClockGen: unknown action method {method:?}")
    }
    fn tick(&mut self, _port: &str, _now: u64) {}
}

/// SyncVar: cross-domain variable with Verilog non-blocking-assignment
/// visibility — a read at the same simulation time as the write sees the
/// previous value (two clock edges coinciding in time behave as if the
/// reader sampled before the writer's edge).
struct SyncVar {
    prev: Value,
    cur: Value,
    written_at: u64,
}

impl SyncVar {
    fn new(v: Value) -> SyncVar {
        SyncVar { prev: v.clone(), cur: v, written_at: u64::MAX }
    }
    fn read(&self, now: u64) -> Value {
        if self.written_at == now {
            self.prev.clone()
        } else {
            self.cur.clone()
        }
    }
    fn write(&mut self, x: Value, now: u64) {
        self.prev = std::mem::replace(&mut self.cur, x);
        self.written_at = now;
    }
    fn force(&mut self, x: Value) {
        self.prev = x.clone();
        self.cur = x;
        self.written_at = u64::MAX;
    }
}

/// SyncBit family (MOD_Sync2 / MOD_Sync15 / MOD_Sync1): 1-bit two-flop (or
/// one-flop) synchronizer.  `send` writes the source-side flop; each
/// destination-clock tick shifts toward `read`.
struct SyncBit {
    two_stage: bool,
    d1: Value,
    d2: Value,
    s: SyncVar,
    reset_value: Value,
    in_reset: bool,
}

impl SyncBit {
    fn new(consts: &[Value], two_stage: bool) -> SyncBit {
        let rv = consts.first().cloned().unwrap_or_else(|| Value::zero(1)).zext(1);
        SyncBit {
            two_stage,
            d1: Value::undet(1),
            d2: Value::undet(1),
            s: SyncVar::new(Value::undet(1)),
            reset_value: rv,
            in_reset: false,
        }
    }
}

impl Prim for SyncBit {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "read" | "_read" => {
                if self.two_stage {
                    self.d2.clone()
                } else {
                    self.d1.clone()
                }
            }
            m => panic!("SyncBit: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], now: u64) {
        match method {
            "send" | "write" | "_write" => {
                if !self.in_reset {
                    self.s.write(args[0].zext(1), now);
                }
            }
            m => panic!("SyncBit: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, port: &str, now: u64) {
        match port {
            "clk_dst" => {
                if self.two_stage {
                    self.d2 = self.d1.clone();
                }
                self.d1 = self.s.read(now);
            }
            "clk_src" => {}
            p => panic!("SyncBit: unknown tick port {p:?}"),
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted {
            self.d1 = self.reset_value.clone();
            self.d2 = self.reset_value.clone();
            self.s.force(self.reset_value.clone());
        }
    }
}

/// MOD_SyncPulse: send toggles the source flop; the destination sees a
/// one-cycle pulse when the toggle propagates through the two-flop chain.
struct SyncPulse {
    d_pulse: Value,
    d2: Value,
    d1: Value,
    s: SyncVar,
    in_reset: bool,
}

impl SyncPulse {
    fn new() -> SyncPulse {
        SyncPulse {
            d_pulse: Value::undet(1),
            d2: Value::undet(1),
            d1: Value::undet(1),
            s: SyncVar::new(Value::undet(1)),
            in_reset: false,
        }
    }
}

impl Prim for SyncPulse {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "pulse" | "read" | "_read" => self.d2.xor(&self.d_pulse, 1),
            m => panic!("SyncPulse: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, _args: &[Value], now: u64) {
        match method {
            "send" => {
                if !self.in_reset {
                    let cur = self.s.read(now);
                    let flipped = Value::from_u64(1, (cur.as_u64() == 0) as u64);
                    self.s.write(flipped, now);
                }
            }
            m => panic!("SyncPulse: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, port: &str, now: u64) {
        match port {
            "clk_dst" => {
                self.d_pulse = self.d2.clone();
                self.d2 = self.d1.clone();
                self.d1 = self.s.read(now);
            }
            "clk_src" => {}
            p => panic!("SyncPulse: unknown tick port {p:?}"),
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted {
            self.d_pulse = Value::zero(1);
            self.d2 = Value::zero(1);
            self.d1 = Value::zero(1);
            self.s.force(Value::zero(1));
        }
    }
}

/// MOD_SyncHandshake: pulse synchronizer with a return path so the next
/// send is blocked until the previous pulse reached the destination.
struct Handshake {
    d_sync2: SyncVar,
    d_last: SyncVar,
    s_toggle: SyncVar,
    s1: u64,
    s2: u64,
    d1: u64,
    s_rdy: bool,
    en: bool,
    param_init: bool,
    param_delayreturn: bool,
    in_reset: bool,
}

impl Handshake {
    fn new(init: bool, delayreturn: bool) -> Handshake {
        Handshake {
            d_sync2: SyncVar::new(Value::undet(1)),
            d_last: SyncVar::new(Value::undet(1)),
            s_toggle: SyncVar::new(Value::undet(1)),
            s1: 1,
            s2: 1,
            d1: Value::undet(1).as_u64(),
            s_rdy: false,
            en: false,
            param_init: init,
            param_delayreturn: delayreturn,
            in_reset: false,
        }
    }
    fn pulse(&self, now: u64) -> bool {
        self.d_sync2.read(now).as_u64() != self.d_last.read(now).as_u64()
    }
    fn rdy_send(&self) -> bool {
        !self.in_reset && self.s_rdy
    }
    fn send(&mut self) {
        self.en = true;
    }
    fn clk_src(&mut self, now: u64) {
        if !self.in_reset {
            self.s2 = self.s1;
            self.s1 = if self.param_delayreturn {
                self.d_last.read(now).as_u64()
            } else {
                self.d_sync2.read(now).as_u64()
            };
        }
        if self.en {
            let cur = self.s_toggle.read(now).as_u64();
            self.s_toggle
                .write(Value::from_u64(1, (cur == 0) as u64), now);
            self.s_rdy = false;
        } else {
            self.s_rdy = self.s2 == self.s_toggle.read(now).as_u64();
        }
        self.en = false;
    }
    fn clk_dst(&mut self, now: u64) {
        let v2 = self.d_sync2.read(now);
        self.d_last.write(v2, now);
        self.d_sync2.write(Value::from_u64(1, self.d1), now);
        self.d1 = self.s_toggle.read(now).as_u64();
    }
    fn reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted {
            let init = Value::from_u64(1, self.param_init as u64);
            let not_init = Value::from_u64(1, !self.param_init as u64);
            self.d_sync2.force(init.clone());
            self.s_toggle.force(init.clone());
            self.d1 = init.as_u64();
            self.d_last.force(init);
            self.s1 = not_init.as_u64();
            self.s2 = not_init.as_u64();
            self.s_rdy = false;
            self.en = false;
        }
    }
}

struct SyncHandshake {
    hs: Handshake,
}

impl Prim for SyncHandshake {
    fn value_method(&mut self, method: &str, _args: &[Value], now: u64) -> Value {
        match method {
            "pulse" | "read" | "_read" => Value::from_u64(1, self.hs.pulse(now) as u64),
            "RDY_send" => Value::from_u64(1, self.hs.rdy_send() as u64),
            m => panic!("SyncHandshake: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, _args: &[Value], _now: u64) {
        match method {
            "send" => self.hs.send(),
            m => panic!("SyncHandshake: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, port: &str, now: u64) {
        match port {
            "clk_src" => self.hs.clk_src(now),
            "clk_dst" => self.hs.clk_dst(now),
            p => panic!("SyncHandshake: unknown tick port {p:?}"),
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.hs.reset(asserted);
    }
}

/// MOD_SyncReg: a data register whose write is carried across domains by
/// an internal handshake (init=false, delayreturn=true).
struct SyncReg {
    data: SyncVar,
    d_out: Value,
    reset_value: Value,
    hs: Handshake,
    in_reset: bool,
}

impl SyncReg {
    fn new(consts: &[Value]) -> SyncReg {
        let width = carg(consts, 0) as u32;
        let rv = consts
            .get(1)
            .cloned()
            .unwrap_or_else(|| Value::undet(width))
            .zext(width);
        SyncReg {
            data: SyncVar::new(Value::undet(width)),
            d_out: Value::undet(width),
            reset_value: rv,
            hs: Handshake::new(false, true),
            in_reset: false,
        }
    }
}

impl Prim for SyncReg {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "read" | "_read" => self.d_out.clone(),
            "RDY_write" => Value::from_u64(1, self.hs.rdy_send() as u64),
            m => panic!("SyncReg: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], now: u64) {
        match method {
            "write" | "_write" => {
                self.data.write(args[0].clone(), now);
                self.hs.send();
            }
            m => panic!("SyncReg: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, port: &str, now: u64) {
        match port {
            "clk_src" => self.hs.clk_src(now),
            "clk_dst" => {
                if self.hs.pulse(now) {
                    self.d_out = self.data.read(now);
                }
                self.hs.clk_dst(now);
            }
            p => panic!("SyncReg: unknown tick port {p:?}"),
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        self.hs.reset(asserted);
        if asserted {
            self.data.force(self.reset_value.clone());
            self.d_out = self.reset_value.clone();
        }
    }
}


// ===============
// Reset generators (bs_prim_mod_resets.h).  Output transitions are
// reported through take_reset_out as (asserted, immediate) pairs; the
// interpreter routes immediate ones as cascading reset_fn calls and
// deferred ones through the end-of-timeslice queue.

/// MOD_SyncReset / MOD_SyncResetA: output asserts with the input (async
/// variant immediately, sync variant at the next clk tick) and deasserts
/// `hold` clk ticks after the input deasserts.
struct SyncReset {
    hold: u32,
    is_async: bool,
    count: u32,
    in_reset: bool,
    call_reset_fn: bool,
    pending: Vec<(bool, bool)>,
}

impl SyncReset {
    fn new(hold: u32, is_async: bool) -> SyncReset {
        SyncReset {
            hold,
            is_async,
            count: 0,
            in_reset: false,
            call_reset_fn: false,
            pending: Vec::new(),
        }
    }
    fn input(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted {
            self.count = self.hold + 1;
            if self.is_async {
                self.pending.push((true, true));
            } else {
                self.call_reset_fn = true;
            }
        }
    }
    fn clk(&mut self) {
        if self.call_reset_fn {
            if self.in_reset {
                self.pending.push((true, false));
            }
            self.call_reset_fn = false;
        }
        if !self.in_reset && self.count > 0 {
            if self.count == 1 {
                self.pending.push((false, false));
            }
            self.count -= 1;
        }
    }
}

impl Prim for SyncReset {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("SyncReset: unknown value method {method:?}")
    }
    fn action_method(&mut self, method: &str, _args: &[Value], _now: u64) {
        panic!("SyncReset: unknown action method {method:?}")
    }
    fn tick(&mut self, port: &str, _now: u64) {
        match port {
            "clk" => self.clk(),
            p => panic!("SyncReset: unknown tick port {p:?}"),
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.input(asserted);
    }
    fn take_reset_out(&mut self) -> Vec<(bool, bool)> {
        std::mem::take(&mut self.pending)
    }
}

/// MOD_SyncReset0: combinationally forwards its input reset.
struct SyncReset0 {
    pending: Vec<(bool, bool)>,
}

impl SyncReset0 {
    fn new() -> SyncReset0 {
        SyncReset0 { pending: Vec::new() }
    }
}

impl Prim for SyncReset0 {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("SyncReset0: unknown value method {method:?}")
    }
    fn action_method(&mut self, method: &str, _args: &[Value], _now: u64) {
        panic!("SyncReset0: unknown action method {method:?}")
    }
    fn tick(&mut self, _port: &str, _now: u64) {}
    fn set_in_reset(&mut self, asserted: bool) {
        self.pending.push((asserted, true));
    }
    fn take_reset_out(&mut self) -> Vec<(bool, bool)> {
        std::mem::take(&mut self.pending)
    }
}

/// MOD_InitialReset: output starts asserted (reset_init at time 0, set up
/// by the interpreter) and deasserts after `count` clk ticks.
struct InitialReset {
    count: u32,
    pending: Vec<(bool, bool)>,
}

impl InitialReset {
    fn new(count: u32) -> InitialReset {
        InitialReset { count, pending: Vec::new() }
    }
}

impl Prim for InitialReset {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("InitialReset: unknown value method {method:?}")
    }
    fn action_method(&mut self, method: &str, _args: &[Value], _now: u64) {
        panic!("InitialReset: unknown action method {method:?}")
    }
    fn tick(&mut self, port: &str, _now: u64) {
        match port {
            "clk" => {
                if self.count > 0 {
                    if self.count == 1 {
                        self.pending.push((false, false));
                    }
                    self.count -= 1;
                }
            }
            p => panic!("InitialReset: unknown tick port {p:?}"),
        }
    }
    fn take_reset_out(&mut self) -> Vec<(bool, bool)> {
        std::mem::take(&mut self.pending)
    }
}

/// MOD_MakeReset / MOD_MakeReset0: a RegA-like `rst` register driven by
/// the assertReset method, auto-returning to 1 each clk tick.  MakeReset
/// feeds the register through an internal SyncReset synchronized to
/// dst_clk; MakeReset0's output is the register itself.
struct MakeReset {
    rst_reset_value: u8,
    rst: u8,
    old_rst: u8,
    written: u64,
    in_reset: bool,
    sync: Option<SyncReset>,
    /// rst-register transitions awaiting end of timeslice before they
    /// reach the internal SyncReset (reset_at_end_of_timeslice on
    /// static_reset_syncRst$rst)
    internal_pending: Vec<bool>,
    pending: Vec<(bool, bool)>,
}

impl MakeReset {
    fn new(rst_reset_value: u8, sync: Option<SyncReset>) -> MakeReset {
        MakeReset {
            rst_reset_value,
            rst: 1,
            old_rst: 1,
            written: u64::MAX,
            in_reset: false,
            sync,
            internal_pending: Vec::new(),
            pending: Vec::new(),
        }
    }
    fn route(&mut self, asserted: bool, immediate: bool) {
        match &mut self.sync {
            None => self.pending.push((asserted, immediate)),
            Some(s) => {
                if immediate {
                    // reset_RST calls sync.reset_IN_RST directly
                    s.input(asserted);
                    self.pending.append(&mut s.pending);
                } else {
                    self.internal_pending.push(asserted);
                }
            }
        }
    }
}

impl Prim for MakeReset {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "isAsserted" => Value::from_u64(1, (self.rst == 0) as u64),
            m => panic!("MakeReset: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, _args: &[Value], now: u64) {
        match method {
            "assertReset" => {
                if !self.in_reset {
                    self.old_rst = self.rst;
                    self.rst = 0;
                    self.written = now;
                }
            }
            m => panic!("MakeReset: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, port: &str, now: u64) {
        match port {
            "clk" => {
                if !self.in_reset {
                    if self.written != now {
                        self.old_rst = self.rst;
                        self.rst = 1;
                    }
                    if self.rst != self.old_rst {
                        let a = self.rst == 0;
                        self.route(a, false);
                    }
                }
            }
            "dst_clk" => {
                if let Some(s) = &mut self.sync {
                    s.clk();
                    self.pending.append(&mut s.pending);
                }
            }
            p => panic!("MakeReset: unknown tick port {p:?}"),
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted {
            self.old_rst = self.rst;
            self.rst = self.rst_reset_value;
            if self.old_rst != self.rst {
                let a = self.rst == 0;
                self.route(a, true);
            }
        }
    }
    fn take_reset_out(&mut self) -> Vec<(bool, bool)> {
        std::mem::take(&mut self.pending)
    }
    fn end_of_timeslice(&mut self) {
        if let Some(s) = &mut self.sync {
            for a in std::mem::take(&mut self.internal_pending) {
                s.input(a);
            }
            self.pending.append(&mut s.pending);
        }
    }
}
