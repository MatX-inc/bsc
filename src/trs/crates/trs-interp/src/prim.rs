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
    /// ports, ...).  `now` is the simulation time of the ticking edge;
    /// `clk_val` is the clock level after the edge (true on posedge) —
    /// Both-edge ticks (ClockInverter, GatedClock) depend on it.
    fn tick(&mut self, port: &str, now: u64, clk_val: bool, gate: bool);
    /// The prim's output clock gate (PORT_CLK_GATE_OUT) for `Expr::Gate`
    /// reads; 1 for prims without a gate output.
    fn gate_out(&self) -> bool {
        true
    }
    /// Reset line transition (assert = true).  Mirrors the `reset_RST`
    /// handlers in bs_prim_mod_*.h: while asserted, state-mutating methods
    /// are ignored and state is forced to the reset value.  Prims without
    /// a reset connection never see this.
    fn set_in_reset(&mut self, _asserted: bool) {}

    /// VCD: declare this prim's $vars in the current scope, reserving its
    /// own ids (the parent's per-prim slot deliberately stays unused).
    /// `clk_vcd_id` is the prim's kernel clock id for CLK aliases;
    /// `clk` its clock index for vcd_set_clock.
    fn vcd_defs(
        &mut self,
        _w: &mut crate::vcd::Vcd,
        _name: &str,
        _clk: usize,
        _clk_vcd_id: u32,
    ) {
    }

    /// VCD: dump values per the dump type.  `clk_edge_now` = the prim's
    /// clock posedged at `now` (gates method-signal resampling in
    /// CHANGES mode).
    fn vcd_dump(
        &mut self,
        _w: &mut crate::vcd::Vcd,
        _dt: crate::vcd::DumpType,
        _now: u64,
        _clk_edge_now: bool,
    ) {
    }
    /// Indexed reset-line transition for prims with several reset inputs
    /// (ResetMux/ResetEither A_RST/B_RST); the index is the ordinal of
    /// the Reset argument in the instantiation.  Single-input prims fall
    /// through to set_in_reset.
    fn set_reset_input(&mut self, _input: usize, asserted: bool) {
        self.set_in_reset(asserted);
    }
    /// Conditional reset tick (rst_tick_*): posedge of the prim's clock
    /// while some reset is asserted; loads the reset state if this prim's
    /// own reset line is asserted.
    fn rst_tick(&mut self, _now: u64) {}
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
    /// For clock-generating prims (MakeClock, ClockDiv, ClockInverter):
    /// drain output edges triggered at the current instant
    /// (bk_trigger_clock_edge).  true = posedge.
    fn take_clock_edges(&mut self) -> Vec<bool> {
        Vec::new()
    }
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
        "CrossingRegN" | "CrossingRegA" => Box::new(Reg::new(consts, true, name == "CrossingRegA")),
        "CrossingRegUN" => Box::new(Reg::new(consts, false, false)),
        // a reverting virtual reg exists for scheduling; Bluesim uses the
        // no-reset MOD_Reg ctor, which loads the init value directly at
        // construction (regType NRst — no reset line, no ticks)
        "RevertReg" => Box::new(Reg::preset(consts)),
        "Probe" => Box::new(Probe::new(consts)),
        // ProbeWire contributes nothing to VCD (bs_prim_mod_probe.h:103-133)
        "ProbeWire" => Box::new(ProbeWire),
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
        "CRegN5" | "CRegA5" | "CRegUN5" => Box::new(CReg::new(consts, !name.ends_with("UN5"), name == "CRegA5")),
        // raw args: FIFO1/2/L1/L2 = [width, guarded]; the 0-variants drop
        // width; SizedFIFO(L) = [width, depth, cnt_width, guarded],
        // SizedFIFO0 = [depth, cnt_width, guarded]
        "FIFO1" => Box::new(Fifo::new(carg(consts, 0) as u32, 1, carg(consts, 1) != 0, FifoType::Simple, false, path)),
        "FIFO2" => Box::new(Fifo::new(carg(consts, 0) as u32, 2, carg(consts, 1) != 0, FifoType::Simple, false, path)),
        "FIFO10" => Box::new(Fifo::new(0, 1, carg(consts, 0) != 0, FifoType::Simple, true, path)),
        "FIFO20" => Box::new(Fifo::new(0, 2, carg(consts, 0) != 0, FifoType::Simple, true, path)),
        "FIFOL1" => Box::new(Fifo::new(carg(consts, 0) as u32, 1, carg(consts, 1) != 0, FifoType::Loopy, false, path)),
        "FIFOL2" => Box::new(Fifo::new(carg(consts, 0) as u32, 2, carg(consts, 1) != 0, FifoType::Loopy, false, path)),
        "FIFOL10" => Box::new(Fifo::new(0, 1, carg(consts, 0) != 0, FifoType::Loopy, true, path)),
        "FIFOL20" => Box::new(Fifo::new(0, 2, carg(consts, 0) != 0, FifoType::Loopy, true, path)),
        "SizedFIFO" => Box::new(Fifo::new(carg(consts, 0) as u32, carg(consts, 1), carg(consts, 3) != 0, FifoType::Simple, false, path)),
        "SizedFIFO0" => Box::new(Fifo::new(0, carg(consts, 0), carg(consts, 2) != 0, FifoType::Simple, true, path)),
        "SizedFIFOL" => Box::new(Fifo::new(carg(consts, 0) as u32, carg(consts, 1), carg(consts, 3) != 0, FifoType::Loopy, false, path)),
        "SizedFIFOL0" => Box::new(Fifo::new(0, carg(consts, 0), carg(consts, 2) != 0, FifoType::Loopy, true, path)),
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
        "RegTwoN" | "RegTwoA" => Box::new(RegTwo::new(consts, true, name == "RegTwoA")),
        "RegTwoUN" => Box::new(RegTwo::new(consts, false, false)),
        "ClockMux" | "UngatedClockMux" => Box::new(ClockMux::new()),
        "ClockSelect" | "UngatedClockSelect" => Box::new(ClockSelect::new(consts)),
        "ResetMux" => Box::new(ResetMux::new()),
        "ResetEither" => Box::new(ResetEither::new()),
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
        // raw args: [width, depth, indexWidth] ([depth, indexWidth] for
        // the zero-width variants, [width] for depth-1); the clear
        // interface exists only on the Level variants
        "SyncFIFO" => Box::new(SyncFifo::new(carg(consts, 0) as u32, carg(consts, 1), false)),
        "SyncFIFO0" => Box::new(SyncFifo::new(0, carg(consts, 0), false)),
        "SyncFIFO1" => Box::new(SyncFifo::new(carg(consts, 0) as u32, 1, false)),
        "SyncFIFO10" => Box::new(SyncFifo::new(0, 1, false)),
        "SyncFIFOLevel" => Box::new(SyncFifo::new(carg(consts, 0) as u32, carg(consts, 1), true)),
        "SyncFIFOLevel0" => Box::new(SyncFifo::new(0, carg(consts, 0), true)),
        // BRAMs: [pipelined, addr_width, data_width, memsize]; BE adds
        // [chunk_size, we_width] before memsize; Load variants carry the
        // file in strs plus a binary flag as the last const
        "BRAM1" | "BRAM2" => Box::new(Bram::new(
            carg(consts, 0) != 0,
            name.starts_with("BRAM2"),
            carg(consts, 1) as u32,
            carg(consts, 2) as u32,
            carg(consts, 2) as u32,
            1,
            carg(consts, 3),
            path,
            None,
        )),
        "BRAM1BE" | "BRAM2BE" => Box::new(Bram::new(
            carg(consts, 0) != 0,
            name.starts_with("BRAM2"),
            carg(consts, 1) as u32,
            carg(consts, 2) as u32,
            carg(consts, 3) as u32,
            carg(consts, 4) as u32,
            carg(consts, 5),
            path,
            None,
        )),
        "BRAM1Load" | "BRAM2Load" => Box::new(Bram::new(
            carg(consts, 0) != 0,
            name.starts_with("BRAM2"),
            carg(consts, 1) as u32,
            carg(consts, 2) as u32,
            carg(consts, 2) as u32,
            1,
            carg(consts, 3),
            path,
            strs.first().map(|f| (f.clone(), carg(consts, 4) != 0)),
        )),
        "BRAM1BELoad" | "BRAM2BELoad" => Box::new(Bram::new(
            carg(consts, 0) != 0,
            name.starts_with("BRAM2"),
            carg(consts, 1) as u32,
            carg(consts, 2) as u32,
            carg(consts, 3) as u32,
            carg(consts, 4) as u32,
            carg(consts, 5),
            path,
            strs.first().map(|f| (f.clone(), carg(consts, 6) != 0)),
        )),
        // dynamic clock sources (bs_prim_mod_clockgen.h)
        "MakeClock" => Box::new(MakeClock::new(consts)),
        "ClockDiv" => Box::new(ClockDivider::new(consts)),
        "ClockInverter" | "GatedClockInverter" => Box::new(ClockInverter::new()),
        "GatedClock" => Box::new(GatedClock::new(consts)),
        // a BypassWire crossing domains; the clk tick is bookkeeping only
        "CrossingBypassWire" => Box::new(BypassWire::new(consts, false)),
        _ => panic!("trs-interp: unimplemented primitive {name:?} (P1 bring-up)"),
    }
}

// ===============

/// Probe: waveform-only sink (bs_prim_mod_probe.h:11-99): one $var
/// named "<inst>$PROBE", clock-backdated, value from the last write.
struct Probe {
    value: Value,
    vcd_id: u32,
    vcd_back: Option<Value>,
}

impl Probe {
    fn new(consts: &[Value]) -> Probe {
        let width = carg(consts, 0) as u32;
        Probe { value: Value::undet(width.max(1)), vcd_id: 0, vcd_back: None }
    }
}

impl Prim for Probe {
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        clk: usize,
        _clk_vcd_id: u32,
    ) {
        self.vcd_id = w.reserve_ids(1);
        w.set_clock(self.vcd_id, clk);
        w.write_def(self.vcd_id, &format!("{name}$PROBE"), self.value.width);
    }
    fn vcd_dump(
        &mut self,
        w: &mut crate::vcd::Vcd,
        dt: crate::vcd::DumpType,
        now: u64,
        _clk_edge_now: bool,
    ) {
        let v = self.value.clone();
        vcd_flat_dump(w, dt, now, self.vcd_id, &v, &mut self.vcd_back);
    }
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("Probe: unknown value method {method:?}")
    }
    fn action_method(&mut self, _method: &str, args: &[Value], _now: u64) {
        if let Some(v) = args.first() {
            self.value = v.clone();
        }
    }
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, _gate: bool) {}
}

/// ProbeWire: sink with no VCD contribution.
struct ProbeWire;

impl Prim for ProbeWire {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("ProbeWire: unknown value method {method:?}")
    }
    fn action_method(&mut self, _method: &str, _args: &[Value], _now: u64) {}
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, _gate: bool) {}
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
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, _gate: bool) {}
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
    c: Value,
    c_at: u64,
    f: Value,
    f_at: u64,
    in_reset: bool,
    suppress: bool,
    vcd_base: u32,
    vcd_back: Option<CounterVcdBack>,
}

#[derive(Clone)]
struct CounterVcdBack {
    val: Value,
    adda: bool,
    a: Value,
    addb: bool,
    b: Value,
    setc: bool,
    c: Value,
    setf: bool,
    f: Value,
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
            c: Value::zero(width),
            c_at: u64::MAX,
            f: Value::zero(width),
            f_at: u64::MAX,
            in_reset: false,
            suppress: false,
            vcd_base: 0,
            vcd_back: None,
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
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        clk: usize,
        _clk_vcd_id: u32,
    ) {
        // bs_prim_mod_counter.h:168-193: parent-scope var, then a scope
        // of clocked port signals; q_state/Q_OUT alias the parent var
        let n0 = w.reserve_ids(9);
        self.vcd_base = n0;
        let bits = self.width;
        w.write_def(n0, name, bits);
        w.scope_start(name);
        let mut n = n0 + 1;
        for (pname, pw) in [
            ("ADDA", 1),
            ("DATA_A", bits),
            ("ADDB", 1),
            ("DATA_B", bits),
            ("SETC", 1),
            ("DATA_C", bits),
            ("SETF", 1),
            ("DATA_F", bits),
        ] {
            w.set_clock(n, clk);
            w.write_def(n, pname, pw);
            n += 1;
        }
        w.write_def(n0, "q_state", bits);
        w.write_def(n0, "Q_OUT", bits);
        w.scope_end();
    }
    fn vcd_dump(
        &mut self,
        w: &mut crate::vcd::Vcd,
        dt: crate::vcd::DumpType,
        now: u64,
        clk_edge_now: bool,
    ) {
        use crate::vcd::DumpType as D;
        let bits = self.width;
        let bit = |b: bool| Value::from_u64(1, b as u64);
        let mut num = self.vcd_base;
        let mut back = self.vcd_back.take().unwrap_or_else(|| CounterVcdBack {
            val: Value::undet(bits),
            adda: false,
            a: Value::zero(bits),
            addb: false,
            b: Value::zero(bits),
            setc: false,
            c: Value::zero(bits),
            setf: false,
            f: Value::zero(bits),
        });
        let adda = self.a_at == now;
        let addb = self.b_at == now;
        let setc = self.c_at == now;
        let setf = self.f_at == now;
        match dt {
            D::Xs => {
                for pw in [bits, 1, bits, 1, bits, 1, bits, 1, bits] {
                    w.write_x(num, pw, now);
                    num += 1;
                }
            }
            D::Changes => {
                if back.val != self.val {
                    w.write_val(num, &self.val, now);
                }
                num += 1;
                if clk_edge_now {
                    if back.adda != adda {
                        w.write_val(num, &bit(adda), now);
                        back.adda = adda;
                    }
                    num += 1;
                    if back.a != self.a {
                        w.write_val(num, &self.a, now);
                    }
                    num += 1;
                    if back.addb != addb {
                        w.write_val(num, &bit(addb), now);
                        back.addb = addb;
                    }
                    num += 1;
                    if back.b != self.b {
                        w.write_val(num, &self.b, now);
                    }
                    num += 1;
                    if back.setc != setc {
                        w.write_val(num, &bit(setc), now);
                        back.setc = setc;
                    }
                    num += 1;
                    if back.c != self.c {
                        w.write_val(num, &self.c, now);
                    }
                    num += 1;
                    if back.setf != setf {
                        w.write_val(num, &bit(setf), now);
                        back.setf = setf;
                    }
                    num += 1;
                    if back.f != self.f {
                        w.write_val(num, &self.f, now);
                    }
                }
            }
            _ => {
                w.write_val(num, &self.val, now);
                num += 1;
                for (flag, data) in [
                    (adda, self.a.clone()),
                    (addb, self.b.clone()),
                    (setc, self.c.clone()),
                    (setf, self.f.clone()),
                ] {
                    w.write_val(num, &bit(flag), now);
                    num += 1;
                    w.write_val(num, &data, now);
                    num += 1;
                }
                back.adda = adda;
                back.addb = addb;
                back.setc = setc;
                back.setf = setf;
            }
        }
        back.val = self.val.clone();
        back.a = self.a.clone();
        back.b = self.b.clone();
        back.c = self.c.clone();
        back.f = self.f.clone();
        self.vcd_back = Some(back);
    }

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
        if self.suppress {
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
                self.c_at = now;
                self.c = args[0].clone();
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
                self.f_at = now;
                self.f = args[0].clone();
                self.val = args[0].clone();
            }
            m => panic!("Counter: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, _gate: bool) {}
    fn rst_tick(&mut self, _now: u64) {
        if self.in_reset {
            self.val = self.init.clone();
            self.saved_at = u64::MAX;
            self.a_at = u64::MAX;
            self.b_at = u64::MAX;
            self.c_at = u64::MAX;
            self.f_at = u64::MAX;
            self.suppress = true;
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if !asserted {
            self.suppress = false;
        }
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

    /// Port of mem_file.cxx read_mem_file + the {Hex,Bin}FormatHandler.
    fn load_memfile(&mut self, path: &str, bin: bool) {
        let (addr_bits, width, lo, hi) = (self.addr_bits, self.width, self.lo, self.hi);
        let mem_name = self.mem_name.clone();
        load_mem_file(path, bin, addr_bits, width, lo, hi, &mem_name, &mut |a, v| {
            self.data.insert(a, v);
        });
    }

    fn addr_hex(&self, a: u64) -> String {
        let digits = ((self.addr_bits + 3) / 4).max(1) as usize;
        format!("{a:0digits$x}")
    }
}

/// mem_file.cxx read_mem_file + format handlers: same state machine, same
/// messages (to stdout), same partial-load behavior on errors.  `sink`
/// receives in-range parsed (address, value) pairs.
fn load_mem_file(
    path: &str,
    bin: bool,
    addr_bits: u32,
    width: u32,
    lo: u64,
    hi: u64,
    mem_name: &str,
    sink: &mut dyn FnMut(u64, Value),
) {
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
        let in_range = |a: u64| -> bool { a >= lo.min(hi) && a <= lo.max(hi) };
    let decreasing = lo > hi;
        let mut addr = lo;
        let mut rt = RangeTracker::new();
        let mut set_entry = |rt: &mut RangeTracker, s: &str, addr: &mut u64,
                             sink: &mut dyn FnMut(u64, Value)|
         -> bool {
            if in_range(*addr) {
                let parsed = if bin {
                    parse_mem_bin(s, width)
                } else {
                    parse_mem_hex(s, width)
                };
                match parsed {
                    Some(v) => {
                        sink(*addr, v);
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
                        let err = match parse_mem_hex(&tok, addr_bits) {
                            None => Some("Malformed address".to_string()),
                            Some(v) => {
                                let a = v.as_u64();
                                if !in_range(a) {
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
                        if !set_entry(&mut rt, &tok, &mut addr, sink) {
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
                if !set_entry(&mut rt, &tok, &mut addr, sink) {
                    println!("Error: value processing error at line {line} of file '{path}'");
                    println!("       Malformed value.");
                }
            }
            _ => {}
        }
        rt.check_range(path, mem_name, lo, hi);
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
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, _gate: bool) {}
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
    suppress: bool,
    // clock-crossing registers (CrossingReg*): NBA-visible previous value
    prev: Value,
    written_at: u64,
    vcd_id: u32,
    vcd_back: Option<Value>,
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
        Reg {
            reset_value,
            prev: Value::undet(width),
            value: Value::undet(width),
            in_reset: false,
            async_rst,
            suppress: false,
            written_at: u64::MAX,
            vcd_id: 0,
            vcd_back: None,
        }
    }

    /// The no-reset ctor variant: value loaded at construction.
    fn preset(consts: &[Value]) -> Reg {
        let width = carg(consts, 0) as u32;
        let v = consts
            .get(1)
            .cloned()
            .unwrap_or_else(|| Value::undet(width))
            .zext(width);
        Reg {
            reset_value: v.clone(),
            prev: v.clone(),
            value: v,
            in_reset: false,
            async_rst: false,
            suppress: false,
            written_at: u64::MAX,
            vcd_id: 0,
            vcd_back: None,
        }
    }
}

impl Prim for Reg {
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        _clk: usize,
        _clk_vcd_id: u32,
    ) {
        self.vcd_id = vcd_flat_defs(w, name, self.value.width);
    }
    fn vcd_dump(
        &mut self,
        w: &mut crate::vcd::Vcd,
        dt: crate::vcd::DumpType,
        now: u64,
        _clk_edge_now: bool,
    ) {
        let v = self.value.clone();
        vcd_flat_dump(w, dt, now, self.vcd_id, &v, &mut self.vcd_back);
    }

    fn value_method(&mut self, method: &str, _args: &[Value], now: u64) -> Value {
        match method {
            "read" | "get" | "_read" => self.value.clone(),
            // crossing read: a same-instant write is not yet visible
            "crossed" => {
                if self.written_at == now {
                    self.prev.clone()
                } else {
                    self.value.clone()
                }
            }
            m => panic!("Reg: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], now: u64) {
        match method {
            "write" | "set" | "put" | "_write" => {
                // sync-reset registers never suppress writes — the reset
                // tick re-forces the reset value at the end of each
                // in-reset edge; only async regs block once suppressed
                // (METH_write, bs_prim_mod_reg.h:100)
                if !(self.async_rst && self.suppress) {
                    self.prev = std::mem::replace(&mut self.value, args[0].clone());
                    self.written_at = now;
                }
            }
            m => panic!("Reg: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, _gate: bool) {}
    fn rst_tick(&mut self, _now: u64) {
        // rst_tick__clk__1
        if self.in_reset {
            self.value = self.reset_value.clone();
            self.suppress = true;
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted {
            if self.async_rst {
                // async: reset_RST performs the tick immediately
                self.value = self.reset_value.clone();
                self.suppress = true;
            }
        } else {
            self.suppress = false;
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
    suppress: bool,
    vcd_id: u32,
    vcd_back: Option<Value>,
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
            suppress: false,
            vcd_id: 0,
            vcd_back: None,
        }
    }
}

impl Prim for ConfigReg {
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        _clk: usize,
        _clk_vcd_id: u32,
    ) {
        self.vcd_id = vcd_flat_defs(w, name, self.value.width);
    }
    fn vcd_dump(
        &mut self,
        w: &mut crate::vcd::Vcd,
        dt: crate::vcd::DumpType,
        now: u64,
        _clk_edge_now: bool,
    ) {
        let v = self.value.clone();
        vcd_flat_dump(w, dt, now, self.vcd_id, &v, &mut self.vcd_back);
    }

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
                if self.async_rst && self.suppress {
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
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, _gate: bool) {}
    fn rst_tick(&mut self, _now: u64) {
        if self.in_reset {
            self.value = self.reset_value.clone();
            self.old_value = self.reset_value.clone();
            self.written_at = u64::MAX;
            self.suppress = true;
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted {
            if self.async_rst {
                self.value = self.reset_value.clone();
                self.old_value = self.reset_value.clone();
                self.written_at = u64::MAX;
                self.suppress = true;
            }
        } else {
            self.suppress = false;
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
    /// latched at the clock tick: fired during the just-ended cycle
    written: bool,
    vcd_id: u32,
    vcd_back: Option<(bool, Value)>,
}

impl RWire {
    fn new(consts: &[Value], zero_width: bool) -> RWire {
        let width = if zero_width { 0 } else { carg(consts, 0) as u32 };
        RWire {
            width,
            value: Value::zero(width.max(1)),
            valid: false,
            written: false,
            vcd_id: 0,
            vcd_back: None,
        }
    }
}

impl Prim for RWire {
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        clk: usize,
        _clk_vcd_id: u32,
    ) {
        // bs_prim_mod_wire.h:87-97: one id, no scope; changes back-dated
        // to the clock edge; zero-width wires declare width 1
        self.vcd_id = w.reserve_ids(1);
        w.set_clock(self.vcd_id, clk);
        w.write_def(self.vcd_id, name, self.width.max(1));
    }
    fn vcd_dump(
        &mut self,
        w: &mut crate::vcd::Vcd,
        dt: crate::vcd::DumpType,
        now: u64,
        _clk_edge_now: bool,
    ) {
        use crate::vcd::DumpType as D;
        if dt == D::Xs {
            w.write_x(self.vcd_id, self.width.max(1), now);
            return;
        }
        let written = self.written;
        let dump = match (&self.vcd_back, dt) {
            (Some((bw, bv)), D::Changes) => {
                written != *bw || (written && *bw && self.value != *bv)
            }
            _ => true,
        };
        if dump {
            if self.width > 0 {
                if written {
                    w.write_val(self.vcd_id, &self.value, now);
                } else {
                    w.write_x(self.vcd_id, self.width, now);
                }
            } else {
                // zero-width (PulseWire): dump the 1-bit fired flag
                w.write_val(self.vcd_id, &Value::from_u64(1, written as u64), now);
            }
        }
        self.vcd_back = Some((written, self.value.clone()));
    }

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
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, gate: bool) {
        if !gate { return; }
        // latch for VCD: did the wire fire during the cycle that just ended
        self.written = self.valid;
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
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, _gate: bool) {}
}

// ===============

/// CReg with up to 5 ports (bs_prim_mod_reg.h:817): sequential port
/// writes are immediate; tick commits the registered view.
struct CReg {
    value: Value,       // live value, mutated by port writes
    value_reg: Value,   // value registered at the last edge
    reset_value: Value,
    in_reset: bool,
    async_rst: bool,
    suppress: bool,
    // VCD state (bs_prim_mod_reg.h:817+): per-port write history, the
    // registered value at cycle start, latched ENs
    write_val: Vec<Value>,
    did_write: Vec<bool>,
    did_write_rec: Vec<bool>,
    read_val0: Value,
    vcd_base: u32,
    vcd_back: Option<(Vec<Value>, Vec<bool>, Vec<Value>)>,
}

impl CReg {
    fn new(consts: &[Value], has_reset: bool, async_rst: bool) -> CReg {
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
            async_rst,
            suppress: false,
            write_val: (0..5).map(|_| Value::undet(width)).collect(),
            did_write: vec![false; 5],
            did_write_rec: vec![false; 5],
            read_val0: Value::undet(width),
            vcd_base: 0,
            vcd_back: None,
        }
    }
}

impl Prim for CReg {
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        clk: usize,
        _clk_vcd_id: u32,
    ) {
        // bs_prim_mod_reg.h:989-1022: parent-scope alias shares Q_OUT_0's
        // id; per-port Q_OUT_i/EN_i/D_IN_i, all clock-backdated
        let bits = self.value.width;
        let mut n = w.reserve_ids(3 * 5);
        self.vcd_base = n;
        w.write_def(n, name, bits);
        w.scope_start(name);
        for i in 0..5 {
            w.set_clock(n, clk);
            w.write_def(n, &format!("Q_OUT_{i}"), bits);
            n += 1;
            w.set_clock(n, clk);
            w.write_def(n, &format!("EN_{i}"), 1);
            n += 1;
            w.set_clock(n, clk);
            w.write_def(n, &format!("D_IN_{i}"), bits);
            n += 1;
        }
        w.scope_end();
    }
    fn vcd_dump(
        &mut self,
        w: &mut crate::vcd::Vcd,
        dt: crate::vcd::DumpType,
        now: u64,
        _clk_edge_now: bool,
    ) {
        use crate::vcd::DumpType as D;
        let bits = self.value.width;
        let bit = |b: bool| Value::from_u64(1, b as u64);
        let mut num = self.vcd_base;
        // chained Q_OUT values: port i sees earlier ports' writes
        let mut qouts: Vec<Value> = Vec::with_capacity(5);
        let mut tmp = self.read_val0.clone();
        for i in 0..5 {
            qouts.push(tmp.clone());
            if self.did_write_rec[i] {
                tmp = self.write_val[i].clone();
            }
        }
        match dt {
            D::Xs => {
                for _ in 0..5 {
                    w.write_x(num, bits, now);
                    num += 1;
                    w.write_x(num, 1, now);
                    num += 1;
                    w.write_x(num, bits, now);
                    num += 1;
                }
            }
            D::Changes => {
                let (bq, be, bd) = self.vcd_back.clone().unwrap_or_else(|| {
                    (
                        (0..5).map(|_| Value::undet(bits)).collect(),
                        vec![false; 5],
                        (0..5).map(|_| Value::undet(bits)).collect(),
                    )
                });
                for i in 0..5 {
                    if qouts[i] != bq[i] {
                        w.write_val(num, &qouts[i], now);
                    }
                    num += 1;
                    if self.did_write_rec[i] != be[i] {
                        w.write_val(num, &bit(self.did_write_rec[i]), now);
                    }
                    num += 1;
                    if self.write_val[i] != bd[i] {
                        w.write_val(num, &self.write_val[i], now);
                    }
                    num += 1;
                }
            }
            _ => {
                for i in 0..5 {
                    w.write_val(num, &qouts[i], now);
                    num += 1;
                    w.write_val(num, &bit(self.did_write_rec[i]), now);
                    num += 1;
                    w.write_val(num, &self.write_val[i], now);
                    num += 1;
                }
            }
        }
        self.vcd_back =
            Some((qouts, self.did_write_rec.clone(), self.write_val.clone()));
    }

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
            if !(self.async_rst && self.suppress) {
                self.value = args[0].clone();
                let i = method.as_bytes()[4].saturating_sub(b'0') as usize;
                if i < 5 {
                    self.did_write[i] = true;
                    self.write_val[i] = args[0].clone();
                }
            }
        } else {
            panic!("CReg: unknown action method {method:?}")
        }
    }
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, gate: bool) {
        if !gate { return; }
        // Q_OUT_0 starts from the value registered before this cycle
        self.read_val0 = self.value_reg.clone();
        self.value_reg = self.value.clone();
        for i in 0..5 {
            self.did_write_rec[i] = self.did_write[i];
            self.did_write[i] = false;
        }
    }
    fn rst_tick(&mut self, _now: u64) {
        if self.in_reset {
            self.value = self.reset_value.clone();
            self.value_reg = self.reset_value.clone();
            self.suppress = true;
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted {
            if self.async_rst {
                self.value = self.reset_value.clone();
                self.suppress = true;
            }
        } else {
            self.suppress = false;
        }
    }
}

// ===============

/// FIFO (bs_prim_mod_fifo.h): ring buffer with element count; guarded
/// FIFOs warn (and drop the op) on enq-to-full / deq-from-empty, judged
/// against the begin-of-cycle count when the opposite op already ran
/// this instant.  Loopy FIFOs allow deq-then-enq; bypass FIFOs allow
/// enq-then-deq.
#[derive(PartialEq, Clone, Copy)]
enum FifoType {
    Simple,
    Loopy,
    Bypass,
}

struct Fifo {
    data: Vec<Value>,
    fst: usize,
    elems: usize,
    saved_elems: usize,
    size: usize,
    ftype: FifoType,
    guard: bool,
    zero_width: bool,
    width: u32,
    enq_at: u64,
    deq_at: u64,
    clear_at: u64,
    full_name: String,
    in_reset: bool,
    suppress: bool,
    /// last attempted enqueue value (assigned before guard checks)
    dummyval: Value,
    vcd_base: u32,
    vcd_back: Option<FifoVcdBack>,
}

#[derive(Clone, PartialEq)]
struct FifoVcdBack {
    rst: bool,
    full_n: bool,
    empty_n: bool,
    enq: bool,
    d_in: Value,
    deq: bool,
    clr: bool,
    elems: usize,
    slots: Vec<Value>,
}

impl Fifo {
    fn new(
        width: u32,
        depth: u64,
        guard: bool,
        ftype: FifoType,
        zero_width: bool,
        path: &str,
    ) -> Fifo {
        let size = depth.max(1) as usize;
        let full_name = if path.is_empty() {
            "top".to_string()
        } else {
            format!("top.{path}")
        };
        Fifo {
            data: (0..size).map(|_| Value::undet(width.max(1))).collect(),
            fst: 0,
            elems: 0,
            saved_elems: 0,
            size,
            ftype,
            guard,
            zero_width,
            width,
            enq_at: u64::MAX,
            deq_at: u64::MAX,
            clear_at: u64::MAX,
            full_name,
            in_reset: false,
            suppress: false,
            dummyval: Value::undet(width.max(1)),
            vcd_base: 0,
            vcd_back: None,
        }
    }
}

impl Prim for Fifo {
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        clk: usize,
        clk_vcd_id: u32,
    ) {
        // bs_prim_mod_fifo.h:263-293
        let bits = self.width;
        let extra = if bits > 0 { 1 } else { 0 };
        let mut n = w.reserve_ids(self.size as u32 + 6 + extra);
        self.vcd_base = n;
        w.scope_start(name);
        w.write_def(clk_vcd_id, "CLK", 1);
        w.write_def(n, "RST", 1);
        n += 1;
        w.write_def(n, "FULL_N", 1);
        n += 1;
        w.write_def(n, "EMPTY_N", 1);
        n += 1;
        w.set_clock(n, clk);
        w.write_def(n, "ENQ", 1);
        n += 1;
        if bits > 0 {
            w.set_clock(n, clk);
            w.write_def(n, "D_IN", bits);
            n += 1;
        }
        w.set_clock(n, clk);
        w.write_def(n, "DEQ", 1);
        n += 1;
        w.set_clock(n, clk);
        w.write_def(n, "CLR", 1);
        n += 1;
        if bits > 0 {
            // alias of arr_0
            w.write_def(n, "D_OUT", bits);
        }
        for i in 0..self.size {
            w.write_def(n + i as u32, &format!("arr_{i}"), bits);
        }
        w.scope_end();
    }
    fn vcd_dump(
        &mut self,
        w: &mut crate::vcd::Vcd,
        dt: crate::vcd::DumpType,
        now: u64,
        clk_edge_now: bool,
    ) {
        use crate::vcd::DumpType as D;
        let bits = self.width;
        let bit = |b: bool| Value::from_u64(1, b as u64);
        let mut num = self.vcd_base;
        let rst = !self.in_reset;
        let full_n = self.elems < self.size;
        let empty_n = self.elems != 0;
        // fresh-backing state matches the C++ ctor-built shadow instance
        let mut back = self.vcd_back.take().unwrap_or_else(|| FifoVcdBack {
            rst: true,
            full_n: true,
            empty_n: false,
            enq: false,
            d_in: Value::undet(bits.max(1)),
            deq: false,
            clr: false,
            elems: 0,
            slots: (0..self.size).map(|_| Value::undet(bits.max(1))).collect(),
        });
        match dt {
            D::Xs => {
                for _ in 0..4 {
                    w.write_x(num, 1, now);
                    num += 1;
                }
                if bits > 0 {
                    w.write_x(num, bits, now);
                    num += 1;
                }
                w.write_x(num, 1, now);
                num += 1;
                w.write_x(num, 1, now);
                num += 1;
                for _ in 0..self.size {
                    w.write_x(num, bits, now);
                    num += 1;
                }
            }
            D::Changes => {
                if rst != back.rst {
                    w.write_val(num, &bit(rst), now);
                }
                num += 1;
                if full_n != back.full_n {
                    w.write_val(num, &bit(full_n), now);
                }
                num += 1;
                if empty_n != back.empty_n {
                    w.write_val(num, &bit(empty_n), now);
                }
                num += 1;
                // ENQ/DEQ/CLR only re-evaluated at a posedge of our clock;
                // their backing flags update only when written
                if clk_edge_now {
                    let did = self.enq_at == now;
                    if did != back.enq {
                        w.write_val(num, &bit(did), now);
                        back.enq = did;
                    }
                }
                num += 1;
                if bits > 0 {
                    if self.dummyval != back.d_in {
                        w.write_val(num, &self.dummyval, now);
                    }
                    num += 1;
                }
                if clk_edge_now {
                    let did = self.deq_at == now;
                    if did != back.deq {
                        w.write_val(num, &bit(did), now);
                        back.deq = did;
                    }
                }
                num += 1;
                if clk_edge_now {
                    let did = self.clear_at == now;
                    if did != back.clr {
                        w.write_val(num, &bit(did), now);
                        back.clr = did;
                    }
                }
                num += 1;
                for i in 0..self.size {
                    let idx = (self.fst + i) % self.size;
                    if i < self.elems
                        && (i >= back.elems || self.data[idx] != back.slots[i])
                    {
                        w.write_val(num, &self.data[idx], now);
                    } else if i >= self.elems && i < back.elems {
                        w.write_x(num, bits, now);
                    }
                    num += 1;
                }
            }
            _ => {
                let enq = self.enq_at == now;
                let deq = self.deq_at == now;
                let clr = self.clear_at == now;
                w.write_val(num, &bit(rst), now);
                num += 1;
                w.write_val(num, &bit(full_n), now);
                num += 1;
                w.write_val(num, &bit(empty_n), now);
                num += 1;
                w.write_val(num, &bit(enq), now);
                num += 1;
                if bits > 0 {
                    w.write_val(num, &self.dummyval, now);
                    num += 1;
                }
                w.write_val(num, &bit(deq), now);
                num += 1;
                w.write_val(num, &bit(clr), now);
                num += 1;
                for i in 0..self.size {
                    if i < self.elems {
                        w.write_val(num, &self.data[(self.fst + i) % self.size], now);
                    } else {
                        w.write_x(num, bits, now);
                    }
                    num += 1;
                }
                back.enq = enq;
                back.deq = deq;
                back.clr = clr;
            }
        }
        back.rst = rst;
        back.full_n = full_n;
        back.empty_n = empty_n;
        back.d_in = self.dummyval.clone();
        back.elems = self.elems;
        for i in 0..self.size {
            back.slots[i] = self.data[(self.fst + i) % self.size].clone();
        }
        self.vcd_back = Some(back);
    }

    fn value_method(&mut self, method: &str, _args: &[Value], now: u64) -> Value {
        match method {
            "first" => self.data[self.fst].clone(),
            "notFull" => Value::from_u64(1, (self.elems < self.size) as u64),
            "notEmpty" => Value::from_u64(1, (self.elems != 0) as u64),
            "i_notFull" => {
                let v = if self.ftype != FifoType::Loopy
                    && (self.enq_at == now || self.deq_at == now || self.clear_at == now)
                {
                    self.saved_elems < self.size
                } else {
                    self.elems < self.size
                };
                Value::from_u64(1, v as u64)
            }
            "i_notEmpty" => {
                let v = if self.ftype != FifoType::Loopy
                    && (self.enq_at == now || self.deq_at == now || self.clear_at == now)
                {
                    self.saved_elems != 0
                } else {
                    self.elems != 0
                };
                Value::from_u64(1, v as u64)
            }
            m => panic!("FIFO: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], now: u64) {
        if method == "enq" && !self.zero_width {
            // saved for VCD display before suppress/guard checks
            self.dummyval = args[0].clone();
        }
        if self.suppress {
            return;
        }
        match method {
            "enq" => {
                self.enq_at = now;
                if self.deq_at != now {
                    self.saved_elems = self.elems;
                }
                if self.elems == self.size
                    || (self.ftype != FifoType::Loopy
                        && self.guard
                        && self.deq_at == now
                        && self.saved_elems == self.size)
                {
                    println!("Warning: {} -- Enqueuing to a full fifo", self.full_name);
                } else if self.elems < self.size {
                    let v = if self.zero_width {
                        Value::zero(1)
                    } else {
                        args[0].clone()
                    };
                    self.data[(self.fst + self.elems) % self.size] = v;
                    self.elems += 1;
                }
            }
            "deq" => {
                self.deq_at = now;
                if self.enq_at != now {
                    self.saved_elems = self.elems;
                }
                if self.elems == 0
                    || (self.ftype != FifoType::Bypass
                        && self.guard
                        && self.enq_at == now
                        && self.saved_elems == 0)
                {
                    println!("Warning: {} -- Dequeuing from empty fifo", self.full_name);
                } else if self.elems != 0 {
                    self.fst = (self.fst + 1) % self.size;
                    self.elems -= 1;
                }
            }
            "clear" => {
                self.clear_at = now;
                if self.enq_at != now && self.deq_at != now {
                    self.saved_elems = self.elems;
                }
                self.elems = 0;
            }
            m => panic!("FIFO: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, _gate: bool) {}
    fn rst_tick(&mut self, now: u64) {
        // rst_tick_clk calls METH_clear (bs_prim_mod_fifo.h:227-233), so
        // clear_at is stamped — the VCD shows CLR=1 on the reset edge
        if self.in_reset && !self.suppress {
            self.clear_at = now;
            if self.enq_at != now && self.deq_at != now {
                self.saved_elems = self.elems;
            }
            self.elems = 0;
            self.suppress = true;
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if !asserted {
            self.suppress = false;
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
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        _clk: usize,
        clk_vcd_id: u32,
    ) {
        // bs_prim_mod_clockgen.h:40-46: single CLK_OUT var aliasing the
        // kernel-owned clock id; no ids reserved, no value dumping
        w.scope_start(name);
        w.write_def(clk_vcd_id, "CLK_OUT", 1);
        w.scope_end();
    }
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("ClockGen: unknown value method {method:?}")
    }
    fn action_method(&mut self, method: &str, _args: &[Value], _now: u64) {
        panic!("ClockGen: unknown action method {method:?}")
    }
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, _gate: bool) {}
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
    vcd_base: u32,
    vcd_back: Option<(Value, Value, Value)>,
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
            vcd_base: 0,
            vcd_back: None,
        }
    }
}

impl Prim for SyncBit {
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        _clk: usize,
        _clk_vcd_id: u32,
    ) {
        // MOD_Sync2/Sync15: dSyncReg1/dSyncReg2/sSyncReg;
        // MOD_Sync1: dSyncReg1/sSyncReg
        let n = w.reserve_ids(if self.two_stage { 3 } else { 2 });
        self.vcd_base = n;
        w.scope_start(name);
        w.write_def(n, "dSyncReg1", 1);
        if self.two_stage {
            w.write_def(n + 1, "dSyncReg2", 1);
            w.write_def(n + 2, "sSyncReg", 1);
        } else {
            w.write_def(n + 1, "sSyncReg", 1);
        }
        w.scope_end();
    }
    fn vcd_dump(
        &mut self,
        w: &mut crate::vcd::Vcd,
        dt: crate::vcd::DumpType,
        now: u64,
        _clk_edge_now: bool,
    ) {
        use crate::vcd::DumpType as D;
        let n = self.vcd_base;
        let ss = self.s.read(now);
        match dt {
            D::Xs => {
                w.write_x(n, 1, now);
                w.write_x(n + 1, 1, now);
                if self.two_stage {
                    w.write_x(n + 2, 1, now);
                }
            }
            D::Changes => {
                let (b1, b2, bs) = self.vcd_back.clone().unwrap_or((
                    Value::undet(1),
                    Value::undet(1),
                    Value::undet(1),
                ));
                if self.d1 != b1 {
                    w.write_val(n, &self.d1, now);
                }
                if self.two_stage {
                    if self.d2 != b2 {
                        w.write_val(n + 1, &self.d2, now);
                    }
                    if ss != bs {
                        w.write_val(n + 2, &ss, now);
                    }
                } else if ss != bs {
                    w.write_val(n + 1, &ss, now);
                }
            }
            _ => {
                w.write_val(n, &self.d1, now);
                if self.two_stage {
                    w.write_val(n + 1, &self.d2, now);
                    w.write_val(n + 2, &ss, now);
                } else {
                    w.write_val(n + 1, &ss, now);
                }
            }
        }
        self.vcd_back = Some((self.d1.clone(), self.d2.clone(), ss));
    }

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
    fn tick(&mut self, port: &str, now: u64, _clk_val: bool, gate: bool) {
        if !gate { return; }
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
    vcd_base: u32,
    vcd_back: Option<(Value, Value, Value, Value)>,
}

impl SyncPulse {
    fn new() -> SyncPulse {
        SyncPulse {
            d_pulse: Value::undet(1),
            d2: Value::undet(1),
            d1: Value::undet(1),
            s: SyncVar::new(Value::undet(1)),
            in_reset: false,
            vcd_base: 0,
            vcd_back: None,
        }
    }
}

impl Prim for SyncPulse {
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        _clk: usize,
        _clk_vcd_id: u32,
    ) {
        let n = w.reserve_ids(4);
        self.vcd_base = n;
        w.scope_start(name);
        w.write_def(n, "dSyncReg1", 1);
        w.write_def(n + 1, "dSyncReg2", 1);
        w.write_def(n + 2, "dSyncPulse", 1);
        w.write_def(n + 3, "sSyncReg", 1);
        w.scope_end();
    }
    fn vcd_dump(
        &mut self,
        w: &mut crate::vcd::Vcd,
        dt: crate::vcd::DumpType,
        now: u64,
        _clk_edge_now: bool,
    ) {
        use crate::vcd::DumpType as D;
        let n = self.vcd_base;
        let ss = self.s.read(now);
        let cur = [self.d1.clone(), self.d2.clone(), self.d_pulse.clone(), ss];
        match dt {
            D::Xs => {
                for i in 0..4 {
                    w.write_x(n + i, 1, now);
                }
            }
            D::Changes => {
                let b = self.vcd_back.clone().unwrap_or((
                    Value::undet(1),
                    Value::undet(1),
                    Value::undet(1),
                    Value::undet(1),
                ));
                let back = [b.0, b.1, b.2, b.3];
                for (i, v) in cur.iter().enumerate() {
                    if *v != back[i] {
                        w.write_val(n + i as u32, v, now);
                    }
                }
            }
            _ => {
                for (i, v) in cur.iter().enumerate() {
                    w.write_val(n + i as u32, v, now);
                }
            }
        }
        self.vcd_back =
            Some((cur[0].clone(), cur[1].clone(), cur[2].clone(), cur[3].clone()));
    }

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
    fn tick(&mut self, port: &str, now: u64, _clk_val: bool, gate: bool) {
        if !gate { return; }
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
    fn tick(&mut self, port: &str, now: u64, _clk_val: bool, gate: bool) {
        if !gate { return; }
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
    fn tick(&mut self, port: &str, now: u64, _clk_val: bool, gate: bool) {
        if !gate { return; }
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
    vcd_base: u32,
    vcd_back: Option<(bool, u32)>,
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
            vcd_base: 0,
            vcd_back: None,
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
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        _clk: usize,
        clk_vcd_id: u32,
    ) {
        // bs_prim_mod_resets.h SyncReset: CLK alias + IN_RST/OUT_RST.
        // The generated C++ never calls set_clk_0 on SyncReset, so its
        // CLK alias uses bk_clock_vcd_num(BAD_CLOCK_HANDLE) = the first
        // kernel clock's id — mirror that quirk (ids start at 0).
        let _ = clk_vcd_id;
        let n = w.reserve_ids(2);
        self.vcd_base = n;
        w.scope_start(name);
        w.write_def(0, "CLK", 1);
        w.write_def(n, "IN_RST", 1);
        w.write_def(n + 1, "OUT_RST", 1);
        w.scope_end();
    }
    fn vcd_dump(
        &mut self,
        w: &mut crate::vcd::Vcd,
        dt: crate::vcd::DumpType,
        now: u64,
        _clk_edge_now: bool,
    ) {
        use crate::vcd::DumpType as D;
        let bit = |b: bool| Value::from_u64(1, b as u64);
        let rst_out = self.in_reset || self.count > 1;
        match dt {
            D::Xs => {
                w.write_x(self.vcd_base, 1, now);
                w.write_x(self.vcd_base + 1, 1, now);
            }
            D::Changes => {
                let (b_in, b_count) = self.vcd_back.unwrap_or((false, 0));
                if self.in_reset != b_in {
                    w.write_val(self.vcd_base, &bit(!self.in_reset), now);
                }
                let b_out = b_in || b_count > 1;
                if rst_out != b_out {
                    w.write_val(self.vcd_base + 1, &bit(!rst_out), now);
                }
            }
            _ => {
                w.write_val(self.vcd_base, &bit(!self.in_reset), now);
                w.write_val(self.vcd_base + 1, &bit(!rst_out), now);
            }
        }
        self.vcd_back = Some((self.in_reset, self.count));
    }
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("SyncReset: unknown value method {method:?}")
    }
    fn action_method(&mut self, method: &str, _args: &[Value], _now: u64) {
        panic!("SyncReset: unknown action method {method:?}")
    }
    fn tick(&mut self, port: &str, _now: u64, _clk_val: bool, gate: bool) {
        if !gate { return; }
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
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, _gate: bool) {}
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
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        _clk: usize,
        _clk_vcd_id: u32,
    ) {
        // bs_prim_mod_resets.h: InitialReset writes an empty scope yet
        // reserves 3 ids that are never used
        let _ = w.reserve_ids(3);
        w.scope_start(name);
        w.scope_end();
    }
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("InitialReset: unknown value method {method:?}")
    }
    fn action_method(&mut self, method: &str, _args: &[Value], _now: u64) {
        panic!("InitialReset: unknown action method {method:?}")
    }
    fn tick(&mut self, port: &str, _now: u64, _clk_val: bool, gate: bool) {
        if !gate { return; }
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
    vcd_id: u32,
    vcd_back: Option<Value>,
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
            vcd_id: 0,
            vcd_back: None,
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
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        _clk: usize,
        _clk_vcd_id: u32,
    ) {
        // bs_prim_mod_resets.h:340-347: one scope with a single 1-bit
        // "rst" var (the internal rstSync synchronizer dumps nothing)
        self.vcd_id = w.reserve_ids(1);
        w.scope_start(name);
        w.write_def(self.vcd_id, "rst", 1);
        w.scope_end();
    }
    fn vcd_dump(
        &mut self,
        w: &mut crate::vcd::Vcd,
        dt: crate::vcd::DumpType,
        now: u64,
        _clk_edge_now: bool,
    ) {
        let v = Value::from_u64(1, self.rst as u64);
        vcd_flat_dump(w, dt, now, self.vcd_id, &v, &mut self.vcd_back);
    }

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
    fn tick(&mut self, port: &str, now: u64, _clk_val: bool, gate: bool) {
        if !gate { return; }
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

// ===============
// Dynamic clock sources (bs_prim_mod_clockgen.h): kernel clocks with no
// predefined waveform; edges are triggered from input-clock ticks.

/// MOD_MakeClock: clock level driven by setClockValue, gated by
/// setGateCond (latched while the output is low).
pub struct MakeClock {
    init_high: bool,
    init_gate: bool,
    current_high: bool,
    old_out_high: bool,
    gate_out: bool,
    new_gate: bool,
    in_reset: bool,
    edges: Vec<bool>,
}

impl MakeClock {
    fn new(consts: &[Value]) -> MakeClock {
        let init_high = carg(consts, 0) != 0;
        let init_gate = consts.get(1).map(|v| v.as_u64() != 0).unwrap_or(true);
        MakeClock {
            init_high,
            init_gate,
            current_high: init_high,
            old_out_high: init_high,
            gate_out: init_gate,
            new_gate: init_gate,
            in_reset: false,
            edges: Vec::new(),
        }
    }
}

impl Prim for MakeClock {
    fn gate_out(&self) -> bool {
        self.gate_out
    }
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "getClockValue" => Value::from_u64(1, self.current_high as u64),
            "getGateCond" => Value::from_u64(1, self.new_gate as u64),
            m => panic!("MakeClock: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], _now: u64) {
        match method {
            "setClockValue" => {
                if !self.in_reset {
                    self.current_high = args[0].as_bool();
                }
            }
            "setGateCond" => {
                if !self.in_reset {
                    self.new_gate = args[0].as_bool();
                }
            }
            m => panic!("MakeClock: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, port: &str, _now: u64, _clk_val: bool, _gate: bool) {
        match port {
            "clk" => {
                if self.in_reset {
                    return;
                }
                if !self.old_out_high && self.current_high && self.gate_out {
                    self.edges.push(true);
                    self.old_out_high = true;
                } else if self.old_out_high && !self.current_high {
                    self.edges.push(false);
                    self.old_out_high = false;
                }
                if !self.current_high {
                    self.gate_out = self.new_gate;
                }
            }
            p => panic!("MakeClock: unknown tick port {p:?}"),
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted {
            self.current_high = self.init_high;
            self.gate_out = self.init_gate;
            self.new_gate = self.init_gate;
        }
    }
    fn take_clock_edges(&mut self) -> Vec<bool> {
        std::mem::take(&mut self.edges)
    }
}

/// MOD_ClockDivider: counts input posedges through [lower, upper]; the
/// output is high while cntr >= 2^(width-1).
pub struct ClockDivider {
    transition: u64,
    lower: u64,
    upper: u64,
    offset: u64,
    cntr: u64,
    gate_out: bool,
    in_reset: bool,
    edges: Vec<bool>,
}

impl ClockDivider {
    fn new(consts: &[Value]) -> ClockDivider {
        let width = carg(consts, 0);
        let lower = carg(consts, 1);
        let upper = carg(consts, 2);
        let offset = carg(consts, 3);
        ClockDivider {
            transition: 1 << (width - 1),
            lower,
            upper,
            offset,
            cntr: upper - offset,
            gate_out: false,
            in_reset: false,
            edges: Vec::new(),
        }
    }
}

impl Prim for ClockDivider {
    fn gate_out(&self) -> bool {
        self.gate_out
    }
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "clockReady" => Value::from_u64(1, (self.cntr == self.transition - 1) as u64),
            m => panic!("ClockDiv: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, _args: &[Value], _now: u64) {
        panic!("ClockDiv: unknown action method {method:?}")
    }
    fn tick(&mut self, port: &str, _now: u64, _clk_val: bool, gate: bool) {
        match port {
            "clk" => {
                if self.in_reset {
                    return;
                }
                if self.cntr < self.transition {
                    self.gate_out = gate;
                }
                if self.cntr == self.upper {
                    self.cntr = self.lower;
                    if self.gate_out {
                        self.edges.push(false);
                        self.gate_out = gate;
                    }
                } else {
                    self.cntr += 1;
                    if self.cntr == self.transition && self.gate_out {
                        self.edges.push(true);
                    }
                }
            }
            p => panic!("ClockDiv: unknown tick port {p:?}"),
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted {
            let prev = self.cntr;
            self.cntr = self.upper - self.offset;
            if prev >= self.transition && self.cntr < self.transition {
                self.edges.push(false);
            } else if prev < self.transition && self.cntr >= self.transition {
                self.edges.push(true);
            }
            self.gate_out = false;
        }
    }
    fn take_clock_edges(&mut self) -> Vec<bool> {
        std::mem::take(&mut self.edges)
    }
}

/// MOD_ClockInverter: output is the complement of (clk & gate), updated
/// on both input edges.
pub struct ClockInverter {
    current_high: bool,
    gate_out: bool,
    edges: Vec<bool>,
}

impl ClockInverter {
    fn new() -> ClockInverter {
        ClockInverter { current_high: false, gate_out: true, edges: Vec::new() }
    }
}

impl Prim for ClockInverter {
    fn gate_out(&self) -> bool {
        self.gate_out
    }
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "clockReady" => Value::from_u64(1, 1),
            m => panic!("ClockInverter: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, _args: &[Value], _now: u64) {
        panic!("ClockInverter: unknown action method {method:?}")
    }
    fn tick(&mut self, port: &str, _now: u64, clk_val: bool, gate: bool) {
        match port {
            "clk" => {
                let mut new_high = !(clk_val && gate);
                if !self.gate_out {
                    new_high = false;
                }
                if new_high != self.current_high {
                    self.edges.push(new_high);
                }
                self.current_high = new_high;
                if !new_high {
                    self.gate_out = gate;
                }
            }
            p => panic!("ClockInverter: unknown tick port {p:?}"),
        }
    }
    fn take_clock_edges(&mut self) -> Vec<bool> {
        std::mem::take(&mut self.edges)
    }
}

// ===============
// MOD_SyncFIFO (bs_prim_mod_synchronizers.h:845): a depth-2^k FIFO whose
// source and destination sides run on different clocks; head/tail indices
// cross domains through SyncVar-like registers, counting modulo 2*depth.

/// A cross-domain index register with non-blocking-assignment visibility
/// (SyncVar<I> over plain integers).
struct SyncIdx {
    prev: u64,
    cur: u64,
    written_at: u64,
}

impl SyncIdx {
    fn new() -> SyncIdx {
        SyncIdx { prev: 0, cur: 0, written_at: u64::MAX }
    }
    fn read(&self, now: u64) -> u64 {
        if self.written_at == now {
            self.prev
        } else {
            self.cur
        }
    }
    fn probe(&self) -> u64 {
        self.cur
    }
    fn write(&mut self, x: u64, now: u64) {
        self.prev = self.cur;
        self.cur = x;
        self.written_at = now;
    }
    fn force(&mut self, x: u64) {
        self.prev = x;
        self.cur = x;
        self.written_at = u64::MAX;
    }
}

struct SyncFifo {
    width: u32,
    depth: u64,
    has_clear: bool,
    idx_bits: u32,
    mask: u64,
    data: Vec<Value>,
    d_dout: Value,
    src_hi: SyncIdx,
    dst_lo: SyncIdx,
    src_lo: u64,
    dst_hi: u64,
    src_hi_plus_1: u64,
    dst_lo_plus_1: u64,
    d_sync_reg1: u64,
    s_sync_reg1: u64,
    s_count: u64,
    d_count: u64,
    not_empty: bool,
    not_full: bool,
    in_reset: bool,
    s_reset: bool,
    d_reset: bool,
    did_enq: bool,
    did_deq: bool,
    did_sclear: bool,
    did_dclear: bool,
    s_clr: Handshake,
    d_clr: Handshake,
}

impl SyncFifo {
    fn new(width: u32, depth: u64, has_clear: bool) -> SyncFifo {
        let depth = depth.max(1);
        let idx_bits = 64 - depth.leading_zeros(); // index_size(depth)
        SyncFifo {
            width,
            depth,
            has_clear,
            idx_bits,
            mask: (1u64 << idx_bits) - 1,
            data: (0..depth).map(|_| Value::undet(width.max(1))).collect(),
            d_dout: Value::undet(width.max(1)),
            src_hi: SyncIdx::new(),
            dst_lo: SyncIdx::new(),
            src_lo: 0,
            dst_hi: 0,
            src_hi_plus_1: 1,
            dst_lo_plus_1: 1,
            d_sync_reg1: 0,
            s_sync_reg1: 0,
            s_count: 0,
            d_count: 0,
            not_empty: false,
            not_full: true,
            in_reset: false,
            s_reset: false,
            d_reset: false,
            did_enq: false,
            did_deq: false,
            did_sclear: false,
            did_dclear: false,
            s_clr: Handshake::new(false, false),
            d_clr: Handshake::new(false, false),
        }
    }

    fn meth_not_empty(&self) -> bool {
        !self.d_reset
            && if self.depth != 1 {
                self.not_empty
            } else {
                self.dst_hi != self.dst_lo.probe()
            }
    }
    fn meth_not_full(&self) -> bool {
        !self.s_reset && self.not_full
    }

    fn clk_src(&mut self, now: u64) {
        self.s_reset = self.in_reset;
        if self.s_reset
            || (self.has_clear
                && (self.did_sclear || !self.s_clr.rdy_send() || self.d_clr.pulse(now)))
        {
            self.src_hi.force(0);
            self.src_hi_plus_1 = 1;
            self.not_full = false;
            self.s_count = 0;
        } else if self.did_enq {
            self.not_full = (self.src_hi_plus_1 ^ self.depth) != self.src_lo;
            self.s_count = if self.src_hi_plus_1 > self.src_lo {
                self.src_hi_plus_1.wrapping_sub(self.src_lo) & self.mask
            } else {
                (self.src_hi_plus_1 + 2 * self.depth - self.src_lo) & self.mask
            };
            self.src_hi.write(self.src_hi_plus_1, now);
            self.src_hi_plus_1 = (self.src_hi_plus_1 + 1) % (2 * self.depth);
        } else {
            let h = self.src_hi.read(now);
            self.not_full = (h ^ self.depth) != self.src_lo;
            self.s_count = if h > self.src_lo {
                h.wrapping_sub(self.src_lo) & self.mask
            } else {
                (h + 2 * self.depth - self.src_lo) & self.mask
            };
        }
        self.did_sclear = false;
        self.did_enq = false;

        // synchronize index from destination side
        self.src_lo = self.s_sync_reg1;
        self.s_sync_reg1 = self.dst_lo.read(now);

        if self.depth == 1 {
            self.not_full = self.src_hi.probe() == self.src_lo;
            self.s_count = if self.not_full { 0 } else { 1 };
        }

        self.s_clr.clk_src(now);
        self.d_clr.clk_dst(now);
    }

    fn clk_dst(&mut self, now: u64) {
        self.d_reset = self.in_reset;
        if self.d_reset
            || (self.has_clear
                && (self.did_dclear || !self.d_clr.rdy_send() || self.s_clr.pulse(now)))
        {
            self.dst_lo.force(0);
            self.dst_lo_plus_1 = 1;
            self.not_empty = false;
            self.d_count = 0;
        } else if self.did_deq {
            self.not_empty = self.dst_hi != self.dst_lo.read(now);
            self.d_count = if self.dst_hi > self.dst_lo_plus_1 {
                self.dst_hi.wrapping_sub(self.dst_lo.read(now)) & self.mask
            } else {
                (self.dst_hi + 2 * self.depth - self.dst_lo.read(now)) & self.mask
            };
            if self.not_empty {
                if self.depth != 1 {
                    self.d_dout =
                        self.data[(self.dst_lo.read(now) % self.depth) as usize].clone();
                }
                self.dst_lo.write(self.dst_lo_plus_1, now);
                self.dst_lo_plus_1 = (self.dst_lo_plus_1 + 1) % (2 * self.depth);
            }
        } else {
            self.d_count = if self.dst_hi > self.dst_lo.read(now) {
                self.dst_hi.wrapping_sub(self.dst_lo.read(now)) & self.mask
            } else {
                (self.dst_hi + 2 * self.depth - self.dst_lo.read(now)) & self.mask
            };
            if self.depth != 1 && !self.not_empty && self.dst_hi != self.dst_lo.read(now) {
                self.d_dout =
                    self.data[(self.dst_lo.read(now) % self.depth) as usize].clone();
                self.dst_lo.write(self.dst_lo_plus_1, now);
                self.dst_lo_plus_1 = (self.dst_lo_plus_1 + 1) % (2 * self.depth);
                self.not_empty = true;
            }
        }
        self.did_dclear = false;
        self.did_deq = false;

        // synchronize index from source side
        self.dst_hi = self.d_sync_reg1;
        self.d_sync_reg1 = self.src_hi.read(now);

        if self.depth == 1 {
            self.not_empty = self.dst_lo.probe() == self.dst_hi;
            self.d_count = if self.not_empty { 1 } else { 0 };
        }

        self.s_clr.clk_dst(now);
        self.d_clr.clk_src(now);
    }
}

impl Prim for SyncFifo {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "notEmpty" | "dNotEmpty" | "RDY_first" | "RDY_deq" => {
                Value::from_u64(1, self.meth_not_empty() as u64)
            }
            "first" => self.d_dout.clone(),
            "notFull" | "sNotFull" | "RDY_enq" => {
                Value::from_u64(1, self.meth_not_full() as u64)
            }
            "sCount" => Value::from_u64(self.idx_bits.max(1), self.s_count),
            "dCount" => Value::from_u64(self.idx_bits.max(1), self.d_count),
            "RDY_sClear" => Value::from_u64(1, self.s_clr.rdy_send() as u64),
            "RDY_dClear" => Value::from_u64(1, self.d_clr.rdy_send() as u64),
            m => panic!("SyncFIFO: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], now: u64) {
        match method {
            "enq" => {
                if self.width > 0 {
                    let x = args[0].clone();
                    if self.depth == 1 {
                        self.d_dout = x.clone();
                    }
                    let idx = (self.src_hi.read(now) % self.depth) as usize;
                    self.data[idx] = x;
                }
                self.did_enq = true;
            }
            "deq" => self.did_deq = true,
            "sClear" => {
                self.s_clr.send();
                self.did_sclear = true;
            }
            "dClear" => {
                self.d_clr.send();
                self.did_dclear = true;
            }
            m => panic!("SyncFIFO: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, port: &str, now: u64, _clk_val: bool, gate: bool) {
        if !gate { return; }
        match port {
            "clk_src" => self.clk_src(now),
            "clk_dst" => self.clk_dst(now),
            p => panic!("SyncFIFO: unknown tick port {p:?}"),
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        self.s_clr.reset(asserted);
        self.d_clr.reset(asserted);
        if asserted {
            self.src_lo = 0;
            self.dst_hi = 0;
            self.src_hi.force(0);
            self.dst_lo.force(0);
            self.src_hi_plus_1 = 1;
            self.dst_lo_plus_1 = 1;
            self.d_sync_reg1 = 0;
            self.s_sync_reg1 = 0;
            self.s_count = 0;
            self.d_count = 0;
            self.s_reset = true;
            self.d_reset = true;
            self.did_enq = false;
            self.did_deq = false;
            self.did_sclear = false;
            self.did_dclear = false;
        }
    }
}

// ===============
// MOD_BRAM (bs_prim_mod_bram.h): request-latched block RAM.  put records
// the request; the port's clock tick performs the memory access and loads
// the output register (write-first).  `pipelined` adds one more output
// stage.  Byte enables write `chunk_size`-bit lanes selected by the
// write-enable bits.

struct BramPort {
    upd_at: u64,
    upd_addr: u64,
    upd_wens: u64,
    upd_val: Value,
    written_at: u64,
    upd_prev: Value,
    out: Value,
    out2: Value,
}

impl BramPort {
    fn new(width: u32) -> BramPort {
        BramPort {
            upd_at: u64::MAX,
            upd_addr: 0,
            upd_wens: 0,
            upd_val: Value::undet(width),
            written_at: u64::MAX,
            upd_prev: Value::undet(width),
            out: Value::undet(width),
            out2: Value::undet(width),
        }
    }
}

struct Bram {
    pipelined: bool,
    dual: bool,
    addr_bits: u32,
    width: u32,
    hi_addr: u64,
    chunk_size: u32,
    num_wens: u32,
    full_name: String,
    data: std::collections::HashMap<u64, Value>,
    a: BramPort,
    b: BramPort,
    vcd_base: u32,
    vcd_back: Option<(BramVcdBack, BramVcdBack)>,
}

#[derive(Clone)]
struct BramVcdBack {
    en: bool,
    wens: u64,
    addr: u64,
    di: Value,
    dout: Value,
}

impl Bram {
    #[allow(clippy::too_many_arguments)]
    fn new(
        pipelined: bool,
        dual: bool,
        addr_bits: u32,
        width: u32,
        chunk_size: u32,
        num_wens: u32,
        mem_size: u64,
        path: &str,
        file: Option<(String, bool)>,
    ) -> Bram {
        let leaf = path.rsplit('.').next().unwrap_or(path).to_string();
        let full_name = if path.is_empty() {
            "top".to_string()
        } else {
            format!("top.{path}")
        };
        let mut b = Bram {
            pipelined,
            dual,
            addr_bits,
            width,
            hi_addr: mem_size.saturating_sub(1),
            chunk_size,
            num_wens,
            full_name,
            data: Default::default(),
            a: BramPort::new(width),
            b: BramPort::new(width),
            vcd_base: 0,
            vcd_back: None,
        };
        if let Some((f, bin)) = file {
            let (ab, w, hi) = (b.addr_bits, b.width, b.hi_addr);
            let data = &mut b.data;
            load_mem_file(&f, bin, ab, w, 0, hi, &leaf, &mut |a, v| {
                data.insert(a, v);
            });
        }
        b
    }

    fn addr_hex(&self, a: u64) -> String {
        let digits = ((self.addr_bits + 3) / 4).max(1) as usize;
        format!("{a:0digits$x}")
    }

    fn put(&mut self, port_b: bool, wens: u64, addr: u64, val: Value, now: u64, pname: &str) {
        if addr > self.hi_addr {
            println!(
                "Warning: BRAM '{}' -- {} address on port {} is out of bounds: {}",
                self.full_name,
                if wens != 0 { "Write" } else { "Read" },
                pname,
                self.addr_hex(addr)
            );
        }
        let p = if port_b { &mut self.b } else { &mut self.a };
        p.upd_at = now;
        p.upd_addr = addr;
        p.upd_wens = wens;
        p.upd_val = val;
    }

    fn clk(&mut self, port_b: bool, now: u64) {
        let (pa, pb) = (&mut self.a, &mut self.b);
        let (me, other) = if port_b { (pb, pa) } else { (pa, pb) };
        me.out2 = me.out.clone();
        if me.upd_at != now {
            return;
        }
        let is_write = me.upd_wens != 0;
        if me.upd_addr > self.hi_addr {
            me.out = Value::undet(self.width);
        } else if is_write {
            let cur = self
                .data
                .get(&me.upd_addr)
                .cloned()
                .unwrap_or_else(|| Value::undet(self.width));
            // previous value: if the other port wrote the same address at
            // this instant, use its pre-write value
            me.written_at = now;
            me.upd_prev = if other.written_at == now && other.upd_addr == me.upd_addr {
                other.upd_prev.clone()
            } else {
                cur.clone()
            };
            let merged = {
                let mut r = cur;
                for n in 0..self.num_wens {
                    if me.upd_wens >> n & 1 != 0 {
                        let lo = (n * self.chunk_size) as u64;
                        let hi = lo + self.chunk_size as u64 - 1;
                        let chunk = me.upd_val.extract(hi, lo, self.chunk_size);
                        let width = self.width;
                        let mut nv = chunk;
                        if lo > 0 {
                            nv = nv.concat(&r.extract(lo - 1, 0, lo as u32), (hi + 1) as u32);
                        }
                        if hi + 1 < width as u64 {
                            let high_bits = (width as u64 - 1 - hi) as u32;
                            nv = r
                                .extract(width as u64 - 1, hi + 1, high_bits)
                                .concat(&nv, width);
                        }
                        r = nv.zext(width);
                    }
                }
                r
            };
            self.data.insert(me.upd_addr, merged.clone());
            me.out = merged;
        } else {
            // read: if the other port wrote the same address at this
            // instant, read the pre-write value
            let v = if other.written_at == now && other.upd_addr == me.upd_addr {
                other.upd_prev.clone()
            } else {
                self.data
                    .get(&me.upd_addr)
                    .cloned()
                    .unwrap_or_else(|| Value::undet(self.width))
            };
            me.out = v;
        }
    }
}

impl Prim for Bram {
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        clk: usize,
        clk_vcd_id: u32,
    ) {
        // bs_prim_mod_bram.h:756-797: ports only, never memory contents
        let mut n = w.reserve_ids(if self.dual { 10 } else { 5 });
        self.vcd_base = n;
        w.scope_start(name);
        let ports: &[(&str, &str, &str, &str, &str, &str)] = if self.dual {
            &[
                ("CLKA", "ENA", "WEA", "ADDRA", "DIA", "DOA"),
                ("CLKB", "ENB", "WEB", "ADDRB", "DIB", "DOB"),
            ]
        } else {
            &[("CLK", "EN", "WE", "ADDR", "DI", "DO")]
        };
        for (pclk, pen, pwe, paddr, pdi, pdo) in ports {
            w.write_def(clk_vcd_id, pclk, 1);
            w.set_clock(n, clk);
            w.write_def(n, pen, 1);
            n += 1;
            w.set_clock(n, clk);
            w.write_def(n, pwe, self.num_wens);
            n += 1;
            w.set_clock(n, clk);
            w.write_def(n, paddr, self.addr_bits);
            n += 1;
            w.set_clock(n, clk);
            w.write_def(n, pdi, self.width);
            n += 1;
            w.write_def(n, pdo, self.width);
            n += 1;
        }
        w.scope_end();
    }
    fn vcd_dump(
        &mut self,
        w: &mut crate::vcd::Vcd,
        dt: crate::vcd::DumpType,
        now: u64,
        clk_edge_now: bool,
    ) {
        use crate::vcd::DumpType as D;
        let bit = |b: bool| Value::from_u64(1, b as u64);
        let fresh = |width: u32| BramVcdBack {
            en: false,
            wens: 0,
            addr: 0,
            di: Value::undet(width.max(1)),
            dout: Value::undet(width.max(1)),
        };
        let (mut back_a, mut back_b) = self
            .vcd_back
            .take()
            .unwrap_or_else(|| (fresh(self.width), fresh(self.width)));
        let mut num = self.vcd_base;
        let nports = if self.dual { 2 } else { 1 };
        for pi in 0..nports {
            let (p, back) = if pi == 0 {
                (&self.a, &mut back_a)
            } else {
                (&self.b, &mut back_b)
            };
            let en = p.upd_at == now;
            let dout = if self.pipelined { p.out2.clone() } else { p.out.clone() };
            match dt {
                D::Xs => {
                    w.write_x(num, 1, now);
                    num += 1;
                    w.write_x(num, self.num_wens, now);
                    num += 1;
                    w.write_x(num, self.addr_bits, now);
                    num += 1;
                    w.write_x(num, self.width, now);
                    num += 1;
                    w.write_x(num, self.width, now);
                    num += 1;
                }
                D::Changes => {
                    // both ports gate on the (single modeled) clock edge
                    if clk_edge_now {
                        let did_write = en && p.upd_wens != 0;
                        let back_did_write = back.en && back.wens != 0;
                        if en != back.en {
                            w.write_val(num, &bit(en), now);
                            back.en = en;
                        }
                        num += 1;
                        if did_write != back_did_write || p.upd_wens != back.wens {
                            // WE displays 0 while EN is low in CHANGES mode
                            let wv = if en { p.upd_wens } else { 0 };
                            w.write_val(
                                num,
                                &Value::from_u64(self.num_wens.max(1), wv),
                                now,
                            );
                        }
                        num += 1;
                        if p.upd_addr != back.addr {
                            w.write_val(
                                num,
                                &Value::from_u64(self.addr_bits.max(1), p.upd_addr),
                                now,
                            );
                        }
                        num += 1;
                        if p.upd_val != back.di {
                            w.write_val(num, &p.upd_val, now);
                        }
                        num += 1;
                        if dout != back.dout {
                            w.write_val(num, &dout, now);
                        }
                        num += 1;
                    } else {
                        num += 5;
                    }
                }
                _ => {
                    w.write_val(num, &bit(en), now);
                    num += 1;
                    w.write_val(num, &Value::from_u64(self.num_wens.max(1), p.upd_wens), now);
                    num += 1;
                    w.write_val(
                        num,
                        &Value::from_u64(self.addr_bits.max(1), p.upd_addr),
                        now,
                    );
                    num += 1;
                    w.write_val(num, &p.upd_val, now);
                    num += 1;
                    w.write_val(num, &dout, now);
                    num += 1;
                    back.en = en;
                }
            }
            if dt != D::Xs {
                back.wens = p.upd_wens;
                back.addr = p.upd_addr;
                back.di = p.upd_val.clone();
                back.dout = dout;
            }
        }
        self.vcd_back = Some((back_a, back_b));
    }

    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "read" | "a_read" => {
                if self.pipelined {
                    self.a.out2.clone()
                } else {
                    self.a.out.clone()
                }
            }
            "b_read" => {
                if self.pipelined {
                    self.b.out2.clone()
                } else {
                    self.b.out.clone()
                }
            }
            m => panic!("BRAM: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], now: u64) {
        match method {
            "put" | "a_put" => {
                let wens = args[0].as_u64();
                let addr = args[1].as_u64();
                self.put(false, wens, addr, args[2].clone(), now, "A");
            }
            "b_put" => {
                let wens = args[0].as_u64();
                let addr = args[1].as_u64();
                self.put(true, wens, addr, args[2].clone(), now, "B");
            }
            m => panic!("BRAM: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, port: &str, now: u64, _clk_val: bool, gate: bool) {
        if !gate { return; }
        match port {
            "clk" | "clkA" => self.clk(false, now),
            "clkB" => self.clk(true, now),
            p => panic!("BRAM: unknown tick port {p:?}"),
        }
    }
}



// ===============
// VCD helpers shared by flat single-var prims (MOD_Reg-style):
// one id, one $var named by the instance, XS -> x, CHANGES -> diffed.

fn vcd_flat_defs(w: &mut crate::vcd::Vcd, name: &str, width: u32) -> u32 {
    let id = w.reserve_ids(1);
    w.write_def(id, name, width);
    id
}

fn vcd_flat_dump(
    w: &mut crate::vcd::Vcd,
    dt: crate::vcd::DumpType,
    now: u64,
    id: u32,
    value: &Value,
    back: &mut Option<Value>,
) {
    use crate::vcd::DumpType as D;
    match dt {
        D::Xs => w.write_x(id, value.width, now),
        D::Changes => {
            if back.as_ref() != Some(value) {
                w.write_val(id, value, now);
            }
        }
        _ => w.write_val(id, value, now),
    }
    *back = Some(value.clone());
}

// ===============
// Reset combinators (bs_prim_mod_resets.h).

/// MOD_ResetMux: two reset inputs, a select register (updated at end of
/// timeslice when changed), output follows the selected input.
struct ResetMux {
    sel_a: bool,
    new_sel_a: bool,
    a_asserted: bool,
    b_asserted: bool,
    select_changed: bool,
    pending: Vec<(bool, bool)>,
}

impl ResetMux {
    fn new() -> ResetMux {
        ResetMux {
            sel_a: false,
            new_sel_a: false,
            // rst_in values start 0 (asserted) in the C++
            a_asserted: true,
            b_asserted: true,
            select_changed: false,
            pending: Vec::new(),
        }
    }
}

impl Prim for ResetMux {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("ResetMux: unknown value method {method:?}")
    }
    fn action_method(&mut self, method: &str, args: &[Value], _now: u64) {
        match method {
            "select" => self.new_sel_a = args[0].as_bool(),
            m => panic!("ResetMux: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, port: &str, _now: u64, _clk_val: bool, gate: bool) {
        if !gate { return; }
        match port {
            "xclk" => {
                if self.new_sel_a != self.sel_a {
                    self.select_changed = true;
                }
            }
            p => panic!("ResetMux: unknown tick port {p:?}"),
        }
    }
    fn set_reset_input(&mut self, input: usize, asserted: bool) {
        if input == 0 {
            self.a_asserted = asserted;
            if self.sel_a {
                self.pending.push((asserted, true));
            }
        } else {
            self.b_asserted = asserted;
            if !self.sel_a {
                self.pending.push((asserted, true));
            }
        }
    }
    fn end_of_timeslice(&mut self) {
        if self.select_changed {
            self.select_changed = false;
            self.sel_a = self.new_sel_a;
            if self.a_asserted != self.b_asserted {
                let v = if self.sel_a { self.a_asserted } else { self.b_asserted };
                self.pending.push((v, false));
            }
        }
    }
    fn take_reset_out(&mut self) -> Vec<(bool, bool)> {
        std::mem::take(&mut self.pending)
    }
}

/// MOD_ResetEither: output asserted while either input is asserted;
/// transitions propagate only while the other input is deasserted.
struct ResetEither {
    a_asserted: bool,
    b_asserted: bool,
    pending: Vec<(bool, bool)>,
}

impl ResetEither {
    fn new() -> ResetEither {
        ResetEither { a_asserted: false, b_asserted: false, pending: Vec::new() }
    }
}

impl Prim for ResetEither {
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("ResetEither: unknown value method {method:?}")
    }
    fn action_method(&mut self, method: &str, _args: &[Value], _now: u64) {
        panic!("ResetEither: unknown action method {method:?}")
    }
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, _gate: bool) {}
    fn set_reset_input(&mut self, input: usize, asserted: bool) {
        if input == 0 {
            if asserted != self.a_asserted && !self.b_asserted {
                self.pending.push((asserted, true));
            }
            self.a_asserted = asserted;
        } else {
            if asserted != self.b_asserted && !self.a_asserted {
                self.pending.push((asserted, true));
            }
            self.b_asserted = asserted;
        }
    }
    fn take_reset_out(&mut self) -> Vec<(bool, bool)> {
        std::mem::take(&mut self.pending)
    }
}


/// MOD_GatedClock: a gate-condition register (async-reset, ConfigReg-like
/// latch) whose output gate updates while the input clock is low.
struct GatedClock {
    reg: bool,
    reset_value: bool,
    gate_out: bool,
    clk_in_gate: bool,
    clk_low: bool,
    in_reset: bool,
    suppress: bool,
}

impl GatedClock {
    fn new(consts: &[Value]) -> GatedClock {
        let v = carg(consts, 0) != 0;
        GatedClock {
            // C++ starts reg undet (1-bit undet = 0) and gate_out 0
            reg: false,
            reset_value: v,
            gate_out: false,
            clk_in_gate: false,
            clk_low: true,
            in_reset: false,
            suppress: false,
        }
    }
    fn update_new_gate(&mut self) {
        if self.clk_low {
            self.gate_out = self.clk_in_gate && self.reg;
        }
    }
}

impl Prim for GatedClock {
    fn gate_out(&self) -> bool {
        self.gate_out
    }
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        match method {
            "getGateCond" => Value::from_u64(1, self.reg as u64),
            m => panic!("GatedClock: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], _now: u64) {
        match method {
            "setGateCond" => {
                if !self.suppress {
                    // rules run while the clock is high (the kernel flips
                    // the level before edge logic), so the transparent-low
                    // latch never updates here; the value lands at the
                    // next negedge clk_in tick
                    self.reg = args[0].as_bool();
                }
            }
            m => panic!("GatedClock: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, port: &str, _now: u64, clk_val: bool, gate: bool) {
        match port {
            // called on both edges of the input clock
            "clk_in" => {
                self.clk_low = !clk_val;
                self.clk_in_gate = gate;
                self.update_new_gate();
            }
            p => panic!("GatedClock: unknown tick port {p:?}"),
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted {
            self.suppress = true;
            self.reg = self.reset_value;
            self.update_new_gate();
        } else {
            self.suppress = false;
        }
    }
}

// ===============
// MOD_RegTwo (bs_prim_mod_reg.h:648): a register with two write ports
// (setA wins over a same-instant setB) whose get() reads the
// begin-of-instant value (NBA visibility).

struct RegTwo {
    value: Value,
    old_value: Value,
    reset_value: Value,
    written: u64,
    a_at: u64,
    async_rst: bool,
    in_reset: bool,
    suppress: bool,
    vcd_id: u32,
    vcd_back: Option<Value>,
}

impl RegTwo {
    fn new(consts: &[Value], has_reset: bool, async_rst: bool) -> RegTwo {
        let width = carg(consts, 0) as u32;
        let reset_value = if has_reset && consts.len() > 1 {
            consts[1].zext(width)
        } else {
            Value::undet(width)
        };
        RegTwo {
            value: Value::undet(width),
            old_value: Value::undet(width),
            reset_value,
            written: u64::MAX,
            a_at: u64::MAX,
            async_rst,
            in_reset: false,
            suppress: false,
            vcd_id: 0,
            vcd_back: None,
        }
    }
    fn note_write(&mut self, now: u64) {
        if self.written != now {
            self.old_value = self.value.clone();
            self.written = now;
        }
    }
}

impl Prim for RegTwo {
    fn vcd_defs(
        &mut self,
        w: &mut crate::vcd::Vcd,
        name: &str,
        _clk: usize,
        _clk_vcd_id: u32,
    ) {
        self.vcd_id = vcd_flat_defs(w, name, self.value.width);
    }
    fn vcd_dump(
        &mut self,
        w: &mut crate::vcd::Vcd,
        dt: crate::vcd::DumpType,
        now: u64,
        _clk_edge_now: bool,
    ) {
        let v = self.value.clone();
        vcd_flat_dump(w, dt, now, self.vcd_id, &v, &mut self.vcd_back);
    }

    fn value_method(&mut self, method: &str, _args: &[Value], now: u64) -> Value {
        match method {
            "get" | "read" => {
                if self.written == now {
                    self.old_value.clone()
                } else {
                    self.value.clone()
                }
            }
            m => panic!("RegTwo: unknown value method {m:?}"),
        }
    }
    fn action_method(&mut self, method: &str, args: &[Value], now: u64) {
        if self.async_rst && self.suppress {
            return;
        }
        match method {
            "setA" => {
                self.note_write(now);
                self.a_at = now;
                self.value = args[0].clone();
            }
            "setB" => {
                self.note_write(now);
                if self.a_at != now {
                    self.value = args[0].clone();
                }
            }
            m => panic!("RegTwo: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, _port: &str, _now: u64, _clk_val: bool, _gate: bool) {}
    fn rst_tick(&mut self, _now: u64) {
        if self.in_reset {
            self.value = self.reset_value.clone();
            self.suppress = true;
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if asserted {
            if self.async_rst {
                self.value = self.reset_value.clone();
                self.suppress = true;
            }
        } else {
            self.suppress = false;
        }
    }
}

// ===============
// MOD_ClockMux / MOD_ClockSelect (bs_prim_mod_clockmux.h): output clock
// muxes.  ClockMux switches combinationally on select; ClockSelect
// registers the selector on xclk and generates a synchronized reset held
// for `stages` output cycles after a switch.

struct ClockMux {
    sel_a: bool,
    a_clk: bool,
    a_gate: bool,
    b_clk: bool,
    b_gate: bool,
    new_clk: bool,
    gate_out: bool,
    edges: Vec<bool>,
}

impl ClockMux {
    fn new() -> ClockMux {
        ClockMux {
            sel_a: false,
            a_clk: false,
            a_gate: false,
            b_clk: false,
            b_gate: false,
            new_clk: false,
            gate_out: true,
            edges: Vec::new(),
        }
    }
    fn do_clock(&mut self) {
        let old = self.new_clk;
        self.new_clk = if self.sel_a { self.a_clk } else { self.b_clk };
        self.gate_out = if self.sel_a { self.a_gate } else { self.b_gate };
        if self.new_clk != old {
            self.edges.push(self.new_clk);
        }
    }
}

impl Prim for ClockMux {
    fn gate_out(&self) -> bool {
        self.gate_out
    }
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("ClockMux: unknown value method {method:?}")
    }
    fn action_method(&mut self, method: &str, args: &[Value], _now: u64) {
        match method {
            "select" => {
                self.sel_a = args[0].as_bool();
                self.do_clock();
            }
            m => panic!("ClockMux: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, port: &str, _now: u64, clk_val: bool, gate: bool) {
        match port {
            "aClk" => {
                self.a_clk = clk_val;
                self.a_gate = gate;
                self.do_clock();
            }
            "bClk" => {
                self.b_clk = clk_val;
                self.b_gate = gate;
                self.do_clock();
            }
            "xclk" => self.do_clock(),
            p => panic!("ClockMux: unknown tick port {p:?}"),
        }
    }
    fn take_clock_edges(&mut self) -> Vec<bool> {
        std::mem::take(&mut self.edges)
    }
}

struct ClockSelect {
    reset_delay: u64,
    reset_hold: u64,
    sel: bool,
    sel2: bool,
    written: u64,
    in_reset: bool,
    a_clk: bool,
    a_gate: bool,
    b_clk: bool,
    b_gate: bool,
    new_clk: bool,
    gate_out: bool,
    changed: bool,
    changed_negedge: u64,
    last_now: u64,
    edges: Vec<bool>,
    rst_pending: Vec<(bool, bool)>,
}

impl ClockSelect {
    fn new(consts: &[Value]) -> ClockSelect {
        let stages = carg(consts, 0);
        ClockSelect {
            reset_delay: stages,
            reset_hold: stages + 1,
            sel: false,
            sel2: false,
            written: u64::MAX,
            in_reset: false,
            a_clk: false,
            a_gate: true,
            b_clk: false,
            b_gate: true,
            new_clk: false,
            gate_out: true,
            changed: false,
            changed_negedge: u64::MAX,
            last_now: 0,
            edges: Vec::new(),
            rst_pending: Vec::new(),
        }
    }
    fn do_clock_and_reset(&mut self, now: u64) {
        let old_clk = self.new_clk;
        self.new_clk = if self.sel { self.a_clk } else { self.b_clk };
        self.gate_out = if self.sel { self.a_gate } else { self.b_gate };

        let prev_changed = self.changed;
        self.changed = (self.sel != self.sel2) || self.in_reset;
        if !self.changed && prev_changed {
            self.changed_negedge = now;
        }

        if self.new_clk != old_clk {
            self.edges.push(self.new_clk);
        }

        if (self.new_clk && !old_clk) || (self.changed && !prev_changed) {
            if self.changed || self.changed_negedge == now {
                if self.reset_hold > self.reset_delay {
                    // assert the output reset at end of timeslice
                    self.rst_pending.push((true, false));
                }
                self.reset_hold = 0;
            } else {
                if self.reset_hold <= self.reset_delay {
                    self.reset_hold += 1;
                }
                if self.reset_hold > self.reset_delay {
                    self.rst_pending.push((false, false));
                }
            }
        }
    }
}

impl Prim for ClockSelect {
    fn gate_out(&self) -> bool {
        self.gate_out
    }
    fn value_method(&mut self, method: &str, _args: &[Value], _now: u64) -> Value {
        panic!("ClockSelect: unknown value method {method:?}")
    }
    fn action_method(&mut self, method: &str, args: &[Value], now: u64) {
        match method {
            "select" => {
                self.last_now = now;
                if !self.in_reset {
                    self.written = now;
                    self.sel2 = self.sel;
                    self.sel = args[0].as_bool();
                    self.do_clock_and_reset(now);
                }
            }
            m => panic!("ClockSelect: unknown action method {m:?}"),
        }
    }
    fn tick(&mut self, port: &str, now: u64, clk_val: bool, gate: bool) {
        self.last_now = now;
        match port {
            "aClk" => {
                self.a_clk = clk_val;
                self.a_gate = gate;
                self.do_clock_and_reset(now);
            }
            "bClk" => {
                self.b_clk = clk_val;
                self.b_gate = gate;
                self.do_clock_and_reset(now);
            }
            "xclk" => {
                if !self.in_reset && self.written != now {
                    self.sel2 = self.sel;
                    self.do_clock_and_reset(now);
                }
            }
            p => panic!("ClockSelect: unknown tick port {p:?}"),
        }
    }
    fn set_in_reset(&mut self, asserted: bool) {
        self.in_reset = asserted;
        if !asserted {
            self.sel = false;
            self.sel2 = false;
        }
        // reset_RST re-evaluates the clock/reset logic (this is what
        // asserts the generated reset while the input reset is held)
        self.do_clock_and_reset(self.last_now);
    }
    fn take_clock_edges(&mut self) -> Vec<bool> {
        std::mem::take(&mut self.edges)
    }
    fn take_reset_out(&mut self) -> Vec<(bool, bool)> {
        std::mem::take(&mut self.rst_pending)
    }
}
