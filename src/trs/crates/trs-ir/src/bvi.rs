//! Imported BVI Verilog modules: the contract an external RTL engine
//! (Verilator, or a substituted model — same eight-function shim ABI)
//! must satisfy.  Design of record: the KB draft "KB: BVI-via-Verilator
//! design (trs)", v4; the M0 spike (`src/trs/spike/bvi-m0/`) validated
//! this shape end-to-end against Verilator 5.020 (XML metadata) and
//! 5.050 (JSON metadata) before this schema was written.
//!
//! Everything here is *declared* information from the `import "BVI"`
//! statement (VModInfo + AVInst types), pre-checked by the exporter's
//! refusal suite: paths are consistently directed (source method ordered
//! before reader), output ports are un-aliased, self-SBR ActionValue
//! results are consumed only atomically-with-the-last-call.  The runtime
//! trusts the contract; lying modules are the external checker's and
//! `TRS_BVI_CHECK=observe`'s problem, by standing decision.
//!
//! Per-output dependency cones are deliberately NOT stored: they derive
//! deterministically from `methods` + `paths` (owning method's args +
//! enable, plus declared path sources), and storing them would invite
//! inconsistency.  The loader computes them once at instantiation.

use serde::{Deserialize, Serialize};

use crate::StrId;

/// Port property bit: `(*reg*)` on an input (latched-on-arrival).  The
/// exporter refuses these in v1; carried so the refusal can be asserted
/// at decode too and so a future lift needs no schema change.
pub const BVI_PROP_REG: u32 = 1;
/// Port property bit: `(*inhigh*)` enable.  Such an enable has NO
/// physical port on the model; it appears in the port table for method
/// bookkeeping but must never be matched against model metadata.
pub const BVI_PROP_INHIGH: u32 = 2;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BviContract {
    /// Verilog module name (the verilate top).
    pub verilog_name: StrId,
    /// Physical port table; indices below refer into this.
    pub ports: Vec<BviPort>,
    pub methods: Vec<BviMethod>,
    /// Input clocks (output clocks are refused at export in v1).
    pub clocks: Vec<BviClock>,
    /// Input resets.
    pub resets: Vec<BviReset>,
    /// Output resets (v1.2): an output port the runtime samples and
    /// routes into the interpreter's derived-reset network.  At most one
    /// per import (the export refuses more).  The parent module's reset
    /// list carries the derived node under the "<inst>$<port>" wire
    /// name, exactly like the native reset-generator prims.
    #[serde(default)]
    pub out_resets: Vec<BviOutReset>,
    /// Output clocks (v1.2): oscillator output ports the runtime
    /// samples at the commit point; edges route through the
    /// interpreter's dynamic-clock network (the ClockDiv mechanism)
    /// under the "<inst>$<port>" wire name.  Gated output clocks are
    /// refused at export.
    #[serde(default)]
    pub out_clocks: Vec<BviOutClock>,
    /// Module parameters, typed — baked at verilate time via `-G`
    /// (semantics-preserving serialization is the link step's job).
    pub params: Vec<BviParam>,
    /// Combinational paths (input port -> output port): the declared
    /// `path(..)` clauses PLUS the exporter-synthesized implicit
    /// arg->own-result paths of every value/ActionValue method.
    pub paths: Vec<(u32, u32)>,
    /// Directories and explicitly named files for resolving the Verilog
    /// source closure at link (`vpath_nub` semantics + `-y` search).
    pub vpath: Vec<StrId>,
    pub vfiles: Vec<StrId>,
    /// `-D` defines the design was elaborated under; part of the
    /// verilate invocation AND the cache fingerprint (an `ifdef`-guarded
    /// source must not fingerprint-match a different elaboration).
    #[serde(default)]
    pub defines: Vec<(StrId, Option<StrId>)>,
    /// Constant module Port arguments: (port index, value).  Driven once
    /// at construction, before the startup eval — unlike `params` these
    /// are runtime port drives, not verilate-time `-G` bakes.
    #[serde(default)]
    pub const_args: Vec<(u32, BviParamValue)>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BviPort {
    /// Verilog port name (origName; the shim maps to mangled members).
    pub name: StrId,
    pub width: u32,
    pub dir: BviDir,
    pub kind: BviPortKind,
    /// BVI_PROP_* bits.
    #[serde(default)]
    pub props: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BviDir {
    Input,
    Output,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BviPortKind {
    Clock,
    ClockGate,
    Reset,
    Enable,
    Rdy,
    MethodArg,
    MethodResult,
    /// Constant module Port argument, driven once at construction
    /// (dynamic Port args are refused at export).
    ConstArg,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BviMethod {
    pub name: StrId,
    pub kind: BviMethodKind,
    /// Index into `clocks`; None = clockless (legal for value methods
    /// only — clockless Action/AV is refused at export).
    pub clock: Option<u32>,
    /// Port indices, in argument order.
    pub args: Vec<u32>,
    pub results: Vec<u32>,
    pub enable: Option<u32>,
    pub rdy: Option<u32>,
    /// Declared self-SBR: multiple same-instant calls REPLACE (register
    /// semantics, last firing caller's arguments win at the edge).  The
    /// exporter has already enforced the atomic-read condition on AV
    /// results (consumers are schedule-last or pairwise-exclusive), so
    /// the runtime applies replacement unconditionally.
    #[serde(default)]
    pub self_sbr: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BviMethodKind {
    Value,
    Action,
    ActionValue,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BviClock {
    /// BVI-side clock name (`default_clock`, named input_clocks).
    pub name: StrId,
    /// Oscillator port index: driven with the RAW oscillator level on
    /// both edges (never gated — `realClockPorts`, AState.hs:1404).
    pub osc_port: u32,
    /// Optional gate port index: a LEVEL input, settled pre-edge in
    /// commit phase (a).
    pub gate_port: Option<u32>,
    /// Tick-port name this clock's edges arrive under (QualifiedTick
    /// routing; same-(instance, oscillator, edge) ticks are merged at
    /// export so coincident edges commit in one batched eval).
    pub tick_port: StrId,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BviReset {
    pub name: StrId,
    pub port: u32,
    /// Which Reset instantiation argument carries this reset, as a
    /// position in `Instance::args`.  Stated rather than counted: a
    /// portless input reset is a legal association with nothing to
    /// drive, so it is a Reset argument that has no entry here, and the
    /// two lists do not correspond position for position.  Same
    /// convention as `const_args` and `BviParamValue::FromArg`.
    pub arg: u32,
    pub active_low: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BviOutReset {
    /// BVI-side logical reset name.
    pub name: StrId,
    /// Output port index (dir Output, kind Reset; active-low level, the
    /// bsc reset convention).
    pub port: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BviOutClock {
    /// BVI-side logical clock name.
    pub name: StrId,
    /// Oscillator output port index (dir Output, kind Clock).
    pub port: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BviParam {
    pub name: StrId,
    pub value: BviParamValue,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BviParamValue {
    /// Signed integer parameter — serialized as a signed decimal
    /// literal, never an unsigned hex reinterpretation.
    IntSigned { width: u32, value: i64 },
    /// Unsigned bit-vector of any width — serialized as a sized hex
    /// literal; the hex digits live in the string table.
    Bits { width: u32, hex: StrId },
    /// String parameter — exact Verilog escaping is the link step's job.
    Str(StrId),
    /// Real parameter — round-trip formatted.
    Real(f64),
    /// A parameter FORWARDED across a synthesis boundary: its value is
    /// not a literal at the import site but a closed function of the
    /// enclosing module's own parameters (`mkWrap#(Integer w) ...
    /// parameter W = w`).  `arg` indexes the instance's generic args;
    /// the interpreter evaluates it in the parent context at
    /// instantiation and the verilate step runs with the RESOLVED
    /// value (each distinct valuation is its own cache class).
    FromArg {
        arg: u32,
        width: u32,
        kind: BviParamRefKind,
    },
}

/// The serialization class of a forwarded parameter, from the
/// import-site argument's type (the resolved runtime Value alone
/// cannot distinguish a real from a 64-bit vector).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BviParamRefKind {
    Bits,
    Real,
    Str,
}

impl BviContract {
    /// Per-output declared dependency cone: the owning method's args and
    /// enable, plus every declared/implicit path source targeting the
    /// port.  Used by observation-frontier caching and by
    /// `TRS_BVI_CHECK=observe` for witness attribution.
    pub fn cone_of(&self, out_port: u32) -> Vec<u32> {
        let mut cone = Vec::new();
        for m in &self.methods {
            let owns = m.results.contains(&out_port) || m.rdy == Some(out_port);
            if owns {
                cone.extend_from_slice(&m.args);
                if let Some(en) = m.enable {
                    cone.push(en);
                }
            }
        }
        for &(from, to) in &self.paths {
            if to == out_port && !cone.contains(&from) {
                cone.push(from);
            }
        }
        cone.sort_unstable();
        cone.dedup();
        cone
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Design, Instance, InstanceKind, Module, Schedule, BIR_VERSION};

    /// The M0 counter fixture (spike/bvi-m0/contracts/counter.json)
    /// re-expressed in the real schema -- the R1 gate.
    fn counter_contract(strings: &mut Vec<String>) -> BviContract {
        let mut sid = |s: &str| -> StrId {
            strings.push(s.to_string());
            (strings.len() - 1) as StrId
        };
        BviContract {
            verilog_name: sid("BviCounter"),
            ports: vec![
                BviPort {
                    name: sid("CLK"),
                    width: 1,
                    dir: BviDir::Input,
                    kind: BviPortKind::Clock,
                    props: 0,
                },
                BviPort {
                    name: sid("RST_N"),
                    width: 1,
                    dir: BviDir::Input,
                    kind: BviPortKind::Reset,
                    props: 0,
                },
                BviPort {
                    name: sid("EN_bump"),
                    width: 1,
                    dir: BviDir::Input,
                    kind: BviPortKind::Enable,
                    props: 0,
                },
                BviPort {
                    name: sid("bump_amt"),
                    width: 8,
                    dir: BviDir::Input,
                    kind: BviPortKind::MethodArg,
                    props: 0,
                },
                BviPort {
                    name: sid("count"),
                    width: 8,
                    dir: BviDir::Output,
                    kind: BviPortKind::MethodResult,
                    props: 0,
                },
                BviPort {
                    name: sid("RDY_bump"),
                    width: 1,
                    dir: BviDir::Output,
                    kind: BviPortKind::Rdy,
                    props: 0,
                },
            ],
            methods: vec![
                BviMethod {
                    name: sid("bump"),
                    kind: BviMethodKind::Action,
                    clock: Some(0),
                    args: vec![3],
                    results: vec![],
                    enable: Some(2),
                    rdy: Some(5),
                    self_sbr: false,
                },
                BviMethod {
                    name: sid("read"),
                    kind: BviMethodKind::Value,
                    clock: Some(0),
                    args: vec![],
                    results: vec![4],
                    enable: None,
                    rdy: None,
                    self_sbr: false,
                },
            ],
            clocks: vec![BviClock {
                name: sid("clk"),
                osc_port: 0,
                gate_port: None,
                tick_port: sid("clk"),
            }],
            resets: vec![BviReset {
                name: sid("rst"),
                port: 1,
                arg: 0,
                active_low: true,
            }],
            params: vec![],
            paths: vec![],
            vpath: vec![],
            vfiles: vec![],
            defines: vec![],
            const_args: vec![],
            out_resets: vec![],
            out_clocks: vec![],
        }
    }

    fn design_with_counter() -> Design {
        let mut strings = vec!["mkTop".to_string(), "the_counter".to_string()];
        let contract = counter_contract(&mut strings);
        Design {
            str_ids: Default::default(),
            uses_wave_tasks: false,
            strings,
            top: 0,
            modules: vec![Module {
                name: 0,
                externs: vec![],
                foreign_calls: vec![],
                content_hash: [0; 32],
                keep_fires: false,
                default_clock: None,
                default_reset: None,
                clock_domains: vec![],
                resets: vec![],
                inputs: vec![],
                input_clocks: vec![],
                ifc_clocks: vec![],
                ifc_clock_gates: vec![],
                ifc_resets: vec![],
                instances: vec![Instance {
                    name: 1,
                    kind: InstanceKind::Bvi(Box::new(contract)),
                    clock_args: vec![],
                    elab_order: 0,
                    prim_clocks: None,
                    args: vec![],
                    method_order: vec![],
                    port_counts: vec![],
                }],
                defs: vec![],
                def_ix: Default::default(),
                rules: vec![],
                methods: vec![],
                method_ix: Default::default(),
                schedule: Schedule::default(),
            }],
            compositions: vec![],
            foreign_funcs: vec![],
            default_clock: None,
            default_reset: None,
        }
    }

    #[test]
    fn bvi_roundtrip_and_verify() {
        let d = design_with_counter();
        let bytes = d.encode();
        let d2 = Design::decode(&bytes).expect("decode+verify");
        let InstanceKind::Bvi(c) = &d2.modules[0].instances[0].kind else {
            panic!("kind lost in round-trip");
        };
        assert_eq!(d2.name(c.verilog_name), "BviCounter");
        assert_eq!(c.ports.len(), 6);
        assert_eq!(c.methods[0].rdy, Some(5));
        // cone of `count` (port 4): read has no args/EN, no declared paths.
        assert!(c.cone_of(4).is_empty());
        // cone of RDY_bump (port 5): bump's arg + enable.
        assert_eq!(c.cone_of(5), vec![2, 3]);
    }

    #[test]
    fn bvi_verify_rejects_aliased_output() {
        let mut d = design_with_counter();
        // make `read` also claim RDY_bump as a result: output aliasing.
        if let InstanceKind::Bvi(c) = &mut d.modules[0].instances[0].kind {
            c.methods[1].results.push(5);
        }
        let bytes = d.encode();
        let err = Design::decode(&bytes).unwrap_err().to_string();
        assert!(err.contains("aliased"), "unexpected error: {err}");
    }

    #[test]
    fn bvi_verify_rejects_clockless_action() {
        let mut d = design_with_counter();
        if let InstanceKind::Bvi(c) = &mut d.modules[0].instances[0].kind {
            c.methods[0].clock = None;
        }
        let bytes = d.encode();
        let err = Design::decode(&bytes).unwrap_err().to_string();
        assert!(err.contains("clockless"), "unexpected error: {err}");
    }

    #[test]
    fn bvi_verify_rejects_backward_path() {
        let mut d = design_with_counter();
        if let InstanceKind::Bvi(c) = &mut d.modules[0].instances[0].kind {
            c.paths.push((4, 3)); // output -> input: nonsense
        }
        let bytes = d.encode();
        let err = Design::decode(&bytes).unwrap_err().to_string();
        assert!(err.contains("input -> output"), "unexpected error: {err}");
    }
}
