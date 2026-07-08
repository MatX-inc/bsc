//! BIR — the Bluesim IR.
//!
//! This is the data contract between bsc's Haskell exporter
//! (`src/comp/SimExportIR.hs`, phase P0) and the Rust backend.  It mirrors
//! the post-scheduling `SimPackage` view of a module (`SimPackage.hs`):
//! the `APackage` contents (defs, rules, state instances, interface) plus
//! the parts of `AScheduleInfo` that simulation consumes.
//!
//! Design notes (see DESIGN.md §3.1):
//! - Serialized as CBOR with an explicit schema version; decode-time
//!   validation, no silent skew against bsc.
//! - This models what the *backend* needs, not everything bsc knows.

pub mod expr;
pub mod schedule;
pub mod verify;

use serde::{Deserialize, Serialize};

pub use expr::{Action, Expr, PrimOp, Stmt};
pub use schedule::{Composition, ModuleSchedule, SchedNode, Schedule, Segment};

/// Schema version; bumped on any incompatible change.  The bsc exporter
/// writes it, `Design::decode` rejects mismatches.
pub const BIR_VERSION: u32 = 1;

/// Identifier interned per design; display names live in `Design::strings`.
pub type StrId = u32;

/// A whole linked design: the top module and every module in its hierarchy.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Design {
    pub version: u32,
    /// String table; all `StrId`s index into this.
    pub strings: Vec<String>,
    pub top: StrId,
    pub modules: Vec<Module>,
    /// Hierarchical instance path -> module name (`ssys_instmap` analogue).
    pub instance_map: Vec<(StrId, StrId)>,
    /// Per-(clock, edge) interleavings of instance segments — the design
    /// schedule, exported hierarchically (see `schedule` module docs).
    pub compositions: Vec<Composition>,
    /// Foreign (BDPI) function signatures used anywhere in the design.
    pub foreign_funcs: Vec<ForeignFunc>,
    pub default_clock: Option<StrId>,
    pub default_reset: Option<StrId>,
}

/// One synthesized module (one `.ba` / one `SimPackage`).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub name: StrId,
    /// Hash of the module's exported content, for the object cache.
    pub content_hash: [u8; 32],
    pub clock_domains: Vec<ClockDomain>,
    pub resets: Vec<Reset>,
    pub inputs: Vec<Port>,
    /// Interface output clocks: external port name (e.g. CLK_outclk) ->
    /// the internal osc wire being re-exported (a constant = noClock,
    /// which never ticks).
    pub ifc_clocks: Vec<(StrId, Expr)>,
    /// Submodule / primitive instances.
    pub instances: Vec<Instance>,
    /// Combinational defs, including CAN_FIRE_* / WILL_FIRE_*.
    pub defs: Vec<Def>,
    pub rules: Vec<Rule>,
    pub methods: Vec<Method>,
    /// This module type's segmented schedule; the design-level interleaving
    /// lives in `Design::compositions`.
    pub schedule: Schedule,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClockDomain {
    pub id: u32,
    /// Clocks in this domain: (oscillator, gate) expressions.
    pub clocks: Vec<(Expr, Expr)>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Reset {
    pub id: u32,
    pub wire: Expr,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Port {
    pub name: StrId,
    pub width: u32,
    pub kind: PortKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PortKind {
    Clock,
    ClockGate,
    Reset,
    MethodArg,
    MethodEnable,
    Parameter,
}

/// A state-element or submodule instantiation (`AVInst` analogue).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Instance {
    pub name: StrId,
    pub kind: InstanceKind,
    /// Instantiation arguments; constant by construction (Bluesim rejects
    /// dynamic instantiation args, `SimExpand.hs:2158`).
    pub args: Vec<Expr>,
    /// Pairs (a, b) of methods where a must execute before b within one
    /// atomic action — the `sSB` relation (`MethodOrderMap`).
    pub method_order: Vec<(StrId, StrId)>,
    /// Method name -> number of used ports (multi-ported methods).
    pub port_counts: Vec<(StrId, u32)>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InstanceKind {
    /// A primitive with codegen support (possibly fully inlined).
    Prim(Primitive),
    /// Another user module in this design.
    Module(StrId),
}

/// Primitives the backend knows how to lay out or call into trs-rt.
/// The full set today is `SimPrimitiveModules.hs:263-348`; this enum grows
/// with the phases in DESIGN.md §10.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Primitive {
    /// Reg / RegU / RegA — inlined to a plain state field.
    Reg { width: u32, reset: RegReset },
    /// ConfigReg: reads see begin-of-cycle value regardless of order.
    ConfigReg { width: u32, reset: RegReset },
    /// CReg with `ports` sequential read/write ports.
    CReg { width: u32, ports: u8, reset: RegReset },
    /// RWire / Wire / PulseWire (width 0 = PulseWire).
    Wire { width: u32 },
    Fifo { width: u32, depth: u32, guarded: bool, loopy: bool, bypass: bool },
    RegFile { width: u32, addr_width: u32, binary_init: Option<StrId> },
    Bram { width: u32, addr_width: u32, ports: u8, byte_enables: u32 },
    ClockGen { params: Vec<u64> },
    GatedClock,
    ClockDivider { divisor: u32 },
    SyncReg { width: u32, stages: u8 },
    SyncFifo { width: u32, depth: u32 },
    /// Escape hatch during bring-up: named primitive handled by trs-rt.
    Other { name: StrId },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RegReset {
    None,
    Sync,
    Async,
}

/// A combinational definition (`ADef`).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Def {
    pub name: StrId,
    pub width: u32,
    pub expr: Expr,
    pub props: DefProps,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct DefProps {
    pub can_fire: bool,
    pub will_fire: bool,
    /// Signed display preference (from removed sign casts).
    pub signed: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rule {
    pub name: StrId,
    /// Reference to the CAN_FIRE def for this rule.
    pub can_fire: StrId,
    /// Reference to the WILL_FIRE def for this rule.
    pub will_fire: StrId,
    pub body: Vec<Stmt>,
    pub clock_domain: u32,
    /// `clock_crossing_rule` — executed in the after-edge function.
    pub crossing: bool,
    /// Intra-module ME inhibitors: disjoint rules executing *earlier* in
    /// this module's segment order whose CAN_FIREs are negated into this
    /// rule's effective CAN_FIRE — the destructive-execution correctness
    /// patch (`mkMERuleInhibits`, `SimMakeCBlocks.hs:1636-1658`).  Fixed
    /// per module type; cross-module pairs are in
    /// `Composition::cross_inhibits`.
    pub me_inhibits: Vec<StrId>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Method {
    pub name: StrId,
    pub kind: MethodKind,
    pub args: Vec<Port>,
    pub ready: Option<Expr>,
    pub body: Vec<Stmt>,
    pub result: Option<Expr>,
    pub clock_domain: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MethodKind {
    Value,
    Action,
    ActionValue,
}

/// BDPI import signature (`ForeignFunctions.hs`); the C ABI is preserved
/// exactly (DESIGN.md §5.4).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForeignFunc {
    pub name: StrId,
    pub c_name: StrId,
    pub ret: ForeignType,
    pub args: Vec<ForeignType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ForeignType {
    Void,
    /// Narrow value passed/returned by value: char for <=8 bits,
    /// unsigned int for <=32, unsigned long long for <=64 (toCtype).
    Bits(u32),
    /// Wide value: passed as an `unsigned int*` little-endian 32-bit limb
    /// pointer; a wide RETURN becomes an out-pointer first argument with a
    /// void return (mkFFDecl).
    Wide(u32),
    /// Polymorphic: pointer to the value in 32-bit storage (any actual
    /// width); returns use the wide out-pointer convention.
    Poly,
    CString,
}

#[derive(Debug)]
pub enum DecodeError {
    Cbor(String),
    VersionMismatch { found: u32, expected: u32 },
    Invalid(verify::VerifyError),
}

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DecodeError::Cbor(e) => write!(f, "CBOR decode error: {e}"),
            DecodeError::VersionMismatch { found, expected } => write!(
                f,
                "BIR version mismatch: file has {found}, this trs expects {expected} \
                 (regenerate with a matching bsc)"
            ),
            DecodeError::Invalid(e) => write!(f, "invalid BIR: {e}"),
        }
    }
}

impl std::error::Error for DecodeError {}

impl Design {
    pub fn decode(bytes: &[u8]) -> Result<Design, DecodeError> {
        // deep expression trees (long fold chains) exceed ciborium's
        // default recursion limit of 128
        let design: Design =
            ciborium::de::from_reader_with_recursion_limit(bytes, 65536)
                .map_err(|e| DecodeError::Cbor(e.to_string()))?;
        if design.version != BIR_VERSION {
            return Err(DecodeError::VersionMismatch {
                found: design.version,
                expected: BIR_VERSION,
            });
        }
        verify::verify(&design).map_err(DecodeError::Invalid)?;
        Ok(design)
    }

    pub fn encode(&self) -> Vec<u8> {
        let mut out = Vec::new();
        ciborium::into_writer(self, &mut out).expect("CBOR encoding cannot fail");
        out
    }

    pub fn name(&self, id: StrId) -> &str {
        &self.strings[id as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tiny_design() -> Design {
        Design {
            version: BIR_VERSION,
            strings: vec!["mkTop".into()],
            top: 0,
            modules: vec![Module {
                name: 0,
                content_hash: [0; 32],
                clock_domains: vec![],
                resets: vec![],
                inputs: vec![],
                ifc_clocks: vec![],
                instances: vec![],
                defs: vec![],
                rules: vec![],
                methods: vec![],
                schedule: Schedule::default(),
            }],
            instance_map: vec![],
            compositions: vec![],
            foreign_funcs: vec![],
            default_clock: None,
            default_reset: None,
        }
    }

    #[test]
    fn roundtrip() {
        let d = tiny_design();
        let bytes = d.encode();
        let d2 = Design::decode(&bytes).unwrap();
        assert_eq!(d2.name(d2.top), "mkTop");
        assert_eq!(d2.modules.len(), 1);
    }

    #[test]
    fn version_check() {
        let mut d = tiny_design();
        d.version = BIR_VERSION + 1;
        let bytes = d.encode();
        assert!(matches!(
            Design::decode(&bytes),
            Err(DecodeError::VersionMismatch { .. })
        ));
    }
}
