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
pub use schedule::{Composition, ModuleSchedule, SchedAlt, SchedNode, Schedule, Segment};

/// Schema version; bumped on any incompatible change.  The bsc exporter
/// writes it, `Design::decode` rejects mismatches.
pub const BIR_VERSION: u32 = 2;

/// Snapshot sidecar magic (`<base>.birsnap`, see `Design::snap_encode`).
/// The trailing byte is the HEADER format; \x02 added the layout rev
/// and the payload checksum.  (bincode over a probed rkyv variant:
/// rkyv was 2x the bytes and slower overall once integrity-checked.)
const SNAP_MAGIC: &[u8; 8] = b"TRSSNAP\x02";

/// bincode is POSITIONAL: unlike the name-keyed CBOR .bir (which
/// tolerates `#[serde(default)]` growth without a BIR_VERSION bump —
/// five such fields exist), ANY serde-visible change to the types
/// reachable from `Design` — added/reordered fields, enum variant
/// insertion — silently changes the snapshot payload layout.  Bump
/// this with every such change (the AOT twin of this rule is
/// `AOT_LAYOUT_REV` in trs-codegen); a stale rev makes readers fall
/// back to the .bir instead of misdecoding.
const SNAP_LAYOUT_REV: u32 = 3;

/// magic(8) | BIR_VERSION le32(4) | SNAP_LAYOUT_REV le32(4) |
/// bir_hash le64(8) | payload fnv1a le64(8) = 32 bytes.
const SNAP_HEADER: usize = 32;

/// FNV-1a: the project-wide fingerprint (AOT artifacts fingerprint
/// their source .bir with it; snapshots checksum their payload).
pub fn fnv1a(bytes: &[u8]) -> u64 {
    let mut h: u64 = 0xcbf2_9ce4_8422_2325;
    for &b in bytes {
        h ^= b as u64;
        h = h.wrapping_mul(0x100_0000_01b3);
    }
    h
}

/// Identifier interned per design; display names live in `Design::strings`.
pub type StrId = u32;

thread_local! {
    /// Snap ENCODE side-blob: while `Some`, `Lazy` fields serialize as
    /// (offset, len) into this accumulator instead of inline.  Set only
    /// by `snap_encode`; the CBOR .bir path never sets it, so the .bir
    /// wire format is unchanged.
    static SNAP_SIDE: std::cell::RefCell<Option<Vec<u8>>> =
        const { std::cell::RefCell::new(None) };
    /// Snap DECODE side-blob: while `Some`, `Lazy` fields deserialize
    /// as (offset, len) referencing this blob and stay PENDING until
    /// first touch.  Set only by `snap_decode`.
    static SNAP_BLOB: std::cell::RefCell<Option<std::sync::Arc<Vec<u8>>>> =
        const { std::cell::RefCell::new(None) };
}

/// Reset the thread-local snap contexts on scope exit (panic-safe).
struct SnapCtxGuard;
impl Drop for SnapCtxGuard {
    fn drop(&mut self) {
        SNAP_SIDE.with(|s| *s.borrow_mut() = None);
        SNAP_BLOB.with(|s| *s.borrow_mut() = None);
    }
}

/// A design subtree that decodes on first touch when loaded from a
/// snap (expression trees are fallback/debug-side on a full-AOT run —
/// eagerly decoding them was most of the snap's load cost).  From the
/// name-keyed CBOR .bir it decodes eagerly and transparently — the
/// .bir wire format does not know this type exists.  `Deref` forces:
/// consumers write `&*def.expr` where they wrote `&def.expr`.
pub struct Lazy<T> {
    cell: std::sync::OnceLock<T>,
    /// (side-blob, offset, len) while un-forced from a snap
    pending: Option<(std::sync::Arc<Vec<u8>>, u32, u32)>,
}

impl<T> Lazy<T> {
    pub fn new(v: T) -> Self {
        Lazy { cell: std::sync::OnceLock::from(v), pending: None }
    }
}

impl<T: serde::de::DeserializeOwned> std::ops::Deref for Lazy<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.cell.get_or_init(|| {
            let (blob, off, len) =
                self.pending.as_ref().expect("Lazy with neither value nor blob");
            // the blob rode the same gated (and, for sidecars,
            // checksummed) snap payload as the eager half, under the
            // same layout rev; a decode failure here is the corruption
            // class the gates exist to exclude
            bincode::deserialize(&blob[*off as usize..(*off + *len) as usize])
                .expect("snap lazy subtree decode (gated payload corrupt?)")
        })
    }
}

impl<T: Clone> Clone for Lazy<T> {
    fn clone(&self) -> Self {
        match self.cell.get() {
            Some(v) => Lazy::new(v.clone()),
            // un-forced: share the blob, stay pending
            None => Lazy { cell: std::sync::OnceLock::new(), pending: self.pending.clone() },
        }
    }
}

impl<T: std::fmt::Debug + serde::de::DeserializeOwned> std::fmt::Debug for Lazy<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: Serialize + serde::de::DeserializeOwned> Serialize for Lazy<T> {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        let diverted = SNAP_SIDE.with(|side| {
            let mut side = side.borrow_mut();
            match &mut *side {
                Some(blob) => {
                    let off = blob.len() as u32;
                    bincode::serialize_into(&mut *blob, &**self)
                        .map_err(|e| e.to_string())?;
                    let len = blob.len() as u32 - off;
                    Ok::<Option<(u32, u32)>, String>(Some((off, len)))
                }
                None => Ok(None),
            }
        });
        match diverted {
            Ok(Some(pair)) => pair.serialize(s),
            Ok(None) => (**self).serialize(s),
            Err(e) => Err(serde::ser::Error::custom(e)),
        }
    }
}

impl<'de, T: serde::de::DeserializeOwned> Deserialize<'de> for Lazy<T> {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let blob = SNAP_BLOB.with(|b| b.borrow().clone());
        match blob {
            Some(blob) => {
                let (off, len) = <(u32, u32)>::deserialize(d)?;
                // bounds must fail the LOAD, not a later force
                if off as usize + len as usize > blob.len() {
                    return Err(serde::de::Error::custom(
                        "snap lazy reference out of blob bounds",
                    ));
                }
                Ok(Lazy {
                    cell: std::sync::OnceLock::new(),
                    pending: Some((blob, off, len)),
                })
            }
            None => Ok(Lazy::new(T::deserialize(d)?)),
        }
    }
}

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
    /// bsc was invoked with -keep-fires: CAN_FIRE/WILL_FIRE defs and
    /// method ports are never demoted to stack locals, so they all get
    /// VCD variables (SimCOpt shouldMove's cfwfOkToMove/portOkToMove).
    #[serde(default)]
    pub keep_fires: bool,
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
    /// Interface output clock GATES, keyed by the clock's interface
    /// method name (what `Expr::Gate` references): a parent rule calling
    /// a method clocked by a child's gated clock reads the gate through
    /// this (Bug 1677 lifts the gate into the rule condition).
    #[serde(default)]
    pub ifc_clock_gates: Vec<(StrId, Expr)>,
    /// Interface output resets: external port name -> the internal reset
    /// wire being re-exported (parents refer to it as "<inst>$<port>").
    #[serde(default)]
    pub ifc_resets: Vec<(StrId, StrId)>,
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
    pub expr: Lazy<Expr>,
    pub props: DefProps,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct DefProps {
    pub can_fire: bool,
    pub will_fire: bool,
    /// Signed display preference (from removed sign casts).
    pub signed: bool,
    /// Survives as a C++ member in the reference (post-SimCOpt
    /// public defs): the debug-tier symbol set (bk symbol tree).
    /// Absent in pre-flag BIRs -> false (no def symbols).
    #[serde(default)]
    pub sym: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Rule {
    pub name: StrId,
    /// Reference to the CAN_FIRE def for this rule.
    pub can_fire: StrId,
    /// Reference to the WILL_FIRE def for this rule.
    pub will_fire: StrId,
    pub body: Lazy<Vec<Stmt>>,
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
    /// (* always_enabled *): bsc drops the caller-side RDY condition, so
    /// the method body must check its own RDY at runtime (the C++
    /// backend's cvtIFace check_rdy wrapper).
    #[serde(default)]
    pub always_enabled: bool,
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
    /// Decoded-design snapshot sidecar (`<base>.birsnap`): the
    /// `SNAP_HEADER` fields, then a bincode image of the decoded
    /// Design.  It is a CACHE, never a source of truth — `snap_decode`
    /// gates on EVERY header field before touching the payload, and
    /// callers fall back to `Design::decode` of the .bir on any
    /// mismatch.  Startup skips the CBOR parse when the gates hold.
    /// NOTE: runs on the caller's thread — spawning even a short-lived
    /// helper thread permanently drops glibc malloc's single-threaded
    /// fast path (measured ~50% on interp-heavy runs).  Recursion depth
    /// matches what `Design::decode` already does on this stack.
    pub fn snap_encode(&self, bir_hash: u64) -> Result<Vec<u8>, String> {
        // two sections: Lazy subtrees divert into a side blob (see
        // Lazy) so the load can defer them; blob FIRST so the decoder
        // has it in hand before the design section references it
        let _g = SnapCtxGuard;
        SNAP_SIDE.with(|s| *s.borrow_mut() = Some(Vec::new()));
        let mut design = Vec::new();
        bincode::serialize_into(&mut design, self).map_err(|e| e.to_string())?;
        let blob = SNAP_SIDE
            .with(|s| s.borrow_mut().take())
            .expect("snap side-blob vanished mid-encode");
        drop(_g);
        let mut out = vec![0u8; SNAP_HEADER];
        out.extend_from_slice(&(blob.len() as u64).to_le_bytes());
        out.extend_from_slice(&blob);
        out.extend_from_slice(&design);
        let sum = fnv1a(&out[SNAP_HEADER..]);
        out[..8].copy_from_slice(SNAP_MAGIC);
        out[8..12].copy_from_slice(&BIR_VERSION.to_le_bytes());
        out[12..16].copy_from_slice(&SNAP_LAYOUT_REV.to_le_bytes());
        out[16..24].copy_from_slice(&bir_hash.to_le_bytes());
        out[24..32].copy_from_slice(&sum.to_le_bytes());
        Ok(out)
    }

    /// Header-gated parse: `None` (= fall back to the .bir) unless
    /// EVERY gate passes, all checked BEFORE the payload deserialize:
    /// magic (embeds the header format), BIR_VERSION, SNAP_LAYOUT_REV
    /// (bincode is positional — see the const), the expected .bir
    /// fingerprint, and the payload checksum (fs::write is not atomic,
    /// and the fingerprint covers the .bir, not this payload — a
    /// corrupt-but-parseable payload would otherwise load as a WRONG
    /// design, the one failure class byte parity cannot tolerate).
    /// The decoded design passes the same structural `verify` that
    /// guards `Design::decode`, so residual misdecode degrades to the
    /// fallback, never a panic.
    pub fn snap_decode(bytes: &[u8], bir_hash: u64) -> Option<Design> {
        Self::snap_decode_inner(bytes, bir_hash, true)
    }

    /// `snap_decode` for a snap EMBEDDED in an artifact .so: the
    /// checksum gate exists to catch torn sidecar writes (fs::write is
    /// not atomic), but an embedded snap has exactly the integrity of
    /// the artifact it rides in — whose compiled code we execute
    /// without a checksum — and artifacts are written temp+rename.
    /// Skipping the byte-serial fnv pass saves ~25% of the decode
    /// (3.7ms on an 11MB FloatTest snap).  All other gates still hold.
    pub fn snap_decode_embedded(bytes: &[u8], bir_hash: u64) -> Option<Design> {
        Self::snap_decode_inner(bytes, bir_hash, false)
    }

    fn snap_decode_inner(
        bytes: &[u8],
        bir_hash: u64,
        checksum: bool,
    ) -> Option<Design> {
        if bytes.len() < SNAP_HEADER || &bytes[..8] != SNAP_MAGIC {
            return None;
        }
        if u32::from_le_bytes(bytes[8..12].try_into().ok()?) != BIR_VERSION {
            return None;
        }
        if u32::from_le_bytes(bytes[12..16].try_into().ok()?) != SNAP_LAYOUT_REV {
            return None;
        }
        if u64::from_le_bytes(bytes[16..24].try_into().ok()?) != bir_hash {
            return None;
        }
        let payload = &bytes[SNAP_HEADER..];
        if checksum
            && u64::from_le_bytes(bytes[24..32].try_into().ok()?) != fnv1a(payload)
        {
            return None;
        }
        // section split: [blob_len u64][side blob][design]; the blob is
        // COPIED into an Arc so pending Lazy fields outlive the caller's
        // byte buffer (an mmapped artifact may be a shorter-lived view)
        let blob_len =
            u64::from_le_bytes(payload.get(..8)?.try_into().ok()?) as usize;
        let blob = payload.get(8..8 + blob_len)?;
        let design = payload.get(8 + blob_len..)?;
        let _g = SnapCtxGuard;
        SNAP_BLOB
            .with(|b| *b.borrow_mut() = Some(std::sync::Arc::new(blob.to_vec())));
        // caller's thread on purpose — see snap_encode's NOTE
        let d: Design = bincode::deserialize(design).ok()?;
        drop(_g);
        verify::verify(&d).ok()?;
        Some(d)
    }

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
                ifc_clock_gates: vec![],
                ifc_resets: vec![],
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
            keep_fires: false,
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
