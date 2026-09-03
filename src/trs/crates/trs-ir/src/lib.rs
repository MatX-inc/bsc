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
pub mod link;
pub mod merge;
mod psq;
pub mod schedule;
pub mod fold;
pub mod verify;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

pub use expr::{Action, Expr, PrimOp, Stmt};
pub use schedule::{
    Composition, ModuleSchedule, QualRule, SchedAlt, SchedNode, Schedule, Segment,
};

/// Schema version; bumped on any incompatible change.  The bsc exporter
/// writes it, `Design::decode` rejects mismatches.
pub const BIR_VERSION: u32 = 11;

/// magic(8) | BIR_VERSION le32(4) = 12 bytes, ahead of the CBOR body.
///
/// The version has to be readable without decoding the body, or it
/// cannot do its job: a schema change makes the body fail to
/// deserialize, and a reader that learns the version from inside the
/// body reports that failure instead of the mismatch that explains it.
/// The last magic byte is the header's own format.
const BIR_MAGIC: &[u8; 8] = b"TRSBIR\0\x01";
const BIR_HEADER: usize = 12;

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
const SNAP_LAYOUT_REV: u32 = 4;

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

/// The contents of one .bir file.
///
/// A .bir holds either one synthesized module or a whole linked
/// design, and nothing else -- those are the two things anything
/// produces.  bsc writes the first (`trs-bir --single-fragment`); a
/// link writes the second, as the .bir an artifact carries beside it.
/// bsc has no whole-design format of its own: its .ba set *is* the
/// design, and everything design-level is derived by whatever walks
/// the hierarchy.  This format follows that, one reduction further
/// along.
///
/// The fields out here are the ones both bodies need.  `strings` is
/// file-wide: every `StrId` under `body` indexes it, whichever body it
/// is.  `foreign_funcs` is the same kind of thing, a table the content
/// references by position.  `uses_wave_tasks` is a fact about the
/// content as a whole, which a link takes the union of.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bir {
    pub strings: Vec<String>,
    /// Foreign (BDPI) function signatures the content calls.
    pub foreign_funcs: Vec<ForeignFunc>,
    /// Whether the content calls a wave-recording task ($dumpvars and
    /// family).  Recorded by the exporter, where rule bodies are plain
    /// data: the runtime sees them deferred behind `Lazy`, and the
    /// string table cannot distinguish a call to `$dumpvars` from a
    /// string literal equal to it.
    pub uses_wave_tasks: bool,
    pub body: BirBody,
}

/// Exactly the two things a .bir can hold.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BirBody {
    /// One synthesized module: one `(* synthesize *)`, one .ba.  What
    /// bsc writes.  It names no top and carries no schedule, because
    /// those describe a design and this is not one.
    Fragment(Module),
    /// A linked design.  What a link writes.
    Design(BirDesign),
}

/// The design body of a .bir.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BirDesign {
    pub modules: Vec<Module>,
    pub top: StrId,
    /// Per-(clock, edge) interleavings of instance segments -- the
    /// design schedule, exported hierarchically (see `schedule` module
    /// docs).
    pub compositions: Vec<Composition>,
}

/// A borrowed view of a `Design` in the file's shape, so writing one
/// out copies nothing.
#[derive(Serialize)]
struct BirOut<'a> {
    strings: &'a Vec<String>,
    foreign_funcs: &'a Vec<ForeignFunc>,
    uses_wave_tasks: bool,
    body: BirBodyOut<'a>,
}

/// Only the design case: a `Design` is never written as a fragment.
/// serde tags externally, by variant name, so this encodes exactly as
/// `BirBody::Design` does.
#[derive(Serialize)]
enum BirBodyOut<'a> {
    Design(BirDesignOut<'a>),
}

#[derive(Serialize)]
struct BirDesignOut<'a> {
    modules: &'a Vec<Module>,
    top: StrId,
    compositions: &'a Vec<Composition>,
}

/// A whole linked design: the top module, every module in its
/// hierarchy, and the schedule over them.
///
/// This is the runtime object, not a file: it is what `link::assemble`
/// produces and what everything downstream reads.  `Bir` is the file,
/// and the two are deliberately separate -- a .bir normally holds one
/// module and no design at all.  The serde derive here is for the
/// `.birsnap` sidecar, which caches this object; the .bir encoding is
/// `Bir`'s.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Design {
    /// String table; all `StrId`s index into this.
    pub strings: Vec<String>,
    /// Reverse of `strings`, so a name resolves to its id without a
    /// scan.  Read it through `str_id`.  Derived, not serialized: the
    /// decode paths build it.
    #[serde(skip)]
    str_ids: HashMap<String, StrId>,
    /// Whether the design calls a wave-recording task ($dumpvars and
    /// family).  A fact recorded by the exporter, where rule bodies are
    /// plain data: the runtime sees them deferred behind `Lazy`, and the
    /// string table cannot distinguish a call to `$dumpvars` from a
    /// string literal equal to it.
    pub uses_wave_tasks: bool,
    pub top: StrId,
    pub modules: Vec<Module>,
    /// Per-(clock, edge) interleavings of instance segments — the design
    /// schedule, exported hierarchically (see `schedule` module docs).
    pub compositions: Vec<Composition>,
    /// Foreign (BDPI) function signatures used anywhere in the design.
    pub foreign_funcs: Vec<ForeignFunc>,
    pub default_clock: Option<StrId>,
    pub default_reset: Option<StrId>,
}

/// A rule, as its position in its module's `rules` list.  A rule
/// reference never leaves the module that defines it, so a position
/// says everything a name would and cannot be resolved against the
/// wrong module by accident.
///
/// Serializes as the bare integer it wraps.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[serde(transparent)]
pub struct RuleRef(pub u32);

impl RuleRef {
    #[inline]
    pub fn idx(self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Display for RuleRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A synthesized module this fragment references across its boundary.
/// Not a package import -- those are a source-level notion -- but the
/// linker's view of what this fragment needs supplied.
///
/// It records only the name for now: any change to the referenced
/// fragment invalidates this one.  Narrowing that to an interface hash,
/// so a body-only change need not rebuild the referrer, slots in here
/// without disturbing how references are written.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Extern {
    pub module: StrId,
}

/// A position in a module's `externs` list.
///
/// Serializes as the bare integer it wraps.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[serde(transparent)]
pub struct ExternRef(pub u32);

impl ExternRef {
    #[inline]
    pub fn idx(self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Display for ExternRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A method, as its position in its module's `methods` list.
///
/// Serializes as the bare integer it wraps.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[serde(transparent)]
pub struct MethodRef(pub u32);

impl MethodRef {
    #[inline]
    pub fn idx(self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Display for MethodRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// What a schedule orders.  bsc ranks a module's rules and its interface
/// methods in one order, so a node in the schedule graph -- and an entry
/// in the Esposito conflict list -- is either.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum SchedEntity {
    Rule(RuleRef),
    Method(MethodRef),
}

impl SchedEntity {
    /// The rule this names.  A node in a *segment* is always a rule --
    /// the exporter routes interface-method nodes to the segment's cut
    /// -- so a method reaching a segment walker is a bug rather than a
    /// case to skip past.
    pub fn rule(self) -> RuleRef {
        match self {
            SchedEntity::Rule(r) => r,
            SchedEntity::Method(m) => {
                panic!("schedule segment names method {m}, not a rule")
            }
        }
    }
}

/// One synthesized module (one `.ba` / one `SimPackage`).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    pub name: StrId,
    /// The synthesized modules this fragment references.  `ExternRef`
    /// indexes it, so a cross-boundary reference names a position here
    /// rather than repeating a module name at every use.
    #[serde(default)]
    pub externs: Vec<Extern>,
    /// name -> index over `defs` and `methods`, so a reference resolves
    /// without a scan.  Read them through `def`/`def_idx`/`method_idx`.
    /// Derived, not serialized: the decode paths build them.
    #[serde(skip)]
    def_ix: HashMap<StrId, usize>,
    #[serde(skip)]
    method_ix: HashMap<StrId, usize>,
    /// Hash of the module's exported content, for the object cache.
    pub content_hash: [u8; 32],
    /// This module was built with -keep-fires: its CAN_FIRE/WILL_FIRE
    /// defs and method ports were never demoted to stack locals, so
    /// they all get VCD variables (SimCOpt shouldMove's
    /// cfwfOkToMove/portOkToMove).  Per module because the effect is:
    /// a design may keep the fire signals of some boundaries and not
    /// others, and get waveforms for just those.
    #[serde(default)]
    pub keep_fires: bool,
    /// The oscillator and reset names bsc derives for this module when
    /// it is the top of a design: its `default_clock` port's osc, and a
    /// legacy reset name that matches no port.  Both come from the
    /// module's own pragmas (`abmi_pps`), so they belong here rather
    /// than to a design -- but bsc only derives them for the module an
    /// export was rooted at, so in a whole-design .bir every other
    /// module leaves them None.  A link reads the top's.
    #[serde(default)]
    pub default_clock: Option<StrId>,
    #[serde(default)]
    pub default_reset: Option<StrId>,
    pub clock_domains: Vec<ClockDomain>,
    pub resets: Vec<Reset>,
    pub inputs: Vec<Port>,
    /// The clocks this module imports: the clock's own name, and the
    /// ports carrying its oscillator and gate.  `inputs` has the ports
    /// but not which clock they belong to, and the link needs the
    /// grouping to unify a child's domain with the parent clock wired
    /// to it (`lookupInputClockWires`).
    #[serde(default)]
    pub input_clocks: Vec<InputClock>,
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
    /// A method argument's own name, without the method that qualifies
    /// it in `name`.  The exporter records it so that reaching an
    /// argument takes no knowledge of how the port name is composed.
    pub base: Option<StrId>,
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

/// A clock a module takes in, and the ports it arrives on.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct InputClock {
    /// the clock's name in this module's interface
    pub name: StrId,
    /// the port carrying its oscillator
    pub osc: StrId,
    /// the port carrying its gate, when it is gated
    pub gate: Option<StrId>,
}

/// The clock structure of a primitive instance.
///
/// A submodule brings its own fragment, which says what domains it has
/// and which clocks it exports.  A primitive has none, so the module
/// instantiating it carries the same three facts on its behalf
/// (`getPrimDomainInfo`, `SimPrimitiveModules.hs`).  A clock
/// divider, for instance, has a domain for the clock it takes in and
/// another for the slower one it hands back.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PrimClocks {
    /// Clocks it takes in, by the port each arrives on.
    pub inputs: Vec<InputClock>,
    /// Its own clock domains, over its ports.
    pub domains: Vec<ClockDomain>,
    /// Clocks it exports: the port each leaves on, and the oscillator
    /// behind it.
    pub outputs: Vec<(StrId, Expr)>,
}

/// A clock argument of an instantiated module, as bsc's `VArgInfo`
/// describes it.  A fragment carries this rather than a reader looking
/// it up in a table of known primitives: an imported Verilog module's
/// clock and reset wiring comes from its declaration, so it is design
/// data, and the fixed-primitive assumption expires with the Verilog
/// path.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ClockArg {
    /// the clock port's name on the instantiated module
    pub name: StrId,
    /// which of `Instance::args` carries it
    pub arg: u32,
    /// whether an input reset of that module is associated with this
    /// clock (`input_resets`), which is what makes its ticks reset ticks
    pub has_reset: bool,
    /// which edges of this clock tick the instance (`TickDirection`).
    /// Read from bsc's primitive table today; carried here so the merge
    /// consults no table of known primitives, and so a declared Verilog
    /// import can say it for itself.
    pub ticks: Ticks,
}

/// The edges on which a clock ticks its instance.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub enum Ticks {
    /// the port does not tick this instance
    Never,
    Pos,
    Neg,
    Both,
}

impl Ticks {
    pub fn on_posedge(self) -> bool {
        matches!(self, Ticks::Pos | Ticks::Both)
    }

    pub fn on_negedge(self) -> bool {
        matches!(self, Ticks::Neg | Ticks::Both)
    }
}

/// A state-element or submodule instantiation (`AVInst` analogue).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Instance {
    pub name: StrId,
    pub kind: InstanceKind,
    /// The clock arguments this instance is wired with.  Empty for an
    /// instance with no clock, and for user modules, whose clocking the
    /// link reads from their own fragment.
    #[serde(default)]
    pub clock_args: Vec<ClockArg>,
    /// Where this instance sits in the order the module elaborated its
    /// instances.  The list itself is ordered for construction, which
    /// is what load-time output depends on; tick accumulation follows
    /// elaboration order instead (`di_prims`), and the two differ.
    #[serde(default)]
    pub elab_order: u32,
    /// Present for a primitive that has clock domains of its own.
    #[serde(default)]
    pub prim_clocks: Option<PrimClocks>,
    /// Instantiation arguments; constant by construction (Bluesim rejects
    /// dynamic instantiation args, `SimExpand.hs`).
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
    /// Another synthesized module, named through this fragment's
    /// `externs`.
    Module(ExternRef),
}

/// Primitives the backend knows how to lay out or call into trs-rt.
/// The full set today is `SimPrimitiveModules.hs`; this enum grows
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
    /// patch (`mkMERuleInhibits`, `SimMakeCBlocks.hs`).  Fixed
    /// per module type; cross-module pairs are in
    /// `Composition::cross_inhibits`.
    pub me_inhibits: Vec<RuleRef>,
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
    /// The sibling method carrying this one's ready signal, when the
    /// module exports one; None = constant ready.
    pub rdy: Option<StrId>,
    /// The def this method's function writes to record that it fired
    /// (cvtIFace wf_stmts).  Action and ActionValue methods only.
    pub will_fire: Option<StrId>,
    /// The def carrying this method's enable, when the module has one.
    /// The exporter names it, so nothing downstream spells the
    /// convention.
    pub en: Option<StrId>,
}

impl Module {
    /// The module a cross-boundary reference names.
    pub fn extern_module(&self, r: ExternRef) -> StrId {
        self.externs[r.idx()].module
    }

    /// Index of a def by name, or None if this module has no such def.
    pub fn def_idx(&self, name: StrId) -> Option<usize> {
        self.def_ix.get(&name).copied()
    }

    /// A def by name, or None if this module has no such def.
    pub fn def(&self, name: StrId) -> Option<&Def> {
        self.def_idx(name).map(|i| &self.defs[i])
    }

    /// Index of a method by name, or None if this module has no such
    /// method.
    pub fn method_idx(&self, name: StrId) -> Option<usize> {
        self.method_ix.get(&name).copied()
    }
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
    /// the design's fragments do not compose into a schedule
    Unschedulable(String),
    /// the files given do not make up one design
    Link(String),
}

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DecodeError::Cbor(e) => write!(f, "CBOR decode error: {e}"),
            DecodeError::Unschedulable(e) => write!(f, "cannot schedule the design: {e}"),
            DecodeError::VersionMismatch { found, expected } => write!(
                f,
                "BIR version mismatch: file has {found}, this trs expects {expected} \
                 (regenerate with a matching bsc)"
            ),
            DecodeError::Invalid(e) => write!(f, "invalid BIR: {e}"),
            DecodeError::Link(e) => write!(f, "{e}"),
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
        let mut d: Design = bincode::deserialize(design).ok()?;
        drop(_g);
        d.index_strings();
        verify::verify(&d).ok()?;
        Some(d)
    }

    /// Read one .bir and link it on its own.
    ///
    /// A whole-design file is the one-input case of a link, not a
    /// separate path: it is derived and checked exactly as a set of
    /// fragments would be.
    pub fn decode(bytes: &[u8]) -> Result<Design, DecodeError> {
        link::assemble(vec![Bir::decode(bytes)?])
    }

    /// Write this design as a .bir with a design body.
    pub fn encode(&self) -> Vec<u8> {
        let out = BirOut {
            strings: &self.strings,
            foreign_funcs: &self.foreign_funcs,
            uses_wave_tasks: self.uses_wave_tasks,
            body: BirBodyOut::Design(BirDesignOut {
                modules: &self.modules,
                top: self.top,
                compositions: &self.compositions,
            }),
        };
        let mut bytes = Vec::with_capacity(BIR_HEADER);
        bytes.extend_from_slice(BIR_MAGIC);
        bytes.extend_from_slice(&BIR_VERSION.to_le_bytes());
        ciborium::into_writer(&out, &mut bytes)
            .expect("CBOR encoding cannot fail");
        bytes
    }
}

impl Bir {
    pub fn decode(bytes: &[u8]) -> Result<Bir, DecodeError> {
        // header first, and completely: everything below assumes a body
        // this reader understands
        if bytes.len() < BIR_HEADER || &bytes[..8] != BIR_MAGIC {
            return Err(DecodeError::Cbor("not a .bir file".to_string()));
        }
        let found = u32::from_le_bytes(bytes[8..12].try_into().unwrap());
        if found != BIR_VERSION {
            return Err(DecodeError::VersionMismatch {
                found,
                expected: BIR_VERSION,
            });
        }
        // deep expression trees (long fold chains) exceed ciborium's
        // default recursion limit of 128
        ciborium::de::from_reader_with_recursion_limit(&bytes[BIR_HEADER..], 65536)
            .map_err(|e| DecodeError::Cbor(e.to_string()))
    }

}

impl Design {

    /// Everything a design needs that its file does not carry: the
    /// structural check and the merged schedule.  Shared by the
    /// whole-design decode and by `link::assemble`.
    pub(crate) fn finish(&mut self) -> Result<(), DecodeError> {
        let design = self;
        verify::verify(design).map_err(DecodeError::Invalid)?;
        // the merge reaches for a composition's instance paths and
        // clock names through the string table, so they have to be in
        // there before it runs
        merge::intern_names(design);
        // TRS_MERGE_CHECK=<file>: while the exporter still writes the
        // design schedule, it is the oracle for the merge being ported
        // here.  Appends to the named file rather than a stream the
        // tests compare, so measuring does not perturb what it
        // measures.  Scaffolding -- it goes when the export does.
        // TRS_MERGE_DUMP=<file>: what the merge computes, written out
        // so it can be frozen while the oracle above still vouches for
        // it.  That recording is the coverage that outlives the export.
        if let Some(p) = std::env::var_os("TRS_MERGE_DUMP") {
            let _ = std::fs::write(&p, merge::render(design));
        }
        if let Some(p) = std::env::var_os("TRS_MERGE_CHECK") {
            use std::io::Write;
            let lines = merge::diff(design);
            if let Ok(mut f) =
                std::fs::OpenOptions::new().create(true).append(true).open(&p)
            {
                for line in &lines {
                    let _ = writeln!(f, "{line}");
                }
                if lines.is_empty() {
                    // a design with no compositions matches whatever the
                    // merge does, including nothing: say so, or a run of
                    // such designs reads as evidence it is not
                    let n = design.compositions.len();
                    let _ = writeln!(
                        f,
                        "{}",
                        if n == 0 { "ok vacuous".to_string() } else { format!("ok {n}") }
                    );
                }
            }
        }

        // The schedule the design runs on is the one derived here, not
        // the one the exporter wrote.  Both are still in the file and
        // the check above compares them -- which is why it has to run
        // first, or it would be comparing the merge against itself.
        //
        // A design the merge cannot schedule does not fall back to the
        // exported answer.  Falling back would hide exactly the case
        // worth knowing about, and the design would be running on a
        // schedule nothing in trs derived.
        let inp = merge::Inputs::of(design);
        if inp.is_some() {
            design.compositions =
                merge::compositions(inp.as_ref().expect("just checked"))
                    .map_err(DecodeError::Unschedulable)?;
        }

        Ok(())
    }

    /// Build `str_ids` from `strings`.  Every path that produces a
    /// Design must call this — the field is derived, so neither the
    /// CBOR body nor the snapshot image carries it.
    pub(crate) fn index_strings(&mut self) {
        self.str_ids = self
            .strings
            .iter()
            .enumerate()
            .map(|(i, s)| (s.clone(), i as StrId))
            .collect();
        for m in &mut self.modules {
            m.def_ix =
                m.defs.iter().enumerate().map(|(k, d)| (d.name, k)).collect();
            m.method_ix =
                m.methods.iter().enumerate().map(|(k, x)| (x.name, k)).collect();
        }
    }

    /// The id of an interned string, or None if the design has no such
    /// string.
    pub fn str_id(&self, name: &str) -> Option<StrId> {
        self.str_ids.get(name).copied()
    }

    /// The id of a string, adding it to the table if it is new.
    pub(crate) fn intern(&mut self, s: &str) -> StrId {
        if let Some(i) = self.str_ids.get(s) {
            return *i;
        }
        let i = self.strings.len() as StrId;
        self.strings.push(s.to_string());
        self.str_ids.insert(s.to_string(), i);
        i
    }

    pub fn name(&self, id: StrId) -> &str {
        &self.strings[id as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub(crate) fn tiny_design() -> Design {
        Design {
            strings: vec!["mkTop".into()],
            str_ids: HashMap::new(),
            uses_wave_tasks: false,
            top: 0,
            modules: vec![Module {
                name: 0,
                externs: vec![],
                def_ix: HashMap::new(),
                method_ix: HashMap::new(),
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
                instances: vec![],
                defs: vec![],
                rules: vec![],
                methods: vec![],
                schedule: Schedule::default(),
            }],
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
        let d = tiny_design();
        let mut bytes = d.encode();
        bytes[8..12].copy_from_slice(&(BIR_VERSION + 1).to_le_bytes());
        assert!(matches!(
            Design::decode(&bytes),
            Err(DecodeError::VersionMismatch { .. })
        ));
    }
}
