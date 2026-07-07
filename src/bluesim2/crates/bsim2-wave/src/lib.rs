//! Waveform capture and output.
//!
//! One capture pipeline feeds two writers (VCD, FST) behind the
//! `WaveWriter` trait.  Capture is *push-based*: codegen emits
//! compare-and-append at state commit points, so there is no backing copy
//! of the model and no full-hierarchy walk per timeslice (DESIGN.md §8) —
//! the two costs of the current C++ scheme (`bs_vcd.h`, `vcd.cxx`).
//!
//! Time correction: values computed at a clock edge are recorded against
//! the *previous* edge time of their driving clock (`combinational_at`),
//! matching today's observable VCD timing (`vcd.cxx:15-34`).

pub mod hierarchy;
pub mod vcd_id;

pub use hierarchy::{Hierarchy, Scope, SignalDef, SignalId, VarKind};

use bsim2_kernel::Time;

/// A captured value change, buffered until its timeslice settles.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Change {
    pub at: Time,
    pub signal: SignalId,
    /// Little-endian 32-bit limbs, width from the signal definition.
    pub limbs: Vec<u32>,
}

/// Sink for settled changes; implemented by the VCD and FST writers.
pub trait WaveWriter {
    /// Emit the header from the design hierarchy (scopes carry both the
    /// instance name and the defining BSV module name).
    fn write_header(&mut self, hierarchy: &Hierarchy) -> std::io::Result<()>;
    /// Changes arrive grouped by settled time, ascending.
    fn write_changes(&mut self, at: Time, changes: &[Change]) -> std::io::Result<()>;
    /// All signals to X / unknown ($dumpoff analogue).
    fn write_all_unknown(&mut self, at: Time) -> std::io::Result<()>;
    fn flush(&mut self) -> std::io::Result<()>;
}
