//! LLVM lowering of BIR (DESIGN.md §5).
//!
//! Per module: a state struct type (registers and wires as plain fields),
//! functions for rule bodies and methods, and per-clock-domain *segments*
//! assembled by the link planner into the per-edge schedule functions.
//! Backends: ORC JIT (dev loop) and AOT object emission with a
//! content-addressed cache (§6).
//!
//! Everything LLVM-touching is behind the `llvm` feature so the rest of the
//! workspace builds without llvm-18-dev installed.

pub mod abi;
#[cfg(feature = "llvm")]
pub mod lower;

/// Optimization effort, surfaced to users as `-sim-opt 0..3` (DESIGN.md §6).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptLevel {
    /// Straight lowering, no LLVM passes; fastest build (JIT default).
    O0,
    O1,
    /// AOT default.
    O2,
    O3,
}
