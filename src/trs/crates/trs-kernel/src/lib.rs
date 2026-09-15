//! The trs simulation kernel.
//!
//! A port of the Bluesim kernel's *semantics* (`src/bluesim/kernel.cxx`,
//! `event_queue.cxx`, `priority.cxx`): a single event queue ordered by
//! `(time, packed priority)`, clocks with periodic waveforms or aperiodic
//! injected edges, and per-edge schedule callbacks.  The observable ordering
//! (event tie-breaking, `$display` interleaving across clock domains) must
//! match the C++ kernel exactly; the priority packing below is therefore
//! ported, not redesigned.

pub mod clock;
pub mod event;
pub mod priority;

pub use clock::{Clock, ClockHandle, EdgeDirection};
pub use event::{Event, EventKind, EventQueue};
pub use priority::Priority;

/// Simulation time in ticks (`tTime`).
pub type Time = u64;
