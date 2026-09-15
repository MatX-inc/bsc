//! Runtime primitives.
//!
//! Only primitives with genuinely stateful protocols live here (FIFOs,
//! RegFiles, BRAMs, synchronizers, clock/reset generators); registers and
//! wires are inlined by codegen into plain state fields (DESIGN.md §5.1).
//!
//! Semantics reference: `src/bluesim/bs_prim_mod_*.h`.  The load-bearing
//! pattern, ported here, is *in-place mutation plus a begin-of-cycle
//! snapshot*: rules execute in the static schedule order and mutate
//! primitive state immediately; methods whose BSV semantics are
//! conflict-free with enq/deq (e.g. `i_notEmpty`) report against a snapshot
//! taken before the first same-cycle mutation.

pub mod fifo;

pub use fifo::Fifo;
