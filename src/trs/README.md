# TRS

A Rust/LLVM simulation backend for BSC — a modern replacement for the
C++-generating Bluesim backend.  See [DESIGN.md](DESIGN.md) for the full
architecture, rationale, and phasing.

Status: a legacy simulator implementation is present; the hierarchy-first
replacement has an **unpassed P0 feasibility gate**.  Start new work with
[P0-HANDOFF.md](P0-HANDOFF.md).  Existing build/test commands below exercise
the current tree, not a completed P0 spike or proof of hierarchical scaling.

## Layout

| Crate | Purpose |
|---|---|
| `trs-ir` | BIR (Bluesim IR) schema, loader, verifier — the contract with bsc |
| `trs-kernel` | Simulation kernel: event queue, priorities, clocks, resets |
| `trs-rt` | Runtime primitives (FIFO, RegFile, …), system tasks |
| `trs-wave` | Waveform capture; VCD and FST writers |
| `trs-codegen` | LLVM lowering (feature `llvm`; needs `llvm-18-dev`, `libzstd-dev`) |
| `trs` | CLI: link planner, JIT/AOT driver, native runner |

## Building

```sh
cargo build            # everything except codegen (no LLVM required)
cargo test
cargo build -p trs-codegen --features llvm   # requires llvm-18-dev + libzstd-dev
```
