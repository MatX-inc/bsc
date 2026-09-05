# P0a execution environment

Observed 2026-09-05.  This checkpoint uses the topic branch
`trs/p0-hierarchical-feasibility`, started from handoff commit
`054ba3f6400bfed18c20c1552981212e8cc956bd`.

| Input | Revision |
| --- | --- |
| Compiler/legacy TRS | `a9462e0e28102f07a89b3f3c478b33c806d62a3e` in MatX-inc/bsc |
| BVI reference | `d4a72767c63e6dbdf569cac4c61aeb8afc597c04` in nanavati/bsc |
| Architecture/handoff | `054ba3f6400bfed18c20c1552981212e8cc956bd` in MatX-inc/bsc |

The tracked compiler, Bluesim, TRS crates, and exporter source subtrees
match the compiler reference.  BVI source is inspected at its separate pin,
not silently merged into this worktree.  Source checks can be reproduced
with `sh src/trs/spike/p0-hierarchy/run.sh source`.

Available and relevant to this checkpoint: Python 3.12.13 and Git 2.51.1 on
Linux x86_64.  GCC/G++ 13.3.0, GNU Make 4.3, and Node 24.19.0 are also present;
they do not supply the missing hardware-language toolchain.

Not installed/on PATH or found in the workspace: `bsc`, `bluetcl`, `trs`,
`trs-bir`, `rustc`, `cargo`, `ghc`, `cabal`, `verilator`, `iverilog`, `vvp`,
and `fst2vcd`.  No compiler build, RTL simulation, Rust build, real BVI
execution, or waveform differential test is claimed.  No replacement
compiler revision or unverified binary was substituted for the pin.

The Python models are P0a experiments, not a language choice for the P0b
interpreter.  P0b remains Rust-first.  Actual oracle execution requires a
verified build of the pinned compiler and the simulator/tool dependencies;
the raw environment report lists those missing inputs.  Missing tools leave
the relevant evidence inconclusive, not passed or outside semantic scope.
