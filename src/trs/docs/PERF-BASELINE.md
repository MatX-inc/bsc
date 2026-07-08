# Performance baseline: trs (interpreter) vs reference Bluesim

Measured 2026-07-08 on the development container (16 GB, shared load —
link times are min-of-3; treat as ~±20%).  Reference = installed bsc's
C++ Bluesim backend (`-sim`); trs = `-trs` (CBOR BIR export) run by
the P1 reference interpreter (release build).  All outputs byte-match.

## Compile and link

The `.bsv -> .ba` phase is shared between backends (identical cost).
The backend difference is the link phase: C++ codegen + g++ + ld
versus BIR export + wrapper.

| design                       | .bsv->.ba | link -sim (cold) | link -trs | speedup |
|------------------------------|-----------|------------------|------------|---------|
| sysVcdGT (toy, 5 prims)      | 0.57 s    | 0.61 s           | 0.030 s    | ~20x    |
| dft64 sysTb_v1 (PAClib FFT)  | 4.9 s     | 8.8 s            | 0.70 s     | ~13x    |
| sudoku mkGenerateTest3       | 35.7 s    | 14.7 s           | 1.31 s     | ~11x    |

Notes:
- bsc's per-object cache makes warm -sim relinks much cheaper (dft64:
  1.1 s), but -trs is still faster with no cache at all; BIR-side
  caching (content_hash) is designed but not implemented.
- For big designs the shared front-end dominates total build time, so
  end-to-end "edit -> run" improves by the link delta (sudoku: 50.4 s
  -> 37.0 s cold).

## Simulation throughput

| design                                | reference        | trs (interp)   | ratio    |
|---------------------------------------|------------------|------------------|----------|
| sysLongCnt (3 rules, 5M cycles)       | 0.16 s (~30 Mcps)| 54.9 s (~91 Kcps)| ~335x    |
| sudoku mkGenerateTest3 (SAT-ish cones)| 0.36 s           | >600 s (killed)  | >1600x   |
| process startup (empty run)           | ~60 ms (bluetcl) | ~4 ms            | 15x (win)|

The interpreter walks the expression tree per evaluation and clones
`Value`s; large combinational cones (sudoku's solver) are the worst
case.  This is the expected P1 posture -- the interpreter is the
semantic oracle, not the product.  Closing the gap is exactly P2: JIT
the BIR to LLVM (rule bodies and CF/WF cones as native code, prims as
calls into trs-rt), differential-tested against the interpreter.
Target: within ~2x of the C++ backend initially, then faster than
Verilator per the DESIGN.md goals.
