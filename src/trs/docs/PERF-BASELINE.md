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

# Pre-edge-SSA baseline (2026-07-09, quiet machine)

Frozen BEFORE the edge-SSA transformation (task #24) so its wins are
measured against solid numbers.  Binary: decc231e + leash-fairness
tree (all g2 fixes; gate all-green 966/0/0 x 3 legs).  Reference =
installed bsc C++ Bluesim at -O3.  All runs byte-identical, x3.

## Wall clock (sudoku mkGenerateTest3, -m default full run)

| metric              | trs artifact (O1) | reference | ratio  |
|---------------------|---------------------|-----------|--------|
| run                 | 0.48-0.51 s         | 0.31 s    | ~1.55x |
| startup (-m 1)      | 0.06 s              | 0.08 s    | win    |
| link                | 8.1 s (one-module)  | 13.9 s    | win    |
| LongCnt 5M (artifact, central loop) | 0.05-0.06 s | 0.27 s | 5x AHEAD |

## Where trs's 0.51s goes (TRS_PROF)

total 0.556s | dispatch 0.000s (fused edges: schedule IS code) |
ticks 0.137s (~25% — prim tick machinery, UNTOUCHED by edge-SSA;
also the central-loop #9 disqualifier) | prim cb 0 calls |
foreign cb 768 calls, 0.001s.  Remainder ~0.32s = compiled edge code
vs reference sim-only ~0.25s.

## Hardware counters (perf stat, P-core, full run)

| counter             | trs     | reference | ratio |
|---------------------|-----------|-----------|-------|
| cycles              | 1.060 B   | 0.667 B   | 1.59x |
| instructions        | 2.775 B   | 1.747 B   | 1.59x |
| IPC                 | 2.62      | 2.62      | SAME  |
| branches            | 514 M     | 317 M     | 1.62x |
| branch-miss rate    | 0.43%     | 0.62%     | win   |
| L1-dcache loads     | 1.128 B   | 0.435 B   | 2.59x |
| loads / instruction | 0.41      | 0.25      | —     |
| L1d load misses     | 17.2 M    | 1.15 M    | 15x   |
| LLC load misses     | 77 k      | 65 k      | ~same |

READING: IPC is IDENTICAL — the gap is pure instruction/load VOLUME,
not stalls.  Excess = 1.03B instructions, of which 0.69B (67%) are
data loads: the arena-slot round-trip signature.  Edge-SSA attacks
exactly this (values in registers, cross-rule sharing); the share
census sizes the recompute side at ~45% of per-edge def-eval mass
(recompute 16.6k vs total DAG 20.3k; hottest module type mir=2:
recompute 12.3k EXCEEDS its own DAG 10.5k).  The 15x L1d miss count
is the slot-scatter side effect (absorbed at same IPC today, but
free to remove).  Ticks (0.137s) are the post-edge-SSA residue to
attack next.  O-ladder: PARKED until full-edge composition (O3 today
buys only ~0.05s for +3s link on pre-SSA IR).

# Post-edge-SSA (2026-07-09, task #24 complete)

Binary 1b323b3d (edge-SSA + symbol elision + outline cost model +
reset-tick skip), opt-in TRS_EDGE_SSA=1.  Eight perfect sweep legs
this session; byte-identical everywhere.

## Sudoku vs the frozen pre-edge-SSA baseline

| metric            | baseline | edge-SSA+model | reference |
|-------------------|----------|----------------|-----------|
| run (O1)          | 0.48 s   | 0.36-0.44 s    | 0.31 s    |
| run (O3)          | —        | 0.32-0.34 s    | 0.29-0.32 |
| link (O1)         | 8.1 s    | 9.3 s          | 13.9 s    |
| link (O3)         | —        | 10.7 s         | (they ARE -O3) |
| instructions      | 2.78 B (1.59x) | 2.10 B (1.28x) | 1.65 B |
| L1d loads         | 1.13 B (2.59x) | 0.82 B (2.0x)  | 0.41 B |
| L1d load misses   | 17.2 M   | 2.6 M          | 1.1 M     |

O-LADDER VERDICT: pre-edge-SSA O3 bought ~0.05s (nothing for the
middle-end to see through per-body call boundaries); post-edge-SSA
O3 buys ~0.09s and lands AT REFERENCE PARITY (0.32-0.34 vs
0.29-0.32) while linking 25% faster than the reference's -O3.  The
outline cost model (outline iff body_mass > max(800, 2 x consumed-
sharable-mass)) is what makes O3 affordable: monsters (shared/mass
~0.28) leave the mega-function — runtime-POSITIVE (L1-miss count
6.5x down) — while the sharing band (~1.0) stays inline.

Remaining interp-side residual: ~0.09s of real ticks (wire valid-bit
clears + __me_check R0001 checkers), untouched by opt level and also
the central-loop #9 blocker — compiling them into the edge fn is the
projected below-reference crossing.

# Grid benchmark: replicated-design scaling (2026-07-09, N capped at 32)

bench/grid: N x N always-fire tile ring, ONE synthesized module type
(pure replication), byte-identical at every point.  Binary 4693cd04
(edge-SSA defaults + outline model + RegFile inline).

| N  | tiles | bsc frontend | ref build | b3 link | ref run | b3 run |
|----|-------|--------------|-----------|---------|---------|--------|
| 2  | 4     | 0.96 s       | 2.65 s    | 0.12 s  | 0.101 s | 0.034 s |
| 4  | 16    | 1.11 s       | 2.98 s    | 0.42 s  | 0.110 s | 0.036 s |
| 8  | 64    | 2.00 s       | 4.84 s    | 1.99 s  | 0.116 s | 0.059 s |
| 16 | 256   | 7.5 s        | 12.0 s    | 8.3 s   | 0.180 s | 0.118 s |
| 32 | 1024  | 71.1 s       | 59.8 s    | 36.5 s  | 0.191 s | 0.298 s |

(b3 link phase split at N=32: ir-passes 29.7 s, backend 5.9 s;
b3 RSS 68 MB vs ref 37 MB.)

VERDICTS:
1. The true scale wall is BSC'S FRONTEND: 71 s at 1024 tiles,
   ~9.4x growth per 4x tiles — elaboration, upstream of any backend.
2. NO spine explosion: b3 link grows ~4.4x per 4x tiles (ref 5.0x),
   still 1.6x ahead at N=32.  The outline cost model + call-based
   spine keep LLVM near-linear; loop-rolled spine is a want, not an
   emergency.
3. WE LOSE THE RUN AT N=32 (0.298 vs 0.191 s) after winning every
   smaller N; reference run is startup-flat while ours grew 2.5x
   from N=16 and RSS doubled.  Prime suspect: O(instances) startup
   (plan walk / per-instance analysis) — TYPE-KEYED ANALYSIS is
   hereby promoted from link-time nicety to the run-time scaling
   fix.  Startup decomposition (-m 1) at N=32 pending a quiet slot.
