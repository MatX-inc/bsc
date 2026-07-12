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

| N  | tiles | bsc frontend | ref build | trs link | ref run | trs run |
|----|-------|--------------|-----------|---------|---------|--------|
| 2  | 4     | 0.96 s       | 2.65 s    | 0.12 s  | 0.101 s | 0.034 s |
| 4  | 16    | 1.11 s       | 2.98 s    | 0.42 s  | 0.110 s | 0.036 s |
| 8  | 64    | 2.00 s       | 4.84 s    | 1.99 s  | 0.116 s | 0.059 s |
| 16 | 256   | 7.5 s        | 12.0 s    | 8.3 s   | 0.180 s | 0.118 s |
| 32 | 1024  | 71.1 s       | 59.8 s    | 36.5 s  | 0.191 s | 0.298 s |

(trs link phase split at N=32: ir-passes 29.7 s, backend 5.9 s;
trs RSS 68 MB vs ref 37 MB.)

VERDICTS:
1. The true scale wall is BSC'S FRONTEND: 71 s at 1024 tiles,
   ~9.4x growth per 4x tiles — elaboration, upstream of any backend.
2. NO spine explosion: trs link grows ~4.4x per 4x tiles (ref 5.0x),
   still 1.6x ahead at N=32.  The outline cost model + call-based
   spine keep LLVM near-linear; loop-rolled spine is a want, not an
   emergency.
3. WE LOSE THE RUN AT N=32 (0.298 vs 0.191 s) after winning every
   smaller N; reference run is startup-flat while ours grew 2.5x
   from N=16 and RSS doubled.  Prime suspect: O(instances) startup
   (plan walk / per-instance analysis) — TYPE-KEYED ANALYSIS is
   hereby promoted from link-time nicety to the run-time scaling
   fix.  Startup decomposition (-m 1) at N=32 pending a quiet slot.

## Grid v2 amendments (rich tiles, packed link rules; binary bceb3f30)

v2 ladder (see results.csv gen=v2): byte-identical at every N; at
N=32 trs link 45.7s vs ref build 129s (2.8x ahead).

CONTROLLED EXPERIMENT — Ravi's O(rules^2) packing hypothesis, same
v2 tiles, only link packing varied, back-to-back on one machine:
1024 link rules 151.9s frontend vs 4 packed rules 142.4s (~6%,
within the box's ±30% bsc run-to-run wobble).  REFUTED for this
shape: the benchmark's link rules are PAIRWISE DISJOINT, and bsc
disposes of disjoint pairs cheaply — the quadratic bites when rule
pairs share state.  The packing stays (right shape for interacting
rules); the v1->v2 frontend growth is tile RICHNESS, and the true
attribution inside bsc (elaboration vs top-level compose vs .ba
emission) needs bsc phase timing (no public flag; upstream
question).

STARTUP DECOMPOSITION, N=32 quiet (-m 1 vs full):
  trs  startup 0.18-0.19s, sim ~0.04s
  ref startup 0.08s,      sim ~0.10s
Our SIMULATION is 2.5x FASTER than reference at 1024 tiles; the
entire run-time deficit is O(instances) STARTUP (plan walk +
analysis + load).  TYPE-KEYED ANALYSIS is confirmed as the scale
fix — promoted to the first rung of the scale arc.

## Grid v3: PROGRAM tiles + ActionValue drains (binary 19110cdc)

v3 tiles each run a small program (12-entry case-ROM {op,rs,rd},
PC, 8-entry full-range RegFile, opcode case-dispatch, conditional
writeback/send) and the packed link rules drain tiles through an
ActionValue oTake bound inside the conditional arm — the arm-def +
AvAction-inline classes (21bacd87..19110cdc) in the hot path.
Byte-identical at every N (results.csv gen=v3):

| N  | tiles | bsc frontend | ref build | trs link | ref run | trs run |
|----|-------|--------------|-----------|---------|---------|--------|
| 2  | 4     | 0.99 s       | 2.85 s    | 0.21 s  | 0.101 s | 0.036 s |
| 4  | 16    | 1.15 s       | 3.91 s    | 0.78 s  | 0.097 s | 0.036 s |
| 8  | 64    | 2.21 s       | 5.23 s    | 6.97 s  | 0.274 s | 0.111 s |
| 16 | 256   | 47.5 s       | 71.6 s    | 91.0 s  | 0.343 s | 0.320 s |
| 32 | 1024  | 511.7 s      | 150.1 s   | 202.2 s | 0.157 s | 0.387 s |

(trs link split at N=32: ir-passes 166.0 s, backend 34.3 s.)

VERDICTS (they invert v2's happy link story):
1. bsc frontend is still everyone's wall — 8.5 MINUTES at N=32,
   2.5x our link and 3.4x the reference's whole C++ build.
2. Tile richness FLIPS our link advantage: 7.0 vs 5.2 s already at
   N=8, 202 vs 150 s at N=32 (1.35x behind; v2 was 2.8x AHEAD).
   The mega-edge inlines every instance's sections, so LLVM input
   is O(instances x body mass); Bluesim's O(instances) part is a
   thin call sequence into per-TYPE class methods.  The outline
   cost model is REPLICATION-BLIND (body_mass > max(800, 2 x
   shared_mass) has no term for how many times the module type
   repeats) — the fix is a replication-aware dial (amortize
   outline cost by the type's instance count), then the
   loop-rolled spine (one loop over stride-regular instance
   regions, intra-tile fusion preserved).
3. Run at N=32: 0.387 vs 0.157 s (2.5x behind).  Same O(instances)
   startup attribution as v2 (sim-only was 2.5x FASTER there);
   type-keyed analysis remains the scale-arc rung 1.  Note the
   reference run DROPPED from N=16's 0.343 s — Bluesim's per-type
   compilation keeps its startup flat while ours grows with
   instances.

## Grid v3 + replication-aware outline dial (binary 99f167a9)

The outline floor now amortizes over the module type's replication
count in the composition (OUTLINE_FLOOR / k; k=1 designs keep every
prior decision bit-for-bit).  Same v3 artifacts, byte-identical at
every N (results.csv gen=v3d; frontend/ref columns carried from the
v3 rows — same builds):

| N  | trs link (was) | trs link | trs run (was) | trs run | vs ref build | vs ref run |
|----|---------------|---------|--------------|--------|--------------|------------|
| 2  | 0.21 s        | 0.14 s  | 0.036 s      | 0.030  | 20x ahead    | 3.4x ahead |
| 4  | 0.78 s        | 0.30 s  | 0.036 s      | 0.038  | 13x ahead    | 2.6x ahead |
| 8  | 6.97 s        | 0.94 s  | 0.111 s      | 0.042  | 5.6x ahead   | 6.5x ahead |
| 16 | 91.0 s        | 5.11 s  | 0.320 s      | 0.079  | 14x ahead    | 4.3x ahead |
| 32 | 202.2 s       | 27.4 s  | 0.387 s      | 0.262  | 5.5x ahead   | 1.7x BEHIND|

(link split at N=32: ir-passes 15.9 s, backend 10.0 s — down from
166/34.)  The link story is fully restored (ahead at every N) and
the mega-edge unroll turns out to have been a RUN-time cost too
(N=16 run 0.320 -> 0.079: one per-type body stays hot in I-cache
across 256 instances).  Remaining N=32 deficits: run is O(instances)
STARTUP (type-keyed analysis, scale-arc rung 1) and the residual
link is 1024 sched sections + call sites in the mega-edge
(loop-rolled spine).  Sealing sweep on 99f167a9: 975 PASS / 0 DIFF
(new high — the 420s ref-build ceiling also recovered two designs
misfiled as permanent LINK_FAILs); fence rebaselined (925 designs
above the timing floors).

# Startup attack: snapshot sidecar (2026-07-10)

MEASURED FIRST (new TRS_STARTUP_TIME laps, grid v3 N=32, quiet box):
the doc's standing "plan walk / per-instance analysis" attribution was
WRONG at this scale — the plan phases total ~4 ms.  The real cost was
the 4.8 MB BIR CBOR decode (80.3 ms of the 138 ms total).

| phase (final v2-header binary) | before   | after    |
|--------------------------------|----------|----------|
| BIR decode (+fps, +integrity)  | 88.1 ms  | 36.1 ms (read + .bir fnv + snap gates+fnv+decode) |
| instantiate (instance tree)    | 11.5 ms  | 12.9 ms  |
| LazyJit build (Design clone)   |  8.8 ms  | 0.03 ms (no clone on Load) |
| prime cones + all plan passes  |  ~7 ms   | ~5.6 ms  |
| process init (incl. libLLVM)   | ~20 ms   | ~20 ms   |
| **total (-m 1, min of 5)**     | **0.138 s** | **0.08-0.09 s** |

Reference startup 0.08 s (same box/method): trs lands AT reference at
1024 tiles (was 55% above).  The v2 integrity header (fnv1a over the
3.49 MB payload + the pre-decode gates) costs ~4 ms of the 36.1 —
the win survives it.  Full N=32 run (startup+sim), re-measured at the
final binary: trs 0.115 s vs reference 0.143 s (grid CSV; parity
PASS) — the v3d table's last BEHIND cell stays flipped to ahead.

The shipped change: `<base>.birsnap` decoded-design snapshot, written
by `trs link` next to the .bir.  Header v2 (32 bytes): magic (embeds
the header format) | BIR_VERSION | SNAP_LAYOUT_REV | .bir fingerprint
| payload fnv1a — every gate checked BEFORE the payload deserialize,
and the decoded design passes the same structural verify() as
Design::decode.  A cache, never a source of truth: any mismatch falls
back to the CBOR decode.  `trs link` itself loads via
load_file_fresh (snapshot-blind) — the WRITER always decodes the .bir
source of truth, so a gate-passing-but-wrong sidecar can never be
laundered into a fresh artifact.  LazyJit stashes a design clone only
when cold compilation is possible (None on the pure-Load path).

## Review fleet + the integrity header (2026-07-10, same day)

A 48-agent review fleet (6 lenses, 3 adversarial verifiers per
finding) confirmed two real defects in the first-cut 20-byte header,
both fixed by header v2:

1. NO LAYOUT REVISION: bincode is positional, so any serde-visible
   trs-ir change (five `#[serde(default)]` fields were added
   historically without a BIR_VERSION bump — that is the point of the
   name-keyed CBOR pattern) silently changes the snapshot layout; a
   stale snap under a newer binary MISDECODES instead of falling
   back.  SNAP_LAYOUT_REV now mirrors the AOT_LAYOUT_REV discipline
   (that gate was already at rev 6 — this staleness class is routine).
   BUMP IT with every serde-visible trs-ir change.
2. NO PAYLOAD INTEGRITY: the fingerprint covers the .bir, not the
   payload; a verifier's harness showed a SINGLE flipped payload byte
   decoding "successfully" into a wrong design ("mkTop" -> "okTop") —
   the one failure class byte parity cannot tolerate.  The payload
   fnv1a closes it (and trailing-garbage acceptance with it).

Probed on the final binary, all byte-identical fallbacks: missing,
corrupt payload byte, truncated, garbage, old-magic v1, wrong layout
rev, wrong bir hash, header-only, trailing garbage, restored-good
(loads via snapshot, verified by lap).  Poison-at-link probe: a
forged sidecar (other design's valid-checksum payload under the
target's bir_hash) is IGNORED by link, the artifact simulates the
.bir, and the sidecar is rewritten byte-identical to the honest
snapshot (snap_encode is deterministic).

## The two dead ends (measured, reverted)

1. rkyv "zero-copy" payload: 2x the bytes (6.7 vs 3.5 MB), and with
   the integrity checksum unsafe access requires, the load phase cost
   43 ms vs bincode's ~20.  True zero-copy needs the interp to consume
   archived types — parked as not worth the refactor for ~15 ms.
2. Fingerprint/snapshot overlap on a helper thread: saved 12 ms of
   startup and cost interp-fallback designs ~50% OF THEIR WHOLE RUN
   (dft64 22s -> 33-53s; the fence caught it).  One short-lived thread
   permanently drops glibc malloc's single-threaded fast path, and the
   interp's Value-clone eval loop lives on malloc.  DOCTRINE: no
   threads on the run startup path (startup.rs header).  The chase
   also falsified Arc<Design>-indirection and code-layout/CGU theories
   (codegen-units=1 probed and reverted) — the discriminating
   instrument was a frozen pre-change binary, 5 runs, idle box.

Remaining startup headroom (queued): process init ~20 ms (libLLVM
linkage on the run path), instantiate ~12 ms (path-String interning),
snapshot decode ~20 ms (single-thread streaming format).  TYPE-KEYED
ANALYSIS stays queued for the scale arc (loop-rolled spine wants its
stride-regular regions) but is no longer the startup fix — at N=32 it
buys ~3 ms.
