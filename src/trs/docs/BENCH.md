# Three-simulator benchmark — Bluesim vs Verilator vs trs

`tools/bench.py`. Two axes per design:

- **build**: bsc frontend (elaboration) + backend-specific build —
  Bluesim's C++ codegen + g++, Verilator's `--cc --build`, trs's
  `.bir` export + `trs link`.  The testsuite corpus is already an
  excellent COMPILE benchmark (conflict_free_large: 417s Bluesim
  build vs ~18s trs link at last measure); runtime needs the designs
  below.
- **run**: wall time to natural `$finish` (median of `--runs`), peak
  RSS, and cycles/s where the design's cycle count is known.

## Fairness ground rules

- Every leg runs SINGLE-THREADED.
- Every leg's generated code compiles `-O3`: Bluesim ships `c++ -O3`;
  Verilator's packaged `verilated.mk` defaults `OPT_FAST`/`OPT_GLOBAL`
  to `-Os`, which measured 34% slower on the dispatch floor (mkLong),
  so the harness raises both to `-O3`; trs AOT uses its own shipped
  LLVM pipeline.  Build seconds are reported, so the compile cost of
  `-O3` stays visible.
- Verilator runs with NO timing flags (per Ravi: `--timing` has known
  issues, and `--no-timing` would silently ignore stray delays).
  Instead: bsc emits its delays behind `BSV_ASSIGNMENT_DELAY` and
  `BSV_TASKS_DELAY` (the 0-tick system-task ordering guard), both
  \`define'd empty on this leg — the guard is inert under the C++
  driver anyway (the negedge instant is a plain eval long after
  posedge NBAs settle) — and a generic C++ driver toggles CLK/RST_N
  on the bsc top, so all three legs simulate the same closed
  testbench module.  A literal-`#0;` strip survives only as a
  fallback for benchmarking bsc revisions that predate the macro.
  Anything else timing-shaped errors loudly for explicit handling.
- Designs terminate themselves (`$finish`): identical workload per
  leg, no `-m` games.
- Cross-leg stdout is byte-compared (Verilator's own `$finish`
  trailer normalized away).  `$random` designs are exempt on the
  Verilator leg only — Verilator's RNG is not glibc's, so the operand
  stream legitimately differs; Bluesim and trs must still match.
- Numbers from an uncalibrated or shared box are INDICATIVE.  The
  authoritative fence runs on the calibrated machine.

## In-repo pool (from sweep telemetry: per-cycle weight + distinct stress characters)

| design | character |
|---|---|
| Long | raw edge/rule dispatch floor (300M posedges, one rule) |
| ConflictFreeLarge | huge rule count per cycle (scheduler-bound) |
| DFT64 v1/v5 | FP dataflow pipeline, FIFO-heavy |
| FloatTest | FP arithmetic battery, wide values |
| TrafficBRAM | BRAM + MIMO buffering, memory-port bound |
| BRAM0Test | BRAM variants battery, wide state |
| Sudoku | deep combinational cones, backtracking search |
| Dividers | iterative arithmetic + $random operands |
| SparseRF | RegFile range traffic |
| Mesa | app-scale packet pipeline |

## External macro pool (clones under the session scratchpad, integration staged)

- **Flute** (bluespec/Flute, RV32/64 5-stage) and **CHERI-Toooba**
  (CTSRD-CHERI/Toooba — per Ravi it DISPLACES upstream Toooba: it is
  strictly the heavier stressor (129-bit capability datapaths
  everywhere, plus a TagController — an extra cache level for tag
  traffic — on top of RiscyOO's superscalar OOO), it is the more
  actively maintained fork (2026-07 vs 2026-04), and its
  `Tests/benchmarks` submodule is `CTSRD-CHERI/toooba-sim-benchmarks`:
  PREBUILT MiBench/Olden-class RISC-V binaries (dijkstra, qsort,
  picojpeg, rsa, basicmath, adpcm, bisort) with a Makefile — the
  in-repo real-workload runtime images every other surveyed repo
  lacks.  src_Core is only ~9% more BSV text than upstream Toooba
  (58.7k -> 63.9k lines) but the submodule libs (cheri-cap-lib,
  TagController, BlueStuff) and the widened state put the real
  simulation delta well beyond that.  Per Ravi, Piccolo is
  NOT worth a slot either: it is Flute's 3-stage sibling from the same
  lineage with a near-identical simulation profile (byte-identical
  Include_bluesim.mk); keep it only as a fallback if Flute fights the
  build.  Both share one recipe: `bsc -u -elab -sim` →
  `bsc -sim -e mkTop_HW_Side` + C_Imported_Functions.c → `Mem.hex` +
  `symbol_table.txt` in CWD (elf_to_hex, needs libelf) →
  `./exe +v1 +tohost` → PASS/FAIL + `$finish`.  In-repo workloads are
  the RISC-V ISA ELFs (short — fine for correctness and BUILD
  benchmarking; Toooba-class elaboration is a compile benchmark all
  by itself).  Core RUNTIME numbers come from the
  toooba-sim-benchmarks binaries above (upstream repos ship no
  Dhrystone/CoreMark).  CHERI-Toooba needs its submodules fetched
  (BlueStuff, TagController, cheri-cap-lib, Tests/benchmarks).
- **BlueLight** (kammoh/bluelight, LWC crypto: Ascon/Xoodyak/GIFT-COFB/
  Gimli/Subterranean) — compute-dense and modern-bsc-friendly, but it
  has NO Bluesim testbench (cocotb/Verilog only) and its `lwc` top is
  not `Empty`, so it needs a small closed BSV TB wrapper (drive
  LwcIfc with in-BSV vectors, self-check, `$finish`) before it can
  enter the pool.  Staged as follow-up.
- Survey results (2026-08-20, full ranked table in the session notes) —
  the starter set beyond the two cores, all cloned and all with
  SELF-TERMINATING in-repo Bluesim testbenches:
  - **JpegEncoder** (WangXuan95/BSV_Tutorial_cn, GPL-3.0):
    `mkTbJpegEncoderMulti` reads 12 in-repo 1816x1008 .pgm images via
    $fopen — a multi-million-cycle DSP run (2-D DCT, quantizer,
    Huffman; dense wide fixed-point) with ZERO external deps.  The
    non-CPU compute character the cores don't cover.
  - **blue-rdma** (datenlord, GPL-2.0): RoCEv2 engine — 256-bit+
    datapaths, header parse/deparse, CRC, FIFO meshes; ~30
    independent self-terminating Bluesim tops
    (`Makefile.test TESTFILE=.. TOPMODULE=..`), run length dialed via
    MAX_CMP_CNT.  Its CI pins stock B-Lang bsc — proven open-bsc flow.
  - **Fife running FreeRTOS** (rsnikhil/Learn_Bluespec_and_RISCV_Design,
    MIT): 5-stage RV32 running the in-repo RTOSDemo.memhex32 —
    millions of cycles of REAL SOFTWARE with no RISC-V toolchain;
    paired b_*/v_* Bluesim/Verilator targets and DPI C devices
    (exercises -use-dpi).  One-line `cycle_limit` edit bounds the run.
    The most modern-bsc-idiomatic code in the pool (active 2026).
  - **bluecheck** (CTSRD-CHERI/bluecheck — note: `mn416/blue-check`
    does not exist): QuickCheck-style rule churn, dial-a-length;
    correctness/scheduler stressor rather than throughput.
  - Also surveyed and shelved with reasons: MAERI (accelerator
    alternate), MIT 6.375 audio (FIR/FFT, license unclear), RVBS
    (elaboration stressor), tinsel + airblue + riscy-OOO + SHAKTI +
    quartz (all high-effort: dead TB infra, external toolchains, or
    buck2), OpenSMART/Accel_AES/chromite (stale or no TB).  No
    surveyed repo ships prebuilt Dhrystone/CoreMark images — Fife's
    FreeRTOS and the JPEG encoder are the long real-workload runs.

## Running

    BSC=.../bsc TRS=.../trs python3 tools/bench.py \
        [--filter substr] [--runs 3] [--legs bluesim,verilator,trs]

Emits a table per design and `bench-results.json`.  Run only on an
otherwise-idle machine.
