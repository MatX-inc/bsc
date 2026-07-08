# TRS — session handoff

Branch: `claude/trs` (all work committed and pushed through
`b9276b1`).  Read `DESIGN.md` (goals/architecture), `BIR.md` (export
format), `docs/VCD-CONTRACT.md` (byte-level VCD semantics), and
`docs/PERF-BASELINE.md` (measured numbers) alongside this.

## What this is

A replacement Bluesim backend: bsc grows a first-class `-trs` flag
whose compile phase is identical to `-sim` (same `.ba`), but whose link
phase exports a CBOR "BIR" file (`src/comp/SimExportIR.hs`), compiles
any user BDPI C into a companion `<top>.bdpi.so`, and writes a wrapper
script that execs the Rust runtime: `trs run <top>.bir [args]`.
Everything downstream of the `.ba` is Rust (`src/trs/crates/`).

Phase status:
- **P0/P1 complete** — full BIR export + a reference interpreter
  (`trs-interp`) with exact TRS semantics: 551/699 differential
  designs byte-match reference Bluesim stdout+exit (rest are
  no-source/negative/unsupported), zero diffs, zero panics.
- **BDPI complete** — dlopen + integer-class trampolines, wide/poly
  out-pointer ABI, string args; stdout flush discipline around calls.
- **VCD complete** — `-V` and all `$dump*` tasks at **byte parity**
  with reference Bluesim (modulo the `$date` line).  The 9-design
  regression battery is `tests/vcd/run.sh`
  (`BSC=... TRS=... sh run.sh [workdir]`) — keep it at 9/9.
- **P2 next: LLVM JIT** (task list #19).  The interpreter is the
  oracle, not the product: measured ~335x slower than compiled Bluesim
  on a tight loop, >1600x on sudoku (see PERF-BASELINE.md).  Link is
  already 11-20x faster than `-sim` C++ codegen.

## In flight right now (finish this first)

1. **bsc rebuild running** (`make install-src`, log:
   `$SCRATCH/bscbuild3.log`).  It carries the new `ifc_resets` export
   (SimExportIR.hs) — the fix for interface **output resets**
   (`fifo.rd_rst_o` style).  Without it, parents bind such resets to a
   fresh never-driven node and downstream registers never leave their
   undet state (this froze the read side of the
   `bsc.interra/MCD_library/SpecialSyncFIFO` and `SpecialSyncReg`
   testbenches — they spin forever).  The Rust side (BIR field with
   serde default + node-merge in `instantiate()`) is already merged.
2. After the rebuild: `make -C src/Libraries install` if Prelude .bo
   version errors appear, then **regenerate and verify**:
   `cd testsuite/bsc.interra/MCD_library/SpecialSyncFIFO`, rebuild with
   `bsc -trs`, run each `mkTestbench_*` and diff against
   `*.out.expected`.  Same for SpecialSyncReg and a spot-check of
   NullCrossing.
3. **Rebuild the release runtime**: `cd src/trs && cargo build
   --release` (the release binary predates today's DualPortRam,
   LatchCrossingReg, BDPI-flush, BypassWire0 and driver-flag fixes).
4. **Regression battery**: `tests/vcd/run.sh` (9/9) and a full
   differential sweep (`python3 tools/diffsweep.py`, ~40 min; last
   clean run: sweep 22, 551 PASS / 0 DIFF).
5. **Clean full testsuite run** for the final tally:
   `cd testsuite && env CONFIG_SHELL=/bin/sh SIM_BACKEND_FLAG=-trs
   TRS=<repo>/src/trs/target/release/trs
   make VTEST=0 SYSTEMCTEST=0 check`
   (locale `en_US.UTF-8` must exist; dejagnu + csh installed).  The
   last run processed 862 .exp files and aborted near the end on an
   unrelated `-dparsed` dump flake in `bsc.syntax/bsv05_parse_pretty`
   (passes standalone — likely load-transient; re-check).

## Expected remaining testsuite failures (all triaged)

- `bsc.bluesim/interactive` (+ scattered `-c {sim ...}` tests in
  bsc.if, bsc.mcd, bsc.bluesim/misc): the **Tcl scripting surface**.
  trs handles `-m`, `-V`, `+args`, `-h/-v`, and exits 0 on the
  deprecated `-s/-ss/-r/-cc` exactly like bluesim.tcl, but `-c`/`-f`
  (interactive `sim step/run/time/clock/lookup...`) is an open product
  decision — ask the user (task #20) before building anything.
- Codegen-inspection tests are guarded via `cxx_codegen_tests` in
  `testsuite/config/unix.exp` (skip under `-trs`, run under `-sim`).
- Transient missing-file flakes (bsc.arrays, primtcons,
  evaluator/prims/name): pass standalone; re-run.
- `bsc.bsv_examples/mcd_Rand` mkTop diff and the
  `bsc.interra/libraries/SRAMFile` link failure are **unverified** —
  investigate.  `bsc.codegen/rdy_en_pragmas` (3 tests) is a **real
  open bug**: top-level `always_enabled` methods must be gated on
  their RDY when invoked every cycle (cvtIFace `check_rdy` wraps the C++
  method body in `if (RDY)`; the interpreter's top-method invocation
  path doesn't).  Reference: Cycle N shows Count N-1; trs shows N.

## Cardinal rules / gotchas

- **Never rebuild `inst/bin/bsc` or `target/release/trs` while a
  sweep, battery, or testsuite run is using them.**  Develop against
  `CARGO_TARGET_DIR=$SCRATCH/cargo-alt cargo build` (debug) instead.
- The interpreter panics on unimplemented prims — a wedged/spinning
  `trs run` in the testsuite means a *semantic* bug (like the
  ifc_resets one), and dejagnu may not time it out; watchdog-kill
  processes older than ~6 min during suite runs.
- Reference executables are scripts needing `bluetcl` on PATH
  (`PATH=<repo>/inst/bin:$PATH`).
- BIR files are re-exported by the *installed* bsc; after exporter
  changes, stale `.bir`/`.ba` need regeneration (Binary version
  mismatch errors → recompile the design; stale Prelude → reinstall
  src/Libraries).
- The undet pattern is 0xAA...; prim state dumps it, but **method
  ports zero-initialize** (mkPortInit) — VCD initial values differ
  accordingly.
- VCD member selection replicates `SimCOpt.moveDefsOntoStack` (defs
  referenced by >=2 generated functions, or >64-bit, or task defs, or
  pinned by `-keep-fires`).  See `vcd_mod_vars` in
  `crates/trs-interp/src/lib.rs` and the reference quirks noted
  inline (SyncReset/handshake clock aliases use kernel clock 0's id;
  `backing.in_reset` never updates; ClockDivider blocks ticks in
  reset; the FIFO's reset calls METH_clear).
- Prim VCD hooks still TODO: SyncFIFO (depth+13 ids), RegAligned;
  everything else common is done and battery-verified.

## Key file map

- `src/comp/SimExportIR.hs` — BIR exporter (runs on `sim_system_opt`,
  the exact SimPackage the C++ backend consumes).
- `src/comp/bsc.hs` — `-trs` flow: `genModuleC` early-exit +
  `trsLink` (BIR copy, BDPI .so via C compiler + `cxxCompile
  -shared`, wrapper script).  Flags/FlagsDecode/GenABin carry
  `genTrs` (Bin Flags is 138 fields — keep read/write in sync).
- `crates/trs-ir` — schema (serde CBOR; new fields need
  `#[serde(default)]`).
- `crates/trs-interp` — `lib.rs` (instantiation, eval, run loop,
  reset network, VCD walks), `prim.rs` (all primitives + VCD hooks),
  `vcd.rs` (writer: back-dating via `time_of_change`, `min_pending`
  buffering), `bdpi.rs`, `format.rs`, `value.rs`.
- `tools/diffsweep.py` — 699-design differential harness.
- `tests/vcd/run.sh` — VCD byte-parity battery.

## Task list

#18 finish the triage/verification above; #19 P2 LLVM JIT
(inkwell/LLJIT, interpreter as differential oracle, wide-data lowering
to u32-limb ops, prims stay as calls into the runtime); #20 the Tcl
`-c/-f` decision (user input needed).  Standing directive: keep
commits small and pushed; keep going fast.
