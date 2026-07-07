# Bluesim2: a Rust/LLVM simulation backend for BSC

Status: design proposal (working name "Bluesim2"; user-facing name stays
"Bluesim").  This document is grounded in the current implementation — file
references point at the code on the `vlink-regen` branch (PR #2), which this
work builds on.

## 1. Goals

1. **Faster than Verilator, and better scaling with design size.**
   Single-thread throughput first; a credible path to multi-threading second.
2. **Fast build turnaround.**  Code generation and linking must not be the
   bottleneck of the edit-compile-run loop.  Today the generated-C++ → g++
   path dominates link time for large designs; the replacement generates
   machine code directly through LLVM, in parallel, with content-addressed
   caching, and offers a JIT mode with no object files at all.
3. **Same semantics.**  Execute the static schedule computed by bsc and
   implement TRS (one-rule-at-a-time) semantics exactly as today's Bluesim
   does, validated against the existing testsuite.
4. **Hierarchical code generation.**  Per-module compilation units that are
   reusable across instantiations and cacheable across links — extending the
   staged-codegen model of PR #2 (`-c` is point codegen, link is the closure)
   — instead of today's design-wide monolithic schedule file.
5. **First-class waveforms.**  VCD *and* FST output, carrying full module
   hierarchy/definition information, without the current "backing model"
   double-instantiation cost.
6. **Aggressive state inlining.**  Registers and wires become plain struct
   fields / SSA values with direct loads and stores, not objects with method
   calls; only primitives with genuinely stateful protocols (FIFOs, BRAMs,
   synchronizers, clock generators) remain runtime calls.
7. **Drop-in compatibility.**  Keep the `bk_*` kernel C ABI and the
   `bluesim.tcl`/bluetcl driver working unchanged; keep BDPI, `$display`
   formatting, plusargs, and (eventually) the SystemC wrapper.

Non-goals (initially): 4-state simulation (Bluesim is 2-state today), save/
restore checkpointing (does not exist today either), Verilog co-simulation.

## 2. Where Bluesim stands today

A condensed map of the current implementation; this is what we must be
equivalent to, and what we are replacing.

### 2.1 Compile pipeline

The `.ba` file stores, per synthesized module, the **post-scheduling
`APackage`** (rules still rules) plus the **`AScheduleInfo`**
(`ABin.hs:37-93`).  Bluesim does *not* consume `ASPackage` — that flattened,
mux-based form is Verilog-only (`AState.hs:90-156`).

At link time (`bsc.hs::genModuleC`, driven from `simLink`):

- `SimExpand` reads the `.ba` hierarchy into one `SimPackage` per module
  (`SimPackage.hs:83-108`) and **merges every module's schedule into one
  global graph**, then splits it per clock domain and topologically flattens
  it into a single linear order of `Sched r`/`Exec r` nodes
  (`SimExpand.hs:720-868`, `314-378`).
- `SimMakeCBlocks` turns each `SimPackage` into a `SimCCBlock` (a C++ class
  model: state instances, defs, one function per rule/method) and each
  flattened per-domain order into a `SimCCSched` — the schedule function
  (`SimMakeCBlocks.hs:695-841`).
- `SimBlocksToC` prints C++: one `.h`/`.cxx` per module class, plus
  `schedule_<Top>.cxx` and `model_<Top>.{h,cxx}`; g++ compiles everything
  (optionally in parallel, `-parallel-sim-link`) and links a `.so` against
  `libbskernel.a`/`libbsprim.a`.  The "executable" is a shell script running
  `bluesim.tcl` against the `.so` (`bsc.hs::cxxLink`).

Per-module C++ is instantiation-independent ("reusable block" — see the PR #2
user-guide text), and object reuse exists (`SimFileUtils.hs`), but **the
schedule is a design-wide monolith**: it grows with the whole design, is
regenerated on any change, and is the worst compile unit for g++ (one huge
function full of cross-module member accesses).

### 2.2 Execution model (the TRS contract)

- Per (clock, edge) the kernel calls one generated **schedule function**
  whose body is, in order: (1) zero all rule `WILL_FIRE`s and method enables;
  (2) for each `Sched r` node, compute the defs feeding `CAN_FIRE_r`/
  `WILL_FIRE_r`; for each `Exec r` node, `if (WILL_FIRE_r) rule_r();`
  — in the flattened **earliness order**; (3) `clk()` ticks for primitives
  that need end-of-cycle bookkeeping; (4) a reset-tick block guarded by a
  global counter (`SimMakeCBlocks.hs:808-841`, `reset.cxx:1-43`).
- `WILL_FIRE_r = CAN_FIRE_r && !WILL_FIRE(more-urgent conflicting rules)` —
  the Esposito encoding (`AAddScheduleDefs.hs:28-84`; conflict pairs from
  `ASchedEsposito`, `ASyntax.hs:380-401`).
- Rules mutate state **in place**; registered semantics fall out of the
  schedule ordering reads before writes.  Because execution is destructive,
  Bluesim adds **ME inhibitors**: if rule r2 is disjoint with an earlier
  executed rule r1, `CF_r2 &&= !CF_r1` so r2 cannot observe r1's effects and
  fire when the TRS says at most one fires (`SimMakeCBlocks.hs:1636-1658`).
- Primitives that can be *read after being written* in the same instant keep
  a begin-of-cycle shadow guarded by a `bk_now()` timestamp: ConfigReg,
  RegTwo, CReg (port rotation at `clk()`), crossing regs, FIFO's
  `i_notEmpty/i_notFull`, RegFile write-forwarding (`bs_prim_mod_reg.h`,
  `bs_prim_mod_fifo.h:33-200`, `bs_prim_mod_regfile.h:364-410`).
- Intra-rule ordering of two method calls on one instance follows the
  submodule's `sSB` relation, captured as the `MethodOrderMap`
  (`SimExpand.hs:1842-1846`) and enforced by a topological sort of actions
  and defs inside each rule body (`tsortActionsAndDefs`,
  `SimMakeCBlocks.hs:1248-1533`).
- Rules with `clock_crossing_rule` run in a separate **after-edge** function
  (`ss_early_rules`; `run_combo_schedule_event`, `kernel.cxx:315`).

### 2.3 Kernel and runtime

A single binary-heap event queue ordered by `(time, packed priority)`;
priority packs `group << 28 | slot << 24 | clock#` — groups: INITIAL,
BEFORE_LOGIC, LOGIC, AFTER_LOGIC, FINAL; slots: RESET, UI, CYCLE_DUMP, VCD,
EXECUTE, … (`priority.cxx`, `event_queue.cxx`).  Clocks are `tClockInfo`
records with waveforms; derived/gated clocks are **aperiodic** clocks whose
edges are injected by generator primitives calling `bk_trigger_clock_edge`
from their `clk()` tick (`bs_prim_mod_clockgen.h`).  The simulation runs on
its own pthread; bluetcl drives it through the `bk_*` C API over a `dlopen`ed
`.so` (`bs_model.h`, `bluesim_kernel_api.h`, `BluesimLoader.hs`).

### 2.4 VCD

Change detection instantiates a **second complete copy of the model** (the
"backing instance") and walks the whole hierarchy every active timeslice
comparing live vs backing values (`SimBlocksToC.hs:512-546`,
`bs_prim_mod_reg.h:162`).  Signal IDs are sequential ints in base-94; times
are corrected so combinational signals appear to change after the *previous*
edge, via per-signal clock association and a pending-changes buffer keyed by
time (`vcd.cxx:15-34`, `387-462`).  No FST support.

### 2.5 What is slow today

Compile side:
- C++ is generated as *text*, then g++ re-parses it plus the template-heavy
  primitive headers per translation unit, at -O2, serially by default.
- The monolithic schedule file scales with the design and recompiles on any
  change; PR #2's reuse machinery explicitly cannot reuse it ("the top
  module, the schedule, and the model files are always generated by the link
  itself").

Run side:
- Every Reg/Wire/CReg is a C++ object; reads/writes are member calls into
  another object's storage.  Within one .o g++ inlines them, but rule code in
  module A calling methods of module B crosses translation units — no
  cross-module inlining without LTO (not used).
- The VCD backing model doubles memory and walks *all* signals per timeslice.
- Wide data is heap-ish (`WideData` with pooled allocation, word loops).
- Symbol tables, `Module` bookkeeping, and per-instance name strings are
  built eagerly for every instance at startup.

## 3. Architecture overview

```
                bsc (Haskell, unchanged front/middle)
   .bsv ──► elaboration/scheduling ──► .ba  (APackage + AScheduleInfo)
                                        │
                                        │  NEW: SimExportIR  (-sim2 codegen)
                                        ▼
                                 .bir  per module            ┌──────────────┐
                     (portable post-schedule IR, versioned)  │ user C/C++   │
                                        │                    │ BDPI objects │
                                        ▼                    └──────┬───────┘
        ┌──────────────────────── bsim2 (Rust) ─────────────────────┼──────┐
        │  ir: load/verify  ─►  plan: link closure, schedule merge, │      │
        │  inline decisions ─►  codegen: LLVM IR per module (parallel)     │
        │        │                        │                                │
        │        │             ┌──────────┴──────────┐                     │
        │        │             ▼                     ▼                     │
        │        │      ORC JIT (dev loop)   AOT .o + content cache        │
        │        │             └──────────┬──────────┘                     │
        │        ▼                        ▼                                │
        │  kernel + rt + wave (Rust staticlib)  ──►  link                  │
        └──────────────────────────────┬───────────────────────────────────┘
                                       ▼
                     <top>.so exporting bk_* C ABI   +  <top> native runner
                     (bluetcl/bluesim.tcl unchanged)    (no tcl dependency)
```

Split of responsibilities:

- **bsc keeps** everything through scheduling — evaluation, rule splitting,
  urgency/earliness computation, `CAN_FIRE`/`WILL_FIRE` def insertion.  A new
  small Haskell module (`SimExportIR.hs`) serializes the per-module
  post-schedule data to a stable, documented format.  bsc's semantic
  knowledge is not reimplemented.
- **bsim2 owns** everything downstream: the link closure, schedule merging,
  optimization, code generation, runtime, and waveforms.  It is a standalone
  Rust program invoked by `bsc` exactly where `simLink`/`genModuleC` runs
  today (and usable directly by build systems).

### 3.1 Why a new exchange format instead of reading `.ba`

`.ba` is a bespoke lazy Haskell binary encoding with structure sharing,
defined by `Bin` instances over bsc's internal types (`BinData.hs`,
`GenABin.hs`).  A Rust reader would be version-locked to bsc's internals and
break on every datatype change.  Instead, bsc gains an export pass emitting
**BIR** (Bluesim IR): CBOR (self-describing, fast, serde-native) with an
explicit schema version, containing only what simulation needs.  Note the
`.ba` already drops information Bluesim must recompute (e.g. `UseCond`s are
not round-tripped, `GenABin.hs:404-408`), so `.ba` was never a complete
interface either; BIR makes the actual contract explicit and testable.

Contents per module (essentially `SimPackage` minus Haskell-isms —
see `SimPackage.hs:83-108` and the `APackage` fields it mirrors):

- module name, wrapper flag, generation-options descriptor (for staleness);
- inputs (clocks with gates, resets, method args/enables), clock domains,
  reset list, output-clock/gate info;
- state instances: primitive kind or user-module name, instantiation
  parameters (constants — Bluesim already requires this,
  `SimExpand.hs:2158-2195`), per-method port types, multi-port counts,
  and the instance's `MethodConflictInfo` (for `sSB` ordering);
- local defs (`ADef`s, including `CAN_FIRE_*`/`WILL_FIRE_*`), rules
  (predicate ref + action list), interface methods;
- the schedule: `Sched`/`Exec` node order, the dependency graph, Esposito
  conflict pairs, disjoint/exclusive rule sets, early (clock-crossing)
  rules, schedule pragmas;
- foreign-function signatures used (name, C name, arg/return styles).

Expressions and actions mirror `AExpr`/`AAction` (`ASyntax.hs:936-1148`):
prim ops, constants, def/port/param refs, method calls/values, foreign
calls, task actions with cookies, gate refs.  The format is a data contract,
not an ABI: it is versioned, and `bsim2 ir dump` pretty-prints it for
diff-testing against bsc's own `-ddumpsimexpand` output.

## 4. Execution semantics in bsim2

Identical to today, restated as the invariants the code generator must
uphold:

1. Per (clock, edge), execute the flattened earliness order: compute fire
   conditions at `Sched` nodes, conditionally run rule bodies at `Exec`
   nodes, in place.
2. `WILL_FIRE` per Esposito; ME inhibitors for disjoint rules executed
   earlier in the same edge (destructive-execution correctness patch).
3. Intra-rule action/def ordering per `MethodOrderMap` (`sSB`).
4. Begin-of-cycle shadows for the read-after-write primitives (ConfigReg,
   RegTwo, CReg ports, crossing regs, FIFO `i_*` methods, RegFile
   forwarding); everything else reads live state.
5. Primitive ticks after rules, producers before consumers
   (`sortTickCalls`, `SimMakeCBlocks.hs:646-680`); then the guarded
   reset-tick block.
6. Clock-crossing rules in the after-edge function at FINAL priority.
7. Event ordering by `(time, group, slot, clock#)` exactly as
   `priority.cxx` packs it — this is observable through `$display`
   interleaving across domains and must match.

The plan is to encode these as a *semantics test kit* first (see §10):
a reference interpreter over BIR that the LLVM backend is differentially
tested against, and both against today's Bluesim.

## 5. Code generation

### 5.1 State layout: inline registers and wires

Each module becomes an LLVM struct type; each instance a field (or array of
fields for replicated instances) inside its parent — the whole design is
**one contiguous state allocation** with statically known offsets.

- `Reg`/`RegU`/`RegA`, `ConfigReg`, `RWire`/`Wire`/`PulseWire`, `BypassWire`,
  `CReg`, `Probe`, `Counter`, `RegTwo` are **not objects**: their storage is
  plain fields (`iN` for N ≤ 64, `[n x i32]`/`iN` beyond).  Reads are loads,
  writes are stores; the schedule order supplies register semantics.
  ConfigReg/RegTwo/CReg keep their small shadow fields with the same
  timestamp-free trick where possible: because the schedule is static, the
  codegen *knows* whether a same-cycle earlier write can reach a read and can
  materialize the shadow only when the schedule actually requires it — most
  ConfigRegs degrade to plain Regs after this analysis (today's runtime pays
  the `bk_is_same_time` check on every access, unconditionally).
- Wires zero their valid bits at edge start (fused with the existing
  enable-zeroing pass over a contiguous region — a few `memset`-like stores)
  or, when a wire's writer and readers are all in one domain segment and the
  liveness is local, the wire is **SSA-converted away** entirely.
- FIFOs, BRAMs, RegFiles, synchronizers, clock/reset generators remain
  runtime primitives in Rust (`bsim2-rt`), *monomorphized by codegen*: the
  generator emits calls to width-specialized `extern "C"` entry points
  (≤ 8/32/64-bit and wide variants), so no C++-template-style header cost and
  no dynamic dispatch.  Small FIFOs (depth ≤ 2, the overwhelmingly common
  `mkFIFO`/`mkPipelineFIFO` cases) get direct inline-IR expansions in a later
  optimization pass.
- Wide data (> 64 bits) uses LLVM's native arbitrary-width integers (`i128`,
  `i347`, …) for values and ops — LLVM legalizes them well — with `[n x i32]`
  storage in state structs for layout stability; no heap, no `WideData`
  objects, no `wop_*` out-parameters.

### 5.2 Hierarchical code generation

The unit of code generation is the **module** (as in PR #2's `-c` model),
not the design:

- Per module, per clock domain *that the module participates in*, emit a
  **domain segment**: `seg_<Mod>_<domain>_<edge>(state*)` containing that
  module's `Sched`/`Exec`/tick statements in schedule order, plus calls into
  child segments at the points the merged schedule dictates.  The top-level
  per-domain schedule function is then a short driver calling segments — not
  a design-wide flat function.
- This is possible because the merged schedule usually interleaves modules in
  large runs, not randomly: the merge qualifies child rules by instance path
  and topologically sorts; contiguous runs from one instance become segment
  calls, and only genuinely interleaved rules force segment splits.  Worst
  case degrades to today's flat order (correctness never depends on
  segmentation).
- Rule bodies and methods are per-module LLVM functions; method calls across
  module boundaries are direct calls with the callee's state pointer — and
  since the whole design is one LLVM program at link, **cross-module inlining
  is an optimization-pass decision, not a translation-unit boundary**.  Small
  value methods (the `_read` of an inlined reg, RDY exprs) disappear
  entirely.
- Per-module code is emitted into its own LLVM module keyed by
  `(module, codegen options, BIR hash)` → object cache.  Instantiating the
  same BSV module N times costs one codegen.  The only always-regenerated
  pieces are the thin top-level drivers (mirroring PR #2: "the top module,
  the schedule, and the model files are always generated by the link").

### 5.3 Fire-condition and rule optimization

All standard LLVM scalar optimization applies after inlining, but the
schedule-aware wins come from our own passes over BIR before LLVM:

- **Dead-def pruning per cone**: only defs feeding a `WILL_FIRE`, an action
  argument/condition, or a wave-visible signal are materialized; others fold
  into rule bodies as SSA (today `SimCOpt.moveDefsOntoStack` approximates
  this; we do it by construction).
- **Disjointness short-circuits**: the `ExclusiveRulesDB` lets us emit
  `else`-chains instead of independent tests for mutually exclusive rules,
  and skip ME-inhibitor terms that LLVM cannot know are redundant.
- **Branch metadata**: `WILL_FIRE` tests get profile-informed or
  heuristic (`likely taken`) weights; rule bodies are laid out cold/hot.
- **Gate/reset hoisting**: gated-clock and in-reset tests hoist out of
  segment bodies.

### 5.4 System tasks and BDPI

`$display`-family keeps the current architecture (compiler-known format
string + parallel width-descriptor string, `dollar_display.cxx:169-350`) but
with a non-varargs ABI: codegen packs arguments into a stack array of
`(descriptor, value/pointer)` slots and calls Rust runtime formatting.  BDPI
imported C functions keep their exact current C ABI (including the
`Direct`/`Buffered` return styles and polymorphic `unsigned int*` marshaling,
`ForeignFunctions.hs:305-341`) so existing user C code links unchanged.

## 6. Compile-time strategy

This is a first-class requirement, not a byproduct.

- **No C++ in the loop.**  IR is constructed in memory and lowered by LLVM
  directly to objects.  We skip: text generation, g++ parsing (~500 KB of
  primitive headers per TU today), template instantiation, and EH/RTTI
  bookkeeping.  For a mid-size design where today's link spends minutes in
  g++, LLVM -O1 on already-clean IR is expected to be an order of magnitude
  faster; -O0+JIT nearly free.
- **Parallel by module.**  One LLVM context/module per BSV module, codegen
  and object emission fanned across cores (rayon).  Today only the g++ step
  parallelizes (`-parallel-sim-link`), and the biggest TU (the schedule)
  serializes the tail.  Segmented schedules (§5.2) break that tail up.
- **Content-addressed object cache.**  Key = BIR hash ⊕ codegen options ⊕
  bsim2 version, mirroring PR #2's `StaleUtils` conventions ("missing product
  is never fresh; equal times are fresh") but by content, not mtime, so
  rebuilding an unchanged module is a cache hit even after `touch`.  This
  extends `-c` point codegen naturally: `bsc -sim -c mkFoo` can emit
  `mkFoo.o` via bsim2, and link reuses it — same mental model, same flags,
  as the Verilog side of PR #2.
- **Two execution modes.**
  - **JIT (default for iterate-run):** ORC/LLJIT, lazy per-segment
    compilation at -O0/-O1 — simulation starts in milliseconds after link
    planning; hot segments can be recompiled at higher opt while idle.
  - **AOT (default for `bsc -o`)**: emit objects, link the `.so` +
    runner; -O2/-O3 for long-running regressions.
- **Tiered effort knobs** surfaced as flags (`-sim-opt 0..3`), because "run
  a 10-second smoke test" and "run a 10-hour soak" deserve different
  compile budgets.

## 7. Runtime kernel (Rust)

A port of the current kernel's *semantics* with its accidental complexity
removed:

- Event queue: binary heap of `(time, packed priority)` exactly reproducing
  `priority.cxx` packing (observable ordering).  Handlers are enum variants,
  not fn pointers, so the hot path (clock edge → segment calls) is a direct
  match and call.
- Clocks: periodic waveforms and aperiodic derived clocks with
  `trigger_clock_edge` from generator primitives, `combinational_at`
  bookkeeping for wave time-correction, edge counters/limits for
  bluetcl `step`.
- Reset: global `reset_tick_requests` counter gating a per-edge reset block;
  async resets act immediately; generated resets defer to end-of-timeslice —
  as today (`reset.cxx`).
- Threading: the kernel itself is single-threaded and synchronous; the
  `bk_advance`/`bk_sync` async protocol is provided by an optional driver
  thread in the compat layer (bluetcl expects it), not baked into the core.
- The `bk_*` API is exported from a `cdylib` with the same symbol set the
  export maps allow today (`bs_elf_export_map.txt`), so `BluesimLoader.hs`
  and `bluesim.tcl` work unmodified.  Additionally a **native runner** binary
  links the same core so `./sim` runs without tcl (plusargs, `-V vcd`,
  `--fst`, `-m cycles` style flags) — removing tcl startup from CI hot paths.

## 8. Waveforms: VCD and FST

One `wave` subsystem with two writers behind a trait; both fed by the same
change-capture machinery.

- **Change capture without a backing model.**  Codegen knows every store to
  wave-visible state.  In wave-enabled builds it emits, at each commit point,
  a compare-and-append into a per-domain **change buffer** (signal id, new
  value) — no second model instance, no full-hierarchy walk per timeslice.
  Signals whose writes are unconditional every cycle (clocks, counters) can
  opt into periodic snapshotting instead.  Wave-disabled builds pay zero: the
  instrumentation is a codegen variant, selected at link (JIT mode can
  re-lower segments when `$dumpvars` first fires, so even "wave-capable"
  binaries pay nothing until enabled).
- **Time correction** (combinational values appearing after the previous
  edge) is kept: each signal carries its driving-clock association; the
  buffered changes flush once a timeslice's `combinational_at` frontier
  passes, as `vcd.cxx:387-462` does today.
- **VCD writer**: base-94 ids, `$scope module` hierarchy, same output shape
  as today (byte-compatibility where feasible makes diff-based migration
  testing possible).
- **FST writer**: via the pure-Rust `fst-writer` crate (validated to build
  in-tree), giving compressed, seekable waves.  Hierarchy carries **module
  definition information**: FST scopes support `(scope type, instance name,
  definition name)`, so every instance scope records its BSV module name
  (from `InstModMap`), and signals carry width/type plus rule
  `CAN_FIRE`/`WILL_FIRE` when `-keep-fires` is on.  VCD approximates the
  same with `$comment` metadata per scope.
- Symbol/introspection tables (for bluetcl `sim lookup/get`) are generated as
  static data (sorted, shared per module type), not per-instance heap
  constructions.

## 9. Performance versus Verilator: why this can win

Structural advantages we inherit from Bluespec + TRS:

- **The schedule is computed once, by the compiler.**  Verilator evaluates a
  levelized combinational netlist and re-evaluates fanout cones; Bluesim
  executes ~one branch + one body per rule per cycle, and dead rules cost a
  single well-predicted branch.  There is no convergence loop, no eval/trigger
  bookkeeping.
- **Coarse grain.**  Rules are much bigger than gates; the work per branch
  decision is larger, and the state accesses within a rule body are
  register-allocatable SSA after inlining.
- **Two-state, word-packed** — same as Verilator; no penalty there.
- **Less materialized state.**  Verilog-visible intermediate wires don't
  exist unless waves need them; Verilator must keep everything its scheduler
  or taps touch.

What we must do well to actually win (and how):

- **Cross-module inlining** (§5.2) — Verilator gets this from generating one
  C++ program; we get it inside LLVM at link, with a whole-design view.
- **Memory locality** — one contiguous state allocation, fields ordered by
  schedule-adjacency (rules touching them are neighbors in execution order),
  hot/cold splitting (wave shadows and rarely-used state segregated).
- **No per-access overhead** — no `bk_is_same_time` timestamp checks on the
  common path (statically resolved, §5.1), no virtual calls, no symbol/name
  machinery in the hot path.
- **Wave capture that doesn't tax non-wave runs** (§8) — today VCD-capable is
  always paid for (backing model allocated when dumping starts, dump walk per
  slice).
- **Scaling**: compile is per-module and cached (Verilator recompiles the
  world), and the runtime's flat state + segment structure is the natural
  substrate for later **partitioned parallel execution**: per clock domain
  first (independent by construction outside crossing rules), then
  rule-graph partitioning within a domain (conflict-free segments with
  private commit buffers).  Parallelism is explicitly phase 6 — single-thread
  wins come first, and Verilator's multithread mode is the bar for the
  parallel phase, not the serial one.

Benchmark plan: the testsuite's larger designs plus external cores that
already build with bsc (Piccolo/Flute-class RISC-V SoCs, Ethernet/DMA-style
designs), measured three ways — wall-clock cycles/sec, link-to-first-cycle
latency, and full edit-relink-rerun loop — against current Bluesim and
Verilator (`--threads 1` and best-N) on the same RTL.

## 10. Phasing

- **P0 — BIR export + loader.**  `SimExportIR.hs` behind `-sim2` (reusing
  `SimExpand`'s checks); Rust `ir` crate loads/verifies/dumps; golden-file
  diffs against bsc dumps.  Deliverable: every testsuite `.ba` round-trips.
- **P1 — Reference interpreter.**  Tree-walking evaluator over BIR with the
  §4 semantics, wired to kernel + rt + `$display`.  Slow but complete; it is
  the differential oracle.  Deliverable: testsuite `bsc.bluesim` cases pass
  bit-identically (stdout diff) vs current Bluesim.
- **P2 — LLVM codegen, single domain.**  Registers/wires inlined; flat (not
  yet segmented) schedule function; JIT mode; kernel `bk_*` cdylib; VCD.
  Deliverable: interpreter-vs-JIT differential green; first perf numbers.
- **P3 — Full surface.**  MCD (derived/gated clocks, synchronizers,
  crossing rules), resets, full primitive set, BDPI, bluetcl parity,
  native runner.  Deliverable: full testsuite parity.
- **P4 — Hierarchical codegen + caching.**  Domain segments, per-module
  object cache, `-c` integration per PR #2 conventions, AOT mode, FST.
- **P5 — Performance program.**  Layout, branch metadata, small-FIFO
  inlining, wave-capture tuning; publish benchmark suite vs Verilator.
- **P6 — Parallel execution; SystemC wrapper.**

## 11. Risks and mitigations

- **Semantics drift** (ME inhibitors, timestamp shadows, `$display`
  ordering, event tie-breaking): mitigated by the interpreter-first plan and
  bit-identical stdout/VCD differential testing; the priority packing and
  earliness flattening are ported, not reinvented.
- **BIR schema churn**: versioned schema, decode-time validation, and the
  exporter lives in bsc's tree so datatype changes break the build, not the
  wire format silently.
- **LLVM API churn / packaging**: pin via `inkwell` (LLVM 18 validated
  in-tree); prerequisites are `llvm-18-dev` + `libzstd-dev`; JIT-only mode
  needs no system linker.  Fallback codegen via textual `.ll` emission is
  kept behind a feature for debugging.
- **Haskell-side maintenance**: `SimExportIR.hs` is small (serialization
  only) and colocated with `SimPackage`; the heavy semantic passes it reuses
  (`SimExpand`, checks) already exist.
- **`$display` fidelity**: the format engine is ported with its tests; the
  descriptor-string contract is preserved.
- **fst-writer maturity**: it is young; we keep the writer behind a trait,
  validate against GTKWave/Surfer readers in CI, and can swap to C `fstapi`
  bindings without touching capture code.

## 12. Repository layout

```
src/bluesim2/               Rust workspace (this directory)
  DESIGN.md                 this document
  crates/
    bsim2-ir/               BIR schema, loader, verifier, pretty-printer
    bsim2-kernel/           event queue, priorities, clocks, resets, bk_* core
    bsim2-rt/               primitives (FIFO, RegFile, BRAM, sync*), system
                            tasks, plusargs, wide-data helpers
    bsim2-wave/             change capture, VCD writer, FST writer
    bsim2-codegen/          LLVM lowering (feature "llvm", needs llvm-18-dev)
    bsim2/                  CLI: link planner, JIT/AOT driver, native runner
src/comp/SimExportIR.hs     (P0) BIR exporter, invoked from the -sim2 path
```

`cargo build` in `src/bluesim2` builds everything except `bsim2-codegen`
unless `--features llvm` is given, so the workspace compiles on machines
without LLVM dev packages.
