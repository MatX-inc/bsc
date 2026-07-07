# BIR — the TRS export format

Status: draft, schema version 1.  This document and the Rust types in
`crates/trs-ir/src/` jointly define the contract; where prose and code
disagree, the code is authoritative and this document has a bug.

BIR is the data contract between bsc (Haskell) and the trs backend
(Rust).  bsc exports it after elaboration, scheduling, `simExpand`, and
`simPackageOpt` — the point in today's pipeline where all semantic analysis
is done and C++-shaping is about to begin (`bsc.hs::genModuleC`).
Everything upstream of BIR stays in bsc; everything downstream (LLVM
lowering, runtime, waveforms) lives in Rust and never re-derives schedule
semantics.

## 1. Encoding

- **CBOR** (RFC 8949).  Haskell side: `serialise`/`cborg` (chosen as the
  new dependency on the cabalization path; Debian/Ubuntu package it as
  `libghc-serialise-dev`).  Rust side: `ciborium` + `serde`.
- The Haskell encoder is written to match the `serde` derivation of the
  Rust types (structs as arrays in declaration order; enums as
  `{variant: payload}` maps / bare strings for unit variants — ciborium's
  externally-tagged convention).  The Rust `Design::decode` is the
  conformance checker: it validates the schema version and reference
  integrity before any use.
- One `.bir` file per link (design-level), containing per-module bodies.
  A per-module export for `-c`-style point codegen reuses the same
  `Module` encoding standalone; module `content_hash` is the
  content-addressed cache key.
- All identifiers are interned: `StrId` indexes `Design::strings`.
  Conventions: rules are `RL_*` names as in the `.ba`; instance paths are
  dotted (`"a.b.c"`, `""` = top); qualified rule paths are
  `"a.b.RL_r"` (matching `qualifyChildId`, `SimExpand.hs:1711`).

## 2. Versioning

`BIR_VERSION` (a single u32) is bumped on **any** change to the encoded
shape, compatible or not — no in-band schema evolution.  bsc and trs
releases are expected to move together; a mismatch is a hard, descriptive
error at decode time.  The schema is expected to churn during P0-P3;
stability promises begin when the version is declared 1.0 in both trees.

## 3. Content

### Per module (`Module`) — instantiation-independent, cacheable

Mirrors the post-`simPackageOpt` `SimPackage` (`SimPackage.hs:83-108`):

| Field | Source | Notes |
|---|---|---|
| `inputs`, `clock_domains`, `resets` | `sp_inputs`, `sp_clock_domains`, `sp_reset_list` | |
| `instances` | `sp_state_instances` (`AVInst`) | primitive kind or module ref; constant args; `method_order` = the `sSB` pairs (`MethodOrderMap`); port counts |
| `defs` | `sp_local_defs` | includes `CAN_FIRE_*`/`WILL_FIRE_*` (flagged) |
| `rules` | `sp_rules` | body **pre-linearized** by bsc (`tsortActionsAndDefs` order), plus `me_inhibits` (intra-module, see §4) |
| `methods` | `sp_interface` | value/action/actionvalue, ready expr, linearized body |
| `schedule` | derived (§4) | segmented per (domain, edge) |

Expressions/actions mirror `AExpr`/`AAction` (`ASyntax.hs:936-1148`)
post-`simPackageOpt`: dynamic selects expanded, cases inserted, `ASAny`
resolved, concats normalized.  Rule and method bodies are exported in
final execution order — the intra-rule topological sort (method-order
constraints, foreign-call ordering) is bsc's job, not the backend's.

### Per link (`Design`)

- `modules`, `instance_map`, `top`, `default_clock`/`default_reset`,
  `foreign_funcs` (BDPI signatures; the C ABI itself is unchanged from
  today's conventions).
- `compositions`: the design schedule, hierarchically (§4).

## 4. The hierarchical schedule

**Problem.** bsc's link-time merge produces a global per-domain order over
all rules of all instances (`mergeSchedules` → `flattenCombSchedGraph`).
Exporting that flat order would scale with instance count — a tile grid's
internal scheduling would all be manifest at top level, defeating
per-module-type code generation and caching.

**Factoring.** A module's rules interact with the outside world only
through its interface methods: every cross-boundary schedule constraint
attaches to a method node, and the merge fuses method nodes into the
calling parent's rule nodes (`SimExpand.hs:1040-1076`).  Therefore:

1. **Segments (per module type).**  The module's own schedule order
   (which contains its rule *and* method nodes) is cut at two kinds of
   positions: its interface-method nodes (each cut labeled with the
   methods that execute there, `Segment::cut`), and — because a rule
   that calls a *user-submodule* method is itself a fusion point the
   merge attaches cross-boundary constraints to — every child-calling
   rule node becomes a singleton segment (a child's segments may have
   to run between two such rules; found via sysBug898, where an FSM
   pulses a child and reads the propagated result in the same cycle).
   Primitive method calls do not cut: primitives are not scheduled
   modules.  Segment count is bounded by interface methods plus
   child-calling rules, still independent of internal rule count and
   still per module type.
   Note that a rule's `Sched` and `Exec` nodes may land in *different*
   segments: a method position can legitimately fall between them (e.g. a
   child rule competing with a parent-called method under
   `descending_urgency` computes its fire condition before the method
   runs but executes after), so segment membership is per node, not per
   rule.
2. **Composition (per link).**  The design-level order becomes a sequence
   of `(instance, domain, segment)` references: parent rules execute
   inside the parent's own segments; a child's segment k+1 is scheduled
   after the parent activity that calls the methods in cut k.  bsc
   derives the composition by projecting the merged constraint graph
   (`ss_sched_graph`) onto (instance, segment) units and topologically
   sorting (ties broken by first appearance in bsc's flat order) — the
   flat merged order itself cannot simply be run-collapsed, because it
   freely interleaves `Sched`/`Exec` nodes of different instances.  The
   domain id disambiguates the per-domain segment numbering for modules
   spanning several clock domains.  Size is O(Σ instances × segments),
   i.e. O(instances × methods) — independent of internal rule counts.
3. **Degradation is graceful.**  Any interleaving the constraint graph
   forces (heavily coupled boundaries) shows up as more, smaller
   composition entries — never as a semantic change.  The flat schedule is
   the degenerate case where every segment holds one rule.

**What doesn't factor** (composition-level, small):

- **Cross-module ME inhibitors.**  The merge derives parent↔child disjoint
  pairs through method use (`combineSchedDRDB`, `SimExpand.hs:1362-1429`).
  Which rule inhibits which depends on the composed order, so these are
  exported as qualified pairs (`Composition::cross_inhibits`); codegen
  wires them as per-instance inhibit inputs, constant-folded away when an
  instantiation context makes them dead.  Intra-module inhibitors are
  fixed by the module's own segment order and are exported per rule
  (`Rule::me_inhibits`), keeping the compiled module code shared.
- **Cross-instance tick order** (producers before consumers,
  `sortTickCalls`) and **early rules** (`clock_crossing_rule`), both
  qualified lists.  Ticks are direction-filtered against the primMap
  tick specs; a posedge schedule also owns the opposite edge's tick
  function, exported as a rule-less negedge composition (Neg/Both tick
  ports: ClockInverter, GatedClock, SyncBit05/15).  Each tick carries
  its clock's gate expression (None = ungated) — the `gate_value`
  argument of the C++ tick calls.  Conditional **reset ticks**
  (`mkResetTickStmt`, from `di_prim_resets`) are flagged `reset: true`:
  while a prim's reset is asserted, each posedge of its clock loads the
  reset state.

**Clocks and resets at run time.**  Composition clocks name oscillators:
the default clock (LOW, first edge t=0, high 5 / low 5), a ClockGen
output (`<inst>$CLK_OUT`, waveform from instantiation args), or a
dynamic clock (MakeClock/ClockDiv/ClockInverter) whose edges the driving
prim triggers at tick time (`bk_trigger_clock_edge`).  Clock gating is
already folded into gated rules' CAN_FIREs by bsc; `Expr::Gate` reads a
prim's gate output, and flattened gate wires encode as
`<path>$CLK_GATE_OUT` ports.  Reset semantics follow the kernel: the top
reset asserts at t=0 and deasserts at t=2 after that instant's logic;
derived resets (`<inst>$OUT_RST` / `$RST_OUT` wires) are driven by the
reset-generator prims with async assertion cascading immediately and
other transitions applying at end of timeslice.  Rule bodies carry their
own reset guards as exported Cond statements over reset wires — the
backend only answers "is this wire asserted".

**Semantic invariants** (unchanged from today's Bluesim; DESIGN.md §4):
executing segments in composition order with each rule gated by its
WILL_FIRE, inhibitors applied, ticks after rules, reset ticks guarded —
is TRS-equivalent by the same argument as the current flat schedule,
because the composition is itself a topological order of the same merged
constraint graph.

## 5. What BIR deliberately omits

- Urgency computation, RDY construction, conflict resolution — already
  folded into the exported `WILL_FIRE`/`RDY` defs by bsc.
- The schedule dependency *graph* — trs consumes orders, not graphs; the
  graph stays in bsc (and in `.ba`) for `-show-schedule` tooling.
- `UseCond`-style conditional-use detail (not round-tripped by `.ba`
  either, `GenABin.hs:404-408`).
- Verilog-oriented info: `VPathInfo`, port naming for synthesis, etc.

## 6. Testing the contract

- `trs ir dump <design.bir>` pretty-prints for golden-file diffing
  against bsc's `-ddumpsimexpand`/`-ddumpsimpackageopt` dumps.
- Round-trip property tests on the Rust side; decode-time verification
  (string refs, module refs, segment indices, def references).
- The P1 reference interpreter executes BIR directly; its
  bit-identical-stdout differential runs against today's Bluesim are the
  end-to-end validation that the exported information is *sufficient*.
