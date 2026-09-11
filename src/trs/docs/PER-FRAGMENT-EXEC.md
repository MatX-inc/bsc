# Per-fragment execution: scaling, and linking external Verilog

Status: the leaf seam is done -- external Verilog runs as a prim.  The
fragment seam has its substitution point but neither an execution contract
nor a compilation unit.  Sections 4 and 5 are what is left.

*Fragment* here means a synthesis boundary: a synthesized module plus every
instance beneath it that is not itself a synthesis boundary.  In BIR that is
exactly a `Module` (bsc exports one per `.ba`), and `InstanceKind::Module`
names a nested fragment.

## 1. Why

1. **Scaling.**  One rule emitted 66.8M LLVM instructions -- 64% of a
   design's whole IR, in a single function the chunker cannot split.  That
   link took 6.6 hours, 61% of it in two per-function codegen passes.
2. **External Verilog.**  A Verilated fragment has no rules, no segments and
   no visible schedule -- only ports and `eval()`.  A final design must let
   one sit where a Bluespec fragment sits.

These are two seams, not one, and most of the scaling work is independent of
Verilog import rather than a prerequisite for it.  They converge only for the
ambitious case: verilating bsc's own generated Verilog for a fragment and
swapping it in, which BOUNDARY-CONTRACT.md calls "the auto-derived
BVI-equivalent of a synthesized module".

## 2. The design was already specified; the implementation diverged

DESIGN.md 5.2 specifies per-segment functions per module type, a composition
driver of (instance, segment) calls, per-module LLVM modules keyed by
`(module, options, BIR hash)`, and the load-bearing sentence: "method calls
across module boundaries are direct calls with the callee's state pointer
[...] cross-module inlining is an optimization-pass decision, not a
translation-unit boundary."

The schedule already factors that way -- 67 module types for 56,953
instances, cut into `Segment`s at interface-method positions
(`schedule.rs:1-29`).  Codegen did not: `value_call` inlined each
child's method result cone recursively at every call site, which is where the
66.8M function came from, and which injects *foreign absolute* slots into a
parent body and so defeats the per-module-type code sharing that exec dedup
already provides.  The mechanism and the thing that broke it were the same
line.

So none of this is new architecture.  It is closing the gap between DESIGN.md
5.2 and `lower.rs`.

## 3. What has landed

| | state |
| --- | --- |
| De-inline cross-fragment method calls into per-module fns | **done**, 376/376, none left inline |
| One emission strategy: split by module type | **done**; largest fn 66.8M -> 2.09M, total instructions -33% |
| Split the LLVM compile out of the link | **done**; codegen knobs are argv, so a build system can key on them |
| BVI contract + `VPathInfo` carried into BIR | **done**, as `InstanceKind::Bvi` / `BviContract::paths` |
| A prim backed by a Verilated model | **done**; `INTERP_PANIC` 33 -> 1, PASS +8, no regressions over 2291 designs |

Outlining shares cones that inlining duplicated at every call site, which is
why the total instruction count went DOWN.  Correctness is settled: the
outlined artifact's output is byte-identical to baseline.

**The leaf seam's settle contract**, as implemented:

- **Calls write shadows and do not evaluate.**  An Action or ActionValue call
  updates a shadow copy of the argument and enable ports.
- **Reads are observation frontiers.**  A value read, an RDY read or an AV
  result read publishes every dirty shadow, runs ONE `eval()`, then reads.
  This is what makes combinational boundary paths work -- the eval happens
  exactly where the path's effect is visible.
- **The tick is a three-phase commit**, once per timeslice per instance:
  publish the final non-clock vector and eval; apply ALL coincident clock
  levels in ONE eval, so simultaneous edges commit together; clear the fired
  enables and eval.  Splitting phase two would let one clock's effect be
  visible to another in the same timeslice.

`VPathInfo` is therefore an optimisation hint, not a gate -- a path-empty
fragment can skip the frontier eval between ticks.  Every fragment is
importable; path-empty ones are cheaper.  Verilator needs nothing exotic
from us: plain `--cc --top-module <M>` emits a C++ class with public port
members and `eval()` for any module, combinational paths included.

## 4. What is left, part one: the fragment execution contract

An inlined cone has no substitution point; a direct call with the callee's
state pointer has one, and that pointer slot is where a model handle goes.
That point now exists.  What does not exist is the contract for using it.

TRS runs rule bodies sequentially against in-place state (DESIGN.md 2.2);
Verilator settles combinationally and commits at the edge.  The shadow-vector
model above bridges them for a LEAF.  For a fragment the open parts are:

- **Where foreign state lives.**  A Verilated object cannot sit in the arena,
  so region-relative addressing does not reach it; it needs a handle table
  indexed by instance.
- **Per-INSTANCE realization vs per-TYPE code.**  Substitution is chosen per
  instance while the code running a fragment is shared per module type.
  Those reconcile only if everything instance-specific is PASSED rather than
  baked (see section 6).
- **Composition-level facts for a fragment with no rules.**  `cross_inhibits`
  are qualified (parent rule, child rule) pairs precisely because they do not
  factor by type (`schedule.rs:22-29`); cross-instance tick ordering is
  global by construction.  Position: facts derived from a completed schedule
  belong to the per-link composition, not the per-type contract.  A rule-less
  fragment contributes none; the parent's side survives as an ordinary
  inhibitor on the parent's own rule.
- **Port-level ordering.**  A combinational path in one port and out another
  constrains PORTS, not rules, and the composition orders rules.  Two
  fragments with paths crossing both ways settle in Verilog and deadlock a
  static total order; bsc has never analysed a loop *through* two fragments.
- **ActionValue methods with caller-latched args** (`lower.rs:4021-4026`)
  already record an interp/compiled asymmetry.  A call boundary must define
  this rather than inherit it.

## 5. What is left, part two: the compilation unit

`trs compile` redoes work.  Per-type objects are emitted from the ASSEMBLED
whole-design IR, so a type two targets both instantiate is lowered,
optimized and codegen'd once per target.  Move the unit of compilation to the
fragment: `.ba` -> `.bir` + `.o`, the link aggregating objects it did not
build.

Most of the mechanism is there.  Emission is already per module type; the
compile is already a separate argv-keyed action; cross-boundary inlining is
gone.  And the enabling invariant already holds -- exec fns take `(arena,
env, region base index, ordinal)` and address in-region state as
`base + (slot - region.0)` (`lower.rs:5504`, `lower.rs:2407`), so per-type
code is position-independent.  `inst_sig` (`jit.rs:4882`) is a cache key in
all but name.

Five hurdles:

1. **Reset slots are the one absolute address.**  Every map in `inst_sig` is
   hashed region-relative (`b - r0`) except `reset_slot`.  A fragment
   compiled standalone would bake the wrong node.  Small, and the concrete
   blocker to position-independence.
2. **The cache key does not exist.**  `Module::content_hash` is 32 zero bytes
   from the exporter (`SimExportIR.hs:550`, `P0 TODO`), ignored by the link
   (`link.rs:544`).  `inst_sig` is the right CONTENT but is computed
   post-link and never persisted.  An export-time equivalent must cover
   children's signatures recursively, the parameter valuation, the codegen
   knobs and the LLVM version.
3. **Parameter specialization multiplies the unit.**  `port_consts` and
   friends are per-instance and hashed into the signature, so the cacheable
   unit is (module type, parameter valuation).  `trs-vlt`'s `run_identity` is
   the in-tree precedent.
4. **Layout comes from a whole-design walk** -- "subtree extents (known only
   after the whole subtree walked)" (`jit.rs:4851`).  The composability a
   cache depends on holds today, but is DERIVED rather than contracted.  It
   needs to become a stated and checked property.
5. **Two design-wide pre-passes.**  `compile_design_objects_split`
   (`lower.rs:1124`) realizes the boundary map on a throwaway module first,
   and eligibility is all-or-nothing -- `trial_lower` (`lower.rs:98`) returns
   one Result for the whole spec list, so one ineligible rule turns AOT off
   design-wide.

**First measurement, needing no caching**: compile a type into two different
enclosing designs and check the objects are byte-identical.  That tests
hurdles 1, 3 and 4 at once.  Then the number that decides whether the
contract work in 4 is worth it: what fraction of a real `trs compile` goes to
types that recur across targets.

## 6. Two lessons that keep recurring

**Portless is not absent.**  A portless clock or reset is an association --
it places a module's methods in a domain without wiring a port, which is how
combinational IP declares its methods belong to the caller's domain.  Reading
it as absence cost a silent wrong answer once (reset ordinals desyncing, so a
ported reset was never driven) and an over-broad refusal once (every
combinational import whose inputs arrive as always_enabled Actions).  A third
instance will look the same: a `Maybe` filtered out, then reasoned about as
though the filtered thing never existed.

**Share code by type; materialise instance-specific data into per-ordinal
tables at plan time.**  Two designs died on the other side of this: absolute
instance indices baked into a shared call-site table (wrong for every twin),
and a runtime `self_inst` argument added to carry identity a per-ordinal
table already distinguishes.  What survived resolves identity at plan time,
with no ABI change and no new runtime table.  Check any new per-type artifact
against this before building it.

## 7. The one number still missing

What de-inlining costs at RUN time.  Outlining is now unconditional and
correctness is settled, so the experiment is purely a cost question: an A/B
on simulation RATE -- not link time -- for a design whose worst function is a
replicated child cone.  Everything above trades link time for run time and
nobody has measured the other side.
