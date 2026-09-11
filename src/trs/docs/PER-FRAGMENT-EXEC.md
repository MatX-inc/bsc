# Per-fragment execution: scaling, and linking external Verilog

Status: the leaf seam is done -- external Verilog runs as a prim.  The
compilation unit is done: a compiled object is one CLASS -- a module type
at one parameter valuation -- it carries a content key, and it does not
depend on the design it was compiled in.  Two things remain.  A fragment
still has no EXECUTION contract, so a Verilated model cannot yet stand
where a Bluespec fragment does (section 4).  And a build graph cannot yet
name these objects, because a fragment's valuation is not known until its
parent elaborates (section 5, hurdle 7) -- which is where this meets the
build rules rather than the compiler.

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
| A per-class object that does not depend on its design | **done**; byte-identical across enclosing designs (section 5) |

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
- **ActionValue methods with caller-latched args** (`lower.rs:4158-4163`)
  already record an interp/compiled asymmetry.  A call boundary must define
  this rather than inherit it.

## 5. What is left, part two: the compilation unit

`trs compile` redoes work.  Per-type objects are emitted from the ASSEMBLED
whole-design IR, so a type two targets both instantiate is lowered,
optimized and codegen'd once per target.  Move the unit of compilation to the
fragment: `.ba` -> `.bir` + `.o`, the link aggregating objects it did not
build.

### Measured: the reuse is real, and the `.bir` layer already takes it

One build tree, 355 designs: **7,271 (design, fragment) pairs over 1,099
distinct fragments -- 6.6x sharing**, and 86% by bytes (5.12 GB staged, 0.71
GB distinct).  Two fragments appear in 143 designs each.

But the export layer already exploits that.  A `.bir` is built once per `.ba`
and attached to the library rather than to the design -- a fragment's export
reads no other file, because bsc's elaboration stops at a synthesis boundary
and never reads a child's `.ba` -- and the link stages symlinks to those
shared files by name.

**The whole 6.6x is unclaimed at the object layer and only there.**  The
compile action takes the ASSEMBLED whole-design `.bir` as its input, so its
key is per-design and every design recompiles every fragment it contains.
The structural precedent for fixing that already exists one layer up, in the
export.

### It should be an action, not a cache

Under a build system that keys actions on their inputs, a content-addressed
side cache is the same mechanism one level down, and the outer one decides
whether the inner one is consulted at all.  Worse, a side cache is either
invisible to the sandbox (so the work is redone anyway) or reachable as an
undeclared input, where a stale entry yields a wrong object filed under a
key that looks right.

So `content_hash` should be a key the build system can compute -- inputs the
fragment's `.bir` plus its children's, argv the codegen knobs and the LLVM
version, output the `.o` -- not a cache `trs` consults.  A local cache still
earns its place for direct command-line use, placed by the caller the way
`TRS_VLT_CACHE` already is.

### The unit is the class, not the type

Emission grouped a type's classes into one object.  Each BODY in it was
design-independent, but the object was not: it depended on the SET of
valuations this design happened to use, so two designs sharing one
valuation emitted the same symbols and the same code into objects that
differed, and neither could be reused for the other.

Measured on two designs, one using a type at k=3 and the other at k=3 and
k=7: the shared body was byte-identical and the object was not.  Splitting
emission per class makes the shared one shareable --

    mkP_4d920a930e1e57e3.o   IDENTICAL across both designs
    mkP_4d6ff54c38a6df17.o   only in the design that uses k=7

Helpers are declared per module type, but only for a type whose instances
all share one signature, so a helper's type has exactly one class and lands
in it unambiguously.

The cost is more, smaller objects, and it is measured rather than
estimated.  Compiling a controller design of 536 fragments:

| | |
| --- | ---: |
| distinct module types | 47 |
| distinct classes | 194 |
| objects emitted (design + classes) | 195 |
| boundary method fns, all realized | 1,481 |
| parallel pipelines + emit | 35.0s |
| whole compile | 329s |
| artifact | 60 MB |

**4.1x the objects** a per-type emission would produce, and the emit phase
is 35s of a 329s compile -- so at this scale the extra objects cost
nothing measurable, and each one is independently reusable.  The
multiplicity is as skewed here as everywhere else: 27 of the 47 types have
a single class, and three types account for 99 of the 194.

Worth recording alongside: this compile is 329 SECONDS.  The 6.6 hours in
section 1 is what the monolithic strategy cost on a design of this family,
and the figure has been quoted since; per-type emission already retired it,
and per-class does not bring it back.

### Why this is possible at all

Most of the mechanism is already there.  Emission is per class; the
compile is a separate argv-keyed action; cross-boundary inlining is gone.
And the enabling invariant holds: exec fns take `(arena, env, region base
index, ordinal)` and address in-region state as `base + (slot - region.0)`
(`lower.rs:5641`, `lower.rs:2473`), so per-type code is already
position-independent -- exec dedup would be unsound otherwise.  `inst_sig`
(`jit.rs:4947`) is the key in all but name; what it is not is persisted.

### Hurdles

1. ~~**Reset slots are the one absolute address.**~~  **Done**, and not by
   the arithmetic the name suggests.  A reset node is DESIGN-GLOBAL: it is
   allocated ahead of every region, so there is no region-relative form of
   its address to convert to, and what a fragment's code depends on is which
   of the design's nodes its port happens to be wired to.  Each instance's
   region now opens with a reset table -- one word per reset port, holding
   the slot that drives it -- and shared-by-type code loads from there.  One
   extra GEP per exec invocation, emitted into the entry block; at a measured
   1.02 reset ports per fragment that is the whole cost.
2. ~~**The exec symbol names a position in the design.**~~  **Done.**  An
   exec fn was `exec_i{inst}_{ordinal}`, so the same code came out under a
   different symbol in every design.  It is now named for its class: module
   type, rule, and subtree signature.  Fixing that exposed the signature
   itself describing the design rather than the type, twice -- see section 6.
3. ~~**The key does not exist.**~~  **Done**, and not at export.  The
   exporter cannot build one: it reads one `.ba` and by design never reads
   a child's, so it knows neither the children's hashes nor the parameter
   valuation -- and a file cannot carry its own digest.  The LINK can, and
   it is the last place that can, since an assembled design has only
   modules.  So a decode records the SHA-256 of the file it read and
   assembly copies it into `Module::content_hash`, where the compile finds
   it.  `inst_sig` folds it in, which is what makes the key cover what a
   module SAYS and not only its name and shape.
   The other half of the old wording -- codegen knobs and the LLVM version
   -- is not ours.  Those are the ACTION's identity, not the fragment's,
   and the compile already takes its knobs on argv for exactly that reason.
   A key that tried to carry them would be describing the toolchain in a
   field that describes the design.
4. **Parameter specialization multiplies the unit** -- measured against
   `inst_sig` at 3.42x, and it is what breaks cross-target reuse rather
   than merely multiplying the count.  See below.
5. ~~**Layout comes from a whole-design walk**~~ -- "subtree extents (known
   only after the whole subtree walked)" (`jit.rs:4916`) -- **now stated and
   checked**.  The signature splits in two.  INPUT is what a fragment IS:
   its type, what it says, the parameters and bound gates it was
   instantiated with, and its children's inputs; these legitimately differ
   between instances of one type.  LAYOUT is where everything sits,
   region-relative.  The contract is that **layout is a function of input**
   -- a fragment laid out differently because of where it sits could not be
   compiled once and reused -- and a violation is reported by name.  Clean
   over 700 designs.  What remains derived is the guarantee itself: the
   check catches a break, it does not prevent one.
6. ~~**A design-wide pre-pass.**~~  **Done**, and it was not design-wide in
   substance.  The boundary map -- "the eligibility/width decisions every
   module lowers against" -- was realized on one throwaway module over
   every request in the design.  But a request is realized with the map
   UNSET, so its cones inline their callees rather than diverting, and
   nothing it produces depends on any other request's outcome.  The
   batching was convenience.  Realization is now per CLASS, which is what
   a fragment compiled on its own would do with the classes beneath it,
   and runs on the workers rather than serially ahead of them.  Proof that
   nothing was lost: the emitted objects are byte-identical to the
   design-wide realization's.  The parallelism is NOT the point and is not
   a win worth claiming: realizing 193 classes takes 396ms on one worker
   and 45-79ms on eight, against a 329s compile -- a tenth of a percent.
   The structural change is the whole of it.
   The other half -- all-or-nothing eligibility, where `trial_lower`
   (`lower.rs:98`) returns one Result for the whole spec list, so one
   ineligible rule turns AOT off design-wide -- was listed here as an equal
   concern and is **measured at zero**: 502 of 502 corpus designs that
   linked produced an object.  Small designs have fewer chances to contain
   an exotic rule, so this is a floor rather than a verdict.
7. **The valuation is not known when the graph is built.**  A build system
   needs its outputs declared before any action runs, and a fragment's
   parameter valuation comes from its parent's elaboration, not from its own
   `.ba`.  Discovering valuations inside an action forces the whole design
   into that action's inputs, which destroys the reuse -- the discovering
   action is a design-wide action wearing a per-fragment name.  So the
   valuations have to be settled before the graph is, which is what makes
   hurdle 4's distribution the load-bearing measurement.

### What specialization costs, measured against `inst_sig`

The earlier numbers used `Instance::args` as a proxy and warned it was a
bound from below.  Measured against `inst_sig` itself -- `TRS_SIG_DUMP`
writes one line per instance and declines, so this costs a second per
design rather than the hours a compile costs -- on six real designs that
share 593 fragments:

| | types | objects | builds | reuse |
| --- | ---: | ---: | ---: | ---: |
| specialized (type + valuation) | 207 | 707 | 941 | **1.33x** |
| generic (type only) | 207 | 207 | 380 | **1.84x** |
| types with 1 valuation | 109 | 109 | 242 | 2.22x |
| types with >1 valuation | 98 | 598 | 699 | **1.17x** |

Multiplicity is 3.42 valuations per type, against 3.04 from the proxy --
the bound held, and the distribution is the same shape: 53% of types have
one valuation, 79% have at most two, and one type has 102.

**This revises the policy, and not in the direction the count distribution
suggested.**  Counting types, specialization looks cheap: half of them have
a single valuation, so specializing costs nothing.  Counting OBJECTS it is
the dominant cost -- the 98 multi-valuation types produce 598 of the 707
objects, and those objects barely share at all (1.17x, against 2.22x for
the single-valuation ones).  Specialization is what breaks cross-target
reuse, because parameters are exactly what differs between targets.

The like-for-like figure is **1.84x generic against 1.33x specialized: a
quarter of the available sharing, spent on folding constants in 98 types**.
Whether that is a good trade is the run-rate question in section 7, now
asked about a specific 98 types rather than in general.  For the other 109
the question does not arise.

What this sample cannot say is the absolute rate.  Six designs cap reuse at
6x, and these six are controller variants -- closely related, so both more
likely to share fragments and more likely to differ in the parameters they
share them at.  The 6.6x quoted earlier is the `.bir` layer over 355
designs and is not comparable; the same type-level measure over these six
is the 1.84x above.  Re-run `TRS_SIG_DUMP` over a wider, less related set
before trusting any absolute number.

Whichever way the trade goes, the generic path is real work: those
constants are folded today (`port_consts` is "the compiled mirror of the
interpreter's Port/Param fallthrough", `jit.rs:4825`), so unbaking them
turns folds into loads and gives up the downstream branch elimination a
width or a mode selector buys.

### The first measurement, taken

Compile one type into two different enclosing designs and diff the objects.
It needs no key and no caching, and it either confirms the addressing is
design-independent or names what is not.  `TRS_TYPE_OBJ_DIR=<dir>` writes
each class module out as `.o` and `.ll`, named `<module>_<class signature>`
-- a mir is a position in one design's module list and a class id an index
over its instances, so neither would line up in the next design.

**It passes**: a leaf that reads its reset and is wired to a different reset
node in each design, and a nested fragment calling across a synthesis
boundary, both come out byte-identical.

It took three fixes, not the one predicted, and the order in which they
surfaced is the point: each was invisible until the one before it was
cleared.  A first attempt passed for the wrong reason -- its fragments never
read a reset, which 73.6% of real fragments do, because bsc gates a
`$display` on the reset wire.  Any regression test for this has to contain
a fragment that reads one.

## 6. Four lessons that keep recurring

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

**An identity that is really a position.**  Three times now, something that
read as a stable name turned out to be an index into one design's table.
`mir` is a position in the module list.  `StrId` is a position in the string
table -- and `inst_sig` was keyed by StrId in all twenty-odd of its
components, values and recursive child names included, so two designs
disagreed about a type they had both compiled identically.  The exec symbol
itself was an instance index and a schedule ordinal.  Each looked like an
identity at the use site and was only a coordinate.  The test is whether the
thing survives being carried to another design: if it does not, hash or emit
the NAME.  `TRS_SIG_TRACE=<module>` exists for this -- it dumps the running
signature per component, so two designs that should agree can be diffed to
the first component that does not.

**A contract nothing enforces is a guess.**  `inst_sig`'s own comment says
it "must cover EVERY input the exec lowering reads".  It did not:
`bypass_slot` is read by the lowering and was never hashed.  Nothing had
gone wrong, because a type's BypassWire children and their allocation order
come from the module, so the slots agreed whenever the rest did -- the
omission was latent, waiting for the first thing that made it not agree.
It was found by enumerating `InstEnv`'s fields and grepping the consumer for
each, which took a minute and is worth repeating whenever the sig grows a
consumer.  The lesson is not about that field: a stated invariant with no
check is a comment, and this one now has both a check (hurdle 5) and an
audit that can be re-run.

## 7. The one number still missing

What de-inlining costs at RUN time.  Outlining is now unconditional and
correctness is settled, so the experiment is purely a cost question: an A/B
on simulation RATE -- not link time -- for a design whose worst function is a
replicated child cone.  Everything above trades link time for run time and
nobody has measured the other side.
