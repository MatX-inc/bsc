# Per-fragment execution: scaling, and linking external Verilog

Status: exploration.  No code written.  Findings are cited to file:line or
to a measurement; open questions are marked as such.

*Fragment* here means a synthesis boundary: a synthesized module plus every
instance beneath it that is not itself a synthesis boundary.  In BIR that is
exactly a `Module` (bsc exports one per `.ba`), and `InstanceKind::Module`
names a nested fragment.

## 1. Why

Two motivators, and the second constrains the design more than the first.

1. **Scaling.**  The global-schedule model produces single functions large
   enough to dominate a link.  One rule in a MatX design emits 66.8M
   LLVM instructions -- 64% of the design's whole IR -- and its chunk cannot
   be split, because the chunker splits at function granularity.  That link
   takes 6.6 hours, 61% of it in two per-function codegen passes.
2. **External Verilog.**  We want to import Verilog modules compiled by
   Verilator and drive Verilator's interface from inside TRS execution.  A
   Verilated fragment has no rules, no segments and no visible schedule --
   only ports and `eval()`.  Any final design must let such a fragment sit
   where a Bluespec fragment sits.

The near-term scaling work is worth doing on its own, but it must not
foreclose (2).  Section 5 is the test each step has to pass.

## 2. The schedule already factors; codegen does not

`crates/trs-ir/src/schedule.rs:1-29` states the factoring: a module's
internal order is cut into `Segment`s at its interface-method positions, and
`Design::compositions` interleaves `(instance, segment)` references.  Flat
export would be "the monolithic-schedule problem reborn in the wire format".

Measured on one MatX design's `.bir`:

| | count |
| --- | ---: |
| module types (fragments) | 67 |
| instances | 56,953 |
| rules, per type | 7,767 |
| methods, per type | 920 |
| segments, per type | 15,639 |
| compositions | 1 |
| composition entries | 47,744 |

67 types for 56,953 instances.  The schedule scales.  `Module::content_hash`
(`lib.rs:240`) exists for a per-module object cache, so the data model
already anticipates separate compilation per fragment.

**DESIGN.md 5.2 already specifies the codegen to match**: `seg_<Mod>_<domain>_
<edge>_<k>(state*)` per segment per module type, a composition driver of
(instance, segment) calls, per-module LLVM modules keyed by
`(module, options, BIR hash)`, and -- the load-bearing sentence --

> method calls across module boundaries are direct calls with the callee's
> state pointer [...] cross-module inlining is an optimization-pass
> decision, not a translation-unit boundary.

The implementation does the opposite.  `lower.rs:4277`:

```
// user-module child: inline the method's result cone
```

`value_call` builds a `child_frame` and inlines the child's method result
cone, recursively, at every call site.  That is where the 66.8M function
comes from: one rule absorbing the cone of the 1,675 instances beneath its
fragment -- a replicated datapath, so the same sub-fragment many times over.

The second-order effect is worse than the size.  From the dedup key
(`jit.rs:3658`):

> own-region slots are region-relative in codegen (twins share safely),
> foreign-instance slots are absolute (twins must not share)

and `jit.rs:4434`: "twin instances of one module type get identical
region-relative layouts (code dedup)".  Per-module-type code sharing already
exists -- exec bodies take a region base as an argument.  Inlining a child's
cone is precisely what injects *foreign absolute* slots into a parent body
and defeats that sharing.  The mechanism and the thing that breaks it are the
same line.

So this is not a new architecture.  It is closing the gap between DESIGN.md
5.2 and `lower.rs`.

## 3. What inlining buys, and why it cannot simply be deleted

Inlining exists for simulation speed: a value-method read becomes a few
instructions in the caller instead of a call, and the cone is exposed to CSE
against the caller's own logic.  De-inlining trades link time for run time.

The right shape is a policy, not a flat rule -- and the codebase already has
one for the analogous case.  `EDGE_INSN_BUDGET = 16_000` with
`EmitFail::EdgeOverBudget` (`jit.rs:1000-1011`) measures oversized inlined
sections in an edge function, extends the plan's outlined set and re-emits.
Exec bodies are simply not covered by it: the 66.8M function is 4,175x the
budget an edge function would get.

Note DESIGN.md's intent is that **LLVM** decides inlining, given direct calls
and one LLVM program at link.  The current front-end inlining removes that
choice, and then hands LLVM a function too large for it to handle well.

## 4. The Verilator constraint

Bluesim cannot simulate imported Verilog at all, so there is no existing
semantics to inherit.  This is new ground and needs a defined contract.

What a Verilated fragment is, concretely: an opaque handle; input ports you
write; `eval()` which settles the module internally; output ports you read.
It has no `Rule`s, no `Segment`s, no `CAN_FIRE`/`WILL_FIRE`, and no method
result cone that could be inlined even in principle.

### BIR's prims ARE BVI imports, and that is the seam

bsc's primitives are not a separate species from imported Verilog.  `mkReg`
is declared in `Libraries/Base1/Prelude.bs:1960` as

    module verilog "RegN" (("width",valueOf n), ("init",v)) "CLK" "RST" {
        read  = "Q_OUT"{reg};
        write = "D_IN"{reg} "EN";
    } [ read <> read,
        read < write,
        write << write ]

-- a BVI import in Classic syntax: Verilog module name, parameters, clock,
reset, method-to-port map, schedule annotations.  `src/Verilog/RegN.v` is the
module.  Bluesim and TRS merely substitute native implementations for a known
subset (`SimPrimitiveModules.hs:263-348`).

So the mechanism for "an instance whose behaviour lives outside the compiled
code, called by method, clocked by tick, one object per instance" is already
built and working:

- `PrimCb(env, token, args, out)` is a BVI method call.
- `PrimCallSpec {inst, method, port, arg_widths, ret_width, is_action}`
  (`abi.rs:90-102`) is a BVI method signature.
- `Instance::args` are the BVI parameters, `method_order` the sSB relation,
  `port_counts` the multi-ported methods (`lib.rs:301-312`).
- Ticks are the clock edge (DESIGN.md 2.2 step 3).
- `Primitive::Other { name }` is already "escape hatch during bring-up:
  named primitive handled by trs-rt".

**What BIR discards is the contract, not the concept.**  Known prims get a
typed variant (`Reg { width, reset }`, `Fifo { .. }`) because the backend
knows RegN's ports; `Other` keeps only a name.  A Verilated module needs the
BVI contract carried: method-to-port map, port widths and directions,
clock/reset ports, and combinational in-to-out paths.  That is a field
addition on an existing kind, not a new `InstanceKind`.

One simplification falls out: the schedule annotations above are consumed by
bsc *before* the `.ba` -- conflicts are already baked into `WILL_FIRE`.  TRS
never needs the conflict matrix at run time, only the port protocol to drive.

`Design::foreign_funcs` stays irrelevant: BDPI is C functions
(`lib.rs:217`, `443-455`), not modules with state and a clock.

### Consequence: two seams, not one

External Verilog *leaves* ride the prim path, which exists.  Bluespec
fragment boundaries are a different seam, and the per-fragment exec work is
mostly independent of Verilog import rather than a prerequisite for it.

They converge for the ambitious case.  BOUNDARY-CONTRACT.md calls a
synthesized module's contract "the auto-derived BVI-equivalent of a
synthesized module", so if a fragment can be *presented* as a prim, one
mechanism serves both importing foreign Verilog and substituting a Verilated
copy of a Bluespec fragment.

An earlier draft of this note proposed adding `InstanceKind::Foreign` and
treating the fragment method boundary as the substitution point.  Both were
wrong: it invented a kind that duplicates `Prim`, and it put the seam
somewhere external Verilog does not live.

The hard part is not the data model; it is the execution model.  TRS runs
rule bodies **sequentially against in-place state** (DESIGN.md 2.2), so each
method call happens at a point in a total order.  Verilator settles
combinationally and commits at the edge.  Bridging them raises questions
that the rule-level composition cannot answer:

- A value-method read from a foreign fragment requires its inputs already
  driven and `eval()` already run *at that point in the order*.  An action
  method written later requires another `eval()`.  Naively that is one
  `eval()` per method call.
- A combinational path in through one port and out through another means the
  ordering constraint is between **ports**, not between rules.  The
  composition orders rules; it has nothing to say here.
- Two fragments with combinational paths crossing in both directions settle
  in Verilog and deadlock a static total order.  bsc forbids combinational
  loops within a fragment, but a loop *through* two foreign fragments is not
  something bsc has analysed.

## 4a. What already exists, and what it does not cover

De-inlining is **implemented and now unconditional** (commit "trs: outline a
method cone that makes callbacks").  This section records where it started and
what the limits turned out to be; the resolution is below.

As found, it was partly implemented.  `lower_boundary_fns` (`lower.rs:936`)
emits standalone per-`(mir, method, kind)` functions with symbol
`trs_bnd<mir>_<mi>_<kind>`, and call sites consult them through
`boundary_hit` (`lower.rs:5970`).  Two gates select it (`jit.rs:1178-1191`):
`TRS_BOUNDARY_MODULE=<name|mir>` for one module type, `TRS_JIT_SHARD=1` for
every instantiated type.  `rules.bzl:1841` documents
`--define=trs_env=TRS_JIT_SHARD=1` as the supported way to run it.

Three limits, measured on a MatX design:

- **Coverage was 62%** -- 235 of 376 boundary method fns realized, the other
  141 all reporting "callback sites in method cone".  A cone recording
  `foreign_stmts` or `prim_calls` was deleted and its call sites fell back to
  inlining, because its tokens would dangle from the sentinel spec.
  **Now 376 of 376, none left inline.**
- **`always_enabled` methods are excluded outright** (`jit.rs:1131-1140`),
  their rdy-gated call protocol differing.
- **Only one of the two emission paths has it.**  `TRS_AOT_ONE_MODULE`
  defaults to true; `compile_design_object` / `compile_design_objects_split`
  install the boundary map, `compile_object_chunk` (the
  `TRS_AOT_ONE_MODULE=0` fallback) does not.

Link time on that design was unchanged: 343.3s baseline vs 335.7s sharded.
That is not evidence against the approach -- that design's worst function is
446,212 instructions of which one basic block is 422,761, i.e. the rule's own
straight-line logic, not child method cones.  Outlining cannot move it.  Both
paths produce that same function, so the shape is not a path artifact.

For reference, the two paths on that design: one-module 172.1MB IR / 1 LLVM
module / 33,224 fns / 343s; chunked 166.5MB / 60 modules / 69,762 fns / 65s.

### Why the callback bail was a blocker, not just a coverage gap

For scaling, "falls back to inline" merely limited the win.  For Verilog it
was fatal: since a foreign module is reached through the prim trampoline,
**every call into external Verilog is itself a callback site**, so any
fragment containing an imported Verilog module was exactly the kind that
refused to outline.

### How it was resolved: the caller's table

A shared body cannot own a call-site table -- entries name absolute instances,
so one table would name the exemplar and be wrong for every twin.  But the
table it needs already exists: **its caller's**.  Those are per ordinal and
looked up at runtime, which is why a rule body containing callbacks dedups
across twins today.

So a caller reserves a block in each of its two tables (prim and foreign have
independent local index spaces), materialises the callee's sites into them
with the child's absolute instance, and hands the callee one token base per
table -- its own, offset to the block.  A site inside the callee adds its
index and lands in the caller's table.  `BoundaryFn` carries those sites as
templates with `inst` a delta from the fragment instance; **nothing resolves a
delta at run time.**

The trampoline, the token layout, the artifact format and `encode_protos` are
untouched, and no runtime table is added.  `site_token` gains the one
distinction that matters: a rule body's base is `ordinal << 17`, whose local
field is zero, so one OR still places index and kind bit; a boundary fn's base
already carries an offset, so the index is added.  The only cone still
declined is one whose site names an instance outside the fragment's subtree.

The scheme rests on every member of a dedup class laying its tables out
identically, since block offsets bake into the one shared body.  Membership
already implies same module type, same rule, same region-relative slots and
same baked params (`inst_sig`), so a divergence means the dedup key stopped
covering something the lowering reads -- a compiler bug.  The check therefore
**panics at link time and never falls back**: a fallback would turn the bug
into unexplained slowness, the worst failure mode for something whose whole
purpose is sharing.  Verified by forcing a skew.

Measured: 376 of 376 realized, the outlined artifact runs, and its output is
byte-identical to the baseline over 1543 lines -- which is what exercises the
token arithmetic.  The no-boundary path is unchanged (69,762 fns, zero
differing rows).  Link time 335.7s -> 356.6s on that design, which says
nothing either way, for the shape reason above.

### What Verilog does instead

bsc's Verilog backend has no equivalent problem, and how it avoids one is the
guide.  A foreign call becomes a DPI name or `$imported_<name>` emitted
**lexically inside the module's own always block** (`AVerilogUtil.hs:1233-1236`;
negedge-triggered so values are ready at posedge, `AVerilogUtil.hs:191-201`).
Boxed prims are library modules -- `FIFO2.v`, `RegFile.v`, `BRAM1.v` --
instantiated as `VMInst` submodules, i.e. a FIFO gets exactly the treatment a
synthesis boundary gets.

So Verilog never needs call-site identity: the module's text is emitted once
per module *type* and the simulator instantiates it per instance, with
identity carried by position in the instantiation hierarchy rather than by
anything the code holds.  There is no token because nothing needs one.

### Why this is harder in TRS, and what the fix actually is

TRS's compiled body is also per module type, but it must be *told* which
instance it is.  Today a callback answers that with `PrimCallSpec::inst` /
`ForeignSpec::inst` -- **global absolute instance indices** (`abi.rs:90-102`,
`286-293`).  That is the same absolute-vs-region-relative split that defeats
exec dedup, one level down.

A first sketch of the fix -- give each boundary fn a **pseudo-ordinal** past
the real ordinals so it owns its own `protos` entry -- is NOT sufficient, and
is worse than the bail it would replace.  The table's `inst` fields would be
baked to the request's exemplar instance and silently wrong for every other
instance of the type: incorrect simulation on exactly the twinned fragments
this work exists to serve.

A first answer -- make callback target identity **runtime**-relative to the
caller's fragment, resolved as `self_inst + delta` inside the trampoline --
was built and then discarded.  It needed a `self_inst` argument on both the
callback and exec ABIs (`PrimCb` is `fn(env, token, args, out)`; the token is
all that crosses today), a layout-rev bump, and new runtime table storage.
The caller's-table design above does the same job with no ABI change, no rev
bump and no new table, because the delta never has to survive to run time.
Recorded because the reasoning generalises: prefer resolving identity at plan
time, where a per-ordinal table already distinguishes twins.

What the discarded attempt did establish, and which the surviving design
relies on, is that **instance numbering is type-uniform**: two twin fragment
instances each have 1,675-instance subtrees with identical index deltas
(contiguous 0..1674), identical relative names, and identical module types in
order.  That is what makes a delta from the fragment instance meaningful in a
template.  It holds by derivation rather than by contract -- instance ids are
the index into `M.toList` of a map keyed by dotted path (`SimExportIR.hs:175`),
i.e. lexicographic, and `'.'` (46) sorts below every identifier character
(digits 48, `A` 65, `_` 95, `a` 97), so a subtree is contiguous -- which is
why the materialisation computes deltas from the real table rather than
assuming them, and why the class shape check panics.

(Unrelated but noted while reading: `PrimCb`'s doc comment says
`Token = rule ordinal << 16 | local`, while `abi.rs:331` puts the exec flag at
bit 16 and the ordinal at 17+.  The comment is stale.)

## 4b. Prior art: Ravi's Verilator stack (PRs #41-#46, all open)

Six stacked PRs on `MatX-inc/bsc`, 20 commits, 19 by Ravi.  **In none of
`origin/main`, the trs line, or upstream** -- #41's base is `upstream-main`, so
the stack is aimed there and has not landed.  `verilator/6-link-tools` carries
the whole stack rebased; `verilator/1`-`5` are stale pre-rebase copies, so diff
against 6.  (`ci/verilator-shard`, PR #115, is CI-side and separate.  The
Verilator mentions inside the trs stack -- #131, #135, #147-#149 -- are
benchmark baselines, not import work.)

What is relevant to us, and what is not:

- **`74ad684d`, hierarchical verilation.**  A module with **no combinational
  input-to-output paths** compiles as a Verilator *hier_block*, a separate
  compilation unit, "without creating false combinational loops at the
  boundary".  `BSC_VSIM_HIER=1` marks every eligible module; `-check-only`
  writes a manifest of eligibility, sources, `-y` dirs, defines, needs-timing,
  DPI objects and link options, "for build systems that drive the simulator
  toolchain themselves".
  **But this is not our substrate.**  `hier_block` solves the problem of
  Verilator composing a block inside a larger verilation *of its own*, which we
  do not have -- TRS does the composing.  An earlier draft of this note had it
  as the mechanism a Verilated fragment would sit on; that was wrong.
- **`f01a06f1` / `2322cb3f`, polymorphic BDPI over the DPI path.**  Bears on
  the two-runtimes wrinkle section 4 raises for the fragment case.

### Measured: what Verilator will actually emit for us

Verilator 5.032, two one-module probes -- `NoComb` (registered output) and
`HasComb` (an `assign y = d ^ 8'hA5` input-to-output path, i.e. hier-INeligible):

- **Any module verilates standalone.**  `--cc --top-module <M>` succeeded on
  both, producing a C++ class with public port members and `eval()`.  There is
  **no hier-block-only corner**: eligibility never enters into verilating a
  module as its own top.
- **The drive/eval/read loop works on the ineligible one.**  Driving ports from
  C++: `d=0x0F -> y=0xAA`, `d=0x33 -> y=0x96` with no clock edge (re-evaluating
  settles the comb path again), `q` unchanged pre-edge and `0x55` after
  toggling `clk` and re-evaluating.  That is exactly the contract section 4
  posits for a foreign instance, holding for a module Verilator would refuse as
  a hier_block.
- **`--lib-create` is a different integration style, not ours.**  It emits a
  `.sv` wrapper -- "Wrapper module for DPI protected library ... requires
  liblib<M>.a or .so" -- so another *Verilog simulator* can instantiate the
  compiled model over DPI.  It also ran on both modules, so it does not gate on
  eligibility either, but TRS is not a Verilog simulation and wants the C++
  class directly.

So the leaf mechanism needs nothing from the stack above: plain `--cc`, link
the generated C++, drive ports, call `eval()`.

### What this does to the settle contract (question 6)

The fact both hang on is `VPathInfo` -- `[(VName, VName)]`, the
combinationally-connected (input port, output port) pairs (`VModInfo.hs:141`)
-- and it is **already in the `.ba`** as `abmi_pathinfo` (`ABin.hs:64`).  The
exporter does not currently carry it (`SimExportIR.hs`: no mentions), so
putting it in BIR is an export change, not a bsc change.

`VPathInfo` matters, but as a SCHEDULING fact, not an eligibility one -- the
measurements above show Verilator will emit a model either way.  What it
decides is how often *we* must evaluate.

### A candidate settle contract: shadow vectors and observation frontiers

Section 4 worried that a value-method read from a foreign fragment needs its
inputs driven and `eval()` run *at that point in the order*, so "naively that
is one `eval()` per method call".  An earlier draft answered that by
restricting the first importable class to `VPathInfo`-empty fragments, whose
outputs are a function of pre-edge state and so need only one `eval()` per
edge.  That restriction turns out to be unnecessary.  The following model
handles both cases, and is offered as the design option to adopt:

- **Calls write shadows, and do not evaluate.**  An Action or ActionValue call
  updates a shadow copy of the instance's argument and enable ports.  No
  `eval()` runs.  A rule that calls three methods on a foreign instance costs
  three shadow writes.
- **Reads are observation frontiers.**  A value-method read, an RDY read, or an
  ActionValue result read is the only point where the model's outputs must be
  correct.  At such a read: publish every dirty shadow, run ONE `eval()`, then
  read the port.  Consecutive reads with no intervening write cost nothing
  extra.  This is what makes a fragment with combinational boundary paths work
  -- the eval happens exactly where an output is observed, which is the only
  place the path's effect is visible.
- **The tick is a three-phase commit.**  Once per timeslice, per instance:
  (a) publish the final non-clock vector -- args, enables, gates, reset levels
  -- and eval; (b) apply ALL coincident clock levels in ONE eval, so
  simultaneous edges commit together and non-blocking-assignment semantics
  hold; (c) clear the enables that fired, and eval.  Splitting (b) across
  several evals would let one clock's effect be visible to another in the same
  timeslice, which Verilog semantics forbid.

Under this model `VPathInfo` stops being a gate and becomes an optimisation
hint: for a path-empty fragment the frontier eval can be skipped entirely
between ticks, because no input can change an output before the edge.  Every
fragment is importable; the path-empty ones are merely cheaper.

Two consequences worth stating.  Startup needs its own step -- drive every
input, hold resets DEASSERTED, and run one unconditional eval so the model's
`initial` blocks execute, so that the kernel's first reset assertion arrives as
a real transition rather than as the initial value.  And the contract is only
as good as the declared paths: a module whose real combinational behaviour
exceeds what it declared will read stale outputs at a frontier.  That is a
checking problem, not a scheduling one -- an opt-in observe mode that re-reads
after later evals and reports a disagreement is the way to catch it, and the
runtime should otherwise trust the declaration.

## 5. The test each near-term step must pass

Section 4 splits the goal in two, so there are two tests, and a step can pass
one without touching the other.

> **Leaf test.**  Can an instance's behaviour be served from outside the
> compiled code -- called by method, clocked by tick, one object per
> instance?

Already true, via the prim trampoline.  What it needs is the BVI contract
carried in BIR and a trs-rt prim implementation backed by a Verilated model.
This is the path for importing external Verilog and it does NOT depend on the
per-fragment exec work.

> **Fragment test.**  Can a synthesis boundary's implementation be a
> Verilated model instead of compiled BIR?

Not yet.  An inlined cone has no substitution point; a direct call with the
callee's state pointer has one, and that pointer slot is where a model handle
goes.  This is the ambitious case -- verilating bsc's own generated Verilog
for a fragment and swapping it in -- and it is where per-fragment exec and
Verilog import converge, on BOUNDARY-CONTRACT.md's "auto-derived
BVI-equivalent of a synthesized module".

| step | leaf | fragment | state |
| --- | --- | --- | --- |
| De-inline cross-fragment method calls into per-module functions | no | **yes** -- creates the substitution point | **done**, 376/376 |
| Carry the BVI contract on `Primitive::Other` + a Verilated trs-rt prim | **required** | no | not started |
| Per-(module type, segment) functions + composition driver | no | **yes** -- a fragment's segment becomes `eval()` | not started |
| Per-fragment `.bir` + `content_hash` object cache | no | yes, orthogonal and compatible | upstream (see 4b) |
| Extending an inline budget to exec bodies | no | partly -- caps the damage, leaves inlining the default | not started |
| Port-level dependency ordering at fragment boundaries | required once a foreign leaf has combinational in-to-out paths | required | not started |
| Runtime callback identity (`self_inst`) | not needed (q7) | no longer needed | discarded |
| Carry `VPathInfo` through the exporter into BIR | **required** -- it sets the eval cadence | useful | `.ba` has it; exporter does not |

The fragment seam's substitution point now exists: every cross-fragment method
call can be a call.

The leaf seam no longer has an identity problem (question 7).  What it needs
is the CONTRACT and a runtime: `VPathInfo` and the port map carried into BIR,
and a trs-rt prim backed by a Verilated model.  Section 4b measures the
Verilator side: plain `--cc` on any module, drive ports, `eval()` -- nothing
needed from Ravi's stack, and no hier-block restriction.  The path-empty
fragments are the first importable class because of the eval cadence they
allow, not because Verilator requires it.

## 6. Open questions

1. ~~Are arena regions contiguous per fragment?~~  **Answered: yes.**
   `InstEnv::region` is the instance's *subtree extent*, assigned from the
   walk's `(s0, s1)` after the subtree is known (`jit.rs:5104-5108`).  One
   base pointer per fragment addresses the fragment and everything beneath
   it, which is what makes a shared per-type function possible.
2. **`cross_inhibits`** are qualified (parent rule, child rule) pairs living
   at composition level *because they do not factor by type*
   (`schedule.rs:23-27`).  DESIGN.md 5.2 says they "become per-instance
   inhibit inputs".  What is the equivalent for a fragment with no rules?
   Position: facts derived from a completed schedule -- rule liveness,
   arbitration, enable folding, these inhibitors -- belong to the per-link
   composition and should be keyed by it, NOT pushed into the per-type
   contract.  A fragment with no rules contributes none of them; the parent's
   side of the pair survives as an ordinary inhibitor on the parent's own
   rule.  That keeps the type-level contract free of schedule-dependent
   material, which is what lets one compiled body serve every instance.
3. **Cross-instance tick ordering** (`QualifiedTick`, producers before
   consumers) is global by construction.  Same question.
4. **ActionValue methods with caller-latched args** -- `lower.rs:4290-4300`
   already records an interp/compiled asymmetry and bails to ineligible in
   one case.  A call boundary has to define this, not inherit it.
5. **What does de-inlining cost at run time?**  Still unmeasured, and still
   the number the whole trade turns on.  Outlining is now available for every
   method, so the experiment is a `TRS_JIT_SHARD=1` A/B on simulation RATE
   (not link time) on a design whose worst function is a replicated child
   cone.  Correctness is settled -- output is byte-identical -- so this is
   purely a cost question.
6. **The settle contract has a candidate** (section 4b): shadow writes on
   calls, one eval at each observation frontier, three-phase commit at the
   tick.  It needs no `VPathInfo` restriction, so every fragment is
   importable.  What remains open is whether the frontier eval is cheap
   enough in practice, and how the declared-paths contract is CHECKED rather
   than trusted.
7. ~~Can a foreign instance's call sites be enumerated at plan time?~~
   **Answered: yes, and the question was aimed at the wrong thing.**  There is
   nothing to enumerate INSIDE a foreign module -- TRS never lowers it, and
   Verilator owns its internals including any DPI.  What must be enumerable is
   the call TO it, which lives in the calling Bluespec cone: `lower.rs:4382`
   trampolines any prim child the arena does not model, so a foreign leaf is
   reached exactly as a FIFO is.  Measured: the 141 cones that used to bail on
   "callback sites in method cone" now realize carrying 5,518 call-site
   templates between them.  `self_inst` stays discarded; identity is off the
   leaf seam's critical path.
8. **Where does a foreign instance's state live?**  A Verilated object cannot
   sit in the arena, so region-relative addressing does not reach it and it
   needs a handle table indexed by instance.  This collides with question 6's
   neighbour -- the callback-identity fix is *also* about making instance
   identity region-relative -- so resolve this one first, before designing
   that.
9. **Per-INSTANCE realization vs per-TYPE code.**  Substitution is chosen per
   instance -- this instance is BIR, that one is a Verilated model -- while
   the code that runs a fragment is shared per module type (exec bodies,
   boundary fns, and the planned per-segment fns).  Those reconcile only if
   everything instance-specific is PASSED rather than baked.  This is the same
   fault line that killed two earlier designs: absolute instance indices baked
   into a shared call-site table, and a runtime `self_inst` argument added to
   carry identity that a per-ordinal table already distinguishes.  The rule
   that survived: share code by type, and materialise instance-specific data
   into per-ordinal tables at plan time.  Any new per-type artifact should be
   checked against it before it is built.
10. **Are foreign instances always leaves?**  Imported Verilog presents
   outputs but never calls back into Bluespec, which collapses the ordering
   problem to "eval before reading, after driving".  Believed true; a BVI
   module with an output clock or an interface the parent connects onward
   would test it.

## 7. What can be done now, on monolithic `.bir`

The remaining fragment-column rows of section 5's table.  A whole-design
`.bir` already contains per-module schedules, segments, `content_hash`, and
the full module/instance structure -- the same 67 types.  Per-fragment `.bir`
adds incremental export and bounded memory, not new information.  Nothing here
needs to wait on the Rust global-synthesis port.

The first measurement to take, now that outlining covers every method: run
`TRS_JIT_SHARD=1` against the huge design and record (a) what happens to the
66.8M-instruction function, (b) link time, and (c) **simulation rate**.  That
last one is question 5, the pivot for everything above, and it should not be
estimated.  Note the design measured so far is the wrong shape to answer it --
its worst function is the rule's own straight-line logic, not child cones.
