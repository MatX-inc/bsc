# verifyschedule: bsc's schedules, checked in Lean

`verifyschedule` validates bsc's scheduling one module at a time. From the
`.ba` file bsc writes for a module, it generates a self-contained Lean 4 file
whose main theorem states that the hardware bsc built --- the
`CAN_FIRE`/`WILL_FIRE` logic, the submodule enables and the argument muxes ---
does in every clock cycle what running the rules and methods that fire, one at
a time, in bsc's logical execution order, does. Further theorems check the
module's exported scheduling annotations against its hardware. Lean checks the file with
nothing but its core library; the proofs are by `bv_decide`.

    bsc -elab -u -verilog LookupQ.bsv               # writes mkLookupQ.ba
    inst/bin/verifyschedule mkLookupQ.ba            # writes MkLookupQ.lean
    lean MkLookupQ.lean                             # checks it

The tool is `src/comp/verifyschedule.hs`, a bsc utility like `dumpba`:
`make verifyschedule` in `src/comp` builds it, and `make install-src`
installs it as `inst/bin/verifyschedule`.  On large designs it is Lean, not
the tool, that takes the time.  (Work in progress: the tool is being renamed
schedlean, and check.sh and mutate.py are to become `--check` and
`--mutants` options of the tool itself.)

## Running it

bsc writes a `.ba` file only with `-elab`. Then

    verifyschedule [-o OUT.lean] [--dump-design] MODULE.ba

writes `<Module>.lean`, the module name capitalised (`mkLookupQ` gives
`MkLookupQ.lean`, namespace `MkLookupQ`), or `OUT.lean`. Warnings go to
standard error. `--dump-design` prints the model the tool extracted instead
(see *How it works*), for checking that phase on its own.

The `.ba` file must come from the same build of bsc as the tool. The Lean file needs Lean 4 (tested with
v4.35.0) and no Mathlib or lake. Small modules check in seconds, and the
largest designs in bsc's testsuite in a minute or two.

## What the file proves

The file has five sections:

1. **Submodule & Method State Structure.** `State` records, for each copy
   of each action method of each submodule, whether a cycle calls it and
   with what arguments; the module's outputs; and two flags,
   `guard_violated` and `illegal_access`.
2. **Combinational Definitions & Symbolic Datapath Tokens.** The token
   sorts and constants, the free variables of a cycle (`Inputs`), the values
   that change within a cycle as one step sees them (`View`), and every
   definition both semantics share.
3. **Pre-Scheduled Sequential Semantics.** The one-rule-at-a-time execution,
   field by field: for each step and each `State` field it writes, one
   definition `Seq.<step>.<field>`, in terms of each field as the last
   earlier step left it. `stepSequential` is the `State` of the last
   versions. A step runs when the hardware fires it. It sees submodule
   outputs as they stand before it, has its guard checked there, and has
   each action-method call, and each system task's reads, checked against
   what earlier steps did. A value it reads where the submodule's
   annotations forbid it (after a call in the cycle that it may not
   follow, or in one rule with a call it may not share a rule with) is
   arbitrary: a value of the step's own, which a congruence ties to the
   hardware's only when the step does not fire or no such call came first.
   A flag (`illegal_access`, `guard_violated`, an enable, a read that a
   later read may not follow) is set once any step sets it; another field
   keeps what the last step that wrote it wrote. When a read reaches what
   a task prints, or what a step does, is worked out from the top (see
   *The model*). Only for a definition on the way that several others, or
   the step itself, refer to is the condition the step's own
   (`<def>@prints@<step>`, `<def>@does@<step>`, a definition when more
   than one term uses it); along a chain of definitions only one other
   refers to, the conditions are the same for every step (`<def>@up`,
   `<def>@reaches@<copy>`, `<def>@reaches@<def>`).

   When the module exports conflict-free pairs of methods, each step is
   also stated as an effect on the whole state (`Run.fp.<step>`,
   `Run.U.<step>`, `Run.C.<step>`): the fields it reads, the fields it
   sets to values computed from those, and the flags it sets when
   conditions computed from those hold, by field number. `Run.seg` runs
   steps of the schedule's order, and `CF.Order.run` an order of
   `CF.orders`: for each exported CF pair, the schedule's order with the
   steps of the method that comes first in it, and those that must follow
   them, moved after the other method's last step. A step's view is then a
   function of the calls it sees (`view@<step>`, a free variable), so that
   in another order it sees what that order's calls make it see; for a
   read the annotations rule out, of whether the step fires after a call
   that makes it arbitrary.
4. **Post-Scheduled Synchronous Hardware Semantics.** Every submodule input
   and module output as bsc's back end computes it (`Hw.*`), collected
   into `stepScheduled`.
5. **Disjointness Oracles & Main Schedule Soundness Theorem.** `Agree`,
   the hypotheses, and the theorems:

       theorem schedule_sound (i : Inputs) (env : Environment i) ...
           : Agree i (stepSequential i) (stepScheduled i) = true

   `Agree` says the sequential run kept every guard and every method
   ordering rule, and printed nothing from a read those rules forbid (see
   *The model*), and that both runs make the same calls with the same
   arguments and produce the same outputs, each when it matters (a value
   method's result only when it is ready, for instance). When the hardware
   has a parallel mux, the conclusion also has `Hw.oneHot i = true`: its
   selectors are one-hot, so reading it as a priority mux is exact.

   The proof first checks that the hypotheses are consistent: `bv_decide`
   must fail to derive `False` from them (`fail_if_success`), or the proof
   fails.
   Contradictory hypotheses would make the theorem vacuous. Only the user's
   assertions can contradict each other, for instance a run-time check
   that fails in every cycle, and then this check fails.

   `exported_CF` proves the same for every order of `CF.orders` (and
   `exported_<a>_CF_<b>` for each, when there are few): its run, stated
   from its own steps, is the schedule's run, so it agrees with the
   hardware by `schedule_sound`. Each step an order moves commutes with
   each step it moves past (`CF.Order.run_eq`): two steps whose fields do
   not overlap, but for flags both set, commute by one generic theorem
   (`Run.Eff.comm`) and a decided check of their footprints (`Run.indep`;
   `CF.iv` lists the ranges of steps each moved step is checked against,
   and a step that reads and flags nothing and sets only fields of its own
   commutes with all, `Run.free`); other pairs by facts `bv_decide`
   proves from the hypotheses in one proof (`Run.comms`, `Comm.<a>.<b>`,
   given also that a view function gives equal values at arguments equal
   in value, which the proof proves for each two of its applications;
   each pair's facts are a definition, `Run.sem_<x>_<y>`, and the pairs'
   are gathered by a balanced tree of definitions, so that each pair
   reaches its own through few small unfoldings). The
   schedule's run is the field-by-field execution of section 3, step by
   step and by definition (`Run.link`, `Run.final`). `CF.checks` decides,
   for each order, that it moves only such steps and runs all of one
   method's steps before the other's. A long list of orders is packed into
   numbers (`CF.ordersOf`), which Lean elaborates at once, in chunks of 64
   that are checked apart and concatenated as a balanced tree.

   The proof keeps the problem `bv_decide` sees linear in the size of the
   design. The definitions of sections 2, 3 and 4 and the views are
   definitions, not abbreviations, so `bv_decide` treats each application of
   one as a value of its own; the proof supplies the equation of every
   application the facts and the goal reach (`d.eq_1`, or `rfl` for a field
   of a view). Unfolded instead, the definitions every fact shares would be
   copied into each fact. Each equation is printed once, in the one proof:
   first those the hypotheses reach, which is all the consistency check
   needs, then the rest. The theorems take their hypotheses as one
   structure, `Hyps`, which that proof takes apart. The theorem's conclusion names
   `stepSequential` and `stepScheduled`, which are definitions for the same
   reason: `bv_decide` also unfolds the abbreviations in the statement of
   the theorem being proved; the proof unfolds them, and `Agree`, in the
   goal (`dsimp only [...]`). Facts that conditions are pairwise disjoint
   (oracles, exported `C` pairs) and the one-hot condition are stated as
   chains (each condition against the OR of those before it), which say
   the same in a size linear in the conditions. So are the facts the solver
   confirms: a use that may not follow the earlier uses of a method from
   two or more steps is checked, with one query, against the OR of all of
   them (`Hw.uses_<k>.upto_<n>`). Long disjunctions whose tails recur, as
   in bsc's checks of a `mutually_exclusive` group, share a definition per
   tail (`Hw.ors.<k>`). `exported_order_agrees` checks the exported `SB` and
   `SBR` relations against the execution order: of the two methods' rules,
   each pair whose submodule uses are not all conflict-free must come in that
   order. Its doc comment names the pairs it skips.

The file ends with `#print axioms`. It lists Lean's three standard axioms
and the axioms through which Lean trusts `bv_decide`'s compiled certificate
checker.

**One cycle, any state.** Every submodule output at the start of the cycle
and every module input is a free variable. The theorems therefore hold in
every cycle, reachable or not. They also cannot rely on an invariant of the
design. `LookupQ` never finds its FIFO full when a request arrives, but
nothing here needs that.

## The model

* **Submodules are black boxes.** Each is known by its `VModInfo`: its
  methods and ports, and the scheduling annotations between them (`CF`,
  `SB`, `SBR`, `C`, ...). As in bsc's scheduler, only `CF`, `SB` and `SBR`
  allow two methods in one cycle. A value method's result depends on the
  state at the start of the cycle, on the same-cycle calls it sees (by
  annotation or by a combinational path), and on its arguments. Each step's
  view of it is tied to the hardware's wire by a congruence: same calls
  before, same arguments, same result. A multi-ported method (a `RegFile`'s
  `sub`) is one method per port. The one exception: bsc's wire primitives
  (`RWire`, `RWire0`, `BypassWire`) get the behaviour of their Verilog,
  output equals input within the cycle.
* **A read the annotations rule out is arbitrary.** A value method read
  after a call of the same submodule that it may not follow (a register's
  `_read` after its `_write`, a FIFO's `notEmpty` after its `enq`), or in
  one rule with a call it may not share a rule with, returns no value the
  submodule promises. The step then sees an arbitrary value, a free
  variable of the cycle: its congruence ties it to the hardware's value
  only when the step does not fire or no such call came first. So the
  theorem holds whatever such a read returns, or the proof fails. Which
  reads are such follows from the expressions each step reads and from the
  annotations alone. A read is not counted after a call by a step bsc
  proved never to fire with it (the theorem's `disjoint_` fact; each step
  fires only when its `CAN_FIRE` holds, and the `CAN_FIRE` the step's firing
  implies is the very definition the fact names). A read of a value method
  after a read of another that its annotations forbid (possible only in an
  imported module) counts only when the earlier read mattered: when the
  value it read reached what its step did, as for system tasks below.
  That is sound because a value method has no effect: a read whose value
  reaches nothing cannot be told apart from no read at all. An action
  method called where the annotations forbid it is illegal
  (`illegal_access`), with the calls' exact conditions.
* **What a system task prints must be what the hardware prints.** A
  `$display` or other system task has no effect on the submodules, and the
  tool does not model what it does or compare its arguments with the
  hardware's task. But what it prints is observable, so its reads are not
  made arbitrary, as other reads are. Instead the theorem proves that no
  task prints, or decides whether to print on, a value read where the
  annotations rule the read out: such a read is illegal (`illegal_access`)
  when its value can reach the task's condition, or one of its arguments
  while the condition holds. Whether it can follows the selections between
  the read and the task, as the sequential semantics makes them: an arm of
  an `if`, a `case`, a mux or an array selection counts only when its
  condition selects it, and through any other operator the read reaches
  the result. The condition is worked out from the top, through the
  definitions between the task and the read, so that a chain of n
  selections over n reads takes a size linear in n, not n². So
  `$display(counters[j].readCount)` in a rule that also calls
  `counters[j+1].incCount` is legal: it reads every copy, but prints only
  copy `j`, which the rule does not increment. When the theorem holds,
  every value a task prints, and whether it prints, depends only on reads
  the annotations allow where the task runs; such a read sees what the
  hardware's wire shows once the calls before it agree (the congruences),
  and the theorem proves that they do. That the hardware's task computes
  its arguments from the same expressions is `aState`'s transcription, which
  is not checked.
* **The datapath is abstract, the control concrete.** Booleans are
  booleans. A wider value is a token and an operator on such values is an
  uninterpreted function, made consistent by Ackermann's congruences. Each
  token sort has room for every distinct term of its width in the file plus
  one, so a theorem about tokens implies the theorem about the real values
  (the small-model property of equality logic).
* **The hardware is bsc's.** The tool runs bsc's own `aState` on the `.ba`
  to get the `ASPackage` the Verilog back end starts from. The back end's
  later passes, such as inlining wires and renaming ports, are not covered.

## What is trusted

The hypotheses of `schedule_sound` are structures, each fact with a comment
saying where it comes from. A file has only the structures it needs.

* `Environment`: the parent obeys the module's exported schedule. A method
  is enabled only when ready, and two methods exported `C` are never enabled
  together.
* `Primitives`: what bsc's wire primitives do, from their Verilog.
* `Oracles`: facts bsc's disjointness solver (Yices or STP) proved. Some
  bsc recorded in the `.ba`. The others it relied on without recording, and
  the tool asks the same solver about the same expressions again. Only
  confirmed facts are used.
* `Assertions`: the user's `mutually_exclusive` and `conflict_free`
  assertions, and bsc's other run-time checks; bsc reports at simulation
  time if one fails.
* `Congruence`: functional consistency. These facts hold by construction;
  they are hypotheses only to keep the uninterpreted functions out of the
  SAT problem. A read the annotations rule out is tied to the hardware's
  value only when its step does not fire or no call it may not follow came
  first.

Beyond the hypotheses, the proof trusts Lean's kernel and `bv_decide`'s
compiled certificate checker, and the conditions of the action calls in
bsc's packages. It does not trust bsc's use analysis (`AUses.buildUseMaps`,
run again): the conditions under which that analysis says a method use
matters only choose which disjointness facts to ask bsc's solver for. A
fact the solver confirms is true of the hardware whatever the condition,
and a wrong condition only yields a weaker fact or none, so the proof can
only fail. It trusts this tool's extraction, which is not itself verified:
a bug there could make the model differ from bsc's hardware. The generated
file is written to be read against the design, and `--dump-design` shows
the extracted model, with bsc's use conditions marked as hints.

## How it works

The tool runs in three pure phases:

1. **Extraction** (`Design`). Read the `.ba` and run `aState`. Then rebuild
   the three things the `.ba` does not record:
   * the conditions under which each method use matters (bsc's
     `AUses.buildUseMaps`, run again), only as hints of which disjointness
     facts to ask for;
   * which copy of a multi-ported method serves each use (from the
     resource allocation table);
   * the disjointness facts bsc relied on without recording them (asked of
     its solver again).

   Then list the steps in execution order, with each step's guard, the
   submodule outputs it reads, its calls, and its outputs, and find the
   reads the annotations rule out, in the schedule's order or in another
   order to check.
2. **Analysis** (`Proof`). Evaluate each step in the sequential semantics,
   and every hardware signal. Abstract the datapath. Find the values each
   step observes within the cycle, with their congruences. Build the orders
   to check: the schedule's, and one for each exported `CF` pair, each
   the schedule's with a block of steps moved (`orderBlock`), and each
   step's effect on the whole state.
3. **Emission.** Print the Lean file.

## Limitations

* Only modules with one clock and one reset are supported; others are
  refused, as are modules with size parameters.
* Exported clocks, clock gates and resets are not compared; the tool warns
  when a module has them.
* `$display` and other system tasks are not modelled, and their arguments
  are not compared with the hardware's; bsc's run-time checks are the
  exception: they are assertions. What a task prints is checked only
  against the annotations, as above. Whether a read reaches a task, or what
  a step does, is decided by the selections on the way, not by the
  operators: a read in `x && false` still counts.
* A Lean structure has at most 255 fields. A larger `Inputs` is split into
  parts, and larger hypothesis structures into several. A `View` or `State`
  that large is not handled yet.
* A failing proof prints a counterexample. It shows either a real
  disagreement between bsc's hardware and the sequential semantics, or
  something outside the model. `bv_decide` calls it "potentially spurious"
  because it treats each variable `i.x` as an opaque atom, but the variables
  are independent, so the assignment is a real one. To see which part of
  `Agree` fails, prove its conjuncts one at a time.

## Examples

`examples/` holds small designs, each exercising a part of the model, and
`check.sh` compiles each with bsc, runs the tool and checks the result with
Lean:

    util/verifyschedule/check.sh               # all of them
    util/verifyschedule/check.sh LookupQ.bsv   # some of them
    BSC=/path/to/bsc LEAN=/path/to/lean util/verifyschedule/check.sh

| Example | What it exercises |
|---|---|
| `LookupQ` | A credit-controlled lookup unit. RWire and PulseWire forwarding, a multi-ported RegFile, a FIFO, and an exported CF pair. |
| `MethodRelations` | The module's own exported `SB`, `C` and `CF` relations, and the swap theorems. |
| `WireKinds` | Wire kinds: RWire, PulseWire, BypassWire and DWire. |
| `CRegPorts` | A 3-port CReg: a write on one port seen by later ports in the same cycle. |
| `RegFilePorts` | Copies of a multi-ported method, and a value method with arguments. |
| `FifoChain` | FIFO kinds, including loopy ones with a combinational path from `deq` to `notFull`. |
| `UrgencyBlocking` | A rule blocked by a more urgent conflicting rule. |
| `SplitRule` | A rule split by `(* split *)` into disjoint halves. |
| `SplitMethod` | A method split into two rules, and the module's exported relations that name it. |
| `ActionValuePump` | Action-value methods, of the module and of a separately synthesized child. |
| `BranchWrites` | One register written under disjoint conditions in one rule. |
| `ValueAfterAction` | A value method reading state an action method writes in the same cycle. |
| `WireResult` | A value method's result computed from a wire that sees same-cycle calls. |
| `SysTasks` | System tasks, which are not modelled, next to a forwarded value. |
| `BviSchedKinds` | An imported Verilog child with `P` and a self-`SBR` annotation. |
| `AssertedSchedules` | `mutually_exclusive` and `conflict_free`, which bsc checks at run time. |
| `NotEmptyOrder` | A guard whose method uses matter only under conditions, which bsc's scheduler exploits: a guard read after an `enq` it may not follow sees an arbitrary value, which does not matter. |
| `RenamedPorts` | Methods with renamed enable and ready ports, and an `always_ready` method. |
| `LeanNames` | bsc names that Lean reserves, or defines itself in a structure's namespace. |
| `AvArg` | An action-value method whose argument is a value that sees same-cycle calls (a CReg port). |
| `CfOrders` | Several pairwise-CF methods: orders checked by commutation, free steps, footprints and a semantic fact; `mutate.py`'s order mutant. |
| `UseFact` | A register read after a write it may not follow, which only a fact bsc's solver proves about the two uses' conditions shows not to matter. |
| `FutureReadC` | A value method read before a call it conflicts with (`C`), in an imported child: the read sees an arbitrary value unless it sees the cycle's calls. |
| `RdyAfterWrite` | A ready signal reading a register a conflicting method writes first: the read is arbitrary only when its method fires, since the ready signal is an output in every cycle. |
| `TaskSelect` | `$display(counters[j].readCount)` next to `counters[j+1].incCount`, with `readCount SBR incCount` (bsc's `SBRValueOK_small`, smaller): the task reads every copy, but prints only the one `j` selects, which the rule does not increment. |
| `TaskChain` | A task printing a value chosen by a chain of selections over reads that are out of order where the chain does not select them: the condition under which each read reaches the task takes a definition per link (`<def>@up`), not one per link and read. |
| `ValueReads` | An imported child whose value methods may not be read in one order (`v1 SB v2`): a read of `v1` after a read of `v2` counts only when the earlier read mattered, here under a condition disjoint from the later read's. |
