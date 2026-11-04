# schedlean: bsc's schedules, checked in Lean

`schedlean` (formerly `verifyschedule`) validates bsc's scheduling one module at a time. From the
`.ba` file bsc writes for a module (`bsc -elab`), it writes a Lean 4 file whose theorem states that
the hardware bsc built --- the `CAN_FIRE`/`WILL_FIRE` logic, the submodule enables and the argument
muxes --- does in every clock cycle what running the rules and methods that fire, one at a time, in
bsc's logical execution order, does, down to the system tasks it calls and their order. Lean checks the
file with nothing but its core library; the proofs are by `bv_decide` and kernel-decided scans.

    bsc -elab -u -verilog LookupQ.bsv               # writes mkLookupQ.ba
    inst/bin/schedlean --gate mkLookupQ.ba          # does the modular proof apply?
    inst/bin/schedlean mkLookupQ.ba                 # writes MkLookupQ.lean (the modular obligation)
    inst/bin/schedlean --check mkLookupQ.ba         # writes it and checks it in Lean
    inst/bin/schedlean --mutants 6 mkLookupQ.ba     # and checks that six broken hardwares fail

The default output is the **modular** obligation (`theory/modular/design.md`, conventions in
`theory/modular/emit/conventions.md`), proved against the Lean library in `theory/modular`
(`build.sh` builds it). `--flat` writes the older whole-module theorem, which the rest of this README
describes from *What the file proves* on; it stays for comparison.

## The tool

`make schedlean` in `src/comp` builds it, and it is installed as `inst/bin/schedlean` (the `UTILEXES`
list). The driver `src/comp/schedlean.hs` only parses options, reads the `.ba` and writes files. The
verifier is in hierarchical modules under `src/comp/Lean/`:

- `Lean.Sched.*`, the schedule verifier. Extraction from bsc's in-memory `APackage` and schedule:
  `Types`, `Extract`, `Conflicts`, `Order`, `OutOfOrder`, `BscFacts`, `Domain` (clock domains, gated
  clocks and resets). The modular model: `Expr`,
  `Enc`, `Reach`, `MECheck`, `Sites` (site conditions), `Kinds`, `Model`, `Scan`, `Facts`, `Shadow`,
  `Bounds`, `Gate`. Synthesized children: `Export` (a child's exported interface, its kinds in a
  parent) and `Hier` (the instance DAG, children first). The emitter: `Emit` and `Emit.*`. Views and tests: `Show`, `APkg` (a parse of
  bsc's printed `APackage`, kept only as a test oracle for `--show-model=apkg-text`), `Py`, `Time`.
  `Flat` is the flat theorem behind `--flat`.
  `Check` runs Lean for `--check` and `--mutants`; `Mutate` makes the mutants.
- `Lean.Print`, `Lean.BV`, `Lean.Solver` and `Lean.Run` can be reused by other Lean verifiers of bsc
  output. `Lean.Solver` asks bsc's own disjointness solver (Yices or STP) about Boolean skeletons; an
  unknown verdict never counts as unsatisfiable. Yices keeps one live context per process (making a
  state resets it), so a module's bsc facts, gate checks and skeleton queries share one state
  (`Live`, the skeleton names apart from the module's definitions), and a state made before another
  (a parent's, after its children's) is remade before it is asked again. `Lean.Run` finds and runs Lean, capped and timed, and
  reads its verdict.

Options:

- (none) the modular file, `<Module>.lean` or `-o FILE`; parts above the part bound are written as
  `<stem>_p<k>.lean`. It prints a `note:` line for each method whose task calls are left out of task
  agreement (bsc G0020), the scan segments and a summary line.
- `--gate` reports whether the modular proof applies to the module in the bsc mode it was compiled in
  (design.md §11): the `GATE` record (scan, escapes and their facts with how each was confirmed,
  last-caller ports, bounds, generic leaves, assertions relied on), a `counts` row (largest step,
  port users, run-time check groups), the shadow classes, and one summary row: design, mode, `A` or
  `flat`, whether the emitter writes a file (a dry run in memory), and the reasons. A module with
  gated clocks or several resets also gets a `domain` line (its clocks, gates and resets). A module
  of several clock domains gets its own verdict, `not checked: multi-domain`, neither `A` nor `flat`
  (a `GATE` record with the verdict and the summary row with its domains; exit status 0); see
  *Clocks and resets*.
- `--show-model[=VIEW]` prints an intermediate view: the extracted design (no `VIEW`), `apkg`,
  `apkg-tree`, `apkg-text`, `apkg-mem`, `sites`, `model`, `facts`.
- `--flat` writes the flat theorem; `--time-phases` prints each phase's wall and CPU time on standard
  error; `-V/--version`, `-h/--help`.
- Measurement options, each labelled in the file: `--limit NAME=N` lowers a gate bound (never raises
  one), `--first-rules N` keeps the first N steps (a partial theorem), `--no-task-agreement` leaves
  out task agreement (a weaker theorem).
- `--check` checks the file written in Lean (the parts first, each compiled for the next) and prints
  `check FILE: VERDICT`: `proved` (Lean exits 0, no error, no `sorry`, and every `#print axioms` lists
  only `propext`, `Classical.choice`, `Quot.sound`), `counterexample` (`decide` proves a proposition
  false, or `bv_decide` gives an assignment), `vacuous` (a `--flat` file's consistency check fails),
  `timeout` or `error`, the reason and the log (`<stem>.check.log`). Works on `--flat` files too.
- `--mutants N` checks the modular file, then N mutants of its hardware, each of which must fail:
  an enable dropped or forced on, an argument under a fresh operator, two mux members' arguments
  exchanged, a guard reading another port, two task statements of steps in a forced order numbered the
  other way round (CF order), a last-caller mux whose first caller wins, a call condition left out of
  a port's enable where a `conflict_free` check names that caller and another (so the two may both
  call in the hardware, while the check still reads the condition). Only mutants whose obligation
  is false are made (a step that can fire, a call that can be made, two steps that can fire together
  in a forced order), so a mutant whose file checks is a gap in the theorem. Kinds take turns and each
  kind's candidates are spread over the design; the first N are the same for any larger N. A killed
  mutant's files are removed, a missed or timed-out one's kept as `<stem>_mut<i>.lean`
  (`--keep-mutants` keeps them all). A mutant the emitter refuses to write (only a CF-order one can
  be) is reported `not written` and counted apart, not as killed: there is no Lean evidence for it.
  `util/schedlean/mutate.py` mutates `--flat` files.
- `--lean PATH` (else `$LEAN`, else `lean` on `PATH`), `--lean-timeout SECONDS` (default 1800);
  every Lean run is `-j1 -DElab.async=false` under a 24 GiB address-space cap (`prlimit`, else
  `ulimit -v`). `--lean-lib DIR` holds the library (`Modular`, `EmitLemmas`, `Scan`, `LastCaller`,
  `SiteCond`) compiled, or its sources, which are compiled once into `$SCHEDLEAN_CACHE` (else
  `~/.cache/schedlean`), keyed by Lean's version and the sources. Default: `$BLUESPECDIR/schedlean`
  (`make install-utils` installs the sources there), else the checkout's `theory/modular`.
- `-p DIRS`/`--search-path DIRS` (colon-separated, may repeat): where synthesized children's `.ba`
  files are, after the parent's directory; see *Synthesized children*.
- Exit status: 0 written (and proved, every mutant killed); 1 the emitter stops; 2 usage; 3
  counterexample; 4 vacuous; 5 timeout (also a mutant's); 6 any other Lean error; 7 Lean could not
  run (not found, or the library did not build); 8 a mutant's file checks; 9 a mutant's file is not
  written (the emitter stops on it).

Library children are told apart from user imports through the installation: schedlean reads
`$BLUESPECDIR` (set by the installed wrapper), else the bsc directory the `.ba` records.

## Clocks and resets

The modular proof checks a module of **one clock domain** (bsc's clock domains are clock families):

- **Gated clocks** of that family are supported when the gate is an input of the module (a gated
  input clock, `gate_input_clocks`, with `clock_family`). bsc ANDs a clock's gate into the condition
  of every action call on the state it clocks (`IExpand`, `handleMethod`), so the gate is in the
  `CAN_FIRE` of each rule that writes that state and in the `RDY` of each method that does.
  schedlean checks, for every such call, that the step's guard (a method's ready signal) and the
  call's condition imply the gate, structurally or with bsc's solver. Then in a cycle with the gate
  off that state is not called and holds, as its Lean model does without calls, whether or not the
  clock still ticks. The gate is not part of the module's Lean interface: a guard that reads it is an
  opaque operator of its other reads, and the theorem holds for every `ops`, so for the gate's value
  in each cycle. The emitter stops, and the gate fails, with a named reason when a gate is not an
  input (a gate a child drives, such as `mkGatedClock`'s, has no Lean model yet), when a child's
  Verilog takes a gate as a port, or when a call may be enabled with its gate off. That a gated clock
  shares the default clock's edges is the `clock_family` attribute, a user assertion schedlean does
  not check (a trust point, named in the top theorem's docstring; see *What is trusted*).
- **Several resets**: the theorem covers the cycles **outside reset**, with every reset deasserted;
  the file's first comment and the top theorem's docstring say so. A module of one reset is covered
  the same way (the library models have no reset), and its file is unchanged. Async-reset primitives (`RegA`, `ConfigRegA`; FIFOs choose async reset by a Verilog define,
  not by module name) get the model of their synchronous form, which they equal outside reset.
- **Several clock domains**: not checked, with no fallback. `--gate` reports `not checked:
  multi-domain` with each domain's clocks; the emitter and `--show-model` stop with the same message.
  `--flat` still refuses any module with several clocks or resets.

## Synthesized children

A module with synthesized children (bsc compiled them separately, `(* synthesize *)`) is checked one
file per module (design.md §3.1). schedlean finds each child's `.ba` beside its parent's, else in the
`-p` directories, and writes the files children first: `<Child>.lean` (the module name capitalised)
beside the top file, each child's file imported by its parents. The instance tree is read from each
`.ba`'s instances alone; a module is read in full only at its turn, and its file is written (and with
`--check` checked, with its mutants) before the next is read, so a run holds one module at a time.
`--gate` prints each module's record and rows, children first, then a `hierarchy` row giving each
child instance its kind and place; `--check` checks the files in order (each child's compiled file on
its parents' path), stopping at the first that does not prove, and `--mutants N` mutates each module
once it proves, each mutant checked against the real children.

A child **on path A** enters its parent through its own file's `built` theorem (Lean's `Built.node`):
its own solver verdicts, operators and assumed leaves become fields of the parent's `Subs` record, a
named hypothesis of the top theorem, one entry per instance (a child's file folds its inputs, method
arguments and module ports, into its operators, so two instances fed differently have different
ones). A child's
file exports its whole interface: action methods (each C with itself, whatever bsc exports for a method
with itself), value methods without arguments, bsc's relations between distinct methods, and
`alwaysRdy` for the methods bsc builds always ready (their ready output is the constant true, which
bsc's wrapper of the child assumes; the child's file proves its guard is true). A child is on path A
when its file is written with a top theorem from its hypotheses alone, the parent reads none of its
ready ports, and the parent's view of its schedule equals its own. The parent's theorem then holds
under what the child's file assumes (its assumed leaves and user assertions, and its own children's):
the parent's `Leaves` docstring, its header and its `hierarchy` row name them.

Otherwise the child is an **assumed leaf** of the parent, named in `Leaves` (per instance, a `Module`,
its `Realizes`, `Causal` and `Aliased`), with an interface from the parent's view of bsc's schedule,
each `RDY_m` a value port aliased to `m`'s ready output; the `hierarchy` row says why. The commonest
reason is a parent reading a child's ready port (a guarded method); the others are a value method with
arguments (not exported, and a parent reading one stops the emitter), a child whose `.ba` is not
found, the parent's view of its schedule or always-ready methods differing from its own `.ba`, and the
child's own limits (no `Acyc`/`top`, unconfirmed facts, a scan failure). An assumed leaf whose `.ba` is
not found is assumed to fire each action method on its enable. The top module's file keeps its value
methods outside its interface, as before.

A child with an `always_enabled` or `enabled_when_ready` action method is neither: bsc runs such a
method in every cycle (or whenever it is ready), not on the parent's enable, so no contract with an
enable describes it. Its export is refused (its `.ba` names the attribute, or its `WILL_FIRE` is not
the method's enable), and a parent that uses it stops, with the reason in its gate row (verdict not A)
and in the `hierarchy` row. Fix path: export `alwaysEn` for such a method and emit the parent's `alw`
obligation (the library's `CertB.alw`: a step that fires in every cycle calls the port).

## User assertions

bsc checks `mutually_exclusive` and `conflict_free` only at run time (`$error` R0001 and R0002). The
modular file states every such check in `asmP`, the module's assertions, and `top` covers the cycles
in which they pass (design.md §5):

- a `mutually_exclusive` line `(WILL_FIRE_a1 || …) && (WILL_FIRE_b1 || …)` is one check per rule of
  its first side, `!(WF ai && (WF b1 || …))`, which together are exactly bsc's line; a chain of
  single-rule lines is one `SufAsm` group;
- a `conflict_free` check in rule `R`, `!(WILL_FIRE_R && line)`, is `cfL_c`: the line's conjuncts, each
  instrumentation wire (`_<method>_<rule>$EN_<instance>`) read as its one writer's argument, the
  writer's call condition. The `$error` that reports it is a task statement of `R` that the step never
  makes; its hardware enable is `WF R && cfL_c`, which `asm` makes false;
- a dynamic fact the solver confirms only with the `conflict_free` checks assumed (`assert` in the
  `GATE` line), an escape or the exclusivity of two callers of one port, is stated under `asmP`.

The first comment lists them. A check line in another form stops the emitter.

## Running it

bsc writes a `.ba` file only with `-elab`. Then

    schedlean --flat [-o OUT.lean] MODULE.ba
    schedlean --show-model MODULE.ba

writes `<Module>.lean`, the module name capitalised (`mkLookupQ` gives
`MkLookupQ.lean`, namespace `MkLookupQ`), or `OUT.lean`. Warnings go to
standard error. `--show-model` prints the model the tool extracted instead
(see *How it works*), for checking that phase on its own.

The `.ba` file must come from the same build of bsc as the tool. The Lean file needs Lean 4 (tested with
v4.35.0) and no Mathlib or lake. Small modules check in seconds, and the
largest designs in bsc's testsuite in a few minutes (about seven for
`sysConflictFreeOKLarge`, whose two rules make 2050 system-task calls, 1536
of them bsc's run-time checks).

## What the file proves

The file has five sections:

1. **Submodule & Method State Structure.** `State` records, for each copy
   of each action method of each submodule, whether a cycle calls it and
   with what arguments; the module's outputs; two flags,
   `guard_violated` and `illegal_access`; and for each system-task call
   (`$display`, `$finish`, bsc's run-time checks, ...), its position among
   the calls made, from 1, or 0 when it is not made (`«$task<k>»`, a
   `TaskPos` number), and its arguments (`«$task<k>_<j>»`), with the count
   of calls made so far (`«$tasks»`).
2. **Combinational Definitions & Symbolic Datapath Tokens.** The token
   sorts and constants, the free variables of a cycle (`Inputs`), the values
   that change within a cycle as one step sees them (`View`), and every
   definition both semantics share.
3. **Pre-Scheduled Sequential Semantics.** The one-rule-at-a-time execution,
   field by field: for each step and each `State` field it writes, one
   definition `Seq.<step>.<field>`, in terms of each field as the last
   earlier step left it (a version used in one place only is written out
   there instead). `stepSequential` is the `State` of the last
   versions. A step runs when the hardware fires it. It sees submodule
   outputs as they stand before it, has its guard checked there, and has
   each action-method call, and each system task's reads, checked against
   what earlier steps did. Each system-task call it makes, in program
   order, takes the next position and records its arguments as they are
   where the step runs. A value it reads where the submodule's
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
   them, moved after the other method's last step; there a step's
   system-task calls take their positions in that order. A step's view is then a
   function of the calls it sees (`view@<step>`, a free variable), so that
   in another order it sees what that order's calls make it see; for a
   read the annotations rule out, of whether the step fires after a call
   that makes it arbitrary.
4. **Post-Scheduled Synchronous Hardware Semantics.** Every submodule input
   and module output as bsc's back end computes it (`Hw.*`), collected
   into `stepScheduled`; and the system-task calls of the module's always
   block for tasks, in its order, each `if (cond) task(args)`. Each fills
   the fields of the sequential call of the same rule, task and source
   position: its arguments, and its position, the count of the block's
   calls made up to it (`Hw.«$tasks@k»`). One that matches no sequential
   call fills fields of its own, which the sequential run never sets.
5. **Disjointness Oracles & Main Schedule Soundness Theorem.** `Agree`,
   the hypotheses, and the theorems:

       theorem schedule_sound (i : Inputs) (env : Environment i) ...
           : Agree i (stepSequential i) (stepScheduled i) = true

   `Agree` says the sequential run kept every guard and every method
   ordering rule, and printed nothing from a read those rules forbid (see
   *The model*), and that both runs make the same calls with the same
   arguments and produce the same outputs, each when it matters (a value
   method's result only when it is ready, for instance). That includes the
   system-task calls: each has the same position in both (0 if neither
   makes it) and, when made, the same arguments. So both make the same
   calls, in the same order, with the same arguments: the order of this
   module's Verilog, which leaves open how different modules' calls in one
   cycle interleave (see *Limitations*). When the hardware
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
   reaches its own through few small unfoldings). Two steps with
   system-task calls never commute by footprint, as both count the calls.
   When both can make a call in one cycle, their calls change positions,
   that fact fails, and so does the proof, with that cycle as the
   counterexample. For two methods that both print (`TaskMethods`) the
   difference is real: the hardware's order is fixed, and a parent may
   call them the other way round. bsc warns of it (G0020). The block an
   order moves also passes the steps between it and the other method's
   last step, some of which another legal order might leave on its other
   side: a counterexample from such a step's calls may come from the order
   chosen, not from every order the parent allows (in the bsc testsuite
   designs tried, no order moves a step with calls past another). The
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
   application the facts and the goal reach, as `have : d i = <body> :=
   rfl` (at the view an application has, if any), in blocks of 64.
   Unfolded instead, the definitions every fact shares would be copied
   into each fact. Each equation is printed once, in the one proof: first
   those the hypotheses reach, which is all the consistency check needs,
   then the rest. A definition used in one place only (counting a use in
   `stepSequential` or `stepScheduled` as often as `Agree` reads that
   field) is written out there instead, which copies nothing; one that a
   fact, a congruence, a view or the other orders' sections name is
   always kept. Lean's memory at `-j1` grows with the square of the number
   of top-level commands, so the small definitions share `mutual` blocks of
   up to 100, and there is one `Agree.<shape>` abbreviation per shape of
   port, and of system-task call. The theorems take their hypotheses as one
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
  `$display` or other system task has no effect on the submodules, but
  what it prints, and when, is observable. So each call is compared with
  the hardware's, as above: its position and its arguments. A
  value-returning task's result (`$time`, `$random`, `$fopen`,
  `$test$plusargs`, ...) is a free variable both runs share: the result is
  the hardware's. The task's reads are not made arbitrary, as other reads
  are. Instead the theorem also proves that no
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
  and the theorem proves that they do. The comparison does not make this
  check redundant: a read only a task makes is not arbitrary, so even out
  of order it sees, by its congruence, the hardware's value, and the
  comparison passes; only this check says the annotations allow it.
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
  time if one fails. A gated clock's `clock_family` attribute (it shares
  the default clock's edges) is a user assertion too, checked by nothing;
  the modular file names it in its top theorem's docstring.
* `Congruence`: functional consistency. These facts hold by construction;
  they are hypotheses only to keep the uninterpreted functions out of the
  SAT problem. A read the annotations rule out is tied to the hardware's
  value only when its step does not fire or no call it may not follow came
  first.

Beyond the hypotheses, the proof trusts Lean's kernel and `bv_decide`'s
compiled certificate checker, and the conditions of the action calls in
bsc's packages. Of the system tasks, it takes the hardware's calls from
the `ASPackage` (`aspkg_foreign_calls`, which `aState` fills with each
rule's calls, rules in execution order), the list bsc's Verilog back end
prints, and trusts the back end after `aState` to keep each call, its
order, condition and arguments: `vForeignBlock` prints one
`always @(negedge CLK)` block, each call `if (cond) task(args)` in list
order (an `$SVA` call becomes an `assert` outside it), and AVeriQuirks
drops the calls whose condition is constant false. It also trusts the
rewrites the back end makes in printing a call: `$fatal(code, msg)` as
`$display(msg); $finish(code)`, `$error`/`$warning`/`$info` as `$display`
outside SystemVerilog mode, a string-mux argument as an if/else of two
calls, and a C function's wide result as its first argument.
Verilog also skips a call while its rule's reset is asserted, which is
not modelled: the theorem's trace is that of a cycle out of reset. With
gated clocks of the module's family the block is sensitive to each
oscillator's falling edge (`always @(negedge CLK_g or negedge CLK)`), and
the trace assumes it runs once per cycle (the `#0` at its start makes the
edges of one time step one run). The
trace is this module's alone: how calls of different modules in one cycle
interleave, and task state they share, are left out (see *Limitations*). It
matches each hardware call to a sequential one by rule, task (as Verilog
names it) and source position; the match only chooses what is compared,
so a wrong one can only make the proof fail. It does not trust
bsc's use analysis (`AUses.buildUseMaps`, run again): the conditions under
which that analysis says a method use matters only choose which
disjointness facts to ask bsc's solver for. A fact the solver confirms is
true of the hardware whatever the condition, and a wrong condition only
yields a weaker fact or none, so the proof can only fail. It trusts this
tool's extraction, which is not itself verified: a bug there could make
the model differ from bsc's hardware. The generated file is written to be
read against the design, and `--show-model` shows the extracted model,
with bsc's use conditions marked as hints.

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
   submodule outputs it reads, its calls, its system-task calls and its
   outputs; match each of the hardware's system-task calls to a step's
   (`matchTasks`); and find the reads the annotations rule out, in the
   schedule's order or in another order to check.
2. **Analysis** (`Proof`). Evaluate each step in the sequential semantics,
   and every hardware signal. Abstract the datapath. Find the values each
   step observes within the cycle, with their congruences. Build the orders
   to check: the schedule's, and one for each exported `CF` pair, each
   the schedule's with a block of steps moved (`orderBlock`), and each
   step's effect on the whole state.
3. **Emission.** Print the Lean file.

## Limitations

* The modular file covers modules of one clock domain (see *Clocks and
  resets*); `--flat` covers only modules with one clock and one reset.
  Modules with size parameters are refused.
* Exported clocks, clock gates and resets are not compared; the tool warns
  when a module has them.
* The order of system-task calls is a property of the module's Verilog. It
  does not hold across synthesis boundaries: Verilog leaves open the order
  of different modules' always blocks on one clock edge, so their calls in
  one cycle can come in any order, and a task with state shared between
  modules (an unseeded `$random`) can give different results. Bluesim
  follows the whole design's rule order, a property of its back end outside
  this tool, so Bluesim and Verilog can print such calls in different
  orders.
* What a task does is not modelled beyond its call: a value-returning
  task's result is a free variable, and a call's arguments are values,
  not formatted text. Two different calls that print the same text count
  as different calls. bsc's run-time checks are also assertions.
* Whether a read reaches a task, or what a step does, is decided by the
  selections on the way, not by the operators: a read in `x && false`
  still counts.
* A Lean structure has at most 255 fields. A larger `Inputs`, `View` or
  `State` is split into parts (a field is then written by its path,
  `i.part<k>.x`), and larger hypothesis structures into several.
* A failing proof prints a counterexample. It shows either a real
  disagreement between bsc's hardware and the sequential semantics, or
  something outside the model. `bv_decide` calls it "potentially spurious"
  because it treats each variable `i.x` as an opaque atom, but the variables
  are independent, so the assignment is a real one. To see which part of
  `Agree` fails, prove its conjuncts one at a time.

## Examples

`examples/` holds small designs (BSV, and Bluespec Classic `.bs`), each
exercising a part of the model, and `check.sh` compiles each with bsc, runs
the tool with `--flat` and checks the result with Lean (and `mutate.py`'s mutants); a module whose source says
`check.sh: <module> fails with a counterexample in <theorem>` must instead
fail in that theorem alone, with a counterexample, while `schedule_sound`
checks:

    util/schedlean/check.sh               # all of them
    util/schedlean/check.sh LookupQ.bsv   # some of them
    BSC=/path/to/bsc LEAN=/path/to/lean util/schedlean/check.sh

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
| `SysTasks` | System tasks in two rules, printing a forwarded value, and a conditional `$finish`. |
| `TaskOrder` | Tasks in two rules that fire in one cycle (`mutate.py`'s task-order mutant swaps them), tasks under complementary conditions, a value-returning `$time` (Verilog sets a temporary to it before the calls that print it), and a conditional `$finish`. |
| `TaskMethods` | Two exported CF methods that print. `mkTaskMethods` fails with a counterexample in `Run.comms`, as `check.sh` expects: a parent calling both in the order the hardware does not print them. `mkTaskMethodsSplit`, whose two never print in one cycle, proves. |
| `ValTasks` | Value-returning tasks, each result a variable both semantics share: `$random` written to a register and printed, `$test$plusargs` guarding a print, `$stime`, and `$fopen`'s handle used by `$fdisplay`. |
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
