# P0a: public-port rendezvous, with unresolved completeness obligations

Status: **blocked/inconclusive; candidate for design-owner review**. This is
not a frozen ABI, a P0 pass, or permission to start substantial P0b work.
The small Python experiment is a hand-authored model, not a BSC interpreter
and not evidence of Bluesim/RTL parity. No compiler or RTL simulator was
available for this review. See `inventory.json` and `RESULTS.md`.

## 1. Pins and the first material finding

| Input | Revision |
| --- | --- |
| Assignment/worktree base | `054ba3f6400bfed18c20c1552981212e8cc956bd` |
| Compiler and legacy TRS, denoted M below | `a9462e0e28102f07a89b3f3c478b33c806d62a3e` |
| BVI reference, denoted B below | `d4a72767c63e6dbdf569cac4c61aeb8afc597c04` |

**Exporting only `asi_dyn_scheds` cannot close the required dynamic scope.**
M `src/comp/ASchedule.hs`, `verifyStaticScheduleOneRule` and
`verifyStaticScheduleTwoRules`, explicitly distinguish a static-simulation
failure from a Verilog rejection. Their unresolved dynamic cases produce a
warning and a Verilog-only `.ba` when targeting Verilog. They do not all
produce an `ADynSched` record. In particular:

- `mkDynSched` requires disjoint **CAN_FIRE** predicates, excludes interface
  methods as callers, and requires `aInlineStableCone` to succeed.
- `mkDynSelf` also excludes interface methods and requires a stable cone.
- `aInlineStableCone`, M `AIntraCycleStability.hs`, accepts integer
  constants, definitions, pure primitive operators and no-argument `read`
  of `RegN`/`RegUN`/`RegA`; it rejects other leaves and has a 4096-node
  expansion budget. These are restrictions on this export/simulation path,
  not a characterization of the Verilog backend's full accepted behavior.
- The compiler also checks WILL_FIRE-exclusive pairs and pairs whose
  conditional uses are disjoint even when CAN_FIREs are not disjoint.
  Those checks appear immediately above `verifyStaticScheduleTwoRules`.

The minimized source in `fixtures/DynamicConditionalCalls.bsv` preserves the
shape of M `testsuite/bsc.scheduler/sched-conditions/`
`SchedCondsDynamicSchedule.bsv`: two unconditional parent rules have a
fixed local order, but their opposite child calls are conditional. Both
parent rules may fire, so the disjoint-CAN_FIRE-only record is insufficient.
The adjacent `sched-conditions.exp` explicitly specifies Verilog compile
success with warning G0100 under `-sched-conditions`, and compile success
with G0010 but no G0100 under `-no-sched-conditions`. This is a recorded
compiler regression expectation, not a rerun in the present environment.
All registers in the new fixture are initialized. Acceptance and generated
RTL behavior remain **untested**; run its documented recipe before calling
it an executable compiler counterexample. The original source and test
expectation already give a source-grounded
counterexample to the claim that ADynSched enumerates the Verilog target.

This finding asks for **Verilog parity**, not a more general dynamic
scheduler. It must not be dispositioned as outside scope merely because
Bluesim, the current exporter, or this candidate cannot execute it.

## 2. What is private, and what can cross a boundary

The owner is one separately synthesized module specialization. It may read
its local post-scheduling package and direct-child public contracts. It
must not read a child's package to prepare its own executor. The kernel
stores opaque endpoints, time/clock/reset routing, and observations; it
never stores a within-edge list of descendant rules, segments or requests.

The following is a **candidate schema**, not a claim that every field is
already derivable or sufficient. A method channel includes its declared
port multiplicity: collapsing multiple legal uses to one Boolean enable
is not generally sound.

| Candidate field | Owner-local source or derivation | Size and current obligation |
| --- | --- | --- |
| Version, specialization parameters, interface methods and argument/result/RDY/EN widths | M `VModInfo.hs`, `VArgInfo`, `VFieldInfo`; local `AVInst` types; B `SimBvi.hs:deriveBvi` is an extraction precedent | O(public ports + parameter encoding). Do not hash a child body into the parent's contract dependency. |
| Clock/reset ports, domains, ancestry, crossing permissions, combinational paths | M `VModInfo`, local `sp_clock_domains`, `sp_reset_list`; public clock/reset/path composition through direct children | O(public clock/reset/path surface + direct bindings). Clock numbering/tie-breaking still needs derivation. |
| Method CF/SB/SBR/C/ME/P/EXT and multiplicity | M `SchedInfo.hs:MethodConflictInfo`; `ASchedule.hs` derives `asi_v_sched_info` from local use/conflict facts | At most quadratic pair surface, plus declared groups. Preserve P/EXT as well as the common four relations. No runtime SAT. |
| `needs_progress_before(method)` and `progress_between(early,late)` | Candidate projection of M `rulesBeforeMethods` and `rulesBetweenMethods` to public method names only | O(M + M²), independent of hidden rules. The projection's semantic sufficiency is **unproved**. |
| Public input/observation dependencies and inactive-use closure | Local `asi_method_uses_map`, `asi_rule_uses_map`, resource-allocation facts, conditional uses and direct-child paths; needs new compiler extraction | O(local call sites + declared boundary dependencies) in the private artifact. A parent's representation must not import child call sites. Whether public projection closes all dynamic cases is **open**. |
| Public method availability/admission response | Computed by the owning scheduler using private Esposito, urgency, ready and disjointness facts | One response per method-channel observation, not a vector of child CAN_FIREs. Cross-boundary ME equivalence is **unproved**. |
| Local guard sampling recipe | M local AExpr/defs and `aInlineStableCone` where applicable; additional Verilog-supported conditional-use cases need extraction | Private guard DAG retained once per specialization. Not a public list of guarded alternative schedules. Stable guard support alone is insufficient. |
| Phase capabilities: sample, settle, commit, final, reset; private next-time deadline | Primitive clock/reset/tick declarations and B executor capabilities | Constant capabilities plus public domains; deadlines identify opaque endpoints, never internal work. Correct phase composition is **unproved**. |
| Public observation descriptors and effect/stop capability | Public methods plus owner-local symbol/observation descriptors; local foreign-action classification | Symbol paths may be used for observation, not schedule decisions. A complete cross-boundary effect ordering contract is **missing**. |
| Optional subordinate-executor bridge ports for an RTL wrapper | **No corresponding field exists in B `BviContract`**; requires an explicitly declared wrapper cut and public child protocol | O(explicit cut ports + direct subordinate bindings). Needs design-owner approval of the bridge specification and a real Verilator witness. |

M `SchedInfo.hs` itself comments that rule names in
`rulesBetweenMethods` do not exist on the boundary. Its current
`rulesBetweenMethods` and `rulesBeforeMethods` contain rule IDs, and
`ASchedule.hs:makeRuleBetweenEdges` introduces nodes for paths through
submodule calls. Copying those paths or IDs into a new contract is not a
valid extraction. A prototype must replace them with public obligations
*during local derivation*, using virtual public-port constraints for direct
children. Erasing names only after the old linker has merged the design
would violate P0 even if the final serialized object looked small.

## 3. Candidate state machine

Each module owns a persistent private scheduler. On an edge its states are
`Idle -> Sample -> Active -> Commit -> Final -> Idle`. `Active` can suspend
on a **public method-channel obligation** and later resume. Errors and a
latched finish request are orthogonal: finish does not imply abandoning
the current edge's state updates.

The sketch operations are:

- `begin(edge, sampled_clock_reset_inputs)`: create an edge epoch; take
  required primitive shadows and proven pre-edge guard samples locally.
  Recursion reaches direct children only. Shared time delivery is allowed.
- `offer(method, channel, args)` / `close(method, channel)`: the caller
  either offers that scheduled channel's use or declares no further use
  for this edge. A public method may have multiple allowed calls; a close
  terminates a compiler-declared use opportunity, not the entire interface
  after an arbitrary first caller.
- `observe(method, args)` / `admit(method, channel)`: let the owning module
  settle the compiler-declared predecessors for that observation/admission.
  It can return `Pending(public-input-obligation)` or `Ready(value/token)`.
  A token is scoped to that module, public method/channel and edge epoch;
  it never identifies a private rule or segment.
- `resume(token)`: re-enter the same owner's continuation when a relevant
  public obligation changes. A parent may continue other **local** work or
  coordinate a sibling through its own direct binding; it does not steal
  the child's worklist. Repeated calls use an ordinary recursive call stack
  or opaque continuation trampoline, not a collected descendant plan.
- `finish_edge`: discharge open uses, complete owner-local work, deliver
  public phase obligations, then commit/tick/reset/final in the required
  order. The BVI commit includes a batch of coincident edges for one model.

Offers and closes are provisional protocol concepts, not permissions to
speculate irreversible method effects. A pure readiness computation must
not accidentally execute a BDPI value function twice. Arbitrary guard
retries and post-hoc rollback of effects are not acceptable substitutes.

Two distinct sampling rules are essential:

1. A guarded alternative proven stable is selected from its specified
   pre-edge sample, before relevant writes. Independent instances own
   independent choices; no `2^K` alternative product exists.
2. This does **not** freeze every CAN_FIRE at the beginning of an edge.
   PulseWire/RWire readiness can become true after an early public call.
   M Bug898 is the concrete seed: `start -> display -> display2 -> rule_ran`.
   The toy experiment disproves blanket guard freezing for that interaction.

Closing absent conditional uses is necessary to avoid waiting forever on
an inactive early call. But closure may depend on another child observation
or a WILL_FIRE decision. The candidate currently has no complete proof that
these decisions can always be obtained without a circular wait for every
Verilog-supported input. That is the main progress question, not an
implementation detail to hide behind a retry limit.

## 4. Concrete attempted counterexamples

### C1: completing a child once per edge is insufficient

Public interface: `start` action and `done` value. Initially both internal
pulses are false. `start` creates a pulse; a private child rule relays it;
`done` observes the relayed pulse in the same edge. Required boundary
observation is true. Running the child before the parent misses the newly
created pulse; running it only after the parent's `done` observes false.
Owner-local progress at `done` can return true without exporting the relay
rule. `local_protocol.py` checks precisely this supplied model, including
an arbitrary private relay length. It is not a proof about compiled BSC.

### C2: freezing every rule predicate pre-edge is insufficient

The same `start -> relay -> done` fixture has relay CAN_FIRE false at edge
entry and true after start. The blanket-snapshot candidate observes false.
Selective stable guard sampling plus an interface observation frontier
observes true. This disproves a shortcut, not the proposed complete protocol
(which remains unproved).

### C3: unconditional union of possible dynamic calls blocks a legal-shaped case

The BSV fixture has local parent order `first < second`, child public order
`early < private relay < late`, and calls `first: if c then late`,
`second: if !c then early`. The union of possible calls demands relay before
first and after second, although only one direction is active on an edge.
`local_protocol.py` demonstrates actual public-use closure for each c; the
BSV compiler/reference acceptance is still untested. Both parent rules fire,
so ADynSched's disjoint-CAN_FIRE condition does not describe this case.

### C4: local legality alone does not prove compositional progress

A parent waiting for a child's RDY while the child waits for the caller to
close an input is a possible protocol wait cycle. It is not automatically
an illegal BSV combinational cycle: a different elaborated combinational
ordering may resolve the conditions, or the candidate may have waited on
an inactive use. No legal BSC witness establishing such a deadlock has yet
been minimized. A bounded watchdog may detect failure, but may not classify
a required input as outside scope. The exact missing theorem is in §6.

### C5: effects and finish can distinguish otherwise legal schedules

M `SimExpand.hs:flattenCombSchedGraph` stable-sorts qualified scheduling
nodes (`AScheduleInfo.hs:Ord SchedNode`, `Id.hs:cmpIdByName`) before sorting
the graph; it can drop foreign-function-only bias edges if needed to break
cycles. M `SimMakeCBlocks.hs:mkMERuleInhibits` derives inhibitors from the
*merged execution prefix*. These are semantic dependencies to replace,
not reusable implementations. Public pairwise scheduling alone has not
been shown to reproduce their observable effect ordering.

Next minimization: an autonomous effectful child, an effectful parent, and
two public interactions forcing only part of the child work between them;
vary hidden child names/extra private rules while keeping public facts
fixed. Compare exact Bluesim stdout/stderr and finish-edge state. If the
required oracle order depends on an unpublishable hidden rank, record the
smallest counterexample and obtain a requirement decision. It is premature
to claim either that such a counterexample exists or that effect ordering
is automatically compositional.

M `tests/regress/FinishEdge.bsv` (under `src/trs`) and
`tests/interactive/FinishPeek.bsv` explicitly witness that post-finish
state writes still land while later display output is suppressed;
`src/bluesim/dollar_display.cxx` checks `bk_finished`. Therefore aborting
the edge at finish and comparing only output is unsound. A finish token
must suppress the applicable effects but retain the remaining state/phase
work. `$stop` resume position, BDPI behavior after finish, and mixed BVI
finish need their own reference observations; no blanket all-effects rule
has been proved here.

## 5. BVI below BSV is not BSV below BVI

B `src/trs/tests/bvi/PosWrap.bsv` is
`BSV top -> synthesized BSV wrapper -> BVI WrapShow`. It exercises forwarded
parameters. It does **not** execute an interpreted BSV child beneath opaque
RTL. `BviContract` declares only top-level ports, methods, clocks/resets,
parameters, paths and source-build inputs; the Verilator shim operates one
model handle and has no subordinate BSV executor binding.

An RTL module may instantiate BSC-generated RTL for a BSV child. Verilating
that closure executes the child inside the RTL model and fails the mixed
executor witness. Simply connecting a BVI peer and a BSV peer from an outer
BSV module also does not make the BSV module a child owned by the opaque
wrapper. M `VModInfo.hs:VArgInfo` has parameter/port/clock/reset/inout
arguments, not an executable-module callback; M `Error.hs:EInterfaceArg`
also forbids separate code generation of an interface-argument module.
These source facts establish the **missing present mechanism**, not that
all possible bridges are inexpressible.

A bounded proposed witness is an explicitly authored RTL wrapper cut:

- The wrapper owns one private counter and emits public subordinate
  request/argument/enable signals; a separately synthesized BSV child owns
  its own state and supplies result/RDY signals.
- For the reference, a thin RTL shell connects these cut ports to ordinary
  BSC-generated child RTL. The shell is reference-only.
- For P0, the wrapper is Verilated without child RTL. Its adapter owns one
  direct opaque child executor binding and maps cut ports to that child's
  public methods, preserving parent/child ownership rather than delegating
  a flat bridge worklist to the kernel.
- The wrapper adapter must alternate declared port settling and child
  admission/observation until the specified frontier; clock/reset delivery
  and commit are explicitly batched. The child is then swapped for an
  equivalent executor without changing the generic parent.

This requires a reviewed cut-port protocol and extraction/binding story.
Do not insert DPI callbacks into the existing build and claim support:
B `trs-vlt/src/lib.rs` deliberately refuses models emitting a DPI header.
Do not bypass that refusal. Whether an explicitly authored cut meets the
intended imported-RTL surface is a design-owner decision before P0b.

B timing support is real: `trs-vlt/src/lib.rs` always requests `--timing`,
and `trs-interp/src/bvi.rs:advance_to` drains private events before publishing
new current-time drives. Observation is publish/settle/read; commit is
non-clock settle, simultaneous raw-clock changes/eval, then EN clear/settle.
However `timing-outclock` remains a refusal: multiple output-clock edges
between commits can be lost. For correctly annotated two-state designs
within the current handoff's combined timed-event/output-clock surface,
this is **required-but-unimplemented**, not evidence those capabilities are
out of scope. This does not imply that every arbitrary rejected Verilog
construct is required; the refusal inventory records scope/review conditions.

## 6. Invariants and the argument still owed

The model experiment establishes only I1/I2 for its tiny implementation and
the explicit toy ladders. All semantic invariants below require compiler
extraction and actual references before a P0 pass.

1. **Opacity:** every executable lookup is own-local or a direct binding;
   no ancestor stores qualified descendant rule IDs/segments. Tokens name
   only public obligations and epochs. Child snapshots remain child-private.
2. **Representation:** one prepared artifact per unique specialization;
   instance state is separate. A fixed wrapper uses constant-size own code
   and direct binding independent of hidden R or deeper D.
3. **Admission safety:** each admitted action belongs to the same legal
   fire set and method ordering as the applicable compiler reference.
   Local urgency/conflicts stay local; a method decision includes the
   consequences of private contenders without exposing their identities.
4. **Snapshot safety:** pre-edge selectors and required primitive shadows
   cannot change when resumed. Live wire readiness must still respond to
   correctly ordered preceding actions. Cross-boundary exclusivity must
   prevent a destructive update from enabling a second excluded rule.
5. **Exactly-once effects:** a suspended action cannot repeat or speculate
   BDPI/tasks; action-value cookies/results remain bound to the exact call.
   Required effect and stop order is preserved, not sorted afterwards.
6. **Phase safety:** coincident edges, gate/reset levels, BVI timed wakeups,
   commit and FINAL crossings follow the designated time/phase contract.
7. **Substitution:** two executors satisfying the same public contract and
   matching public transition/observation semantics are contextually
   interchangeable. Same signatures alone do not imply equivalence.
8. **Progress:** a legal module composition eventually resolves every
   required observation/close and completes an edge, with a cost charged
   to local executed work and genuine public dependency changes.

A possible induction begins with leaf-local scheduler correctness and
substitution of each child's public transition system. It needs a lemma
that the projected public obligations are sound *and complete* for all
allowed caller contexts, including conditional uses, multi-call correlation,
ME, effects and phase changes. Then it needs a well-founded measure showing
that recursively waiting on a genuine public obligation decreases remaining
work or yields a finite notification. Independent acyclic local graphs do
not themselves prove this lemma: a circular wait can cross the boundary.

An edge counter or finite retry budget is not a progress proof. A whole-
design dependency graph/toposort is not an allowed proof implementation.
A mathematical global trace used to specify equivalence is permissible;
constructing that trace as an executable plan is not. No such global plan
is constructed by the toy experiment.

The main open obligations are therefore: complete local compiler extraction
for Verilog dynamic scope; adequacy of public before/between/closure
projection; cross-boundary ME/fire-set equivalence; compositional progress;
exact effects/finish/stop; clock/reset/FINAL composition; genuine BVI child
bridging; and byte/semantic observation equivalence. **None is waived.**

## 7. Bounds to test, not fitted performance promises

Let L be own-local executable/guard work, B the public boundary surface,
C the number of direct bindings, and E the declared local dependency edges.
The proposed representation target is O(L + E + C*B) per unique module
specialization, with O(C*B) bindings per instance; hidden descendants'
local L never appear in an ancestor artifact. A path of fixed-shape
forwarders uses O(D) total forwarding work/stack and O(1) metadata per level.
For independent instances the target coordination is O(N*B), not O(N*R).
For independent binary decisions it is K guard evaluations and O(K)
coordination per changing vector, not a table of 2^K outcomes.

The toy measures only public method transitions and leaf-local work:
`start` plus `done` costs two crossings per wrapper; a hidden relay length R
costs R leaf steps and does not change the supplied two-method contract.
Each independent choice costs one guard evaluation. It deliberately does
not measure compiler export/preparation, executable bytes, real scheduling
revisits, BVI calls, or artifacts derived from BSC. Those remain blocked.
Bounds for genuinely coupled guards/readiness are **unproved** and cannot
be extrapolated from the independent toy ladder.

## 8. Review decision requested

Review the dynamic-target gap and the public-rendezvous direction first.
Authorize a bounded extraction prototype only after deciding the BVI bridge
surface and the next effect/ME/progress witnesses. A valid outcome is to
reject this candidate and revise it. Disproving this candidate would not
prove hierarchical TRS impossible. P1 and production LLVM/JIT/AOT remain
blocked.
