# P0 handoff: hierarchical TRS feasibility

Status: assignment, 2026-09-05.  P0 has not passed.  This handoff is part of
[design PR #177](https://github.com/MatX-inc/bsc/pull/177); it does not start
production migration or freeze an executor ABI.

## 1. Assignment and authority

Construct and justify a reusable module-local execution protocol that
preserves the required TRS behavior, including guarded dynamic scheduling
and mixed BVI execution, without a global rule or segment schedule at any
stage.  Demonstrate both semantic sufficiency and hierarchical scaling.
The protocol is the deliverable, not an assumed input to this assignment.

**Dynamic scheduling means parity with the BSC Verilog backend at the
pinned compiler revision, not the full generality of possible dynamic
scheduling schemes.**

Read [DESIGN.md](DESIGN.md) §§0, 0.1, 4, and 10 first.  Those requirements
govern; this file supplies the operational handoff.  Legacy `HANDOFF.md`,
`BIR.md` §4, code comments, and old test-runner defaults are reference
material, not permission to retain global scheduling or whole-edge fusion.

Use these immutable input pins; branch names are only navigation aids:

| Input | Repository and revision | Purpose |
| --- | --- | --- |
| Architecture decision | [MatX-inc/bsc, f7cd263877d3](https://github.com/MatX-inc/bsc/commit/f7cd263877d3b712ebf06b7536d50a5707a88529) | No-global-schedule design; this handoff adds execution details |
| Compiler and legacy TRS baseline | [MatX-inc/bsc, a9462e0e2810](https://github.com/MatX-inc/bsc/commit/a9462e0e28102f07a89b3f3c478b33c806d62a3e) | `trs/development` base of the design PR; Bluesim reference and source inventory |
| BVI implementation reference | [nanavati/bsc, d4a72767c63e](https://github.com/nanavati/bsc/commit/d4a72767c63e6dbdf569cac4c61aeb8afc597c04) | Audited `claude/trs-bvi-verilator` snapshot; BVI contracts, adapter, and fixtures |

Start implementation on a separate topic branch from the revision containing
this handoff in `MatX-inc/bsc`.  Record that SHA as well as the input pins.
Do not follow historical remote/push instructions or silently replace a pin
with a newer branch head.  A necessary reference change needs a recorded
reason and a new comparison baseline.  No private knowledge-base draft is
required to start; unresolved claims referenced only there remain unresolved.

## 2. Scope and milestones

**P0a: first reviewable result.**  Produce the semantic inventory, a candidate
protocol/state machine, its invariants, and the strongest attempted
counterexamples.  For every proposed contract field, show how it is derived
from the owning module and its immediate children's public contracts, and
why its size does not track hidden descendant rules.  Explain pre-edge
sampling, local choices, boundary suspension/resumption, progress, and
effect/clock/reset ordering.  Resumption is permitted, not a predetermined
solution.  Obtain design-owner review before substantial P0b implementation;
small experiments to challenge P0a are encouraged.  P0a is not a P0 pass.

**P0b: executable evidence.**  Build an unoptimized Rust interpreter spike,
suggested location `src/trs/spike/p0-hierarchy/`.  Keep it separate from
production execution paths.  Reuse local expression/primitive helpers only
after auditing their dependencies; do not import the legacy design linker,
merged compositions, global inhibitors, or flat execution planner.

Hand-authored IR and test doubles are useful during P0a, but are insufficient
for P0b acceptance.  Demonstrate the proposed extraction path on actual
BSC module-local outputs, including static and guarded-dynamic cases, with
no global-schedule preparation step.  Prototype compiler/export changes are
in scope when needed to expose local facts.  BSC still owns scheduling
legality/conflict reasoning; the TRS runtime must not redo SAT reasoning.

Use a real Verilator-backed BVI executor for the foreign-boundary witnesses,
not only a hand-written behavioral substitute.  Include interpreted BSV
above and below an opaque BVI boundary: specify and exercise the bridge to
the independently executed BSV child.  Compiling that child into the BVI RTL
model does not satisfy this witness.  Replacing an equivalent child executor
must leave the generic parent's artifact and behavior unchanged.

P0 must demonstrate child-edit isolation for generic TRS contracts, fragments,
prepared code, and their declared build inputs.  Prove that the required
compiler information is available through the same boundary; a fixture-only
stub with no derivation from BSC is insufficient.  Audit and report existing
`.bo`/`.ba` naming/hash invalidation separately using
[BOUNDARY-CONTRACT.md](docs/BOUNDARY-CONTRACT.md).  Completing the production
frontend refactor is follow-on work, not a prerequisite to the prototype's
feasibility verdict.  Record its failing gates and migration dependencies;
do not claim end-to-end compiler isolation from a TRS-only result.  If the
proposed semantics themselves need child bodies or a global schedule, P0
is blocked; that cannot be deferred as a hashing fix.

LLVM/JIT/AOT implementation, production packaging, complete CLI/debugger
plumbing, optimization, and parallel execution are out of P0 implementation
scope.  Their boundary semantics remain in scope: use small API/observation
drivers and executor substitution to demonstrate them.  Full production
coverage belongs to P1-P3, after the protocol evidence is accepted.

## 3. Semantic target and oracle policy

Close the DESIGN.md §0.1 matrix against BSC scheduling constructs, BIR
operations, native primitives, and the BVI contract.  The target is not
merely the subset the current TRS implementation accepts.  Each inventory
entry records its source, required interaction, owning module/kernel,
witness IDs, oracle, comparison, reduction argument if any, and status.

For dynamic scheduling, inventory the constructs and behaviors supported by
the pinned BSC Verilog backend.  Its generated RTL is the primary reference;
Bluesim's support limits do not narrow this target.  Conversely, a scheduling
scheme outside that backend's capabilities is not a required P0 feature.
Document that boundary with compiler/source evidence, not a speculative list
of every legal TRS rule ordering.  Missing simulator tools or a failed
candidate implementation are not evidence that a case is outside scope.

Include static and conditional method uses; action/value/action-value calls;
CF/SB/SBR/C relations, urgency, ME inhibition and pre-edge snapshots; dynamic
pair and conditional self-call cases; independently choosing instances and
parent/child/sibling dependencies; combinational paths; stateful primitives;
BDPI and system-task effects; clocks/resets/crossings; BVI observations and
commit; interactive state access and VCD/FST behavior.  Test interactions
between these classes, not just each class in isolation.  In particular:

- No inherited single-module dynamic restriction or 16-combination cap may
  exclude Verilog-supported behavior.  Independent choices must not be
  enumerated as a whole-design product.  This does not require inventing
  additional dynamic-scheduling capabilities beyond the Verilog backend.
- BVI includes the declared clock/reset/path/method surface, parameters,
  same-cycle observation, coincident-edge batching, and private timed events.
  Exercise actual `--timing` behavior and wakeups between clock edges;
  the old M0 README's untimed-only scope is not the current target.
- Classify every existing refusal as an invalid contract, an explicit
  DESIGN.md non-goal, outside the defined Verilog-backend dynamic scope,
  or a required-but-unimplemented capability.  Only the first three can be
  expected rejections, with evidence for the classification.  Correct BVI
  annotations are an input assumption; designing a general Verilog contract
  checker is not P0.
- DESIGN.md's non-goals remain unchanged, including general four-state
  simulation and event-kernel co-simulation outside the BVI contract.

Choose and record the comparator before interpreting a test result:

| Surface | Reference and acceptance |
| --- | --- |
| Non-dynamic behavior supported by Bluesim | Build the pinned compiler/reference; compare stdout, stderr and exit status, plus timed architectural-state checkpoints and relevant effects. Preserve the required order, not merely final register values. |
| Dynamic scheduling | Run BSC-generated RTL for the same BSV through an independent simulator. Match the Verilog backend's supported scheduling behavior and contractually observable results, including timed state/method/effect observations. Bluesim is supplementary where applicable; arbitrary simulator event-region races do not define a new rule-order requirement. A missing general-purpose scheduling feature beyond this backend's capability is not a P0 failure. |
| BVI | Run the same BSV and imported RTL through BSC's Verilog flow with an independent simulator (the BVI branch uses Icarus). Record the reference compiler, simulator and flags. For required non-dynamic interactions neither reference supports, review an explicit semantic specification and independently implemented small reference model before claiming parity; this cannot expand the dynamic target. |
| Clocks, resets, stop/finish, interactive access | Compare event times/phases, edge counts, reset transitions, required effect ordering, stop/resume behavior and visible state. A print-only test is insufficient for suppressed output after finish. |
| Waveforms | For Bluesim-backed cases, retain VCD byte comparison with only date-field normalization. For Verilog-backed cases, declare the corresponding signal/observation mapping. Compare FST through decoded hierarchy, widths, aliases and timed values, retaining ordering where observable; compressed FST bytes are not the oracle. |

Use deterministic initialized fixtures for comparisons with a four-state
Verilog reference.  Any unavoidable two-state/reference difference needs
an explicit, reviewed comparison rule; do not copy broad startup/output
filters from legacy scripts.  Simulator banners may be classified separately
from design effects, but never remove `$display` or `$finish` effects merely
because they differ.  Never sort effect traces to manufacture agreement.

An oracle disagreement blocks the applicable claim.  Do not silently replace
the designated reference behavior with "some legal TRS order."  For dynamic
cases, Verilog-backend parity governs; do not add a requirement to reproduce
an incompatible incidental Bluesim ordering.  Disposition any ambiguous or
racy reference behavior explicitly before claiming parity.  If required
observable ordering appears incompatible with the boundary, submit that
counterexample and the needed requirement decision.  Reference engines may
construct global schedules only in their separate processes; neither those
schedules nor derived planning metadata may feed the P0 engine.

## 4. Structural and scaling acceptance

Instrument export, contract derivation, preparation, binding and execution
separately.  Count unique artifacts, analysis/preparation invocations,
prepared executable bytes, binding/protocol bytes, boundary transitions,
guard evaluations and dependency revisits.  Separate instance state and
observation storage from executable/planning metadata.  Include cold runs
with persistent caches disabled so reuse is architectural, not a warm-cache
artifact.  A module-local artifact is prepared once per unique specialization.

These are minimum controlled ladders; keep the external contract fixed
except for the intentionally varied direct-instance wiring:

| Ladder | Sizes | Required result |
| --- | --- | --- |
| Repeated identical leaves, N | 1, 2, 4, 8, 16, 32, 64, 128 | One leaf analysis/preparation; shared leaf executable bytes unchanged. Binding size and coordination work for this independent-leaf fixture are O(N), not O(N × hidden leaf rules). Necessary leaf execution/state may grow with N. |
| Hidden leaf rules/defs, R | 8, 32, 128, 512 | With fixed methods and child bindings, only the leaf's executable representation grows. Generic ancestor artifacts and scheduling metadata remain unchanged; increased leaf execution work is allowed. |
| Single forwarded interaction through depth D | 1, 2, 4, 8, 16, 32 | Constant local scheduler/binding size per fixed-shape wrapper; O(D) total forwarding work and stack/protocol state. No ancestor accumulates descendant schedules. |
| Independent binary dynamic choices, K | 1, 2, 4, 8, 16, 32, 64 | Per-instance choice state and O(K) guard/coordination work for this fixture; no preparation or runtime enumeration of 2^K schedules. Exercise changing choice vectors, not one fixed selection. |
| Leaf edit and equivalent executor swap | Each representative static, dynamic and mixed-BVI fixture | Parent contract/fragment/prepared hashes unchanged and parent preparation not rerun. Verify declared build inputs exclude child bodies and demonstrate the changed leaf actually executes. |

Assert deterministic counts and artifact identities, not noisy wall-clock
ratios alone.  State symbolic bounds and their constants from the protocol
before fitting measurements.  For genuinely coupled fixtures, justify costs
in terms of local executed work, declared dependencies and propagation
depth; do not rename speculative whole-design search as necessary work.
Report build/startup/run time and peak memory as supporting measurements;
throughput competitiveness is not a P0 requirement.

Add dependency/route checks that reject global graphs, qualified descendant
rule/segment plans, per-edge global worklists, and body-reading parent/link
paths.  Audit the reachable preparation and execution code, not merely
function names.  Include a negative test proving the guard detects a
deliberately forbidden dependency.  A generic call stack, module-local
worklists and shared time/event delivery remain allowed as defined in §0.1.

## 5. Deliverables and decision

Deliver the following under the spike directory (paths are to be created
by P0; no runner is supplied or claimed to pass by this handoff):

- `PROTOCOL.md`: module state machine, proposed boundary schema, local
  extraction/derivation, invariants, and a compositional argument covering
  child substitution, arbitrary nesting, safety and progress.  Justify
  termination/no deadlock for the legal cases, and diagnostics for illegal
  cycles.  Separate proved claims, assumptions and remaining obligations;
  a finite test suite alone is not the argument.  A machine-checked theorem
  is not required for P0, but the reasoning must be reviewable.
- `inventory.json` and fixtures: the complete semantic inventory, per-witness
  source/tool pins, initial conditions, stimuli, oracle/comparator,
  reductions, expected observations, bounds and status.  Local semantic
  reductions still need regression tests; omitted features are not passes.
- `README.md` and `run.sh`: clean-checkout setup and separate semantic,
  structural and scale modes plus an aggregate mode.  Record exact tool
  versions, command lines and inputs; bound every test run.  Missing tools,
  timeouts, required-case skips or absent references must produce non-success,
  not an aggregate pass.  Use isolated output directories; retain raw traces
  and counters without committing generated binaries or caches.
- `RESULTS.md`: reproducible evidence, counterexamples, remaining frontend
  integration gates, and one verdict: **established for the declared target**,
  **candidate disproved**, or **blocked/inconclusive**.  The first requires
  every DESIGN.md §0.1 gate, closed inventory and reviewed composition
  argument.  Resource/tool failures and unreviewed semantic assumptions are
  inconclusive, never success.

A counterexample includes the smallest design, local contracts and initial
state, required versus observed behavior, and the missing information or
failed invariant.  Distinguish an implementation bug, an insufficient
candidate protocol, and a possible incompatibility among requirements.
Disproving one candidate is not a proof that all hierarchical approaches
are impossible.  A blocked result is a valid handoff outcome but does not
permit P1.  Design-owner acceptance of the evidence precedes contract
freezing and production migration; scope or semantic relaxation needs an
explicit decision, not a fallback flag.

## 6. Source map and useful starting witnesses

Paths below are relative to the repository root at the indicated pin.

| Reference | Read for |
| --- | --- |
| MatX, `src/comp/`: `AScheduleInfo.hs`, `SimPackage.hs`, `SimExpand.hs`, `SimMakeCBlocks.hs` | Local compiler facts and the global scheduling mechanisms being replaced; retain semantics, not the merged execution plan |
| MatX: `src/trs/trs-bir/SimExportIR.hs`; `src/trs/crates/trs-ir/src/`: `schedule.rs`, `merge.rs` | Existing export/schema; `between` child-rule identities and `dyn_alternatives` are known boundary/scaling hazards, not the proposed ABI |
| MatX, `testsuite/bsc.bugs/bluespec_inc/`: `b898/Bug898.bsv`, `b1302/RFile2.bsv`; `testsuite/bsc.scheduler/sched-conditions/SchedCondsDynamicSchedule.bsv` | Seed interleaving, guard/snapshot and dynamic witnesses; minimize and extend them across boundaries |
| MatX, `src/trs/tests/`: `regress/FinishEdge.bsv`, `interactive/FinishPeek.bsv`, `vcd/`; `src/bluesim/` and `src/trs/crates/trs-interp/src/` | Effects, primitive/kernel behavior, state visibility and waveform reference material |
| nanavati, `src/trs/crates/`: `trs-ir/src/bvi.rs`, `trs-interp/src/bvi.rs`, `trs-vlt/src/`; `src/trs/tests/bvi/`: `run-r2.sh` through `run-r5.sh` | Actual BVI contract/adapter and acceptance/refusal inventory; test `PosDelay`, `PosClocks`, `PosShadowFixed`, `PosWrap` and add genuine mixed nesting |
| MatX: `src/trs/docs/BOUNDARY-CONTRACT.md`, `src/trs/tools/ba-stability-audit.sh` | Separate compilation and leaf-edit audit; distinguish prototype evidence from frontend integration completion |

The old regress runner insists on compiled execution; the interactive and
BVI runners also invoke legacy link paths.  Reuse their fixtures/reference
setup, not their execution path as the P0 implementation.  The BVI M0 README
predates timed-event support present in `PosDelay`/R5; inventory actual source
and tests rather than treating historical status paragraphs as requirements.
