# P0a first checkpoint: blocked/inconclusive

Observed 2026-09-05. Analysis and small experiments were delegated to
`gpt-6-astra` with `ultra` reasoning, as requested. This checkpoint does not
establish full hierarchical feasibility and does not disprove it. The
production compiler/runtime are unchanged. Substantial P0b work remains
behind the design-owner review gate in [P0-HANDOFF.md](../../P0-HANDOFF.md).

## Findings that affect the architecture

1. **The existing dynamic export is too narrow for the required target.**
   At compiler pin `a9462e0e28102f07a89b3f3c478b33c806d62a3e`,
   `ASchedule.hs:verifyStaticScheduleOneRule/TwoRules` distinguishes
   Verilog-supported dynamic cases from the subset represented by
   `ADynSched`. The latter requires stable inlining and, for rule pairs,
   disjoint CAN_FIRE predicates. The original `SchedCondsDynamicSchedule`
   has two unconditional parent rules with opposite conditional child
   calls. Its `sched-conditions.exp` regression expects Verilog compile
   success with G0100 under `-sched-conditions`. This is pinned source/test
   evidence, not a fresh compiler run. The new initialized BSV variant is
   authored but uncompiled. Exporting only `asi_dyn_scheds` cannot be the
   full parity strategy. See [PROTOCOL.md §1](PROTOCOL.md#1-pins-and-the-first-material-finding)
   and inventory D05–D07.
2. **Two simple execution shortcuts fail the supplied model.** A child
   private relay must run between public `start` and `done` in the same
   edge. Running the child once before or after the parent observes false;
   the supplied observation-driven child returns true. Freezing all guards
   pre-edge also misses this pulse. Conditional-use closure lets the toy
   distinguish active early/late calls without a global schedule. These are
   counterexamples to those shortcuts, not compiler-derived proofs of the
   proposed general protocol. See inventory X01/X02 and
   [the experiment](experiments/local_protocol.py).
3. **The mixed-BVI witness needs a real child bridge.** The pinned `PosWrap`
   test is BSV above BVI, not an independently interpreted BSV child beneath
   opaque RTL. The current BVI contract/shim has no subordinate executor
   binding. [PROTOCOL.md §5](PROTOCOL.md#5-bvi-below-bsv-is-not-bsv-below-bvi)
   proposes an explicit RTL-wrapper port cut for review, not an implemented
   capability. Timed output-clock behavior is another scoped capability
   gap; existing adapter refusals do not automatically narrow requirements.

The candidate public-port protocol is intentionally not frozen. Local
contract extraction, safe admission/ME, exactly-once effects, conditional
closure and compositional progress are unresolved. Neither a finite retry
limit nor an old global plan is an acceptable substitute.

## What ran

Commands and exit codes are recorded in [results/commands.json](results/commands.json).
Raw combined evidence is in [results/all.json](results/all.json), and unit
test output in [results/unit-tests.txt](results/unit-tests.txt).

| Check | Observed result | What it establishes |
| --- | --- | --- |
| Pinned source identity | Exit 0; eight audited blobs and tracked compiler/runtime subtrees match the compiler pin; BVI reference exists | Input identity only, not source-claim correctness |
| Hand-authored model | Exit 0; explicit `compiler_derived=false`, `rtl_oracle_compared=false`, `p0_pass=false` | Narrow assertions for supplied contracts and the two shortcut counterexamples |
| Harness/index tests | 21 tests pass | Fail-closed reporting, source/witness references, explicit open coverage, and refusal to run model assertions under optimized Python; not 21 semantic parity tests |
| Tool environment | Exit 2 | BSC, RTL simulators, Rust and observation dependencies are unavailable; see [ENVIRONMENT.md](ENVIRONMENT.md) |
| Aggregate | Exit 2, `blocked/inconclusive` | The checkpoint cannot turn model success or missing tools into a P0 pass |

The model runs all 27 N/R/D/K ladder configurations requested in the
handoff, but only on hand-authored objects:

| Ladder | Observed toy counts | Claim limit |
| --- | --- | --- |
| Depth D = 1, 2, 4, 8, 16, 32 | `2*(D+1)` public calls, eight leaf steps | Recursive forwarding only; no compiler preparation or general suspension costs |
| Hidden R = 8, 32, 128, 512 | Four public calls, R leaf steps | Parent byte strings and hashes are fixed by construction; not compiler-derived child-edit isolation |
| Repeated N = 1 through 128, powers of two | `4*N` public calls, `8*N` leaf steps, one shared wrapper byte object | Does not measure actual executable sharing, preparation, binding bytes or memory |
| Independent K = 1 through 64, powers of two | `4*K` guard evaluations across four changing vectors | Independent toy choices only; not coupled dynamic admission or a general complexity proof |

The toy route guard rejects a deliberately qualified private-rule route;
it is not a reachable-code dependency audit. No real build/startup/runtime
or peak-memory scaling result is reported.

## Inventory and unexecuted work

[inventory.json](inventory.json) currently has 79 entries and 46 witness
records: 56 entries researched, 15 blocked, six untested, and two toy
shortcut candidates disproved. Of the witnesses, only the hand model ran;
one new BSV fixture is authored but untested, 15 existing seeds/audits are
referenced but unrun, and 29 witnesses are not authored. The source index
catalogs the current primitive-dispatch names and BIR operation families;
this is not a closed census of all Verilog-supported constructs or every
loader refusal. Provisional refusal classifications need review and source
or executable evidence before becoming accepted expected rejections.

No BSC extraction, Bluesim run, Verilog/Icarus comparison, real Verilator
execution, Rust interpreter, VCD/FST comparison, stop/resume driver, or
frontend leaf-edit audit ran. [The fixture recipe](fixtures/README.md)
records the intended bounded compiler/oracle commands and initializes the
new witness. Its added observations may alter scheduling and must be
checked before accepting the intended coverage discriminator.

## Next review and execution gates

1. Review the public-port direction, the Verilog dynamic-export gap, and
   whether the explicit subordinate cut is the intended mixed-BVI bridge.
   No reduction of the no-global-schedule or dynamic-parity requirements
   is proposed.
2. Continue P0a with minimized ME/effect-order/progress witnesses. Resolve
   closure and public-obligation sufficiency; distinguish failure of this
   candidate from an incompatibility among requirements. Close the semantic
   inventory and derive public contracts locally, without importing child
   private paths or deriving them from a merged plan.
3. Obtain a verified pinned BSC build and required tools; run the original
   dynamic seed and initialized variant through Verilog first. Record raw
   diagnostics, timed state, methods and effects. Do not substitute a
   different compiler revision or use a simulator limitation as a scope cut.
4. Only after design-owner review, implement the bounded Rust P0b extraction/
   interpreter and real mixed-BVI witnesses. Measure actual structural and
   scaling gates, executor substitution, and declared build dependencies.
   Report production `.bo`/`.ba` invalidation separately.

P1, ABI freezing and production migration remain blocked. The verdict is
**blocked/inconclusive**, not "established for the declared target."
