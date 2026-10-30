ECO test groups
===============

Every group uses the ordinary DejaGNU harness. `make -C testsuite/bsc.eco
check` runs the native groups; the external groups live in `postba/` and run
through `make -C testsuite eco_<group>.check`. `TEST_RELEASE` selects the
compiler installation and `TEST_BLUEHS` the bluehs runtime; the Haskell
programs run through the shared `bluehs` hook in `testsuite/config/unix.exp`
and are `UNTESTED` without it; the external groups find their SDK, launcher
and tools once per group with `eco_sdk_env` and run their programs with
`eco_run_script` (both in `testsuite/config/unix.exp`). Install `bsc` and
`bluehs` from the same build. Each group keeps its artifacts in a `work_*`
or `eco-*` directory removed by the normal cleanup.

Environment: `ECO_BLUEHS` (bluehs launcher; overrides the `bluehs` the
harness finds), `ECO_SDK` (a packaged SDK, see `util/eco/README.md`), `ECO_YOSYS`,
`PRODUCTION_BSC` (a second compiler for ordinary-output comparisons; untested
when unset), `CORE_REPLAY_BSC` (an executable of a different native version
for the adaptation version guard; untested when unset), `ECO_LIBDIR` (an
extra `-i` library directory for the dependency group), `ECO_FUZZ_COUNT` and
`ECO_FUZZ_SEED` (fuzz budget), `ECO_NATIVE_OBSERVATIONS=1` (enables the
observation-producer controls; needs the optional producer). A missing SDK,
runtime, Yosys or Icarus makes a group `UNSUPPORTED`, never a pass. No
Python, RTL parser or external RTL tool other than Yosys and Icarus is
needed.

Native groups
-------------

| Group (PR) | Establishes |
|---|---|
| `intern/intern.exp` (#241) | Each script runs in a fresh process. `Capture.hs`: the initial table is empty and a snapshot records every spelling with its id and the counter. `Restore.hs`: a restore reproduces relative id order and spellings, allocation continues at the saved counter, a second restore is rejected. `Reentrant.hs`/`RestoreReentrant.hs`: a nested lookup of one spelling takes one id, and that state survives restore. `MethodName.hs`: compiler-generated method names keep consistent ids. `DuplicateRestore.hs`, `Invalid.hs`, `Empty.hs`, `Late.hs`: duplicate spellings, a negative counter, missing, repeated or out-of-order ids, and a restore after a live allocation are rejected without changing the table, while an empty snapshot round-trips and allocation then starts at zero; `InternTest.hs` covers spellings that collide under the interner's hash. |
| `replay/options.exp` (#242) | `-eco-replay` and `-eco-adapt` validate their required `-eco-output` before opening a capture; `Invocation.hs` checks every mode and conflict of `parseECOOptions`, including an unknown `-eco-` option. |
| `replay/sidecar.exp` (#242) | A fresh process restores the intern table, outputs and headers of a capture; captures record flags, paths, bytes and escaped hash filenames; a bad schema, bad producer, changed working directory, modified payload or dependency bytes are rejected. |
| `replay/dependency.exp` (#242) | Multi-module replay with a Verilog filter, a BDPI foreign dependency and library lookups: exact replay bytes for each module and for a symlinked library alias, rejection of modified filter or foreign bytes and of a changed original lookup identity, a preserved foreign dependency symlink, and ordinary-output parity with `PRODUCTION_BSC`. |
| `replay/incremental.exp` (#242) | Capture regenerates every module-producing package under `-u` even when ordinary outputs are current, for a first build, a cached build and a recapture; an imported helper without a synthesised module is rebuilt; each captured module replays exactly and strictly. |
| `replay/native.exp` (#242, #253) | Native capture, exact and repeated replay, production parity, companion export and native `VProgram` retention; ECO options come from `argv` only (`BSC_OPTIONS` can neither redirect a replay nor request a capture); capture refuses an existing stem and leaves the source intact; a replay or adaptation whose output would alias, hard-link into, or lie inside the capture creates nothing there; a modified native payload is rejected; adaptation reproduces the ordinary bytes, writes its receipt and companions beside the new capture, rejects a tampered captured output before any write and a forged output digest at the byte control, and leaves no divergent RTL, capture or receipt on failure. |
| `backend/backend.exp` (#243) | On the `FanoutLoss` fixture: capture, an empty plan and a keep plan replay with the separate ordinary-byte control, actual keep survival, no port file or control directory written by a plan replay, rejection of plans with a wrong schema, module, capture digest, preopt digest, type, RHS, protected, unknown or duplicate request without emitting RTL, rejection of every output path that would overwrite a companion (per suffix, through symlinks and hard links), a populated plan directory and an adaptation inside the capture; `BackendContract.hs` plans and verifies keeps, `BackendPlanContext.hs` interleaves two invocations of the same module name, `BackendSafety.hs` rejects undetermined, opaque and cyclic cones, invalid graphs, latch and state eligibility and port changes. |
| `verilog-info/verilog_info.exp` (#253) | `util/eco/lib/ECOASTLexical.hs` is textually the native predicate section; captures retain the native AST and typed child metadata, render exactly and refuse relocation; a comment with an embedded newline, an identifier containing `;` and a wire used before its declaration are refused before any byte is written; `Gates.hs` captures every fixture of this suite (13 modules) and each passes both gates and emits its captured bytes; replay refuses to overwrite bound companions; with `CORE_REPLAY_BSC`, adaptation rejects a different native version before running any control or capture and preserves every source artifact. |

External groups (`postba/`)
---------------------------

| Group (PR) | Establishes |
|---|---|
| `eco_sdk` (#246) | `package-sdk.hs --check` accepts the matched runtime, library and sources, a relocated copy checks its own tree, and a changed library digest, changed source digest, modified copy or unregistered file is rejected. |
| `eco_proof` (#246) | `ASTProof.hs`: the checker's acceptance and rejection rules with real Yosys (fixed ports, inventory, alpha rules, opaque skeletons, child pins including the X-literal and width negatives, strict residual failure, receipts with subjects); `ASTConcatProof.hs`: exact concat-field projections, including Icarus runs over all 256 four-state vectors and an incorrect-slice negative. |
| `eco_proof_fuzz` (#246) | 3,000 random reference/candidate pairs (`ECO_FUZZ_COUNT`, `ECO_FUZZ_SEED`): every strict structural verdict agrees with Icarus four-state simulation; seeded wrong rewrites are rejected with a witness. |
| `eco_relational_complement`, `eco_logical_ac` (#246) | Directed strict controls for comparison complements and logical reassociation, plus an exhaustive X/Z Icarus oracle. |
| `eco_matching` (#246) | `ASTPlan.hs` (keep proposals from distinct pre/final graphs, stage negatives), `Rename.hs`, `MatchTest.hs`, `GraphTest.hs`, `AggressiveTest.hs` (injective, typed, maximum-cardinality assignments, determinism under permutation). |
| `eco_filters`, `eco_renderer` (#246) | Captures with identity, macro and candidate filters: ordinary and candidate byte controls; string-parameter rendering, layout and filter guards; a filter that changes the AST rendering cannot qualify silently. |
| `eco_rules` (#247) | `TransformTest.hs` (the eleven-code feature table, every generated candidate passes both gates, 140 directed checks) and `TransformProofTest.hs` (proof gates with Yosys, `proveTautology` controls). |
| `eco_search` (#247) | `ASTSearch.hs` (acceptance, hash-chain tamper rejection, gated initial rename) and `ASTExternalRules.hs` (an unbundled rule through the frozen SDK with negative controls for ports, behaviour, evidence, source mutation, duplicate registration, a use-before-declare and a comment-escape candidate, and a partial cost provider). |
| `eco_shell`, `eco_vacancy`, `eco_state_view` (#247) | `HistoricalShell.hs`, `InactiveHelpers.hs`, `PreservationBarriers.hs`, `StateVacancy.hs`, `StateView.hs`: each family's candidates, barriers, strict/binary proofs and saved replay; every candidate is emittable. |
| `eco_metrics`, `eco_translated_metrics` (#249) | Authenticated raw and normalized counts over captures and candidates, the packaged `measure.hs` CLI with the same OLD, comment/whitespace/structural controls, translate-off toggle controls, and five git-configuration sentinel/control pairs (ten checks) showing that repository attributes, local, global and ambient configuration and `core.autocrlf` cannot change a count. |
| `eco_objective` (#249, #251) | Translated objectives: search, proof and tamper controls, flow and branch selection, accepted-prefix selection, the packaged flow and `replay.hs --flow`. |
| `eco_invariants`, `eco_core_without_observations` (#249) | Synthetic obligations are independently bound and proved; a core capture without a companion reports `unavailable-in-capture` with zero facts and explicit deferral; with `ECO_NATIVE_OBSERVATIONS=1` and the optional producer, proved and unproved captures are checked. |
| `eco_correspondence`, `eco_pipeline_chain`, `eco_helper_reuse`, `eco_live_aliases`, `eco_live_shell`, `eco_alias_orientation` (#250) | One directed candidate/proof/replay program per family with its exclusions and wrong-function controls; `eco_helper_reuse` also runs the standard refinement preset end to end (bounded family breadth, packaged CLI, native translated flow and saved replay). |
| `eco_flow` (#251) | `ASTFlow.hs`: a complete library flow with a schema-6 report, a bound `effective.config` and a `verified/` receipt. |
| `eco_guide` (#251, #252) | `util/eco/examples/run.sh` end to end (package check, captures, configure, search, finishing, both metrics, `replay.hs --flow`, byte comparison); `FinishOrderCLIControls.hs` (five input tampers) and `FlowReplayControls.hs` (record, report, plan, candidate and witness tampers; a relocated copy still verifies; roots inside the flow are refused; a forged `SUCCESS`-printing solver is refused and never invoked). |
| `eco_binary_flow` (#251) | `BinaryFlow.bs` in the Binary domain: the fresh verification receipt binds the root solver by path and digest; every fresh proof passed and at least one needed the solver (`prove.ys` with the root timeout, `proof.log` with the `SUCCESS` line and its digest in the receipt subject, `miter.v`, `reference.ast`, `candidate.ast`), and the selected step's own search receipt is a SAT-decided pass; a behaviour-changing twin's candidates are `rejected` as `inequivalent` with a model in the solver log and none reaches its plan; a flow configured with a forged `SUCCESS`-printing solver records its passes, but under the real roots `replaySelectedFlow` and `replay.hs --flow` refuse that flow, and a relocated copy whose `effective.config` names the forged solver, at the solver binding without invoking it, while a consistently relocated copy of the SAT-decided flow verifies. The fixture's one accepted step (the F08 swap-negate-left Boolean form of a restored OLD helper) equals NEW only under binary valuation, so the step is solver-decided and, under the default whole-file objective, strictly cheapest (cost 2 against the structural candidate's 6 and ordinary's 11, no tie-break); on the twin every solver-decided candidate is rejected and only structurally proved steps are kept, with no residual root in any kept step's fresh replay proof. |
| `eco_lexical_gates` (#251) | `LexicalGateControls.hs`: the selected branch's accepted step artifact is tampered three ways (a comment element with a line break ending in an `assign`, an internal identifier that ends its statement, the leading declaration block moved after its uses); each is refused by the pure predicates and by `runASTProofWithPins` as `unresolved` with exactly the issue `lexical`, `lexical` or `declaration-order` naming the offender (also in the rendered receipt), and after a consistent hash-chain rebind (step hash, plan output, branch plan identity) `replaySelectedFlow` and `replay.hs --flow` refuse the copy as not emittable; the untampered relocated copy verifies first. |
| `eco_selection` (#251, #252) | `select.hs` with roots, `SelectionPolicy.hs` and `SelectionReject.hs` (six negatives on the kept plan, including swapped adapted sides). |
| `eco_declaration_order` (#252) | `ASTDeclarationOrder.hs` and `ASTJointDeclarationOrder.hs`: ambiguous, occupied and protected identities, arrays and initialisers, metadata, strict proof, failed-best fallback and saved replay. |
| `eco_finish_changed` (#252) | `FinishChanged.bs` (OLD declares registers `alpha` and `beta`; NEW retires `alpha` and adds `gamma := value + beta`) run by `FinishChanged.hs` through the library with features F01 and F21 (the vacancy provider, no layout feature) under the whole-file objective; the public `finish-order.hs ROOTS FLOW OUT` then reports `changed` (raw 26 -> 16, substantive 14 -> 8) with one `DeclarationOrder` step, a fresh search proof and a saved-plan replay proof (`replay/proof-1`, reference = the finishing input, candidate = its output), a receipt whose status is `changed` with `freshSavedPlanReplay` and `inputEqualsOutput = false`, every evidence identity re-hashed and bound, and the `// register alpha` group moved back before `beta`. |

Groups that need Yosys are `UNSUPPORTED` without it (`eco_flow` included,
because the flow's own verifier hashes the root solver); `eco_proof_fuzz`,
`ASTConcatProof.hs` and the two X/Z oracles need Icarus.
