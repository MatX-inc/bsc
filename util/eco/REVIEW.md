# Reviewing the bluehs ECO flow

The series adds six external layers on top of the native ECO facilities and
the existing bluehs runtime. It changes Haskell policy and public tests, not
compiler passes or the native codec. See [README.md](README.md) for commands and
[COMPATIBILITY.md](COMPATIBILITY.md) for the upstream/preview AST distinction.

## Review order

| Layer | Start here | Responsibility | Directed DejaGNU groups |
|---|---|---|---|
| 1. Packaging, proof and matching | `package-sdk.hs`, `lib/ECOASTProof.hs`, `lib/ECOMatch.hs` | Bind runtime/source identity; prove typed candidates; audit correspondence; authenticate native rendering | `eco_sdk`, `eco_proof`, `eco_matching`, `eco_filters`, `eco_renderer`, `eco_relational_complement`, `eco_alias_history`, `eco_logical_ac` |
| 2. Typed edits and search | `lib/ECOASTTransform.hs`, `lib/ECOASTSearch.hs`, `lib/ECOASTCost.hs` | Bounded external proposals, independent proof, transactional selection and saved replay; optional historical policies | `eco_rules`, `eco_search`, `eco_shell`, `eco_vacancy`, `eco_state_view` |
| 3. Observations and translated objectives | `lib/ECOASTInvariant.hs`, `lib/ECOASTTranslatedMetric.hs`, `lib/ECOASTTranslatedObjective.hs` | Reprove supported observations; score raw and substantive changes over synthesis-translated regions | `eco_invariants`, `eco_core_without_observations`, `eco_metrics`, `eco_translated_metrics`, `eco_objective` |
| 4. Ordinary matching refinements | `lib/ECOASTRefinements.hs`, `lib/ECOASTCorrespondence.hs`, `lib/ECOASTPipelineChain.hs`, `lib/ECOASTHelperReuse.hs` | Width/value roles, complete pipeline correspondences, live helper/alias reuse, fair bounded proposal families | `eco_correspondence`, `eco_pipeline_chain`, `eco_helper_reuse`, `eco_live_aliases`, `eco_live_shell`, `eco_alias_orientation` |
| 5. Complete capture flow | `lib/ECOASTFlow.hs`, `lib/ECOASTReplay.hs`, `captures.hs`, `lib/ECOASTSelection.hs`, `examples/run.sh` | Capture adaptation, helper requests, branch/prefix selection, shared fresh ordinary-NEW-to-output replay and usage guide | `eco_flow`, `eco_guide`, `eco_selection`, objective/helper flow controls |
| 6. Declaration finishing | `lib/ECOASTDeclarationOrder.hs`, `lib/ECOASTFinishOrder.hs`, `finish-order.hs` | Preserve material declaration structure and accept finishing changes only after objective, independent proof and replay checks | `eco_declaration_order`, public finish CLI/tamper controls |

Every prefix packages only files present in that layer. Earlier proof and
objective tests run their standalone checks; later layers add flow controls.
The last layer extends the guide's complete command sequence. Core qualification
checks unavailable observations with zero facts. Native-positive observation
controls require the optional producer and its matched runtime/SDK, selected by
`ECO_NATIVE_OBSERVATIONS=1`; synthetic proof checks run in either qualification.

## Correctness and cost

OLD supplies naming, ordering and textual-cost evidence. Ordinary NEW defines
the behavior a candidate must preserve. An ECO intentionally changes OLD's
behavior, so a successful candidate-to-NEW proof does not establish OLD-to-NEW
equivalence. Hand-edited targets are never inputs to matching or acceptance.

Inspect the proof domain and supported construct checks in `ECOASTProof` before
reviewing the transformations. Binary SAT and settled four-state checks have
different scopes. Unsupported obligations must not become successful proofs.
Saved replay checks the accepted steps again; prefix selection independently
checks its candidates and records the selected objective.

Raw and normalized costs always use the same OLD baseline. Normalization omits
ordinary informational comments and whitespace/wrapping; declaration grouping,
member order and directive-bearing structures remain material. Candidate-to-
direct distance is informational, and textual cost is not a physical ECO-size
or QoR measurement.

## Compiler and policy boundary

The compiler supplies complete intern snapshots, authenticated native replay,
retained RTL AST and renderer/child metadata, checked helper-retention requests,
typed checkpoints, and optional bound observations. Its existing printer and
serialization functions are available through the compiled `bsc` library.

Matching, candidate generation, proof policy, search, ordering and metrics are
external bluehs modules. `CandidateProvider` is the extension point for a new
ECO rule; policy updates require repackaging these sources rather than a
compiler update. `package-sdk.hs` records the actual library/runtime identities
and copies policy sources. Use its explicit `--check` before and after a run.

`optimizeAST` takes one `SearchRequest`. Start with `defaultSearchRequest`
and override the named provider, proof runner, contracts, renames or objective.
Each `SearchInput` binds a program to its complete renderer. `SearchResult`
reports complete-file costs (`srCost`, `srOrdinaryCost`) separately from the
selected objective (`srObjectiveCost`, `srOrdinaryObjectiveCost`) and its
verified cost envelope. Persisted plans retain their existing schema and costs.

`--vacant-state` enables historical state-name alternatives, including typed
output projections. `--state-view` can give an existing aggregate wire the OLD
packed-state name. These options retain NEW storage, reset and transition
semantics; matching evidence is not itself a correctness proof.

## Scope and qualification

Module ports are fixed and instance renaming is unsupported. New state is
allowed. The flow operates on retained native ASTs rather than parsing RTL.
The existing invariant consumer handles independently proved combinational
assertions. Temporal contracts and temporal candidate acceptance are outside
this series.

The public tests use the existing DejaGNU infrastructure and bluehs, with Yosys
for SAT obligations and Git for textual costs. Missing optional tools produce
unsupported results. No Python runtime or separate Haskell runner is added.
Public toolchain qualification and results obtained using another compiler
version or adapter must retain their separate producer identities.

The final declaration-order refinement also includes a coupled wire-name and
internal declaration-order proposal. Review `ECOASTJointDeclarationOrder.hs`
with the schema-2 finishing receipt: same surviving state/group identity and
shape, unique vacant scalar-wire correspondence, selective value alpha rename,
exact serialized inverse preservation, and permutations within existing rows.
`ASTJointDeclarationOrder.hs` exercises ambiguous/occupied/protected identities,
arrays/initializers, metadata, strict proof, failed-best fallback and saved replay.
The private corpus and direct references remain outside this repository.
