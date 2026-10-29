# Candidate families

Every candidate is a complete `VProgram` proposed by a `CandidateProvider`
(`ECOASTSearch`), and no provider can accept anything. The one gate is
`ECOASTSearch.tryCandidate`: a candidate is skipped when it is binary-only in
the `StrictFourState` domain or changes the module ports; it is rejected
with a `.rejected` note when `lexicallyEmittable` or `declaredBeforeUse`
fails; it is scored and must strictly improve the objective; and it is
proved against the current program by the request's `searchProof`
(`runASTProofWithPins` with the capture's native child pins, the configured
solver, domain and timeout) before it can be accepted. The best accepted
candidate of each round becomes the next current program and one step of
`search.plan`; saved-plan replay re-applies both gates and re-proves every
step. Budgets: `soCandidates` proposals per round and `soRounds` rounds;
features not in `soFeatures` are not generated, and an unknown feature is an
error. Each family is a proposal only: OLD supplies names, order and layout
evidence and never a replacement value.

The corpus column counts the cases of the private 53-case corpus whose final
search receipt accepted a step of that family (the cases themselves are not
public). Those runs used the preview
producer (`3b310af1e`); the cleaned stack has not rerun the corpus, and 18 of
the cases do not compile on this lineage, so the column is evidence of what
each family did, not of this build.

| Code | Provider (PR) | Proposes | Enabled when | Corpus cases | Proof gate |
|---|---|---|---|---|---|
| F01 | `ECORename` (#246) for the initial rename (`match.hs`); `ECOASTTransform` (#247) for width-growth read aliases | the simultaneous rename of eligible internal names from the OLD/NEW graph match, and unique register-read alias identities across width growth | default (`featureTable`); the rename only when a match plan is supplied | 36 cases | `optimizeAST` scores the rename first and proves it (`proof-names`) with the inverse relation; alias candidates go through `tryCandidate` |
| F02 | `ECOASTTransform` (#247) | unique complete late process identities (state names from identical process bodies) | default | 2 private cases | `tryCandidate` with the state map as alpha |
| F03 | `ECOASTTransform` (#247) | historical process order within the current containers | default | 3 private cases | `tryCandidate` |
| F04 | `ECOASTTransform`, `ECOASTDeclarations` (#247) | historical declaration, member and group layout with current types; populated-signal order of assignment groups; declaration partitions with new state | default | 16 private cases | `tryCandidate` |
| F06 | `keep.hs` (#246) over `-eco-backend-plan` (#243); not a generator | a checked helper-keep plan from `proposeKeeps` replayed natively as the kept module | default; only when the keep proposal is nonempty | 2 private cases (selected) | the kept module must be proved equivalent to ordinary NEW before it is searched against |
| F07 | `ECOASTTransform` (#247; `restoreRoles`, `cloneProcesses`) | historical typed roles that compute current values; selective clones of complete pure processes (binary-only when a clone needs it) | default | 5 private cases | `tryCandidate` |
| F08 | `ECOASTTransform`, `ECOASTAliases` (#247) | historical alias representatives computing the current value; inlining of new-only exact-width pure helpers at their consumers, with or without historical layout; role restoration that needs a non-alias definition | default | 8 private cases | `tryCandidate` |
| F09 | `ECOASTTransform` (#247) | historical explicit case-arm skeletons with the current default; a historical default recovered from an exhaustive binary case (binary-only) | default; removed by `OrdinaryStructure` (`ECOStructurePolicy`) | 2 private cases | `tryCandidate` |
| F10 | `ECOASTTransform` (#247) | historical equal-arm conditional templates | default; removed by `OrdinaryStructure` (`ECOStructurePolicy`) | none | `tryCandidate` |
| F11 | `ECOASTTransform` (#247) | typed direct relational forms for negated comparisons, exact range simplification under a bit guard, comparison complements with swapped branches, guarded historical pure cones, grouped Boolean forms; all binary-only | default; never tried in the strict domain | 3 private cases | `tryCandidate`, Binary miter |
| F15 | `ECOASTTransform`, `ECOASTIdentity` (#247) | historical state identity from unchanged typed output roles (injective simultaneous state rename) | default | 1 private case | `tryCandidate` with the inverse state map |
| F21 | `ECOASTVacancy` (#247, over `ECOASTIdentity`) | compatible vacant historical state-group names, ranked by output-role votes, with bounded alternatives; a narrower payload after a packed-state split | `vacancyProvider`, or the standard refinement preset (`ECOASTRefinements`) | 4 private cases | `tryCandidate` with the inverse relation |
| F23 | `ECOASTStateView` (#247) | one existing combinational aggregate of complete NEW registers takes a vacant OLD packed-state name when the typed input packing is identical | `stateViewProvider`, or the standard preset | 2 private cases | `tryCandidate` |
| F24WidthRole, F24ExactLeaf, F24Layout | `ECOASTCorrespondence` (#250) | vacant OLD continuous-wire names recovered after width growth from uniquely corresponding ordered leaves; names for unique exact typed terminal expressions (occupied names move simultaneously); `F24Layout` composes a naming proposal with up to four F04 layouts | the standard preset; `F24Layout` only when F04 is enabled | F24Layout: 3 private cases; the two plain variants accepted no step | `tryCandidate` |
| ValueNames, ValueNamesLayout | `ECOASTValueNames` (#250) | vacant OLD names for unique exact typed pure wire expressions, including value reads inside assertion and effect statements; the layout variant adds up to four F04 layouts | the standard preset; the layout variant only with F04 | ValueNamesLayout: 1 private case | `tryCandidate` |
| F25 | `ECOASTPipelineChain` (#250) | complete unbranched typed register chains aligned to OLD tail identities, or extra head stages given unused names, as one injective permutation (at most 4096 chain pairs examined) | the standard preset | 3 private cases | `tryCandidate` with the inverse map |
| F26LiveHelpers | `ECOASTHelperReuse` (#250) | OLD live helper roles reconstructed after state removal: obsolete OLD state and OLD-only disconnected helpers removed, unchanged CURRENT scaffolding kept, several drivers and consumers changed together | the standard preset (`enableStandardRefinements`) | 1 private case | `tryCandidate` |
| LiveAliases | `ECOASTLiveAliases` (#250) | after a pipeline insertion, an OLD packed output view restored by rebinding its existing data/valid aliases to the current output values and reconnecting the new stage's inputs to the aliases' previous values, jointly | the standard preset | 1 private case | `tryCandidate`; the checker's exact concat-field projection identity makes it strict-eligible |
| LiveShell | `ECOASTLiveShell` (#250) | the F22 pure-shell role proposals projected onto the live cone: disconnected OLD-only continuous helpers removed, CURRENT's unchanged unused scaffold kept; modules with more than 512 continuous scalar definitions on either side are ineligible | the standard preset | 1 private case | `tryCandidate`; the Boolean-reassociation variant is binary-only |
| AliasOrientation | `ECOASTAliasOrientation` (#250) | when CURRENT has `a = b; b = e;` and OLD had `a = e;`, propose `a = e; b = a;`: same-width unique scalar wires with explicit writers, pure expressions, no cycle; single and disjoint-pair proposals | the standard preset | 1 private case | `tryCandidate` |
| F22 | `ECOASTShell` (#247) | a historical pure combinational shell with CURRENT values at the typed consumer roles (helper declarations placed with `withDeclarations`) | `shellProvider` only; not in the standard preset | not in the corpus preset | `tryCandidate`; the operand-order variant is binary-only |
| F22Inactive | `ECOASTInactive` (#247) | OLD inactive constant helper definitions retained at their original declaration and assignment groups | `inactiveProvider` only | not in the corpus preset | `tryCandidate` |

## What the families share

- Fixed ports, instance identities, functions and DPI declarations: no
  family may touch them, and the checker rejects a candidate that does.
- Directive-bearing comments and translate-off regions are barriers for the
  naming families (`directiveFree`), and procedural or translate-off writers
  reserve their names.
- Declaration partitions, widths, wrappers and comments are preserved by
  every family except where its row says otherwise; new declarations are
  placed before their first use (`withDeclarations`).
- The standard preset interleaves the families under the shared
  proposal budget (`balancedCandidateProviders`) so a prolific family cannot
  consume every slot; `combineCandidateProviders` keeps caller priority
  order. A budget smaller than the number of nonempty families still bounds
  coverage.
- A declined, unvisited or budget-limited proposal is not a proof that no
  better correspondence exists; an accepted one is not a physical-ECO
  measurement.

## Normaliser identities (strict domain)

`ECOASTProof` accepts without a solver, in either domain, rewrites that are
identities over settled four-state values of typed operands: typed
comparison complements (`!(a <= b)` against `a > b`, `!(a == b)` against
`a != b`, with context-sized operands), logical `&&`/`||` reassociation and
reordering with repeated operands and the Z-to-X truth conversion kept,
exact typed concatenation-field projections (every 0/1/X/Z bit copied),
equal-arm conditional collapse, width-preserving renames and alias
substitutions, and reductions treated as one-bit self-determined operators.
`eco_proof_fuzz` runs 3,000 random pairs against Icarus and seeded wrong
rewrites must be rejected with a simulation witness. Candidates whose
correctness needs two-state reasoning (`acBinaryOnly`) are proposed only for
the Binary domain.
