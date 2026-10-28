# Running the ordinary ECO refinements

Use the shared `bluehs` runtime paired with the capture compiler, and package
these external sources as described in [README.md](README.md). No additional
compiler changes or BA extensions are required by these refinements. Keep the
SDK and its original paths with saved plans: provenance includes canonical paths
and content hashes. Run from a clean working directory without unrelated local
Haskell modules: ordinary `runghc` module search includes the current directory.

For a fresh `CaptureFlow` configuration, the recommended command is:

```sh
"$ECO_BLUEHS" "-i$ECO_SDK/lib" "$ECO_SDK/scripts/captures.hs" \
  --objective translated-normalized --refinements standard \
  --structure ordinary --invariants defer FLOW.config
```

Set `soDomain = StrictFourState` in the configuration for strict four-state-safe
validation. Selecting `Binary` explicitly permits binary SAT obligations and does
not establish four-state equivalence. The command uses all ordinary refinement
families regardless of the example being processed. `translated-raw` is an
alternative objective when raw line count is preferred; run it in a separate
output directory and report both counts against the same original OLD RTL.
Declaration grouping and order remain material. Neither objective measures a
physical ECO or proves a global minimum.

The standard provider adds these external rules to the configured ordinary
features:

- F21: compatible vacant state identities.
- F23: historical packed state views.
- F24: typed consumer-role and exact-expression wire correspondence.
- ValueNames: exact pure wire-value names, including value reads in effects.
- F25: complete typed pipeline-chain tail correspondence.
- F26LiveHelpers: reconstruct OLD live helper roles after state removal.
- LiveAliases: restore live historical output aliases and exact packed views.
- LiveShell: restore historical live helper roles while deleting disconnected OLD-only helpers; see [LIVE_SHELL.md](LIVE_SHELL.md).
- AliasOrientation: restore an OLD helper definition by reversing an existing same-width CURRENT alias; see [ALIAS_ORIENTATION.md](ALIAS_ORIENTATION.md).

The standard preset interleaves candidate families under the shared proposal
budget so a prolific early naming family cannot consume every slot before later
families receive their first proposal. A budget smaller than the number of
nonempty families still limits coverage; this is a bounded search. The generic
`combineCandidateProviders` API retains explicit caller-priority ordering.

F24Layout and ValueNamesLayout jointly compose their naming proposals with
historical layout only when F04 is enabled. OLD supplies correspondence and edit
cost evidence; CURRENT supplies the required behavior. Every accepted candidate
passes the independent CURRENT proof, and saved replay repeats that proof
without invoking its proposer. Structural normalization, the native AST
infrastructure and binary Yosys when selected remain trusted components; this
is translation validation, not a mechanically verified checker or downstream
commercial equivalence signoff.

The standard preset removes inherited F22/F22Inactive helper-shell features.
It does not package or enable unused-state retention. Explicit
`--historical-shell` / `--inactive-helpers` remain separate experimental requests.
`--structure preserve` independently enables historical case/conditional
restoration, including equal conditional arms; `--structure ordinary` disables
those F09/F10 rules. It does not disable ordinary name or layout correspondence.

F26LiveHelpers removes obsolete OLD registers and OLD-only disconnected helpers.
It retains unchanged scaffolding already present in ordinary CURRENT and may
reconstruct OLD helpers that implement CURRENT live behavior. A bypass inferred
from an OLD pipeline input is proposal evidence, never an assumed equality
between stages. The fixed-port, scalar-state-removal proposer declines instances,
functions/DPI, unsafe protected regions, ambiguous writers, width mismatches,
cycles and expansion-budget exhaustion. No temporal invariant is assumed.

The typed scripting interface is
`ECOASTRefinements.standardRefinementProvider` and
`enableStandardRefinements`. A caller can compose a different provider set through
`combineCandidateProviders` without updating the compiler. The standard semantic
search and `objective.plan` formats continue to apply. Use the objective-aware
`replay.hs` command from [README.md](README.md) to reprove a saved branch and check
its projected scoring witnesses. The complete selected AST and RTL remain the
proof and deployment artifacts; the translated projection is only a cost view.

The existing DejaGNU groups `eco_correspondence.exp`, `eco_pipeline_chain.exp`,
`eco_helper_reuse.exp`, `eco_live_aliases.exp`, `eco_live_shell.exp`,
`eco_alias_orientation.exp`, `eco_objective.exp` and `eco_proof.exp` exercise the
refinements through the shared bluehs runtime. They cover directed matching
exclusions, wrong-function controls, public BH capture, the combined CLI and
proposal budget, translated objectives, strict/binary proof boundaries and fresh
saved-plan replay. The [README test command](README.md#build-and-test-a-matched-toolchain)
also includes the existing matching, search, flow, selection and metric controls.
Private MatX examples and their result tables remain outside this repo.
