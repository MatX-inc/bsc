# ECO utilities

The native compiler can capture a scheduled module and replay its Verilog
backend byte for byte. The bluehs scripts in this directory use two such
captures, OLD and NEW, to produce RTL whose text is close to OLD while every
accepted edit is proved against ordinary NEW: OLD supplies names, order and
cost evidence; NEW defines the behaviour. Module ports stay fixed, instance
renaming is unsupported, new state is allowed, no RTL is parsed and no
hand-edited target is an input. [REVIEW.md](REVIEW.md) names the functions a
reviewer must trust, [RULES.md](RULES.md) lists every candidate family, and
[COMPATIBILITY.md](COMPATIBILITY.md) lists the format tags and the version
binding.

## Build and package

Build bsc and bluehs from one source tree ([INSTALL.md](../../INSTALL.md),
[bluehs](../bluehs/README.md)). The bluehs launcher exports `GHC_ENVIRONMENT`
naming the compiled `bsc` package; `package-sdk.hs` reads it.

```sh
make install-src
util/bluehs/setup.sh
repo=$(pwd)
export BSC="$repo/inst/bin/bsc"
export BLUESPECDIR="$repo/inst/lib"
export ECO_BLUEHS="$repo/util/bluehs/bluehs"
export ECO_SDK="$repo/eco-sdk"
export ECO_YOSYS=$(command -v yosys)
"$ECO_BLUEHS" util/eco/package-sdk.hs "$BSC" "$ECO_BLUEHS" "$repo" "$ECO_SDK"
"$ECO_BLUEHS" "$ECO_SDK/scripts/package-sdk.hs" --check "$ECO_SDK/build.manifest"
```

Packaging copies `util/eco/lib/*.hs` to `$ECO_SDK/lib` and `util/eco/*.hs` to
`$ECO_SDK/scripts`, and refuses to run unless those directories match
`bluehs.modules` and `bluehs.scripts` exactly, so an unregistered module cannot
run from a stale SDK. `build.manifest` (schema 4) records the compiler, the
bluehs launcher, the GHC environment file, the compiled `bsc` library files,
the exact `bsc -v` and GHC versions, and every copied source as a
manifest-relative path with its SHA-256. `--check` re-hashes the tree holding
the manifest: a relocated copy checks itself, a modified copy or a stray `.hs`
file under `lib/` or `scripts/` fails. The manifest is provenance for the
verifiers below, which bind its hash; it does not sandbox the host. Repackage
after any change to `util/eco`.

Run every script as `"$ECO_BLUEHS" -i "-i$ECO_SDK/lib" "$ECO_SDK/scripts/X.hs"
...`: the bare `-i` clears bluehs's default search path so a module in the
current directory can never shadow a packaged one. Yosys supplies SAT proofs,
Git supplies line counts, `sha256sum` (or `shasum`) supplies identities, and
Icarus (`iverilog`, `vvp`) is needed only by the four-state oracle tests.

## Test

The native suites and the external groups use the existing DejaGNU harness;
[testsuite/bsc.eco/README.md](../../testsuite/bsc.eco/README.md) describes
every group. With `ECO_SDK`, `ECO_BLUEHS` and `ECO_YOSYS` exported:

```sh
make -C testsuite/bsc.eco check TEST_RELEASE="$repo/inst" TEST_BLUEHS="$ECO_BLUEHS"
make -C testsuite eco_sdk.check eco_proof.check eco_proof_fuzz.check eco_matching.check \
  eco_rules.check eco_search.check eco_invariants.check eco_core_without_observations.check \
  eco_flow.check eco_filters.check eco_relational_complement.check eco_logical_ac.check \
  eco_vacancy.check eco_shell.check eco_metrics.check eco_guide.check eco_selection.check \
  eco_state_view.check eco_translated_metrics.check eco_renderer.check eco_objective.check \
  eco_correspondence.check eco_pipeline_chain.check eco_helper_reuse.check \
  eco_live_aliases.check eco_live_shell.check eco_alias_orientation.check \
  eco_binary_flow.check eco_lexical_gates.check eco_declaration_order.check \
  eco_finish_changed.check TEST_RELEASE="$repo/inst"
```

A missing bluehs, SDK, Yosys or Icarus makes a group `UNSUPPORTED`, never a
pass. The complete public example below is `eco_guide.check`.

## Capture OLD and NEW

Compile each source revision with its usual flags, `-verilog -elab`, and
`-eco-capture DIR`. `-u` is safe: capture regenerates every package that
produces a module even when ordinary outputs are current.

```sh
mkdir -p old-build old-capture
"$BSC" -u -verilog -elab -g mkDemo -bdir old-build -vdir old-build \
  -info-dir old-build -eco-capture old-capture Demo.bs
# NEW checkout, same module ports:
mkdir -p new-build new-capture
"$BSC" -u -verilog -elab -g mkDemo -bdir new-build -vdir new-build \
  -info-dir new-build -eco-capture new-capture Demo.bs
```

A capture directory holds, per module, `mkDemo.ba` (the native package, copied
unchanged, with the retained final `VProgram`), `mkDemo.flags` (effective
flags), `mkDemo.intern` (the complete intern table), `mkDemo.output-N.v`
(every generated output) and the manifest `mkDemo.replay`, which binds all of
them, the producer executable, the working directory, dependencies and
Verilog filters by SHA-256. Beside them the compiler writes the companions
`mkDemo.eco-preopt` and `mkDemo.eco-graph` (typed definition graphs before and
after optimisation), `mkDemo.eco-ast` (envelope), `mkDemo.eco-ast.bin`
(header, `VProgram`, child instances) and `mkDemo.eco-ast.output.v` (the
ordinary bytes). Every check precedes the first write and an existing stem is
refused, so a failed capture leaves nothing behind. Filters must be bare
executables. Keep the producer binary, the working directory, libraries and
filters: a capture is not a portable archive.

The four ECO options are read from `argv` only; `BSC_OPTIONS` still supplies
ordinary flags to compilation and is never consulted by replay or adaptation.

| Invocation | Effect |
|---|---|
| `bsc ... -verilog -elab -eco-capture DIR SRC` | ordinary compilation plus a capture of every generated module |
| `bsc -eco-replay CAP/mkDemo.replay -eco-output DIR` | same executable, same working directory: restore the intern table, rerun the backend, require the captured bytes; a divergent `.v` is removed; companions are written beside the output |
| `bsc -eco-replay CAP/mkDemo.replay -eco-output DIR -eco-backend-plan PLAN` | replay with a checked helper-keep plan ([RULES.md](RULES.md), F06) into a new or empty `DIR`; the fixed ports are compared with the capture's retained `VProgram` |
| `bsc -eco-adapt CAP/mkDemo.replay -eco-output DIR` | another executable of the same full native version: reproduce the captured bytes into `DIR/current-control`, recapture into `DIR/capture` (companions beside it) and write `DIR/adapter-receipt` |

Replay and adaptation take no ordinary arguments, run from the captured
working directory, require the same `bsc -v` version, and refuse any output
that would overwrite a bound file or lie inside the capture directory
(through symlinks and hard links too). Exact replay also requires the same
producer SHA-256. Adaptation never runs the original producer; its receipt
records the original producer by identity only. A different compiler version
needs fresh captures.

## Configure

```sh
"$ECO_BLUEHS" -i "-i$ECO_SDK/lib" "$ECO_SDK/scripts/configure.hs" strict \
  "$BSC" mkDemo \
  /abs/old-capture/mkDemo.replay /abs/new-capture/mkDemo.replay \
  /abs/old-source-directory /abs/new-source-directory \
  "$BLUESPECDIR" "$ECO_SDK/build.manifest" "$ECO_YOSYS" \
  /abs/fresh-eco-output /abs/demo.config
```

`demo.config` is a `CaptureFlow` record (`Read`/`Show`): compiler, module,
both captures, both working directories, both libraries (`cfOldLibrary`,
`cfNewLibrary`, initialised to `LIB`), the output directory, the search
options `cfSearch` (`soDomain` = `StrictFourState` for `strict` or `Binary`,
`soFeatures`, `soRounds` 8, `soCandidates` 64, `soYosys`, `soProofSeconds` 30),
the child-compiler timeout `cfCommandSeconds` (120) and the SDK manifest.
Edit it as text when a capture used another library or budget. The file is
also the trust-roots file of every verifier below: keep it outside the flow
directory.

## Search

```sh
"$ECO_BLUEHS" -i "-i$ECO_SDK/lib" "$ECO_SDK/scripts/captures.hs" \
  --objective translated-normalized --refinements standard \
  --structure ordinary --invariants defer /abs/demo.config
```

`captures.hs [--objective whole|translated-raw|translated-normalized]
[--refinements standard] [--structure preserve|ordinary] [--invariants
export|defer] [--vacant-state] [--state-view] [--historical-shell]
[--inactive-helpers] FLOW.config`; `--objective` must come first and defaults
to `whole`, `--structure` to `preserve`, `--invariants` to `export`. The
options only choose the candidate provider, the feature list and the scoring
policy ([RULES.md](RULES.md)); the runner is `ECOASTFlow.runCaptureFlow` and
it does the following, in order, under `cfOutput`:

1. writes `effective.config`, the configuration it actually runs with, as the
   flow's first artifact;
2. adapts both captures with the configured compiler (`adapted-old/`,
   `adapted-new/`, each holding `current-control/`, `capture/` and
   `adapter-receipt`), reads the two `capture/mkDemo.eco-ast` snapshots,
   requires that each renders to its captured bytes and that the fixed ports
   agree;
3. proposes helper keeps from the four graphs (`keep-proposal`); when any
   exist and F06 is enabled, writes `keep.plan`, replays NEW with it into
   `kept/`, matches NEW to the kept graph (`keep-control-match.eco-plan`) and
   proves ordinary NEW equivalent to the kept module (`proof-keep/`,
   `proof-keep.json`); a failed kept branch is recorded and skipped;
4. for each branch (`ordinary`, and `kept` when proved) matches OLD to the
   branch graph (`<branch>-match.eco-plan`, when F01 is enabled) and runs
   the bounded search into
   `<branch>-search/`: `search.plan` (the accepted steps, schema 3),
   `steps/N.ast`, `selected.ast`, `selected.v`, `objective.plan`, the
   `objective/` scoring witnesses, every candidate's `.ast`/`.v`, its
   `.proof.json` or `.rejected` note, and `summary.txt`;
5. selects the branch with the smaller objective cost, never worse than
   ordinary NEW (otherwise `ordinary-rollback`), emits `mkDemo.v` through the
   capture's renderer and filters, and writes `flow-objective.report` and
   `flow.report` (schema 5, binding the configuration, every input, the
   branch plans and the output);
6. calls the shared verifier on its own output directory with roots derived
   from `effective.config` (`verified/`, see Replay below) and fails unless the
   freshly replayed RTL equals `mkDemo.v`;
7. writes `summary.txt` and `selected-invariants/status.json`; with
   `--invariants export` it also rebinds and re-proves the optional
   observations (`selected-invariants/report.json`, `assertions.svh`).

The child compilers run from the captured working directories with
`BLUESPECDIR` set to the configured library and `GHCRTS` scrubbed; their logs
are `<label>.log` and `<label>.log.command`.

The objective drives the initial rename, every accepted step, branch choice
and the final survival check. `whole` counts Git Myers added-plus-deleted
lines of the emitted RTL against OLD's; `translated-raw` counts the same
over the synthesis-translated projection (native `translate_off` regions
removed by the printer's own toggles), and `translated-normalized` over that
projection with wrapping, blank lines and informational comments removed.
Declaration grouping and member order are material in every metric. The
scoring witnesses under `objective/` are never deployed RTL; `mkDemo.v` is.
None of the objectives proves a global minimum or a physical ECO cost.

`--structure preserve` keeps the historical case and equal-arm conditional
families F09/F10 enabled; `ordinary` removes them. `--refinements standard`
adds the external naming, pipeline-chain, live-helper and alias families and
removes the F22 shell policies; `--vacant-state` (F21) and `--state-view`
(F23) add those families alone; `--historical-shell` (F22) and
`--inactive-helpers` (F22Inactive) are separate experimental policies.
`--invariants defer` leaves the optional observation export explicit; the
core compiler writes no observation companion, so `export` reports
`unavailable-in-capture` with zero facts unless the optional producer made
the capture. Nothing exported ever enters a proof as an assumption.

## Finish declaration order

This stage and its `eco_declaration_order` group arrive with #252; the
commands above are complete without it.

```sh
"$ECO_BLUEHS" -i "-i$ECO_SDK/lib" "$ECO_SDK/scripts/finish-order.hs" \
  /abs/demo.config /abs/fresh-eco-output /abs/finished-eco-output
```

`finish-order.hs ROOTS.config FLOW OUT` first replays the selected flow
freshly into `OUT` (exactly `replay.hs --flow`), then runs
`ECOASTFinishOrder.finishDeclarationOrderFrom` on the verified program into
`OUT/finish`: it scores the two finishing families of [RULES.md](RULES.md)
(`DeclarationOrder`: historical order of intact CURRENT declaration blocks;
`JointDeclarationOrder`: vacant wire identities coupled with internal order
inside a surviving register group) with the translated raw and normalized
metrics, tries the improving proposals in lexicographic
(normalized, raw) order, and accepts the first one whose fresh proof and
saved-plan replay pass; the solver, domain and timeout come from the roots.
`OUT/finish` holds `input.ast` and `input/mkDemo.v`, one `search[-N]/` and
`replay[-N]/` per attempt with `attempt-N.json`, `selected.ast`, `mkDemo.v`,
`metric/`, `finish-order.report` and `receipt.json` (schema 2; `status` is
`changed` or `unchanged` with the reason, both cost pairs and any forward
wire map). A no-op leaves byte-identical input and output.

## Measure

```sh
"$ECO_BLUEHS" -i "-i$ECO_SDK/lib" "$ECO_SDK/scripts/measure.hs" \
  /abs/eco-output/adapted-old/capture/mkDemo.eco-ast \
  /abs/eco-output/adapted-new/capture/mkDemo.eco-ast /abs/ordinary-metrics
"$ECO_BLUEHS" -i "-i$ECO_SDK/lib" "$ECO_SDK/scripts/measure.hs" --translated --candidate \
  /abs/eco-output/adapted-old/capture/mkDemo.eco-ast \
  /abs/eco-output/kept/mkDemo.eco-ast \
  /abs/eco-output/kept-search/selected.ast /abs/eco-output/mkDemo.v /abs/selected-metrics
```

`measure.hs [--translated] OLD.eco-ast NEW.eco-ast OUT` compares two
snapshots; `--candidate OLD.eco-ast CONTEXT.eco-ast CANDIDATE.ast ACTUAL.v
OUT` compares a candidate AST rendered in its branch context (`adapted-new`
for `ordinary`, `kept` for `kept`) with its actual emitted RTL, which must
match the rendering byte for byte. Always compare automatic and direct
outputs with the same OLD. `report.json` (schema 2) carries typed inputs
(`{"kind":"snapshot",...}` or `{"kind":"candidate",...}` with path and
sha256), `old_rtl`/`new_rtl`, every evidence identity,
`equivalence_proof_performed: false`, and one count object per policy with
`added`, `deleted` and `changed`:

| policy | counted text |
|---|---|
| `whole-file-raw-v1` | the actual RTL bytes (the search's own `whole` objective) |
| `whole-file-normalized-v1` | `old.normalized.v`/`new.normalized.v`: the native printer with wrapping, indentation, blank lines and informational comments removed; comments carrying known tool or directive markers, native macros and translate markers are kept |
| `translated-raw-v1` | `old.translated.v`/`new.translated.v`: the projection re-rendered with the capture's header policy (`--translated`; the whole-file report sits under `whole/`) |
| `translated-normalized-v1` | `old.translated.normalized.v`/`new.translated.normalized.v` |

Git runs without a repository, system, global or ambient configuration, so
`.gitattributes`, diff drivers, filters or `core.autocrlf` cannot change a
count. The projection follows the ordered native `translate_off`/`on` flags
only, rejects textual switches in comments or headers, keeps active helpers
even when only excluded assertions use them, and evaluates no macros. The
normalized count is a separate metric, not raw minus a subset, and may
exceed raw. No count proves equivalence, and a candidate AST is hashed, not
proved, by this command.

## Select an accepted prefix

```sh
"$ECO_BLUEHS" -i "-i$ECO_SDK/lib" "$ECO_SDK/scripts/select.hs" [--translated] \
  /abs/demo.config /abs/fresh-selection /abs/eco-output ...
```

`select.hs [--translated] ROOTS.config OUT FLOW...` loads up to 16 flows
against the roots (they must share the authenticated OLD and ordinary NEW
ASTs, RTL and child metadata), measures every accepted prefix of every
recorded `ordinary` and `kept` plan, writes `frontier.json` (every point and
the Pareto set over raw and normalized counts, whole-file or translated), and
for `RawFirst` and `NormalizedFirst` replays the chosen prefix freshly
(`replayFlowPrefix`, including the ordinary-to-kept bridge proof), measures it
again, and writes `selection.json`; `completed.json` follows a final recheck
of every binding. Selection never generates candidates or reinterprets a
saved search's objective.

## Replay a selected flow

```sh
"$ECO_BLUEHS" -i "-i$ECO_SDK/lib" "$ECO_SDK/scripts/replay.hs" \
  --flow /abs/demo.config /abs/eco-output /abs/fresh-flow-replay
```

`replay.hs --flow ROOTS.config FLOW OUT` is the one verifier
(`ECOASTReplay.replaySelectedFlow`), shared by the flow itself, by
`finish-order.hs` and by `select.hs`. The compiler, both captures, the SDK
manifest, the proof domain, the solver and the timeout come from the roots
file, which is refused when it lies inside the flow directory. The verifier
then requires `effective.config` to agree with the roots by file identity,
links both adapted captures to the root captures through their adapter
receipts (ordinary-byte parity, same adapter producer), re-emits the three
snapshots as byte controls, binds every recorded plan chain (hash chain and
containment of every step artifact), proves ordinary NEW equivalent to the
kept module afresh when `kept` was selected, re-proves every accepted step
with runners built from the roots, re-checks the objective envelope, emits
the program through the capture's renderer, and requires the bytes to equal
the flow's `mkDemo.v`. `OUT` holds the controls, `replay/`, `qualified-input.ast`,
`mkDemo.v` and `upstream-qualification.json` (schema 2: status, branch,
domain, solver identity, timeout and every bound input). Saved receipts are
bindings, never authority: nothing in the flow directory is believed without
being recomputed.

## Correctness domains

`strict` (`StrictFourState`) accepts only candidates whose every observable
cone is structurally equal to NEW's after the normaliser's four-state
identities (typed comparison complements, logical `&&`/`||` reassociation,
exact concat-field projections, and the others listed in RULES.md); it never
calls a solver and fails on any residual. `binary` additionally hands
residual cones to a Yosys SAT miter; it proves two-state equivalence of the
settled composition with corresponding state and fixed children and cannot
waive an X/Z counterexample. Neither domain assumes reset completion,
reachability, initialisation, delta-cycle order or timing; a changed child
input connection is proved only when native metadata supplies its width and
is otherwise rejected. See [REVIEW.md](REVIEW.md).

## Add a policy without a compiler change

Matching and edit policies are bluehs modules; `bsc` imports none of them. A
new rule is a `CandidateProvider` with unique feature names:

```haskell
provider = CandidateProvider ["MY_RULE"] [ruleSource] $ \features budget old current ->
  take budget (myTypedProposals features old current)
combined = combineCandidateProviders [provider, defaultCandidateProvider]
-- enable MY_RULE in cfSearch.soFeatures, then
runCaptureFlow defaultFlowPolicy {fpProvider = combined, fpInvariants = DeferInvariants} config
```

Each `ASTCandidate` carries its feature, the proposed `VProgram`, diagnostic
operations, whether it needs the binary domain and the candidate-to-current
state correspondence. The provider only proposes: the search rejects
unknown features, applies the lexical and declaration-order gates, requires
an objective improvement, proves the candidate against CURRENT and binds the
rule sources named in `cpSources` (packaged modules are bound by the SDK
manifest instead). `testsuite/bsc.eco/postba/ASTExternalRules.hs` is a
complete example with negative controls. `match.hs OLD.eco-graph
NEW.eco-graph STEM` (writes `STEM.eco-plan`), `keep.hs OLD.pre OLD.final
NEW.pre NEW.final CAPTURE_SHA PREOPT_SHA OUT` (writes a keep plan and
`OUT.decisions`) and `invariants.hs SNAPSHOT YOSYS SECONDS OUT` expose the
matching, keep-planning and observation-consumer steps on their own.

## Diagnose a nonmatch or a larger diff

Read `<branch>-match.eco-plan` and `<branch>.log` first: a protected name or
an incompatible graph fails `matchGraphs` with an explicit reason, and the
aggressive completion is a maximum-cardinality assignment inside same-type
components, not a claim that its choices minimise cost. Then read the
`.rejected` notes and `.proof.json` receipts of the candidates, the
`summary.txt` stop reason (`round-limit`, `candidate-limit`,
`no-improving-proved-candidate`) and `named.rejected`: a budget is an
explicit limit, not a proof that no better edit exists. Check which families
are enabled ([RULES.md](RULES.md)) and whether the intended transformation is
one of them. Ordinary compiler scaffolding and new logic are retained; the
flow does not chase formatting-only matches. For a new source ECO keep ports
and instance names stable, retain logical state and helper roles where
practical, keep a behaviour fix separate from refactoring, and preserve
declaration groups; when removing a pipeline stage keep a surviving endpoint
register and redirect its input. Compare against a direct edit validated
against ordinary NEW, and treat commercial physical-ECO results as the final
measure.
