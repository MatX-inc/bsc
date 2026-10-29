# ECO utilities

The optional bluehs tools use native compiler captures to produce RTL close to
OLD while proving each accepted candidate against ordinary NEW. OLD and NEW may
have different behavior: the requested ECO is precisely that change. Module
ports stay fixed; instance renaming is not supported. New state is allowed.
The flow neither parses RTL nor learns correctness from a hand-edited target.

These scripts use the existing [bluehs runtime](../bluehs/README.md) and its
compiled `bsc` Cabal library. The runtime and Cabal build are prerequisites;
this directory does not create a launcher or another Haskell runtime. The native
ECO compiler facilities are prerequisites; the matching, edit and proof policies
below run as external scripts.
This external review stack starts on
[native ECO PR 243](https://github.com/MatX-inc/bsc/pull/243), after native capture,
Verilog-info and bluehs prerequisites. Native modules are exposed by `bsc.cabal`.
The [observation producer in PR 244](https://github.com/MatX-inc/bsc/pull/244) is an
optional follow-up; the core flow works without it.

## Build and test a matched toolchain

Build bsc and bluehs from the same source tree as described in
[INSTALL.md](../../INSTALL.md) and [bluehs setup](../bluehs/README.md).
The checkout launcher uses the matching GHC on PATH and the Cabal-generated
package environment. The installed launcher uses its bundled runtime.
For a checkout build:

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
```

The SDK output must be fresh. Packaging checks compiler/library version stamps,
queries the actual runtime's `ghc-pkg`, and records compiler, interpreter,
launcher, package environment/registration, compiled bsc and Haskell dependency
libraries/interfaces, external sources and scripts in `build.manifest`. It does
not copy the runtime, guides or examples: preserve the paired bluehs installation
or Cabal build and its system libraries, and keep this guide with the checkout.
The manifest is provenance; the flow records its hash but does not automatically
revalidate every SDK/runtime file. Check those
recorded identities explicitly before and after a run:

```sh
"$ECO_BLUEHS" "$ECO_SDK/scripts/package-sdk.hs" --check "$ECO_SDK/build.manifest"
```

The complete example performs both checks. Repackage after a legitimate runtime
or policy update; do not reuse a stale manifest. The checker binds the recorded
Haskell library closure, not arbitrary host shared libraries or a hostile
execution environment.

`bluehs.modules` and `bluehs.scripts` list the external sources and entry points.
Scripts load these with bluehs's existing `-i` option, so policy updates require
only a new source SDK, with no compiler rebuild. This is a version-matched SDK,
not a stable cross-version compiler ABI. These utilities introduce no extra
Haskell package or Python runtime beyond bluehs's prerequisites. Yosys supplies
SAT solving; Git supplies textual cost calculation. Native capture identities
require `sha256sum` or `shasum` on PATH. The emitted-Verilog concat proof controls
also require Icarus (`iverilog` and `vvp`); ordinary flow execution does not.

The default invariant exporter remains enabled. With the core compiler it
reports `unavailable-in-capture`, with zero facts; it never invents observations.
The optional observation producer needs its own matched compiler, library and
SDK. The native observation producer does not define the only possible source of
invariants. External protocol or software assumptions can be useful inputs to
downstream ECO verification. The current consumer accepts independently proved
typed assertions; it does not yet import unproved external assumptions or
discharge them in commercial tools.

The directed tests use the existing DejaGNU infrastructure:

```sh
make -C testsuite eco_sdk.check eco_proof.check eco_matching.check eco_rules.check \
  eco_search.check eco_invariants.check eco_core_without_observations.check \
  eco_flow.check eco_filters.check eco_relational_complement.check \
  eco_alias_history.check eco_logical_ac.check \
  eco_vacancy.check eco_shell.check eco_metrics.check eco_guide.check \
  eco_selection.check eco_state_view.check eco_translated_metrics.check \
  eco_renderer.check eco_objective.check eco_correspondence.check \
  eco_pipeline_chain.check eco_helper_reuse.check eco_live_aliases.check \
  eco_live_shell.check eco_alias_orientation.check eco_declaration_order.check TEST_RELEASE="$repo/inst"
```

Missing optional bluehs/Yosys tools produce unsupported tests, not successful
proofs. Test logs retain positive and negative proof controls. The public example
below is also exercised by `eco_guide.check`. The core invariant tests exercise
synthetic obligations plus a fresh capture without observations. For the optional
producer, use its separately matched runtime and SDK and run
`ECO_NATIVE_OBSERVATIONS=1 make -C testsuite eco_invariants.check`; this enables
the strict native-positive controls. The core absence test belongs to the core
compiler qualification.

## Capture OLD and NEW

Both BH (`.bs`) and BSV (`.bsv`) modules use the same native capture mechanism.
The public example uses the small BH `FanoutLoss` fixture. Run the complete
example from this checkout:

```sh
bash util/eco/examples/run.sh "$BSC" "$ECO_BLUEHS" "$ECO_SDK" \
  "$ECO_YOSYS" "$BLUESPECDIR" "$repo/eco-example"
```

The script exercises the recommended translated objective, ordinary refinement
preset and declaration-order finishing stage, and reports both metrics. Each output path must be fresh.
For your own module, compile each source revision with its usual flags and add
`-eco-capture`:

```sh
mkdir -p old-build old-capture
"$BSC" -u -verilog -elab -g mkDemo -bdir old-build -vdir old-build \
  -info-dir old-build -eco-capture old-capture Demo.bs
# In the NEW source checkout, using the same module ports:
mkdir -p new-build new-capture
"$BSC" -u -verilog -elab -g mkDemo -bdir new-build -vdir new-build \
  -info-dir new-build -eco-capture new-capture Demo.bs
```

Keep each capture's producer binary, original working directory, library and
filter dependencies. Captures include native `.ba` replay data, intern identity,
retained RTL AST, renderer context, child-interface metadata and authenticated
companions. This upstream native prerequisite uses its existing optional
VProgram slot and codec tag `bsc-ba-20260712-1`; ECO capture populates that slot.
Use a matched compiler and library and freshly generated captures. The separate
preview-based prototype uses a different `.ba` codec and Verilog AST; its captures
cannot be loaded directly here. Retaining the AST does not change ordinary
emitted RTL. Captures are not portable source archives. See
[Compiler compatibility](COMPATIBILITY.md) for the exact boundary.

The example uses `-no-show-timestamps` and explicit stabilization flags to make
its benchmark reproducible. Use the production compile settings for production
captures. This upstream prerequisite has no `-stable-verilog` flag. The native
helper-retention interface requests final helper identities and checks their
survival; unavailable requests fail closed and preserve the ordinary branch.
On a compiler with additional renumbering controls, select a version-specific
adapter and qualify its capture/replay behavior separately.

Native exact replay and explicit adaptation within the same full native version
are available separately:

```sh
"$BSC" -eco-replay old-capture/mkDemo.replay -eco-output old-replay
"$BSC_NEW" -eco-adapt old-capture/mkDemo.replay -eco-output adapted-old
```

`BSC_NEW` is the explicit path to the destination compiler for adaptation.
Both compilers must report the same full native version. Exact replay checks the
producer and companion identities; adaptation runs old and current ordinary
controls before publishing a new capture. A different compiler version requires
fresh captures and separate qualification.

## Run the checked flow

Create the typed configuration with explicit paths and correctness domain:

```sh
"$ECO_BLUEHS" "-i$ECO_SDK/lib" "$ECO_SDK/scripts/configure.hs" strict \
  "$BSC" mkDemo \
  /absolute/old-capture/mkDemo.replay /absolute/new-capture/mkDemo.replay \
  /absolute/old-source-directory /absolute/new-source-directory \
  "$BLUESPECDIR" "$ECO_SDK/build.manifest" "$ECO_YOSYS" \
  /absolute/fresh-eco-output /absolute/demo.config
"$ECO_BLUEHS" "-i$ECO_SDK/lib" "$ECO_SDK/scripts/captures.hs" \
  --objective translated-normalized --refinements standard \
  --structure ordinary --invariants defer /absolute/demo.config
"$ECO_BLUEHS" "-i$ECO_SDK/lib" "$ECO_SDK/scripts/finish-order.hs" \
  "$ECO_SDK/build.manifest" /absolute/fresh-eco-output /absolute/finished-eco-output
```

The finishing command freshly replays the selected upstream plan, including its
ordinary-to-kept bridge when needed, then scores historical order for intact
CURRENT declaration blocks and a coupled wire-identity/internal-order proposal.
The coupled proposal matches a surviving register group by its unchanged state
identity, class and width. Unchanged members anchor the match; unmatched scalar
wires must have unique matching types and ranges, vacant historical identities,
and one live continuous driver. It also proposes internal declaration order alone when wire names already match.
Unusable groups record a reason and leave independent eligible groups available.
It declines ambiguous matches, arrays,
initializers, ports, instance/call/event/index identities and protected comments.
It changes value names and permutes intact declarations inside existing rows;
executable expressions, state and provenance metadata remain CURRENT. Combining
these two choices matters when either isolated step would worsen the substantive
diff. No direct-edit reference or signal-name suffix is used.

Both families preserve each declaration member partition,
width, type, wrapper and comment, and never crosses effect or translate-off
barriers. Unmatched blocks retain their order beside the nearest preceding
matched block, or the first following one when there is no preceding match.
It accepts a lower translated substantive count, or an equal substantive count
with fewer raw changed lines. Each changed result requires a new independent
proof and saved-plan replay in the original proof domain. Proposals are tried in
lexicographic cost order; a rejected proof leaves its artifacts and permits the
next improving proposal. Schema-2 finishing receipts record any forward wire
map, and proof plans record its inverse. The chosen scalar
objective is recorded for replay; both metrics are checked again after emission.

The final RTL and receipt are under `finished-eco-output/finish`. A no-op receipt
records the reason and byte-identical input/output artifacts. The input variant
remains available if a substantive improvement increases the raw count. This
step does not merge wrappers to remove blank separators. The lower-level
`finishDeclarationOrderFrom` API proves CURRENT-to-finished only; orchestration
that calls it directly must separately establish and bind the upstream
ordinary-NEW-to-CURRENT proof chain. Stored success JSON is not proof authority.

This finishing pass selects one proved proposal from the two families; it does
not claim a fixed point or a globally minimal edit. A caller that needs both a
whole-block move and a separate internal-order/name choice may compose qualified
low-level stages, binding each stage to its predecessor. Repeating the public
CLI against the original flow starts from that flow again; it does not consume
the preceding finishing output automatically.

`CaptureFlow` is an editable Haskell `Read`/`Show` record. It exposes feature,
round, candidate, proof timeout and command timeout limits, optional typed
contracts and a custom graph matcher. The helper initializes both libraries to LIB; edit `cfOldLibrary` and
`cfNewLibrary` for captures with different original libraries. The CLI saves its effective configuration,
including switches that change feature selection, beside `flow.report`.

The runner adapts both captures, evaluates ordinary and relevant helper-keep
branches, checks name-allocation audits, searches typed proposals, replays each
accepted plan through fresh proofs, and emits the selected RTL. A failed or
nonimproving search can fall back to ordinary NEW. The selected output cannot
have a larger OLD-relative cost under the selected objective than ordinary NEW.
Only the default `--objective whole` gives this guarantee for whole-file raw
lines; translated objectives can trade those lines for a smaller translated diff.
See [search objectives](#synthesis-translated-search-objective). `flow.report`,
`summary.txt`, branch logs, search proposals and proof JSON files explain the
outcome. The [standard refinements](REFINEMENTS.md) remain external bluehs rules.

`--state-view` enables F23: an existing combinational aggregate of complete NEW
registers may reuse a vacant OLD packed-state name when both have exactly the
same typed input packing. The rule follows continuous helper definitions to
fixed input ports; it does not use generated-name suffixes or parse RTL. It
renames only the existing wire, retaining every NEW register, reset, enable,
transition, declaration grouping and expression. This is naming evidence, not
an assertion that OLD and NEW state values are equivalent. Each candidate is
still proved against NEW and accepted only if its OLD-baseline cost improves
under the selected search objective.
Occupied historical identities, unsupported packing, protected regions and
bounded-search omissions appear explicitly in the rule audit.

`--vacant-state` enables F21: complete current state groups may take compatible
unused historical names. Output-role votes rank proposals first; bounded
compatible alternatives cover changed output logic. These are naming proposals,
not claims that old and current stage values are equal. Each inverse state
correspondence is independently checked against NEW. The audit distinguishes
unique role evidence, enumerated alternatives, rejected pairs and unvisited
pairs when limits apply. Instance identities and module ports remain protected.

A unique fixed-output role can also identify a narrower payload after splitting
packed state: OLD selects an in-range constant slice, while NEW drives that same
output from a whole state of the slice width. This permits historical-name reuse
across a width or register-class change while retaining every NEW width, reset,
initialization and transition. It does not equate OLD and NEW reset semantics.
Unvoted alternatives still require the original exact group signature.

`--structure preserve|ordinary|compare` controls historical case/equal-arm
conditional restoration (F09/F10). `compare` retains both independently checked
outputs. Optional `--historical-shell` permits reconstruction of a historical
pure helper layout while preserving current values; `--inactive-helpers`
permits retention of unused historical constant helpers. These optional
historical forms are not assumed to help downstream tools. All proposals stay
subject to proof and measured cost, and directive/opaque barriers are protected.
The shell's Boolean-reassociation variant requires the binary domain.

## Read the two OLD-relative metrics

Compare the selected candidate and ordinary NEW with the same OLD snapshot:

```sh
"$ECO_BLUEHS" "-i$ECO_SDK/lib" "$ECO_SDK/scripts/measure.hs" \
  /absolute/eco-output/adapted-old/mkDemo.eco-ast \
  /absolute/eco-output/adapted-new/mkDemo.eco-ast /absolute/ordinary-metrics
"$ECO_BLUEHS" "-i$ECO_SDK/lib" "$ECO_SDK/scripts/measure.hs" --candidate \
  /absolute/eco-output/adapted-old/mkDemo.eco-ast \
  /absolute/eco-output/kept/mkDemo.eco-ast \
  /absolute/eco-output/kept-search/selected.ast \
  /absolute/eco-output/mkDemo.v /absolute/selected-metrics
```

Use the selected branch's context and AST. `report.json` records added, deleted
and total changed lines for both metrics with input identities. A raw AST alone
is insufficient: candidate RTL must exactly match emission in its authenticated
context. This integrity check qualifies counting, not behavioral equivalence.
A retained native-AST direct edit can be measured with the same candidate form;
handwritten RTL without a native AST can be counted separately with Git, but
cannot receive a qualified normalized count from this utility.

Raw changed lines are Git Myers additions plus deletions over actual emitted
RTL. Whole-file raw cost is the default search objective when `--objective`
is omitted; the recommended command above selects translated normalized cost
instead. The second metric renders the retained native AST with wrapping,
indentation, blank lines and ordinary informational comments removed. It preserves declaration grouping and member order, statement
order, names, values, operators and directive-bearing comments. A declaration
regrouping is therefore material in both metrics. Known synthesis/tool comments
are retained conservatively; this is a reporting convention, not a semantic
classifier for every possible annotation language.

The normalized count is a separate metric, not raw lines minus a subset; it can
sometimes exceed the raw count. Always compare OLD→automatic and OLD→direct
using the same OLD baseline and contexts. Automatic→direct distance is only
informational. Equal-sized, correct alternatives need not match the hand edit's
placement. Textual cost does not establish minimum physical ECO cells, routes,
area or timing; those require downstream-tool measurements.

## Count synthesis-translated lines

Measure the synthesis-translated projection to exclude assertions, debug
statements and initialization in native `translate_off` regions from the cost:

```sh
"$ECO_BLUEHS" "-i$ECO_SDK/lib" "$ECO_SDK/scripts/measure-translated.hs" --candidate \
  /absolute/eco-output/adapted-old/mkDemo.eco-ast \
  /absolute/eco-output/adapted-new/mkDemo.eco-ast \
  /absolute/eco-output/ordinary-replay/selected.ast \
  /absolute/eco-output/mkDemo.v /absolute/translated-metrics
```

The input forms match `measure.hs`. The output contains both translated raw and
normalized counts, preserved whole-file metrics under `whole/`, projected native
ASTs, and input/output identities. Compare automatic and direct candidates to
the same OLD capture. This measurement command reports existing outputs; it does
not rerun a search or change its objective or equivalence checks. The
[translated search objectives](#synthesis-translated-search-objective) use the
same projection when choosing candidates. The projection is a cost view;
complete candidate RTL remains the deployment artifact.

`projectTranslatedProgram` follows the compiler printer's ordered native
`VMStmt`/`VMGroup` off/on flags. Nested wrappers are counted and follow those
literal toggles; the report does not claim every downstream tool interprets
nested pragmas identically. Ambiguous textual switches in comment/header fields
or unquoted assertion templates are rejected. No emitted RTL is parsed.
Active declarations, grouping, names, helper definitions and item ordering stay
material, including helpers whose only consumers are excluded assertions.
There is no dead-cone elimination. Unflagged initial blocks remain counted;
flagged initialization and its generated guard disappear together.

The projection does not evaluate macros or choose preprocessor branches. It
preserves active reset, delay, string-parameter and initial-block macros under
the authenticated rendering policy. The raw projection uses normal printer
wrapping and active comments; blank separators adjacent to removed items can be
regenerated. Normalization additionally excludes informational comments and
formatting, preserving declaration partitions. These projected files are metric
artifacts, not replacement RTL for simulation or deployment. Any edit budget
must use the same projection; subtracting an estimated initialization count from
a whole-file bound is not an equivalent audit.

## Retain raw/normalized tradeoffs

A raw-line improvement can increase normalized changes. The optional selector
examines every accepted prefix of ordinary and kept search plans and retains the
Pareto alternatives: no other recorded prefix is better in both counts.

```sh
"$ECO_BLUEHS" "-i$ECO_SDK/lib" "$ECO_SDK/scripts/select.hs" strict "$ECO_YOSYS" 30 \
  /absolute/fresh-selection /absolute/eco-output
```

It writes `frontier.json` plus independently re-proved `RawFirst` and
`NormalizedFirst` outputs, with explicit tie-breaks and copied prefix bundles.
This command does not rerun or reinterpret the saved search objective. Use
`select.hs --translated` for translated raw/normalized selection, as described
[below](#synthesis-translated-search-objective). Selection considers recorded
accepted prefixes, not every possible edit, and makes no global-minimum claim.
Multiple flow directories may be supplied only when their authenticated OLD/ordinary NEW
ASTs, RTL, renderer context and child metadata agree. Each selected kept prefix
gets a fresh ordinary-NEW→kept proof before fresh saved-plan replay. Input/tool
identities are rechecked before `completed.json` is written; frontier entries
alone are measurements and do not authorize a candidate.

## Correctness domain and invariants

Use `strict` for conservative settled four-state checking. It accounts for X/Z
behavior for supported constructs with arbitrary corresponding state. Typed
comparison complements, such as `!(a <= b)` versus `a > b` or `!(a == b)`
versus `a != b`, preserve the same sized operands and four-state result; they
do not require binary inputs. Ordinary equality is not replaced by case equality.
Logical `&&` and `||` may also be reassociated and reordered within the same
operator. Their four-state truth conversion is preserved, including Z becoming
X; this rule does not replace logical operators with bitwise operators or erase
a repeated raw operand. Shared repetitions are represented compactly.

The checker can also establish a local common-history relation for duplicate
incomplete combinational processes. Eligible internal scalar registers have no
initializer or other writer, so both start at Verilog's all-X value. Identical
level-sensitive events and assignments preserve equality, including when both
processes retain their previous value. The rule requires a shared surviving
process, matching widths and complete sensitivity, and excludes feedback and
unsupported dependencies or effects. It preserves missing defaults rather than
turning them into don't-cares. Proof inventory records each duplicate relation.
This argument does not cover independently forced retained register values.

These checks concern settled values, not delta-cycle ordering or transient
glitches. They are not a complete event-driven simulator or a general temporal
equivalence proof. Unsupported inventory and uncertain child bindings fail
closed. Child interfaces come from authenticated native metadata, not the
editable candidate. No reset-completion or software-initialization assumption
is introduced by either rule.

Choose `binary` explicitly when the ECO's verification contract is two-state.
The Yosys SAT proof in this mode does not establish four-state equivalence.
A binary result cannot waive an X/Z counterexample. The flow never assumes
reset has completed, ignored method arguments are harmless, a type encoding is
valid, or software initialized architectural state merely because that is the
intended use. An assertion must be represented in supported typed form, bound to
the checked AST, and independently discharged before it can help a proof.

`--invariants export` requests bound assertion proposals and independent proofs;
`defer` leaves this optional product explicit. Native method/type/scheduler and
proof-obligation observations are metadata, not automatic assumptions. Export
status distinguishes completed, deferred and unresolved work from the proof of
the selected ECO. This is not a finished Formality or Conformal integration, nor
a general proof of temporal/environment assumptions. Commercial ECO results
and accepted assumption mappings still need downstream qualification.

## Replay a selected plan

A flow already replays every accepted plan once. To freshly verify the entire
selected chain, including the ordinary-NEW→kept bridge and objective witnesses:

```sh
"$ECO_BLUEHS" "-i$ECO_SDK/lib" "$ECO_SDK/scripts/replay.hs" \
  --flow /absolute/eco-output /absolute/fresh-flow-replay
```

This also handles `ordinary-rollback`. `ECOASTReplay.replaySelectedFlow` is the
shared library entrypoint used by declaration finishing; prefix selection uses
`replayFlowPrefix` for the same capture authentication and fresh bridge/plan
proofs, then checks its own measured objective. The opaque `VerifiedFlow` binds
its complete output and evidence; call `validateReplay` after subsequent work.
No saved receipt is accepted as a new proof.

The lower-level branch primitive remains available for an explicitly supplied
reference, without loading its generator code:

```sh
"$ECO_BLUEHS" "-i$ECO_SDK/lib" "$ECO_SDK/scripts/replay.hs" \
  --objective translated-normalized /absolute/eco-output/adapted-old/mkDemo.eco-ast \
  /absolute/eco-output/effective.config \
  /absolute/eco-output/kept/mkDemo.eco-ast \
  /absolute/eco-output/kept-search/search.plan \
  /absolute/fresh-replay-output
```

Use `adapted-new/mkDemo.eco-ast` and `ordinary-search/search.plan` for the ordinary
branch. Match the objective used for search; omit `--objective` and its OLD
argument for a whole-file plan. The selected branch is in `summary.txt`; an
`ordinary-rollback` result has no accepted improved plan. Replay checks exact options/domain, renderer and
AST hashes, all candidate/contract artifacts and fixed ports, then re-proves the
branch chain using the authenticated child context. A kept-branch replay proves
against the kept reference; it does not rerun the original ordinary-NEW→kept
control proof. Use `--flow` to re-establish that complete link. A saved receipt alone never
authorizes a candidate. Preserve the complete plan directory and referenced
capture files; moving a partial bundle or editing a hash invalidates it.

## Add a policy without updating the compiler

The compiler exports snapshots, typed graph/namespace validation, selected
helper retention and observations. Matching and edit selection live in
`util/eco/lib`; `bsc` imports none of those external policy modules. New policies
can be ordinary bluehs scripts using `CandidateProvider`:

```haskell
provider = CandidateProvider ["MY_RULE"] [ruleSource] $ \contracts features budget old current ->
  take budget (myTypedProposals contracts features old current)
combined = combineCandidateProviders [provider, defaultCandidateProvider]
-- Enable MY_RULE in cfSearch.soFeatures, then:
runCaptureFlowWithProvider combined DeferInvariants config
```

Each `ASTCandidate` supplies its feature, typed VProgram, diagnostic operations,
proof-domain requirement, candidate→current state correspondence and any typed
contracts. The provider only proposes; shared allocation checks, proof, cost,
rollback and saved replay remain independent. Register unique feature names and
identify all external rule/data source files in `cpSources`. The runner binds
those supplied identities and detects mutation; it does not discover arbitrary
Haskell dependencies or sandbox script execution. Budgets bound cooperative
proposal generation, not arbitrary user code runtime.

`testsuite/bsc.eco/postba/ASTExternalRules.hs` is an executable example: an
unbundled rule improves a typed fixture through a frozen SDK, with negative
controls for ports, behavior, evidence, source mutation and duplicate feature
registrations. F21's vacancy provider is a larger example of an ECO-specific
policy extension that needed no native compiler change. Add new directed tests
to the existing suite when introducing a new match/edit rule.

## Diagnose a nonmatch or a larger diff

Inspect the match audit first: protected namespaces, incompatible graph types
and component surplus have explicit witnesses. The aggressive matching
certificate establishes maximum cardinality inside its complete same-type
components; it is not a theorem that its semantic naming choices minimize ECO
cost, nor a universal certificate for arbitrary sparse future policies.

Then inspect branch failures, proposal rejection proofs, counterexamples and
budget stops. A search budget is an explicit limit, not a proof that no better
edit exists. Check whether helper retention, ordering, declaration grouping or
vacant state names are enabled and whether the intended transformation is in the
provider. Ordinary compiler scaffolding and new logic are retained; the flow
does not chase formatting-only matches or inline fresh enable helpers solely to
imitate a manual edit. Report formatting/comments separately and retain the
hypothesis that downstream sensitivity may justify revisiting them.

For a new source ECO, keep ports and instance names stable, retain logical state
and helper roles where practical, and localize the intended behavior change.
When removing a pipeline stage, preserve a surviving endpoint register identity
and redirect its input where the source structure allows it; this can avoid
renaming many consumers. Keep a behavior fix separate from unrelated helper or
record refactoring, and preserve declaration groups where practical because
downstream tools may use them. These are candidate-crafting heuristics, not
physical ECO guarantees.

Construct a direct edit from the source change as a comparison target, validate
it against ordinary NEW, and improve that target if the automatic flow finds a
smaller correct change. Neither target construction nor the matching algorithm
should assume an old state value equals a different pipeline stage. Commercial
physical ECO results remain the final measure when available.

The `ECOAST` facade delegates checked loading, rendering, emission and verification
to the paired native `ECO.VerilogInfo` API. The historical rendering-policy type
is retained only for reading external reports; native rendering has no policy
switch. Every edited
candidate still passes through the original filter and must match the complete
native AST rendering byte for byte; changed parameter interfaces and filters
that add unrepresented output fail closed. Run `eco_renderer.exp` for public
parameter, ordinary-rendering and candidate-filter controls. The preview
compiler's typed-string macro adapter is a separate version-specific
implementation; these upstream tests do not claim to exercise its type flag.

### Synthesis-translated search objective

Use `captures.hs --objective translated-raw ... FLOW.config` to minimize raw
lines in the native synthesis-translated projection. Use
`--objective translated-normalized` to minimize the same projection with ordinary
formatting and information comments removed. Declaration grouping and ordering
remain material. Put `--objective` before the other options. Omitting it retains
the existing whole-file raw objective. These are alternative bounded searches;
neither proves a globally minimal edit or physical ECO cost.

The objective controls initial naming rollback, accepted transformations, branch
selection, and the final survival check. Full candidate ASTs and complete RTL are
still emitted and independently proved against ordinary NEW. Scoring never
replaces deployed RTL, removes assertions from that RTL, or changes the proof
domain. Native translate-off regions are excluded only from cost; active helpers
used exclusively by an excluded assertion still count. The projection uses native
printer toggles and rejects textual switch ambiguity; arbitrary preprocessor
branches are preserved without evaluation.

`ECOASTCost.CostProvider` is the external callback boundary. Its two pure rendering
callbacks produce OLD and CURRENT scoring witnesses, separately from the full
renderers used by `ECOASTSearch`. Callers supply an explicit policy identifier and
policy source/data paths. `ECOASTTranslatedObjective.translatedCostProvider`
implements the two translated modes. `runCaptureFlowWithCost` accepts a provider
factory for authenticated OLD/branch contexts. New matching and objective rules
remain external bluehs changes.

`search.plan` remains the schema-2 semantic transcript, including historical
whole-file step costs. `objective.plan` is a separate envelope binding that plan,
the policy/source identities, OLD AST, complete accepted AST hashes, scoring
witness hashes and recomputed costs. Search summaries and schema-4 flow reports
use the selected objective as the primary cost. Whole-file costs remain in the
semantic plan and `flow-objective.report` for audit. The full deployed result is
`MODULE.v`; branch replay's `selected.v` and `selected.ast` are also complete.
Files under `objective/` are scoring witnesses only.

Replay translated plans with:

```
bluehs -iSDK/lib SDK/scripts/replay.hs \
  --objective translated-normalized OLD.eco-ast \
  FLOW.config BRANCH.eco-ast search.plan FRESH_OUTPUT
```

Replay rechecks complete semantic proofs, then verifies the expected objective,
source identities, baseline, accepted AST sequence and fresh witness costs. The
CLI requires an explicit objective for a translated plan; legacy semantic plans
remain readable. Preserve the paired source SDK paths with saved plans because
source identities include canonical paths as well as content hashes.

`select.hs --translated strict YOSYS SECONDS FRESH_OUTPUT FLOW...` compares
recorded accepted prefixes with translated raw/normalized costs and reproves both
RawFirst and NormalizedFirst selections. It does not generate new candidates or
reinterpret a saved search's original objective. The selected metric is measured
again after semantic replay. Ports remain fixed, instance renaming remains out of
scope, and temporal contracts remain deferred.
`ECOASTPipelineChain.pipelineChainProvider` adds the optional `F25` matching
rule to a caller's provider set. Combine it with `defaultCandidateProvider`
and add `F25` to `soFeatures` when constructing the external flow. It identifies
complete, unbranched typed register chains through their continuous data-input
transfers and uniquely matching head input expressions. It aligns a shorter
CURRENT chain with OLD tail identities; for a longer chain it assigns the extra
head stages unused CURRENT group names in one injective permutation. All group
members move together. The rule uses neither source names nor numeric suffixes.

Every proposal retains CURRENT registers, resets, enables, initialization,
logic and latency. The usual independent CURRENT-equivalence proof uses the
inverse name map. It does not assume that OLD and CURRENT stage values are
equal and does not retain disconnected OLD storage. Fixed ports, declaration
uniqueness, name collisions, callable/instance identities, hierarchical uses,
semantic directives, branching and ambiguous input roles are guarded. A
procedural first-stage data input with a different group shape can prevent a
complete-chain proposal; that is a discovery limit, not an impossibility proof.

The rule uses the caller's selected search objective. A whole-chain alternative
can have a smaller normalized diff but a larger raw diff because informational
comments differ. Report both against the same OLD, identify the search objective,
and prove/replay the selected native AST. An accepted normalized improvement
does not establish an improvement under a raw objective.

`pipelineChainAudit` explains discovery separately from acceptance. It records
unmatched input/type roles, multiple OLD or CURRENT matches, branch/cycle and
noncanonical-state exclusions, occupied/protected/hierarchical target barriers,
and eligible/emitted counts. At most 4096 chain pairs are examined; unvisited
pairs and the caller's proposal-budget limit are explicit. Accepted proposal
metadata includes this audit. A declined or unvisited proposal is not a proof
that no better correspondence exists.

### Combined ordinary refinements

Use `captures.hs --objective translated-normalized --refinements standard
--structure ordinary --invariants defer FLOW.config` to run the external naming,
pipeline-chain, live-helper and alias refinements together. See
[REFINEMENTS.md](REFINEMENTS.md) for the fixed-port scope, explicit proof domain,
policy switches and replay procedure. The preset does not retain unused OLD
state or enable historical unused-helper shells. Omitting `--refinements`
preserves the previous provider selection.
