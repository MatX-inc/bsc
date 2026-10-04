# Testsuite planner and Buck2 migration: Stage 1

## Base and scope

Started 2026-10-04 on `codex/testsuite-planner-stage1`, based on
`67084af8e8f839ff1b85454448dd102ee9bcbf26`, the head of
[upstream PR #1135](https://github.com/B-Lang-org/bsc/pull/1135) verified that day.
No compiler changes or test reclassification are included. Golden changes
require explicit approval; the approved `b1197` correction is the sole exception
in this checkout and remains separate from the planner work.

The first review slice is the population audit needed before translating tests:

- A standalone Haskell package in `testsuite/planner`, using only GHC boot
  libraries, with `import-sum`, `compare`, and `census` commands.
- Strict legacy DejaGNU import: discovered test scripts, all nine dispositions,
  duplicate-label multiplicity, multiline labels, diagnostics, and footer totals.
- Required expected-test manifests: two equally incomplete runs must not pass
  merely because they agree with each other.
- A static Tcl census with file/line locations. Every command is explicitly
  not yet semantically lowered. Recognizing syntax is not implementing a check.
- A reproducible two-run baseline capture script. It invokes the full suite,
  enables SystemC, monitors summaries during execution, and fingerprints the
  complete compiler installation before and after each run.

This does **not** complete D1: DejaGNU result labels are not stable semantic
identities. For example, `compile_pass` changes its label on failure and
`exit_status` emits the shared label `Exit status` on success. The importer
therefore uses a versioned **legacy** identity `(exp, exact label, occurrence)`.
Changed labels are conservatively reported as removed and added checks. See
[`IDENTITY.md`](../testsuite/planner/IDENTITY.md). Semantic invocation/assertion
IDs and an oracle-side mapping remain required before Buck2 parity is claimed.

## Local evidence location

Local evidence lives under `testsuite/.stage1-validation/`; `/testsuite/.*` is
ignored by Git. Legacy discovery, statistics, log archiving, and recursive
cleanup prune hidden suite directories so archived tests and results cannot be
counted as current output. Relocation verified all 18,611 existing files byte for
byte. Capture-time paths embedded in old logs and metadata remain unchanged;
map the old repository-root `.stage1-validation/` prefix to the new location.
The checksum manifest and move record are retained in the evidence directory.
Post-move discovery retained exactly 869 entries (866 active BSC scripts plus
three infrastructure inputs). All six hidden-directory regression tests, 13
timing/path tests, eight capture-lifecycle tests, the Haskell planner tests, and
the two suite layout checks passed. No full compiler-suite rerun was needed for
this relocation.

## Canonical Buck2 rule location

Use the brief's two repository-root rule directories:

```text
rules/bluespec/           reusable public Bluespec build rules
rules/bsctest/            BSC-specific assertions, verdicts, and comparisons
testsuite/planner/       semantic model, Tcl lowering, and backend emitters
testsuite/buck2/         testsuite cell configuration and generated targets
```

These rule directories are the intended next-stage layout, not implemented
rule APIs in this slice. The generic public entry point will be
`rules/bluespec/defs.bzl`; it must not import testsuite-specific policy.
Compiler installations and platform instances belong in the consuming cell,
separately from reusable toolchain rule definitions.

Buck2 imposes no language-specific directory convention: its
[rule-author guide](https://buck2.build/docs/rule_authors/writing_rules/#new-rules)
allows new rules outside the bundled prelude. Root-level `rules/` makes the
public rules discoverable and keeps their ownership distinct from their first
consumer, the testsuite. A named cell can later expose the same rule tree to
downstream projects; a cell is a configuration boundary, not a requirement to
move these rules into a separate repository.

## Existing DejaGNU execution traces

The archived per-directory `testrun.log` files already contain useful command
traces. For example, the Arith log records `bsc_compile_verilog`,
`def_link_verilog`, simulation commands, output-filter commands, redirections,
and adjacent verdicts. Its header also records compiler and simulator details.
The baseline capture preserves these logs alongside the `.sum` files; the
current importer reads verdict summaries only.

Use these existing traces when building the harness catalogue and validating
lowering: associate observed operations with checks, then compare the planner's
steps, flags, ordering, and transcript handling with the oracle's execution.
Inspect trace coverage before adding instrumentation. A command is not a check:
one compile command can produce both a compilation verdict and an
intermediate-file assertion. The harness source remains necessary to establish
expectations, phase-specific failures, implicit inputs, and unexecuted branches.
Logged commands are evidence for the semantic TestPlan, not its definition.

## Repeated labels and stateful tests

The frozen first baseline contains **303 of 866 scripts** with repeated exact
labels inside a script: **1,553 groups, 3,209 occurrences**, or 1,656 occurrences
beyond the first in each group. These are checks, not redundant tests.
The script path is already part of identity; identical wording in separate
scripts is not a collision.

The following primary causes partition all groups, based on archived command
traces and source review. A group can also involve secondary dimensions.
“Requested build configuration” records the requested flags, not a claim that
the old harness regenerated every artifact under those flags.

| Reviewed primary cause | Repeated groups | Check occurrences |
| --- | ---: | ---: |
| Ordinary/VCD simulation pair | 1,077 | 2,154 |
| Requested build-configuration variants | 404 | 857 |
| Runtime-option variants | 16 | 49 |
| Different source/artifact under matching names | 15 | 58 |
| Separate backend checks | 18 | 39 |
| Incremental/reuse/staleness checks | 12 | 28 |
| Different working directories | 2 | 5 |
| Additional explicit VCD runs | 4 | 9 |
| Repeated or overlapping assertions | 3 | 6 |
| Suspect checks against the previous artifact | 2 | 4 |
| **Total** | **1,553** | **3,209** |

Only four scripts contribute those 12 incremental/reuse/staleness groups:
`bsc.driver/bluesim`, `bsc.driver/depend`, `bsc.bluesim/parallel`, and
`bsc.compile`. This is not an inventory of every stateful test: a stateful test
whose labels differ is outside this repeated-label audit. Automatic internal
load checks were disabled in this frozen run, so they are not counted here.
There are still 17 repeated ordinary intermediate-file-existence groups.

The source-review assignments, evidence locations, and source hashes are in
`testsuite/.stage1-validation/repeated-label-causes.json`; the structural and command
inventory is `testsuite/.stage1-validation/repeated-label-audit.json`.

The existing execution logs establish that **1,077** of the Bluesim groups
contain exactly one ordinary invocation and one `-V` invocation with otherwise
matching command text. Of the 1,106 repeated Bluesim execution groups, the other groups contain further repetition
(15 groups), other command variants with VCD (six), or no VCD option (eight).
This is command-text evidence, not a proof that inputs or filesystem state were
unchanged. The inventory is reproducible with:

```sh
python3 testsuite/planner/scripts/audit-repeated-labels.py \
  testsuite/.stage1-validation/baselines/run-1.json \
  --log-root testsuite/.stage1-validation/baselines/run-1 \
  > testsuite/.stage1-validation/repeated-label-audit.json
```

Source inspection demonstrates several distinct meanings:

- Independent flags, such as output-directory options in `bsc.options`, and
  ordinary/VCD simulator runs. Give variants explicit identities and isolated
  outputs only when freshness and semantics match. `NullCrossing` currently
  erases `.bi` rather than `.bo` before changing flags; the compiler does not
  use that `.bi` deletion to invalidate its dependency state. This requires
  review before claiming that a fresh variant workspace preserves behavior.
- Stateful builds in `bsc.driver/bluesim`: full build, touch `Sub1.bsv`, partial
  rebuild, then unchanged rebuild. The repeated calls intentionally reuse
  `bd`/`sd`; golden transcripts check what was rebuilt.
- Dependency tests in `bsc.driver/depend`: changed include contents, missing
  `.ba` files, deleted BDPI wrapper files, relative object/source timestamps,
  and the same source compiled in different directories.
- Different assertions reported with the same words. For example, internal
  `vcdcheck` predicates are absent from its success label. Some scripts repeat
  or overlap an assertion literally; those need review, not automatic removal.
  The two suspect groups in `bsc.scheduler/earliness/earliness.exp:79` inspect
  `ExecutionOrderAttrRule` output after compiling `ExecutionOrderAttrModule`.
  That possible wrong-file assertion is recorded without changing the test.

Other compiler suites use both named independent variants and ordered
scenarios. Rust's [revisions](https://rustc-dev-guide.rust-lang.org/tests/compiletest.html#revisions)
parameterize tests, while its [incremental tests](https://rustc-dev-guide.rust-lang.org/tests/compiletest.html#incremental-tests)
start with an empty cache and reuse it through successive revisions. LLVM
[regression tests](https://llvm.org/docs/TestingGuide.html#writing-new-regression-tests)
can execute multiple `RUN` commands sharing a per-test temporary path. These
support using a structured scenario, not inferring dependencies from labels.

Initially lower each genuine mutate/recompile scenario into one ordered action
with a fresh private workspace, explicit operations, and separate per-phase
verdicts. Preserve its shared intermediate state within the scenario and apply
`never` cache policy to the scenario and dependent checks. Do not use output
retention to share state across suite invocations. Buck2
[`local_only` and `allow_cache_upload`](https://buck2.build/docs/api/build/AnalysisActions/)
control placement and uploading, not all cache reuse; the backend must also
prevent daemon and remote result reuse and verify that the compiler phases
actually execute. That enforcement remains unimplemented.

## Validation and configuration

Commands and local evidence are documented in the
[planner README](../testsuite/planner/README.md). The two initial baselines
below used Bluesim, Icarus, and SystemC, with internal
checks disabled. Their installation lacked optional developer tools,
`showrules`, the Bluetcl package `InstSynth`, and the script `expandPorts`.
They remain evidence for that historical configuration, not the subsequently
expanded installation.

The compiler was built via root `make -j32 GHCJOBS=16 install-src`. Following
the user's requirement to enable internal checks, it was supplemented with
`make -C src/comp -j1 GHCJOBS=16 install-extra` (there is no root `install-extra`
target). This installs `bsc2bsv`, `bsv2bsc`, `dumpbo`, `dumpba`, `vcdcheck`, and
`showrules`. The planner's `scripts/install-bluetcl-extras.py` installs the four
optional Bluetcl scripts needed by `InstSynth` and `expandPorts`, preserving the
normal package index. Package loading and `expandPorts -help` passed. The full
suite with this installation and internal checks enabled is being validated.
It exposed failures in the previously unavailable `InstSynth` and `expandPorts`
tests. Their output is preserved under
`testsuite/.stage1-validation/internal-checks/failure-artifacts/`. Three targeted repairs
are now present, with the original test drivers and goldens unchanged:

- `bluetcl_opt_pass` checks the status of its one option-aware invocation,
  rather than calling the nonexistent `run_bluetcl` helper afterward.
- `portUtil` reads current `results` metadata as well as the legacy `result`
  field, restoring missing method-result ports. All 13 existing fixtures pass
  in isolation; all 26 wrapper/include outputs match the goldens byte for byte.
- `InstSynth` selects the BSV metadata required by its BSV generator before
  querying Bluetcl. Both original startup-mode invocations match the goldens;
  frozen BSV remains usable, while frozen Bluehs is rejected before altering
  output. Downstream compilations, message counts, and `dumpbo` also pass.

Probe evidence is in `testsuite/.stage1-validation/extra-tool-probes/`. The first full
run remains diagnostic evidence, not a clean baseline or a performance sample.
The queued measurement was stopped when a third failure appeared in
`bsc.bugs/bluespec_inc/b1197`: the internal signature dump differs from its
expected file. A fresh isolated compile reproduces the dump byte for byte.
The source explicitly gained `Min#(ni, 1, 1)` in commit `10e1952c388e` (2023-10-24),
and commit `41812f5d5c4f` (2026-04-17) changed the serialized signature to retain
only explicit imports. The user approved the correction on 2026-10-04, and
the golden now matches the fresh isolated dump byte for byte (SHA-256
`8053384c64166f947bdc808c8ff0fd69397922ab30f3d7f579ec3ef88994a21e`).
The patch is preserved in `testsuite/.stage1-validation/b1197-golden-proposal.patch`.

The diagnostic run completed with 25,541 PASS, 134 XFAIL, 11 FAIL,
1 UNRESOLVED, and 9 ERROR across all 866 scripts. The previously interrupted
measurement wrapper continued into its first enabled run. It completed with
25,590 PASS, 134 XFAIL, and one `InstSynth` Bluehs-output mismatch, and correctly
detected the overlapping golden edit. This rejected attempt is preserved in
`testsuite/.stage1-validation/internal-cost-rejected-20261004T162417Z/`, with its runner
log beside it. It is excluded from performance samples.

The remaining mismatch was a missing expected-file selection: `InstSynth.exp`
did not specify a Bluehs expectation, so the helper tried a nonexistent
`instsynth.tcl.bluetcl-bh-out.expected`. After that run ended, the harness was
corrected to explicitly reuse the existing console golden for both startup
modes. Fresh probes of the exact harness commands match that golden and both
generated-include goldens; evidence and the applied patch are preserved in
`testsuite/.stage1-validation/extra-tool-probes/InstSynth/harness-expected-selection/`.
The earlier isolated validation checked output bytes but missed this harness
selection defect. No additional golden change was made.

The clean experiment started at 16:25 UTC on 2026-10-04, after the rejected
attempt's completed result and failed runner state were verified. Capture now
holds a shared checkout lock and cleans up its process group on interruption;
seven lifecycle tests and nine timing-report tests pass. No suites overlap.

Internal checks belong in `rules/bsctest`, not the public language rules. The
migration must compare its ordinary checks against DejaGNU run with
`DO_INTERNAL_CHECKS=0`, while independently requiring the additional internal
checks to pass. Each automatic internal check is associated with its specific
producer invocation, artifact version, and checker role; a repeated display
label is insufficient. Preserve `.ba` existence assertions in the ordinary
population: only their following `dumpba` load is internal-check-gated. The
capture script defaults to internal checks enabled and provides an explicit
off setting for the parity oracle. No cross-configuration semantic mapping has
yet been implemented.

Validated with GHC 9.6.7: the Cabal library/executable build and test suite;
lexical parsing and 18 inert `tclsh` differential fixtures (10 script and eight
list fixtures, Tcl 8.6.17); malformed JSON,
summary/footer checks, duplicate identities, missing-population tests, and a
20,000-check JSON round trip. A real-CLI regression verifies exclusion of root
aggregate summaries and symlinked inputs without concealing missing leaf tests.
An end-to-end CLI probe imported a summary, round-tripped its JSON, accepted an
identical comparison, and rejected both a missing check and a missing script.
Repository whitespace and testsuite layout lint returned success.

The optional native-Tcl differential gate also passed over all 866 active
scripts after long-test enabling: 6,189 top-level commands and 18,133 words,
zero parse errors and zero command/word boundary differences (including UTF-8
offsets). Source and lexer hashes remained unchanged. The gate uses
`Tcl_ParseCommand`, never evaluates test Tcl, and is available as
`testsuite/planner/scripts/check-tcl-boundaries.py`. This establishes lexical
boundary agreement, not semantic lowering or nested-script evaluation.

Initial source census, before enabling long tests:

| Population | Scripts | Unlowered command sites | Uninspected script/expression sites | Lexical errors |
| --- | ---: | ---: | ---: | ---: |
| Active `bsc.*` `.exp` files | 861 | 10,266 | 637 | 0 |
| Including five inactive templates | 866 | 10,283 | 639 | 0 |

These are source inventory counts, not executed checks. All semantic lowering
remains unimplemented. The 864 tracked `.exp` files include three infrastructure
files outside `bsc.*`: `config/unix.exp`, `lib/bsc.exp`, and `site.exp`. The test
population is separate from them; enabling long tests produces 866 scripts.
Local detailed census: `testsuite/.stage1-validation/census-before-long-tests.json`.
The final active 866-script census is `testsuite/.stage1-validation/census-full.json`.

The full root compiler build passed using the host GHC 9.14.1, with the exact
command `make -j32 GHCJOBS=16 install-src`. Both cleaned full DejaGNU runs
completed successfully with SystemC enabled:

| Run | Scripts | Checks | PASS | XFAIL | FAIL | Other unexpected results |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Baseline | 866 | 20,428 | 20,294 | 134 | 0 | 0 |
| Second run | 866 | 20,428 | 20,294 | 134 | 0 | 0 |
| Count delta | 0 | 0 | 0 | 0 | 0 | 0 |

Both runs matched the independently discovered script population and preserved
the complete installation digest
`a2f8521efb50d8e1c5cdced9bdcf65d9aa63f1f746255a362d3a3f916aaa968d`.
Their recorded commands also confirm all optional developer-tool paths resolve
under this same installation.

The per-check comparison found **zero missing/added scripts, zero missing/added
check IDs, and zero changed dispositions**. The strict whole-manifest comparison
returned **exit 1**, correctly reporting six removed/added diagnostic records:
three tests log a different random seed on each run.

| Test under `bsc.interra/operators/` | Baseline seed | Second-run seed |
| --- | ---: | ---: |
| `Arith/arith.exp` | 163679 | 44490 |
| `BitSel/bitsel.exp` | 13645 | 274223 |
| `Logic/logic.exp` | 184757 | 290435 |

These are real input differences, not only cosmetic log text: each test's
`generate/gen.pl` calls unseeded `srand()`, emits a random Verilog seed, and
generates the vectors consumed by the test. Source inspection and an isolated
Icarus 12.0 probe show that Arith and Logic's seeds affect operand bits; BitSel's
seeded value is assigned to an unused `dummy` and does not affect its subsequent
unseeded random stream on this Icarus. No diagnostic filtering or allowlist was
applied. Random generation needs an explicit input/seed contract before
claiming identical inputs across the two future execution lanes. This is a
testsuite/planner finding, not a proposed compiler change.

An existing control is available on supported Perl builds:
[`PERL_RAND_SEED`, introduced in Perl 5.38](https://perldoc.perl.org/perl5380delta#PERL_RAND_SEED).
With this host's Perl 5.40.1, two runs of each unmodified generator with value
`42` produced identical Verilog bytes, while value `43` changed them. This
control is not assumed portable across Perl versions or builds. Future captures
record the variable and `/usr/bin/perl` identity, but the two full baseline runs
above did not set the variable.

Evidence is under `testsuite/.stage1-validation/baselines/`: raw per-directory summaries
and logs, independent discovery lists, configuration and completion records,
both imported JSON manifests, `comparison.txt`, and `comparison-summary.json`.
The second run's generated random-test inputs are additionally preserved under
`run-2/generated-inputs/`. No Buck2 parity or caching result is claimed.

## Measured internal-check cost

Four sequential full-suite runs completed on 2026-10-04 in
enabled-disabled-disabled-enabled order. All used the same installation,
tracked testsuite sources, `PERL_RAND_SEED=42`, SystemC settings, and root
`make -j128 -C testsuite fullparallel` command. Each run covered all 866 scripts,
matched independent discovery, and preserved source and installation hashes.
The two rejected attempts above are excluded.

| Run order | Internal checks | Suite wall time | PASS | XFAIL | Unexpected |
| --- | --- | ---: | ---: | ---: | ---: |
| 1 | Enabled | 1,014.54 s | 25,591 | 134 | 0 |
| 2 | Disabled | 909.77 s | 20,405 | 134 | 0 |
| 3 | Disabled | 915.28 s | 20,405 | 134 | 0 |
| 4 | Enabled | 996.91 s | 25,591 | 134 | 0 |

Enabled mean: **1,005.73 s**. Disabled mean: **912.52 s**. Internal checks add
**93.20 s, or 10.21%**, in this experiment. Both strict within-mode Haskell
comparisons passed with zero differences, including diagnostics. This is a
local estimate with two observations per mode on an i9-13980HX host with 32
logical CPUs; it does not establish performance on other hosts or Buck2.

Each enabled run adds **5,186 passing assertions** and **5,173 timed tool
invocations**. Mean direct checking costs per enabled run are:

| Timing category | Invocations | User CPU (s) | System CPU (s) | Summed elapsed (s) |
| --- | ---: | ---: | ---: | ---: |
| `dumpbo` | 3,866 | 849.640 | 240.440 | 2,832.380 |
| `dumpba` | 1,269 | 77.210 | 55.115 | 488.620 |
| `dumpbi` (`dumpbo -bi`) | 10 | 0.045 | 0.200 | 1.640 |
| `vcdcheck` | 27 | 0.385 | 0.735 | 7.085 |
| `bsc2bsv` | 1 | 0.030 | 0.040 | 0.465 |
| Total | 5,173 | 927.310 | 296.530 | 3,330.190 |

These categories are absent from the disabled timing records. They consume
1,223.84 s of direct CPU per enabled run. Across all timed categories, mean
user CPU is 17,447.855 s enabled versus 16,223.740 s disabled (+7.55%); mean
system CPU is 3,511.935 s versus 3,187.985 s (+10.16%). Invocation counts for
all other categories match. Timed-command totals omit untimed harness work;
summed child elapsed time is not suite wall time because commands execute
concurrently, and nested command timings can overlap.

The 13-assertion excess over timed invocations is explained by `gensign`'s
three shadowing dumps (12 assertions from three `dumpbi` commands), its two
re-export dumps (five assertions from two `dumpbo` commands), and `noinline`'s
one assertion reusing an automatic dump. A multiset comparison by script,
exact label, and disposition found 5,186 additions, all PASS, and zero removals.
This reconciles counts; it is not an outcome-independent semantic mapping or
a Buck2 parity claim.

Raw evidence, imported manifests, comparisons, repeated-label inventories,
and `report.json` are in `testsuite/.stage1-validation/internal-cost/`. Source SHA-256:
`b5f295854a1bf44c95ccdb0c5865a79b0a450af91dbab83b6db2cf26fc6b45ed`.
Installation SHA-256:
`92fefbc40c57182263d2e18812bd7c8d6276c13da0b7bb0947a89cc836075226`.

The first report invocation mistook generated `run-1.json` and repeated-label
sidecars for extra run directories. Directory enumeration was corrected, with
regressions confirming sidecars are accepted while missing/extra run
directories remain errors. All 11 report tests pass. Reporting was rerun from
the unchanged archives, with no suite rerun; the original failure state and
runner log are preserved. Independent raw-record aggregation agrees with the
report. The measurement monitor is paused.

## Next gates

1. Model the three random generators and their seeds explicitly so both lanes
   can test identical generated inputs. Preserve the raw diagnostic differences
   until that contract is implemented; do not suppress them to obtain equality.
2. Add outcome-independent semantic check IDs and mapping from DejaGNU. Complete
   the semantic catalogue of `config/unix.exp` and `config/verilog.tcl`.
3. Add versioned TestPlan semantics, static Tcl evaluation, `plan` and `explain`,
   golden plans, and extend differential coverage to nested-script evaluation.
4. Pin a dated Buck2 binary and prelude. First test a trivial action against
   `bazel-remote`: verify a cache hit after daemon restart, then a miss after
   changing the toolchain-key platform property. The CI default is a per-job
   sidecar whose storage is persisted with `actions/cache`; this has not been
   validated here. Implement generic rules and tool identities, then testsuite
   rules and `emit-buck2`; checks produce verdict records through build actions.
   Verify sandbox-independent artifacts before claiming that property.
5. Lower all remaining constructs or prepare independent behavior-preserving
   cleanups; add same-installation per-check parity CI and measurements.

The cache endpoint and credentials are not configured. No shared cache writes,
publication, or upstream cleanup PRs are part of this first slice.
