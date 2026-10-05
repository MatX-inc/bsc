# BSC testsuite planner bootstrap

This independent Haskell package provides strict population comparison, a Tcl
source census, an initial compile-only semantic planner, and supported-test
provenance correlation. Its first execution backend runs supported compilation
tests and their internal checks through Buck2 build actions. DejaGNU and make
remain the full-suite behavioral oracle. See [the Buck2 workflow](../buck2/README.md).

Run these commands from the repository root. GHC 9.6.7 or newer is required;
the package uses only GHC boot libraries and needs no Hackage downloads.

```sh
cabal build --offline --project-dir=testsuite/planner all
cabal test --offline --project-dir=testsuite/planner all
python3 testsuite/planner/test/check_hidden_evidence.py
python3 testsuite/planner/test/check_timing_report.py
python3 testsuite/planner/test/check_capture_lifecycle.py
cabal run -v0 --offline --project-dir=testsuite/planner bsc-test-plan -- --help
cabal run -v0 --offline --project-dir=testsuite/planner bsc-test-plan -- census testsuite
cabal run -v0 --offline --project-dir=testsuite/planner bsc-test-plan -- census --json testsuite
```

`plan --config NAME --suite-root ROOT TARGET` lowers a `.exp`, a `bsc.*` group,
or the whole suite to version 3 semantic TestPlan JSON. Supported tests and
located unsupported or unresolved items remain in source order. The command
emits JSON and exits successfully for an incomplete plan, with diagnostics and
counts on stderr; configuration, selection, and I/O errors remain fatal.
Counts describe planned tests, unsupported constructs, and unresolved items,
not runtime assertions. `explain PLAN.json TEST-OR-ISSUE-ID` describes one test's
semantic obligations or one numbered issue's reason, with its source origin.

Internal checks default to enabled and are derived by semantic procedures for
expected-success compilation tests and unexpected success in diagnostic-error
tests; they are not separate planned tests. The
initial lowerer supports package compilation, audited scalar assignments, and
finite loops. See [PLAN.md](PLAN.md) for the exact restrictions, v3 schema, and
execution and identity boundaries.

The Haskell test suite includes plan and explanation goldens. To additionally
test the real command-line interface:

```sh
PLANNER=$(cabal list-bin --project-dir=testsuite/planner bsc-test-plan)
python3 testsuite/planner/test/check_cli_plan.py "$PLANNER"
python3 testsuite/planner/test/check_provenance.py
python3 testsuite/planner/test/check_cli_correlate.py "$PLANNER"
```

The direct-logging and correlation checks use installed Tcl and DejaGNU framework
procedures with stub compiler calls. They do not run the compiler suite.

The census reads source without executing Tcl, shell commands, or test programs.
It reports lexical command sites and unsupported script bodies with their
locations. This lexical census does not assess the new lowerer's supported
subset. These are source inventory counts, not PASS/XFAIL/FAIL counts. Inactive long-test templates are
reported separately; `fullparallel` enables the active `.exp` symlinks.

To compare top-level lexical command and word boundaries against Tcl's native
parser over every active test script:

```sh
python3 testsuite/planner/scripts/check-tcl-boundaries.py . \
  --output-dir testsuite/.stage1-validation/tcl-boundaries
```

This optional test requires `cc`, `pkg-config`, Tcl 8.6 development files, and
GHC (`--ghc PATH` selects another GHC). It calls `Tcl_ParseCommand`; it never
evaluates or sources repository Tcl. It checks raw source boundaries including
UTF-8 offsets and verifies that the corpus and lexer did not change during the
comparison. It rejects empty corpora, parse errors, and all mismatches. Native
Tcl folds constant argument expansions differently from our lexical `Expanded`
word; such an input fails explicitly, and is not silently excluded. This gate
does not validate semantic evaluation of nested script bodies.

## Structure and reading guide

The library modules live directly under `src/`. Start with the data types in
`TestPlan.hs`, then `Procedures.hs` for what those tests mean. Next read
`Lower.hs` for Tcl adaptation and `app/Main.hs` for command dispatch, selection,
and reporting. For legacy correlation, read the small logging helpers and
their direct calls in `testsuite/config/unix.exp`. The adapter uses `Tcl.hs` to read commands and words with source
positions, statically evaluates audited assignments and finite loops, and
adapts compilation declarations through the semantic procedures. Unsupported test
procedures are retained without discarding adjacent supported tests. Opaque
setup can make later declarations unresolved, and unknown assignments
invalidate values instead of allowing stale substitution. `Tcl` is a lexical
reader with limited scalar substitution support; it does not run Tcl.

A plan contains configuration and scripts; script items contain
tests or located planning issues. A test has a file-local test number, origin, and
kind, currently compilation plus its expected result. `Procedures.hs` shares
the meaning of `compilePass`, `compileFail`, and `compileFailError`, including
exact diagnostic counts and conditional internal-check obligations. These Haskell functions need
not correspond one for one to Tcl helpers. The core contains no execution
steps, action graph, workspace snapshots, or cache policy. Input and tool
binding live in `Buck2.hs` and `Execute.hs`. Read `Execute.hs` next for private
source staging, compiler observations, and internal object checks, then
`Buck2.hs` for snapshot emission and the rules under `rules/`. Shared-state
scenarios and complete host isolation remain future work.
Read the JSON codec and lexical parser last unless investigating serialization
or Tcl syntax; they are supporting machinery rather than the test vocabulary.

Two separate paths support migration: `Census.hs` inventories lexical source
sites without proving they can be lowered, while `Verdict.hs` imports and
strictly compares legacy DejaGNU summaries. Verdict identities and plan test
identities remain separate. Opt-in provenance provides a checked bridge for
supported compile-test invocations; see [IDENTITY.md](IDENTITY.md). A successful
plan or supported-subset correlation does not establish whole-suite execution
parity with the legacy harness.

## Correlate supported tests with the legacy harness

IDs use the `file-test-number-v1` identity: the suite-relative `.exp` path and a
positive number. The counter resets for each script and advances only for
recognized `compile_pass`/`compile_fail`/`compile_fail_error` invocations. A recognized call reserves
a number even if static argument lowering fails. Other unsupported constructs
have no number. Repeated loop invocations remain distinct, and internal object
checks retain their parent test number. `explain` selectors have the form
`v3:LENGTH:FILE:NUMBER`, such as `v3:18:bsc.plan/basic.exp:1`.

Set `BSC_TEST_TRACE=1` when running the legacy suite to enable direct logging
in `testsuite/config/unix.exp`. The existing baseline capture workflow below
supplies the required suite settings and monitoring. DejaGNU's per-file tool
hooks, `bsc_init` and `bsc_finish`, delimit each script. `compile_pass` and
`compile_fail` call small logging helpers directly and supply one caller frame
for source location. Calls from procedure wrappers are excluded from numbering.
Metadata and ordinary verdicts stay in `testrun.log`; no separate trace files
are created.

Create a plan with the same internal-check setting and no global
`--compiler-option` arguments, then:

```sh
BSC_TEST_TRACE=1 \
  python3 testsuite/planner/scripts/capture-baselines.py \
  --internal-checks 1 --runs 1 \
  --output-dir testsuite/.stage1-validation/trace-baseline
PLANNER=$(cabal list-bin --project-dir=testsuite/planner bsc-test-plan)
"$PLANNER" correlate plan.json testsuite/.stage1-validation/trace-baseline/run-1
```

The capture requires the built compiler and supplemental tools described
below. The version 1 trace protocol does not record global compiler options,
so `correlate` rejects a nonempty plan `compiler_options` field. Even
with that field empty, correlation does not verify the tool installation or
ambient configuration; it establishes declaration correspondence, not full
configuration equivalence.

The second argument is a directory searched recursively for `testrun.log`
files, or a specific log file. The readable report contains `MATCH`/`SKIP` rows,
counts, and problems. Correlation checks source file and line, procedure
family, resolved arguments, internal-check policy, and the expected
compilation/object-load result roles. Ordinary final verdicts between the
invocation markers are authoritative; internal checks have an explicit role
marker. The initial decoder supports single-line verdicts only. Unhandled
multiline metadata produces an unsupported marker and fails correlation.
Missing or inconsistent markers, script errors, missing/extra or mismatched
invocations, unexpected roles, and non-PASS ordinary results also fail. Numbered planning
issues are skipped explicitly; unnumbered unsupported constructs alone do not
block the supported tests. Unknown expansion and opaque setup remain coverage
gaps. See [PLAN.md](PLAN.md) for the data protocol and exact boundary.

## Capture validation runs

Build the compiler from the root, then install the supplemental internal-check
tools and optional Bluetcl scripts:

```sh
make -j32 GHCJOBS=16 install-src
make -C src/comp -j1 GHCJOBS=16 install-extra
python3 testsuite/planner/scripts/install-bluetcl-extras.py
```

Capture defaults to two full runs with internal checks enabled
(`--internal-checks 1 --runs 2`). It checks that `dumpbo`, `dumpba`, `vcdcheck`,
and `bsc2bsv`, including their installed core executables, are present and
executable before creating evidence. For one complete validation run:

```sh
python3 testsuite/planner/scripts/capture-baselines.py --internal-checks 1 --runs 1 \
  --output-dir testsuite/.stage1-validation/internal-checks
```

One run validates that configuration; it does not establish two-run parity.
The initial comparison used the ordinary lane with internal checks disabled.
Request that lane explicitly when capturing another pair:

```sh
python3 testsuite/planner/scripts/capture-baselines.py --internal-checks 0 --runs 2
```

The capture script uses `make -j128 -C testsuite fullparallel`, which cleans the
tree and enables long tests before discovery. It sets `inst/bin` on `PATH`,
`TEST_SYSTEMC_INC=/usr/include`, `TEST_SYSTEMC_LIB=/usr/lib/x86_64-linux-gnu`,
and `TEST_SYSTEMC_CXXFLAGS=-std=c++17`. It reports per-directory `testrun.sum`
counts every two minutes, flags failures as they appear, and reports final
tallies. It stops after a failed run and refuses to overwrite existing evidence.

Captures share a checkout lock at `testsuite/.stage1-validation/capture.lock`; never
remove this file to bypass an active capture. The capture cleans up its command
process group on SIGINT/SIGTERM and refuses to advance while that group remains.
These guards do not cover old capture processes, manually launched suites, or
commands that deliberately create a separate session. Wait for any such run to
finish before starting a capture.

Evidence is saved under `testsuite/.stage1-validation/baselines/`. Hidden
directories immediately under `testsuite` are ignored by Git and excluded from
legacy test discovery, statistics, log archiving, and recursive cleanup. Each run contains its raw log, a directory-preserving summary snapshot,
`all_tests.mk`, `expected-tests.txt`, and `result.json`. The shared
`configuration.json` records the compiler-installation digest, requested run
count, and configuration, including `DO_INTERNAL_CHECKS`.
`expected-tests.txt` is independently discovered under `bsc.*` and checked
against the scheduled test paths. Infrastructure inputs such as `site.exp`
and `config/unix.exp` are recorded separately in `harness-inputs.txt`.
Keep the internal-enabled and ordinary lanes in separate evidence directories
and use different configuration names when importing them. No Buck2 comparison
is claimed by this capture script. Custom evidence output may be under
`testsuite/.stage1-validation` or outside `testsuite`; ordinary test directories
are rejected, including paths redirected there by symlinks.

Existing evidence was relocated intact from the repository-root
`.stage1-validation` directory. Paths embedded in archived logs and metadata
retain their capture-time values; substitute the new evidence root when locating
those files. `testsuite/.stage1-validation/relocation.json` records the move and
checksum verification.

```sh
PLANNER=$(cabal list-bin --project-dir=testsuite/planner bsc-test-plan)
python3 testsuite/planner/test/check_cli_import.py "$PLANNER"
EVIDENCE=testsuite/.stage1-validation/baselines
CONFIG=linux-iverilog-bluesim-systemc-internal0
"$PLANNER" import-sum --config "$CONFIG" --suite-root "$PWD/testsuite" \
  --expected "$EVIDENCE/run-1/expected-tests.txt" \
  "$EVIDENCE/run-1" "$EVIDENCE/run-1.json"
"$PLANNER" import-sum --config "$CONFIG" --suite-root "$PWD/testsuite" \
  --expected "$EVIDENCE/run-2/expected-tests.txt" \
  "$EVIDENCE/run-2" "$EVIDENCE/run-2.json"
"$PLANNER" compare --expected "$EVIDENCE/run-1/expected-tests.txt" \
  "$EVIDENCE/run-1.json" "$EVIDENCE/run-2.json"
```

`import-sum` requires an archive mirroring the **original test directory layout**;
`--suite-root` names the original absolute testsuite root. Absolute execution
paths and relative `Running` paths are converted to suite-relative paths.
Only per-directory `testrun.sum` files are imported, avoiding double counting
an aggregate summary. An expected manifest contains one canonical suite-relative
`.exp` path per line and is mandatory for import and comparison.

Comparison returns nonzero on missing/added scripts, missing/added checks,
changed dispositions or diagnostics, invalid inputs, or differing configuration
names. Equality is not a clean-test verdict: identical FAIL results still compare
equal. Inspect dispositions and require a clean baseline independently. The
capture script does this before allowing a second run.
The initial full runs have matching checks but different random-seed notes in
three generated tests; strict comparison reports these differences. See the
status document for the exact records and the required future seed contract.
For a future capture on a Perl build supporting `PERL_RAND_SEED` (introduced in
Perl 5.38), that environment variable can hold the generators' Perl seed fixed
across the pair:

```sh
PERL_RAND_SEED=42 python3 testsuite/planner/scripts/capture-baselines.py \
  --internal-checks 0 --runs 2 \
  --output-dir testsuite/.stage1-validation/seeded-baselines
```

The capture records this setting and the identity of `/usr/bin/perl`, which the
generators' shebangs select. Verify support on that Perl build; this controls
Perl randomness, not every possible source of nondeterminism, and the comparison
remains strict.

## Measure internal-check cost

Install the same extra tools for both configurations. Once all tests pass,
capture four full runs in enabled-disabled-disabled-enabled order:

```sh
export PERL_RAND_SEED=42
python3 testsuite/planner/scripts/capture-baselines.py --internal-checks 1 --runs 1 \
  --output-dir testsuite/.stage1-validation/cost-enabled-first
python3 testsuite/planner/scripts/capture-baselines.py --internal-checks 0 --runs 2 \
  --output-dir testsuite/.stage1-validation/cost-disabled-pair
python3 testsuite/planner/scripts/capture-baselines.py --internal-checks 1 --runs 1 \
  --output-dir testsuite/.stage1-validation/cost-enabled-last
python3 testsuite/planner/scripts/report-timings.py --compare-internal-checks \
  testsuite/.stage1-validation/cost-enabled-first testsuite/.stage1-validation/cost-disabled-pair \
  testsuite/.stage1-validation/cost-enabled-last --output testsuite/.stage1-validation/internal-cost.json
```

The report requires clean completed runs with identical installation, test
source, seed, and configuration metadata apart from the internal-check setting.
The capture archives raw `time.out` files and measures suite wall time before
archiving. The report gives every run's wall time, enabled/disabled means and
deltas, verdict counts, and per-category command timings. CPU totals cover the
harness's timed commands, not all harness work; summed command elapsed time is
not suite wall time because commands run concurrently. Four runs provide a
local estimate with ordering balanced, not a general performance guarantee.

The ordinary verdict populations still need the strict importer/comparator;
the timing report is not a substitute for semantic parity verification.

## Audit repeated labels

```sh
python3 testsuite/planner/scripts/audit-repeated-labels.py \
  testsuite/.stage1-validation/baselines/run-1.json \
  --log-root testsuite/.stage1-validation/baselines/run-1
```

This preserves every occurrence and reports exact within-script repetition,
cross-script shared wording, label families, and the recorded commands for
repeated Bluesim execution labels. It does not infer semantic equivalence or
deduplicate assertions. Reviewed causes and limitations are in the Stage 1
status document.

See [IDENTITY.md](IDENTITY.md) for legacy identity limitations and the semantic
test-ID contract, and the [Stage 1 status](../../doc/testsuite-planner-stage1.md)
for rule placement, evidence, and remaining gates.

On a machine whose Cabal repository has never been initialized, Cabal may try
to bootstrap repository metadata even with `--offline`. For a strictly
offline build, pass `--config-file=PATH` before the subcommand, using a Cabal
config with no `repository` blocks and `active-repositories: :none`. The
package itself needs no downloaded dependencies.
