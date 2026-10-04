# BSC testsuite planner bootstrap

This independent Haskell package provides strict population comparison, a Tcl
source census, and an initial compile-only semantic planner. It does not execute
tests or emit Buck2 targets yet. DejaGNU and make remain the execution backend.

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

`plan --config NAME --suite-root ROOT TARGET` lowers a supported `.exp`, a
`bsc.*` group, or the whole suite to versioned TestPlan JSON. If any selected
script is unsupported, the command reports its location and emits no plan.
`explain PLAN.json CHECK-ID` shows one check and all its prerequisites.
Internal checks default to enabled. The initial vocabulary is package
compilation, internal object loading, scalar assignments, and finite loops;
see [PLAN.md](PLAN.md) for configuration, exact restrictions, and the schema's
execution and identity boundaries.

The Haskell test suite includes plan and explanation goldens. To additionally
test the real command-line interface:

```sh
PLANNER=$(cabal list-bin --project-dir=testsuite/planner bsc-test-plan)
python3 testsuite/planner/test/check_cli_plan.py "$PLANNER"
```

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

See [IDENTITY.md](IDENTITY.md) for legacy identity limitations and the future
semantic-ID contract, and the [Stage 1 status](../../doc/testsuite-planner-stage1.md)
for rule placement, evidence, and remaining gates.

On a machine whose Cabal repository has never been initialized, Cabal may try
to bootstrap repository metadata even with `--offline`. For a strictly
offline build, pass `--config-file=PATH` before the subcommand, using a Cabal
config with no `repository` blocks and `active-repositories: :none`. The
package itself needs no downloaded dependencies.
