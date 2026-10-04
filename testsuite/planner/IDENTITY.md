# Legacy verdict identity and planned test identity

This importer establishes a conservative baseline for the migration. It is
**not completion of D1's shared semantic check identity**, and it does not
claim that all PASS/FAIL transitions can be matched to the same legacy ID.
The planner and Buck2 backend must eventually identify the assertion being
tested independently of the message used to report its result.

The version 3 `plan` command emits the separate `file-test-number-v1` identity:
the suite-relative script and a positive test number, with no assertion role.
The counter resets in each `.exp` and advances only for recognized
`compile_pass` and `compile_fail` invocations, including calls whose arguments
cannot be lowered. Those calls retain their reserved IDs as planning issues;
other unsupported constructs have a null ID and do not advance the counter.
The selector is `v3:LENGTH:FILE:NUMBER`, where length counts Unicode characters
in the path. Assignments and loop structure do not consume numbers; each
supported invocation expanded from a loop does. Outcomes, labels, comments,
whitespace, and internal-check settings do not choose IDs.

Numbers are stable only within a frozen supported-invocation sequence. Adding
a supported call or changing loop expansion can renumber later tests; changing
arguments can preserve a number while changing its meaning. The full plan and
configuration determine test meaning. See [PLAN.md](PLAN.md) for the exact
contract. The `.sum` importer described below is unchanged. The opt-in
provenance bridge now checks supported invocations and their observations, but
does not complete D1 or establish whole-suite parity.

One planned test can imply several runtime assertions. An expected-success
compilation test derives its optional internal object check from the global
`internal_checks` setting. That check is not a separate test or test number.
The v3 core records semantic tests and issues, not operations, assertion nodes,
or an action graph. Planned-test and issue counts therefore cannot be compared
directly with legacy PASS/FAIL totals.

## Supported-invocation provenance

Set `BSC_TEST_TRACE=1` when running the legacy harness to enable direct
logging in `testsuite/config/unix.exp`. DejaGNU's per-file `bsc_init` and
`bsc_finish` hooks delimit scripts and reset the counter. `compile_pass` and
`compile_fail` call small helpers directly, using one caller `info frame` for
source location. Calls from procedure wrappers are excluded from numbering.
The planner and harness therefore count the same supported boundary without
promising arbitrary wrapper or call-stack interpretation.

Metadata is written to `testrun.log` as `BSC-TEST: ` followed by a positional
Tcl list, parsed as data. Version 1 records are `script 1 FILE INTERNAL`,
`begin N PROC SOURCEFILE LINE ARGLIST`, `role N object-load`, `end N`, and
`finish COUNT`. Ordinary final verdicts between `begin` and `end` supply the
results; their default compilation role changes at an explicit internal-check
role marker. The object load retains the compilation's parent test number.
No separate result records or sidecar files are required.

`bsc-test-plan correlate PLAN.json LOG-DIRECTORY-OR-FILE` reads `testrun.log`
files recursively from a directory or reads a specified log. It uses the
number to find candidates and validates source file and line, procedure family,
typed resolved arguments, internal-check policy, and expected role order. The readable
report includes `MATCH`/`SKIP` rows, counts, and explicit problems. Numbered
unplanned calls are reported as skipped; unnumbered unsupported constructs do
not block supported-subset correlation. Missing/extra or mismatched
invocations, missing/inconsistent markers, script errors, unexpected result roles, and non-PASS
ordinary results make the command fail. `XFAIL` does not stand in for modeled
expected-failure semantics.

The initial decoder supports single-line verdicts only. Unhandled multiline
metadata emits an unsupported marker and makes correlation fail. The log
protocol is deliberately limited to these common direct calls.

The version 1 trace protocol does not record global compiler options, so `correlate` rejects
plans whose configuration `compiler_options` field is nonempty. An empty
field does not verify the compiler installation or ambient configuration.
The result establishes declaration correspondence, not full configuration
equivalence.

The extra checks matter because counters alone can silently pair unrelated
invocations after expansion drift. Opaque setup and unknown expansion remain
explicit coverage gaps. Provenance does not rewrite the label identities of
archived `.sum` files or provide a Buck2 execution backend.

## What is identified

The `dejagnu-label-v1` identity is a structured tuple:

1. the suite-relative `.exp` path;
2. the complete result label, including internal newlines and whitespace;
3. a one-based occurrence number among identical labels in that `.exp`.

Disposition is separate from identity. Configuration is a manifest field,
and comparing manifests with different configuration names is an error.
The invocation that imports a run is responsible for recording and checking
the compiler installation identity and the full configuration behind that
name; the importer cannot infer either from `.sum` text.

There is no delimiter-based string concatenation, label deduplication, fuzzy
matching, result-dependent renaming, or global ordinal. Reordering distinct
labels does not change their IDs. Repeated identical labels remain separate
checks, and changing their multiplicity changes the population.

The occurrence number cannot distinguish two identical labels when the
underlying assertions trade places or one disappears. That is a limitation
of the legacy input, not evidence that the two assertions are equivalent.

### Repeated labels and internal checks

The first frozen run contains 1,553 repeated `(test, label)` groups in 303 of
866 scripts (3,209 check occurrences). Most repeated Bluesim execution labels
come from ordinary and VCD-enabled executions. Other causes include compiler
flag variants, rebuild phases, different working directories, reused output
paths, and separate assertions whose parameters are absent from the label.
Some source scripts also repeat the same assertion. None is deduplicated.
The reproducible inventory is `scripts/audit-repeated-labels.py`.

Future runtime observation identities must distinguish scenario, configuration
variant, ordered phase, and assertion role. An internal check belongs to a specific
producer invocation and artifact version, not merely to the producer's display
label. For example, `dumpbo` after each of three compilations of the same source
is three child observations, even when all three filenames and labels match.
Attach the checker role and logical artifact to that producer's runtime
observation identity, without turning each assertion into a planned test.
Explicit checks such as `vcdcheck` also need their predicate and source origin;
they are not necessarily children of the last compiler invocation.

Internal checks are required by the migration's validation configuration and
belong in `rules/bsctest`, outside public `rules/bluespec` rules. The ordinary
DejaGNU parity configuration keeps `DO_INTERNAL_CHECKS=0`; the candidate's
additional internal assertions must pass independently. A future comparison
must use an explicit semantic mapping for that ordinary population, never a
label-based filter that could discard ordinary checks. In particular, the
current harness's `.ba` existence assertion is ordinary; its subsequent
`dumpba` load is internal-check-gated.

Stateful recompile tests must retain ordered mutations and artifact versions
inside one fresh scenario workspace. Preserve individual observations even
when compiler arguments repeat. When a scenario executes, its internal compiler
invocations must exercise the compiler's own dependency checking. Reusing a
cached result for the complete scenario is a separate decision: a deterministic
scenario with complete inputs may be cacheable. Execution locality does not
determine cache eligibility. Backend implementation and verification of these
contracts remain pending; the semantic core assigns no cache policy.

Some harness helpers use different messages for success and failure. For
example, `compile_pass` reports “compiles” or “should compile”. Such a
transition appears as a removed check and an added check. When the exact
label remains the same, a disposition transition is reported directly,
including `XFAIL` to `XPASS`. Dynamic text in labels remains significant.
No normalization silently explains away a difference.

## Discoveries, results, and diagnostics

Every `Running <path>.exp ...` line is a discovery record, even if the test
emits no results. The `.exp` path is normalized lexically against the
**original directory of its `.sum` file**, then made relative to the suite
root. Both root and original summary path must be absolute. Paths outside
the suite are rejected. Symlinks are not resolved: the `.exp` names enabled
by the long-test Makefile remain the discovered names.

When archiving summaries, preserve their relative directory structure and
the original suite root. Passing an archive's own directory as the original
execution directory changes the meaning of relative discovery paths.

All DejaGNU dispositions are preserved:

```
PASS FAIL XFAIL XPASS KFAIL KPASS UNRESOLVED UNTESTED UNSUPPORTED
```

`ERROR`, `WARNING`, and `NOTE` are separate diagnostic records. Their test
context, text, and multiplicity are compared. Diagnostics before the first
discovery have no test context. Unknown result categories are rejected.

Each import requires at least one discovery, a final Summary section, a
terminated final line, and exact agreement between parsed result totals and
the summary's disposition counters. Repeated discoveries, counters, and
structured check IDs are errors. Empty imports and overlapping summary
files are errors. A zero-check test is valid if it is explicitly discovered
and the footer has zero result counters.

The supported input is the one-configuration, one-target summary produced
by the suite's `fullparallel` jobs. Multi-target summaries with per-target
and aggregate footer sections are rejected rather than double-counted.

DejaGNU does not escape multiline messages or provide a completion marker
in `.sum`. The reader treats recognized column-zero result prefixes as new
records; other message lines are continuations. Internal blank lines remain
part of a label, while empty separator lines at its end are removed. Thus
trailing blank message lines and message content that imitates record or
footer framing cannot be recovered unambiguously. Counter checks catch
many such ambiguities, but this format is not a lossless event protocol.
The opt-in harness logging delimits supported invocations, but its initial
decoder also supports only single-line verdicts. It does not remove this
limitation from historical `.sum` inputs.

Similarly, a file cut exactly after a valid footer cannot reveal that it
was truncated. The run orchestrator must require successful process
completion and archive results only after all jobs have exited. The reader
rejects missing footers, missing counters for emitted checks, mismatched
totals, incomplete final lines, and records after the footer.

## Qualification and comparison

`validateDiscovery` checks an independent, configuration-specific manifest
of expected `.exp` files. Comparing two result collections is insufficient:
both could have omitted the same test. For `fullparallel`, generate and
archive the expected list after long-test enabling, using the actual suite
discovery population. Do not construct the expected list from result files.

`compareManifests` validates both inputs, then compares discovery sets,
check IDs, dispositions, and diagnostics. An empty difference means those
properties match. It does **not** mean that the run passed: two runs can
have the same FAIL, XPASS, or UNRESOLVED result. The caller must separately
enforce the clean baseline and report all disposition counts. A reviewed
allowlist, if added later, must identify precise differences rather than a
count tolerance.

The machine-readable archive is JSON with an explicit schema and identity:

```json
{
  "schema": "bsc-testsuite-verdicts",
  "version": 1,
  "identity": "dejagnu-label-v1",
  "configuration": "linux-iverilog",
  "tests": ["bsc.example/example.exp"],
  "verdicts": [{
    "id": {"test": "bsc.example/example.exp", "label": "example compiles", "occurrence": 1},
    "disposition": "PASS"
  }],
  "diagnostics": []
}
```

Unknown or duplicate object fields, unknown schema/identity versions,
malformed values, duplicate IDs, and references to undiscovered tests are
rejected. JSON output sorts discoveries, verdicts, and diagnostics for
stable archives. The structured ID avoids collisions caused by separators
inside a filename or label. A future Buck2 adapter may emit this schema only
when it can supply the corresponding legacy labels and occurrence mapping;
semantic IDs require a distinct identity version and an explicit bridge.

## Library API

`Verdict` exports the data constructors and these entry points:

```haskell
parseSummary       :: String -> FilePath -> FilePath -> String -> Either String Manifest
--                    config    suiteRoot   originalSum  contents
mergeManifests     :: [Manifest] -> Either String Manifest
validateManifest  :: Manifest -> Either String ()
validateDiscovery :: [FilePath] -> Manifest -> Either String ()
compareManifests   :: Manifest -> Manifest -> Either String [Difference]
dispositionCounts :: Manifest -> [(Disposition, Int)]
encodeManifest    :: Manifest -> String
decodeManifest    :: String -> Either String Manifest
```

The codec uses boot libraries only. Call `validateManifest` before encoding
manually constructed values; decoding always validates its input.
