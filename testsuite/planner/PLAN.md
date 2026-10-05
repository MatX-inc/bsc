# Semantic TestPlan contract

The version 3 plan records discovered tests and constructs that cannot yet be
planned. Its current test kind is package compilation with an expected success
or failure. The model itself contains no execution policy. `Execute.hs` executes
the supported compilation kind and `Buck2.hs` binds inputs for the first local
Buck2 backend; neither establishes whole-suite parity with DejaGNU.

## Commands

Run from the repository root:

```sh
cabal run -v0 --offline --project-dir=testsuite/planner bsc-test-plan -- \
  plan --config compile-checks --suite-root testsuite \
  testsuite/bsc.example/example.exp > plan.json
cabal run -v0 --offline --project-dir=testsuite/planner bsc-test-plan -- \
  explain plan.json 'TEST-OR-ISSUE-ID'
```

The example test name is a placeholder for a selected script. `plan` accepts
one `.exp`, a top-level `bsc.*` group, or the suite root. It emits JSON and exits
successfully even when some items are unsupported or unresolved. Diagnostics
and counts go to stderr. Counts distinguish planned tests, unsupported
constructs, and unresolved items; they are not counts of runtime assertions or
compiler verdicts. Argument, configuration, selection, and I/O errors remain
fatal. Every selected script appears in the plan, including empty scripts.

`--config NAME` and `--suite-root ROOT` are required. Internal checks default to
enabled; `--internal-checks 0` selects the ordinary configuration. Repeated
`--compiler-option FLAG` arguments provide explicit compiler configuration.
Ambient `BSC_OPTIONS`, Tcl variables, tools, and host facts are not read.

## Semantic model and procedures

`TestPlan.hs` contains the saved model: a `TestPlan` has a `PlanConfig` and a
list of `ScriptPlan` values. Each script has a path and source-ordered items,
either `Planned Test` or `Unplanned PlanIssue`. A test contains a file-local
`Identifier`, a diagnostic source origin, and a `TestKind`. The current kind is
`CompilationTest Compilation Expectation`: the compilation records the source,
ordered compiler options, and whether to compile dependencies; the expectation
is `CompileSucceeds` or `CompileFails`.

`Procedures.hs` gives these tests their shared procedural meaning. `compilePass`
and `compileFail` use `compilationTest` with different expectations. The Tcl
adapters in `Lower.hs` resolve arguments and call those semantic constructors.
The Haskell procedure boundary need not mirror Tcl procedures one for one.
Shared behavior belongs in the semantic procedures, rather than being copied
into each source adapter.

| Declaration | Compilation obligation | Optional internal obligation |
| --- | --- | --- |
| `compile_pass` | Compile the package and require success | Inspect the produced object when `internal_checks` is enabled |
| `compile_fail` | Compile the package and require failure | None |

Dependency compilation corresponds to `-u` and is enabled unless `nodeps=1`.
Configuration compiler options precede invocation options, preserving order
and duplicates. The procedure must preserve the harness's suppression of
compiler version and timestamp text, compiler transcript, and expected-result
check. For an expected-success test, the internal object check is derived from
the expectation and global `internal_checks` configuration. It belongs to that
test and its compilation; it is not another planned test or test number.
Its obligation remains associated with that compilation even if the actual
compile result fails. Planning does not predict either result.

The core model has no `Step`, `Check`, action graph, workspace snapshots, or
cache policy. Runtime assertions and backend actions can be derived from a
semantic test without becoming its identity or inflating the planned-test
count. Phase-specific expected-failure helpers need their own supported
semantics; they are not treated as ordinary compile pass/fail declarations.

## Static lowering and incomplete plans

The current lowerer statically evaluates:

- `compile_pass source ?options? ?nodeps?` and
  `compile_fail source ?options? ?nodeps?`;
- `set name ?value?`, for an audited ordinary scalar name;
- single-variable `foreach name list {body}`, including finite nested loops;
- literal, braced, and quoted words; scalar `$name` and `${name}` substitution;
  Tcl 8.6 escape and list decoding.

Source names must be `.bs` or `.bsv` basenames with a nonempty stem, made from
ASCII letters, digits, periods, underscores, and hyphens, without a leading
hyphen. `nodeps` is `0` or `1`. Supported compiler
flags are `-let-gen`, `-no-let-gen`, `-dinternal`, and `-v`. Other flags remain
unsupported because they can change outputs, search paths, or execution phases.
The option string undergoes a second Tcl parse, as in the harness; command
substitution, argument expansion, and separator-bearing strings are unsupported.

Accepted scalar names are `source`, `sources`, `src`, `file`, `files`,
`filename`, `flags`, `flag`, `opts`, `options`, `name`, `stem`, `extension`,
`suffix`, `variants`, `variant`, `unused`, `x`, `y`, `i`, `j`, and `command`.
Tests are sourced at global scope by DejaGNU, so adding a name requires checking
that it cannot change harness or framework state. Static expansion is bounded
to 100,000 commands and loop iterations per script, and each expanded scalar
value is limited to 1,048,576 characters before storage or list parsing. Hitting
the command limit records an issue and stops expansion; its uninspected
remainder is not included in the counts.

Unsupported constructs remain located items in the script. Recognized
`compile_pass` and `compile_fail` calls reserve their test numbers before
argument lowering, so a call that cannot be lowered retains its number as an
issue. Other unsupported constructs do not consume numbers. A known but
unsupported test procedure, such as `compile_verilog_pass`, does not discard
adjacent supported tests. Opaque setup, `exec`, and file mutations can affect
later declarations, so those dependent declarations become unresolved instead
of being planned using assumed state. An unknown assignment invalidates its
variable; later references cannot reuse an earlier known value. Option strings
that could execute Tcl during the harness's second parse also leave setup
unknown, even when the original argument was braced. Calls with unresolved
arguments are also conservative barriers: the real harness may supply values
that this static adapter does not know, including second-pass substitutions.
This uncertainty
is local to the script and does not prevent independent scripts from lowering.

A top-level parse error represents the script as one located unplanned item;
the parser does not recover its prefix or later source. A failure to parse a
nested body is reported at that construct. Retaining supported items is useful
migration evidence, but an incomplete plan is not complete suite coverage.

## JSON and identity

The schema is `bsc-testsuite-test-plan`, version `3`, with identity
`file-test-number-v1`. Configuration retains its existing fields. For example:

```json
{
  "schema": "bsc-testsuite-test-plan",
  "version": 3,
  "identity": "file-test-number-v1",
  "configuration": {
    "name": "compile-checks",
    "internal_checks": true,
    "compiler_options": []
  },
  "scripts": [{
    "path": "bsc.plan/basic.exp",
    "items": [{
      "status": "planned",
      "id": {"test": "bsc.plan/basic.exp", "number": 1},
      "origin": {"file": "bsc.plan/basic.exp", "line": 1, "column": 1, "offset": 0},
      "kind": {
        "kind": "compilation",
        "source": "Basic.bs",
        "options": [],
        "compile_dependencies": true,
        "expectation": "succeeds"
      }
    }]
  }]
}
```

An unplanned item has `status` equal to `unsupported` or `unresolved`, an
`origin`, and `construct` and `reason` strings in place of `kind`. Its `id` has
the structure above when it represents a recognized compile-test invocation;
otherwise `id` is `null`. A compilation expectation is `succeeds` or `fails`.

An identifier consists of the suite-relative script and a positive test
number. Numbering starts at 1 in each `.exp` and advances only for recognized
`compile_pass` and `compile_fail` invocations, in execution order through the
supported static expansion. Calls with unresolved or unsupported arguments
still reserve a number. Assignments, loops themselves, unsupported helpers,
comments, and whitespace do not consume numbers. Repeated loop values remain
distinct tests. An internal check keeps its parent's test ID and uses a result
role; enabling internal checks does not add test IDs.

The number identifies a supported invocation in a frozen script expansion,
not a semantic content hash or cache key. Adding a supported invocation or
changing loop order can change later numbers. Source and flag changes can
retain a number while changing the test's meaning. Compare the full plan and
configuration when comparing runs. Opaque setup and unknown expansion remain
explicit gaps: the counter alone cannot prove correspondence.

For `explain`, use `v3:LENGTH:FILE:NUMBER`, where length counts Unicode
characters in the script path. For example, `v3:18:bsc.plan/basic.exp:1`
selects the example test. The command describes a test's semantic obligations
and origin, or a numbered issue's construct, reason, and origin. Unnumbered
issues remain visible in the plan and diagnostics. It does not traverse a
saved action graph.

Decoding rejects unknown or duplicate fields, unsupported schema or identity
versions, malformed paths and IDs, duplicate IDs, inconsistent source origins,
noncontiguous or out-of-order test numbers, and compilation options that omit
the configuration prefix. Version 1 action
plans and version 2 structural-ID plans are not silently interpreted as
version 3 plans.
Structural validation permits more relative paths and options than the current
procedures implement. `explain` checks that supported subset before interpreting
a test; decoding alone does not establish executable semantics.

## Correspondence with legacy results

Set `BSC_TEST_TRACE=1` before running the legacy harness to enable direct
logging in `testsuite/config/unix.exp`. DejaGNU's per-file tool hooks,
`bsc_init` and `bsc_finish`, reset the counter and delimit each script.
`compile_pass` and `compile_fail` call small logging helpers directly, supplying
one caller `info frame` for source location. Calls from procedure wrappers are
excluded, so unsupported wrappers do not consume numbers. This boundary does
not attempt arbitrary wrapper or call-stack interpretation. The metadata is
written to `testrun.log`, alongside the ordinary final verdicts; `.sum` output
and its legacy identity remain unchanged.

Each metadata line starts with `BSC-TEST: ` followed by a positional Tcl list,
parsed as data without evaluation. Version 1 uses these records:

```text
BSC-TEST: script 1 FILE INTERNAL
BSC-TEST: begin N PROC SOURCEFILE LINE ARGLIST
BSC-TEST: role N object-load
BSC-TEST: end N
BSC-TEST: finish COUNT
```

`script` identifies the suite-relative script and internal-check setting;
`begin` identifies a numbered invocation, its source, and resolved argument
list. The ordinary final verdicts between `begin` and `end` are authoritative.
Their default role is compilation; `role` switches to the internal object-load
check. `finish` supplies the final invocation count. The implementation uses
direct helper calls and existing log output, without Tcl execution traces,
aliases, callback interception, stack scanning, or sidecar-file management.

The initial decoder supports only single-line verdicts. Unhandled multiline
metadata emits an unsupported marker and correlation rejects the log; this
format does not claim lossless support for arbitrary Tcl or result text.

```sh
cabal run -v0 --offline --project-dir=testsuite/planner bsc-test-plan -- \
  correlate plan.json testsuite/.stage1-validation/trace-baseline/run-1
```

`correlate PLAN.json LOG-DIRECTORY-OR-FILE` reads `testrun.log` files recursively
from a directory or reads a specific log file. It associates planned tests
with observed invocations by script and number, then checks the
internal-check policy, procedure family,
source file and line, and typed resolved arguments through the same lowering
used by the planner. It checks the expected result-role sequence: `compilation`
followed by `object-load` when the test requires an internal check. A compilation
result and its internal object load keep the same parent test ID. Repeated
flag variants stay separate even
when their printed labels are identical. The report identifies mismatches,
missing or extra evidence, and skipped numbered planning issues, with
`MATCH`/`SKIP` rows and final counts. There is no JSON report in this slice.
Missing or inconsistent markers, script errors, mismatches, missing/extra invocations, and
unexpected result roles make the command fail. Non-`PASS` results from matched
ordinary tests are also problems; `XFAIL` is not silently accepted without
represented expected-failure semantics. Unsupported constructs alone do not
block correlation of supported tests. A script containing only unnumbered
issues need not have logged invocations, but a supplied log can still reveal extra
supported invocations.

The version 1 trace protocol does not record global compiler options. `correlate` therefore
rejects plans with nonempty `configCompilerOptions` (the configuration's JSON
`compiler_options` field). Invocation-specific options are still checked
through typed argument lowering. An empty global-options field does not
establish that the tool installation or ambient configuration matches: this
command checks declaration correspondence, not full configuration equivalence.

This is correspondence for the supported compile-test subset, not a complete
DejaGNU-to-Buck2 parity claim. The number alone is insufficient: procedure,
source, and argument checks prevent silently pairing tests after source or
expansion drift. Unknown control flow and opaque setup can leave gaps that
require more semantic coverage. The strict `.sum` importer remains useful for
whole-run population comparison and does not acquire semantic IDs from these
log markers.

## Backend boundary

The initial executor binds an installation, stages private sources, and retains
transcripts, artifacts, and verdicts. Its directory-subtree input policy has
been audited for the current supported corpus; arbitrary cross-directory
inputs and shared-state scenarios need further semantics. Deliberately missing
sources remain observable for negative tests. The backend declares the entire
source and installation snapshots conservatively and runs locally, with cache
upload disabled. This is not a hermetic or persistent-cache guarantee.

An incremental scenario must preserve ordered mutations and compiler invocations
within its shared workspace when it executes. That does not automatically make
the complete scenario ineligible for caching: a deterministic scenario with a
complete input and tool contract may admit whole-scenario reuse. Ensuring its
internal invocations execute when the scenario runs is a separate requirement.
Execution locality is also separate from cache eligibility. These are backend
decisions, not fields of the semantic test model.

Plan/explanation goldens, inert Tcl differential fixtures, and CLI tests cover
the supported contract. The `census` command remains a lexical inventory; its
counts are not semantic lowering coverage or runtime assertion counts.
