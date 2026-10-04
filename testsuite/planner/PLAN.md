# Initial TestPlan contract

The version 1 schema describes a deliberately small subset of test semantics.
It supports package compilation and object loading, with separate assertions.
It does not execute tools, generate Buck2 targets, resolve tool installations,
or establish parity with DejaGNU. Unsupported scripts produce a located error;
the planner never returns their successfully parsed or lowered prefix.

## Commands

Run from the repository root:

```sh
cabal run -v0 --offline --project-dir=testsuite/planner bsc-test-plan -- \
  plan --config compile-checks --suite-root testsuite \
  testsuite/bsc.example/example.exp > plan.json
cabal run -v0 --offline --project-dir=testsuite/planner bsc-test-plan -- \
  explain plan.json 'CHECK-ID'
```

The example test name is a placeholder for a selected script. `plan` accepts
one `.exp`, a top-level `bsc.*` group, or the suite root. It emits JSON only if
every selected script lowers. Unsupported syntax or semantics produces exit
status 2, an empty stdout, and diagnostics naming the script, line, column,
construct, and reason. Selection and argument errors also exit unsuccessfully.
Discovered empty scripts remain in the plan with empty step/check lists.

`--config NAME` and `--suite-root ROOT` are required. Internal checks default to
enabled; `--internal-checks 0` selects the ordinary configuration. Repeated
`--compiler-option FLAG` arguments provide explicit compiler configuration.
Ambient `BSC_OPTIONS`, Tcl variables, tools, and host facts are not read.
A future executor must bind the declared tool roles to the complete compiler
installation, including libraries, and honor the explicit configuration.

## Closed lowering vocabulary

The initial lowerer accepts:

- `compile_pass source ?options? ?nodeps?` and
  `compile_fail source ?options? ?nodeps?`;
- `set name ?value?`, for an audited ordinary scalar name;
- single-variable `foreach name list {body}`, including finite nested loops;
- literal, braced, and quoted words; scalar `$name` and `${name}` substitution;
  Tcl 8.6 escape and list decoding.

Source names must be `.bs` or `.bsv` basenames made from letters, digits,
periods, underscores, and hyphens. `nodeps` is `0` or `1`. Supported compiler
flags are `-let-gen`, `-no-let-gen`, `-dinternal`, and `-v`, with ordering and
duplicates preserved. Unknown flags are rejected because they can alter
outputs, search paths, or execution phases. The option string undergoes a
second Tcl parse, as in the harness; command substitution, argument expansion,
and separator-bearing strings are rejected.

Accepted scalar names are `source`, `sources`, `src`, `file`, `files`,
`filename`, `flags`, `flag`, `opts`, `options`, `name`, `stem`, `extension`,
`suffix`, `variants`, `variant`, `unused`, `x`, `y`, `i`, `j`, and `command`.
Tests are sourced at global scope by DejaGNU, so adding a name requires checking
that it cannot change harness or framework state. Array variables, procedures,
conditionals, arbitrary command substitution, expected-failure helpers,
backend-specific compilation, comparisons, and file mutations are unsupported.
Static expansion is bounded to prevent unbounded plan construction.

## Operations and assertions

| Source helper | Operation | Assertion | Additional work |
| --- | --- | --- | --- |
| `compile_pass` | Package compile, with dependency compilation unless `nodeps=1` | Tool succeeds | Object load and its own success assertion when internal checks are enabled |
| `compile_fail` | Same package compile | Tool fails | None |

`bsc-compile` inherently suppresses compiler version and timestamp text, matching
`bsc_compile` in the harness. The configuration options precede the invocation
options; automatic dependency compilation corresponds to `-u`. Library lookup
uses the bound compiler installation. The merged compiler transcript is
`source.bsc-out`. `internal-load` uses `dumpbo` on `source-stem.bo` and retains
its merged transcript. Status and transcript are first-class outputs of each
operation. Tool failure includes unsuccessful invocation or termination;
planning does not predict the result.

The ordinary compilation assertion and internal object-load assertion have
separate IDs. The internal check refers to the specific compile workspace that
contains the object, including the observation that it might be missing. It
runs after compilation completes even if the actual compile result is a
failure. Assertions do not control whether dependent operations execute.
There is no fallback that treats an unsupported expected-failure helper as an
ordinary pass/fail check; phase-specific expected failures need a later schema
extension.

## Workspace, inputs, and reuse

Each scenario starts in a fresh shared workspace seeded from its test directory.
Steps form an ordered chain of workspace snapshots, preserving intermediate
files and overwritten outputs. A later step consumes its immediate predecessor's
snapshot. A produced-directory reference can select an individual artifact,
such as the object inspected by an internal check.

Suite-file references declare source observations; missing files are values to
preserve for negative tests, not planner errors. Directory snapshots conservatively
retain potential compiler-discovered dependencies. No Bluespec dependency parser
is introduced. Before execution is implemented, input binding must define
transitive external dependencies, initial generated-file cleanup, installation
identity, and snapshot composition. This patch does not claim hermetic execution.

Single-compile scenarios are `cacheable-local-only`; tool identities are not
bound yet. Scenarios containing multiple compiler invocations are conservatively
`never`, because a later invocation may be testing the compiler's own reuse.
Cache restrictions propagate to consumers, including internal checks. File
replacement, delays, timestamp tests, and other explicit recompilation sequences
remain unsupported rather than being flattened into independent compiles.

## Identity and validation

`source-site-check-v1` uses a structured tuple of suite-relative script,
one-based source-command/loop-expansion coordinates, and operation or assertion
role. Comments, whitespace, result labels, and observed outcomes do not choose
an ID. Repeated loop values remain distinct. Internal-check IDs share the source
address of their compiler invocation and have their own role.

This is identity within a frozen source topology and named configuration, not a
semantic content hash or a cache key. Inserting a command or reordering loop
iterations may change addresses. Source and flag changes can retain an address
while changing the plan's semantics. Full plan inputs must therefore be checked
when comparing executions. The legacy `.sum` importer still uses its separate
label-based identity; no automatic mapping or completed D1 parity is claimed.

For `explain`, the selector is `v1:LENGTH:TEST:COORDINATES:ROLE`, where length
counts Unicode characters in the test path and coordinates are dot-separated.
For example, `v1:18:bsc.plan/basic.exp:1:compile.result` selects the ordinary
assertion in the basic plan golden. Explanation includes transitive prerequisites,
inputs, outputs, tools, cacheability, expectation, and source origin.

Decoding rejects unknown/duplicate fields, versions, operation kinds, malformed
paths and IDs, duplicate IDs, inconsistent configuration, missing outputs,
orphan assertions, forward/cyclic references, and weakened cache restrictions.
The Haskell tests include plan/explanation goldens and inert Tcl differential
tests. The CLI tests additionally verify all-or-nothing selection and failure
output. The existing `census` command remains a lexical inventory, not a report
of semantic lowering coverage.
