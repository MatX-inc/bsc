# Compiler dependency queries

`bsc -dependencies FILE` describes the inputs to an otherwise ordinary compiler
invocation. `FILE` is a JSON output file; use `-` for standard output. The option
currently appears in `-help-hidden` because the versioned report is experimental.
It is a command-line query, not a stored compiler flag or a new binary format.
Both file and standard-output reports use UTF-8.

For example:

```sh
bsc -dependencies deps.json -u Top.bs
bsc -dependencies deps.json -u -sim -g mkTop Top.bs
bsc -dependencies deps.json -u -verilog -elab -g mkTop Top.bs
bsc -dependencies deps.json -sim -e mkTop mkTop.ba
bsc -dependencies deps.json -sim -e mkTop -KILLsimBlocksToC mkTop.ba
bsc -dependencies deps.json -verilog -vsim iverilog -e mkTop mkTop.ba mkTop.v
bsc -dependencies deps.json -verilog -vsim iverilog -e mkTop mkTop.v
```

Options still precede filenames. Ordinary argument validation, search-path
expansion, `BSC_OPTIONS`, and `BLUESPECDIR` apply. The query does not compile,
elaborate, emit compiler products, execute filters, or run native compilers or
simulators. It can read existing source and binary metadata. Normal compiler
errors encountered during that discovery are retained as incomplete discovery.
Diagnostics can also be written to standard error; standard output is reserved
for the report when `FILE` is `-`.

## What the report means

The top-level `schema` is `bsc-dependencies`, with `version` equal to `1`.

| Field | Meaning |
| --- | --- |
| `mode` | The source compilation or artifact operation being described. |
| `complete` | Whether discovery established a conservative input boundary. This is not a prediction that compilation succeeds. |
| `requirements` | Requirements grouped by the artifact that uses them, with policies and candidate files. |
| `conditional_requirements` | The same requirements with explicit branch conditions; an empty `when` list means unconditional. |
| `potential_outputs` | Statically identifiable products. This list is neither exhaustive nor a promise that a failing compile creates them. |
| `notes` | Operation-wide interpretation and substitution constraints. |
| `incomplete` | Reasons the report cannot be used alone as a complete input declaration. |

Each requirement has an `owner`, `role`, `policy`, `candidates`, and `notes`.
The owner identifies the source, object, or module that uses those inputs.
Requirements belonging to an alternative artifact describe that alternative;
they do not assert that every alternative will be used on every execution.
Discovery deliberately visits all available alternatives where it can, so the
result may be larger than the files an actual execution reads.

Each `conditional_requirements` entry contains a `requirement` and a `when`
list. Conditions in one list are conjoined. A condition identifies a choice
`occurrence`, its readable `choice` label, a zero-based `branch`, and the number
of `branches`. Different branches of the same occurrence are alternatives.
Repeated entries for a requirement describe alternative ways to reach it;
an entry with an empty `when` makes that requirement unconditional. Occurrence
numbers are local to one report, not persistent artifact identifiers.

The flat `requirements` field remains the conservative union. It must not be
read as a conjunction of all candidate files. The conditional field is an
additive extension of version 1 that retains the choices behind that union.

Each candidate records a `path`, `kind`, and `exists` observation. Candidate
ordering is meaningful for search paths. Missing candidates are retained:
adding a file can change resolution or duplicate-file diagnostics. The query
does not create missing objects or manufacture replacement sources.

| Policy | Interpretation |
| --- | --- |
| `required` | These inputs are required by this operation or owning artifact. |
| `one-of` | An appropriate candidate must be available; the normal compiler's search, validity checks, and diagnostics still apply. |
| `source-or-object` | Source recompilation and object reuse are alternatives under `-u`, subject to the requirement's notes. |
| `optional` | Existing files can affect reuse, freshness, or metadata discovery; their absence alone is not a request to build them. |
| `search` | Preserve the stated search space; filenames are candidates, not an assertion that a module must have that filename. |
| `toolchain` | An executable, runtime, or installation tree used by the operation. Notes identify any unresolved external inputs. |

A directory candidate means the conservative directory **tree**, including
its file names, contents, and relevant metadata. Merely hashing the directory
inode is insufficient. Search spaces can therefore be deliberately broader
than individual package files. An external planner may narrow these scopes
only with additional evidence.

## Source and object substitution

An explicit `.bs` or `.bsv` command-line argument is a source input. An existing
`.bo` does not remove that requirement. Without `-u`, imported packages need
objects: source files do not substitute for missing imported objects.

With `-u`, the existing compiler searches source candidates and parses available
source before deciding which packages need compilation. It considers source,
object, include, and generated-output timestamps. A planner must not silently
remove source files or normalize away the timestamp relationships when those
relationships are what a test exercises.

Binary imports are followed through existing `.bo` metadata as well as source
imports. A poisoned, corrupt, or incompatible object is not permission for the
planner to choose a different compilation strategy. The query preserves the
normal compiler's validity constraints; it is not a recovery scheduler.

The report describes inputs to one compiler invocation. A `.bo` produced and
then used within that invocation is an intermediate, not necessarily an
external prerequisite. The source/object and optional-freshness requirements
must therefore not be flattened into mandatory file inputs.

## Artifact operations

Bluesim linking starts from `.ba` metadata, walks the elaborated hierarchy,
generates C++, compiles native objects, and normally creates an executable
launcher and shared library. `-systemc` instead generates and compiles the
SystemC model. Existing generated headers and native objects are reuse
candidates only under the compiler's freshness and option checks. Neither is
an unconditional replacement for the `.ba` hierarchy. A missing `.ba` is not
automatically rebuilt from `.bo`, `.bs`, or `.bsv` by an artifact-link command.

The existing unconditional `-KILLsimBlocksToC` stop requests C++ generation
without native compilation or linking. Its report uses mode `bluesim-cxx` and
does not require a native toolchain. `-KILLgenSystemC` also stops before native
compilation; its dump point is before SystemC wrapper files are written.
`-KILLbluesimcompile` reports native object generation without executable
linking (`bluesim-objects` or `systemc-objects`). Other or module-qualified
stage stops are explicitly incomplete. These are the compiler's existing
stop flags, not newly introduced code-generation entry points.

Verilog linking uses explicit Verilog sources and module/include search roots.
It can consult `.ba` hierarchy and foreign-function metadata, but the current
compiler does **not** regenerate missing Verilog from module `.ba` files: that
regeneration path is disabled. Consequently `.ba` cannot substitute for `.v`.
There is also no active direct `.ba`-to-Verilog emission command in this
checkout; storing a Verilog program in an elaboration file does not enable one.
Verilog-only linking remains possible without a matching `.ba` hierarchy.
Explicit `.ba` inputs are nevertheless opened and validated. VPI foreign
functions can require generated wrapper C sources and headers in addition to
their `.ba` records.

Explicit C/C++ sources, native objects, and libraries retain their command-line
roles. Naming an object does not ask bsc to find and compile a same-named source;
naming a source does not allow a planner to substitute an arbitrary object.

## Shared orchestration

`CompilerInvocation` constructs one `BuildPlan` for a source, Bluesim, or Verilog
invocation. The driver then chooses `executePlan` or `discoverDependencies`
from the command-line output mode. Source import resolution, elaboration
hierarchy loading, and artifact stage selection are shared. Binary compilation
and dependency inspection share the import traversal; compilation loads and
validates full objects in IO, while inspection reads their import metadata.

`SourceCompile` owns package compilation and `GenModule` owns elaboration,
scheduling, and module output. `SimLink` and `VerilogLink` own their link plans;
`NativeCompile` contains the C/C++ compilation and linking operations they share.
IO callers that need a plan's opaque result use `executeResultPlan`.
`getABIHierarchy` returns the hierarchy plan directly; IO callers such as
Bluetcl execute it explicitly at the call site.

The constructors of `BuildPlan` and `BuildResult` are private, and `BuildPlan`
has no `MonadIO` instance. IO is classified explicitly: `observe` reads inputs,
`perform` runs effects whose result is unused, and `produce` runs work whose
value is needed immediately. Discovery allows observations and skips effects.
If it reaches `produce`, it records an incomplete boundary and suspends that
branch. Independent branches remain inspectable. These API boundaries do not
prove arbitrary IO actions pure or read-only: callers must classify actions
correctly and describe their input requirements.

`requireFiles` probes candidate files only during discovery and returns an
empty candidate list during execution. Runtime lookup therefore supplies its
own actual result as branch zero; probed candidates add discovery alternatives
without controlling execution. Other report facts and `declareInputs` are also
discovery-only. `abort` throws during execution and records an incomplete
boundary while ending its branch during discovery; independent sibling scopes
can still be inspected.

`performResult` carries the result of an execution-only action in `BuildResult`.
Discovery skips the action and returns an unavailable handle, so subsequent
input descriptions can still run. `fmap` and applicative combination assemble
further execution values without exposing an unavailable payload; `withResult`
consumes a value only during execution. An unavailable handle alone does not
make discovery incomplete. `planResult` similarly composes a subplan whose
inputs are execution results. If those inputs are unavailable, its body is
skipped; its input contract must remain in the surrounding plan. Bluesim uses
this to retain expansion, reuse analysis, and code generation in their original
order, while discovery inspects the shared per-artifact reuse contract.
Using an execution value to guide further dependency discovery requires
`requireResult`, which records an incomplete boundary and stops the branch if
the value is unavailable. No placeholder graph or production value is invented.

Source and binary imports use `traverseState`. During execution, each successful
step returns the state and child jobs for the next steps; errors stop the traversal.
Source imports use breadth-first order to preserve diagnostic order, while
binary imports use depth-first order.
Discovery instead visits siblings independently from their common ancestor
state. Each alternative's descendants receive its actual returned state, and
retain its branch conditions. Siblings do not form a Cartesian product, and a
blocked or failing child does not hide the others. The accumulated execution
state is returned as a `BuildResult`, never as a fabricated union of alternatives.

Elaboration hierarchy loading retains its recursive `StateT`/`ExceptT`
structure. Its three loops over foreign modules, foreign functions, and native
or noinline function modules use `independentlyStateT`. Execution runs each loop sequentially,
passing accumulated state to the next action and stopping at the first error.
Discovery starts each sibling from the same incoming ancestor state and restores
the enclosing state afterward. It retains the siblings' dependency facts, not
an aggregate of their state mutations. This is suitable only when later
discovery decisions do not depend on accumulated sibling mutations; it is not a
general replacement for stateful `mapM_`. `runStatePlan` captures the recursive
traversal's result and final state in a `BuildResult`, whose contents are
unavailable during discovery. No final aggregate state is fabricated.

An explicit `choose` or `select` retains every represented alternative during
discovery, even when an observation supplies a concrete selector. `selectStateT`
lifts that behavior through state and error transformers without artifact-specific
policy. Ordinary Haskell conditionals are not intercepted: dependency-relevant
alternatives must be explicit plan nodes.

`withCachedRead` shares immutable read results by key within one interpretation
of its body. The interpreter owns the cache; traversal code does not allocate
or manage references. Caching decoded object metadata avoids repeated decoding
of shared imports, while each use still records its own dependency edge and
conditions. Returned error values can be cached just like successful values.

Hierarchy loading similarly caches repeated elaboration lookups with the same
ordered search path and module name. It still records the conditions of each
use. Traversal bookkeeping such as whether an ancestor has been visited is not
reported as an alternative. Discovery may still walk a shared subgraph once
per import path, and source parsing is repeated on those paths: a source parse
also describes includes and preprocessing effects, so caching its AST alone
would not replace that plan. Discovery cost is therefore not bounded by the
number of unique files alone.

The contract remains relative to observed source and metadata contents. It
does not enumerate every possible future source file, evaluate arbitrary
Bluespec code to find computed filenames, or interpret arbitrary options
forwarded to external tools.

`declareInputs` attaches an input-description plan to an opaque production
step. Discovery inspects that contract; execution leaves it unevaluated and
loads the inputs at the production step's normal point. The contract returns
no value to orchestration. This matters for binary metadata: the compiler's
decoder also interns identifiers, so speculative decoding during normal
execution would change identifier allocation and output ordering. The input
contract and actual import loading share their traversal and resolution rules.

Execution retains the existing syntax-tree forcing performed by `dump` after
parsing. Discovery skips that execution step and does not add a blanket `rnf`
of its own. Forcing an entire syntax tree can change when identifiers are
interned and therefore alter normal compiler ordering. Where a read or decoder
must expose errors before returning, force only the required data at that IO
boundary. Evaluation forcing is not a way to schedule hidden effects; effect
order belongs in the explicit plan.

## Limits and use by a build system

In particular, source parsing can fail before all imports are known;
elaboration can perform file I/O with computed filenames; external C
preprocessing, C/C++ compilation, and simulators have their own include paths,
implicit libraries, environment, and toolchain dependencies. Those boundaries
must be covered by another mechanism or by an explicitly broader fixture or
toolchain snapshot. Known inputs are retained when discovery is incomplete.

A source invocation that selects a module for elaboration is marked
incomplete: parsing alone cannot enumerate files read by elaboration or all
dependencies of later generation stages.

An exit status of zero means a report was written. Always inspect `complete`
and `incomplete` before treating it as a complete action input declaration.
A missing required file can be fully described even though the eventual
operation will fail; conversely, unknown transitive dependencies make a report
incomplete. Do not interpret an incomplete report as an empty input set.

The caller remains responsible for the compiler executable, command line,
configuration environment, working-directory interpretation, and any required
timestamp/existence state. Dependency queries must be refreshed when these or
the relevant source and search-space inputs change. The JSON contains no file
digests and does not itself implement invalidation or choose producer actions.

## Validation

Build the compiler and interpreter regression executable from the repository
root, including the auxiliary tools used to check generated artifacts:

```sh
BSC_BUILD_TESTS=1 BSC_BUILD_EXTRA=1 make -j32 GHCJOBS=16 install-src
```

The dependency suite runs the interpreter tests. End-to-end queries run with
the existing Mips source, Bluesim, and Verilog tests; they verify expected
inputs, outputs, completeness, and preservation of fixture contents and
timestamps. Interpreter tests check the separation of alternative branches.

Run the complete suite, including long and SystemC tests, from the root:

```sh
PATH="$PWD/inst/bin:$PATH" CONFIG_SHELL=/bin/sh DO_INTERNAL_CHECKS=1 \
  make -j128 -C testsuite fullparallel \
  TEST_SYSTEMC_INC=/usr/include \
  TEST_SYSTEMC_LIB=/usr/lib/x86_64-linux-gnu \
  TEST_SYSTEMC_CXXFLAGS=-std=c++17
```
