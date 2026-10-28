Checked native helper retention
===============================

Capture emits a versioned neutral typed graph at the pre-aOpt checkpoint and
a final typed graph. `-eco-replay CAPTURE -eco-backend-plan PLAN` first runs
an exact ordinary replay control. The plan binds the capture SHA, complete
checkpoint SHA, module, scalar type and exact current RHS key for every request.
It rejects protected, unknown, duplicate, undetermined, opaque or cyclic cones.
The authenticated preoptimization checkpoint is shared with deferred observation;
its existing file is revalidated and reused, or copied byte for byte to a separate
graph directory. Changed checkpoints and output aliases are rejected.
Only the existing `IdP_keep` annotation changes. Ports and parameters stay fixed,
and every requested helper must survive the final independent scalar checks.

These are proposals, not an equivalence certificate: an external client must
independently check any changed output. There is no old-source substitution,
matching algorithm, automatic keep-selection policy or RTL parser in the native
compiler. `ECO.Validate` and `ECO.Namespace` are shared integrity/safety checks.

Replay also validates any optional evidence companion. It checks its capture,
native payload and original producer identities; adaptation
preserves that evidence. Captures without a companion remain valid and do not
acquire new observations during adaptation.

Native checkpoint output contains the preoptimization graph, final graph and
port contract. JSON summaries are optional external diagnostics.

The fixture selects one typed Boolean helper lost when its fanout falls to one.
It checks actual keep survival, byte-identical empty requests, and stale or
invalid request rejection using fresh native processes.

Directed Haskell checks also reject undetermined, opaque and cyclic keep cones,
invalid graph records, latch/state eligibility, and changes to the module ports.
They include positive pure-helper and exhaustive combinational-case controls.
Namespace checks cover explicit Verilog constructors and identifier metadata;
the observer uses typed folds without generic traversal or repeated body scans.

Backend-plan replay requires a new or empty output directory before running its
ordinary control. Separate `-eco-graph` directories are supported for replay
and adaptation; checkpoint and AST output paths must not alias source capture
artifacts, the checked plan, or ordinary RTL output.

The Haskell checks run through `bluehs` against its compiled compiler library,
with each invocation in a fresh process. The runtime and native compiler must
come from the same build so their capture versions match. Run
`make -C testsuite/bsc.eco/backend check SYSTEMCTEST=0` after installing the
compiler and bluehs. `TEST_RELEASE` selects another compiler installation and
`TEST_BLUEHS` selects another runtime. DejaGNU reports each replay, byte comparison
and rejection separately and keeps its logs under `work_backend`. Without
bluehs the Haskell checks and dependent plan tests are reported as untested;
native capture and graph-output controls still run.

A checked keep plan is an opaque value owned by one backend invocation.
`applyBackendPlan` returns it alongside the annotated package; the deferred
observation carries it to `writeBackendMetadata`, which rechecks the plan file,
module and final helper survival. No process-global plan state is used.
`BackendPlanContext.hs` interleaves two invocations with the same module name,
checks completion in either order, and exercises failed calls, missing or mixed
contexts, changed inputs and lost helpers. These calls are sequential; this does
not change the existing interner’s single-threaded contract.
