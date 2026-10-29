Captured Verilog information
============================

`ECO.VerilogInfo` exports the compiler's existing `VProgram` with its renderer
header, final typed signals and state outputs, child-instance metadata, and bindings to a
native capture and its reference output. This is final post-lowering and
optimization information; scheduled-package context is loaded separately from
the native `.ba`. It introduces no new RTL representation or RTL parser.

The [native capture/replay interface](../replay/README.md) works independently
of this information. This layer additionally retains the existing optional
`VProgram` field in new captured `.ba` files and writes the info companions
after native capture or verified replay. The `.ba` codec and layout are unchanged.
Backend context passes explicitly to capture; info export is deferred until
the native capture is complete.

`readVerilogInfo` inspects multiple captures using one live interning table,
without restoring a captured table into the inspecting process. Checked
rendering uses the existing Verilog prettyprinter and captured header/filter
context. Candidate emission fixes module ports and parameters. Child metadata
that cannot be bound to the retained scheduled package is reported as unknown.

The in-memory record is `VerilogInfo`. Existing `.eco-ast`, `.eco-ast.bin` and
`.eco-ast.output.v` artifact names, and the serialized `ASTEnvelope` and
`ASTPayload` formats, retain their original spellings for compatibility.
Loading validates the captured flags and package without exposing them as
convenience fields in `VerilogInfo`. Typed signals and state outputs remain
available in the record, and the full serialized payload is unchanged.

Run `make -C testsuite/bsc.eco/verilog-info check`. The tests reuse the BH fixture
in `../replay/ReplaySmoke.bs` and check retained native AST identity, typed
metadata, exact rendering, multiple captures, and output/relocation guards.
They use the shared `bluehs` hook; install `bsc` and `bluehs` from the same build,
or select them with `TEST_BSC` and `TEST_BLUEHS`. The suite is marked untested
when `bluehs` is unavailable. Logs and artifacts stay in `work_verilog_info`
and are removed by normal test cleanup.

Adaptation requires the same full native compiler version, even when the
producer executable differs. It does not migrate captures between compiler
revisions. Set `CORE_REPLAY_BSC` to an archived executable from the
capture/replay layer with a different version (the first line of `bsc -v`) to
test this guard. The test checks that adaptation rejects its capture before
running backend controls or creating a new capture, preserving every source
artifact. For example, run
`CORE_REPLAY_BSC=/path/to/core/bsc make -C testsuite/bsc.eco/verilog-info check`.
Only this additional check is marked untested when the variable is unset.
