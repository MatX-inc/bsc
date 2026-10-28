Native capture and replay controls
=================================

`ECO.Replay` captures a scheduled native package and the context needed to run
the existing `.ba`-to-Verilog backend again. Replay restores the intern table
before decoding compiler values, then checks that the emitted bytes match.
It uses scheduled-package data and does not require a retained `VProgram` or
separately captured Verilog information.

ECO invocation controls live in `Flags.ecoOptions`; their parsing stays in
`ECO.Options`. Parsing returns a validated compile, replay or adaptation mode;
required paths and conflicting arguments are checked before opening a capture
or restoring interning. Native flag serialization excludes these controls, so loading a
capture cannot reactivate its original capture or output paths. The driver
supplies the controls for the current replay or adaptation invocation explicitly.

`-eco-capture DIR` requires `-elab` and records the actual native `.ba`, complete
intern table, effective backend flags, headers, dependency resolution, filter
identities, and output bytes. It preserves the ordinary native `.ba` contents;
the existing optional `VProgram` field is retained only when the ordinary
compiler flags request it. Backend context passes directly to capture after
native serialization, with no global handoff state.

With `-u`, capture regenerates source packages that produce modules, including
imported synthesized modules, even when their ordinary outputs are up to date.
Normal dependency discovery and rebuilding still apply; installed libraries are
not forced to rebuild. Each capture invocation can therefore populate a fresh
capture directory without first cleaning the ordinary build outputs.

`-eco-replay MODULE.replay -eco-output DIR` runs in a fresh compiler process.
It rejects producer, version, working-directory, dependency, filter or payload
changes and checks the replay output bytes. Replay output paths must not replace
bound capture artifacts, including through symlinks and hard links.
`-eco-adapt` first runs the original
producer's exact control, then requires the current producer to emit identical
ordinary bytes before creating a new capture. It does not waive identity checks.
Its output directory must be new or empty, so controls and the adapted capture
cannot overwrite the source capture or reuse outputs from an earlier run.
File identity checks use `sha256sum`, or `shasum -a 256` on platforms such as
macOS; one of these utilities must be on `PATH`.

Ordinary compilation does not create these artifacts. There is no name matcher,
RTL parser or equivalence assumption in this native interface.

Run these native DejaGNU tests with `make -C testsuite/bsc.eco/replay check`.
The hardware fixtures use Bluespec Haskell (`.bs`). `ReplayForeign.bsv` is a
minimal BDPI import shim because BH has no BDPI import syntax; it supplies the
native `.ba` dependency needed by the mutation and lookup-identity checks.
An imported synthesized BH module cannot substitute for this dependency:
backend replay already retains its interface and does not read its `.ba`.
The `native.exp`, `dependency.exp` and `incremental.exp` cases invoke the compiler
directly and compare exact output bytes. The incremental cases cover cached
builds, repeated capture and imported module/dependency rebuilding. The native
test also checks that default capture replays without retained `VProgram` or
Verilog info artifacts; `NativePayload.hs` inspects the native retention field.
Set `PRODUCTION_BSC` to include ordinary output
comparisons against a second compiler. The Haskell inspectors run through the
shared `bluehs` test hook; install `bsc` and
`bluehs` from the same build so their compiler libraries match. The standard
`TEST_BSC` and `TEST_BLUEHS` make variables select their executables. Only the
Haskell cases are marked untested when `bluehs` is unavailable.

Each category keeps its logs and artifacts in a `work_*` directory, removed by
normal test cleanup. No source-tree compilation, Python or external RTL tool
is needed.
