ECO test groups
===============

Every group uses the ordinary DejaGNU harness; `make -C testsuite/bsc.eco
check` runs them. `TEST_RELEASE` selects the compiler installation and
`TEST_BLUEHS` the bluehs runtime; the Haskell programs run through the shared
`bluehs` hook in `testsuite/config/unix.exp` and are `UNTESTED` without it.
Install `bsc` and `bluehs` from the same build. Each group keeps its
artifacts in a `work_*` directory removed by the normal cleanup.

Environment: `PRODUCTION_BSC` (a second compiler for ordinary-output
comparisons; untested when unset), `CORE_REPLAY_BSC` (an executable of a
different native version for the adaptation version guard; untested when
unset), `ECO_LIBDIR` (an extra `-i` library directory for the dependency
group). No Python, RTL parser or external RTL tool is needed.

Native groups
-------------

| Group (PR) | Establishes |
|---|---|
| `intern/intern.exp` (#241) | Each script runs in a fresh process. `Capture.hs`: the initial table is empty and a snapshot records every spelling with its id and the counter. `Restore.hs`: a restore reproduces relative id order and spellings, allocation continues at the saved counter, a second restore is rejected. `Reentrant.hs`/`RestoreReentrant.hs`: a nested lookup of one spelling takes one id, and that state survives restore. `MethodName.hs`: compiler-generated method names keep consistent ids. `DuplicateRestore.hs`, `Invalid.hs`, `Empty.hs`, `Late.hs`: duplicate spellings, a negative counter, missing, repeated or out-of-order ids, and a restore after a live allocation are rejected without changing the table, while an empty snapshot round-trips and allocation then starts at zero; `InternTest.hs` covers spellings that collide under the interner's hash. |
| `replay/options.exp` (#242) | `-eco-replay` and `-eco-adapt` validate their required `-eco-output` before opening a capture; `Invocation.hs` checks every mode and conflict of `parseECOOptions`, including an unknown `-eco-` option. |
| `replay/sidecar.exp` (#242) | A fresh process restores the intern table, outputs and headers of a capture; captures record flags, paths, bytes and escaped hash filenames; a bad schema, bad producer, changed working directory, modified payload or dependency bytes are rejected. |
| `replay/dependency.exp` (#242) | Multi-module replay with a Verilog filter, a BDPI foreign dependency and library lookups: exact replay bytes for each module and for a symlinked library alias, rejection of modified filter or foreign bytes and of a changed original lookup identity, a preserved foreign dependency symlink, and ordinary-output parity with `PRODUCTION_BSC`. |
| `replay/incremental.exp` (#242) | Capture regenerates every module-producing package under `-u` even when ordinary outputs are current, for a first build, a cached build and a recapture; an imported helper without a synthesised module is rebuilt; each captured module replays exactly and strictly. |
| `replay/native.exp` (#242, #253) | Native capture, exact and repeated replay, production parity, companion export and native `VProgram` retention; ECO options come from `argv` only (`BSC_OPTIONS` can neither redirect a replay nor request a capture); capture refuses an existing stem and leaves the source intact; a replay or adaptation whose output would alias, hard-link into, or lie inside the capture creates nothing there; a modified native payload is rejected; adaptation reproduces the ordinary bytes, writes its receipt and companions beside the new capture, rejects a tampered captured output before any write and a forged output digest at the byte control, and leaves no divergent RTL, capture or receipt on failure. |
| `verilog-info/verilog_info.exp` (#253) | Captures retain the native AST and typed child metadata, render exactly and refuse relocation; a comment with an embedded newline, an identifier containing `;` and a wire used before its declaration are refused before any byte is written; `Gates.hs` captures every fixture of this suite and each passes both gates and emits its captured bytes; replay refuses to overwrite bound companions; with `CORE_REPLAY_BSC`, adaptation rejects a different native version before running any control or capture and preserves every source artifact. |
