Compiler observations
=====================

Optional capture records the existing proof pass's Proven, Disproven and
Inconclusive outcomes, pre-check/pre-drop-undetermined typed expressions and
reachable expansion-heap alias identities. The versioned evidence companion is
bound to the exact capture, native package, producer and payload bytes. Each
expansion returns its own detached alias observation; no global slot or module-name
lookup connects expansion to emission. Names are rendered only after ordinary
output and the native intern snapshot have completed. Captures
without it remain valid and explicitly lack this evidence.

Successful compilation is not a proof that every obligation passed. Scheduling
annotations, source representations and enable/ready relations are classified
as observations or user declarations, never silently promoted to assumptions.
An external consumer must bind a fact to the selected final RTL and prove it
independently before using it. The compiler does not perform matching or consume
these facts to constrain replay.

The tests preserve ordinary proof-attempt ordering and diagnostics, exercise all
three outcome classes, and check actual proved/unproved native captures, byte
compatibility, fresh replay and changed-evidence rejection. Per-call controls
also cover repeated source names, discarded and failed compilations, eager heap
detachment, and delayed name rendering without new intern entries.

Run `make -C testsuite/bsc.eco/observations check SYSTEMCTEST=0` after installing
the compiler and bluehs. `TEST_RELEASE` selects another compiler installation
and `TEST_BLUEHS` selects another runtime. The Haskell inspectors run directly
through bluehs against its compiled compiler library from the same build.
DejaGNU reports the proved and unproved controls separately and keeps logs under
`work_observations`. Without bluehs only the Haskell checks are reported as
untested; native compilation, replay and exact byte comparisons still run.
