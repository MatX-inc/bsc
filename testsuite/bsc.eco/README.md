ECO test groups
===============

Every group uses the ordinary DejaGNU harness; `make -C testsuite/bsc.eco
check` runs them. `TEST_RELEASE` selects the compiler installation and
`TEST_BLUEHS` the bluehs runtime; the Haskell programs run through the shared
`bluehs` hook in `testsuite/config/unix.exp` and are `UNTESTED` without it.
Install `bsc` and `bluehs` from the same build. Each group keeps its
artifacts in a `work_*` directory removed by the normal cleanup.

Environment: no variable beyond `TEST_RELEASE` and `TEST_BLUEHS` is read.
No Python, RTL parser or external RTL tool is needed.

Native groups
-------------

| Group (PR) | Establishes |
|---|---|
| `intern/intern.exp` (#241) | Each script runs in a fresh process. `Capture.hs`: the initial table is empty and a snapshot records every spelling with its id and the counter. `Restore.hs`: a restore reproduces relative id order and spellings, allocation continues at the saved counter, a second restore is rejected. `Reentrant.hs`/`RestoreReentrant.hs`: a nested lookup of one spelling takes one id, and that state survives restore. `MethodName.hs`: compiler-generated method names keep consistent ids. `DuplicateRestore.hs`, `Invalid.hs`, `Empty.hs`, `Late.hs`: duplicate spellings, a negative counter, missing, repeated or out-of-order ids, and a restore after a live allocation are rejected without changing the table, while an empty snapshot round-trips and allocation then starts at zero; `InternTest.hs` covers spellings that collide under the interner's hash. |
