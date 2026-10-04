# Module-stage validation — 2026-10-04

The complete 906-script selection finished. The final per-directory aggregate,
including the focused `expandPorts` harness rerun described below, is **34,227
PASS, 57 baseline-reproduced FAIL, and 132 XFAIL**, with zero ERROR, UNRESOLVED,
unclassified failures, or missing scripts. Both compiler regressions found
during validation were fixed and verified with their original fixtures.

Compiler checkpoint:
[`8dd96238b16fe5179875115d4b2598690d5f97d0`](https://github.com/MatX-inc/bsc/commit/8dd96238b16fe5179875115d4b2598690d5f97d0).
Comparison baseline:
[`1769ff38555b360257cc68973fae7cb0e0b3047b`](https://github.com/MatX-inc/bsc/commit/1769ff38555b360257cc68973fae7cb0e0b3047b).
The subsequent one-line test-helper repair did not change compiler binaries
or expected outputs.

## Environment and command

Validation used Debian bookworm, GHC 9.14.1, Cabal 3.16.1.0, Tcl/Expect/DejaGNU,
Icarus Verilog 11, SystemC, a C/C++ toolchain, and `fst2vcd`. The compiler,
utilities, runtime, and libraries were installed in `inst`. All 11 compiler
utility hashes matched the run manifest throughout validation.

The optional `InstSynth`, `expandPorts`, `portUtil`, and `types` Tcl scripts
were installed unchanged from `util/bluetcl-scripts` into
`inst/lib/tcllib/bluespec` before their selected fixtures ran. The package
index registered `InstSynth`, `portUtil`, `types`, and `Types` version 1.0.
These utilities were exercised, not skipped. The FST fixture passed 72 checks,
including actual FST-to-VCD conversions.

From the repository root, with the dependencies above on `PATH`, the portable
equivalent of the executed full-suite command is:

```sh
export PATH="$PWD/inst/bin:$PATH"
export BLUESPECDIR="$PWD/inst/lib"
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
make -C testsuite -j4 TEST_RELEASE="$PWD/inst" \
  DO_INTERNAL_CHECKS=1 RUN_TESTCASES_IN_ORDER_OF_TIME=1 fullparallel
```

The generated selection contained every active `.exp` script exactly once,
including all five long-test fixtures, which passed 115 checks in total.
Cabal's complete build, the strict component manifest check, phase-config
tests, module artifact/replay tests, and materialization-patch tests also
passed. Focused reuse and pragma replay validation passed 683 checks across
10 scripts.

## Raw sweep and focused harness repair

The raw full sweep completed all 906 scripts with 34,212 PASS, 33 FAIL,
132 XFAIL, 9 ERROR, and 1 UNRESOLVED; its launcher exited 2. Its complete logs,
summaries, manifest, and hashes were archived before any rerun.

The errors came from the existing `bluetcl_opt_pass` helper calling the
nonexistent `run_bluetcl` after already executing the requested command.
The one-line repair in [bluetcl.tcl](../testsuite/config/bluetcl.tcl) checks
the returned status instead. Only `expandPorts` was cleaned and rerun:

```sh
make -C testsuite/bsc.bluetcl/packages/expandPorts localclean
make -C testsuite -f parallel.mk CONFDIR="$PWD/testsuite" -j1 -k tool=bsc \
  TEST_RELEASE="$PWD/inst" DO_INTERNAL_CHECKS=1 \
  ./bsc.bluetcl/packages/expandPorts/expandPorts.exp
```

That rerun completed with 15 PASS and 24 baseline-reproduced FAIL, no abort,
and exit 2. All 13 cases ran; their 26 generated outputs matched the baseline
compiler, and all goldens remained unchanged. Only this fixture's `.sum` and
`.log` differed from the archived sweep. The selection and all compiler
binary hashes stayed unchanged. Replacing the aborted result with this
completed rerun gives the final aggregate above.

## Fixed compiler regressions

| Original fixture | Verification |
| --- | --- |
| [evaluator/opt](../testsuite/bsc.evaluator/opt/opt.exp) | 136 PASS. `ConcatOpt3` preserves the compile-time `-unspecified-to A/0` result when linked with the original default Bluesim options. Original source and both goldens match the pinned baseline. |
| [perf-creg-blowup](../testsuite/bsc.bugs/perf-creg-blowup/perf-creg-blowup.exp) | 18 PASS. Original compiler stack budgets are retained; `dumpba` loads the large generated pair at its unchanged default 10 MiB stack. All three original BSV inputs match the pinned baseline. |

The fix stores concrete common-lowering results in `.bsched`, so reading a
pair does not repeat undefined-value selection or noinline lowering. Existing
output reuse and pragma replay coverage are restored. Explicit generation
and generation of missing outputs use the current invocation's backend
options; no invocation Flags are persisted and no new backend cache is added.
See [the implementation plan](engine-first-plan.md) for the artifact split.

## Remaining baseline failures

Every remaining failure was reproduced against the pinned baseline. The
57 failures comprise 23 existing compiler/diagnostic/dump expectations,
10 `InstSynth` helper failures/cascades, and 24 `expandPorts` golden mismatches.

| Test group under `testsuite/` | FAIL |
| --- | ---: |
| `bsc.bluetcl/packages/expandPorts` | 24 |
| `bsc.bluetcl/packages/InstSynth` | 10 |
| `bsc.scheduler/urgency` | 5 |
| `bsc.evaluator/undefined` | 4 |
| `bsc.misc/lambda_calculus` | 4 |
| `bsc.misc/sal` | 4 |
| `bsc.bluetcl/reload` | 1 |
| `bsc.bugs/bluespec_inc/b378` | 1 |
| `bsc.doc` | 1 |
| `bsc.evaluator/pack-unpack` | 1 |
| `bsc.scheduler/many_rules` | 1 |
| `bsc.typechecker/numeric` | 1 |

The validation evidence bundle retains `validation-summary.json/.txt`,
`full-suite-run.json`, the raw `full-suite-third-run-raw` archive, exact
baseline comparisons, original-fixture proofs, and the focused rerun.
Those records contain detailed commands and hashes; this document records
the portable result and its limits.
