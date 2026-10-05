# Buck2 execution slice

The generated standalone cell uses `rules/bluespec` for an installation and
host-identity provider, and `rules/bsctest` for the testsuite execution action.
The public toolchain rule has no test expectations or internal-check policy.
No prelude is required; `buckconfig` is the generated cell's configuration.

`bsc_test` declares the plan, runner executable, suite directory, installation
directory, and host-identity manifest as action inputs. Its command is:

```
tools/bsc-test-plan execute PLAN IDENTIFIER --installation INSTALLATION --suite SUITE --output OUTPUT
```

The runner owns the output directory and reports ordinary test verdicts there.
A failing test verdict still produces a successful build action so its evidence
can be reused. Infrastructure errors fail the action. A separate result check
must interpret the verdicts; `buck2 build` success alone is not a passing suite.

Actions run locally with remote cache upload disabled. The host-identity
manifest must be regenerated when the host interpreter, tools, or shared
libraries change. Directory artifacts conservatively track the whole copied
suite and installation; this initial slice does not narrow dependencies per
test or claim remote hermeticity.

## Run the supported subset

Build the planner as described in `testsuite/planner/README.md`, and use a
complete compiler installation with `dumpbo`. From the repository root:

```sh
python3 testsuite/buck2/bootstrap.py
PLANNER=$(cabal list-bin --project-dir=testsuite/planner bsc-test-plan)
BSC_BUCK2="$PWD/testsuite/.stage1-validation/buck2-tool/buck2"
BSC_CELL="$PWD/testsuite/.buck2/compile"
mkdir -p testsuite/.buck2
"$PLANNER" plan --config compile-internal --suite-root testsuite \
  --internal-checks 1 testsuite > testsuite/.buck2/plan.json
"$PLANNER" emit-buck2 testsuite/.buck2/plan.json --suite-root testsuite \
  --installation inst --output "$BSC_CELL"
(cd "$BSC_CELL" && "$BSC_BUCK2" build -j16 @targets.txt --build-report build.json)
python3 testsuite/buck2/check_results.py "$BSC_CELL" "$BSC_CELL/build.json"
```

The cell directory must be new. Its inputs are copied, not live links back to
the checkout. Re-emit to test source, planner, installation, or host changes.
Only Git-tracked suite inputs enter the snapshot; generated `.bo` files and
legacy logs cannot accidentally satisfy an isolated test. Scripts containing
executable tests are checked against the saved plan before emission. Other
scripts' planning issues remain recorded without blocking the supported tests.

Each action stages its script directory subtree and internal symlink targets
at the same suite-relative locations. This policy has been audited for the
current 344 planned tests. New tests that need other source directories require
a wider explicit input policy; no Bluespec dependency parser is used here.
Compilations without `-u` are reported as execution gaps until shared-state
scenarios are implemented. The ten-minute per-process timeout, unexpected
signals, and launch failures are infrastructure errors, including in negative
compilation tests. Internal object loading still runs after a failed compile.

`check_results.py` requires every emitted target and every expected result
role. It rejects inconsistent configuration, identities, and process outcomes,
and returns nonzero for ordinary FAIL or infrastructure errors. Planning and
execution gaps are counted separately; a passing subset is not full parity.

## Deliberate-failure regression

After building the planner and bootstrapping Buck2, run:

```sh
python3 testsuite/buck2/check_mutation.py \
  --planner "$PLANNER" --buck2 "$BSC_BUCK2" \
  --output testsuite/.buck2/mutation-check
```

The output directory must be new. This selects only `b1213` and runs both real
DejaGNU and Buck2 through PASS, deliberately broken source, and restored source
phases. It requires matching compilation and object-load results, a warm build
with no executed action, and a rerun after the source mutation. DejaGNU gets
clean inputs for each phase; Buck2 keeps the same cell and daemon to exercise
invalidation. A successful regression intentionally captures two FAIL results
from each runner in the broken phase, then two PASS results after restoration.
The script returns zero only when all these assertions hold.

Original source bytes and phase logs are retained in the output directory.
The copied source is restored in a `finally` block and the checkout is never
edited. No broken source or changed golden needs to be checked in.

## Pinned executable

Run `python3 testsuite/buck2/bootstrap.py` from the repository root to obtain
the executable under ignored `testsuite/.stage1-validation/buck2-tool`.
The script downloads the official Linux x86_64 musl build for
[`2026-10-01`](https://github.com/facebook/buck2/releases/tag/2026-10-01), verifies
the release asset's SHA-256 from `buck2-pin.json`, and decompresses it with
`zstd`. The dated release and digest prevent silently following the moving
`latest` release. Nothing is installed globally.

The pin was checked against the official GitHub release API. Buck2's
[source attributes](https://buck2.build/docs/api/build/attrs/#source) support
directory inputs, and its
[action API](https://buck2.build/docs/api/build/AnalysisActions/#analysisactionsrun)
supplies the local-only and cache-upload controls used here.

## Rule validation

Run `python3 testsuite/buck2/check_rules.py -v` after bootstrapping. This uses the
actual pinned Buck2 and a stub runner, without compiling BSC. It verifies that
an unchanged second build executes no action, even with a stored `FAIL`
verdict; changes to every declared input rerun the action; and infrastructure
failure still fails the build. It reads Buck2's build report and `log what-ran`
output in addition to checking result contents.

Warm reuse is verified within one running daemon. With this initial local
configuration an unchanged action runs again after daemon restart. Persistent
or remote action-cache reuse has not been configured or demonstrated.

Buck2 creates its normal per-project daemon state in `~/.buck/buckd` and uses
local sockets. A restricted execution environment may need to permit those
operations. The validation test stops its own daemon when finished.
