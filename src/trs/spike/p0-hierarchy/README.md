# P0a: hierarchical protocol investigation

This is the first execution checkpoint of [P0-HANDOFF.md](../../P0-HANDOFF.md),
not the production interpreter or a completed P0 proof.  The analysis and
small protocol experiments were requested with `gpt-6-astra`, reasoning
effort `ultra`.  The governing dynamic target is parity with the pinned BSC
Verilog backend, not arbitrary dynamic scheduling.

Start with [PROTOCOL.md](PROTOCOL.md), [inventory.json](inventory.json), and
[RESULTS.md](RESULTS.md).  [ENVIRONMENT.md](ENVIRONMENT.md) records which
checks could actually run.  No production compiler/runtime files are changed.

## Reproduce this checkpoint

Run from the repository root with Python 3.12 or later and Git.  Ensure the
reference commits are available; fetch them explicitly if the checkout is
shallow (the runner never fetches or installs anything):

```sh
git fetch --no-tags origin a9462e0e28102f07a89b3f3c478b33c806d62a3e
git fetch --no-tags https://github.com/nanavati/bsc.git d4a72767c63e6dbdf569cac4c61aeb8afc597c04
python3 -m unittest discover -s src/trs/spike/p0-hierarchy/tests -v
sh src/trs/spike/p0-hierarchy/run.sh source
sh src/trs/spike/p0-hierarchy/run.sh model
sh src/trs/spike/p0-hierarchy/run.sh environment
sh src/trs/spike/p0-hierarchy/run.sh all
```

`P0_PYTHON` can select a different Python executable.  Every subprocess has a
20-second bound.  The entry point emits JSON to stdout; recorded checkpoint
reports are under `results/`.

| Mode | What a zero exit means |
| --- | --- |
| `source` | The listed audit-source blobs and tracked compiler/runtime subtrees match the compiler pin, and the pinned BVI source is locally available. It does not validate semantic claims. |
| `model` | The narrow hand-authored protocol experiments met their own assertions. It does not establish BSC/RTL parity or complete hierarchical execution. |
| `environment` | Required tool names are available. Their versions and compatibility still require verification before oracle runs. |
| `all` (default) | Never exits zero at this checkpoint. P0a review and actual compiler/BVI/composition/P0b gates remain open, even if every included check succeeds. |

Exit 2 means blocked/inconclusive; exit 3 means a check failed, timed out, or
could not run correctly.  Unit tests exercise this reporting behavior,
including a successful model result that must not become a P0 pass.

## Scope of the next review

Review the candidate protocol, the Verilog-supported dynamic cases absent
from `ADynSched`, and the actual mechanism for a separately interpreted BSV
child below opaque BVI.  A BSV wrapper around an imported RTL leaf is not
that last witness.  Require local contract derivation and a composition/
progress argument before treating small-model success as an architecture
result.  Substantial P0b implementation remains behind the handoff's
design-owner review gate.
