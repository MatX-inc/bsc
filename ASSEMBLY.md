# integration-carve assembly

Recomposition of the carved PR lines into one MatX build, on top of
`matx/upstream-main` (941eecfe). Ravi's ruling: **stable-verilog stays
DEFAULT OFF** — the sv line is merged only through `sv/4-feature` (#66),
not `stable-verilog-7`/#61 (no flip, no check_verilog_regen infra).

Setup: worktree from matx/upstream-main, plus `src/vendor/yices/v2.6/yices2`
copied from wt-sv7 (untracked vendor payload needed by the build).

After EVERY merge: `make -j16 GHCJOBS=8 install-src` must exit 0 before
the next merge.

## Merge order and conflict resolutions

1. `matx/sv/4-feature` (#66) — clean merge, no conflicts.
