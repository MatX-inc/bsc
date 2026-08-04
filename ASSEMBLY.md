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

2. `matx/verilator/6-link-tools` (#46) — conflicts:
   - `src/comp/Flags.hs`: union of flag fields at record tail; order
     `semanticPortsComment, checkOnly, stableVerilog` (sv/4-feature had
     left checkOnly's comment in place without the field; verilator's
     checkOnly slots there).
   - `src/comp/FlagsDecode.hs`: defaults + showFlagsRaw union, same order.
   - `src/comp/GenABin.hs` (Bin Flags): kept sv-side 10-chunk
     a_000..a_137 layout; added `a_checkOnly` between a_136
     (semanticPortsComment) and a_137 (stableVerilog). IMPORTANT: the
     textual auto-merge placed verilator's `a_dumpFormats` after a_021,
     but in the merged record (which gained sv's blockCodegen at
     position 6) that position is `doICheck`; moved `a_dumpFormats`
     after a_022 so it matches dumpFormats' constructor position (24).
     Header tag kept `bsc-ba-20260803-5` until final unification.
   - `src/comp/AVerilogUtil.hs`: VConvtOpts union (`vco_stable` +
     `vco_ffmap`/`vco_def_widths`); adopted verilator's
     `systemVerilogTasks` -> `systemVerilogOutput` rename everywhere.
   - `src/comp/Depend.hs`: union (sv's elabOnly conditional + verilator's
     DPI-wrapper tracking comment).
   - `testsuite/bsc.options/bsc.print-flags-raw.out.expected`: union in
     showFlagsRaw order (regenerated from the built compiler at the end).
