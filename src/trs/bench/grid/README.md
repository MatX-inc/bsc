# Grid benchmark: replicated-design scaling

An N x N array of identical tiles, generated at any size, built for
both simulators from the same `.bir`, and diffed for byte parity.  It
answers one question the parity tests cannot: **how do trs's costs
grow with instance count?**  (HANDOFF task #20 -- pools/batching/lanes
-- is demonstrated against exactly this shape.)

## What it measures and why

Real large designs are mostly *replication*: one small module type
instantiated thousands of times, plus a top-level "spine" of wiring
that grows with the instance count.  `gen_grid.py N` builds that shape
deliberately:

- `mkTile` is a single parameterless `(* synthesize *)` module -- a
  2-Reg + ConfigReg + FIFO always-fire pipeline stage doing real
  xor/add/rotate arithmetic on `Bit#(32)` every cycle.  N*N instances
  share the one module type.
- The top module is the spine: N*N instantiations plus N*N link rules
  forwarding each tile's output FIFO to its east/south neighbor
  (row-major ring; boundary tiles wrap).  Each link XORs in a distinct
  constant so tile histories diverge by position.  The spine is where
  O(instances) or worse shows up: scheduling, the central dispatch
  loop, per-instance link/codegen work.
- Every rule is provably always-fire (`fire_when_enabled,
  no_implicit_conditions`, unguarded FIFO methods) except the one
  harvest rule that `$display`s an XOR checksum of the diagonal tiles'
  accumulators at the target cycle and `$finish`es -- so the design
  exercises the always-fire short-circuit path (task #23) at scale,
  and its single output line is deterministic for the trs-vs-
  reference diff.

The CSV separates *where* time goes as N grows:

| column | meaning |
| --- | --- |
| `N`, `tiles` | grid edge, N*N instance count |
| `bsc_frontend_s` | `bsc -sim -bir -u -g sysGrid<N>` (parse/typecheck/elaborate) |
| `ref_build_s` | reference Bluesim link (`bsc -sim -bir -e ... -o sim.exe`; also exports the `.bir`) |
| `b3_link_s` | `trs link <top>.bir -o b3sim` wall time |
| `ref_run_s`, `b3_run_s` | wall time of the two simulations |
| `ref_rss_kb`, `b3_rss_kb` | peak RSS (`/usr/bin/time -v`) |
| `ir_passes_s`, `backend_s` | `TRS_JIT_TIME=1` phase lines from the link ("ir passes" / "backend emit"; full log in `<work>/N<n>/b3link.log`) |

Anything superlinear in `tiles` in the link or run columns is a
scaling bug (or the motivation for #20's pooling: collapse N*N
identical tile bodies into one compiled body dispatched N*N times).

## How to run

    BSC=/path/to/inst/bin/bsc \
    TRS=/path/to/trs \
    sh run.sh [workdir]

- `BSC`'s directory is put on `PATH` (the reference executable needs
  `bluetcl`); `TRS` is exported for the artifact wrapper script.
- `results.csv` is appended in the invoking directory (header written
  if absent; override the path with `RESULTS=`).  Per-N sources, logs,
  and outputs stay in `<workdir>/N<n>/`.
- The run fails loudly (`FAIL N=...`, nonzero exit) on any compile
  error, non-compiled artifact (`WARN`), nonzero simulator exit, or
  stdout mismatch.

## How to add an N

Sizes come from the `NS` env var (default `"2 4 8"`):

    NS="2 4 8 16 32" sh run.sh

Each N is independent -- `gen_grid.py <n>` emits `Grid<n>.bsv` with
top module `sysGrid<n>`, and `run.sh` appends one row per N, so re-runs
of a single size are cheap.  `CYCLES=<c>` moves the checksum/finish
cycle (default 1000) if you want run time to dominate startup at small
N.  To inspect a design by hand: `python3 gen_grid.py 4 -o -`.
