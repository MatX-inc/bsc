# Lockstep selfcheck — `trs run --selfcheck`

One process validates EVERY execution tier against the others with no
reference simulator anywhere: the PRIMARY engine (compiled, when the
artifact carries `--code`; the hybrid JIT otherwise) runs the design
normally — it owns stdout, waveforms, and the exit status — while
quiet SHADOWS of the same BIR run beside it.  The default shadow set
covers every other tier in one run: a pure interp always, plus a
hybrid-jit shadow when the primary is the aot artifact — so interp,
jit, and aot cross-check simultaneously, ONE test mode instead of a
per-engine matrix (`TRS_SELFCHECK_ENGINES=interp[,jit]` overrides).
Every `--selfcheck-every` default-clock posedges (default 1000, env
`TRS_SELFCHECK_EVERY`), and at the end of the run, each shadow is
compared against the primary:

- shape first: cycle cursor and $finish status (state addressed at
  different times compares apples to oranges — the capi oracle's
  per-engine gate);
- time, but only at stops where time is architecturally VISIBLE
  ($finish, $stop, event-heap-dry).  A stop consumed by the cycle
  budget alone is an internal point: the fused central player and the
  general event loop credit the last posedge's companion-negedge
  instant differently, so `now` can legitimately sit half a period
  apart with identical architectural history.  No output can observe
  that skew — VCD disables the central player, and interactive stops
  use the heap loop — and which loop is engaged is itself racy (the
  fused bodies compile on a worker thread), so comparing it would be
  a nondeterministic false positive, witnessed before this gate was
  added;
- architectural prim state (`state_divergence`, the same walk the
  bluetcl multi-engine oracle uses at every stop — registers,
  RegFile/BRAM ranges, FIFO contents; edge-transient wires excluded).

A divergence reports on stderr (instant + the first mismatching
entries, primary-vs-shadow) and the run exits 87 AT the divergence,
the oracle doctrine's stop point.

## Why

A passing diffsweep proves compiled == interp on the corpus's stdout;
the selfcheck proves compiled == interp on ARCHITECTURAL STATE, at
checkpoints through the run, on any design, with zero reference
apparatus.  That makes it (a) the cheap per-design validation mode —
`TRS=... ./design.cexe --selfcheck` on an existing artifact, no
relink; (b) a free oracle for fuzzing (any generated design
self-validates); (c) the tool that catches divergences NEAR THEIR
ORIGIN instead of at the first differing $display, with the
mismatching state element named.

## Knobs

- `--selfcheck` — arm, cadence 1000 (or `TRS_SELFCHECK_EVERY`).
- `--selfcheck-every N` — arm with cadence N.
- `TRS_SELFCHECK=1` — arm environmentally: existing artifact wrappers
  run checked with no relink (how the corpus-wide selfcheck sweep is
  driven: `TRS_SELFCHECK=1 diffsweep ...`).
- `TRS_SELFCHECK_TRACE=1` — print each checkpoint (target, per-engine
  time/cycle) to stderr.
- `TRS_SELFCHECK_INJECT=<cycle>` — the detector's negative witness:
  once the primary passes that cycle the shadow is advanced one extra
  posedge, which must trip the next compare (exit 87).  Test-only.

The flag is deliberately absent from the artifact's `-h` usage text:
that text is byte-compared against reference bluesim.tcl output by
the testsuite (mkTest help.cmd).

## Caveats

- BDPI: dlopen of one path is one refcounted image, so user C globals
  are process-global and SHARED across both engines — a lockstep
  shadow DOUBLE-EXECUTES stateful foreign functions and corrupts the
  primary's own outputs (14 foreign-battery witnesses in the first
  selfcheck sweep).  Designs importing BDPI therefore SKIP the shadow
  (stderr note; the run proceeds unchecked).
- FIFO state compares use the ARCHITECTURAL view (occupancy + live
  entries in queue order), not the bk tree's raw ring slots: post-deq
  residue is dead state and the engines' ring disciplines legitimately
  differ there (boxed interp vs compiled arena — dft64/Divide phantom
  divergences in the first sweep).
- The shadow's construction runs under the quiet stamp: elaboration
  diagnostics ($readmem gap warnings) would otherwise print twice.
- The shadow runs interpreted BY DESIGN; it is marked debug-tier and
  exempt from TRS_REQUIRE_AOT, which polices the primary (the
  artifact's execution engine) only.
- Cost: one full interp execution rides along, plus a state walk per
  checkpoint — this is a validation mode, not a fast path.
- `-c`/`-f` script runs dispatch to bluetcl, where the multi-engine
  oracle (`TRS_CAPI_ENGINES=interp,jit` etc.) is the equivalent
  facility; `--selfcheck` applies to batch runs.
