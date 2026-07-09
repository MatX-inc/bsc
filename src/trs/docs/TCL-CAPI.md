# trs-capi: the bluetcl `sim` surface (debug compile mode)

Goal: a cdylib that bluetcl's `sim load` accepts as a drop-in Bluesim
model — `sim load <file>.so <top>` and every interactive command
behaves byte-identically to the reference kernel.  This is the first
deliverable that *defines* the DEBUG compile mode: it needs exports
and interp-visible state, which the FAST compile deliberately strips.

## The contract (measured from the source, 2026-07-09)

Loader: `src/comp/BluesimLoader.hs` `loadBluesimModel` — dlopen
RTLD_NOW, then dlsym of exactly this set (load order):

    new_MODEL_<top>,
    bk_init, bk_now, bk_set_timescale, bk_version,
    bk_append_argument,
    bk_define_clock, bk_num_clocks, bk_get_nth_clock, bk_clock_name,
    bk_get_clock_by_name, bk_clock_initial_value, bk_clock_first_edge,
    bk_clock_duration, bk_clock_val, bk_clock_cycle_count,
    bk_clock_edge_count, bk_clock_last_edge,
    bk_quit_after_edge, bk_schedule_ui_event, bk_remove_ui_event,
    bk_set_interactive, bk_advance, bk_is_running, bk_sync,
    bk_abort_now, bk_finished, bk_exit_status, bk_fataled,
    bk_top_symbol, bk_lookup_symbol, bk_get_size, bk_get_key,
    bk_is_module, bk_is_rule, bk_is_single_value, bk_is_value_range,
    bk_peek_symbol_value, bk_get_range_min_addr, bk_get_range_max_addr,
    bk_peek_range_value, bk_num_symbols, bk_get_nth_symbol,
    bk_set_VCD_file, bk_enable_VCD_dumping, bk_disable_VCD_dumping,
    bk_shutdown

Load sequence: `new_MODEL_<top>()` -> `bk_init(model, master=True)`
(NULL => load fails) -> `bk_top_symbol` seeds `current_directory`.
`sim unload` = `bk_shutdown` + dlclose.  The generated "executable"
is a shell wrapper exec'ing `tcllib/bluespec/bluesim.tcl $0.so <top>`;
bluesim.tcl drives load/args/vcd, then `sim run` / `sim step N`
(batch) or `sim config interactive` + `source <script>` (-f path).

Reference full header: `src/bluesim/bluesim_kernel_api.h`.  Export
maps whitelist `bk_*`, `new_MODEL_*` (bs_elf_export_map.txt).

Run-control mechanics in bluetcl.hs worth mirroring exactly:
- `sim step [N]`: reads current clock val/now/cycle_count, computes
  target edge via `bk_clock_edge_count + N`, `bk_quit_after_edge`,
  `bk_advance` (sync path restores the edge limit if not reached).
- `sim nextedge`: quit_after_edge at pos+1 AND neg+1 on EVERY clock,
  advance, restore previous limits.
- `sim runto <t>`: `bk_schedule_ui_event t` + advance; remove the UI
  event if the target was not reached.
- `sim get`: module handles redirect to their `""` sub-symbol before
  `bk_peek_symbol_value` (handleModuleRedirect).  Values print as
  `<bits>'h<hex>` assembled from the returned word pointer with
  `bk_get_size` bits.
- `sim ls`/globbing walk: `bk_num_symbols` + `bk_get_nth_symbol` +
  `bk_get_key` + glob match; exact segments use `bk_lookup_symbol`
  (dotted paths resolved by the KERNEL, not the Tcl side).

Interactive acceptance: testsuite/bsc.bluesim/interactive/ — 22
sim_output + 22 compare_file assertions over 8 designs; command
frequency: lookup 30, ls 27, cd 23, step 22, get 22, pwd 18,
clock 15, time 11, up 5, run 4, getrange 3, timescale/sync/
nextedge/describe/stop/runto few.

## Engines (Ravi, 2026-07-09): interp, JIT, AOT — one or several

The model .so multiplexes the three engines behind one bk_* surface:

- selection at bk_init: link-time default baked into the shim's
  Model struct (`trs link --interactive --engines=jit,aot,...`),
  overridable by TRS_CAPI_ENGINES at load.  One engine = normal
  interactive use (interp for full-fidelity debug, hybrid JIT for
  fast `sim run`, AOT for artifact-exact execution).
- SEVERAL engines = interactive ORACLE: SimState holds
  Vec<Engine>, run control fans out (every engine advances to the
  same stop condition), queries answer from engines[0] (primary).
  After each advance: compare now, per-clock cycle/edge counts,
  finished/stop/exit status; `sim get`/`getrange` peeks compare
  across engines on demand — a mismatch reports loudly on stderr
  and (policy TBD) flips bk_fataled so scripts stop at the point
  of divergence.  This is diffsweep's differential oracle made
  interactive: step to the divergent instant, then inspect state.
- design wrinkles, decided up front:
  1. stdout ownership: ONLY the primary engine's $display output
     reaches stdout; secondary engines run output-suppressed (an
     Interp quiet flag) — otherwise every task effect prints k
     times and byte-parity vs the reference dies.  Output
     COMPARISON (vs suppression) needs per-engine capture and is
     a later refinement.
  2. AOT engine = the artifact pair (.bir + design .so loaded the
     artifact way, bir_hash/layout-rev checked); JIT engine = the
     hybrid inside the interpreter (TRS_JIT machinery); interp
     engine = plain.  All three are Interp-rooted, so the engine
     vector is Vec<Interp> with per-engine mode flags — the fan
     -out loop is trivial; the work is in the stop-condition and
     comparison plumbing.

## Mapping onto trs

Crate `trs-capi` (cdylib).  `new_MODEL_<top>` cannot be known at
crate compile time — the LINKER emits it: `trs link --interactive`
generates a tiny C (or LLVM) shim object exporting
`new_MODEL_<top>` that returns a heap handle wrapping {bir path or
embedded BIR bytes}, and links it with the capi staticlib into
`<out>.so`.  Everything else is generic.

- Model/state: `bk_init(model, master)` constructs the Interp from
  the BIR (interpreter engine — the debug mode's executor, full
  fidelity), calls `prime()`.  Handle = Box<SimState>{interp, clocks,
  ui_events, run-status}.  master=True installs the default clock
  waveform (5/5) and default reset — prime() already runs the kernel
  reset protocol; verify waveform timing parity (first_edge, phases)
  against kernel.cxx defaults.
- Run control: `bk_advance` = interp.advance() with the stop
  condition set from quit_after_edge/quit_at/ui events; the stepper
  (prime/advance/finish, resumable, VCD-safe across steps) is
  already the exact engine `sim step` needs.  `bk_advance(async)`:
  drive on a thread; `bk_sync` joins it; `bk_is_running` polls.
  (The .cmd corpus uses async only in async.cmd — implement sync
  first, async behind a thread with the interp moved in.)
- Clocks: trs already resolves kernel clocks in prime()
  (VcdClock, composition clock order).  bk clock handles = indices
  into that list; cycle/edge counts and last-edge times are already
  tracked for VCD; expose them.
- Symbol tree: built from the BIR + InstEnvs at bk_init.  Nodes:
  module instances (children in DFS order), rules (SYM_RULE),
  state (prim values: SYM_DEF-equivalent), ports/params.  Bluesim
  sorts symbols case-insensitively (symOrd) — match it.  Value
  peeks return pointers into a per-symbol refresh buffer filled
  from prim state on demand (interp is authoritative; no arena
  export needed — this is the INTERPRETED debug mode).  Ranges:
  RegFile/BRAM expose lo/hi + fetch.  The module `""` sub-symbol
  redirect must exist for `sim get` on modules.
- VCD: bk_set_VCD_file/enable/disable map onto the existing VCD
  writer (vcd_file_pending machinery).
- $finish/$stop/abort: map onto finished/exit-status state; verify
  post-$finish `sim step` errors match ("cannot step" contract of
  the -c driver).

Acceptance gates, in order: (1) `sim load` + `sim time` + `sim ls`
on mkTest; (2) the interactive battery's mkTest.cmd byte-identical;
(3) all 22 interactive outputs byte-identical; (4) VCD-under-Tcl
parity spot checks.
