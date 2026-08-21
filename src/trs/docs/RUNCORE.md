# RunCore: self-sufficient AOT boot

Goal (init-ladder terminal rung): an artifact run whose boot is
`mmap the .so + fill the arena from a baked image + dlsym three
tables`, skipping the three measured boot masses — snap decode
(3-4ms), `Interp::new` instantiation (5-7ms), and planning (~1.5ms,
CFL warm laps).  The remaining Verilator deficits across the bench
pool (`FloatTest` -2.8, `TrafficBRAM` -3.3, `BRAM0Test` -8.2, `CFL`
-9.1ms at the full-pool stamp) each equal their measured boot mass;
RunCore is the sweep lever.

## Census: what compiled code actually calls back into

Measured on the slim runner (callgrind + code audit):

- `trs_bram_tick_cb` — pure arena, no interp state
- `jit_foreign_cb` — $display family, $finish/$stop, value tasks
- `jit_prim_cb` — boxed-prim bounces (ZERO on Dividers; near-zero
  elsewhere post Counter-arena; guarded FIFO warn-bounces exist)
- `STRING_CONCAT` — interns into the dyn string table
- stdio flush hooks

Whole-run Dividers = 4.2M Ir, of which boot ~3.3M (malloc + sip
hashing + snap decode).

## What the artifact already carries (no sidecar needed)

- **Ordinal-indexed fn tables**: `trs_edge_tab` (fused per-comp edge
  fns), `trs_sched_tab`, `trs_exec_tab`, with `_len` globals.  Three
  dlsyms resolve every function; no symbol-name table needed.
- **Foreign/prim call-site tables**: the `trs_protos` global
  (`encode_protos` wire format) bakes per-ordinal
  `ForeignSpec`/`PrimCallSpec` tables — token `(ordinal, kind,
  local)` resolves without trial_lower.
- **Patchable callback globals**: `trs_cb_foreign`, `trs_cb_prim`,
  `trs_cb_stdio`, BDPI callee globals — the loader writes function
  addresses in; a RunCore boot patches in RunCore-flavored callbacks
  instead of the Interp-backed ones.
- **Compiled ticks**: `trs_edge_wire_ticks` level 2 = wire clears +
  cregs + brams inside the edge fns.

## Sidecar v1 (landed, corpus-proven)

`<base>.arena` = `b"TRSARENA"` + LE u64 header `[version=1,
AOT_LAYOUT_REV, salted bir_hash, nslots]` + RLE `(value, run)`
pairs.  Emitted by the link's Emit arm from the SAME four steps as
the load tail (alloc, attach, reset levels, memo stamps).
`TRS_RUNCORE_CHECK=1` bit-compares on every Path load; dual-built
across all 996 corpus designs with zero mismatches.  Gated out:
traced designs (`vcd_trace` — rec_inits apply later, wave engine
needs the interp) and mem-file designs (arena tracks files that may
change between link and run).

## Sidecar v2 (next): the boot descriptor

Additional sections, all emit-time-known in the Emit-Ok arm:

1. **String table** — the design's full `d.strings` (StrDyn tokens
   may select any id at run time, so the whole table, not just the
   referenced subset).  Dyn interning appends past its length,
   exactly like the interp's `dyn_strs`.
2. **Instance paths** — `insts[i].path` for every inst referenced by
   a `ForeignSpec` (%m locations) and every BRAM warn registration.
3. **Clock config** — the single periodic Wave clock's `hi`, `lo`
   (central-loop eligibility guarantees exactly one, on the default
   clock).
4. **Posedge comp order** — the comp ordinals the central loop calls
   per posedge (`pos_rcis`), in call order; plus the verified claim
   that every negedge comp is skippable (rule-less, covered ticks).
5. **BRAM warn names** — (arena block base offset, full name) pairs
   so `BRAM_WARN` warnings keep name parity without prim structs.
6. **Eligibility flag** — the conjunction the emitter can prove:
   all comps fused, tick level 2 with no uncovered non-reset ticks,
   no early rules, single default Wave clock, no reset generators,
   no dynamic/driver clocks, no $dump* strings in the design, no
   BDPI imports (rung 1; BDPI later via the existing callee
   globals).  No flag → classic boot, silently.

Validation discipline (same as v1): the classic boot path under
`TRS_RUNCORE_CHECK=1` recomputes each section from the live plan and
bit-compares against the sidecar — corpus-prove v2 before the driver
trusts a byte of it.

## The driver (after v2)

A `RunCore` context (NOT an `Interp`): the `ForeignEnv` split
(foreign.rs) owns console/file/finish/plusargs/timescale state and
services the design-independent arms wholesale; `envp` points at the
RunCore context and `runcore_foreign_cb` mirrors `jit_foreign_cb`
over baked tables (protos from the .so, strings/paths from v2).

Run shape = the central loop's steady state (fused edge fns over
`pos_rcis`, tp/tn bookkeeping, finished/stop checks) plus the early
instants the general loop handles today:

- t=0: top reset asserted (kernel-driven), initial edges fire with
  reset conds holding rules off (reset wire slots in the arena carry
  the asserted level; the baked image holds boot-time levels).
- t=2: deassert after that instant's logic, then steady loop.

The exact assert/deassert choreography must byte-match the general
loop (witnesses: any design that $displays during the reset window).
This is the driver's correctness core — replicate, then diffsweep.

## Native prim servicing (rung 3b, landed — supersedes "lazy Reflect")

The original plan materialized a full Interp on first bounce.  The
landed design (Ravi's steer: "handle their prims more natively /
build precisely what they need, not everything") never builds an
Interp at all:

- **Seeds**: the link bakes each bounce-reachable prim's STATIC
  config (`Prim::runcore_seed` → sidecar section 9): kind tag +
  layout words + names.  Bounce-reachable = any inst named by a
  `PrimCallSpec` in the protos tables; on an eligible design every
  such prim is arena-attached (the boxed gate), so its DYNAMIC state
  is entirely arena slots — which the baked window image already
  carries.
- **Restore + adopt**: the boot restores each seed as the identical
  prim.rs struct (`runcore_restore`, with the footprint bound
  computed by the prim's own layout arithmetic) and adopt-attaches
  it (`arena_adopt`: set the slot pointer, write NOTHING — attach's
  state writes are exactly what adopting a live arena must skip).
  All restores happen up front, before any output, so every
  hostile-sidecar failure mode is still a silent classic boot.
- **Service**: `runcore_prim_cb` mirrors `jit_prim_cb` — same token
  decode, same marshaling, same trait methods on the same struct
  over the same slots.  A bounce is byte-for-byte the classic
  bounce, at zero materialization cost.

Eligibility change: prim call sites no longer boot classic; only a
site whose target cannot be seeded (unattached prim, unseedable
kind) does.  All 8 arena kinds seed (Reg, RWire, ConfigReg, CReg,
Counter, RegFile, Fifo, Bram).  Sidecar VERSION 3 → 4 (the version
is the eligibility-semantics revision); the `TRS_RUNCORE_CHECK`
witness recomputes the bounce-reachable set from the live plan and
compares every baked seed and slot against the live prim.

Witnesses: RegFileWarnCone (RegFile out-of-bounds warns), BramWideBE
(BRAM out-of-bounds puts, wide byte-enables), FifoWarn (guarded-FIFO
enq-to-full / deq-from-empty warn + drop) — all boot RunCore, fire
their bounces natively, and byte-match the Bluesim reference; the
regress battery runs them classic AND under `TRS_RUNCORE=1`.

The `$dump*` decline arms remain classic-boot territory (wave
machinery needs the interp); they are gated by the wave/plusarg
routing checks, not by materialization.

**Sidecar trust model** (3b panel finding, applies since v1): the
sidecar is a build artifact, not attacker input — the window arena
image IS simulator state, so a crafted file could already alter sim
bytes with or without seeds.  The boot's hardening therefore targets
CORRUPTION and VERSION SKEW (every structural anomaly is a silent
classic boot before any output; the bir-hash pairs sidecar and .so;
footprints bound every slot access to the arena), not adversarial
byte-crafting.  Ledgered follow-up: an integrity checksum over all
sections (bake splice included) to catch content-level corruption
that passes the structural checks.

## Follow-ups queued behind the driver

- Post-reset arena image: skip the reset window too, gated on an
  output-silent reset window (checkable during dual-build).
- BDPI-importing designs (callee globals already exist).
- Relink-on-demand for batch -V on RunCore-booted artifacts.

## Registered predictions (to score when the driver lands)

Dividers ~1.8-2.2ms, CFL ~5-8ms, TrafficBRAM at-or-under Verilator,
8-9/10 bench-pool designs boot RunCore (mem-file + traced classic).

Scored at the 3a stamp: CFL HIT (7.03ms); Dividers/TrafficBRAM/count
MISS (the strict prim-site gate held them classic); unpredicted: the
Long 20.6x flip (the RunCore steady loop is ~2x faster PER CYCLE).

## Registered predictions for the 3b re-stamp

Made BEFORE the re-stamp, driver-armed pool at the 3b head:
- Boots: 6/11 RunCore (Dividers, TrafficBRAM, Sudoku, FloatTest join
  CFL and Long; mem-file trio has no sidecar; DFT64 pair stays
  gated).
- Dividers 1.9-2.3ms → at-or-under Verilator's 2.45 (odds ~65%).
- TrafficBRAM ~22-26ms → at-or-under Verilator's 24.1 (odds ~50%);
  its 403k prim bounces now pay runcore_prim_cb, same order as
  jit_prim_cb, so the win is boot + loop checks, not bounce cost.
- FloatTest ~43-47ms → still behind Verilator's 41.5 (odds ~70%
  behind); Sudoku ~85-92ms (already a win, margin widens).
- CFL/Long/BRAM0Test/Mesa/SparseRF/DFT64: unchanged within noise.

## The driver's parity contract (rung 3a, landed)

Confirmed by the adversarial panel as the items the boot must state,
not assume:

- **Exit code**: 1 iff `$fatal` fired; `$finish(n)`'s status NEVER
  surfaces as the process exit code; `$stop` is a yield and the batch
  run then exits 0.  (`runcore::try_boot` returns exactly
  `fataled as i32`; witnessed by the regress battery under
  `TRS_RUNCORE=1` — FinishEdge et al. compare exit codes.)
- **Routing**: any selfcheck (flag or `TRS_SELFCHECK`), any wave
  request (`-V`, `+bscvcd`/`+bscfst`, link formats), `TRS_JIT`, and
  the interactive/exe tiers all boot classic; the slim binary's
  dispatch execs the full binary for selfcheck/jit before run_file is
  ever reached.
- **Eligibility semantics revision**: the sidecar VERSION is the
  eligibility-semantics revision.  Any change to the gate rules bumps
  it (2 = pre-prim-gate, refused as stale; 3 = current), so a newer
  driver never trusts an `eligible` flag computed under older rules.
- **Known witness blind spots (minor, ledgered)**: the inverse
  eligibility witness triggers only on `$finish`-terminated runs
  (cycle-limit and event-exhaustion endings are unwitnessed), and
  `central_engaged` is monotone per Interp — refine when the witness
  next changes shape.
