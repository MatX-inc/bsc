# Naming research: partial outputs (2026-11-14)

This is a scratch branch with data only, not code. It is based on MatX-inc/bsc
`main` (99677cb33). Three studies for the naming hierarchy were stopped before
they reported, and these are their partial outputs. Only text files under 1 MB
were copied: probe sources, generated Verilog, logs and name lists. Binaries,
.bo/.ba files and copied testsuite trees were left out. Nothing derived from
MatX designs is included.

The compiler snapshot, the decisions so far and the probe scripts are on
`claude/handoff-naming-rank` (`handoff/naming/README.md`).

## anchors/

An inventory of where bsc chooses signal names, classed by provenance, and of
which names stay the same after an unrelated design edit.

- `inv2-probes/`: the second attempt.
  - `src/`: `Anchors.bsv`, plus variants E1–E4, each with one unrelated edit.
  - `out/`: Verilog and logs for each variant and flag set.
  - `names-anchors.txt`: the signal names in each output.
  - `names.py`, `run.sh` and `gen-variants.sh`: the scripts.
- `inv1-probes/`: the first attempt, superseded by inv2.

## anchor-roles/

Probes in which two anchor roles have the same expression: EN equal to RDY,
`CAN_FIRE` equal to `WILL_FIRE`, `WILL_FIRE` equal to `inst$EN` equal to
`r$EN`, a method result equal to a register read, inlined wires, and
`always_ready`/`always_enabled` methods.

`out/` holds one directory per flag set:

| Directory | Flags |
|---|---|
| `def` | defaults |
| `kf` | `-keep-fires` |
| `kib` | `-keep-inlined-boundaries` |
| `kf_kib` | both |
| `kf_kib_kmc` | both, plus `-keep-method-conds` |

## keepflags/

A measurement of `-keep-fires -keep-inlined-boundaries` (K), K plus
`-keep-method-conds` (KM), and `-keep-method-conds` alone (M), against the
defaults (MAIN).

- `suites-logs/<variant>/`: suite logs as far as the runs got. They are
  partial.
- `stability-probe/`: a design plus edited copies (`gen/`, `mkedits.py`), with
  the names under each variant (`names-*.txt`) and `stab.py`, which counts
  names that change after an unrelated edit.
- `bin/`: the comparison scripts (`vnames.py`, `vcanon.py`, `vcmp.py`,
  `cfilt.py`, `job.sh`, `measure-flags.sh`).
