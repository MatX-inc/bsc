# Generic Lean theory behind schedlean (work in progress)

Prototypes, not yet used by the generator. Lean 4.35, core library only.

- `all-orders/`: every order of a module's steps that a parent may use
  (consistent with the exported CF/SB/SBR/C annotations) gives the same
  result as bsc's order, by conflict-serializability proved once
  (`Gen.serial`), a per-field block check, and a parent-order check
  (`Gen.all_orders_parent`).  `Example.lean` instantiates it on a toy design.
  Build: `lean -o Generic.olean Generic.lean`, then
  `LEAN_PATH=.:<toolchain>/lib/lean lean Example.lean`.  Open points are in `design.md`; argument-
  less value methods will become one step read any number of times, replacing
  the K read copies used here.
- `modular/`: the per-module contract `Realizes` (for every legal parent
  call sequence there is a one-at-a-time run, with each caller's calls kept
  together, that matches the hardware's outputs and next state), the
  composition theorem `Modular.compose`, the induction over the instance
  tree `Built.realizes` / `Built.top` (with non-vacuity), and primitive
  contracts checked against bsc's Verilog (Reg, the wire family, CReg n,
  FIFO2).  `Example.lean` has a two-level hierarchy and negative cases that
  must fail (a sandwiching SB export, a wrong mux argument, a forbidden
  parent order, an RWire loop); `split/` composes across separately
  compiled files.  Each module's own system-task calls are part of its
  contract, unordered against other modules' (not true across synthesis
  boundaries in Verilog).  `Scan.lean` proves the linear scan that gives
  the obligation's pair facts, `EmitLemmas.lean` and `SiteCond.lean` the
  emission and site-condition lemmas, and `Fifo2R.lean` an always-ready
  FIFO2 `first`.  `build.sh` builds everything, capped.  `e2e/`, `scan/`,
  `sites/` and `emit/` hold the prototype emitter and measurement tools.
  Open points are in `design.md`; the open review items are in the commit
  that added this.
