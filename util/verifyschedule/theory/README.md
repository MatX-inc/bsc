# Generic Lean theory behind schedlean (work in progress)

Prototypes, not yet used by the generator. Lean 4.35, core library only.

- `all-orders/`: every order of a module's steps that a parent may use
  (consistent with the exported CF/SB/SBR/C annotations) gives the same
  result as bsc's order, by conflict-serializability proved once
  (`Gen.serial`), a per-field block check, and a parent-order check
  (`Gen.all_orders_parent`).  `Example.lean` instantiates it on a toy design.
  Build: `lean -o Generic.olean Generic.lean`, then
  `LEAN_PATH=. lean Example.lean`.  Open points are in `design.md`; argument-
  less value methods will become one step read any number of times, replacing
  the K read copies used here.
- `modular/` (to follow): the per-module contract `Realizes`, the
  composition theorem and primitive contracts.
