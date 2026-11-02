# sysArithPipe end to end

One corpus module's modular obligation, emitted by a script and proved:
`Obl`, `Acyc` and `Built.top` for every value type, operators, constants,
skipped values per step and solver verdicts, with the standard three axioms.
The module's 690 task statements are a `Tasks 690` child, so `top`'s
`Realizes` includes their trace. See design.md §13 for what it shows and the
measurements.

| file | what |
|---|---|
| `tools/design.py` | reads `dump.txt` and the printed APackage (`sites/tools/bapkg.py`) |
| `tools/model.py` | kinds, instances, site lists (reads, calls, task statements), users, exclusions, the scan's masks (a mirror of `Scan.lean`'s pass) |
| `tools/mke2e.py` | writes the Lean file (conventions: `emit/conventions.md`, design.md §8) |
| `tools/pass*.py` | the edits that took the emitter from revision 1 to this one (kept for review) |
| `SysArithPipe.lean` | the whole module: 227 rules, 83 children, 1,859 sites (690 task statements) |
| `SysArithPipeNoTasks.lean` | the same without the `Tasks` child (`E2E_TASKS=0`), for comparison with today's file |
| `SysArithPipe60.lean` | the first 60 rules and their run-time checks (a smoke test) |
| `flatnew/SysArithPipeFlatNew.lean` | today's flat file from the current generator (`quadfix/shape/final-fix/sysArithPipe/out.lean`) |
| `flat/SysArithPipeFlat.lean` | the older hv/new4 flat file (revision 1's comparison) |
| `run_all.sh`, `run_all_v1.log`, `full_mark_run1.log`, `flatnew/*.log` | the measurements |

    python3 -I tools/mke2e.py ../../hv/new4/sysArithPipe/dump.txt ../sites/data/sysArithPipe.ba.txt SysArithPipe.lean SysArithPipe
    E2E_TASKS=0 python3 -I tools/mke2e.py … SysArithPipeNoTasks.lean SysArithPipeNoTasks
    ./run_all.sh            # both files, then leanchecker with and without --fresh

`MARK=1` adds per-section timestamps and heartbeats; an optional fifth
argument emits only the first N rules (with their run-time checks).

**The design's:** children and their kinds, schedules and paths; rules in
schedule order and their reads, calls and task statements (sites), each
task statement under its exact condition with its arguments; each rule's
guard, read at its position; every port's users; the exclusions (disjoint
guards, urgency, `mutually_exclusive` and its run-time checks, from which the
muxes' chains are built); the fan-in of every guard, argument and task
condition (from the APackage), hence the ranks of `Acyc`.

**Synthetic (the dump does not define them; generator requirements):** the
netlist's definitions. `CAN_FIRE` is the AND of the guarded calls' RDYs and
an opaque Boolean operator over the guard's real fan-in; arguments, task
conditions and task arguments are opaque operators over their real fan-in;
constants and format strings are opaque. No `skel` (`bv_decide`). No
conditional child calls or reads (sysArithPipe needs none), no exported
methods, no escapes. FIFOL1 and ConfigRegN are assumed leaves.
