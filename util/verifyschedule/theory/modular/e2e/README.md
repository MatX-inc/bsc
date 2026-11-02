# The end-to-end modules (round 7)

Two corpus modules' modular obligations, emitted by one script and checked:

| file | what | proved |
|---|---|---|
| `SysArithPipe.lean` | sysArithPipe: 227 rules, 83 children, 1,859 sites (690 task statements) | `obl : OblU`, `acyc`, `top : RealizesU …` |
| `SysArithPipeNoTasks.lean` | the same without the `Tasks` child (`E2E_TASKS=0`) | the same |
| `SysArithPipe60.lean` | the first 60 rules and their run-time checks, the scan in 128-use segments (`E2E_SEG_USES=128`; the segmentation regression test) | the same |
| `SysArithPipeSeg.lean` | sysArithPipe with the scan in 4 segments of at most 600 uses (`E2E_SEG_USES=600`) | the same as `SysArithPipe.lean` |
| `MkBufferControl.lean` | mkBufferControl: 9 methods, 18 rules, 60 children, 303 sites, 12 escaped pairs, conditional and readiness reads | `obl : OblU` from `Hyps` and `Unconfirmed` (two facts not confirmed: one readiness fact the site tool refutes, one caller-exclusivity fact that is action shadowing); no `Acyc`/`top` (design.md §14) |

| tool | what |
|---|---|
| `tools/design.py` | reads `dump.txt` and the printed APackage (`sites/tools/bapkg.py`) |
| `tools/condsites.py` | the conditional sites and their formulas, from `sites/tools/sitecond.py` |
| `tools/model.py` | kinds, steps (rules, then methods) as site lists, users, exclusions, the scan's masks, escapes |
| `tools/mke2e.py` | writes the Lean file (`emit/conventions.md`, design.md §6–§8) |
| `tools/shadow.py` | writer pairs of one port that may both call (action shadowing) |
| `tools/mke2e_r7a.py`, `mke2e_r6cost.py` | the emitter before the method extension, and the cost piece's (for diffing) |
| `tools_r7a/` | the round-7 emitter before the r7-1 revision (for diffing and the before/after section times) |
| `flat7/runflat.sh`, `flat7/run1.log` | today's flat files (current generator; task agreement) for both modules |
| `flat7/runr8.sh`, `flat7/r8.log` | the revision's comparison set at one period: `SysArithPipeSeg`, the four flat files, both atom tests (`skel`) |

    python3 -I tools/mke2e.py ../../hv/new4/sysArithPipe/dump.txt ../sites/data/sysArithPipe.ba.txt SysArithPipe.lean SysArithPipe
    E2E_TASKS=0 python3 -I tools/mke2e.py … SysArithPipeNoTasks.lean SysArithPipeNoTasks
    E2E_SEG_USES=128 python3 -I tools/mke2e.py … SysArithPipe60.lean SysArithPipe60 60
    E2E_SEG_USES=600 python3 -I tools/mke2e.py … SysArithPipeSeg.lean SysArithPipeSeg
    python3 -I tools/mke2e.py ../../hv/new4/mkBufferControl/dump.txt ../sites/data/mkBufferControl.ba.txt MkBufferControl.lean MkBufferControl

`MARK=1` adds per-section timestamps and heartbeats. The emitter runs the site
tool on the design (its working files go to `sitework_<NAME>/`).

**The design's:** children, kinds, schedules and paths; rules and methods with
their reads, calls (exact conditions) and task statements; each step's guard
read at its position; every port's users; exclusions (disjoint guards,
urgency with several blockers, `mutually_exclusive` and its run-time checks);
schedlean's site conditions (conservative reach; readiness under the ready
literal, the readiness fact over `R_seq` sequenced by expression structure and
read at the site values of the step's conditional reads, with an invariance
fact where `R` reads another conditional read); escaped pairs and their
dynamic facts, each labelled by the solver's verdict for that pair and the
conditions the file states; bsc's `mutually_exclusive` groups (one `SufAsm`
per group); the fan-in of every expression.

**Synthetic (the dump does not define them; generator requirements):** the
netlist's definitions. `CAN_FIRE` is the AND of the unconditional guarded
sites' RDYs and an opaque Boolean operator over the guard's real fan-in, with
bsc's lifted `!c ∨ RDY` for conditional guarded calls; arguments, conditions
and results are opaque operators over their real fan-in; a use of a
conditional read is split on its reach (`bif R then f(fan-in) else
f'(fan-in without the read)`). A readiness `R` is an opaque operator over the
reads its formula's literals read, conditional reads at their site values;
its facts are checked by the site tool for exactly that formula. Because the
site condition and the split condition of an (a) read are the same opaque
operator, a too-strong (a) condition cannot fail in these files; for (r)
reads the check runs through the stated facts (design.md §18 item 1). Shared RegFile copies' address
selection is not in the dump (mkBufferControl's `Acyc` stops there). No
`skel`. FIFOL1, ConfigRegN, SizedFIFO and RegFile are assumed leaves; FIFO2 is
the library's.
