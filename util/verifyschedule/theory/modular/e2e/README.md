# The end-to-end modules (round 8)

Corpus modules' modular obligations, emitted by one script (`tools/mke2e.py`, the round-8 merge) and
checked by `../build.sh`:

| file | what | proved |
|---|---|---|
| `SysArithPipe.lean` | sysArithPipe: 227 rules, 83 children, 1,859 sites (690 task statements); ConfigRegN is LastCaller's model | `obl : OblU`, `acyc`, `top : RealizesU …` |
| `SysArithPipeNoTasks.lean` | the same without the `Tasks` child (`E2E_TASKS=0`) | the same |
| `SysArithPipe60.lean` | the first 60 rules and their run-time checks, the scan in 128-use segments (the segmentation regression test) | the same |
| `SysArithPipeSeg.lean` | sysArithPipe with the scan in 4 segments of at most 600 uses | the same |
| `MkBufferControl.lean` | mkBufferControl: 9 methods, 18 rules, 60 children, 303 sites; `lockInterLoads.write` is a last-caller port | `obl` from `Hyps` and `Unconfirmed` (one fact: `rdy_3`, refuted by the site tool); no `Acyc`/`top` |
| `MkUrgencyBlocking.lean`, `modes8/MkUrgencyBlocking_{noagg,nosched,both}.lean` | mkUrgencyBlocking in the four bsc modes; `lastOp.write` is a last-caller port | `obl`, `acyc`, `top`, from `Hyps` alone |
| `MkMCR.lean` | mkMCR: 28 rules, 1 method, 44 children; 3 RegN and 1 ConfigRegN last-caller ports | `obl` from `Hyps` alone; no `Acyc`/`top` (a method's enable drives an RWire's path input) |
| `lcprobe/LcProbe64.lean` | 64 rules writing one register every cycle (last caller, 64 callers) | `obl`, `acyc`, `top` |
| `mtask/MethTask.lean` | a rule's `$display` (a statement, checked) and a method's (bsc G0020, left out) | `obl`, `acyc`, `top`; `mtask/MethTaskBadEn.lean` (the rule's statement enabled always) fails |
| `mejoin/` | soundness review r8b-1's negative control: `MeJoin.bs` joins `r1`, `r2` against `r3` (`rJoinMutuallyExclusive`), so bsc's check line has two rules on its first side | no file: the emitter stops (a file states one rule on a line's first side); the gate's run finds `a.write` a last-caller port of `r1` and `r2` (`regen8.log`) |

| tool | what |
|---|---|
| `tools/design.py` | reads `dump.txt` (both read-line forms) and the printed APackage (`sites/tools/bapkg.py`) |
| `tools/condsites.py` | the conditional sites and their formulas, from `sites/tools/sitecond.py` |
| `tools/model.py` | kinds, steps (rules, then methods) as site lists, users, exclusions, the scan's masks, escapes; records the methods whose task calls are left out (`mtasks`) |
| `../sites/tools/mecheck.py` | bsc's `mutually_exclusive` check lines read by structure (C31), for the model and the shadow tool |
| `tools/mke2e.py` | writes the Lean file (`emit/conventions.md` C1–C31, design.md §5–§8, §12); under `E2E_GATE=1` the gate's scan, escape facts and bounds (design.md §11) |
| `tools/r8pieces/` | the three round-8 pieces' emitters (`mke2e_lc.py`, `mke2e_cost.py`, `mke2e_min.py`) and round 7's (`mke2e_r7.py`), for diffing |
| `regen8.sh`, `regen8.log` | every file above from `tools/mke2e.py` |

The shadow tool is `sites/tools/shadow.py` (one tool; the e2e copy was merged into it).
`r8a_files/` holds the files as they were before review r8-1 (they differ only in header comments), and
`r8b_files/` before review r8-2's fixes (since then: the escape check's linear form, C27, in
mkBufferControl; the assumed user assertions and the facts bound in every header), and `r8c_files/`
before review r8b-1's fixes (since then, in every file: `isT`/`tnum`, `okP_task` and the task-site
comparison in `okT_exceptZ` (C29), the facts in groups `HypsG<k>`/`UnconfirmedG<k>` with uses `hy.g<k>.…`
(C30), two more header rows (facts in the module, fields in one group) and `asmP`'s docstring; in
mkBufferControl the escape proofs pass `lo` and the second step's decision `escT2_n`).

    ./regen8.sh                      # all of the above (sysArithPipe variants, mkBufferControl, mkUrgencyBlocking x4, mkMCR)
    E2E_TASKS=0 …                    # no Tasks child
    E2E_SEG_USES=128 … NAME 60       # the first 60 rules, 128-use scan segments
    E2E_LC=0, E2E_CFGLIB=0           # last caller off; ConfigRegN as an assumed leaf
    E2E_BSC_STATS=1                  # bsc's coverage of the escape facts (measurement only)
    MARK=1                           # per-section timestamps, heartbeats and RSS (to stderr and OUT.lean.marks)
    E2E_GATE=1                       # the gate's scan run only (../gate8/gatescan.sh): one GATE line, no file

The emitter runs the site tool on the design (its working files go to `sitework_<NAME>/`). It prints
a `note:` line for each method whose task calls it leaves out of task agreement (design.md §12).

**The design's:** children, kinds, schedules and paths; rules and methods with their reads, calls
(exact conditions) and the rules' task statements; each step's guard read at its position; every
port's users; exclusions (disjoint guards, urgency with several blockers, `mutually_exclusive` and
its run-time checks); schedlean's site conditions; escaped pairs and their dynamic facts, each
labelled by its source (the solver for that pair and the stated conditions, every port its own atom;
or bsc's fact for exactly that pair, with the stated conditions implying bsc's), each pair's check
keyed by first-step site and linear in its facts (C27); the last-caller ports (key: RegN/RegUN/RegA
and ConfigRegN `write`; two callers the solver finds may both call); bsc's `mutually_exclusive`
groups (one `SufAsm` per group), user assertions listed in the first comment (a `conflict_free`
check stops the emitter); the fan-in of every expression.

**Synthetic (the dump does not define them; generator requirements):** the netlist's definitions.
`CAN_FIRE` is the AND of the unconditional guarded sites' RDYs and an opaque Boolean operator over the
guard's real fan-in, with bsc's lifted `!c ∨ RDY` for conditional guarded calls; arguments,
conditions and results are opaque operators over their real fan-in; a last-caller port's argument is
bsc's priority mux, latest caller first, each arm selected by `WILL_FIRE ∧ call condition`; a use of
a conditional read is split on its reach. Because the site condition and the split condition of an
(a) read are the same opaque operator, a too-strong (a) condition cannot fail in these files; for (r)
reads the check runs through the stated facts (design.md §20 item 1). Shared RegFile copies' address
selection is not in the dump (mkBufferControl's `Acyc` stops there). No `skel` (measured separately through atom tests, `../tools/atomize3.py`). FIFOL1, SizedFIFO and
RegFile are assumed leaves; FIFO2 is the library's; ConfigRegN is LastCaller's model.
