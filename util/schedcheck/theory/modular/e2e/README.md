# The end-to-end modules

Corpus modules' modular obligations, written by schedcheck (`regen.sh`) and checked by `../build.sh`:

| file | what | proved |
|---|---|---|
| `SysArithPipe.lean` | sysArithPipe: 227 rules, 83 children, 1,859 sites (690 task statements); ConfigRegN is LastCaller's model | `obl : OblU`, `acyc`, `top : RealizesU …` |
| `SysArithPipeNoTasks.lean` | the same without the `Tasks` child (`--no-task-agreement`) | the same |
| `SysArithPipe60.lean` | the first 60 rules and their run-time checks, the scan in 128-use segments (the segmentation regression test) | the same |
| `SysArithPipeSeg.lean` | sysArithPipe with the scan in 4 segments of at most 600 uses | the same |
| `MkBufferControl.lean` | mkBufferControl: 9 methods, 18 rules, 60 children, 303 sites; `lockInterLoads.write` is a last-caller port | `obl` from `Hyps` and `Unconfirmed` (one fact: `rdy_3`, refuted by schedcheck); no `Acyc`/`top` |
| `MkUrgencyBlocking.lean`, `modes8/MkUrgencyBlocking_{noagg,nosched,both}.lean` | mkUrgencyBlocking in the four bsc modes; `lastOp.write` is a last-caller port | `obl`, `acyc`, `top`, from `Hyps` alone |
| `MkMCR.lean` | mkMCR: 28 rules, 1 method, 44 children; 3 RegN and 1 ConfigRegN last-caller ports | `obl` from `Hyps` alone; no `Acyc`/`top` (a method's enable drives an RWire's path input) |
| `lcprobe/LcProbe64.lean` | 64 rules writing one register every cycle (last caller, 64 callers) | `obl`, `acyc`, `top` |
| `MkAssertedSchedules.lean` | mkAssertedSchedules: a `mutually_exclusive` pair (`up`, `down`) and a `conflict_free` pair (`emitA`, `emitB` both enqueue `outQ`), stated in `asmP` (`cfL_0`: the line with its instrumentation wire read as its writer's argument); the `outQ.enq` escape and mux facts under `asmP` | `obl`, `acyc`, `top`, from `Hyps` alone |
| `mtask/MethTask.lean` | a rule's `$display` (a statement, checked) and a method's (bsc G0020, left out) | `obl`, `acyc`, `top`; its six hardware mutants (`--mutants 6`, among them the rule's statement enabled always) fail |
| `mejoin/SysMeJoin.lean` | soundness review r8b-1's negative control: `MeJoin.bs` joins `r1`, `r2` against `r3` (`rJoinMutuallyExclusive`), so bsc's check line has two rules on its first side, stated as one check per rule of that side (until round 9b the emitter stopped) | `obl`, `acyc`, `top`; `a.write` is a last-caller port of `r1` and `r2`, as `--gate` finds (`regen.log`) |

`regen.sh IN` writes every file above (IN holds the compiled `.ba` files, laid out as the script
says) and logs schedcheck's lines to `regen.log`:

    schedcheck -o SysArithPipe.lean sysArithPipe.ba                     # the file
    schedcheck --no-task-agreement …                                    # no Tasks child (a weaker theorem)
    schedcheck --limit seg-uses=128 --first-rules 60 …                  # the first 60 steps, 128-use scan segments
    schedcheck --gate sysArithPipe.ba                                   # the gate only: GATE record and summary row
    schedcheck --time-phases …                                          # wall and CPU time per phase

Until round 9 these files came from the Python prototype (`tools/mke2e.py` with `sites/tools/`); schedcheck
writes them byte for byte except line 1 (`emitted by schedcheck.`), "schedcheck" in place of "the site
tool" in fact docstrings, and the gate line's byte count, which follows from those two.

schedcheck prints a `note:` line for each method whose task calls it leaves out of task agreement
(design.md §12).

**The design's:** children, kinds, schedules and paths; rules and methods with their reads, calls
(exact conditions) and the rules' task statements; each step's guard read at its position; every
port's users; exclusions (disjoint guards, urgency with several blockers, `mutually_exclusive` and
its run-time checks); its site conditions; escaped pairs and their dynamic facts, each
labelled by its source (the solver for that pair and the stated conditions, every port its own atom;
or bsc's fact for exactly that pair, with the stated conditions implying bsc's), each pair's check
keyed by first-step site and linear in its facts (C27); the last-caller ports (key: RegN/RegUN/RegA
and ConfigRegN `write`; two callers the solver finds may both call); bsc's `mutually_exclusive`
groups (one `SufAsm` per group), user assertions in `asmP` and listed in the first comment
(`conflict_free` checks with each instrumentation wire read as its writer's argument); the fan-in of
every expression.

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
