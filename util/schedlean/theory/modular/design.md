# Round 8: the modular proof

Each module bsc compiles gets a contract, `RealizesU`, stated over its own
interface. A module built from child instances meets it when

- every child meets its own contract and respects its output aliases, and
- the module meets a per-module obligation, `OblU`, which mentions the
  children only through their interfaces.

Composition is proved once (`compose`); induction over the instance tree
(`Built`) gives the top module's contract and a hardware cycle for every input
(`Causal`, from a decided acyclicity certificate `Acyc`).

```lean
theorem Built.top [Inhabited V] {X : Iface} {Mo : ModuleU V X} (h : Built X Mo) :
    RealizesU Mo ∧ ∀ s w, Mo.clean s = s → ∃ o s' g, Mo.hw s w o s' g
```

What round 8 changes (Ravi's decisions, the round-7 second reviews, three
pieces merged here, and the fixes for reviews r8-1, r8-2 and r8b-1):

- **Last caller** (§8). When two callers in one cycle call a self-SBR action
  port of a primitive child, the port carries the argument of the caller
  that runs last in bsc's execution order (bsc's priority mux). The
  certificate states this per port (`CertL`). What Lean checks is `lc_ok` on
  the child model's annotation; the law is carried by the model's proved
  `Realizes` (RegN family as `CReg 1`; ConfigRegN through the generic block
  lemma). ConfigRegN gets a library model, and Counter a model under a
  corrected annotation. bsc's AOpt-folded mux is covered too
  (`argL_of_pmuxF`). mkUrgencyBlocking is proved end to end in all four bsc
  modes, mkBufferControl's shadowing fact is gone, and mkMCR's obligation is
  proved.
- **Cost** (§15, §17). Exception groups are decided against one mask per
  group, every whole-table decision is split per subtree, tables have at
  most 128 entries per definition, and a module above 4 MB is emitted in
  parts. Since review r8-2 an escaped pair's table check is **linear in its
  facts** (bad site pairs keyed by first-step site, a one-pass partner check
  `subS`, proved in `EmitLemmas`/`Scan` and the file's `okT_exceptZ`):
  10.2–10.9 s kernel and 1.68 GiB at 4,096 trivial facts, where round 8's form
  ran out of memory at 2,048. Since review r8b-1 a first-step task statement
  is one comparison (C29; a pair was O(t1 × nT): 482.9 s and 18.40 GiB on
  sysConflictFreeOKLarge's shape, now 0.14 s), and the facts sit in groups of
  at most 128 fields (C30; one wide structure grew about ×5 per doubling), with
  at most 16,384 facts in a module. Every bound is asserted and written into
  the file, including 4,096 facts per escaped pair and 16,384 per module.
- **Soundness minors 1–5** (soundness review r7-2; §7). `'eq'` literals read
  site values; the tool and the file use one ready literal; every port is its
  own atom in every solver-labelled fact; bsc's facts are keyed by both steps
  and used only where the stated conditions imply bsc's; input and wire
  literals are dropped from readiness formulas. `oblU_weakensX` restates the
  weakening lemma for the files' form.
- **Task calls in methods are skipped** (Ravi's decision, §12). System-task and
  BDPI calls inside interface methods (bsc warning G0020) are left out of task
  agreement; task calls in rules stay fully checked. A left-out call must
  return nothing.
- **User assertions** (Ravi's decision, §5). bsc's `mutually_exclusive` and
  `conflict_free` run-time checks are both explicit hypotheses of the
  module's theorem (`asm`), assumed alike in gate items 1, 2 and 4, stated
  in an emitted file's `asmP` and listed in its first comment. Since round
  9b a file states `conflict_free` checks too (each instrumentation wire as
  its writer's argument) and `mutually_exclusive` lines with several rules
  on their first side, and a fact confirmed only with a `conflict_free`
  check assumed is stated under `asmP` (C32). Since review r8b-1 a `mutually_exclusive` line is read by its
  structure (`as × bs`, C31) in the emitter, the gate and the shadow tool
  alike, checked against the dumper's pair list. Six corpus designs rely on
  one (§11).
- **The gate** (§11) computes every item it reports from the emitter's own
  model. Since review r8-2, item 2 (every escaped pair's dynamic facts, call
  against call included) is the emitter's own check in its gate run; Counter
  is `ctrAnn`; a user BVI import takes its declared schedule. **26 of 36**
  corpus designs pass in the default mode, and 6 / 8 / 7 / 9 of the 10
  designs compiled in four modes. By the rules as stated before review r8-2
  the count was 24 (review r8-2); mkAssertedSchedules now passes on its
  assertions, as hypotheses, and mkUseOdd on its BVI import's declared
  schedule. Review r8b-1's rerun (check lines by structure, the new bounds)
  leaves every count and verdict unchanged.
- **Reviews r8-1, r8-2 and r8b-1** (§22): the gate as above, and since
  review r8b-1 it also computes the segment bound (`seg_uses`), the module's
  facts (`mod_facts`) and flags escape facts on footprint-only steps; `skel`
  measured with a repaired atomizer (§14, §15); the shadow tool one solver
  call per port, its input linear in callers plus their pairwise exclusions
  (a `mutually_exclusive` group is one at-most-one counter); ratios, memory
  and per-doubling factors as measured spreads.

§22 maps each review item to its fix. Round 7's design.md is
`../r7/design.md`.

## Files (all in this directory)

| file | what | lines | check (wall, user, peak RSS; `build/all.log`, the final build after review r8b-1, load 13.6–15.4) |
|---|---|---|---|
| `Modular.lean` | the model, `ParentU`/`ModuleU`/`OblU`/`CertU`/`RealizesU`, composition, `Acyc`, `Built`, primitives, task traces; `CertB`/`Cert`/`CertL`, `obl_of_certL_at`, `oblU_of_certL` | 4,896 | 16.3 s, 16.1 s, 0.79 GiB |
| `EmitLemmas.lean` | `Tab` (with `Tab.allB_node`), numbered site lists, per-port facts, `withSitesX`; the one-pass partner check `subS` (`subS_mem`, `mem_idxT`) | 587 | 0.9 s, 0.8 s, 0.51 GiB |
| `Scan.lean` | the scan and `pairFacts_of_scan`, chain kinds, rule codes, `SufAsm` groups, group masks (`exR_of_cmask`), escapes (`okT_except_fn`, and the linear `okT_except_fnS`; emitted files use their own `okT_exceptZ`, with C29's task-site comparison) | 2,440 | 4.2 s, 4.1 s, 0.63 GiB |
| `LastCaller.lean` | `CertL` from the scan (`PairFacts.toCertL`, `lc*_of_scan`), the priority mux (`argL_of_pmuxN`, and bsc's folded form `argL_of_pmuxF`/`NF`), the block lemma, `realizes_lastWins`, the reference key and laws, ConfigRegN and Counter models | 1,072 | 3.1 s, 3.1 s, 0.56 GiB |
| `LastCallerEx.lean` | last caller non-vacuous, and its negatives | 266 | 0.6 s, 0.6 s, 0.50 GiB |
| `SiteCond.lean` | weakened and dropped conditions; `oblU_weakens`, `oblU_weakensX` | 602 | 1.0 s, 1.0 s, 0.52 GiB |
| `Example.lean` | the toy hierarchy, negatives, task traces, `UOrder`, `RdyPort` | 3,425 | 24.3 s, 23.1 s, 1.02 GiB |
| `Scale.lean` | 4,096 children and rules indexed by `Fin n` | 170 | 0.4 s, 0.3 s, 0.48 GiB |
| `split/` | the toy as per-module files | 472 | ≤ 2.1 s wall each |
| `examples/` | `ScanEx`, `ScanExLate`, `ScanTasks`, `EqvTest` | — | ≤ 0.9 s wall each |
| `e2e/` | the files schedlean writes for the corpus (`regen.sh`, `e2e/README.md`), the method-task example and its negatives (`mtask/`), the 64-caller probe (`lcprobe/`), the `mutually_exclusive` negative control (`mejoin/`) | — | §13–§16 |
| `emit/` | `conventions.md` (C1–C31), the emission probes | — | `Em16` 5.3 s, 5.0 s, 0.57 GiB |
| `scan/` | scan benchmarks (`tools/genbench.py` and the generators that use `tools/scanlib.py`, which writes synthetic scan instances), the escape probes (`esc7/`; the linear check `escS/`: `genescS.py`, `run.sh`, `run.txt`; `escT/`: `insmarks2.py`, `mkhyps.py`) | — | §17 |
| `cost/` | the cost piece's probes and logs (`cost/logs/summary.txt`) | — | §17 |
| `lc/` | the last-caller piece's probes (`lc/probe/`, `shadow9/`), its shadow runs and logs | — | §8, §17 |
| `sites/` | `rule.md`, the site-condition rule | — | §5, §7, §8, §11 |
| `../../../../src/comp/Lean/Sched/` | the generator, schedlean: the model, site conditions, scan, facts, shadow classes, clock domains and gate (`Sites`, `Model`, `Scan`, `Facts`, `Shadow`, `Domain`, `Gate`), synthesized children (`Export`, `Hier`), the emitter (`Emit`, `Emit.*`) | — | §3.1, §5–§12 |
| `flat8/` | today's flat files' comparison set at one load (`run.sh`, `run.log`) | — | §14, §15 |
| `tools/` | copies of the dumper (`shape-final.bin`) and the task-agreement flat generator (`task-final.bin`); the repaired atomizer (`atomize3.py`, cost review r8-2) and its control (`cutonly.py`); round 5's `atomize2.py`, which does not match today's flat files | — | §14, §21 |
| `build.sh` | everything above in dependency order, with the negative `EscS_neg_8` (must fail), logged to `build/all.log` (MethTask's negatives are `schedlean --mutants`, run by `e2e/regen.sh`); `build_mbc.sh` rebuilt MkBufferControl after its header's group-width row was made exact (appended to the same log) | — | — |

Import order: `Modular` ← `EmitLemmas` ← `Scan` ← `LastCaller` ←
{`LastCallerEx`, generated files with a last-caller port or a ConfigRegN
child}; `Scan` ← the other generated files; `EmitLemmas` ← `SiteCond`. Core
Lean 4.35 only: no Mathlib, and no `sorry`, `admit`, `axiom`,
`native_decide`, `implemented_by` or `unsafe` in any file. Every Lean run:
`prlimit --as=25769803776`, `timeout`, `-j1`, `-DElab.async=false`, one
process at a time, on a shared machine (load is quoted beside each
measurement; the final build after review r8b-1 ran at load 13.6–15.4, the
comparison sets at 31–56 (`flat8/run.log`), 17–35 (cost review r8-1), 14–30
(cost review r8-2) and 13–17 (cost review r8b-1) on 32 cores, so ratios are taken between runs of one
period and quoted as ranges). Memory is peak RSS in GiB (kB / 1,048,576).
Kernel time is separated only in the escape probes (Lean's profiler, §17);
not in the e2e measurements (§21). Every `#print axioms` lists at most
`propext`, `Classical.choice`, `Quot.sound`, plus `bv_decide`'s axiom for
`Ex.Asm.skel` and what is built on it; `sorryAx` appears only in the
negatives (`EscS_neg_8`, `EscT_neg_8`, and schedlean's mutants).

## 1. The model and the contract

```lean
structure Module (V : Type) (X : Iface) where
  S R : Type
  meth : X.M → V → S → V → S → Prop
  rule : R → S → S → Prop
  clean : S → S
  Sig : Type
  hw : S → Wires X.M V → Outs X.M V → S → Sig → Prop   -- the Verilog's cycles
  ok : Sig → Prop                                      -- the assertions below hold
  L : Type; log : S → L → List (TaskCall V); trc : Sig → L → List (TaskCall V)

def RealizesU (Mo : ModuleU V X) : Prop :=
  ∀ s σ w o s' g, Mo.clean s = s → Legal X.ann σ → Drives X.ann w σ → Mo.hw s w o s' g → Mo.ok g →
    (∀ c ∈ σ.flatten, X.ann.alwaysRdy c.1 = false → o.rdy c.1 = true) →
    ∃ τ, groups τ = σ ∧ ∀ u : Mo.U, ∃ fin, Exec (Mo.at u) o s τ fin ∧ s' = Mo.clean fin ∧
      ∀ l, Mo.trc g l = Mo.log fin l
```

- The run `τ` is chosen before the skipped values of every module in the
  tree, then works at each assignment `u`. `RealizesU.at` gives the per-`u`
  `Realizes`; `realizesU_toU` lifts a module without skipped values (a
  primitive, a BVI leaf, a module off path A).
- `Realizes` covers the cycles in which the assertions hold (`Mo.ok`). It is
  existential over which rules fire; agreement on state, results and task
  calls is the content.
- **Task agreement.** Each module's own task calls are compared with that
  module's calls in the run; different modules' task traces are unordered
  against each other (the Verilog limitation across synthesis boundaries:
  each synthesized module's tasks run in its own always block; Bluesim
  should follow the full order). **Task calls inside interface methods are
  not compared** (Ravi's decision; bsc warns G0020 that system functions in
  methods have no order guarantee): generated modules leave them out of the
  `Tasks` child, and the docstrings of `Realizes` and `Built.top` say so
  (§12).

## 2. Families and the obligation

```lean
def OblU (F : ParentU V) : Prop :=
  ∀ oc w σ, F.asm oc w → (∀ j, AliasOK F.toB (F.CI j).ann (oc j)) → Legal F.X.ann σ → Drives F.X.ann w σ →
    (∀ c ∈ σ.flatten, F.X.ann.alwaysRdy c.1 = false → (F.comb oc w).2.rdy c.1 = true) →
    F.onehot oc w ∧ ∃ τP σC, groups τP = σ ∧
      (∀ j, Legal (F.CI j).ann (σC j) ∧ Drives (F.CI j).ann ((F.comb oc w).1 j) (σC j) ∧
        ∀ c ∈ (σC j).flatten, (F.CI j).ann.alwaysRdy c.1 = false → (oc j).rdy c.1 = true) ∧
      ∀ u, (∀ e ∈ τP, StepOK (F.at u) oc (F.comb oc w).2 e) ∧ ∀ j, sigmaJ (F.at u) oc τP j = σC j

structure CertB (P : Parent V)            -- every field of round 7's Cert but `arg`
structure Cert extends CertB P where arg : …   -- every call's argument on the wire
structure CertL extends CertB P where       -- last-caller ports
  lc : (j : P.I) → (P.CI j).M → Bool      -- self-SBR, with an enable (lc_ok)
  lp : Step P → Nat                        -- the order the scan keeps: methods first, rules by rank
  lpRR lpMR lcMR lcRR lcMM : …             -- callers of an lc port placed in lp order
  argN : …   -- lc = false: every call's argument on the wire (round 7's Cert.arg)
  argL : …   -- lc = true: the argument of a caller with no later (in lp) firing caller of the port
theorem obl_of_certL_at (C : CertL P) … ; theorem oblU_of_certL (F : ParentU V) (C : CertUL F) : OblU F
```

- `OblU` fixes the order `τP` and every child's call sequence `σC` before
  `u`; fixing `σC` is needed, not only `τP`, or a child's run, and the real
  value of a skipped read, could depend on `u`. `CertU` (rules, firing, hits
  fixed before `u`; `trU` checked per site) is as in round 7.
- `obl_of_certL_at` reuses round 7's placement (`place_ok`) with the
  invariant strengthened to keep every last-caller port's callers in `lp`
  order (`LcOk`). `obl_of_cert_at`, `obl_of_cert` and `oblU_of_cert` keep
  their names and statements and are now one-line derivations through
  `Cert.toL` (no last-caller port).
- `Ex.UOrder.not_oblU` rejects round 6's order counterexample.

## 3. Composition, `Built`, and modules off path A

```lean
theorem compose (F : ParentU V) (impl : ∀ j, ModuleU V (F.CI j)) (hC : ∀ j, RealizesU (impl j))
    (hA : ∀ j, Aliased F.toB (impl j).hwm) (hO : OblU F) : RealizesU (F.toModuleU impl)
inductive Built : (X : Iface) → ModuleU V X → Prop
  | leaf (Mo : Module V X) : Realizes Mo → Causal Mo → Built X Mo.toU
  | node (F) (impl) : (∀ j, Built (F.CI j) (impl j)) → (∀ j, Aliased F.toB (impl j).hwm) → OblU F →
      Acyc F.base → Built F.X (F.toModuleU impl)
```

A module off path A enters a modular parent as an assumed leaf
(`Built.leaf Mo hR hC`), with `hR : Realizes Mo`, `hC : Causal Mo` and, if
it exports aliases, `Aliased toB Mo`, all fields of an explicit hypothesis
record of the parent (the e2e files' `Leaves`), named in the top theorem's
statement. Today's flat theorem is the evidence for that hypothesis, but
Lean does not connect them, and it is no evidence for two calls of one method
in a cycle. BVI leaves enter the same way.

**A user BVI import** (neither a bsc library primitive nor a synthesized
module) is an assumed BVI leaf with its declared schedule, the user's own
assertion about the imported Verilog (Ravi's decision; soundness review r8-2
minor 3). The tools tell the three apart with three tests on the APackage and
the dump (soundness review r8b-1 minor 3):

- **synthesized**: bsc prints the instance's interface type with a trailing
  soft hyphen (U+00AD, `mkInterpolator.Interpolator­ = mkInterpolator`; bsc's
  `GenWrapUtils.genSuffix`);
- **user BVI import**: otherwise, when the interface type's package is not
  one of bsc's library packages (`src/Libraries`; `design.ba_bvi`,
  `shadow.ba_bvi`). bsc qualifies an import's interface type by the importing
  package even when the interface is a Prelude one (`BviLib._Prelude.Reg3`),
  so this test does not misclass it. The corpus's one is mkUseOdd's `OddPrim`;
- **library primitive**: everything else; which library model or generic
  kind it gets is decided by its Verilog module name against bsc's
  `src/Verilog` (`model.PRIMS`, `kind_of`).

The two tests agree on the corpus. A user BVI import whose Verilog module
carries a bsc primitive's name is still a user import: the emitter refuses
every user BVI import (no library model by name), and since review r8b-1 the
gate's `generic_kind` gives `ctrAnn` only to a library `Counter`, so such an
import keeps its declared schedule. Only a synthesized child gets "action
methods C with themselves" (below).

**An assumed synthesized leaf exports every action method C with itself**
(soundness review r8-1 minor 2), as generated modules do (`no_certL_sbr_self`).
bsc's exported `m SBR m` or `m CF m` never enters a Lean annotation: under bugs
`synth-sbr` and `synth-cf-share` it would make `hR : Realizes Mo` false
(`acc_not_realizes`), and `top` vacuous for every cycle. Since round 9b the
emitter admits synthesized children (§3.1): on path A through their own files,
else as assumed synthesized leaves; every other assumed leaf is a library
primitive (`ASSUMED_LEAF`: FIFOL1, SizedFIFO, RegFile, and ConfigRegN with
`E2E_CFGLIB=0`); `Built`'s docstring states the rule. The gate's scan run gives
a synthesized child's action methods C with themselves (its export, or the
parent's view of it, §3.1), and a user BVI import its declared schedule (§11).

### 3.1 Synthesized children: one file per module (round 9b)

A synthesized child on path A enters its parent as `Built.node`, through its
own generated file, instead of an assumed leaf.

**Files and order** (`Lean.Sched.Hier`). The modules of a run are the nodes of
the instance DAG: the top module's `.ba`, and for each synthesized instance
the `.ba` of its Verilog module, looked up beside the parent's `.ba`, then in
each directory of `-p`/`--search-path` in turn. Each module is processed once
per run, keyed by module name, however many instances use it: the DAG comes
from each `.ba`'s instances alone, and a module is read in full (extraction,
bsc's facts) only at its turn, its file written (and checked) before the next
is read, so memory stays at one module (cost review r9b-1 M2). The files are
written children first (a depth-first post-order in instance order), each
once: `<NS>.lean` in the top file's directory, namespace `NS`, where `NS` is
the module name capitalised (`mkCounter` → `MkCounter`), with its parts
`<NS>_p<k>.lean` as for any file. A parent imports each child file on path A
(`import MkCounter`, the last part). `--check` checks the files in that order,
each child's `.olean` on its parents' path, and every file's `#print axioms`;
`--mutants N` makes N mutants of each module, each checked against the real
children. A design with no synthesized instance takes none of this (its run
and file are as before).

**A child's exported contract.** A module written as a synthesized child (not
the top module) exports its whole interface in `X`:

- each action and action-value method: an enable, C with itself (§3: bsc's
  exported `m SBR m` / `m CF m` never enters a Lean annotation);
- each value method without arguments: no enable, one argument wire (`share`),
  CF with itself; its result is the method's value (the method is a step of
  the file, `nR + m`, where the top file keeps it a step without calls outside
  `X`);
- between two distinct methods, bsc's exported relation: CF; `a SB b` is
  `prec a b`; `a SBR b` also `sbr` both ways; C, or no statement, is `conf`;
- `alwaysRdy` exactly for the methods bsc builds always ready: the method's
  ready signal and its `CAN_FIRE` are the constant true (or `always_ready`).
  bsc's wrapper of the child assumes such a ready output is 1, so the parent
  never reads it (`true_ifc_ids` in `bsc.hs`). The file proves it: such a
  method's guard is the constant `true` (not an opaque operator) and its reads
  are always ready, so its `CAN_FIRE` is `true` in every cycle (`cf_ar_<m>`, by
  `rfl`), and `Avail` needs no ready output (`cf_of_avail`). Every other method
  keeps its ready output (`alwaysRdy` false);
- no aliases (`same`, `rdyRet`) and no paths: the file's `acyc` proves its
  outputs read no input (`Dwo := []`, checked against `xAnn.path` by `path_w`).

Not exported yet, so the child is off path A: a value method with arguments
(its result reads an input: a path, `Dwo`, and the argument in `retT`), and a
method bsc split into several steps.

**Not stated at all** (soundness review r9b-1 B1): an `always_enabled` or
`enabled_when_ready` action method. bsc runs it in every cycle (or whenever it
is ready), not on the parent's enable: the child's `.ba` has `WILL_FIRE_m = 1`
and no `EN` input, and a parent's Verilog drives no enable. A contract with
`hasEn` would make the child's file model `r := r + x` on `calledM w m`, false
in a parent whose rule calls `m` only sometimes (bsc only warns, G0015). So the
export is refused (the `.ba`'s `alwaysEnabled`/`enabledWhenReady` property, or
a `WILL_FIRE_m` that is not `m`'s enable port), the parent's view marks such a
method by its `inhigh` enable, and a parent using such a child stops, its gate
row naming the child (verdict not A). Fix path: export `alwaysEn m` (and
`hasEn m` false), and emit the parent's `alw` obligation (`CertB.alw`: a step
that is on in every cycle calls the port), which needs a proof that some rule
fires in every cycle. An assumed leaf whose `.ba` is absent is assumed to fire
on its enable (its `enabled_when_ready`, unlike `always_enabled`, is not in the
parent's view).

The relations are bits by method, one row per method in a balanced table
(`xAnnPrecT`, `xAnnSbrT`, `xAnnConfT`: `Tab.get` is logarithmic, where a list's
`getD` made a decided pass over all pairs cubic, cost review r9b-1 M1), and
again by columns (`xAnnPrecCT`, …, the rows' transpose): a pair is related
where both say so, so a parent's kind check reads only rows (below). Columns
that disagreed with the rows would only drop pairs from the relations, a
stronger contract the child's own file must then prove.
The scan's method tables say exactly where `Follow` and `Group` fail for every
argument (`Scan.mannExport`: a value method's own bit is clear, two calls
sharing one wire may follow each other), and the file checks each table
against `xAnn` by a decided lemma for any annotation (`nfB`, `ngB`, `h3B`,
`tab_spec`, `notFollow_of`, `notGroup_of`, `h3_of`), not only pairwise CF;
`selfAnnX` and `enV_ok` (a method without an enable calls no port with one)
are decided too. The top module keeps the old form (pairwise CF methods, else
the emitter stops), so a design with no synthesized child gets the same file
as before.

A module without rules (a child of methods only) gets one rule that never
fires (guard `false`, no sites): files index rules by `Fin nR` with `nR ≥ 1`,
and a contract's rules are existential, so the extra rule changes no
statement.

**The parent.** Each child module is one kind `synK` (numbered by first
instance), shared by all its instances: `KI .synK := NS.X`, its ports the
child's method indices (`NS.mM i`, so a port's code is `m.val`; wires and uses
by index, `finW`, `finU`), its annotation the child's export. The parent
states one record of named hypotheses, `Subs`, with per instance of a child
its operators, constants, solver verdicts, assumed leaves and own `Subs`
(`synKops`, `synKE`, `synKhy`, `synKlv`, `synKsub`, each a function of
`Fin (nk .synK)`). A child's file folds its inputs (method arguments other than
a bare pass-through, module argument ports) into its opaque operators, which is
sound for one module since `top` holds for every `ops`; two instances fed
differently (`a.incr 1`, `b.incr a.value`; `mkCtr 1`, `mkCtr 2`) need different
operators, so the entries are per instance, not per module (soundness review
r9b-1 M1). Each module has its own `Ops` and `EqC`: literal numbers are per
module. `top` holds for every `ops E hy lv sub`.

- `implK ⟨.synK, i⟩ := NS.modU (sub.synKops i) (sub.synKE i) (sub.synKlv i)
  (sub.synKsub i)` (a `ModuleU`; the library kinds become `Mo.toU`);
- `Built.node`'s child premise is `NS.built (sub.synKops i) (sub.synKE i)
  (sub.synKhy i) (sub.synKlv i) (sub.synKsub i)`, so `RealizesU` comes from the
  child's `obl` and `Causal` from its `acyc`, through `Built.realizes` /
  `Built.causal`;
- `Aliased`: the child declares no aliases, so `aliased_of_none` for every
  `toB`;
- `Acyc`: the child has no paths, so its kind's `rk_ok`, `reach_w` and
  `pin_all` cases are vacuous and its outputs have rank 0;
- the scan sees the child's ports with its exported relations, as any kind
  (`kind_synK` decides the scan's kind table against `NS.xAnn`); a rule's calls
  of one child are put in an order its annotation allows where the action
  order is wrong (bsc composes a rule's actions in parallel; `orderCalls`,
  which moves nothing where the order is right);
- a value read of a synthesized child whose ready output is bsc's constant and
  read by no step needs no readiness fact (`Sites.rdyPort`);
- a wide child costs the parent about linearly in its methods (cost reviews
  r9b-1 M1, r9b-2 M1): the kind table is checked by rows (`kind_synK` by
  `Scan.kindOK_of_rows`: for each port, its four table rows against bit masks
  of the ports that its annotation's rows and columns let follow or group with
  it, about n²/64 word operations in the kernel, where the pairwise check was
  O(n² log n) kernel steps); a port's facts are a balanced tree of proofs shaped
  as `fxT_synK` (`Tab.finAll`, each leaf at its port as the file writes it,
  `NS.mM k`, or `pSynK k` for an assumed leaf: a closed term), so no proof's
  type holds the other ports; the ports are `List.finRange n`
  (`portsU_synK` is `List.mem_finRange`, nothing decided); and the wires, uses
  and fixed codes are balanced tables (`finW`, `finU`, `fxT_synK`).
  Measured on Wide (rule i calls `put i` with `get (i+1)`; 32 to 256 ports,
  the parent's `.ba` 18 to 175 KB), Lean wall per file: off path A
  5.7/8.5/15.6/33.7 s, ×1.5–2.2 per doubling of the methods and ×1.9 per
  doubling of the parent's `.ba` at the last step, peak 0.61–1.16 GB (before:
  95.9 s and 4.3 GB at 256 ports); on path A 4.9/8.1/15.9/28.1 s (before:
  78.4 s). The child's own file (6.7/11.9/28.7/114 s) decides its method tables
  over every pair (`nfOK_*`), against a `.ba` that lists its schedule pair by
  pair (34 KB to 706 KB): ×1.8–2.3 per doubling of that `.ba`.

A parent's `top` holds under its path-A children's own assumptions (their
`Leaves` and `asm` reach it through `sub.synKlv` and `sub.synKhy`), so the
parent's `Leaves` docstring, its header's user-assertion line and its
hierarchy row name what each such child's file assumes: its assumed leaves,
its user assertions, and its own path-A children's (soundness review r9b-2
minor 3; testsuite `HierSf.bs`).

No lemma is missing from the library: `Built.node` takes built children
(`ModuleU`), and `compose`, `Built.top` are unchanged. Every file of the
round-9b tests lists only `propext`, `Classical.choice` and `Quot.sound`.

**Path A for a child in a parent** requires: its `.ba` is found and is one
clock domain; its file is written with `obl`, `acyc` and `top` from `Hyps`
alone (no `Unconfirmed`); the parent reads none of its ready ports (every
method the parent uses is always ready); and the parent's view of the
child's schedule (bsc's, in the parent's `.ba`) equals the child's export but
for action methods' self pairs, and its always-ready methods (no `RDY_m`, or a
`[const]` one) are the export's `alwaysRdy` ones (else the `.ba` files are out
of step, or `-unsafe-always-ready` made them differ). In
`--gate`, the child's own verdict must be A and its emitter's dry run must
write such a file.

**Off path A** the child stays an assumed synthesized leaf, named in `Leaves`
per instance (`synK : Fin (nk .synK) → Module V sX_synK`, `synKR`, `synKC`,
`synKA : ∀ i, Aliased toB (synK i)`; instances differ in their module ports),
with its interface stated in the parent from the parent's view of bsc's
schedule: every port a method, action methods C with themselves, value
methods CF, each `RDY_m` an always-ready value port aliased to `m`'s ready
output (`rdyRet m RDY_m`, as FIFO2's `notEmpty`), and `m` not always ready
unless its `RDY_m` is bsc's constant. The parent's emitter stops on a read of
such a leaf's value method with arguments (its argument is not modelled yet;
the gate's model takes it as a leniency, so its emitter column says no). `--gate` prints each module's record and rows
(children first) and one hierarchy row naming each child instance, its kind
and module, and, off path A, why. Fix path for the commonest reason, a parent
reading a child's ready port: export `RDY_m` as an exact port aliased to `m`'s
ready (`rdyRet`, `ParentU.exactRet`, `aliased_toModuleU`, as
`Example.RdyPort`); the child's file must then compute its ready value from
its reads, through its own children's aliases.

## 4. Values, operators, constants

One abstract `V`; each module's operators are a parameter (`Ops V`, with the
design's Bool reading `toB`); constants go through `EqC V` and its law;
value/value comparisons need transitivity (`EqvTest.no_trans`). bsc's solver
verdicts stay hypotheses (`Hyps ops E`).

## 5. Non-vacuity, assertions, parallel muxes

As round 7: `Acyc` ranks children's outputs (`causal_toModuleU`); assertions
are visible (`Mo.ok`); `Cert.onehot` assumes only `EnvC`. The consistency
check is not emitted. RST is not modelled.

**User assertions** (Ravi's decision). bsc's `mutually_exclusive` and
`conflict_free` run-time checks are both user assertions: explicit hypotheses
of the module's theorem, in its `asm` (so in `Env`), and the theorem covers the
cycles in which they pass (`Mo.ok`).

**A `mutually_exclusive` check** is a line that reads only `WILL_FIRE`
signals; a check step may carry several (sysFloatTest's `__me_check_403`).
Since soundness review r8b-1 (M1; `sites/tools/mecheck.py`, C31) it is read by
its structure:

- bsc's `mkMEAssump` asserts `aAnd (aOrs WF as) (aOrs WF bs)`. The line is
  `A && B`, each side one `WILL_FIRE` or a parenthesised `||`; a side has
  several rules when Prelude's `rJoinMutuallyExclusive` joins rule sets.
- It excludes exactly `as × bs`, nothing within one side.
- A line is accepted only when re-rendering `(as, bs)` gives it back; any
  other line of `WILL_FIRE` signals stops every tool.
- The emitter's model and the shadow tool both read lines this way and assert
  that the pairs equal the dumper's `mutually_exclusive (user)` list (trust
  point 3). They agree in all 76 dumps of the modes and the corpus.

Round 8 read the names only (the first as head, the rest as its list). For
`(WILL_FIRE_a1 || WILL_FIRE_a2) && WILL_FIRE_b` that gave `!(a1 ∧ (a2 ∨ b))`:
an exclusion `(a1, a2)` no one asserted, and `(a2, b)` dropped. The
reviewer's `e2e/mejoin/` (r1 and r2 joined against r3) is the negative
control: the emitter now stops, and the gate's run finds `a.write` a
last-caller port of `r1` and `r2`, as the shadow tool does. All 2,538 corpus
lines have one rule on their first side, so no count changed.

**A `conflict_free` check** is any other line, inside a rule `R`: a site-level
assertion `!(WILL_FIRE_R && line)`, in which each of bsc's instrumentation
wires is read as its one unconditional writer's argument (`wget`) or
`WILL_FIRE` (`whas`), as the dump's `wire: …$wget = …$wset_1 within the cycle`
states. mkAssertedSchedules' check in `RL_emitB`, `WILL_FIRE_RL_emitA &&
limit_EQ_9___d12 && _enq_RL_emitA$EN_outQ.wget`, reads as "`emitA` and `emitB`
never both enqueue".

- In an emitted file the `mutually_exclusive` checks are `asmP` (groups as
  `SufAsm`), and the file's first comment lists them as assumed. A line
  with one rule on its first side is one check; each stated check,
  re-rendered, is asserted to be bsc's line. Since round 9b a line
  `(WF a₁ || …) && B` is stated as one check `(aᵢ, bs)` per rule of its first
  side, which together are exactly the line (`!((a₁ ∨ a₂) ∧ B) ⟺ !(a₁ ∧ B) ∧
  !(a₂ ∧ B)`); the emitter asserts that a line's checks have its first side as
  heads, one list, and re-render together to bsc's line, and keeps them out
  of suffix groups. The check step's guard is the whole line,
  `orL [WFd_a₁, …] && orL [WFd_b, …]`.
- **A `conflict_free` check in a file** (round 9b, C32). Check `c` in rule
  `R` is stated in `asmP` as `(WF R && cfL_c) = false`: `cfL_c` is the line's
  top-level conjuncts, each a `WILL_FIRE` signal (`WF k`) or a condition over
  one step's reads, with each instrumentation wire's `wget` read as its one
  unconditional writer's argument (the writer's call condition, over the
  writer's reads) and `whas` as the writer's `WF` (trust point 3). bsc's line
  is `aAnds [WILL_FIRE_r2, c1, wget]`, so `cfL_c` is `WF r2 && c1 && c2`; a line
  with a firing signal or a wire below the top level (through definitions)
  is not stated, and the emitter stops. bsc's `$error` that reports the
  check is a task statement of `R` whose site condition is `false` (the step
  never makes it) and whose hardware enable is `WF R && cfL_c`; its task
  agreement holds because `asm` makes that enable false (`en_tcf`, `cfA_c`).
  The instrumentation wires are children like any `RWire` (their `wset`
  calls are sites of the rules that write them), read by nothing. A dynamic
  fact the solver confirms only with the `conflict_free` checks naming
  either step assumed (`assert`: an escape fact, or since round 9b the
  exclusivity of two callers of one port, `cx_n`) is stated with an
  `asmP ops E oc →` premise, so `Hyps` holds of every `oc` (soundness
  review r8b-1 minor 1); the solver's assumptions are the file's, since the
  emitter states every `conflict_free` line or stops.
- In the gate both kinds are assumptions in items 1, 2 and 4 alike (§11), and
  the summary lists every design and pair that relies on one. A
  `mutually_exclusive` line enters the gate's scan as the `|as|` checks
  `(a, bs)`, which together are exactly bsc's line.

## 6. The headline theorem of a generated module file

As schedlean emits it:

```lean
import LastCaller          -- `import Scan` when the module has no last-caller port and no ConfigRegN child
abbrev F (ops : Ops V) (E : EqC V) : ParentU V := …
theorem F_sites (ops E) : F ops E = withSitesX (F ops E) ssM resM ssR resR := rfl   -- each step's own skipped values
structure HypsG0 (ops E) : Prop where rdy_n … inv_n … esc_n … cx_n …   -- at most 128 facts, each labelled by its source
structure Hyps (ops E) : Prop where disj … tob … g0 : HypsG0 ops E … gK : HypsGK ops E
def certU (ops E) (hy : Hyps ops E) : CertUL (F ops E)                              -- CertU without lc ports
theorem obl [Inhabited V] (ops E) (hy : Hyps ops E) : OblU (F ops E) := oblU_of_certL _ (certU ops E hy)
def acyc (ops E) : Acyc (F ops E).base
theorem top [Inhabited V] (ops E) (hy) (lv : Leaves V ops.toB) :
    RealizesU ((F ops E).toModuleU (fun j => (implK ops lv j.1).toU)) ∧ ∀ s w, … → ∃ o s' g, … := (built …).top
```

- Each fact in `Hyps` carries the verdict for the exact formula the file
  states, and is labelled by its source: readiness and invariance (the site
  tool, site-value form, §7); an escape (the solver for that site pair and
  the two sites' stated conditions, every port its own atom; or bsc's fact
  for exactly that pair, used only where each stated condition with `WF`
  implies bsc's condition with the step's guard); caller exclusivity at a
  mux (the solver over the users' call conditions, or bsc's fact as above).
- A fact that is not confirmed goes into `Unconfirmed ops E`, an explicit
  argument of `obl` and `top`; such a module is off path A
  (mkBufferControl, §14).
- A ConfigRegN child uses LastCaller's model: `cfgAnn_lib : cfgAnn =
  LC.cfgAnn := rfl` checks the instance's schedule against the library's.
- **An escaped pair's table check is linear in its facts** (cost review r8-2
  M1). The pair's bad site pairs are a search tree keyed by first-step site
  (`escB_n : Tab (Nat × List Nat)`, leaves `(i, i2s)`, at most 128 entries per
  definition); each site of the first step, against each port of its child,
  either may follow (`okP`) or has every use of that port in the second step
  among its own partners, checked in one pass over both ascending lists
  (`Emit.subS`, `subS_mem`); the decision is one per subtree of the step's key
  table (`escChkS_n`, as C20). `okT_exceptZ` (in the file) and
  `Scan.okT_except_fnS` derive `OkT` from it and one fact per bad site pair
  (`Tab.All` over `escB_n`). Round 8's form looked each failing pair up in a
  literal list of all of them, so its decision was quadratic (out of memory at
  2,048 facts, §17).
- **A first-step task statement is one comparison** (cost review r8b-1 M1,
  C29). Statements are numbered in rank order, so every statement of the
  earlier rule is below `lo`, the later rule's first statement. A first-step
  site passes on `isT ∧ tnum < lo` before C27's loop over its child's ports,
  and one more decision per subtree of the second step's key table checks
  that its task sites are at least `lo` (`escT2S_n`); `okP_task` gives `okP`
  for two task ports in number order (`taskAnn`). Before, a task site looped
  over the `Tasks` child's `nT` ports, so a pair cost O(t1 × nT) (t1 task
  statements in its first step): sysConflictFreeOKLarge's `RL_r2 → RL_r3`
  shape took 482.9 s of kernel time and 18.40 GiB (cost review r8b-1). The
  other kinds keep the loop over their ports, a library kind's constant
  (`Scan.okT_except_fnS`, the library form the `scan/escS` probe uses, keeps
  the loop over every port: its children are `Reg`s). Negative:
  `scan/escT/EscT_neg_8.lean` (`lo` one too high) fails at `escT2S_0`.
- **Facts in groups of at most 128** (cost review r8b-1 M2, C30). `Hyps` and
  `Unconfirmed` hold their facts in sub-structures `HypsG<k>` /
  `UnconfirmedG<k>` of at most 128 fields, with one field `g<k>` per group in
  the top structure; a fact is used as `hy.g<k>.esc_n`. One structure
  declaration grows about ×5 per doubling of its fields (503 s at 4,096 fields
  in the review's probe), so none is wider than 128; a module has at most
  16,384 facts (128 groups), asserted, written into the header and counted by
  the gate (§11 item 6).
- The file's first comment lists the methods whose task calls are left out
  (§12) and the user assertions it assumes (§5: its `mutually_exclusive`
  groups and single checks). The gate's bounds and the module's values are
  written into the file (`/-! Gate bounds … -/`), including the escape bounds
  (64 escaped rule pairs, 4,096 facts in one escaped pair), the fact bounds
  (16,384 facts in the module, 128 fields in one group) and the three table
  settings `E2E_SPLITN` (128 entries), `E2E_SPLITW` (20,000 characters) and
  `E2E_BIGPORT` (8 users), printed as "setting used", not as a module value;
  each is asserted so that a variable may lower it and never raise it.
- The emitter asserts one site per port in each step (bsc's dump gives each
  copy of a method its own port; corpus maximum 1). The step index check
  (`hchkT`, a `contains` per site) is linear under it and quadratic in sites
  of one step on one port without it (§17, the fan probe).
- `Leaves`' docstring names the assumed kinds the module actually has, or says
  there are none (soundness review r8-1 minor 7).

## 7. Site conditions

A step is a list of sites (child, port, condition and argument over the
read environment `ρ`); `runSites v … v` makes it a program over the step's
own skipped values `v`. The rule:

- **Action calls**: their exact call conditions, never weakened. Task calls
  in rules are action calls on the module's `Tasks` child.
- **Value reads: unconditional**, unless needed: (a) an out-of-order read;
  (b) a shared copy two firing steps read with different arguments (none in
  the corpus); (c) the earlier read of a read flag (none); (r) readiness: the
  port has a ready signal the step's guard does not imply.
- **(a), (b), (c)**: schedlean's conservative reach (`&&` and `||` count both
  operands; only an arm of an `if`, `case`, mux or selection is
  conditional).
- **(r), readiness** (decision b): `&&` and `||` are sequenced left to right
  by expression structure. The read's condition is its **ready literal**,
  read before it.
- **Site values.** A literal of the readiness formula `R` that reads a
  conditional read `y` of the same step reads `y`'s site value `ite(cond_y,
  port_y, fresh)`, as the model does. The facts, checked by cadical through
  the site tool (`sitecond.py` `rdy_site_check`, encoder `satc.EncS`), are
  readiness (`guard ∧ R(site values) ∧ ¬RDY` UNSAT) and, where `R` reads
  another conditional read, invariance (`guard ∧ R(site values) ≠ R(ports)`
  UNSAT; `inv_self` derives it where `R` reads only the site itself).

Round 8 closes four latent gaps in the tools that label facts confirmed
(soundness review r7-2 minors 1–4; none changed a verdict or a count in any
mode):

1. **`'eq'` literals** (case arms, array selections) now read site values:
   `satc.EncS` overrides `eqatom` as it overrides `b`.
2. **One ready literal.** The tool takes the literal the file states (the
   first ready port the step reads) both as `cond_y` and as the conclusion,
   and checks with `notEmpty` and `i_notEmpty` as separate atoms
   (`Design.exact()`), because the file's facts carry no `AliasOK`. The
   emitter's escape and caller-exclusivity checks also keep every port its
   own atom; so does the last-caller detection and the shadow tool.
3. **bsc's facts keyed exactly** (`BscFacts.covers`): both steps and both
   uses; a chain covers every earlier use of its port; a text also listed
   UNCONFIRMED covers nothing. A bsc label also needs the file's `WF` and
   stated conditions to imply bsc's guard and use condition (a second latent
   gap; on mkBufferControl 7 of 50 escape facts would have been mislabelled
   had the solver failed on them). Each fact's docstring names its source.
4. **Input and wire literals in `R`** are dropped (a monotone weakening) and
   the formula re-checked; `condsites.py` asserts none remain in any `R` or
   (a) condition a file states (corpus: 0 in every mode).

Negative controls through the real `analyse()` (`sites/modes8_min/neg_*`):
round 7's tool confirms 4 false readiness facts (`eqcase`, `eqsel`,
`alias`, `hwlit`), the new tool none; the positive controls hold under both.
The key check gets 9 of 9 cases right; round 7's key got 4 wrong.

**Readiness, site-value form** (`gate8/sites_*`, the merged tool):
24 readiness sites in the default and `-no-sched-conditions` modes, 22 with
every fact confirmed (7 needing invariance, all hold); the same two fail safe
(mkBufferControl `RL_inputing / shortTermPicList_returnList.first`,
mkPrediction `RL_outputing / outstatefifo.first`). The other two modes have
no readiness sites. Every new counter (input/wire literals, `'eq'` literals,
sites with no ready literal) is 0 in every mode.

**Why a skipped read is sound.** `OblU` fixes the order and every child's
call sequence before `u`, so the every-assignment argument applies: set each
skipped value to what the real read returns at its position in the fixed run.
A condition that reads a skipped value and changes the calls, or a call
argument that depends on one, fails `OblU` (checked per site by `trU`). This
step from the model to the real design is an argument outside Lean; it
assumes reads at skipped sites are side-effect free.

**Every condition is checked.** A file's conditions need no soundness lemma.
`SiteCond.oblU_weakensX` (the files' `withSitesX` form; `oblU_weakens` for
`withSites`): weakening keeps every step's result and adds calls only, so it
costs completeness. Neither lemma is used in a proof.

**In the e2e files** a use of a conditional read is split on its reach,
`bif R then f(fan-in) else f'(fan-in without the read)`; for (a) `R` is the
read's own condition, so a too-strong (a) condition cannot fail there (trust
point 1); for (r) it is `R_seq` over the site values, with `rdy_n`/`inv_n`.

**Conditions over hardware wires** (sysFloatTest): not expressible in the
site model; its 10 facts stay at risk in the default and
`-no-aggressive-conditions` modes.

**Decisions still to confirm** (soundness review r7-2 minor 6): the
site-value form for cyclic readiness literals replaced the decided
whole-literal dropping (dropping remains for later-site literals), and the
wider orientation search at the two FIFO2R sites was not done. Both are
recorded here for Ravi's explicit agreement; both sites fail safe either way.

## 8. Last caller (Ravi's decision: primitives only)

**What bsc builds.** When two callers of one child action port may fire in
one cycle, bsc drives the port through a priority mux (`AState.mkEmux`,
`PrimPriMux`): the arms are in reverse execution order, so the first arm is
the caller that runs last; each arm's selector is `WILL_FIRE ∧ call
condition`; the enable is the OR of the selectors; the default is an
unspecified value. Only a self-SBR port can have two callers with different
arguments in one cycle (bsc's resource rule gives a self-CF or self-SB port
one use; two different uses are G0002). The order is the execution order
schedlean already reads; urgency never decides it (bsc drops urgency edges
between rules that can fire together). Checked on all 37 corpus pairs in the
default mode and 31 without aggressive conditions, against bsc's Verilog and
G0117 warnings (`../r8-lc/order.md`, `verify-order.md`).

Ravi's framing: a self-SBR method is "infinitely ported like a value method
of no arguments, with last one wins for writes". It is built as a fan-in step
in two parts, both proved once.

**(a) The block lemma** (`LC.block`, from the annotations alone). In any
legal sequence, every call between two calls of `m` is of a port that cannot
observe `m` (`Obs`: ordered with `m` either way, or in conflict with it, is
false, so the port is CF with `m`). The calls of `m` form one block for every
port that can observe them. One caller never calls a self-SBR port twice
(`block_group`). CF ports may sit inside the block (ConfigReg `read`,
McpRegUN `get`, FIFO `i_notEmpty`/`i_notFull` against `clear` read the
start-of-cycle value) and commute out (`CommCF`, `comm_left`, `comm_right`).
`execG_rbl` puts the earlier calls back from the law, and
`realizes_lastWins` turns a contract with `m` called at most once a cycle
(`RealizesA`, `m` C with itself) into `Realizes` with `m` SBR `m`.

**(b) Per-primitive laws.** What Lean checks is `CertL.lc_ok` on the child
model's own annotation (`m` self-SBR with an enable). A last-caller port is
sound because that model's `Realizes`, with `m SBR m`, is proved: the law
`m(a); m(b) = m(b)` is carried by the model's contract (`reg_realizes` for the
RegN family as `CReg 1`; `LC.cfg_realizes` for ConfigRegN, through the block
lemma and `cfg_lastWins`). `lcKey` (self-SBR with an enable, not self-C, not
shared, and no port that observes `m` after it in one caller) and the
`LastWins` lemmas are **reference facts**: no proof uses them except
`cfg_lastWins`. The shadow tool applies `lcKey` to each instance's schedule
(below); the emitter keys by kind.

| primitive | model (its annotation) | port | law (reference) | wired in the emitter |
|---|---|---|---|---|
| RegN, RegUN, RegA | `CReg 1` (`cregAnn 1`: `read SB write`, `write SBR write`) | `write` | `creg_lastWins 1 0`, `reg_key` | yes |
| ConfigRegN/UN/A | `LC.CfgReg` (`cfgAnn`, checked against the instance by `cfgAnn_lib`) | `write` | `cfg_lastWins` (used), `cfg_comm`, `cfg_key` | ConfigRegN only (the emitter has no kind for UN and A) |
| CRegN5/UN5/A5 | `CReg 5` | `port k` write | `creg_lastWins 5 k`, `creg5_key` | no |
| RWireSBR | `RWire _ true true` | `wset` | `rwire_lastWins` (last value; `whas` = OR) | no |
| mkPulseWireOR (RWire0SBR) | `RWire0 _ true true` | `send` | `pulseWireOR_lastWins` (`send; send = send`, the special case Ravi named) | no |
| FIFO2 | `FIFO2` | `clear` | `fifo2_clear_lastWins`, `fifo2_clear_comm` | no |
| Counter | `LC.Counter` (corrected annotation) | `setF` | `counter_setF_lastWins` | no |

**Why the emitter needs no schedule check for RegN, RegUN, RegA** (soundness
review r8-1 minor 1). The emitter maps the three to `CReg 1` by name. The
Lean annotation of the child is the model's (`cregAnn 1`), and `OblU`
requires every child's call sequence to be `Legal` for that annotation, which
the scan decides. RegN.v, RegUN.v and RegA.v are the same `if (EN) Q_OUT <=
D_IN` whatever schedule bsc imported them with. So an instance whose bsc
schedule is looser than `cregAnn 1` makes the proof fail (fail safe), and one
whose schedule is stricter (AlignedFIFOs' RegUN with `write C write`) is still
modelled correctly. ConfigRegN is checked (`cfgAnn_lib`) because its model's
annotation was written from ConfigReg.bs, not from a Verilog-independent fact.

- **Counter.bs** declares `addA`, `addB` CF with themselves and `setC` CF
  with the adds; both are wrong (bsc bug `counter`: identical `up` calls
  merge onto one port, Verilog adds once; `setC` is applied before the adds).
  The library model `LC.Counter` uses `addA C addA`, `addB C addB`, `setC SB`
  the adds, `value SB` every action, and every action SBR `setF` (as
  Counter.bs: `setC CF setC`, `addA CF addB`): `counter_realizes` (for a
  right-commutative add) and `counter_causal`. `counter_addA_not_key`. The
  gate's scan gives Counter the same relations (`model.CTR_SCHED`, §11).
- **ConfigRegN** model `LC.CfgReg`: state = current and cycle-start value, a
  read returns the start value; `cfg_realizes` (through `realizes_lastWins`
  from `cfg_realizes1` with `write C write`) and `cfg_causal`.
- **Not covered:** the Unsafe wire variants (`wset SB` reads in one caller:
  `unsafeRWireSBR_not_key`, `unsafeRWire0SBR_not_key`). The emitter has no
  wire kind in `LC_KEY` and the shadow tool classes them `U`, so a design
  using one with two callers fails to prove (`cert_two_args`). Accepted
  (Ravi). Lean's `lc_ok` alone would admit one, and soundly, since the
  Unsafe wire's model realizes its contract (soundness review r8-1's
  `unsafe_lc_ok` probe); the restriction is the decision's, not a soundness
  need.
- **Synthesized children** stay off path A when two callers call one of
  their exported self-SBR methods in a cycle: bsc's exported `m SBR m` is no
  guarantee (bug `synth-sbr`: bsc derives SBR when the argument reaches a
  self-CF port, a task or a BDPI call, and the earlier call is lost;
  `synth-cf-share` the same for self-CF methods with foreign calls).
  `no_certL_sbr_self`: a generated module still exports every method C with
  itself. The child-side proof of the law is future work.

**The parent side** (`CertL`, §2; `LastCaller.lean`):

- `lp` is the scan's order (methods before rules, rules by rank; `lpOf`).
  Each last-caller port gets one virtual scan slot of a kind with `cout 0 =
  [0]` in every caller's footprint, so the unchanged scan keeps the callers
  in rank order: `lcMR_of_scan`, `lcRR_of_scan`, `lcMM_of_scan`, from
  Scan's own `h1_static`, `h2_static` and `mm_static_out`.
- `argL` from the priority mux (`argL_of_pmuxN`): one selection fact per
  position, the mux order one `incB` decision and one list equality, so the
  check is linear in callers.
- The per-port check is conditioned on which callers may fire together: a
  port is last-caller only where the solver finds two callers that may both
  call (every port its own atom); exported method relations count
  (mkRenamedPorts' `a C b` is not shadowing).

**In the emitter** (C25): `LC_KEY` is RegN/RegUN/RegA and ConfigRegN/UN/A
`write` (since round 9b ConfigRegUN and ConfigRegA take the `cfg` kind); the other laws are proved but not wired (the corpus does not need
them). A port is a last-caller port when the solver finds two of its callers
that may both call (every port its own atom), skipping pairs that a recorded
exclusion covers: urgency, disjoint guards and run-time checks between rules,
and, since review r8-1 (cost minor 3), bsc's disjoint guards and urgency
between a rule and a method; in the gate's run the `conflict_free` checks
naming either caller are assumed (§5). That choice cannot weaken anything: a port that
is not last-caller needs every call's argument on the wire (`argN`), which
needs its callers proved exclusive. A method caller on a last-caller port is
asserted absent (the theory covers it: `lcMR`, `lcMM`). A method executing
after a rule on such a port cannot be certified with this `lp` (fails safe;
the shadow tool marks it `L!order`; the corpus has none).

**bsc's optimized mux** (soundness review r8-1 minor 6). AOpt folds the
lowest-priority arm of a `PrimPriMux` into the default (`lastOp$D_IN = go ?
2'd1 : 2'd2` in mkUrgencyBlocking.v), so that arm has no selector.
`argL_of_pmuxF` and `argL_of_pmuxNF` cover that form: arms `0 … u-1` with
selectors, and the earliest caller's argument as the default. The proof uses
the selector-implies-fires fact only for arms above the winner, never for the
last arm. The prototype's netlist is synthetic and uses the unfolded mux
(`argL_of_pmuxN`); a generator reading bsc's netlist would use the folded
form.

**Shadow classes** (`sites/tools/shadow.py`, the one shadow tool; per port
since review r8-1). For each child action port with an argument and two
callers, one solver call (`two_call`) asks whether two callers with no
recorded exclusion between them may both call: the recorded exclusions
(disjoint guards, urgency, bsc's exported C between two methods, and the
user's `mutually_exclusive` pairs) are facts, so they enter as clauses, every
`conflict_free` check naming a caller is assumed (§5), and "at least two
callers" is a sequential counter, linear in callers. Since cost review r8b-1
(minor 1) the exclusions come from each caller's adjacency, and a
`mutually_exclusive` group (bsc's suffix chain of lines, `mecheck.suffix_groups`,
as the emitter's `GROUPS`) is one at-most-one counter, so **the call's input
is linear in callers plus the pairwise exclusions recorded among them**
(disjoint guards, urgency, and `mutually_exclusive` lines outside a group);
before, it was one clause per excluded pair, quadratic in callers for a group.
Measured on the call alone (`sites/tools/shbench.py`, one group of all n
callers, UNSAT; two runs at load 13, `sites/shbench/run.txt`): 3,296 callers
52,720 clauses, 0.95–0.96 s wall; the old encoding 5.4 million clauses and
12.4–12.6 s at 1,648 callers (0.8 GB). The tool's outputs
are unchanged in every mode. The port's class comes
from the child:

- `L`: the (primitive, port) is on the decided list above, with `m SBR m`
  and `lcKey` on **the instance's schedule**. One pair that may both call
  makes it a last-caller port of all its users. The order condition is then
  static, per pair: two methods must be exported SBR in execution order (SB is
  refused: one grandparent caller could call both, and `CertL` needs SBR or
  C), and no method may execute after a rule on the port (else `L!order`,
  for a pair that may both call).
- `U`: self-SBR outside the decided list, or failing `lcKey` on the
  instance's schedule (the Unsafe wires; Probe, RevertReg, McpRegUN).
- `I`: self-CF or self-SB with identical calls. No Lean model has such a
  port, so `I` is blocked; `I-fail` names Counter's `addA`/`addB`.
- `S`: a synthesized child, whatever its exported self relation. A user BVI
  import is classed by its declared schedule as a primitive is, but never
  `L` (the decided list is of bsc primitives): a self-SBR port with an
  argument is `U`.
- `X`: anything else (a self-C port both called).
- `?`: the solver timed out on the port's first pair (unknown; blocked).

So the tool makes one solver call per port with two callers, plus one per
pair with a bad static order on an `L` port, plus two on a port where an
assertion is needed (to count it). Round 8's first tool made one call per
pair (2,016 / 8,128 / 32,640 at 64 / 128 / 256 callers, and a crash at 512 on a
solver timeout); review r8-1's stopped at the first pair that may both call,
still one call per pair on a port whose pairs are all exclusive (46,344 calls
on sysFloatTest once its `mutually_exclusive` pairs are counted; cost review
r8-2 minor 4). A timeout is unknown in every tool (`satc.Enc.sat` returns
`None`, which no caller reads as UNSAT). Gate item 4 is "no port outside
`L`".

## 9. Emission conventions

`emit/conventions.md` (C1–C31). Round 8 adds:

- **C20** exception groups decided against one code mask per group (`cmT`,
  `exR_of_cmask`); every whole-table decision one per subtree
  (`Tab.allB_node`).
- **C21** at most 128 entries per table definition; per-kind arm tables
  split, `eaAll`/`dqAll` follow them.
- **C22** `certU.rules := (List.range nR).map rR`; `RD` irreducible.
- **C23** a module above 4 MB in parts of about 2 MB, each importing the
  one before; the last part holds `obl`, `top` and the axioms.
- **C24** the gate's bounds asserted and written into the file; an
  environment variable may lower a bound, never raise it. This covers the
  escape bounds (64 pairs, 4,096 facts in one pair) and the table settings
  `E2E_SPLITN`, `E2E_SPLITW`, `E2E_BIGPORT` (printed as "setting used").
- **C25** last-caller ports: the priority mux table, virtual slots,
  `CertL` (§8).
- **C26** task calls inside methods left out, with a note (§12).
- **C27** (review r8-2) an escaped pair's bad site pairs keyed by first-step
  site, each site's partners in one pass (`subS`), decided per subtree:
  linear in its facts (§6, §17).
- **C28** (review r8-2) the user assertions a file assumes, in its first
  comment; a `conflict_free` check stops the emitter (§5).
- **C29** (review r8b-1) a first-step task statement of an escaped pair is one
  comparison against the second step's first statement (`isT`, `tnum`,
  `okP_task`, `escT2S_n`), independent of `nT` (§6, §17).
- **C30** (review r8b-1) facts in groups of at most 128 fields (`HypsG<k>`,
  `UnconfirmedG<k>`, uses `hy.g<k>.…`); at most 16,384 facts in a module (§6,
  §11, §17).
- **C31** (review r8b-1) `mutually_exclusive` lines read by structure
  (`mecheck`), one rule on a stated line's first side, each stated check
  re-rendered to bsc's line (§5).
- **C32** (round 9b) `conflict_free` checks in `asmP` (`cfL_c`, `cfR`, `cfT`,
  `nF`; `asmP`'s single checks are then `.2.1`, its `conflict_free` part
  `.2.2`); their reports' statements under `asm` (`en_tcf`, `cfA_c`); facts
  confirmed only with them assumed under `asmP`; a `mutually_exclusive` line
  with several rules on its first side as one check per rule of it (§5).

## 10. versus today's emitted file

As round 7: exported methods (results checked against the module's outputs,
`resM`), escapes with their dynamic facts, caller exclusivity at muxes
(`cx_n`), and now last-caller muxes. `fail_if_success` is still not emitted.
**bus_2m_2s** stays off path A on the flat theorem (decided): its own
obligation fails for the 16 exported CF pairs bsc records as unrealizable.

## 11. The scan, escapes, and the gate

`pairFacts_of_scan` builds every pair field of the certificate from a
kernel-decided, checkpointed scan (`D.Ok`) and linear glue (`GlueOK`); the
last-caller order facts come from the same scan through one virtual slot per
last-caller port (§8). Rule codes give code 0 to every rule in no exception
group or escape; group membership is a code-mask lookup (C20). Escapes carry
one dynamic fact per statically failing site pair, checked linearly (§6,
C27).

**The gate.** Path A, for a module in a given bsc mode, requires all of
the following. `schedlean --gate` computes each from its own model
(`Lean.Sched.Gate`, with `Facts`, `Sites`, `Shadow`) in the mode the `.ba`
was compiled in and prints the `GATE` record, a `counts` row and one summary
row with the failing items. (Until round 9 the Python prototype computed
them: `mke2e.py` under `E2E_GATE=1`, the site and shadow tools,
`gatecount.py` and `gate8/summary.py`; schedlean reproduces their records and
rows, see *Round 9* below.)

1. **the emitter's own scan passes** (`grp`, `mm`, `mmg`, `h3`) with at most
   **64 escaped rule pairs**. The verdict is the emitter's: the gate builds
   the emitter's model, runs its scan (`Lean.Sched.Scan`, the mirror of
   `Scan.lean` that every emitted file's scan is checked against), computes
   item 2 and prints the `GATE` record; it also runs the emitter in memory
   and reports whether it writes a file. So that the scan can be run on designs the prototype
   cannot yet emit, and only in this run:
   - a child with no library kind gets a **generic kind** from its dump
     schedule: every method and copy a port, copies of one method CF; a
     synthesized child's action methods C with themselves (§3); a user BVI
     import, an assumed BVI leaf, its declared schedule (§3; mkUseOdd's
     `OddPrim`, `c SBR c`, so `RL_rc` and `RL_rd` both call `o.c` with no
     escape); Counter LastCaller's `ctrAnn` (`model.CTR_SCHED`, §8);
   - the module's own annotation is bsc's exported relations (CF, SB, SBR,
     the rest C), which the theory admits and the emitter cannot yet write;
   - a step the site builder cannot build (a fan-in through a call with
     arguments, an (a) condition over a conditional read, a task condition
     over an instrumentation wire) gets its footprint only: every read, then
     every call, then its task statements;
   - **user assertions** (§5): every `mutually_exclusive` check line is
     read by its structure (`mecheck`, soundness review r8b-1 M1) and gives
     the rule exclusions `as × bs`, as the `|as|` checks `(a, bs)`
     (sysFloatTest's multi-line check steps included); the same reading feeds
     item 4, and both assert the pairs equal the dumper's list; a
     `conflict_free` check gives no rule exclusion (the rules may fire
     together) and is assumed in items 2 and 4;
   - an unguarded FIFO is scanned with FIFO2's schedule and reported.
2. **every escaped pair's dynamic facts confirmed, computed by the emitter**
   (soundness review r8-2 M1): in the same run, `escape_facts` checks every
   statically failing site pair of every escaped pair as a file would, call
   against call included: the solver over that site pair and the two sites'
   stated conditions (every port its own atom); else bsc's fact keyed by both
   steps and both uses, with each stated condition implying bsc's; else the
   solver with the `conflict_free` assertions naming either step assumed (a
   user assertion; a file states such a fact under `asmP`, §5). The `GATE` line reports the facts, how each is confirmed
   (`solver`, `bsc`, `assert`, or unconfirmed) and the unconfirmed ones. A
   fact on a step the site builder could not build (item 1's footprint-only
   fallback) is not checked as a file would state it (the check takes the
   APackage's call conditions, the model's site has none; soundness review
   r8b-1 minor 2): the `GATE` line counts such facts (`esc_facts_fallback`)
   and the gate fails a design that has one (no path-A verdict changed:
   the designs with such facts are flat for other reasons);
3. every readiness site's facts confirmed in the site-value form (§7;
   `Lean.Sched.Sites`);
4. **no shadowing port outside class `L`** (§8; `Lean.Sched.Shadow`, per port,
   one solver call each): every child action port two callers may call in
   one cycle is on the decided last-caller list with `lcKey` on the
   instance's schedule, with methods in the scan's order. The user's
   `mutually_exclusive` pairs are exclusions and its `conflict_free` checks
   are assumed, as in items 1 and 2; there is no hand exception (round 8's
   `KNOWN_X` is gone: mkAssertedSchedules' `outQ.enq` pair is excluded by its
   `conflict_free` check, which the tool now reads);
5. the module's rule task statements numbered in rank order (generator
   check); task calls inside methods are not statements (§12);
6. **numeric bounds where cost is not linear, or not measured further**
   (§17), each asserted by the emitter and written into the file, an
   environment variable may only lower it; the `GATE` line counts each the
   emitter's way. Since review r8b-1 the emitter checks the escape bounds
   (pairs, facts per pair, facts per module) before it solves any escape
   fact, and the pairwise bound before it enumerates a mux's caller pairs, so
   a design far above a bound stops before the solver work:
   - at most **4,096 uses in one scan segment**: the emitter cuts segments
     between steps, so the gate checks the largest step's uses in its scan,
     last-caller virtual slots included (`seg_uses` in the `GATE` line, since
     cost review r8b-1 minor 2; before, it was listed and not computed);
   - at most **8,192 slots**, counted as the emitter allocates them: every
     port of every child kind used (an instance takes all its kind's ports;
     an unused child none), one for `Tasks`, and one virtual slot per
     last-caller port; and at most **4,096 rule codes**;
   - at most **4,096 sites in one step** (×1.93–2.13 per doubling to 2,050
     sites, then **×2.19–2.94** for the last doubling in four runs at load
     12–31, the one scan segment's section ×4.2–4.3 in heartbeats; the bound
     sits at the measured point, 4,098 sites; a step cannot be split across
     segments);
   - at most **3,296 users of one port** and **3,296 members of one
     exception group** (the wide-port probe in parts, ×2.03–2.37 then ×2.15);
   - at most **64 users of a port whose enable is proved pair by pair**,
     counted the emitter's way (cost review r8-2 minor 5): an action port's
     users where one is conditional or the module has methods (`en_pairs`),
     or where a user pair has no recorded exclusion (`SELMUX`), without the
     emitter's one-group exception (an upper bound); `en_pairs` is
     superlinear and fails at 256;
   - at most **2,016 mux member pairs proved one by one** (round 9a review: the pairwise
     port's 64 users), counted over ports with differing arguments as the pairs outside one
     check group (an upper bound); 512 callers on one register gave 130,816 pairs, about 18 MB;
   - a **step fan-in of at most 256**: the operator inputs of its sites' conditions and
     arguments, summed (cost reviews r9b-1 M3, r9b-2 M2). Each site states its own (the
     `ea_`/`arg_` proofs and the step's `hq_` query restate an index mux of N reads at each of N
     registers), so a step's text grows with the square of its fan-in. Per doubling of the `.ba`,
     NoCfIdx (one rule indexing N registers) gave files ×2.1 and Lean ×2.0 (its 2.5 s import
     removed) up to fan-in 288 (N = 16), then files ×2.6–2.9 to 1,088 and Lean ×3.8–4.2 to 624; the scaled
     `sysConflictFreeOKLarge` gave ×4.1 from fan-in 592 to 2,208 and Lean ×12 near 4,096 (a 265 KB
     `.ba`, a 12.1 MB file, 715 s). The bound sits at that knee; written files reach 234
     (mkBufferControl). The emitter stops on it; the gate names it when it is the only reason
     (testsuite `FanIn.bs`). Fix path: state a definition the sites share (bsc's `SEL_ARR_…` def)
     once and refer to it by name, so a site's text is its own inputs only;
   - at most **512 callers of one last-caller port**, counting every user
     of the port (×1.53–2.17 per doubling to 512; 1,024 not buildable);
   - at most **64 escaped rule pairs** (item 1) and **4,096 dynamic facts in
     one escaped pair** (the linear check's measured point, §17), and the
     table settings of C21 (128 entries, 20,000 characters, 8 users listed
     entry by entry);
   - at most **16,384 facts in one module** (cost review r8b-1 M2: 128 groups
     of 128 fields, C30, the measured point, §17), counted in the gate's run
     as its escape facts, readiness and invariance facts, and, on every port
     counted for the pairwise bound (the only ports with caller-exclusivity
     facts), each user pair that no recorded exclusion covers (an upper bound
     on the caller-exclusivity facts; n(n−1)/2 above 64 users, where the
     design is flat anyway; `mod_facts`);
   - one site per port in each step (generator check, §6).

   **Emitter checks, not gate items** (cost review r8b-1 minor 2): a module
   above 4 MB of source is emitted in parts of about 2 MB, and each part is
   asserted to be at most 4 MB after writing (every probed part at
   2.1–4.1 GiB; §17). The gate does not compute it, because its run stops
   before the file is written and most corpus designs cannot be emitted; a
   single declaration above 4 MB would stop the emitter (fail safe). No
   structure is wider than 128 fields (C30) and no table definition longer
   than C21's settings, so the remaining risk is one long proof term.
7. no assumed leaf without its hypothesis named in the top theorem, and an
   assumed synthesized leaf exports every action method C with itself (§3).
   Since round 9b `--gate` checks a hierarchy module by module, children first
   (§3.1): each module's record and rows, then a `hierarchy` row with each
   child instance's kind and place (path A, or an assumed leaf and why). A
   parent's model takes each child's export (path A) or the parent's view of
   it (an assumed leaf, `RDY_m` aliased to `m`'s ready), as its emitter does.
8. **one clock domain** (round 9b, `Lean.Sched.Domain`; bsc's domains are its
   clock families). A module of several is not checked, with no fallback:
   `--gate` gives it its own verdict, `not checked: multi-domain` (neither A
   nor B), and the emitter stops. Gated clocks of the family and several
   resets are modelled: bsc ANDs a clock's gate into the condition of every
   action call on the state it clocks (`IExpand.handleMethod`), and the
   gate checks for each such call that the step's guard (a method's ready
   signal, which the contract assumes) and the call's condition imply the
   gate, structurally or with bsc's solver; with the gate off that state is
   not called and holds, as its model does without calls, whether or not
   its oscillator ticks. The gate fails with a named reason when a gate is
   not an input of the module (a gate a child drives: GatedClock has no
   model), when a child's Verilog takes the gate as a port, or when a call's
   enable may be on with its gate off. A gate input is outside the Lean
   interface: a guard reading it is an opaque operator of its other reads,
   and `top` holds for every `ops`, so for each cycle's gate value (bsc's
   solver verdicts in `Hyps` treat the gate as a free variable). With several
   resets, `top` covers the cycles outside reset, every reset deasserted
   (its docstring says so); async-reset primitives (RegA, ConfigRegA) take
   their synchronous models, which they equal outside reset, and FIFOs select
   async reset by a Verilog define, not by name.

**Corpus maxima against item 6** (`gate8/count_*.txt`, `gatescan_*.txt`):
the largest step has 3,074 sites (sysConflictFreeOKLarge, every mode; the next
is 168, sysTrafficBRAM); at most 3 scan segments; slots **40,960**
(mkCregInlineBlowup: 4,096 CRegN5 × 10 ports) and **10,254**
(sysConflictFreeOKLarge: 514 RegN × 2, 3,075 RWire × 3 and `Tasks`; its 512
last-caller virtual slots of round 8 are gone, since its `conflict_free`
checks exclude the coincident writes), both above the bound; among path-A
designs at most 2,287 (sysTrafficBRAM); rule codes at most 384
(sysFloatTest); escaped pairs at most 12 (mkBufferControl, default mode);
dynamic facts in one escaped pair at most 512 (sysConflictFreeOKLarge, flat),
7 (mkBufferControl) and 1 (mkAssertedSchedules, path A); 206 users of one
port and a 204-member group (sysBRAM0Test); 11 users of a port proved pair by
pair the emitter's way (mkMCR; then mkAes 10); a last-caller port has at most
10 users (mkMCR `the_pixelValid_reg`); the largest step's uses in the scan
3,074 (sysConflictFreeOKLarge, `seg_uses`; then 89, mkPrediction); facts in one
module at most 3,072 (sysConflictFreeOKLarge, flat; 1,536 of them escape
facts, the rest the upper bound's), 96 (mkBufferControl, default mode, flat; its
file has 95) and 20 on path A (mkPrediction without aggressive conditions);
the largest file emitted is 2.4 MB (sysArithPipe), one part.

Otherwise path B (case-split pair fields; defined, not built), then today's
flat theorem; the module enters modular parents as an assumed leaf (§3).

**Outcome per design and mode** (`gate8/summary.txt`, from `gate8/run9.sh`
after review r8b-1). The 8 designs with site conditions plus mkUrgencyBlocking
and mkMethodRelations were compiled in all four modes; the other 26 in the
default mode only.

| design | default | `-no-aggressive-conditions` | `-no-sched-conditions` | both off |
|---|---|---|---|---|
| sysArithPipe | A (proved end to end; relies on its two `mutually_exclusive` groups) | A | A | A |
| sysConflictFreeOKLarge | flat (10,254 slots; scan, 1,536 escape facts (bsc's) and shadowing pass, the last through its `conflict_free` checks) | flat (the same) | flat | flat |
| sysFloatTest | flat (10 escape facts unconfirmed) | flat (the same) | A (relies on its `mutually_exclusive` checks) | A |
| mkAssertedSchedules | A (relies on its assertions: `(RL_up, RL_down)` `mutually_exclusive`, and the `outQ.enq` escape fact and shadow pair from its `conflict_free` check; since round 9b the emitter writes its file, which proves) | A | A | A |
| mkBufferControl | flat (readiness fact refuted; its 50 escape facts confirmed by the solver; `lockInterLoads` is `L`) | **A** | flat (readiness) | **A** |
| mkPrediction | flat (readiness fact refuted; 2 escape facts unconfirmed; 12 `L` ports and, every port its own atom, one `X`: `intraMemReqQ.enq`) | **A** (8 `L`) | flat (readiness; 12 `L`) | **A** (8 `L`) |
| mkDeblockFilter | **A** (6 `L` ports) | **A** | **A** | **A** |
| mkMCR | **A** (4 `L`, one on ConfigRegN; `obl` proved) | **A** | **A** | **A** |
| mkUrgencyBlocking | **A** (1 `L`; proved end to end) | **A** (proved) | **A** (proved) | **A** (proved) |
| mkMethodRelations | A (1 `L` port whose callers are two methods, `incA SBR setA`, and the rule `RL_mv`, which executes after both; the emitter cannot write it) | A | A | A |
| bus_2m_2s | flat (decided, §10; the emitter's scan also fails `mm`) | not measured | not measured | not measured |
| mkCregInlineBlowup | flat (40,960 slots) | not measured | not measured | not measured |
| mkRegFilePorts, mkSolver3, mkPump | flat (the emitter's scan fails `h3`: their rule–method exclusions are not used) | not measured | not measured | not measured |
| mkWireResult | flat (the emitter's scan fails `h3`: its value method is modelled as a rule) | not measured | not measured | not measured |
| mkUseOdd | A (`OddPrim` an assumed BVI leaf with its declared `c SBR c`) | not measured | not measured | not measured |
| the other 19 corpus designs | A (sysBRAM0Test and sysBasic rely on their `mutually_exclusive` checks) | not measured | not measured | not measured |

**Bold**: moved to path A in round 8. Per mode, of the 10 designs compiled in
all four: default 6, `-no-aggressive-conditions` 8, `-no-sched-conditions`
7, both off 9.

**Default mode: 26 of 36 pass the gate.** Flat:

- bus_2m_2s (decided);
- mkBufferControl (one refuted readiness fact) and mkPrediction (one refuted
  readiness fact, two unconfirmed escape facts, one `X` port);
- sysFloatTest (10 escape facts unconfirmed);
- **slots** (item 6, the emitter's convention): mkCregInlineBlowup (40,960)
  and sysConflictFreeOKLarge (10,254);
- **the emitter's scan** (item 1): mkRegFilePorts, mkSolver3 and mkPump fail
  `h3` because the emitter gives the scan no rule–method exclusion (urgency
  `(RL_fill, poke)`, disjoint guards `(RL_controller_fsm_start,
  startSolver)`, urgency `(RL_drain, take)`), although `Scan.lean` has the
  field (`exMR`); mkWireResult fails `h3` because the emitter models its
  value method `get` as a rule, so bsc's exported `get C put` does not reach
  the scan.

**Why the count is 26, and what review r8-2 counted.** Review r8-2 found
that item 2 had never been computed for call/call escape facts, and that by
the gate's rules as then stated mkAssertedSchedules (every mode) and mkUseOdd
(default) failed it: 24 of 36, and 5 / 7 / 6 / 8 per mode. Item 2 is now
computed by the emitter for every escaped pair. Under the decisions applied
with it, both designs pass, so the counts stay 26 and 6 / 8 / 7 / 9:

- mkAssertedSchedules' `outQ.enq` fact holds under its `conflict_free`
  check, now an explicit hypothesis (`assert` in its `GATE` line), not a
  hand exception;
- mkUseOdd's `OddPrim` is a user BVI import, so it takes its declared `c SBR
  c`, and the pair `RL_rc → RL_rd` is ordered with no escape; under the old
  "C with itself" its fact was false.

Item 2 also changes a reason: mkPrediction's default-mode escapes carry two
facts neither the solver nor a bsc fact confirms (`RL_intraPredTypeStep` /
`RL_outputing` on `intra4x4typeLeft` and `cur_intra4x4_pred_mode`, write then
read), which the site tool's own count did not flag; mkPrediction was flat
there already. With Counter as `ctrAnn`, sysFloatTest's 9 extra `cntErrors.addA`
escapes that review r8-2 measured are excluded at the rule level by its own
`mutually_exclusive` checks, now read in their multi-line form; its 10
`fCheck_*.deq / notEmpty` facts stay unconfirmed in the two modes with them,
as the site tool also finds.

**User assertions relied on** (the decision asks for the count; `gate8/
summary.txt`, last section). Six designs, four of them path A in the default
mode:

| design | `mutually_exclusive`: scan exclusion bits needed / shadow ports | `conflict_free`: escape facts / shadow ports | verdict |
|---|---|---|---|
| mkAssertedSchedules | 1 / 1 | 1 / 1 | A, every mode |
| sysArithPipe | 2,265 / 0 | — | A, every mode |
| sysBRAM0Test | 20,706 / 0 | — | A (default) |
| sysBasic | 465 / 0 | — | A (default) |
| sysFloatTest | 12,442 / 1 | — | A in the two modes without escapes |
| sysConflictFreeOKLarge | — | 0 / 512 (its escape facts are bsc's) | flat (slots) |

A scan exclusion bit is a pair of rules the scan needs excluded and only a
`mutually_exclusive` check excludes; a shadow port is one where two callers
could both call without the assertion. Every other design relies on none.

Caveats on the count:

- (i) **Emitter coverage.** The merged emitter writes a file for 14 of the
  36 designs (`gate8/emitall/emitall.txt`, rerun after review r8-2; 13 of the
  26 path-A designs): sysArithPipe, mkUrgencyBlocking and mkMCR (`obl`),
  proved in this round's build; mkBranchWrites, mkDutLogic, mkLeanNames,
  mkNotEmptyOrder, mkSysTasks, mkLookupQ, mkValueAfterAction (proved by the
  cost piece: `obl` and `top`, or `obl` only for the last two); sysBasic,
  mkSource, mkSplitRule (emitted, not rebuilt this round); mkBufferControl
  (flat, `obl` proved). The others stop on unsupported children (CRegN5,
  BRAM2, Counter, BypassWire, synthesized children, the user BVI import),
  non-CF exported method pairs, the run-time check forms (a task condition
  over an instrumentation wire; a `conflict_free` check would stop it next),
  or `h3`.
- (ii) The gate's scan uses item 1's fallbacks on 18 designs: generic kinds
  on 12 (CRegN5, BRAM2, Counter, Probe, RevertReg, BypassWire, synthesized
  children, the user BVI import `OddPrim`, …); bsc's exported method
  relations on 4 (bus_2m_2s, mkMethodRelations, mkRenamedPorts,
  mkSplitMethod); footprint-only steps in mkDeblockFilter, sysFloatTest,
  mkPrediction, mkPump, mkAssertedSchedules and sysConflictFreeOKLarge
  (`step_fallback` in `gatescan_*.txt`); 16 unguarded FIFOs in
  sysBRAM0Test, which no library model covers (decided unsafe: start-of-cycle
  status, no readiness obligation), so sysBRAM0Test needs that model before
  a file.
- (iii) mkPrediction's default-mode `X` port appears only with every port its
  own atom (with `notEmpty`/`i_notEmpty` aliased the pair is exclusive). The
  emitter's caller-exclusivity facts use the same exact view, so admitting it
  needs `AliasOK` premises on those facts (§21 item 5); mkPrediction is flat
  there for readiness anyway.
- (iv) Gate item 5 has been checked against bsc's Verilog only on
  sysArithPipe; the 26 designs without site conditions were compiled in the
  default mode only.
- (v) The shadow tool's corpus maximum is sysConflictFreeOKLarge: 41.3–64.5 s
  wall per mode across the runs after reviews r8-2 and r8b-1 (41.3–44.4 s in
  the latest, `gate8/shadow_all.sh`), of which 30.1–49.2 s is the site
  analysis it runs first, with 1,024 solver calls (one per port, one more per
  port to count its `conflict_free` reliance). Every other design takes at
  most 14.2 s (mkCregInlineBlowup, 12.8 s in the latest, nearly all site
  analysis). The emitter's gate run takes 88 s on sysConflictFreeOKLarge (its
  1,536 escape facts; 48 s before item 2 was computed, cost review r8-2), and
  the other 35 corpus designs take 19 s together (`gate8/run.log`).

**Tool changes behind the count** (review r8-2): item 2 from the emitter
(`escape_facts`, in the gate run); user assertions read in every tool;
Counter as `ctrAnn`; synthesized children told apart by the soft hyphen, user
BVI imports by bsc's library packages; the shadow tool one solver call per
port; the pairwise count the emitter's way. Against the run after review r8-1
(`gate8/r8b/`), no verdict changes; the reasons that change are
mkPrediction's (two unconfirmed escape facts added) and sysFloatTest's (its
`mutually_exclusive` checks now read). `gate8/mkcorpus.sh` re-dumps six
corpus designs from their `.ba` with `tools/shape-final.bin` (round 8).

**Review r8b-1's rerun** (`gate8/run9.sh` again, then `gatescan.sh` once the
caller-exclusivity count was limited to pairwise ports and `shadow_all.sh` once
the shadow tool's pair checks were made linear; the run after review r8-2 is
in `gate8/r8c/`): `mutually_exclusive` lines read by structure in the model and
the shadow tool (checked against the dumper's list in every dump), the shadow
call's group encoding, `generic_kind`'s Counter only for library children,
and three new `GATE` fields (`seg_uses`, `mod_facts`, `esc_facts_fallback`).
Every `GATE` field computed before is identical in all four modes and the
corpus, the shadow tool's outputs are identical except wall times, the site
tool's and `gatecount`'s outputs are identical, and the counts stay **26 of
36** and 6 / 8 / 7 / 9, with the same six designs relying on a user
assertion. The new fields add reasons, not verdicts: mkPrediction (default, 3
facts) and sysFloatTest (default and `-no-aggressive-conditions`, 8 facts)
have escape facts on footprint-only steps, and all three are flat already;
no design is above the segment or module-facts bound.

**Round 9: schedlean's gate.** `schedlean --gate` reproduces the prototype's
`GATE` records, count rows and summary rows on all 40 four-mode runs and the 36
corpus designs, with two named differences: bus_2m_2s's reason reads 15
unrealizable exported CF pairs, as both dumpers record them, where
`summary.py` had 16 written in by hand; and sysFloatTest, whose source sets
`(* options = "-aggressive-conditions" *)`, reports `default` and `nosched`
for its "noagg" and "both" compiles, which therefore never ran without
aggressive conditions (its verdicts are unchanged). The counts stay **26 of
36**, and 6 / 8 / 7 / 9. The unrealizable CF swaps come from bsc's
execution-order graph without building the swapped orders: a swap can deadlock
only where a successor of the earlier method's steps reaches a predecessor of
the later one's, so only those pairs are swapped (equal to the full
computation on all 79 parity designs).

In `-no-aggressive-conditions` (MatX's mode), all 36 corpus designs compiled
by this round's bsc: **28 of 36** pass. mkBufferControl and mkPrediction pass
here; the 8 flat are bus_2m_2s, mkPump, mkRegFilePorts, mkSolver3 and
mkWireResult (the emitter's scan), mkCregInlineBlowup and
sysConflictFreeOKLarge (slots), and sysFloatTest, compiled with aggressive
conditions by its own attribute (8 escape facts on a footprint-only step).
mkAes's attributes also keep aggressive conditions (path A either way).

## 12. Task traces

Each module's task statements, numbered in rank order, are the ports of one
`Tasks n` child; `Realizes`' last conjunct compares the hardware's calls with
the run's, per module. Different modules' task traces are unordered against
each other (§1).

**Task calls inside interface methods are skipped** (Ravi's decision of this
round). bsc warns G0020 that system functions called by interface methods
"execute in an unpredictable order during Verilog simulations". System-task
and BDPI calls made inside an interface method are therefore excluded from
task agreement:

- they are not statements of the `Tasks` child, so they are neither in the
  module's task trace nor in the scan's task positions (chain kinds), and a
  method's step has no task site (`model.py`, `M.mtasks`);
- a left-out call must return nothing (soundness review r8-1 minor 5): a
  rule's task result is a free output of the `Tasks` child, but a left-out
  method call has no counterpart, so a method whose later actions or result
  used it would be modelled wrongly. `model._tasks` admits in a method only
  the system tasks that return nothing (`$display`, `$write` and their file
  and radix forms, `$finish`, `$stop`, `$fflush`, `$fclose`, the `$dump*`
  tasks, `$error`/`$warning`/`$info`/`$fatal`) and stops on anything else
  (`$time`, `$random`, `$test$plusargs`, any BDPI function). The corpus has
  no task call in a method, so nothing changes. Negative control
  (`e2e/mtask/negval/`): the example's APackage with the method's `$display`
  replaced by `$fgetc` stops the emitter (`ValueError: method put: task call
  Prelude.$fgetc may return a value`);
- task calls in rules stay statements and fully checked (enable, arguments,
  order);
- the emitter prints `note: NAME: method m: k task call(s) left out of task
  agreement (bsc G0020)` for each such method and writes the list into the
  file's first comment and `top`'s docstring;
- `Realizes`, `Built.top` and Modular's header say so beside the
  cross-module limitation.

The round-7 emitter already never made a method's task call a statement, but
silently; this round states it, notes it per module, and puts it in the
contract's docstring. Round 7's incompleteness cases for task-bearing methods
(two task-bearing methods exported CF, a task-bearing method called twice, a
task-bearing method picking up every task-bearing rule as a hit through the
chain slot, which could trip `h3`) no longer arise.

**Example** (`e2e/mtask/MethTask.bs`, Classic, compiled with bsc's default
flags): rule `tick` does `c := c + 1; $display "tick %d" c`; method `put x`
does `r := x; $display "put %d" x`. bsc warns G0020 for `put`. The emitter
prints `note: MethTask: method put: 1 task call(s) left out of task agreement
(bsc G0020)` and emits `nT = 1`: the `Tasks` child has the rule's statement
only. `MethTask.obl` and `MethTask.top` are proved (2.6 s, 2.4 s, 0.54 GiB, load 14; standard
three axioms). The same file with the rule's task
statement enabled in every cycle (`tenT` entry `true` instead of `WF 1 &&
true`; round 9b: `schedlean --mutants`' "enable force" mutant, byte for byte the
earlier hand-made `MethTaskBadEn.lean` but for its namespace) fails in `tqAll`, the
task statement's enable proof: an application type mismatch at the
statement's enable leaf, line 1163 (`en_one … (en_tk rfl)`), so `obl` and `top` then rest on `sorryAx`. So the rule's task call is checked, and the
method's is not in the contract. Today's flat generator (task agreement)
numbers both calls (`task #0` the method's, `task #1` the rule's).

**Counts.** No corpus design has a task call inside a method, and
`gatecount.py` and the scan mirror already counted rules' statements only,
so no e2e file and no gate count changes: the corpus has 4,606 rule task statements and no task call inside a method
(`gate8/mtask_count.py` over the 36 APackages, `gate8/mtask_count.txt`). Only
the example changes.

## 13. Replay

Per module, `leanchecker` without `--fresh` for incremental builds; the final
verdict for a design is one `leanchecker --fresh` of the top file
(`build/all.log`, the final build after review r8b-1, load 13.6–14.4; every
run rc 0).

| file | elaborate (wall, user, peak RSS) | `leanchecker` | `leanchecker --fresh` |
|---|---|---|---|
| sysArithPipe (tasks) | 118.5 s, 111.6 s, 2.55 GiB | 18.7 s, 1.30 GiB | 62.2 s, 1.36 GiB |
| sysArithPipe, no tasks / 4 segments / 60 rules | 66.0 s, 61.1 s, 1.88 GiB / 104.8 s, 98.0 s, 2.31 GiB / 47.0 s, 45.2 s, 1.42 GiB | — | — |
| mkBufferControl (obligation only) | 27.4 s, 26.4 s, 1.16 GiB (rebuilt by `build_mbc.sh` after a header row changed; in the full build 28.9 s, 27.8 s) | 3.8 s, 0.57 GiB | 43.2 s, 0.61 GiB |
| mkMCR (obligation only) | 18.9 s, 18.7 s, 0.87 GiB | — | — |
| mkUrgencyBlocking, four modes | 2.3–2.7 s, 2.2–2.4 s, 0.54 GiB each | — | — |
| LcProbe64 / MethTask | 6.4 s, 6.1 s, 0.67 GiB / 2.6 s, 2.4 s, 0.54 GiB | — | — |
| toy `ExTopRealizes` | 0.2 s, 0.1 s, 0.48 GiB | — | 41.2 s, 0.51 GiB |

## 14. sysArithPipe end to end

`e2e/SysArithPipe.lean`: 227 rules, 83 children, 1,859 sites including 690
task statements, 10 muxes, two `mutually_exclusive` groups (51 and 45
members), `OblU` through `certU`, `Acyc` and `top : RealizesU …`, standard
three axioms. ConfigRegN is now LastCaller's model (no assumed leaf for it;
`Leaves` keeps FIFOL1). No last-caller port and no conditional read, so its
`trU` and `HQ` are trivial. With `E2E_CFGLIB=0` the merged emitter writes
the cost piece's file byte for byte (checked on `SysArithPipe60`).

Four comparison sets, each back to back with one Lean process at a time:
`flat8` (`flat8/run.sh`, `flat8/run.log`, load 31–56), cost review r8-1's
(`../r8-cost1/logs/summary.txt`, load 17–35), cost review r8-2's
(`../r8-cost2/logs/summary.txt`, load 14–22) and cost review r8b-1's
(`../r8b-review-cost-1/logs/summary.txt`, load 13–17). The e2e files have
changed since only in header comments, the escape lemma's form (no escape in
sysArithPipe) and, after review r8b-1, the fact groups (sysArithPipe has no
facts) and the task-site lemmas (`isT`, `tnum`, `okP_task`, a few lines).

| | modular, tasks | modular, no tasks | modular, 4 segments | today's flat | task-agreement flat (regenerated) | atom test (`skel`) |
|---|---|---|---|---|---|---|
| `flat8` (load) | 258.6 s, 161.3 s, 2.68 GiB (56); again 155.8 s, 139.1 s, 2.63 GiB (37) | 99.4 s, 82.7 s, 1.98 GiB (42) | 213.3 s, 159.9 s, 2.16 GiB (40) | 37.7 s, 26.7 s, 1.38 GiB (48); again 26.3 s, 24.2 s (31) | 138.6 s, 114.3 s, 2.52 GiB (49) | — |
| cost review r8-1 | 110.6 s, 107.0 s, 2.73 GiB; again 110.8 s, 105.1 s | 61.5 s, 58.4 s, 1.94 GiB | — | 17.1 s, 15.8 s, 1.38 GiB; again 16.8 s, 16.6 s | 83.6 s, 78.1 s, 2.53 GiB | — |
| cost review r8-2 | 99.5 s, 99.1 s, 2.74 GiB; again 126.8 s, 109.2 s, 2.62 GiB (19) | 54.7 s, 54.4 s, 1.99 GiB | 123.1 s, 114.1 s, 2.31 GiB (20–22) | 16.9 s, 16.5 s, 1.38 GiB; again 23.2 s, 18.7 s | 75.0 s, 74.2 s, 2.52 GiB | 19.1 s, 15.8 s, 1.34 GiB (its flat file at that load 15.8 s) |
| cost review r8b-1 (13–17) | 98.0 s, 97.6 s, 2.74 GiB | 56.1 s, 55.8 s, 2.00 GiB | — | 16.8 s, 15.8 s, 1.38 GiB | 68.7 s, 67.1 s, 2.52 GiB | 15.3 s, 15.1 s, 1.34 GiB |
| checks task traces | yes, 690 | no | yes | no | yes | — |

**Ratios by user time, across the four periods**: against today's flat file
**5.7–6.8×** with tasks and **3.1–3.7×** without; against the task-agreement
flat file **1.2–1.45×** (1.34–1.45× in the three later periods; cost review
r8b-1 minor 6). Round 7: 6.6×,
3.9×, 1.5×; cost review r7-2: 7.2×, 4.4×, 1.6×. The task-agreement flat file
is the fair comparison for the file with tasks.

**With `skel`, measured** (cost review r8-2 minor 2; round 8's first
estimate is withdrawn). The atom tests are built with the repaired atomizer
(`tools/atomize3.py`: inputs read through parts, `i.partN.NAME`, the lifted
congruences in the same form, and view fields compared inside proofs; round
5's `atomize2.py` matched none of today's comparisons). sysArithPipe's has
167 input atoms, 136 chain facts and 2 lifted facts and costs what its flat
file costs (0.96–1.00×). The modular file plus `skel` is **6.7–7.1×** today's
flat file with tasks, **4.3–4.5×** without, and **1.55–1.68×** the
task-agreement file (cost review r8-2's and r8b-1's periods; cost review
r8b-1 minor 6; round 7: 8.2×, 5.6×, 1.8×). Like round 7's, the atomizer drops
everything after `schedule_sound`.

**Segmenting costs nothing measurable in time and saves 9–19 % of the
memory**: the 4-segment file's user time is −12 % to +4 % of the 1-segment
file's (the build after review r8b-1 −12 %, after review r8-2 −7 %, after
review r8-1 −5 %, `flat8` ±0, cost review r8-2 +4 %), and its memory 9 % (the
build after review r8b-1), 12 % (cost review r8-2), 15 % (the two earlier
builds) or 19 % (`flat8`) less.

The builds' own figures (`build/all.log`, the final build after review r8b-1)
are in the Files table and §13.

## 15. mkBufferControl end to end

9 exported methods, 18 rules, 60 children (43 Reg, 11 FIFO2 from the
library, 1 SizedFIFO and 3 RegFile assumed, one `Tasks 18`), 303 sites, 85
conditional calls, 2 conditional (a) reads, 5 readiness reads, 12 escaped
pairs with 50 dynamic facts, three urgency blockers on one rule, 33 muxes
and one last-caller port (`lockInterLoads.write`, callers
`RL_unlockInterLoads` and `RL_goToNextFrame`).

**Proved**: `MkBufferControl.obl : OblU (F ops E)` from `Hyps` and
`Unconfirmed`, standard three axioms (27.4 s, 26.4 s, 1.16 GiB, load 10.2,
after review r8b-1 (28.9 s, 27.8 s in the full build, before a header row
changed), with C29's task-site comparison and C30's
fact groups: `Hyps` one group of 94 facts, `Unconfirmed` one of 1).

- `Hyps`: disjointness; 4 readiness facts and 1 invariance fact (site
  values); 50 escape facts, each confirmed by the solver for its site pair
  and stated conditions, every port its own atom; 39 caller-exclusivity
  facts.
- `Unconfirmed` (1 fact, was 2): `rdy_3`, the readiness fact of
  `RL_inputing / shortTermPicList_returnList.first`, which the site tool
  refutes. Round 7's `cx_35` (the shadowing of `lockInterLoads.write`) is
  gone: the port is a last-caller port.
- `F_sites` by `rfl`.

**Not proved: `Acyc`, `top`**: the synthetic netlist's shared RegFile copy
address muxes form a combinational cycle that bsc's real netlist cannot have
(the dump lacks `aState`'s select signals).

Because of `rdy_3` the module is off path A in the default and
`-no-sched-conditions` modes; in the two other modes it has no readiness
site and moves to A (§11).

| | modular (obligation only) | today's flat | task-agreement flat (regenerated) | atom test (`skel`) |
|---|---|---|---|---|
| `flat8` (load) | 52.3 s, 40.0 s, 1.04 GiB (56) | 60.5 s, 51.7 s, 1.44 GiB (53) | 86.7 s, 69.0 s, 1.59 GiB (49) | — |
| cost review r8-1 (load 17–35) | 26.3 s, 26.1 s, 1.05 GiB; again 29.8 s, 29.3 s | 36.4 s, 34.8 s, 1.43 GiB | 46.9 s, 45.5 s, 1.59 GiB | — |
| cost review r8-2 (load 17–30) | 26.9 s, 26.7 s, 1.05 GiB; again 38.3 s, 37.7 s (30) | 34.4 s, 34.1 s, 1.44 GiB; again 48.5 s, 47.6 s (30) | 45.1 s, 44.7 s, 1.60 GiB | 23.8 s, 23.4 s, 1.20 GiB (29) |
| cost review r8b-1 (load 13–14) | 26.6 s, 25.8 s, 1.05 GiB | 34.7 s, 33.1 s, 1.43 GiB | — | 18.5 s, 17.3 s, 1.19 GiB |
| ratio by user time, without `skel` | — | **0.75–0.84×** | **0.57–0.64×** | — |

**With `skel`, measured** (cost review r8-2 minor 2): mkBufferControl's atom
test (114 input atoms, 54 view atoms, 543 chain facts, 204 lifted facts) costs
0.49–0.52× its flat file at the same load, and 1.89× a control with the same
cut and no atoms (`tools/cutonly.py`: 12.3 s). The modular file plus `skel` is
**1.28–1.30×** today's flat file and ≈ 0.97× the task-agreement file (round 7:
0.80× / 1.34× in cost review r7-2's pairing). The last-caller port costs
nothing measurable: with `E2E_LC=0` the file takes 26.2 / 28.5 s against 26.1
/ 29.3 s (cost review r8-1) and 36.9 s against 37.7 s (cost review r8-2).

**mkMCR** (obligation only) against today's flat file: **1.00–1.03×** by user
time (28.3 / 27.5 s in `flat8`, 20.0 / 19.5 s in cost review r8-1, 19.3 / 19.3 s
in cost review r8-2, 19.2 / 19.1 s in cost review r8b-1); 0.87 against
1.14–1.16 GiB. With its atom test (35 atoms, 24 chain facts; 7.5–7.9 s user,
1.05 GiB) the modular file plus `skel` is **1.38–1.42×** today's flat file. **Last caller costs nothing measurable** on mkMCR (cost
review r8-2 minor 3): three back-to-back pairs on/off take 29.9 / 28.0 s,
27.4 / 28.1 s and 25.4 / 24.5 s (ratios 1.07, 0.97, 1.04; load 28–32); round
8's "20 % cheaper" compared runs that were not back to back.
**mkUrgencyBlocking**: 2.4–3.7 s against 0.24–0.4 s user; the modular file's
time is nearly all loading `LastCaller` and its imports (about 2–3 s).

## 16. Last caller end to end: mkUrgencyBlocking, mkMCR, the probe

- **mkUrgencyBlocking** (4 rules, 4 children; `lastOp.write` shadowed by its
  test rule): `obl`, `acyc` and `top` from `Hyps` alone, no `Unconfirmed`, in
  all four bsc modes (`e2e/MkUrgencyBlocking.lean`,
  `e2e/modes8/MkUrgencyBlocking_{noagg,nosched,both}.lean`): default 2.5 s,
  2.3 s; noagg 2.7 s, 2.4 s; nosched 2.5 s, 2.3 s; both 2.3 s, 2.2 s; 0.54 GiB
  each (load 14.3).
- **mkMCR** (28 rules, 1 method, 44 children; last-caller ports on 3 RegN
  and the ConfigRegN `the_a`, up to 10 callers): `obl` from `Hyps` alone,
  using the ConfigRegN model (18.9 s, 18.7 s, 0.87 GiB, load 13.8). `Acyc`/`top` are not emitted: a
  method's enable drives an RWire's path input, and the emitter's `Acyc`
  has no input dependencies (a gap round 7's emitter shares).
- **LcProbe64** (64 rules writing one register every cycle, all
  non-exclusive): `obl`, `acyc`, `top` (6.4 s, 6.1 s, 0.67 GiB, load 14.3). The last-caller piece's
  probe series on its own emitter, whole file: 64 / 128 / 256 / 512 callers
  11.6 / 21.5 / 42.9 / 85.9 s wall, 9.3 / 17.4 / 37.8 / 76.3 s user,
  0.66 / 0.82 / 1.15 / 1.84 GiB (load 19–30): ×2.0 per doubling in time, and
  memory a 0.5 GiB base plus a part that doubles. The 1,024-caller probe
  could not be built (bsc's `.ba` is 212 MB and the dumper overflows its
  stack), so the gate bounds a last-caller port at 512 callers.
- mkDeblockFilter, mkPrediction and mkMethodRelations have no file: the
  prototype emitter rejects synthesized children (mkInterpolator,
  mkWorkVectorHor) and non-CF exported method pairs. Their status is the
  gate's.

## 17. Cost, recomputed

Per-unit slopes are user time; loads are quoted; memory is peak RSS in GiB.
Growth is quoted as the factor per doubling with its load and its spread
across runs, not as "linear": loads of 12–111 on 32 cores move single runs by
up to 30 %, and one file's peak memory varies by up to 39 % between runs of
the same file (cost review r8-2 minor 6).

**Per doubling** (user time unless stated; every run capped, one Lean process
at a time; the reviews' runs in `../r8-cost1/logs`, `../r8-cost2/logs`,
`../r8b-review-cost-1/logs`; review r8b-1's fixes measured in `../r8c/`: the
probe files in `escE/` and `hyps/`, the queues `tools/q1.sh`, `q1b.sh`,
`q2.sh`, one line per run in `logs/summary.txt`, full output in
`logs/*.full`, blocks read by `tools/sect.py` and `tools/sect2.py`):

| series | sizes | per doubling (spread across runs) | memory | runs |
|---|---|---|---|---|
| **facts of one escaped pair, linear check** (`scan/escS`, review r8-2's fix; trivial facts, two-port `Reg` children) | 256 / 512 / 1,024 / 2,048 / 4,096 facts | kernel 0.55 / 1.41 / 2.66 / 5.02 / 10.9 s (load 15.5–15.7) and 0.47 / 1.03 / 2.15 / 5.19 / 10.2 s (cost review r8b-1, load 11.5): ×1.9–2.6 per doubling; user ×1.85–2.11 (31.65 / 29.16 s at 4,096; wall 36.95 / 33.59 s). "Kernel" is the profiler's type checking over the step index check `chk2`, the `escChk` chunks and `esc` only; the tables' definitions come before the profiler is on (20.2 s of the 36.95 s wall at 4,096; cost review r8b-1 minor 4) | 0.56–0.57 / 0.62–0.64 / 0.76–0.77 / 1.06 / 1.68 GiB: a 0.5 GiB base plus a part that doubles | two each |
| facts of one escaped pair, round 8's check (cost review r8-2) | 256 / 512 / 1,024 / 2,048 | kernel 4.91 / 14.1 / 80.4 s: ×2.9, ×5.7; out of memory at 2,048 (19.6 GiB) | 1.06 / 2.63 / 8.71 GiB | one each |
| **first-step task statements of one escaped pair** (C29; the real emitter, `../r8c/escE`, cost review r8b-1's `genescE.py` shapes, one fact) | t1 = 128 / 256 / 512 task statements in the first step, nT = 2·t1 | escape block kernel **0.03 / 0.06 / 0.12 s: ×2.0, ×2.0** (wall 0.12 / 0.17 / 0.25 s; load 10.5–12.7); before C29 10.52 / 42.50 / 188.7 s: ×4.0, ×4.4 (cost review r8b-1, load 12–18). With t1 = 128 and nT = 2,048: 0.19 s (before: 41.6 s); the extra is the second step's 1,920 sites in `escT2S` | the block adds nothing measurable (before: +0.84 / +1.97 / +6.66 GiB); files peak 0.79 / 0.99 / 1.41 GiB (before 1.51 / 2.78 / 7.75) | one each, and the review's |
| **facts of one module, in groups of 128 fields** (C30; the emitter's own output) | real files (one escaped pair, `genescE facts W`, `../r8c/escE`): 256 / 512 / 1,024 facts | structure block 0.82 / 1.74 / 3.50 s, kernel 0.38 / 0.81 / 1.62 s: **×2.1, ×2.0** (load 11–12); before C30 1.47 / 5.89 / 28.6 s (kernel 0.91 / 4.42 / 24.3 s): ×4.0, ×4.9 (cost review r8b-1). The escape block beside it 1.80 / 4.06 / 8.59 s, kernel 0.49 / 1.12 / 2.52 s: ×2.1–2.3 (the review's 1.86 / 4.36 / 8.14 s); whole files 30.9 / 66.6 / 138.2 s wall, 30.8 / 65.5 / 132.6 s user (before 33.8 / 75.7 / 164.4 s), dominated by the sites' sections | the block adds 0.03 / 0.04 / 0.08 GiB; files peak 1.11 / 1.70 / 2.88 GiB | one each |
| the same structures at the module bound (`../r8c/hyps`, `scan/escT/mkhyps.py`: the group structures and `Unconfirmed` exactly as the emitter writes them for 1,024 / 4,096 / 16,384 facts, in the 8-fact file's context, plus one theorem per group using every projection) | 1,024 / 4,096 / 16,384 facts | structures 3.42 / 13.61 / 57.51 s, kernel 1.68 / 6.69 / 26.69 s: **×2.0, ×2.06 per doubling**; the uses 1.69 / 7.29 / 34.66 s (×2.1, ×2.2); whole probe 7.8 / 24.1 / 96.6 s wall, 7.8 / 24.1 / 89.3 s user (load 11–16). One structure of 4,096 fields took 503.3 s (cost review r8b-1's `genhyps.py`, load 14) | 0.71 / 1.16 / 3.02 GiB (structures and uses add 0.16 / 0.62 / 2.46 GiB) | one each |
| one large step (`cost/big`) | 1,026 / 2,050 / 4,098 sites | ×1.93–2.13, then **×2.19–2.94** for the last doubling (four runs: ×2.19, ×2.30, ×2.69, ×2.94; load 12–31); the one scan segment's section ×4.2–4.3 in heartbeats and ×5.7–7.4 in time at that doubling | 1.50 / 2.68 GiB, then 3.37 GiB per part | cost piece, r8-1, r8-2 |
| a wide port, one file (`cost/port`) | 206 / 412 / 770 users | 206 → 412: ×2.13–2.16; 412 → 770 (1.87× the users): ×1.90–1.97; the cost piece's 412 → 824: ×2.40 | **×1.6–2.9 per doubling**: P412 2.52 / 3.41 / 3.51 GiB, P770 6.61 / 5.79 GiB in different runs | r8-1, r8-2, cost piece |
| a wide port, in parts | 824 / 1,648 / 3,296 users | ×2.03–2.37, ×2.15 | 3.17–3.60 / 3.49–3.88 / 4.14 GiB per part | all three |
| one last-caller port | 64 / 128 / 256 / 512 callers | ×1.53–1.87, ×2.02–2.17, ×1.89–2.02 | 0.67 / 0.82 / 1.14 / 1.80 GiB | lc piece, r8-1, r8-2 |
| many last-caller ports, two callers each | 64 / 128 / 256 / 512 ports | ×1.99–2.14, ×2.00–2.30, ×2.03–2.14 | 0.85–0.89 / 1.26–1.34 / 2.12–2.42 / 4.21–5.23 GiB | r8-1, r8-2 |
| a task chain | 20,480 / 40,960 / 81,920 statements | wall ×1.81, ×2.21 | 1.25 / 1.75 / 2.74 GiB | round 7 |
| sites of one step on one port, index check (`scan/escS` fan) | 1,024 / 4,096 sites | 32 s → 474 s kernel: ×3.8 per doubling (quadratic); the emitter asserts one site per port per step (§6) | 1.67 / 5.44 GiB | this round |

| item | measured | where |
|---|---|---|
| a site (small steps) | 0.05–0.09 s user per site (sysArithPipe without tasks 54–88 s for 1,169 sites; with tasks 99–161 s for 1,859); 0.09–0.13 s (mkBufferControl: 303 sites, methods, escapes, readiness); memory ≈ 1.2–1.8 MB per site over a 0.5 GiB base where steps are small | §14, §15 |
| one large step (`cost/big`, this emitter) | 514 / 1,026 / 2,050 sites 32.7 / 69.2 / 147.1 s user, 1.00 / 1.50 / 2.69 GiB (cost piece, load 13); 4,098 sites in 3 parts 393–433 s, 3.32–3.37 GiB (load 18–31). The step bound sits at the measured point | `cost/logs`, `../r8-cost1/logs`, `../r8-cost2/logs` |
| a wide port (one group of all users) | above; 770 users is 3.97 MB, admitted whole under the 4 MB bound, and peaks at 5.8–6.6 GiB | `cost/port` |
| a whole module: sysConflictFreeOKLarge's **site shape** (`Bcfq`) | 8,716 sites, steps of 3,074 / 2,562 / 2,050 sites, 2,050 statements, 3 segments: 6 parts, 1,736 s wall, 1,146 s user, 3.36 GiB (load 33–62). Site counts only: 6,667 RegN slots and one rule code; not the design's 3 escapes with 1,536 dynamic facts, its 1,024 conditional (a) reads or its `conflict_free` checks. Its escapes, priced in parts (cost review r8b-1 M1 corrected the price): the task part of `RL_r2 → RL_r3` (its first step's 512 statements, numbered 513–1,024 of 2,050; `tri 513 512 1025`) took **482.9 s of kernel time and 18.40 GiB** in one 2.4 MB file before C29, and now 0.14 s kernel with no memory added in the block (the whole copy 117.4 s wall, 113.7 s user, 2.27 GiB, load 12–13; before 601.4 s, 546.0 s); each pair's 512 facts cost the linear check's 512-fact point (1.03–1.41 s kernel, 0.62–0.64 GiB, trivial facts) plus their 4 fact groups | `cost/big/parts`, `../r8c/escE` |
| a mux member of one group | chain from the group (round 7's chain probe to 32,768 members; conditional users in one group, 16 / 64 / 256 users: 0.54 / 2.52 / 6.48 s) | `cost/port` |
| a port proved pair by pair (conditional user, or a module with methods) | `en_pairs` per-instance lemmas 13 / 164 / 9,180 Mhb at 16 / 64 / 256 users; superlinear, fails at 256: **bounded at 64** (counted the emitter's way since review r8-2: corpus maximum 11, mkMCR) | `cost/port` |
| last caller's own share | about 0.1 s per caller of one port ((57.11 − 30.24) s / 256); +13–18 % at 256 two-caller ports (LcMany256 against `E2E_LC=0`); **nothing measurable** in the e2e modules (mkMCR on/off 1.07, 0.97, 1.04 in three back-to-back pairs; mkBufferControl 37.7 / 36.9 s) | `../r8-cost2/logs` |
| the scan | per checkpointed segment of 4,096 uses at S = 8,192: 25–37 ms/use of kernel time (cost review r7-2's `CO_8192_*` runs); creg shape 35.5 s checkpointed, 16.2 s last | `r7-cost2` |
| an escaped pair | ≈ 1.2 ms per site of its first step (`scan/esc7`; a task site one comparison since C29, independent of `nT`), plus, per fact, in the emitter's own output (256–1,024 facts, the rows above): the escape block (tables, decisions, `esc_n`) 7.0–8.4 ms wall and 1.9–2.5 ms kernel, and the fact's structure field 3.2–3.4 ms wall and 1.5–1.6 ms kernel (cost review r8b-1 minor 4: round 8's 2.6 ms kernel / 7.7 ms user per fact was `scan/escS`'s totals over trivial facts). At most 4,096 facts per pair (gate item 6): a pair that large cannot be emitted within the segment bound (its reading step has 4,097 uses), so the bound is a backstop, priced in parts: the check at 4,096 trivial facts (10.2–10.9 s kernel, 1.68 GiB) and 4,096 facts' structures (13.6 s) | `scan/esc7`, `scan/escS/run.txt`, `../r8c/escE`, `../r8c/hyps` |
| the shadow tool (gate item 4) | one solver call per port with two callers since this round (`two_call`), plus one per bad-order pair on an `L` port and two where an assertion is needed; nearly all of a design's time is the site analysis the tool runs first (`sitecond.analyse`). Corpus: see §11 (wall and solver calls per design in `gate8/shadow_*.txt`). **Worst case** before review r8-2's revision: one call per pair on a port whose pairs are all exclusive (46,344 calls, 46 s, on sysFloatTest); then one call per port with one clause per recorded exclusion, so a `mutually_exclusive` group of n callers gave n(n−1)/2 clauses (5.4 million and 12.4–12.6 s at 1,648 callers, unmeasured at the 3,296 bound; cost review r8b-1 minor 1). Now the call's input is linear in callers plus the pairwise exclusions recorded among them (disjoint guards, urgency, lines outside a group), a group one at-most-one counter: 52,720 clauses and 0.95–0.96 s at the 3,296-caller bound with one group of all (`sites/shbench/run.txt`) | `gate8/shadow_*.txt`, `sites/shbench/run.txt` |
| `skel` (atom tests, the stand-in round 7 used) | **measured** with the repaired atomizer (`tools/atomize3.py`, cost review r8-2 minor 2): sysArithPipe 19.1 s wall, 15.8 s user, 1.34 GiB (load 16) and 15.3 / 15.1 s, 1.34 GiB (cost review r8b-1, load 13–14); mkBufferControl 23.8 / 23.4 s, 1.20 GiB (load 29) and 18.5 / 17.3 s, 1.19 GiB; mkMCR 7.7 / 7.5 s, 1.05 GiB (load 16) and 8.5 / 7.9 s, 1.05 GiB. Ratios, as ranges across both periods, in §14, §15 | `../r8-cost2/logs` (`atom_*`, `atom2_MBC`), `../r8b-review-cost-1/logs` (`skel_*`) |

**Superlinear costs left, each under a gate bound:**

- **A single Lean file's memory** grows ×1.6–2.9 per doubling (one file's
  peak varies by up to 39 % between runs; RSS climbs section by section and
  peaks in `scan`). One file at the 4 MB part bound peaks at **5.8–6.6 GiB**
  (port shape, 3.97 MB) and LcMany512 (3.12 MB) at 4.2–5.2 GiB; in parts every
  probe peaks at 2.1–4.1 GiB per part. `build.sh` waits for 30 GB available
  before any file above 2 MB. Splitting from 2 MB was not done: it would put
  sysArithPipe (2.4 MB) in parts.
- `en_pairs` (64 users).
- One scan segment's time grows faster than its uses (×4.2–4.3 heartbeats per
  doubling at 2,050 → 4,098 uses); the 4,096-use segment bound.
- Many last-caller ports in one module: K log K in heartbeats (512 ports,
  179–186 s in one file); no bound beyond the file bound.
- Last-caller ports beyond 512 callers (not measured).
- Sites of one step on one port (the index check): quadratic, held at one by
  a generator check (§6).

Fixed in review r8-2's revision: an escaped pair's facts (linear, bounded at
the measured 4,096), and the shadow tool's per-pair search (one call per
port). Fixed in review r8b-1's: a first-step task statement's loop over the
`Tasks` child's `nT` ports (O(t1 × nT) per pair, unbounded; C29), one
structure holding all of a module's facts (×4–5 per doubling; C30, with at
most 16,384 facts per module), and the shadow call's input quadratic in a
`mutually_exclusive` group (one at-most-one counter, §8). The escape block
still grows ×2.1–2.3 per doubling of facts in the emitted files (×1.9–2.6 in
`scan/escS`); no bound beyond the 4,096 facts per pair.

**Estimates** (not measured): sysFloatTest (4,472 sites, 614 statements, 2
segments): ≈ 400–600 s and 6–8 GiB in one file, or parts at ≈ 3.5 GiB each;
sysBRAM0Test (3,720 uses, a 206-user port, a 204-member group): one segment,
the port within the measured range.

## 18. Primitives

| primitive | theorems |
|---|---|
| `RegN.v` (`RegUN`, `RegA` map here), `CRegN5.v` | `reg_realizes`, `creg_realizes n`, `creg_causal n`; last caller `creg_lastWins` |
| `ConfigRegN.v` (UN, A) | `LC.cfg_realizes`, `LC.cfg_causal`, `cfg_lastWins`, `cfg_comm` (new: no longer an assumed leaf) |
| `Counter.v` under Counter.bs's import, corrected annotation | `LC.counter_realizes` (right-commutative add), `LC.counter_causal`, `counter_setF_lastWins` (not wired in the emitter) |
| `RWire`, `RWire0`, `BypassWire*` | `rwire*_realizes`, `*_causal`; `rwire_lastWins`, `pulseWireOR_lastWins` |
| `FIFO2.v` (`vMk2FIFOF`, guarded) | `fifo2_realizes`, `fifo2_causal`, `fifo2_aliased`; `fifo2_clear_lastWins`, `fifo2_clear_comm` |
| `FIFO20.v` | `fifo20_realizes`, `fifo20_causal`, `fifo20_aliased` |
| `Tasks n` | `tasks_realizes`, `tasks_causal`, `tasks_aliased` |

Not covered (assumed leaves, explicit `Leaves` fields): FIFOL1, SizedFIFO,
RegFile, the BRAMs, Probe. `Leaves` is not shown inhabited: a wrong assumed
schedule or path table makes `top` vacuous, not false. The library model is
chosen by kind and checked against the instance's schedule (`cfgAnn_lib`,
`fifoAnn_sched`; trust point 5).

## 19. Examples and negatives

`Example.lean` as round 7 (the toy hierarchy, `Asm`, `WireEx`, `AliasEx`,
`Fsm`, `Show`, `UOrder`, `RdyPort`, negatives (a)–(m), `TaskRev`,
`ScanExLate.no_escape`, `h3_catches`, `ScanTasks.no_chain`, `not_okT`,
`no_pairFacts`, `no_pairFacts_all`, `EqvTest.no_trans`, `no_trans_cong`).

`LastCallerEx.lean`:

- `lastCaller_obl`: in a 2-rule parent whose order is forced (`y`'s read SB
  write), bsc's last-caller mux on `x` meets `Obl`;
- `firstCaller_not_obl`: the first-caller-wins mux in the same parent fails
  `Obl` for every order;
- `cert_two_args`: round 7's `Cert` cannot hold when two callers drive
  different arguments (so an Unsafe wire, outside the key, fails to prove);
- `no_certL_sbr_self`: a generated module cannot export `m SBR m`;
- `acc_not_realizes sbr`: an accumulating port, annotated SBR with itself
  (bug `synth-sbr`) or CF (Counter.bs `addA`), fails `Realizes` with two
  different calls in a cycle; `accC_realizes`: the same port modelled C
  realizes.

`schedlean --mutants` (round 9b): each hardware mutant of a generated file
fails (enable drop or force, argument, mux, guard read, CF order, last-caller
order, and a call condition left out of a port's enable where a
`conflict_free` check names both callers, so the two may both call in the
hardware while `cfL_c` still reads the condition); on MethTask, the rule's
task statement with a wrong enable (§12). A `conflict_free` check's report
forced on fails; dropping it or changing its message is covered by `asm`
(its enable is false wherever the check passes), so no such mutant is made:
mkAssertedSchedules' report never enabled proves, checked by hand. `scan/escS/EscS_neg_8.lean`: an escaped pair with one site's
partner left out of its keyed table fails at the decision (`decide` proves it
false), so the linear check does check every partner (C27). `scan/escT/EscT_neg_8.lean`
(cost review r8b-1 M1): the escaped pair of `EscT8` (8 task statements in
each step) with `lo` one too high fails at the second step's decision
(`escT2S_0`), so the task-site comparison checks the second step's numbering
(C29). `e2e/mejoin/` (soundness review r8b-1 M1, the reviewer's design): a
`mutually_exclusive` line with two rules on its first side stopped the emitter
until round 9b; it is now stated as one check per rule of that side, the file
proves, and `a.write` stays a last-caller port of `r1` and `r2`, as the gate's
run (`as × bs`, §5) finds.

## 20. Emission trust points

No Lean check sees these:

1. `comb`'s fidelity to bsc's netlist (`aState`): enables, muxes, RDY,
   outputs, task conditions and arguments; **a last-caller port's mux equals
   bsc's `PrimPriMux`** (arms latest caller first, selector `WILL_FIRE ∧ call
   condition`, enable their OR), in its unfolded form; bsc's netlist after AOpt
   folds the last arm into the default, which `argL_of_pmuxF`/`NF` cover
   (§8). In the e2e files the netlist is synthetic;
   an (a) read's split condition and its site condition are the same
   operator, so a too-strong (a) condition cannot fail there.
2. Completeness of `onehot`: every AND-OR mux listed.
3. `Mod.asm`'s content: only bsc's assertion list (a `mutually_exclusive`
   group as `SufAsm`, asserted to be exactly bsc's checks); a
   `conflict_free` line read with each of bsc's instrumentation wires as its
   one unconditional writer's argument (§5), in the gate and, since round 9b,
   in the file's `cfL_c` and in the hardware enable of the `$error` that
   reports it; a line's conjuncts are bsc's `aAnds` (a condition below the
   top level that reads a firing signal or a wire stops the emitter). Since
   soundness review r8b-1
   M1 the `mutually_exclusive` lines are read by structure (`mecheck`): a
   line is accepted only if re-rendering it gives it back, every stated check
   is asserted to re-render to bsc's line, and the lines' exclusions are
   asserted equal to the dumper's `mutually_exclusive (user)` list; what stays
   trusted is that bsc prints its `aAnd (aOrs …) (aOrs …)` as the dump shows.
4. The `Tasks` child numbered in bsc's Verilog order (rank order required),
   with **task calls inside methods left out by decision** (§12).
5. The library model chosen by kind and checked against the instance's
   schedule (`cfgAnn_lib`, `fifoAnn_sched`); its fidelity to the Verilog
   (ConfigRegN: start-value reads plus RegN.v) rests here.
6. A BVI interface's `hasEn`, `alwaysEn`, `alwaysRdy` and `path`.
7. **Every skipped value is the step's own** is a Lean fact (`F_sites` by
   `rfl`); what remains trusted is that the site lists are the design's.

## 21. Not proved, and open issues

1. **Generator coverage**: schedlean (round 9, the prototype's port) emits
   files for 14 of the 36 corpus designs (§11 caveat i).
2. **The model-to-design step** (§7) is an argument outside Lean; it assumes
   reads at skipped sites are side-effect free. A method called twice shares
   its skipped values (argued, no theorem).
3. **mkBufferControl's and mkMCR's `Acyc`/`top`**: blocked by the synthetic
   RegFile copy address muxes (mkBufferControl) and by `Acyc` without input
   dependencies (mkMCR, a method's enable on an RWire's path input).
   mkLookupQ and mkValueAfterAction prove `obl`; their `top` fails in the
   `Acyc` agreement proof for method-called ports (`agWF_k` where
   `calledM w (mM j) = calledM w' (mM j)` is needed), an emitter bug the old
   `RecursionError` hid.
4. **Last caller, not built:** method callers on a last-caller port (the
   theory covers them; the emitter asserts none: mkSolver3's was one until
   the emitter used bsc's rule–method disjointness); a method executing after
   a rule on such a port (fails safe, `L!order`; none in the corpus); the
   CReg, RWireSBR, PulseWireOR, FIFO `clear` and Counter `setF` laws are
   proved but not wired; bsc's AOpt-folded mux is covered in Lean
   (`argL_of_pmuxF`) but no emitter reads bsc's netlist; synthesized
   children's self-SBR methods (future work: the child's own `RealizesU` must
   prove the law; bsc bugs `synth-sbr`, `synth-cf-share`, `counter`);
   last-caller ports above 512 callers.
5. **Alias facts.** Every solver-labelled fact keeps `notEmpty` and
   `i_notEmpty` apart because the file's facts carry no `AliasOK`. No e2e file depends on the alias, but one gate verdict does: mkPrediction's
   default-mode `intraMemReqQ.enq` caller pair is exclusive only with the alias
   (§11 caveat ii). Admitting such a pair needs `AliasOK` premises on the
   emitted caller-exclusivity facts (the proofs use them only under `Env`); not
   built.
6. `skel` and the consistency check are not emitted; `skel` is measured
   through atom tests (the repaired atomizer, §14, §15), which, like round
   7's, drop everything after `schedule_sound`. Path B is defined, not built.
7. Working-set checkpoints are not proved; full-state checkpoints remain,
   bounded by the gate's slot, segment and code limits. One segment's
   within-segment growth is unexplained; the segment bound holds it.
8. Conditions over hardware wires: not expressible; sysFloatTest's 10 facts
   stay at risk in the default and `-no-aggressive-conditions` modes.
9. The 26 corpus designs without site conditions were compiled in the default
   mode only (plus mkUrgencyBlocking and mkMethodRelations in all four).
10. The readiness, escape and caller-exclusivity verdicts are cadical over
    an abstract skeleton, carried as hypotheses. With a concrete netlist the
    generator could prove them in the file with `bv_decide`, as today's flat
    file proves its facts; which it will do is open.
11. `oblU_weakens`/`oblU_weakensX` derive results and the call superset;
    legality, readiness and `u`-independence of the extra calls are the
    obligations weakening adds. Neither is used in a proof.
12. More primitives (FIFOL1, SizedFIFO, RegFile, BRAMs), reset cycles (the
    theorem covers cycles outside reset), several clock domains (not checked),
    gates a child drives (GatedClock and the other clock generators have no
    model), multi-cycle traces; `Leaves` not shown inhabited; value-returning
    tasks' results in rules are free outputs; a method's value-returning task
    call stops the emitter (§12).
13. **Single-file memory** grows ×1.6–2.9 per doubling, with up to 39 %
    between runs of one file (cause not found); the 4 MB file bound and parts
    hold it, but one file just under the bound peaks at 5.8–6.6 GiB
    (`build.sh` waits for 30 GB above 2 MB). The claim rests on the shapes
    probed: cost review r8b-1 found one it did not cover (one 2.4 MB file at
    18.40 GiB, from the task loop C29 removed; now 2.27 GiB), so a
    declaration whose cost is not linear in its size can still break it. `en_pairs` is
    superlinear (bounded at 64). Parts need build support
    (`cost/tools/runparts.sh`; `leanchecker --fresh` of a last part was not
    run; every current e2e file is one part).
14. Kernel time is not separated in the e2e measurements.
15. Decisions to confirm (soundness review r7-2 minor 6): the site-value form
    for cyclic readiness literals, and the orientation search not done (§7).
16. The task-agreement flat files of round 7 were deleted from the
    scratchpad; this round's comparison regenerates them (§14, §15) with
    `quadfix/task-final.bin`. Tools the scripts depend on were being deleted by
    others: the dumper (`quadfix/shape-final.bin`, which writes the same dump
    as the deleted `havoc2.bin`) and the task-agreement generator are now
    copied into `tools/`, and every script that called `havoc2.bin` calls
    `tools/shape-final.bin` (cost review r8-1 minor 9). The bsc build the
    probes were compiled with (`bsc-fix-all/inst`) is gone, so the
    last-caller probes above 128 callers cannot be recompiled.
17. **`conflict_free` checks in a file** (round 9b, §5, C32): stated in
    `asmP`, and every escape or caller-exclusivity fact confirmed only with
    them assumed is stated under `asmP`, so `Hyps` holds of every `oc`
    (soundness review r8b-1 minor 1); a `mutually_exclusive` line with
    several rules on its first side is one check per rule of that side.
    mkAssertedSchedules (all four modes) now gets its file, which proves;
    sysMeJoin and the testsuite's sysConflictFree too. What remains:
    - the reading of bsc's line (its `aAnds` conjuncts, each instrumentation
      wire as its one unconditional writer's argument) is the generator's
      (trust point 3); a line in another form stops the emitter;
    - an `assert` fact is a named hypothesis (the solver's verdict under the
      stated checks), not derived in Lean from `cfL_c`, though for
      mkAssertedSchedules it follows from it propositionally;
    - the solver works on Boolean skeletons, so in sysConflictFreeOKLarge's
      shape (`rs[i]`, `rs[i+1]`) `i == k` and `i + 1 == k` are separate atoms:
      the exclusivity of two writers of `rs_k` is confirmed only with the
      `conflict_free` checks assumed (`assert`), as the shadow tool finds;
    - sysConflictFreeOKLarge itself stays flat (10,254 slots; the emitter now
      checks the slot bound before the scan). Its source scaled to 16
      registers gives a file that proves (1.3 MB, 57 s, 1.3 GiB); at 16, 32
      and 64 registers its `conflict_free` parts double with the size, but
      the file grows about fourfold (1.3, 5.6 and 31.8 MB): each of the n
      `rs_k.write` sites restates the shared argument `rs[i] + i`, which reads
      all n registers through a split (`hq_k`, `ea_reg_k`). Fix path: state a
      definition shared by several sites of one step once, with its value
      lemma, and refer to it.
18. **Sites of one step on one port**: the step index check (`hchkT`) is
    quadratic in them; the emitter asserts one (corpus maximum 1, §6).
19. **Synthesized children** (round 9b, §3.1): path A needs a child whose
    methods the parent uses are always ready; a parent reading a child's
    ready port keeps the child an assumed leaf (fix path: `RDY_m` as an exact
    port, §3.1). Not exported yet: value methods with arguments (and a parent
    reading one of an assumed leaf stops the emitter), methods bsc split into
    several steps. The top module's value methods stay outside its `X`. Of
    the five corpus designs with synthesized children (their children
    compiled from the sources with this bsc), no child instance reaches path
    A: mkAes's ROMs (mkSboxRom, mkInvSboxRom, mkRconRom) have value methods
    with arguments; mkPump reads mkSource's ready port (mkSource itself gets
    a file); mkDeblockFilter's children fail their own scans (mkWorkVector*,
    mkLeftVector, mkTopVector, mkbSVector: an exported CF that only an RWire
    scheduling hack realizes); mkPrediction's mkInterpolator fails `h3` and
    has a last-caller order problem; sysFloatTest's floating-point children
    pass their gates but get no `Acyc`/`top` (a method drives a FIFO's path
    input, as mkMCR), and the parent reads their ready ports. The parents'
    own verdicts are unchanged (mkAes A, the others flat for their own
    reasons); with the children's `.ba` files absent, each `GATE` record and
    row equals round 9a's but for the synthesized children leaving the
    generic leaves (they are assumed synthesized leaves) and the hierarchy
    row.
20. **always_enabled and enabled_when_ready children** (soundness review
    r9b-1 B1, §3.1): refused, and their parents stop. Fix path: export
    `alwaysEn` and emit `CertB.alw` from a rule proved to fire in every cycle.
21. **Library leaves shared by instances**: the parent's `Leaves` has one
    `sf : Module V FIFO2.X` (and one `rf`) for all its SizedFIFO (RegFile)
    instances, whose depths (sizes) may differ, so `top` covers designs whose
    instances are one module (the per-instance argument of r9b-1 M1, found
    while fixing it; synthesized children's entries are per instance since
    round 9b). Changing it changes the e2e files (round 9a parity); fix path:
    `sf : Fin (nk .sf) → Module V FIFO2.X`, as the synthesized leaves.
22. **A wide child's kind check** (fixed, cost review r9b-2 M1): `kind_synK`
    decided the scan's kind table against the child's annotation for every
    pair of its n ports, O(n² log n) kernel steps (12.2 s of a 29 s parent at
    128 ports). It is now checked by rows with bit operations
    (`kindOK_of_rows`, §3.1), about n²/64 word operations; the kind table's
    codes are still lists (`KC`), each port's turned into a mask once.
23. **One reset**: the "cycles outside reset" caveat is in the README and,
    for several resets, the file's docstring; a one-reset file keeps round
    9a's text (e2e parity), so its docstring does not say it.

## 22. Review items and where they are fixed

Tool and run names here and in §11's round-8 notes (`mke2e.py`, `sitecond.py`, `shadow.py`, `gate8/`, …)
are the Python prototype's, retired in round 9; schedlean's modules took over each check (see *Files*).

| review item | fix | evidence |
|---|---|---|
| **soundness r8b-1 M1**: `mutually_exclusive` lines read by their names, so `(a1 ∨ a2) ∧ b` gave a stronger `asm` and Lean proved the file | lines read by structure (`mecheck`, C31): `as × bs`, accepted only if re-rendered exactly; the emitter's model and the shadow tool read them alike and assert the pairs equal the dumper's list; a file takes one rule on a line's first side (else the emitter stops) and asserts each stated check re-renders to bsc's line; the gate takes the `|as|` checks `(a, bs)`; negative control `e2e/mejoin/` (emitter stops; gate and shadow tool agree `a.write` is last-caller) | §5, §11, §20, `sites/tools/mecheck.py`, `model._checks`, `shadow.py`, `e2e/regen8.log` |
| soundness r8b-1 minor 1: an `assert` fact has no form a file could state | `escape_facts` refuses `assert` outside the gate's run; §21 item 17 says a file stating `conflict_free` checks must state such facts under `asmP` | §21, `mke2e.py` `escape_facts` |
| soundness r8b-1 minor 2: item 2 on a footprint-only step is not "as a file would" | facts on such steps counted in the `GATE` line (`esc_facts_fallback`); `summary.py` fails a design with one; no verdict changed | §11 item 2, `model._gstep`, `gate8/summary.py` |
| soundness r8b-1 minor 3: how a library primitive is told apart | §3 names the three tests (soft hyphen; library package; Verilog name) and the name-collision case; `generic_kind` gives `ctrAnn` only to a library `Counter` | §3, `model.generic_kind` |
| **cost r8b-1 M1**: a first-step task statement cost O(nT), a pair O(t1 × nT), unbounded | a task site is one comparison against the second step's first statement (`isT`, `tnum`, `okP_task`, `escT2S_n`; C29), proved in the file's `okT_exceptZ`; re-measured on the review's shapes (§17); negative `scan/escT/EscT_neg_8.lean` | §6, §17, `mke2e.py`, `scan/escT/`, `../r8c/escE/` |
| **cost r8b-1 M2**: one structure field per fact, superquadratic; the facts bound calibrated without it; no module bound | facts in groups of at most 128 fields (C30), uses `hy.g<k>.…`; at most 16,384 facts in a module, asserted, in the header and computed by the gate (`mod_facts`); measured on the emitter's own output (§17) | §6, §11, §17, `mke2e.py`, `../r8c/hyps/`, `../r8c/escE/` |
| cost r8b-1 minor 1: the shadow tool's call quadratic in callers | exclusions from each caller's adjacency; a `mutually_exclusive` group one at-most-one counter; the call measured at the bound (0.96 s, 52,720 clauses at 3,296 callers) | §8, §17, `sites/tools/shadow.py`, `sites/tools/shbench.py`, `sites/shbench/run.txt` |
| cost r8b-1 minor 2: `seg_uses` and the part bound listed, not computed | `seg_uses` computed in the gate's run and checked by `summary.py`; the part bound is an emitter check, no longer listed as a gate item | §11 item 6 |
| cost r8b-1 minor 3: `genbig.py`, `genport8.py` stale | `synth`/`bvi` added; `genport8.py` writes bsc's rendering of a one-rule list and the dumper's pair list | `cost/big/genbig.py`, `cost/port/genport8.py` |
| cost r8b-1 minor 4: the escape series' labels | §17 says what the kernel column covers; per-fact cost from emitted files | §17 |
| cost r8b-1 minor 5: the fixer summary's diff wording | this revision's summary and `e2e/README.md` list every change to the files | `e2e/README.md` |
| cost r8b-1 minor 6: `skel` ratios as one period's values | ranges across both periods | §14 |
| **soundness r8-2 M1**: gate item 2 did not check call/call escape facts | item 2 is the emitter's own check (`escape_facts`) in the gate run: every failing site pair of every escaped pair, by the solver over the stated conditions (every port its own atom), a keyed bsc fact, or a `conflict_free` assertion; the `GATE` line reports the facts, how each is confirmed, and the unconfirmed ones; `summary.py` reads it; the gate rerun per mode and the counts corrected | §11, `e2e/tools/mke2e.py` (`escape_facts`, gate exit), `gate8/gatescan_*.txt`, `gate8/summary.txt` |
| **decision: user assertions** | `mutually_exclusive` (every check line of the form, several per step) and `conflict_free` checks are hypotheses (`asm`) in gate items 1, 2 and 4 alike; files state their `mutually_exclusive` checks in `asmP` and list them in the first comment; the emitter stops on a `conflict_free` check; the summary lists the designs and pairs relying on one | §5, §11, `model.me_line`, `mke2e.py` `cf_roots`, `shadow.py` `two_call` |
| soundness r8-2 minor 1: the gate's Counter had bsc's annotation | `model.CTR_SCHED` = LastCaller's `ctrAnn` in the gate's generic kind (sysFloatTest's 9 extra `cntErrors.addA` pairs are excluded by its own `mutually_exclusive` checks) | §8, §11 |
| soundness r8-2 minor 2: `KNOWN_X`, mislabelled | removed; mkAssertedSchedules' `outQ.enq` pair is excluded by its `conflict_free` assertion, computed by the shadow tool and the emitter | §11, `gate8/summary.py` |
| soundness r8-2 minor 3: BVI imports treated as synthesized | synthesized children told apart by bsc's soft-hyphen interface type; a user BVI import is an assumed BVI leaf with its declared schedule (mkUseOdd: `c SBR c`, no escape) | §3, §11, `design.ba_synth`, `model.generic_kind`, `shadow.port_class` |
| soundness r8-2 minor 4: wording | §8's table (ConfigRegN only), mkMethodRelations' callers, the files' table settings printed as "setting used" | §6, §8, §11 |
| **cost r8-2 M1**: an escaped pair's facts quadratic, unbounded | the check keyed by first-step site with a one-pass partner check (`Emit.subS`, `subS_mem`, `mem_idxT`; `okT_exceptZ`, `Scan.okT_except_fnS`), decided per subtree; measured at 256–4,096 facts (×1.9–2.6 kernel per doubling, 1.68 GiB at 4,096); bounded at 4,096 facts per pair in the gate, the emitter and the file header | §6, §11, §17, `scan/escS/run.txt`, `EmitLemmas.lean`, `Scan.lean` |
| cost r8-2 minor 1: the large step's last doubling | ×2.19–2.94 and the scan section's ×4.2–4.3 heartbeats quoted | §11, §17 |
| cost r8-2 minor 2: `skel` measurable | `tools/atomize3.py` adopted; measured ratios replace the estimate | §14, §15, §17 |
| cost r8-2 minor 3: mkMCR's "20 % cheaper" | "costs nothing measurable" (on/off 0.97–1.07) | §15, §17 |
| cost r8-2 minor 4: the shadow tool's worst case | one solver call per port (`two_call`: recorded exclusions as clauses, a sequential counter), corpus maximum and worst case stated | §8, §11, §17 |
| cost r8-2 minor 5: the pairwise count | counted the emitter's way in the gate run (`pairwise` in the `GATE` line) | §11 |
| cost r8-2 minor 6: point values of spreads | memory ×1.6–2.9 per doubling with run-to-run spread; mkMCR 1.00–1.03×; segmenting 12–19 % | §14, §15, §17 |
| **cost r8-1 M1**: the gate's scan verdict and slots came from round 7's mirror, not the emitter | the gate's scan, slots, rule codes, escapes and last-caller callers now come from the emitter's own model (`E2E_GATE=1`, `gate8/gatescan.sh`; generic kinds for children it cannot emit); slots counted as the emitter does (every port of every used child); the count corrected | §11, `gate8/gatescan_*.txt`, `gate8/summary.txt` |
| **cost r8-1 M2**: the `skel` figures were not `skel` | marked not measured in round 8's revision; now measured with the repaired atomizer (cost r8-2 minor 2) | §14, §15, §17 |
| cost r8-1 minor 1: a 6.6 GiB single file under the 4 MB bound | single-file peak stated; `build.sh` waits for 30 GB above 2 MB | §17, `build.sh` |
| cost r8-1 minor 2: shadow tool quadratic, crashes on a timeout | per port (since cost r8-2 minor 4 one solver call per port); a timeout is unknown in every tool (`satc.Enc.sat`) | §8, §17 |
| cost r8-1 minor 3: last-caller detection ignored rule–method disjointness | `excl_lc` uses bsc's disjoint guards and urgency between a rule and a method | §8; mkSolver3 now reaches its scan (`h3`) |
| cost r8-1 minor 4 / soundness r8-1 minor 4: last-caller callers counted from satisfiable pairs | every user of the port, as the emitter (shadow tool and `gatescan`) | §11 |
| cost r8-1 minor 5: "linear" | factors per doubling with loads | §11, §17 |
| cost r8-1 minor 6: `Bcfq` | called a site shape, with what it lacks | §17 |
| cost r8-1 minor 7: escape bound and table settings not asserted | asserted and written into the file (C24) | §6, `emit/conventions.md` C24, every e2e file's header |
| cost r8-1 minor 8: load-dependent ratios | ranges across both comparison periods | §14, §15 |
| cost r8-1 minor 9: deleted tools | dumper and task generator copied into `tools/`; scripts repointed | §21 item 16 |
| soundness r8-1 minor 1: "keyed by the annotation" | §8 says what Lean checks (`lc_ok` on the model's annotation, soundness from the model's `Realizes`); `lcKey`/`LastWins` called reference facts in §8 and `LastCaller.lean`; why RegN/RegUN/RegA need no schedule check | §8 |
| soundness r8-1 minor 2: assumed synthesized leaves | stated in §3 and `Built`'s docstring; the emitter asserts every assumed leaf is a library primitive; the gate's generic kinds make a synthesized child's action methods C with themselves | §3, `Modular.lean`, `mke2e.py` `ASSUMED_LEAF` |
| soundness r8-1 minor 3: shadow `L`/`I` too wide, SB between methods | `L` only on the decided list with `lcKey` on the instance's schedule, other self-SBR `U`, `I` blocked, SBR (or C) required between methods | §8, `sites/tools/shadow.py` |
| soundness r8-1 minor 5: a left-out method task's result | `model._tasks` admits only result-free system tasks in methods | §12 |
| soundness r8-1 minor 6: bsc's AOpt-folded mux | `argL_of_pmuxF`, `argL_of_pmuxNF` (and `pmux_off`) | §8, §20, `LastCaller.lean` |
| soundness r8-1 minor 7: stale wording | `Leaves` docstring from the actual assumed kinds; §8 reworded | §6, §8 |
| **decision: last caller, primitives only** | `CertL` (`argL`/`argN`), `obl_of_certL_at`, `oblU_of_certL`; block lemma, `realizes_lastWins`, `lcKey`, per-primitive laws; ConfigRegN and Counter models; emitter C25; shadow classes | §2, §8, §16, `LastCaller.lean`, `LastCallerEx.lean`, `e2e/MkUrgencyBlocking*.lean`, `e2e/MkMCR.lean` |
| **decision: skip task calls in methods** | `model.py` `mtasks`, emitter note, file comment, `Realizes`/`Built.top` docstrings | §1, §12, `e2e/mtask/` |
| **cost r7-2 M1**: sysConflictFreeOKLarge's 3,074-site step; no whole-file bound | statements counted (`gatecount.py`); large steps ×1.93–2.94 per doubling on this emitter (C22), step bound 4,096 measured to 4,098 sites; whole-file bound 4 MB with parts (C23); the design's shape proved in 6 parts at 3.36 GiB | §11, §17, `cost/big`, `emit/conventions.md` C22–C23 |
| **cost r7-2 M2**: wide ports superlinear, unbounded | group masks (C20, `exR_of_cmask`), per-subtree decisions, 128-entry tables (C21); ports and groups bounded at 3,296 users (proved in parts), pairwise ports at 64 | §11, §17, `cost/port` |
| cost minor 1: segment calibration | 25–37 ms/use per checkpointed 4,096-use segment at S = 8,192 (the review's runs); the bound unchanged | §11, §17 |
| cost minor 2: units | GiB throughout; the step bound recalibrated on this emitter's own series (`Bu`), run twice at 2,048 and 4,096 sites | §17 |
| cost minor 3: mkBufferControl's ratio | re-measured back to back with regenerated flat files and atom tests | §15 |
| cost minor 4: pairwise `SELMUX` | chain from the group (`amoChain_le_range`); `en_pairs` bounded at 64 users | §17 |
| cost minor 5: `inT` one decision | split per subtree | §17 |
| cost minor 6: `E2E_STEP_SITES` raises | every bound asserted; a variable may only lower it; bounds written into the file | §6, C24 |
| cost minor 7: emitter coverage | seven `RecursionError` designs now emit; coverage stated beside the count | §11, §21 |
| cost minor 8: `port7` not today's output | `cost/port/genport8.py` on today's emitter | §17 |
| cost minor 9: segmenting is cheap | `SysArithPipeSeg` measured beside `SysArithPipe` | §14 |
| soundness r7-2 minor 1: `'eq'` literals | `EncS.eqatom`; negative controls `eqcase`, `eqsel` | §7 |
| minor 2: ready literal | one literal (the file's), `Design.exact()`; control `alias` | §7 |
| minor 3: bsc keys | `BscFacts.covers` (both steps, chains), stated conditions imply bsc's; labels by source | §6, §7 |
| minor 4: wire literals in `R` | dropped and re-checked; asserted absent; control `hwlit` | §7 |
| minor 5: `oblU_weakens` form | `oblU_weakensX` over `withSitesX` | §7 |
| minor 6: decided rule changed | recorded for Ravi's agreement | §7, §21 |
| minor 7: carried over | disclosed: verdicts as hypotheses, (a) split, trivial `trU` in sysArithPipe, `Leaves`, site values on no path-A module | §7, §14, §18, §20, §21 |
| round 7 and earlier | as in `../r7/design.md` §20 | — |
