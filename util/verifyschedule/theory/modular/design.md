# Round 6: the modular proof

Each module bsc compiles gets a contract, `Realizes`, stated over its own
interface: its method ports and the annotations it exports. A module built
from child instances meets its contract when two things hold:

- every child meets its own contract and respects its output aliases;
- the module meets a per-module obligation, `Obl`, which mentions the
  children only through their interfaces.

The composition theorem is proved once in Lean. Induction over the instance
tree then gives the top module's contract, together with the fact that every
input has a hardware cycle (`Causal`, from a decided acyclicity certificate
`Acyc`). Cycles that violate an assertion are excluded (`Mo.ok`); the
consistency check (§5) rules out an assertion set that excludes every cycle,
as today's does.

```lean
theorem Built.top [Inhabited V] {X : Iface} {Mo : Module V X} (h : Built X Mo) :
    Realizes Mo ∧ ∀ s w, Mo.clean s = s → ∃ o s' g, Mo.hw s w o s' g
```

What this round adds (revision 2, after the round-6 reviews):

- **System tasks** (decision 5): each module's task calls are part of its
  contract, compared with that module's calls in the one-at-a-time run
  (§11). The scan checks them in linear time as a *chain kind* (§10), and the
  end-to-end module carries all 690 of its task statements (§13).
- **The scan, proved** (decision 3): `pairFacts_of_scan` builds every pair
  field of the certificate from a kernel-decided, checkpointed scan (§10).
- **Site conditions** (decision 1): schedlean's own conditions, every one
  checked, with skipped values per step; the readiness rule; four bsc
  compilation modes measured (§7).
- **Emission conventions** (decision 2) that keep a module's check linear
  in children, ports and sites (§8).
- **One real module end to end**: sysArithPipe (227 rules, 83 children of
  8 kinds, 1,859 sites including 690 task statements), emitted by a script
  from its dump, `Obl`, `Acyc` and `Built.top` proved, measured against
  today's flat file from the current generator (§13).

§19 maps each review item (round 3 and round 6) to its fix.

## Files (all in this directory)

| file | what | lines | check |
|---|---|---|---|
| `Modular.lean` | the model, composition, `Acyc`, `Built`, primitives, `Cert`, `obl_of_cert`, task traces (§1–§5, §11, §14) | 4,112 | 16.7 s, 0.76 GB |
| `Fifo2R.lean` | guarded `FIFO2` with `first` always ready, proved against the `FIFO2.v` model (§7, §14) | 242 | 8.2 s, 0.54 GB |
| `EmitLemmas.lean` | `Tab` (balanced tables), numbered site lists, per-port facts, children by kind, read-list filters (§8) | 457 | 1.7 s, 0.53 GB |
| `Scan.lean` | the scan and `pairFacts_of_scan`, chain kinds, glue, exception groups, escapes, muxes and chains from checks (§10) | 2,105 | 4.8 s, 0.64 GB |
| `SiteCond.lean` | unconditional sites, step-level refinement for weakened and dropped conditions (§7) | 391 | 0.9 s, 0.52 GB |
| `Example.lean` | the toy hierarchy and its negatives, task traces (§16) | 2,845 | 21.2 s, 1.00 GB |
| `Scale.lean` | 4,096 children and rules indexed by `Fin n` | 170 | 0.4 s |
| `split/` | the toy as per-module files (§12) | 471 | 0.2–1.9 s each; `leanchecker --fresh` 43.1 s |
| `examples/ScanEx.lean`, `ScanExLate.lean` | the scan on a concrete parent; an escaped pair; a caught `h3` violation | 438 | ≈ 1 s each |
| `examples/ScanTasks.lean` | task statements through the scan as a chain kind: positive, and statement order against rank order | 243 | 1.0 s |
| `examples/EqvTest.lean` | value/value comparisons need transitivity (§4) | 49 | 0.4 s |
| `e2e/` | sysArithPipe end to end: emitter (`tools/`), `SysArithPipe.lean` (with tasks), `SysArithPipeNoTasks.lean`, today's file (`flatnew/`), logs (§13) | 23,193 | 102–113 s, 2.55 GB |
| `emit/` | the emission-convention probe `Em<m>` and `conventions.md` | — | `emit/out/series.txt` |
| `scan/` | scan benchmarks, escape probes, the corpus mirror, chain-kind task probes (`scan/tasks/`), checkpoint data (`scan/tools/wset.py`) | — | `scan/bench/*.log`, `scan/tasks/run2.log` |
| `sites/` | the site-condition tool, corpus results, `rule.md`, the four compilation modes (`sites/modes/`) | — | `sites/results.txt`, `sites/modes/out_*/` |
| `build.sh` | everything above in dependency order, logged to `build/all.log` | — | — |
| `prev3/`, `prev-int1/` | round 5's design; this round's first integration (for diffing) | — | — |

Import order: `Modular` ← `Fifo2R`; `Modular` ← `EmitLemmas` ← `Scan`;
`Modular` ← `SiteCond`. A generated module file imports `Scan` (and `Fifo2R`
when a site needs the always-ready `first`). Everything is core Lean 4.35
(plus `Std.Tactic.BVDecide` where `bv_decide` is used): no Mathlib, and no
`sorry`, `admit`, `axiom`, `native_decide`, `implemented_by` or `unsafe`.
Every Lean run used `prlimit --as=25769803776`, `timeout`, `-j1` and
`-DElab.async=false`, one process at a time, on a shared machine (load 10–32
on 32 cores; wall times vary by about ±20 %).

Every `#print axioms` lists at most `propext`, `Classical.choice` and
`Quot.sound`, plus `bv_decide`'s native axiom only for theorems built on a
`bv_decide` (`Ex.Asm.skel` and the theorems built on it). `Scan.lean`,
`EmitLemmas.lean` and `Fifo2R.lean` print their main theorems' axioms.

## 1. The model and the contract

```lean
structure Ann (M : Type) where
  prec sbr conf : M → M → Bool         -- SB/SBR, SBR, C
  share hasEn alwaysEn alwaysRdy : M → Bool
  same   : M → M → Bool := …false      -- ret p and ret q are one output wire
  rdyRet : M → M → Bool := …false      -- rdy a is the wire whose 1-bit value ret p carries
  path   : M → M → Bool := …false      -- combinational path from p's inputs to q's outputs

structure Module (V : Type) (X : Iface) where
  S R : Type
  meth : X.M → V → S → V → S → Prop
  rule : R → S → S → Prop
  clean : S → S
  clean_idem : ∀ s, clean (clean s) = clean s
  Sig : Type
  hw : S → Wires X.M V → Outs X.M V → S → Sig → Prop  -- the Verilog's cycles, no assertions
  ok : Sig → Prop                                    -- the assertions below the module hold
  L : Type := Empty                                  -- task logs (§11)
  log : S → L → List (TaskCall V) := …               -- the run's task calls this cycle
  trc : Sig → L → List (TaskCall V) := …             -- the hardware's, in statement order
  log_clean : ∀ s l, log (clean s) l = []

def Realizes (Mo : Module V X) : Prop :=
  ∀ s σ w o s' g, Mo.clean s = s → Legal X.ann σ → Drives X.ann w σ → Mo.hw s w o s' g → Mo.ok g →
    (∀ c ∈ σ.flatten, X.ann.alwaysRdy c.1 = false → o.rdy c.1 = true) →
    ∃ τ fin, groups τ = σ ∧ Exec Mo o s τ fin ∧ s' = Mo.clean fin ∧ ∀ l, Mo.trc g l = Mo.log fin l
```

- `Realizes` covers the hardware cycles in which the assertions hold
  (`Mo.ok g`); `Mo.ok` unfolds to each instance's `asm` on its own signals.
- `Realizes` is existential over the run's rules and their order. It does
  not say which rules fired: agreement on state, results and task calls is
  the content.
- Argument-less value methods are one port read any number of times; ports
  with arguments have one port per copy, and `share` makes a copy's calls
  agree; a self-SBR action port drives the last call's argument; readiness
  is assumed at the cycle's start and enabledness is proved at the call's
  position.

## 2. Built modules and the obligation

```lean
structure Parent (V : Type) where
  I : Type; decI; kids; kids_nodup; kids_all
  CI : I → Iface;  X : Iface;  PR : Type
  body : X.M → V → Prog I (fun j => (CI j).M) V
  rbody : PR → Prog I (fun j => (CI j).M) V
  comb : ((j : I) → Outs (CI j).M V) → Wires X.M V → ((j : I) → Wires (CI j).M V) × Outs X.M V
  asm onehot : ((j : I) → Outs (CI j).M V) → Wires X.M V → Prop
  toB : V → Bool := …

def Obl (P : Parent V) : Prop :=
  ∀ oc w σ, P.asm oc w → (∀ j, AliasOK P.toB (P.CI j).ann (oc j)) → Legal P.X.ann σ → Drives P.X.ann w σ →
    (∀ c ∈ σ.flatten, P.X.ann.alwaysRdy c.1 = false → (P.comb oc w).2.rdy c.1 = true) →
    P.onehot oc w ∧ ∃ τP, groups τP = σ ∧ (∀ e ∈ τP, StepOK P oc (P.comb oc w).2 e) ∧
      ∀ j, Legal (P.CI j).ann (sigmaJ P oc τP j) ∧ Drives (P.CI j).ann ((P.comb oc w).1 j) (sigmaJ P oc τP j) ∧
           ∀ c ∈ (sigmaJ P oc τP j).flatten, (P.CI j).ann.alwaysRdy c.1 = false → (oc j).rdy c.1 = true
```

`P.toModule impl` has the children's states, signals `PSig`, task logs
`Σ j, (impl j).L` (each child's kept apart), and hardware: each child's
`hw` on its wires, enables and RDY from `comb`, and data from `comb` under
`onehot`. Its rule `r` is `∃ v, RunP P impl (P.rbody r) s v s'`: the run of
the rule's program on the children's states (used in §7).

## 3. The composition and hierarchy theorems (proved once)

```lean
theorem compose (P) (impl) (hC : ∀ j, Realizes (impl j)) (hA : ∀ j, Aliased P.toB (impl j)) (hO : Obl P) :
    Realizes (P.toModule impl)
theorem causal_toModule [Inhabited V] (P) (impl) (hC : ∀ j, Causal (impl j)) (A : Acyc P) : Causal (P.toModule impl)
inductive Built : (X : Iface) → Module V X → Prop
  | leaf (Mo) : Realizes Mo → Causal Mo → Built X Mo
  | node (P) (impl) : (∀ j, Built (P.CI j) (impl j)) → (∀ j, Aliased P.toB (impl j)) → Obl P → Acyc P →
      Built P.X (P.toModule impl)
```

- A BVI leaf brings its assumed contract and causality (`Built.leaf bvi hR
  hC`); its `path` table comes from its `VModInfo`; **BVI interfaces declare
  no aliases** and get `Aliased` from `aliased_of_none`.
- **A module off path A (§10) enters its parents the same way**: as a leaf
  whose `Realizes` and `Causal` are hypotheses of the parent's `Env` record,
  named in the top theorem. Today's flat theorem for that module is the
  evidence for the hypothesis, but Lean does not connect the two (the flat
  theorem is a per-cycle statement, not `Realizes`). The top theorem lists
  every such module, as it lists BVI leaves.

## 4. Values, operators and constants

One abstract `V`, each module's operators a parameter (`Mod.Ops V`), and a
Boolean skeleton with data comparisons as atoms.

- **Constants:** `EqC V` with `eqc : V → Nat → Bool` and its law (distinct
  constants exclude each other); each operand compared with two or more
  constants gets one at-most-one chain (`amoChain_of_law`).
- **Comparisons between two values:** `EqV V E` (`examples/EqvTest.lean`)
  with reflexivity, symmetry, transitivity and congruence with the module's
  own constants `E`; each law is stated as a Boolean fact only at the
  operand pairs and triples that occur. **Transitivity is needed**: a
  parallel mux selecting on `x == y` and on `y == z && x != z` is one-hot by
  one `bv_decide` with the transitivity fact (`onehot`), and false without it
  (`no_trans`).
- bsc's solver verdicts stay hypotheses (`Mod.Hyps ops`), as today.

## 5. Non-vacuity, assertions, parallel muxes

- **Causal totality** (proved once): `Acyc` ranks the children's outputs so
  that every read through a child's path has a lower rank, and
  `causal_toModule` evaluates the fixpoint (`Ex.Loop` shows why it is needed).
- **Assertions are visible:** `asm` is not inside `hw`; `Realizes` assumes
  `Mo.ok`.
- **The consistency check** refutes all hypotheses together (`Mod.asm`,
  `Hyps`, EqLaw chains, each library child's output equations on
  `(comb oc w).1 j`), as today's does with its `Primitives` facts. Not emitted
  yet (no generator).
- **Parallel muxes:** `Cert.onehot` assumes only `EnvC` (assertions, aliases,
  exported C pairs), never the parent's own RDY.
- **Reset:** RST is not modelled; the theorems cover cycles out of reset.

## 6. The headline theorem of a generated module file

As emitted for sysArithPipe (`e2e/SysArithPipe.lean`):

```lean
abbrev P (ops : Ops V) (E : EqC V) (u : Nat → Nat → V) : Parent V := …   -- u k: rule k's skipped values (§7)
structure Hyps (ops : Ops V) (E : EqC V) : Prop where disj : …         -- bsc's disjointness verdicts
theorem obl (ops E u) (hy : Hyps ops E) : Obl (P ops E u) := obl_of_cert _ (cert ops E u hy)
def acyc (ops E u) : Acyc (P ops E u) := …
structure Leaves (V : Type) where fifo fifoR fifoC cfg cfgR cfgC …     -- assumed leaves (§14)
theorem top [Inhabited V] (ops E u) (hy : Hyps ops E) (lv : Leaves V) :
    Realizes ((P ops E u).toModule (fun j => implK ops lv j.1)) ∧
    ∀ s w, … .clean s = s → ∃ o s' g, … .hw s w o s' g := (built ops E u hy lv).top
```

`implK` maps the `tasks` kind to the library's `Tasks 690`, so `top`'s
`Realizes` includes the task-log conjunct for the module's 690 statements.

## 7. Site conditions (decision 1)

A step is a list of sites (`Site`: child, port, condition, argument over
the read environment `ρ`); `runSites` makes it a program; `trace_runSites`,
`envHw_at` and `calls_sites` are proved once. A site whose condition fails
is skipped: its value is the skipped value for that step and position.

**Skipped values are per step** (soundness review M1). A generated file's
parent takes `u : Nat → Nat → V`, and rule `k`'s program is `runSites (u k)
… (u k)`, so two steps never share a skipped value; the obligation is proved
for every `u`, that is, for every assignment of skipped values per step.
With one shared `u` a file could pass by relying on two steps seeing the
same skipped value (the reviewer's `USharing.lean`). A parent method has one
program and one skipped-value function; calling it twice in a cycle runs that
program twice, and no `Cert` field relates two calls of one method
dynamically (the per-method facts are about `trM P oc w m`, its hardware
unfolding, of which there is one per cycle), so the two calls cannot exploit
a shared value. The e2e and the emission probe (`emit/gen/mkemit.py`, `Em16`)
both use per-step skipped values.

**Every condition is checked, never trusted.** The obligation must hold for
every assignment of skipped values per step, so a condition that is too
strong (the value is used when the condition is false) makes a fact that
depends on the skipped value fail. A condition that is too weak only adds
obligations: legality and readiness of the extra taken reads, `share`
arguments, an escape's `hbad`. `SiteCond.lean` proves the step-level part of
this: `trace_weakens` and `dropLater_sound` (every run of a weakened step is
a run of the exact step at some skipped values, with a superset of its calls,
and dropping later-site literals gives `SitesOK`). No theorem links these
lemmas to `Obl` or `Realizes`; soundness of any condition, weakened or not,
is the every-assignment argument above.

**The conditions.**

- **Action calls:** their exact call conditions (the enable logic, exact in
  the APackage). Never weakened. Task calls are action calls on the
  module's `Tasks` child (§11); a run-time-check rule's `$error` statement
  has the rule's own guard as its condition, so its site is unconditional in
  the rule and its enable is `WILL_FIRE && guard`.
- **Value reads: unconditional by default.** A read gets a condition only if
  it needs one:
  - **(a)** it fails the static footprint scan (an out-of-order read);
  - **(b)** a shared multi-port copy that two firing steps read with
    different arguments;
  - **(c)** the earlier read of a value/value read flag;
  - **(r)** readiness: the port has a ready signal (guarded FIFO2's `first`,
    a generated child's `RDY_m`) and the step's guard does not imply it.
- **Exact hardware conditions** never apply in the corpus: bsc makes the
  users of one shared value copy exclusive by construction (`RSchedule`), so
  (b) is empty, and a `PrimPriMux` select reads other steps' `WILL_FIRE`.
- **(a), (b), (c): schedlean's conservative approximation** (the generator's
  `<def>@reaches@<copy>` rule, "x && false still counts x"): `&&` and `||`
  pass both operands on unconditionally; only an arm of an `if`, `case`, mux,
  priority mux or array selection is conditional on its select. bsc's use
  conditions play no part.
- **(r): the readiness rule** (Ravi, 2026-11-02). Short-circuit reach: in
  `a && b`, `b`'s condition includes `a`; in `a || b`, it includes `!a`. The
  operand that reads the site's own ready signal comes first; other operands
  go left to right. Where that fails, operator orientations are searched (up
  to three flipped operators). Then:
  - The condition the file uses is **the ready literal itself** (`i_notEmpty`,
    `RDY_m`), read first in the step. Under the guard this is exact: the check
    `guard ∧ R_sc ∧ ¬RDY` is UNSAT, so `R_sc ⇒ RDY` wherever the rule fires,
    and `RDY` is a weakening of `R_sc` there. It needs no literal dropping,
    because the ready read has no predecessors. (Ravi's rule taken literally,
    the short-circuit formula with later-site literals dropped, recovers only
    14 of the 24 sites: its literals are whole operands that read other
    FIFOs' `first`, and dropping them loses readiness.)
  - **Only where that cannot recover a site**, guarded FIFO2's `first` gets
    the always-ready contract `FIFO2R` (`Fifo2R.lean`, proved:
    `fifo2r_realizes`, `fifo2r_causal`, `fifo2r_aliased`, standard axioms).
    `FIFO2.v` drives `D_OUT` from the head register; when the FIFO starts the
    cycle empty, `first` returns that register's start-of-cycle value, in the
    hardware and in the run. The read then needs no condition and no
    readiness. **Sites that need it** (2 of 24): mkPrediction
    `RL_outputing / outstatefifo.first` and mkBufferControl
    `RL_inputing / shortTermPicList_returnList.first`.
- **Unguarded FIFOs** (`fifof_ToUGFifof`, `fifof_ToGFifof`): `notFull =
  i_notFull` and `notEmpty = i_notEmpty`, CF with `enq`/`deq`; a user's status
  read sees the start-of-cycle value. They are modelled with that semantics
  and no readiness obligation (unsafe by Ravi's call), as guarded FIFO2's `i_`
  reads already are. **FIFOL1 is different**: its `FULL_N` is `!empty_reg ||
  DEQ`, so its `i_notFull` sees a same-cycle `deq` (`deq SBR i_notFull` with a
  path `deq → i_notFull` in the e2e's assumed contract).

**Readiness, corpus** (default bsc flags, 24 readiness sites, `sites/modes/out_default`):

| design | sites | conservative reach | bsc's use condition | readiness rule | needs `FIFO2R` |
|---|---|---|---|---|---|
| mkBufferControl | 5 | 0 | 4 | 4 | 1 |
| mkPrediction | 9 | 3 | 6 | 8 | 1 |
| mkDeblockFilter | 9 | 9 | 1 | 9 | 0 |
| mkMCR | 1 | 1 | 1 | 1 | 0 |
| total | 24 | 13 | 12 | 22 | 2 |

**mkPrediction `RL_outputing`, resolved** (`sites/out_trace/mkPrediction.txt`,
`SITES_RDY_TRACE=1`). The two sites that failed even under bsc's use
conditions, `infifo_ITB.first` and `predictedfifo.first`, **hold under the
readiness rule**: `guard ∧ R_sc ∧ ¬RDY` is UNSAT for both. bsc's use
condition for `infifo_ITB.first` is `use_cond_207 || (!outfifo.i_notFull &&
infifo_ITB.i_notEmpty && currMbHor < picWidth)`, and its witness (cadical,
data comparisons as free atoms) set `nextoutputfifo.first == 0` with
`infifo_ITB` empty: there the value of `infifo_ITB.first` feeds only `&&`
operands whose other side is false, so it is not needed. bsc's use analysis
counts such operands as uses; it is weaker than the need, not wrong. So it is
neither a model gap nor a bsc lifting problem: bsc's RDY lifting covers every
place the value is needed. The remaining site, `outstatefifo.first`, fails the
readiness rule but holds under bsc's own use condition: the value reaches
`interBSoutput && … && outstatefifo.first == 2` (`…_d678`), whose readiness
operand is elsewhere in the guard. Witness: `outstatefifo` empty,
`nextoutputfifo.first == 0`, `infifo_ITB.first[41:40] == 2`, `outPixelNum ==
0`. Under our rule it takes `FIFO2R`.

**The four compilation modes** (bsc-fix-all build 5e4395b1, `bsc -u -elab
-verilog` plus each design's testsuite RTS flags; `sites/modes/compile.sh`,
flags in `sites/modes/<mode>/flags.txt`). bsc commit 0f50f1ab made
`-aggressive-conditions` and `-sched-conditions` the default; the corpus
`.ba` files (hv/new4) are the default mode, and every corpus number in this
document comes from it unless a mode is named. Conditional reads as
(a) + (r), readiness sites left for `FIFO2R` under the readiness rule, and
dynamic legality facts at risk:

| design | default | `-no-aggressive-conditions` | `-no-sched-conditions` | both off |
|---|---|---|---|---|
| mkBufferControl | 30 + 5, left 1, risk 0 | 0 + 0 (209 sites, another schedule) | 0 + 5, left 1, risk 0 | 0 + 0 (209 sites) |
| mkPrediction | 5 + 9, left 1, risk 0 | 0 + 0 (349 sites) | 0 + 9, left 1, risk 0 | 0 + 0 (349 sites) |
| mkDeblockFilter | 0 + 9, left 0 | 0 + 0 | 0 + 9, left 0 | 0 + 0 |
| mkMCR | 0 + 1, left 0 | 0 + 0 | 0 + 1, left 0 | 0 + 0 |
| mkAssertedSchedules | 1 + 0, risk 0 | 1 + 0, risk 0 | 0 + 0 | 0 + 0 |
| sysFloatTest | 10 + 0, risk 10 | 10 + 0, risk 10 | 0 + 0 | 0 + 0 |
| sysConflictFreeOKLarge | 1,024 + 0, risk 0 | 1,024 + 0, risk 0 | 1,024 + 0, risk 0 | 1,024 + 0, risk 0 |
| sysArithPipe | 0 + 0 | 0 + 0 | 0 + 0 | 0 + 0 |

- Readiness sites are an aggressive-conditions effect: with
  `-no-aggressive-conditions` (MatX's builds) every called method's RDY is in
  the guard and none remain.
- The out-of-order reads of mkBufferControl (30) and sysFloatTest (10), and
  sysFloatTest's 10 facts at risk, are a sched-conditions effect: they vanish
  with `-no-sched-conditions`.
- sysConflictFreeOKLarge's 1,024 reads are conditional in every mode, and
  every one of their facts matches a fact bsc confirmed.

**Conditions over hardware wires** (asked for sysFloatTest: skip the read
exactly when the conflicting earlier call is enabled in hardware, as the flat
file's `(!fire_S || !EN_c) -> tok = hw` does). **The site model cannot express
them; an extension might, but it is not built.** The precise argument, from
the Lean definitions:

1. `Site.cond : (Nat → V) → Bool` reads only `ρ`, the values the step's own
   earlier calls returned (`runSites`, `Prog.callIf`).
2. `Parent.rbody : PR → Prog …` takes no cycle signal, and
   `(P.toModule impl).rule r s s'` is `∃ v, RunP P impl (P.rbody r) s v s'`
   (definition, §2): a rule's calls and skips are determined by the
   children's states at its position and the values they return.
3. A hardware wire such as `EN_c` (the earlier step's call enable) is an
   output of the cycle, not a value the step reads. Two cycles can agree on
   everything a step sees and differ on the wire: take rule `S` that writes
   register `a` with its own value under a guard on another register, and
   rule `T` that reads `a` after it. In a cycle where `S` fires and one where
   it does not, from the same `a`, `T`'s position state and read values are
   identical, while `EN_S` differs. Any function of what `T` reads makes the
   same skip decision in both; "skip iff `EN_S`" does not.
4. Rebuilding the wire from reads inside `T` would need reads of the state
   `EN_c` was computed from at the cycle's start; for sysFloatTest those are
   FSM state registers earlier steps write, so the reads would themselves be
   out of order.

An extension would add a nondeterministic site to `Prog` (taken or skipped,
the hardware's choice used in the obligation) and let `Module.rule` quantify
over the choice. Its contract would say: for every assignment of skipped
values there is a run, with some skip choices, matching the hardware. That
looks as strong as today's every-assignment argument for total, side-effect-
free value methods, but it changes `Prog`, `RunP` and `compose`, and it is not
proved; until it is, the conservative reach condition stays the rule for (a)
and sysFloatTest's 10 facts stay at risk in the default mode (none with
`-no-sched-conditions`).

**The order and the argument** (`sites/rule.md` §2). Sites are numbered
from 1; `ρ 0` holds the method's argument (`SiteCond.envHw_arg`). Hard edges
are data dependence and the child's in-step SB order. The generator fixes
the order and drops any select literal that reads a site not earlier,
another step's `WILL_FIRE` or a foreign input, so `SitesOK` holds by
construction (`sitesOK_dropLater`; an action's exact condition is unchanged,
`at_eq_exact`). In the corpus no (a) condition needs a drop; readiness
conditions read only the ready signal, placed first.

**Use-level solver facts** are stated over these conditions; schedlean asks
bsc's solver itself (planned through the interface it already uses for
unrecorded disjointness; the tool's verdicts so far are cadical over the
abstract skeleton, exact when UNSAT). Of bsc's 3,241 confirmed use-level
facts: 1,439 action/action (unchanged), 750 no longer needed, 1,036 with an
equivalent condition, 16 re-asked (6 hold, 10 fail, all sysFloatTest).

## 8. Emission conventions (decision 2)

Measured in `emit/` (probe from 35 to 2,051 children, user time ≈ n^1.03,
memory ×2.15–2.3 over the last doubling to 6.3–7.1 GB at 4,099 sites) and
used throughout `e2e/SysArithPipe.lean`. The full text is
`emit/conventions.md`; the rules:

1. **Children indexed per kind**: `I := Σ κ : Kind, Fin (nk κ)`; instance
   terms through a helper (`iReg i`); many rules: `PR := Fin nR`.
2. **Every table is a closed term** (`Tab`, balanced); leaf lambdas annotate
   their binders.
3. **`comb` goes through per-kind arm tables built from per-port
   definitions**; shared comb nodes are their own definitions. **No proof
   unfolds `comb`.** The `Tasks` child's enables and arguments are two closed
   tables (`tenT`, `targT`) indexed by statement.
4. **Per-instance facts by a balanced proof tree** (`Tab.AllR`), one
   dispatch lemma (`Tab.get_allR`); task statements by one table of per-
   statement facts (`TQ`, `tqAll`). The index-list facts a port's enable and
   argument proofs both need are decided once per (rule, port) and shared.
5. **`Acyc` read lists are filters of one complete list** (`dep_of_all` for
   inputs without a path; rank filters with node agreement lemmas otherwise).
6. **Steps are numbered site tables** (`keyT`, `sfT`, `kT`, `resT`):
   `mkSite`, `sitesF`, one `SFReads` leaf per site, `trace_fn` and `envHw_fn`
   with no list scan. **A rule's result is its explicit guard, read at its
   position** (`resT`, soundness review M2); `resR` is one lemma per rule
   (`rqT`) from `fire` and the guard reads' value lemmas.
7. **Ports' uses: one table, decided checks** (`decide +kernel` over closed
   data).
8. **Kernel checks** are closed `Bool` computations over tables; never
   `decide` a `List.Nodup`.
9. **Multi-user ports.** A port's enable is `orL` over its users' firing; its
   argument is a priority mux read under the mux's chain (`arg_mux`). **The
   chain comes from bsc's run-time `mutually_exclusive` checks**: members are
   ordered so that a member's check covers its later members
   (`amoChain_of_pairsC`, `forall_split`, `forall_of_check`), and only the
   pairs no check covers get a pairwise proof (cost review M2). In
   sysArithPipe 2,992 of 3,242 member pairs come from 140 check applications
   and 250 pairs (disjoint guards) are proved one by one.

## 9. What changes versus today's emitted file

| today | modular | where it goes |
|---|---|---|
| `State` fields, `Agree` | child wires `(comb oc w).1 j`; `Drives` | `Cert.en`, `Cert.arg` |
| `illegal_access`, per-step checks | `Legal` of each child's induced sequence | the scan's pair fields (§10) |
| `guard_violated` | the rule's guard evaluated at its position (`resT`) | `resR` |
| `View`, `View.at_<step>` | every read is the child's output wire `oc j` | — |
| `Congruence`: havoc views | conditional sites, skipped values per step (§7) | per use site |
| `chAlias` | `Ann.same`, `Ann.rdyRet`, `Aliased` | `fifo2_aliased`, `Env` |
| `Primitives` | library contracts and causality | `Built.leaf` |
| `Oracles` | `Hyps ops`, `∀ oc w`; exception groups | the scan, mux chains |
| constant comparisons on tokens | `EqC.eqc`; EqLaw chains | `skel` |
| `Assertions` | `Mod.asm`, visible as `Mo.ok` | `toModule.ok` |
| `Hw.oneHot` | `Parent.onehot`, from `EnvC` | `Cert.onehot` |
| `fail_if_success` | over `asm`, `Hyps`, chains and library output equations (§5) | — |
| system tasks (not modelled) | a `Tasks` child per module (§11) | `Realizes`' trace conjunct |
| read flags (`dReadFlags`) | condition (c) on the earlier read (§7) | conditional site |
| none (the flat theorem computes the hardware) | `Acyc`; `Causal` | `Built.top` |

**`dUnrealized` and bus_2m_2s.** `Obl` must cover every exported CF pair.
The module's own `Obl` is what fails for bus_2m_2s's 16 pairs: the generator
either exports the narrowed annotation (SBR, `Ann.unv`), moving the burden to
the parents (`Realizes.mono`), or leaves the module off path A, entering its
parents as an assumed leaf (§3). Either way it fails safe and the warning
bsc's `dUnrealized` records is emitted.

**Read flags**: value/value conflicts in imported modules (`dReadFlags`, the
ValueReads example) need the earlier read under a condition, category (c) of
§7. The corpus has none.

## 10. All orders, and the scan behind the certificate (decision 3)

`place_ok` and `obl_of_cert` turn the per-pair fields of `Cert` into `Obl`,
once. **`cert_of_scan` is proved** (`Scan.lean`, `pairFacts_of_scan`): from
`D.Ok`, decided by the kernel over checkpointed segments, and `GlueOK`
(linear facts), it builds every pair field of `Cert` with exactly `Cert`'s
statements, plus `rules_nodup` and `fireIn`.

- **The pass** (`Design.F`): methods first, then rules in rank order; state
  per slot (child base + port code). Per-kind conflict tables (`KC`) are
  checked against the annotation by `kindOK_of_chk`.
- **Chain kinds** (cost review M1). A child whose ports form a total `SB`
  order, such as `Tasks n`, would need `KC` tables quadratic in its ports
  (the reviewer's probe ran out of memory at n = 512). Instead `Glue` marks
  it as a chain (`ch`): all its ports share one slot (code 0, `KC ⟨[], [0],
  [], [0]⟩`, `kindOut_chain`), so the scan computes backward conflicts and
  hits as for any port, and the forward direction comes from positions:
  each step's chain calls lie in an interval `[clo g, chi g)`, the intervals
  increase with the step (`cmono`), and within a step the positions increase
  (`cinR`, `cincR`, decided per step over its keys: `chain_keys`,
  `chain_in_of_sites`, `chain_inc_of_sites`, `stF_mono`). `taskAnn_group`
  turns increasing positions into `Group`. All `*_of_scan` lemmas take the
  chain case (`mm_static_out` for a later or the same method).
  `examples/ScanTasks.lean`: a positive parent with a `Tasks 3` child, and a
  negative one whose statement order is against rank order (no intervals
  exist, and the pair's `OkT` really fails). Cost (`scan/tasks/`, one Tasks
  child, D.Ok plus the chain checks): 512 statements 0.4 s; 2,564 statements
  2.6–5.2 s; 10,240 statements 14.2 s and 40,960 statements 45.8–65.5 s of
  kernel time (140 s wall, 3.5 GB) with a checkpoint every 64 rules
  (2,560 uses). Linear: the
  reviewer's table-based probe took 12.5 s at 128 and ran out of memory at 512.
- **Exceptions are at-most-one groups** (`exR_of_chains`); in sysArithPipe
  the groups are the ten muxes' user sets and six pairs.
- **Escapes** for pairs that fail statically (the corpus's 30 `h4` pairs):
  `okT_except`, `esc_of_pairs`.
- **Corpus** (`scan/tools/corpus.py`): `grp`/`mm`/`mmg`/`h3` fail nowhere;
  `h4` fails exactly the review's 30 pairs. With task statements as a chain
  kind nothing changes: a chain adds no forward conflict.

**The gate** (decision 3; the fallback "is what it is"):

- **Path A, default:** the scan plus `h4` escapes. Admitted when the scan
  passes with `escR` carrying only escaped pairs, at most 64 escapes, and the
  module's task statements numbered in rank order (the chain condition; the
  generator checks it before emitting). No size limit on tasks. Corpus: all
  36 designs pass the scan mirror with at most 12 escapes; the mirror has no
  tasks, but as a chain they add no conflict. The rank-order condition holds
  for sysArithPipe's Verilog (trust point 4, §17); for the other designs with
  task statements it is checked by the generator, not measured here.
- **Path B, fallback:** case-split pair fields, each case `okT_of_static`
  or an `okT_except` proof (about the per-pair escape cost, 50–180 ms; more
  for wide steps, about 0.5 ms per site of the escaped step). Used when a
  `grp`/`mm`/`mmg`/`h3` check fails or escapes exceed 64, and only if `C =
  nR² + nM² + nR·nM + nR²·nM + nR·nM² ≤ 2,048`: nR ≤ 45 with no methods,
  nR ≤ 31 with one, nR ≤ 18 with four. Of the five designs with escapes it
  would admit mkAssertedSchedules (C = 169) and sysConflictFreeOKLarge
  (C = 25) and reject mkBufferControl (4,941), mkPrediction (5,401) and
  sysFloatTest. **Path B is defined, not built.**
- **Otherwise the module stays on today's flat theorem** and enters its
  parents as an assumed leaf (§3).
- The `h3` check is O(nM) per rule (O(nM·nR) per module); no corpus design
  has both many methods and many rules.

**Cost of the scan.** Within one segment the kernel's cost grows faster than
linearly in the segment's length (the state is a lazy chain of updates):
10,240 uses in one segment took 104 s, against 14.2 s in four segments.
Checkpoints every 2,000–5,000 uses keep segments short. Each checkpoint is
the full state, so the data written and compared is O((U/L)·S), quadratic
when the state S grows with the uses U (bench: 2.8 / 14.9 / 103 / 306 s
kernel at 1k / 5k / 20k / 50k uses; segments of ≈ 5k uses cost 14.9 s alone
and 27–35 s in B50k, the difference being the checkpoints). The corpus has at
most 8,192 uses (one or two segments), so the effect there is small.

**Linear design for the checkpoints** (cost review minor 2, decision 6; not
built). A step reads and writes only the slots of the children in its
footprint. A frame lemma for `Design.F` (a step's result on those slots
depends only on them, and the other slots are unchanged) lets each segment be
checked from a checkpoint of its **working set** (the slots of the children
its steps touch), and a per-slot timeline (for each slot, the segments that
touch it, decided) carries each slot's value from the last segment that wrote
it. Measured on the bench generator (`scan/tools/wset.py`): full-state
checkpoints hold 10k / 40k / 100k slot records at 5k / 20k / 50k uses (10
segments) and 1.72M at 200k uses (40 segments); working sets hold 1.7–2.0
records per use (8.6k, 34k, 85k, 404k). The frame lemma and the timeline
composition are not proved; until they are, path A uses full-state
checkpoints with segments of 2,000–5,000 uses (round 4's rule, segment at
least the state size, applies once the working-set form exists).

## 11. Task traces (decision 5)

- Each module's task statements, numbered in rank order and, within a rule,
  in action order, are the ports of one library child `Tasks n` (`taskAnn
  n`: `t SB t'` for `t < t'`, each C with itself, enabled, always ready, no
  paths). `comb` drives port `t` with statement `t`'s condition (`WILL_FIRE`
  included) and its argument tuple. Each step calls port `t` where its task
  call is, as an action site under its exact condition, with the argument
  built from ordinary read sites. **Task arguments are never relaxed to
  arbitrary values.**
- `Realizes`' last conjunct compares, per task log, the hardware's calls
  (`taskTrace`: enabled statements in order, with arguments) with that
  module's calls in the run, in run order. A parent's logs are its
  children's, each compared on its own, so **different modules' task traces
  are unordered against each other**.
- Proved: `tasks_realizes`, `tasks_causal`, `tasks_aliased`; `compose`,
  `Built.*` and every primitive contract re-proved. `Ex.Show` is the pattern;
  negatives (k) `ShowLate`, (l) `ShowCond`, (m) `TaskOrder`,
  `TaskRev.not_realizes`; through the scan, `examples/ScanTasks.lean`.
- **In the scan** the `Tasks` child is a chain kind (§10), linear in the task
  uses.
- **Verilog limitation** (stated in the `Realizes` and `Built.top`
  docstrings): in Verilog each synthesized module's tasks run in its own
  always block, and blocks on one clock edge run in an unspecified order,
  so output and shared-state tasks (an unseeded `$random`) may interleave
  differently across synthesis boundaries. Bluesim should follow the full
  one-at-a-time order; that is a property of the Bluesim back end, outside
  schedlean.
- **Incompleteness:** two task-bearing steps of one module that a caller can
  run in either order (two task-bearing methods exported CF) fail the
  obligation in one order; a task-bearing method called twice in a cycle
  fails `Tasks`' at-most-once rule; a module whose statement order is not
  rank order is off path A. All fail safe.

## 12. How the files compose; replay

- **Per module, three files:** `<Mod>Iface.lean`, `<Mod>.lean` and
  `<Mod>Realizes.lean`, as tested on the toy and integration review 3's
  diamond. BVI interfaces are keyed by Verilog name plus a hash of the
  `VModInfo`.
- **The build** is `lean -o` with `LEAN_PATH` in dependency order.
- **Replay.** `leanchecker --fresh` replays everything a module imports,
  single-threaded: a fixed part (Init, the library, and `Std.Tactic.BVDecide`
  when a file uses `bv_decide`) plus the design.

  | file | elaborate | `.olean` | `leanchecker` (module only) | `leanchecker --fresh` |
  |---|---|---|---|---|
  | toy `ExTopRealizes` | 0.4 s | — | — | ≈ 50 s |
  | sysArithPipe, modular, with tasks | 102.5–112.5 s | 50.0 MB | 17.8–18.3 s, 1.34 GB | 56.8–64.0 s, 1.40 GB |
  | sysArithPipe, today's flat file (`flatnew/`) | 17.1–24.9 s | 33.8 MB | 6.8 s, 1.02 GB | 85.5 s, 1.12 GB |

  The flat file's `--fresh` includes BVDecide's replay (≈ 70 s); the modular
  file adds it once it emits `skel`.
- The modular file's replay is a sixth of its elaboration without `--fresh`
  and about half with it (BVDecide not yet imported); today's file replays in
  a quarter of its elaboration without `--fresh`, and `--fresh` is dominated
  by BVDecide.
- **Answer on replay:** per module, `leanchecker` without `--fresh` (the
  module's own declarations against already-checked imports) suffices for
  incremental builds, since each import was itself replayed when it was
  built. The final verdict for a design stays one `leanchecker --fresh` of
  the top `Realizes` file.
- **File size:** the modular sysArithPipe file is 2.08 MB with tasks (1.19 MB
  without) against today's 1.35 MB; `.olean` 50 MB (29 MB without tasks)
  against 34 MB.

## 13. sysArithPipe end to end

`e2e/tools/mke2e.py` reads sysArithPipe's `dump.txt` and the printed
APackage, and writes `e2e/SysArithPipe.lean` in the conventions of §8.
`Obl`, `Acyc` and `Built.top` are proved for every `V`, operators, `EqC`,
skipped values per step and solver verdicts `Hyps`, from the assumed FIFOL1
and ConfigRegN contracts (`Leaves`). Standard three axioms.
`E2E_TASKS=0` writes `SysArithPipeNoTasks.lean`, the same without the
`Tasks` child, for a comparison with today's file, which does not model tasks.

**What is the design's:**

- 83 children of 8 kinds (21 Reg, 5 ConfigRegN, 43 FIFOL1, 5 + 4 RWire, 2 +
  2 RWire0, one `Tasks 690`), with bsc's schedules and paths;
- 227 rules in schedule order (133 rules and bsc's 94 run-time
  `mutually_exclusive` checks), each a site list of its reads, calls and task
  statements (1,859 sites);
- **the 690 task statements** (594 `$write`, 94 `$error`, 2 `$finish`): each
  an action site on the `Tasks` child, 225 of them under their exact
  condition, with arguments from read sites; the 94 `$error` statements of
  the run-time check rules have the rule's guard as their condition;
- **each rule's explicit guard**, evaluated in its body at its position from
  its read sites (548 guard reads), so `resR` checks what today's
  `guard_violated` checks;
- every port's users; ten muxes; the scan's footprints, kind tables (the
  `Tasks` child as a chain kind) and exception masks (2,421 bits); the
  guards' and arguments' fan-in, hence `Acyc`'s ranks (9 levels, 116 node
  agreement lemmas); `Mod.asm`: the 94 run-time checks.

**What the dump lacks (generator requirements), filled synthetically:** the
netlist's definitions. `CAN_FIRE_r` is the AND of the RDYs of `r`'s guarded
calls and an opaque Boolean operator `ops.g n` over the guard's real fan-in;
`WILL_FIRE` adds urgency blocking; arguments, task conditions and task
arguments are opaque operators over their real fan-in; constants and format
strings are `ops.k n`. The real Boolean skeleton would be proved by one
`bv_decide` (`skel`), not built here (the atom test's cost: 25.0 s, 2.6 GB).
FIFOL1 and ConfigRegN are assumed leaves (§14). **Not exercised here:**
conditional child calls and conditional reads (sysArithPipe needs none in any
bsc mode, §7), exported methods (nM = 0), escapes, more than one urgency
blocker per rule, and child kinds beyond these eight (the emitter raises on
others). These run only in `emit/` (conditional reads, one method),
`ScanEx`/`ScanExLate` (methods, an escape) and `scan/esc`.

**Measurements** (wall at load 10–19; heartbeats are load-independent):

| | modular, with tasks | modular, no tasks | today's flat file (`quadfix/shape/final-fix`) |
|---|---|---|---|
| elaboration | 102.5–112.5 s wall, 102.1–106.9 s user | 59.7–73.5 s wall, 59.4–70.3 s user | 17.1–24.9 s wall, 16.4–21.9 s user |
| heartbeats, per-module sections | 1,365 Mhb | 860 Mhb | — |
| peak RSS | 2.55 GB | 1.85–1.92 GB | 1.45 GB |
| `.lean` | 2.08 MB, 23,193 lines | 1.19 MB, 15,039 lines | 1.35 MB, 18,998 lines |
| `.olean` | 50.0 MB | 29.2 MB | 33.8 MB |
| `leanchecker` / `--fresh` | 17.8–18.3 s / 56.8–64.0 s | — | 6.8 s / 85.5 s |
| not included | `skel` (≈ 25 s) | `skel` | — |
| checks task traces | yes (690 statements) | no | no |

So, by user time, the modular file costs 2.7–4.3× today's without tasks, and
4.7–6.5× with the task traces, which today's file does not check at all (two
runs each, load 10–19; `e2e/run_all_v1.log`, `build/all.log`,
`e2e/final_runs.log`, `e2e/flatnew/run1.log`). Sharing the decided index
facts per port (§8 rule 4) did not reduce the heartbeats (324 against 313 Mhb
in the per-instance section), so the per-port proofs, not the decisions,
dominate.

Per section (`MARK=1`, first run with tasks / no tasks):

| section | with tasks | no tasks | what scales with |
|---|---|---|---|
| uses, site key and function tables | 6.0 s | 3.3 s | sites |
| guards, `WFd` | 3.1 s | 2.8 s | rules |
| arms (incl. `tenT`, `targT`) | 3.9 s | 1.1 s | used ports, statements |
| site facts, trace lemma | 5.6 s | 2.9 s | sites |
| value lemmas, results (`rqT`) | 6.1 s | 5.2 s | read sites used, rules |
| mux chains from checks | 8.0 s | 6.5 s | mux members (plus uncovered pairs) |
| scan (kernel) and design | 6.8 s | 4.5 s | uses |
| glue | 4.6 s | 3.7 s | kinds, groups |
| per-instance `en`/`arg`, task statements | 42.2 s | 18.8 s | users; 34 ms per task statement |
| `Acyc`, `built`, `top` | 5.7 s | 4.6 s | path-bearing inputs |

**Estimates for other corpus designs** (extrapolated from this one module,
not measured; the emitter is a prototype: eight child kinds, one urgency
blocker per rule, unconditional child calls). A module costs about 0.05–0.06 s
per site and about the same per task statement, plus 40–50 ms per mux member
and 4 ms per pair no run-time check covers, plus `skel`. With these slopes
sysFloatTest (3,858 sites, 614 task statements, 25 muxes with 34,965 member
pairs, 2,163 of them with disjoint guards and no check; the 90 s below assumes
about 2,000 members, not counted) comes to roughly 210 + 35 + 90 + 9 s plus `skel` (≈ 186 s): about 530 s, 3×
today's 170 s. sysConflictFreeOKLarge (6,667 sites, 2,050 task statements)
comes to about 8–9 minutes plus `skel`. These are single-design calibrations; a second
end-to-end module with methods, escapes and conditional calls
(mkBufferControl or mkPrediction) is the open check on them (§18).

## 14. Primitives

| primitive | interface | theorems |
|---|---|---|
| `RegN.v` (`mkReg`); `RegUN.v`, `RegA.v` map here | read CF read, read SB write, write SBR write | `reg_realizes`, `creg_causal 1` |
| `CRegN5.v` (`mkCReg`) | ports SBR across; paths `wr k → rd j`, `k < j` | `creg_realizes n`, `creg_causal n` |
| `RWire.v` variants | wset, wget, whas; paths `wset → wget, whas` | `rwire_realizes`, `rwire_causal` |
| `RWire0.v` (`mkPulseWire`) variants | wset, whas; path `wset → whas` | `rwire0_realizes`, `rwire0_causal` |
| `BypassWire.v`, `BypassWire0.v`, unsafe variant | as round 5 | `bypass*_realizes`, `*_causal` |
| `FIFO2.v`: `vMk2FIFOF` | 8 ports, bsc's schedule, aliases, no paths | `fifo2_realizes`, `fifo2_causal`, `fifo2_aliased` |
| `FIFO2.v` with `first` always ready (`FIFO2R`) | as `FIFO2`; `first` returns the head register's start-of-cycle value when the FIFO starts empty | `fifo2r_realizes`, `fifo2r_causal`, `fifo2r_aliased` |
| `FIFO20.v`: `vMk2FIFOF0` | the same without `first` | `fifo20_realizes`, `fifo20_causal`, `fifo20_aliased` |
| `Tasks n` | the module's task statements; `t SB t'` for `t < t'`; each C with itself | `tasks_realizes`, `tasks_causal`, `tasks_aliased` |

- **Register-flag FIFOs' `i_` ports read the cycle's start** (FIFO1, FIFO2,
  sized FIFOs), the semantics bsc's `i_… CF enq/deq` annotation implies. The
  README's "What the file proves" (outside this directory, not edited) should
  gain: *"A read of a register-flag FIFO's (FIFO1, FIFO2, sized FIFOs)
  `i_notEmpty` or `i_notFull`, of an unguarded FIFO's `notEmpty` or
  `notFull`, and of a `ConfigReg`, returns its value at the start of the
  cycle, whatever runs before it in the cycle; bsc schedules these reads
  conflict-free with the FIFO's actions on that basis. FIFOL1's `notFull`
  sees a `deq` in the same cycle."* and *"A read is skipped when its site's
  condition fails, and the proof holds for every value it might have
  returned; today's file makes such a read arbitrary when the annotations
  rule it out and its step fires after a call it may not follow."*
- **Not covered yet:** FIFOL1, SizedFIFO, SizedFIFO0, RegFile, the BRAMs,
  ConfigRegN, Counter and Probe. Until proved, each is a leaf with an assumed
  contract and causality. The e2e's `Leaves V` (FIFOL1, ConfigRegN) is not
  shown inhabited: a wrong assumed schedule or path table would make `top`
  vacuous rather than false.

## 15. bsc features and their cases

| feature | treatment |
|---|---|
| port copies (`mtCopies`) | one port per copy; `share` within a copy |
| argument-less value methods | one port, read any number of times; dummy argument `ops.z` |
| split methods (`dMethodSteps`) | one port; the site list branches on the split conditions; one footprint |
| always_enabled / always_ready | `Cert.alw`; the module proves its guard holds anyway |
| action-value methods | `ret` compared like a value method's |
| two C methods sharing a child port | `Env`'s exported-C part; `calledChain` |
| parallel (AND-OR) muxes | `Parent.onehot`, `Cert.onehot` from `EnvC`; `pmux`, `arg_mux` |
| ports with many users | `en_list`, `arg_users`, chains from run-time checks (§8 rule 9) |
| user assertions, run-time checks | `Mod.asm`, visible as `Mo.ok` |
| solver verdicts | `Hyps ops`; use-level facts re-asked over schedlean's conditions (§7) |
| constants; value/value comparisons | `EqC`, EqLaw chains; `EqV` with its laws at the occurring operands (§4) |
| explicit rule guards | evaluated in the rule body at its position (`resT`, §8 rule 6) |
| use conditions | not used; sites under schedlean's conditions (§7) |
| readiness of guarded reads | the readiness rule; `FIFO2R` where it cannot recover a site (§7) |
| unguarded FIFOs | start-of-cycle status reads, no readiness obligation (§7) |
| read flags | condition (c) (§7) |
| wires, CRegs, FIFOL1 | contracts with paths; `Acyc` ranks the loops away |
| combinational methods of generated modules | `Ann.path` from bsc's path analysis, checked by the parent's `Acyc` |
| BVI; modules off path A | `Built.leaf` with assumed contract and causality (§3) |
| system tasks | a `Tasks` child per module, a chain kind in the scan (§10, §11) |
| action shadowing | not yet (open issue 6) |
| reset | cycles out of reset; FIFO2's CLR is modelled |
| multiple clocks | not done |

## 16. The examples

`Example.lean`, proved for every `V` and operators: the toy hierarchy
`mkTop ── c : mkChild ── a, b : Reg`, `mkTop ── t : Reg`; `Asm`, `WireEx`,
`AliasEx`, `Fsm`, `Show` (a `$display` of a child value under a condition).
`examples/ScanTasks.lean`: task statements through the scan.
`Fifo2R.lean`: the always-ready `first`.

Negatives, each a theorem that the obligation, the contract or the
certificate is false: (a) `ChildSB`, (b) `PutBad`, (c) `Order`, (d) `NoRdy`,
(e) `ShareArg`, (f) `NoAlw`, (g) `AsmNo`, (h) `Loop`, (i) `AlwEnq`,
(j) `AlwRdy`, `FsmNo`, (k) `ShowLate`, (l) `ShowCond`, (m) `TaskOrder`,
`TaskRev.not_realizes`; `ScanExLate.no_escape`, `ScanExLate.h3_catches`;
`ScanTasks.no_chain`, `ScanTasks.not_okT`; `EqvTest.no_trans`.

## 17. Emission trust points

No Lean check sees these; the generator must get them right, as today's
must for `Hw.*`, `Hw.oneHot` and `Assertions`:

1. **`comb`'s fidelity to bsc's netlist** (`aState`): enables, muxes, RDY,
   outputs, task conditions and arguments.
2. **Completeness of `onehot`:** every AND-OR mux read as a priority mux
   lists its selectors. A missing mux is unsound, not fail-safe.
3. **`Mod.asm`'s content:** only bsc's assertion list.
4. **The `Tasks` child:** one port per task statement, numbered in the order
   bsc's Verilog back end prints them (taken from the same source `AVerilog`
   uses), which path A requires to be rank order with each rule's statements
   consecutive; `comb`'s enable and argument for each are the statement's
   condition and arguments. Checked by hand on sysArithPipe's Verilog
   (`e2e/verilog/`, bsc-fix-all): its 596 rule statements appear in the
   `always@(negedge CLK)` block in rank order. Its 94 run-time check
   statements appear elsewhere in that block; the generator numbers them at
   their check rule's rank, which is harmless because their enable is the
   violation the assertions exclude, so they are never in the hardware's
   trace in a cycle the theorem covers.
5. **Choosing the library model**: by Verilog module name, checked against
   the instance's `VModInfo` port bindings and parameters (such as
   `guarded`), not only the schedule.
6. **A BVI interface's `hasEn`, `alwaysEn`, `alwaysRdy` and `path`** come
   mechanically from `VModInfo`; each is part of the assumed contract.

Checked, but resting on the generator's input: the `Acyc` read lists
(checked by `dep`); the site conditions (checked for every assignment of
skipped values per step); the task numbering's rank order (decided per step).

## 18. Open issues and what is not proved

1. **No generator yet.** `e2e/tools/mke2e.py` is a prototype emitter over
   the dump and the APackage; schedlean must emit the netlist itself.
2. **`skel` and the consistency check** are not emitted in the e2e.
3. **A second end-to-end module** with exported methods, escapes,
   conditional calls and reads, readiness sites and multi-blocker urgency
   (mkBufferControl or mkPrediction; cost review M3) is not built. The
   estimate table is calibrated on sysArithPipe alone.
4. **Path B** of the gate is defined, not built.
5. **Linear checkpoints** (working sets, §10): designed with measurements,
   not proved; full-state checkpoints remain, quadratic in total data for
   large U.
6. **Action shadowing** (two simultaneous writers of a self-SBR port) fails
   safe; not modelled.
7. **Conditions over hardware wires** (§7): not expressible in the site
   model; the nondeterministic-site extension is not built. sysFloatTest's
   10 facts stay at risk in the default mode.
8. **Path granularity** is per port, not per pin.
9. **More primitives** (§14), **RST**, **multiple clocks**; `Leaves V` not
   shown inhabited.
10. **Multi-cycle:** `clean_idem` is in place; the trace theorem by
    induction on cycles is not written.
11. **Value-returning tasks** (`$random`, `$time`, `$fopen`) are modelled
    with the result as a free hardware output; bsc's Verilog for them is not
    checked against that.
12. **Per-statement cost.** Task statements cost about 34 ms each in the
    e2e (per-statement value lemmas and decided index facts); one lemma per
    statement class would cut the constant. Mux chains still write each
    member's tail list (O(users²) list entries, decided in the kernel, small
    constant).
13. **`Drives.arg` for a shared value port after its reads are dropped** is
    not formalised.
14. **The readiness rule's search** tries up to three flipped operators
    (2,000 tries); a larger search might recover the two `FIFO2R` sites.

## 19. Review items and where they are fixed

| review item | fix | evidence |
|---|---|---|
| round 6 soundness M1: one `u` for all steps | skipped values per step (`u : Nat → Nat → V`); why one per method suffices (§7) | `e2e/SysArithPipe.lean`, `emit/out/Em16.lean` |
| round 6 soundness M2: e2e rules have no guards | each rule's guard read at its position (`resT`); `resR` per rule (`rqT`) | `e2e/SysArithPipe.lean`, §8 rule 6 |
| round 6 soundness M3 / cost M1: tasks outside scan, e2e, gate | chain kinds in the scan (linear, proved); the e2e carries all 690 statements; gate updated | `Scan.lean`, `examples/ScanTasks.lean`, `scan/tasks/`, §10, §13 |
| round 6 cost M2: mux chains quadratic | chains from run-time checks (`amoChain_of_pairsC`, `forall_split`); pairwise only where no check covers | §8 rule 9; 2,992 of 3,242 pairs |
| round 6 cost M3: simplest module, extrapolated | tasks, guards and conditional task calls added; second module not built | §13, open issue 3 |
| round 6 minors | fallback as assumed leaf (§3); weakening reworded (§7); FIFOL1 sentence (§14); axioms printed in `Scan`, `EmitLemmas`; `EqV` for the module's `E`; path B example corrected; scan cost restated (§10); estimates as calibrated on one design | — |
| Ravi 2026-11-02 (1): readiness rule | short-circuit with per-site readiness-first bias, weakened to the ready literal; 22 of 24; `FIFO2R` proved for the 2 left | §7, `Fifo2R.lean`, `sites/modes/out_default` |
| Ravi (2): four compilation modes | compiled and measured | §7, `sites/modes/` |
| Ravi (3): mkPrediction `RL_outputing` | not a lifting problem: both sites hold under the readiness rule; bsc's use condition is weaker than the need | §7, `sites/out_trace/mkPrediction.txt` |
| Ravi (4): hardware-wire conditions | precise argument from the definitions and a two-cycle witness; extension sketched | §7 |
| Ravi (5): compare with today's file | `quadfix/shape/final-fix` file measured | §12, §13 |
| Ravi (6): superlinear spots | tasks: fixed (chain kinds); mux chains: fixed (checks); checkpoints: linear design with measurements, not proved | §8, §10 |
| round 3 integration M1–M3, minors; soundness minors | as in revision 1 (site conditions, conventions, scan proved, replay, file size, read flags, `Site` name: the generator's `data Site` is renamed `OooRead`) | `prev-int1/design.md` §19 |

**Upstream PR sequence.**

1. The library: `Modular.lean`, `Fifo2R.lean`, `EmitLemmas.lean`,
   `Scan.lean`, `SiteCond.lean`.
2. Emission of interface and obligation files on path A: site lists with
   schedlean's conditions and per-step skipped values, guards in rule
   bodies, the `Tasks` child, the scan's tables and glue, per-kind arm tables,
   chains from run-time checks, `skel`, `Acyc` filters, the consistency check.
3. `<Mod>Realizes.lean`, the `Mod.Env` records (including modules off path A
   as assumed leaves) and the `leanchecker` step in the driver.
4. Path B, linear checkpoints, more primitives.
