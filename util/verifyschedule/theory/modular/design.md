# Round 7: the modular proof

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

What round 7 changes (the round-6 second reviews, Ravi's decisions of
2026-11-02, and the revision after the round-7 first reviews):

- **The statement.** Skipped values are a family: each step's program receives
  only its own `u s`, and the run, the order and every child's call sequence
  are fixed before any skipped value of the tree (`ParentU`, `ModuleU`,
  `OblU`, `CertU`, `RealizesU`, §2). The every-assignment argument for skipped
  reads is valid (§7).
- **The always-ready FIFO2 variant is removed** from the library, build and
  proofs. Ravi's requested wider orientation search for its two sites was
  superseded by decision (b) and not done (§7).
- **Readiness** (decision b and the revision): `&&` and `||` sequenced by
  expression structure, and every literal over a conditional read evaluated
  at that read's site value. 22 of 24 readiness sites have all their facts
  confirmed in the default mode; the 2 others fail safe (§7).
- **Cost**: bsc's `mutually_exclusive` groups stated once (`SufAsm`), so mux
  chains and run-time checks are linear (§15); tables split; uses as tables
  keyed by rule; an emitted escape form; the scan segmented by the emitter,
  with recalibrated bounds (§10).
- **A second end-to-end module**, mkBufferControl: 9 exported methods, 12
  escaped pairs (50 dynamic facts, each confirmed for its pair), conditional
  reads, 5 readiness sites (6 facts), 85 conditional calls, three urgency
  blockers on one rule. Its `OblU` is proved; `Acyc` and `top` are not (§14).
  It stays on the flat theorem in every mode (§10).
- **The gate**: per mode, per corpus design, with numeric bounds (§10).

§20 maps each review item to its fix. `prev-r6/` holds round 6's design.md
and build.sh.

## Files (all in this directory)

| file | what | lines | check (wall, peak RSS) |
|---|---|---|---|
| `Modular.lean` | the model, `ParentU`/`ModuleU`/`OblU`/`CertU`/`RealizesU`, composition, `Acyc`, `Built`, primitives, task traces | 4,610 | 21.0 s, 0.77 GB |
| `EmitLemmas.lean` | `Tab`, numbered site lists, per-port facts, `callsOf_fn_congr`, `withSitesX` | 545 | 1.9 s, 0.51 GB |
| `Scan.lean` | the scan and `pairFacts_of_scan`, chain kinds, rule codes, `covB` chains, `SufAsm` groups, escapes (`okT_except_fn`) | 2,337 | 6.7 s, 0.62 GB |
| `SiteCond.lean` | weakened and dropped conditions; `oblU_weakens` (weakening costs completeness) | 534 | 1.9 s, 0.51 GB |
| `Example.lean` | the toy hierarchy, negatives, task traces, `UOrder`, `RdyPort` | 3,425 | 51.2 s, 1.02 GB |
| `Scale.lean` | 4,096 children and rules indexed by `Fin n` | 170 | 1.3 s, 0.48 GB |
| `split/` | the toy as per-module files | 472 | ≤ 4.7 s each |
| `examples/` | `ScanEx`, `ScanExLate`, `ScanTasks` (with `no_pairFacts`), `EqvTest` | — | ≤ 3.3 s each |
| `e2e/` | the emitter (`tools/`; `tools_r7a/` before the revision), `SysArithPipe{,NoTasks,60,Seg}.lean`, `MkBufferControl.lean`, today's flat files' runs (`flat7/`) | — | §13, §14 |
| `emit/` | the emission-convention probe (`gen/mkemit.py`), `conventions.md` (C1–C19), the port probe (`port7/`) | — | `Em16` 13.9 s, 0.58 GB |
| `scan/` | scan benchmarks; chain (`chain7/`: cover sets, `chain8/`: groups), escape, checkpoint and task-statement probes | — | §15 |
| `sites/` | the site-condition tool, `rule.md`, the four modes (`modes/`), round 7's runs (`modes7/`), action shadowing (`tools/shadow.py`, `modes7/shadow/`) | — | §7, §10 |
| `build.sh` | everything above in dependency order, logged to `build/all.log` | — | — |

Import order: `Modular` ← `EmitLemmas` ← `Scan` ← generated files;
`Modular` ← `SiteCond`. Core Lean 4.35 only: no Mathlib, and no `sorry`,
`admit`, `axiom`, `native_decide`, `implemented_by` or `unsafe` in any file.
Every Lean run: `prlimit --as=25769803776`, `timeout`, `-j1`,
`-DElab.async=false`, one process at a time, on a shared machine (load is
quoted beside each measurement; this revision's runs were at load 21–73 on
32 cores, where user times are 1.6–1.9× the cost reviewer's at load 13–19 for
the same files, so ratios are taken between runs of one period and
heartbeats are quoted where a before/after comparison matters). Check times
in this table are from `build/all.log` (load 25–46). Kernel time is quoted where a
probe prints it (`decide +kernel` scans, chain probes); the e2e files' kernel
time is not separated (§19). Every `#print axioms` lists at most `propext`,
`Classical.choice`, `Quot.sound`, plus `bv_decide`'s axiom for `Ex.Asm.skel`
and what is built on it.

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

/-- steps take skipped values `u : U`; the hardware does not -/
structure ModuleU (V : Type) (X : Iface) where
  U : Type
  meth : U → X.M → V → S → V → S → Prop
  rule : U → R → S → S → Prop
  …

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
- Each module's own task calls are compared with that module's calls in the
  run; different modules' task traces are unordered against each other (the
  Verilog limitation across synthesis boundaries; Bluesim should follow the
  full order).

## 2. Families and the obligation

```lean
structure ParentU (V : Type) where
  I; decI; kids; …; CI : I → Iface; X : Iface; PR : Type
  body : X.M → V → (Nat → V) → Prog I (fun j => (CI j).M) V    -- step's own skipped values
  rbody : PR → (Nat → V) → Prog I (fun j => (CI j).M) V
  comb; asm; onehot; toB; exactRet

def ParentU.at (F : ParentU V) (u : F.Step → Nat → V) : Parent V   -- step s sees only u s

def OblU (F : ParentU V) : Prop :=
  ∀ oc w σ, F.asm oc w → (∀ j, AliasOK F.toB (F.CI j).ann (oc j)) → Legal F.X.ann σ → Drives F.X.ann w σ →
    (∀ c ∈ σ.flatten, F.X.ann.alwaysRdy c.1 = false → (F.comb oc w).2.rdy c.1 = true) →
    F.onehot oc w ∧ ∃ τP σC, groups τP = σ ∧
      (∀ j, Legal (F.CI j).ann (σC j) ∧ Drives (F.CI j).ann ((F.comb oc w).1 j) (σC j) ∧
        ∀ c ∈ (σC j).flatten, (F.CI j).ann.alwaysRdy c.1 = false → (oc j).rdy c.1 = true) ∧
      ∀ u, (∀ e ∈ τP, StepOK (F.at u) oc (F.comb oc w).2 e) ∧ ∀ j, sigmaJ (F.at u) oc τP j = σC j

structure CertU (F : ParentU V) where
  rules; fire; hit                      -- fixed before u
  cert : ∀ u, Cert (F.at u)
  rules_eq : ∀ u, (cert u).rules = rules;  fire_eq …;  hit_eq …
  trU : ∀ oc w s u u', Env F.base oc w → Av F.base fire oc w s → trS (F.at u) oc w s = trS (F.at u') oc w s

theorem oblU_of_cert [Inhabited V] (F : ParentU V) (C : CertU F) : OblU F
```

- `Obl` (the per-`u` view) and `Cert`, `obl_of_cert` are unchanged; `OblU`
  fixes the order `τP` and every child's call sequence `σC` before `u`.
  Fixing `σC` is needed, not only `τP`: otherwise a child's run, and the real
  value of a skipped read, could depend on `u`.
- `trU` is checked per site in generated files (`trUS`, from the `HQ` tree:
  each site's condition, and its argument where taken, agree at any two
  skipped values of its step when the step may run).
- `Ex.UOrder.not_oblU` rejects the round-6 probe (an order that must change
  with `u`); `obl_each` shows the old per-`u` statement held there.

## 3. Composition, `Built`, and modules off path A

```lean
theorem compose (F : ParentU V) (impl : ∀ j, ModuleU V (F.CI j)) (hC : ∀ j, RealizesU (impl j))
    (hA : ∀ j, Aliased F.toB (impl j).hwm) (hO : OblU F) : RealizesU (F.toModuleU impl)
inductive Built : (X : Iface) → ModuleU V X → Prop
  | leaf (Mo : Module V X) : Realizes Mo → Causal Mo → Built X Mo.toU
  | node (F) (impl) : (∀ j, Built (F.CI j) (impl j)) → (∀ j, Aliased F.toB (impl j).hwm) → OblU F →
      Acyc F.base → Built F.X (F.toModuleU impl)
```

**A module off path A enters a modular parent as an assumed leaf**:
`Built.leaf Mo hR hC`, with `hR : Realizes Mo` and `hC : Causal Mo` (and, if
it exports aliases, `Aliased toB Mo`) fields of an explicit hypothesis record
of the parent (as the e2e files' `Leaves`), named in the top theorem's
statement. Never a silent shortcut: today's flat theorem for that module is
the evidence for the hypothesis, but Lean does not connect them (the flat
theorem is a per-cycle statement, not `Realizes`). BVI leaves enter the same
way; BVI interfaces declare no aliases (`aliased_of_none`).

## 4. Values, operators, constants

One abstract `V`, each module's operators a parameter (`Ops V`, with the
design's Bool reading `toB`), constants through `EqC V` and its law;
value/value comparisons need transitivity (`EqvTest.no_trans`; `natEqV`
shows `EqV` inhabited, `no_trans_cong` that every law but transitivity is not
enough). bsc's solver verdicts stay hypotheses (`Hyps ops E`), as today.

## 5. Non-vacuity, assertions, parallel muxes

Unchanged from round 6: `Acyc` ranks children's outputs (`causal_toModuleU`);
assertions are visible (`Mo.ok`); `Cert.onehot` assumes only `EnvC`. The
consistency check is not emitted. RST is not modelled.

## 6. The headline theorem of a generated module file

As emitted by `e2e/tools/mke2e.py`:

```lean
abbrev F (ops : Ops V) (E : EqC V) : ParentU V := …   -- body m x v / rbody r v := runSites v … v
theorem F_sites (ops E) : F ops E = withSitesX (F ops E) ssM resM ssR resR := rfl   -- trust point 7
abbrev P (ops E) (u : St → Nat → V) : Parent V := (F ops E).at u
structure Hyps (ops E) : Prop where disj … rdy_n … inv_n … esc_n … cx_n …     -- solver-confirmed facts
def certU (ops E) (hy : Hyps ops E) : CertU (F ops E)
theorem obl [Inhabited V] (ops E) (hy : Hyps ops E) : OblU (F ops E) := oblU_of_cert _ (certU ops E hy)
def acyc (ops E) : Acyc (F ops E).base
theorem top [Inhabited V] (ops E) (hy) (lv : Leaves V ops.toB) :
    RealizesU ((F ops E).toModuleU (fun j => (implK ops lv j.1).toU)) ∧ ∀ s w, … → ∃ o s' g, … := (built …).top
```

Each fact in `Hyps` carries the verdict for the exact formula the file states
(readiness and invariance: the site tool in the site-value form, §7; an
escape: the solver for that site pair and the two sites' stated conditions,
or bsc's confirmed fact; caller exclusivity at a mux: the solver over the
users' call conditions). A file whose module needs a fact that is not
confirmed states it in a separate structure `Unconfirmed ops E`, an explicit
argument of `obl` and `top` (mkBufferControl, §14); such a module is off
path A.

## 7. Site conditions

A step is a list of sites (child, port, condition and argument over the read
environment `ρ`); `runSites v … v` makes it a program over the step's own
skipped values `v`. The rule (decision 1 and Ravi's decisions):

- **Action calls**: their exact call conditions, never weakened. Task calls
  are action calls on the module's `Tasks` child.
- **Value reads: unconditional**, unless needed: (a) an out-of-order read
  (the static footprint scan fails for it); (b) a shared copy two firing
  steps read with different arguments (none in the corpus); (c) the earlier
  read of a read flag (none); (r) readiness: the port has a ready signal the
  step's guard does not imply.
- **(a), (b), (c)**: schedlean's conservative reach (`&&` and `||` count both
  operands; only an arm of an `if`, `case`, mux or selection is conditional).
  In mkBufferControl 28 of its 30 (a) reads have reach `true`.
- **(r), the readiness rule** (decision b): in the expression as bsc builds
  it, operands are evaluated left to right; a later operand's condition
  includes the earlier operands' short-circuit outcome (true for `&&`, false
  for `||`). The read's condition is its **ready literal** (`i_notEmpty`,
  `RDY_m`), read before it. This is a pragmatic encoding: the hardware
  evaluates both operands and the atomic value is the same; the conditions
  stay checked for every skipped value. No bias and no orientation search.
- **Site values (the revision, soundness review r7-1 M1).** A literal of the
  readiness formula `R` that reads a conditional read `y` of the same step
  (the site itself included) reads `y`'s **site value**, `ite(cond_y,
  port_y, fresh)`, as the model does. The facts, checked by cadical through
  the site tool (`sitecond.py` `rdy_site_check`, encoder `satc.EncS`), are
  - readiness: `guard ∧ R(site values) ∧ ¬RDY` UNSAT;
  - invariance, where `R` reads another conditional read:
    `guard ∧ R(site values) ≠ R(ports)` UNSAT (where `R` reads only the
    site itself, Lean derives invariance from readiness, `inv_self`).

  In the file `R` is an opaque operator over exactly those values, so `trU`
  and the value proofs check every use against them.
- An always-ready `first` was considered and rejected because it drops the
  readiness check for every reader of the instance. Ravi's requested wider
  orientation search for its two sites was superseded by decision (b) and not
  done; both modules stay flat for action shadowing anyway (§10).
- **Unguarded FIFOs** are unsafe (start-of-cycle status, no readiness
  obligation); FIFOL1's `notFull` sees a same-cycle `deq`.

**Why a skipped read is sound.** `OblU` fixes the order and every child's
call sequence before `u`, so the every-assignment argument applies: set each
skipped value to what the real read returns at its position in the fixed run.
For a readiness read skipped because the FIFO is empty there is no real read
at that position; bsc's conditional lifting leaves the value unspecified there,
and "every `u`" covers any value. A condition that reads a skipped value and
changes the calls, or a call argument that depends on one, fails `OblU` (fail
safe; checked per site by `trU`). A method called twice shares `u` (per step);
under `OblU` each call's trace is `u`-free and its result pinned, so each
call's real values can be substituted separately. This step from the model to
the real design is an argument outside Lean; it assumes reads at skipped sites
are side-effect free.

**Every condition is checked.** A file's conditions need no soundness lemma;
the obligation is checked for them. `SiteCond.oblU_weakens`: weakening keeps
every step's result and adds calls only, so it costs completeness (legality
and readiness of the extra calls).

**In the e2e files** (opaque operators): a use of a conditional read is split
on its reach, `bif R then f(fan-in) else f'(fan-in without the read)`; for
(a) `R` is the read's own condition, for (r) it is `R_seq` over the site
values above, with `rdy_n` (and `inv_n`) in `Hyps`. A readiness read's own
readiness comes from the child's alias (`AliasOK`: `rdy first = toB (ret
i_notEmpty)`); a conditional guarded call's from bsc's lifted `!c ∨ RDY` in
the rule's guard. Because the split condition of an (a) read and its site
condition are the same opaque operator, a too-strong (a) condition cannot
fail in these files; for (r) reads the check runs through the stated facts
(trust point 1, §18). The library negative `Ex.UOrder` shows the check
failing.

**Readiness in the site-value form** (`sites/modes7/`, default and
`-no-sched-conditions`; cadical over the abstract skeleton, UNSAT exact):

| design | readiness sites | all facts confirmed | `R` reads another conditional read (invariance holds) | `R` reads only itself | failing (fail safe) |
|---|---|---|---|---|---|
| mkBufferControl | 5 | 4 | 1 | 3 | `RL_inputing / shortTermPicList_returnList.first` |
| mkPrediction | 9 | 8 | 6 | 1 | `RL_outputing / outstatefifo.first` |
| mkDeblockFilter | 9 | 9 | 0 | 0 | — |
| mkMCR | 1 | 1 | 0 | 0 | — |

22 of 24 in both modes; `-no-aggressive-conditions` and both off have no
readiness sites. The two failing sites are the ones that failed under the
round-6 bias rule. The mkBufferControl witness: `infifo.first[38:32] == 47`,
`[53:52] == 3`, the memory-management operation 3, the short-term return list
empty; bsc's own use condition fails there too. A negative control
(`sites/modes7/neg_rdy.py`) with the ready literals replaced by free atoms
loses `RL_adjustingFreeSlots / longTermPicList_returnList.first`, whose fact
holds only because `adjustFreeSlots != 1` forces the short-term list
non-empty.

The first round-7 verdict (`rdyRule` in the tool's output) encoded those
literals on the hardware ports, which did not show the uses independent of the
skipped values; with whole-literal dropping instead (the review's other fix)
the count would be 13 of 24. **Whole-literal dropping is the fallback** only
where an `R` would read a site that is not before a site splitting on it: the
emitter drops those literals, counts them and re-asks the tool for the
formula it emits (mkBufferControl: 0). The three other designs are not
emitted; their count assumes the same, and they are flat for shadowing.
(a)-read conditions over conditional reads would take the same form with an
invariance fact; neither e2e module has one, and the emitter asserts it. The
order pass's later-site drops for reach conditions: mkBufferControl 5,
mkPrediction 65; cycles before weakening: mkBufferControl 3 steps / 4 sites,
mkPrediction 2 / 5.

**The four modes** (bsc-fix-all, `sites/modes/compile.sh`; the corpus `.ba`
files are the default mode, which since bsc 0f50f1ab has
`-aggressive-conditions` and `-sched-conditions` on):

| design | default | `-no-aggressive-conditions` | `-no-sched-conditions` | both |
|---|---|---|---|---|
| mkBufferControl | a 30, r 5; 46 facts UNSAT | a 0, r 0 (209 sites) | a 0, r 5 | a 0, r 0 |
| mkPrediction | a 5, r 9; 5 UNSAT | a 0, r 0 (349 sites) | a 0, r 9 | a 0, r 0 |
| mkDeblockFilter | r 9 | — | r 9 | — |
| mkMCR | r 1 | — | r 1 | — |
| mkAssertedSchedules | a 1; 1 UNSAT | a 1; 1 UNSAT | — | — |
| sysFloatTest | a 10; **10 at risk** | a 10; **10 at risk** | — | — |
| sysConflictFreeOKLarge | a 1,024; 1,536 = bsc's confirmed | the same | the same | the same |
| sysArithPipe | — | — | — | — |

**Conditions over hardware wires** (sysFloatTest): not expressible in the site
model (a site reads only `ρ`, and two cycles can agree on everything a step
reads and differ on another step's enable). The nondeterministic-site
extension is not built.

## 8. Emission conventions

`emit/conventions.md` (C1–C19). In short: children indexed per kind; every
table a closed, balanced `Tab`, split into subtree definitions (no declaration
grows with a table, C10); `comb` through per-kind arm tables, never unfolded
by a proof; per-instance facts by balanced proof trees; `Acyc` read lists as
filters of one list; steps as numbered site tables with `mkSite`/`sitesF`,
`trace_fn`, `envHw_fn`; a port's uses a table keyed by step (C11); kernel
checks over closed data; a module is a `noncomputable section`; an escape as
`okT_except_fn` over numbered site tables (C15). Round 7 adds per-site value
trees `HQ` (`trU`) and, where some guarded site is conditional, `RY`; rules
and methods share one table index; value lemmas take the step's argument `x`.
The revision adds:

- **bsc's `mutually_exclusive` groups once** (C17): `grpT` (each group's
  members, O(u) data), `asmP`'s group part `SufAsm (WF …) (grpT.get g)`,
  check rules' guards over `(grpT.get g).drop (j+1)`, and a mux's chain from
  `amoChain_of_grp` (a sublist decided in one pass) or `amoChain_of_cover_grp`
  (members outside the group first, by cover sets). A generator check asserts
  every check of a group is its suffix. Checks that are not group suffixes
  keep the cover-set form (C13).
- **The scan in segments** (C18), cut at `E2E_SEG_USES` uses (default 4,096).
- **Readiness facts over site values** (C19): `evs_k_j`, `rdy_n`, `inv_n`,
  `inv_self`; escape facts labelled per pair; `F_sites` by `rfl`.

## 9. versus today's emitted file

As round 6's table; the new rows: exported methods (results checked against
the module's outputs, `resM`), escapes with their dynamic facts, caller
exclusivity at muxes (`cx_n`). `fail_if_success` is still not emitted.

**bus_2m_2s (decided).** Its own obligation fails for the 16 exported CF
pairs bsc records as unrealizable (`dUnrealized`). It stays **off path A**, on
the flat theorem, entering its parents as an assumed leaf with its exported
annotation exactly bsc's, and the warning is emitted. Exporting a narrowed
(SBR) annotation instead would change the interface its parents were compiled
against, and their obligations would then have to be re-checked against it.

## 10. The scan, escapes, and the gate

`pairFacts_of_scan` builds every pair field of `Cert` from a kernel-decided,
checkpointed scan (`D.Ok`) and linear glue (`GlueOK`), with exactly `Cert`'s
statements. Rule codes (`Design.rc`) give code 0 to every rule in no exception
group or escape, so outside those the slot records do not grow with nR; each
group or escape member has its own code (sysArithPipe: 111 codes for 227
rules), and the bit sets over codes do grow with them. Sharing a code can only
make the exception obligations harder. Chain kinds check task statements by
position. Escapes (`esc_of_pairs`, `okT_except_fn`) carry a dynamic fact per
statically failing site pair.

**The gate.** Path A, for a module in a given bsc mode, requires all of:

1. the scan passes (`grp`, `mm`, `mmg`, `h3`), with `escR` carrying only
   escaped pairs, at most 64 escapes;
2. **every escape's dynamic fact confirmed**, per fact: the solver over the
   two steps' `WILL_FIRE` and the two sites' conditions as the file states
   them, or bsc's own confirmed fact;
3. **every readiness site's facts confirmed in the site-value form** (§7):
   readiness, and invariance where `R` reads another conditional read;
4. **no action shadowing**: at every child action port with an argument, no
   two callers can call in one cycle unless a recorded exclusion or a
   confirmed fact excludes them (`sites/tools/shadow.py`; the modular `Cert`
   requires every call's argument on the wire, so two simultaneous writers of
   a self-SBR register, legal in bsc, fail the obligation);
5. the module's task statements numbered in rank order (generator check;
   checked against bsc's Verilog only on sysArithPipe so far);
6. **numeric bounds where cost is not yet linear** (measurements below and
   in §15):
   - at most **3,072 sites in one step** (a step's sites are its scan uses):
     the step's site proofs grow ×2.2 in memory per doubling (Em series); a
     3,073-site step takes 12.9 GB and a 4,097-site step does not complete
     under the 24 GB cap; a step cannot be split across scan segments;
   - the scan in segments of at most **4,096 uses** (the emitter cuts them);
   - a state of at most **8,192 slots** and at most **4,096 rule codes**;
7. no assumed leaf without its hypothesis named in the top theorem.

Corpus maxima against item 6: 2,562 sites (uses) in one step (sysConflictFreeOKLarge),
8,192 slots and 8,192 uses (mkCregInlineBlowup: 2 segments of 4,096; it was
admitted in round 7's first table as one segment at exactly the old bound),
and 111 codes in sysArithPipe (the other designs' code counts are not
measured). The emitter asserts each bound (`mke2e.py`, before
the scan).

Otherwise path B (case-split pair fields; defined, not built), then today's
flat theorem; the module enters modular parents as an assumed leaf (§3).

**Outcome per design and mode.** The 8 designs that need any site condition
were compiled in all four modes (`sites/modes/`); the other 28 were compiled
in the default mode only (no conditional reads, no readiness sites, no
escapes there; not recompiled in the other modes, so their outcome there is
not measured).

| design | default | `-no-aggressive-conditions` | `-no-sched-conditions` | both off |
|---|---|---|---|---|
| sysArithPipe | **A** | **A** | **A** | **A** |
| sysConflictFreeOKLarge | **A** (3 escapes, facts = bsc's 1,536 confirmed; writer pairs excluded by its `conflict_free` run-time checks; one 2,562-site step, under the 3,072 bound) | **A** | **A** | **A** |
| sysFloatTest | flat (10 escape facts at risk) | flat (the same) | **A** | **A** |
| mkAssertedSchedules | **A** (2 escapes, 1 read fact UNSAT, the `outQ.enq` pair excluded by its asserted run-time check) | **A** | **A** (1 escape) | **A** (1 escape) |
| mkBufferControl | flat (one readiness fact refuted; shadowing `lockInterLoads.write`) | flat (shadowing) | flat (both) | flat (shadowing) |
| mkPrediction | flat (one readiness fact refuted; 22 shadowing pairs) | flat (16 pairs) | flat | flat (16 pairs) |
| mkDeblockFilter | flat (9 shadowing pairs) | flat | flat | flat |
| mkMCR | flat (4 shadowing pairs) | flat | flat | flat |
| mkUrgencyBlocking | flat (shadowing `lastOp.write`, its test) | not measured | not measured | not measured |
| bus_2m_2s | flat (decided, §9) | not measured | not measured | not measured |
| mkCregInlineBlowup | **A** (2 segments) | not measured | not measured | not measured |
| the other 25 corpus designs | **A** | not measured | not measured | not measured |

Default mode: **29 of 36 pass the gate's checks**, 7 stay flat (sysFloatTest
is flat in the default mode, as expected). Two caveats on the 29: (i) 10 of
them export method pairs that are not CF (mkAes, mkDutLogic with 70 pairs,
mkLeanNames, mkMethodRelations, mkRegFilePorts, mkRenamedPorts, mkSolver3,
mkSplitMethod, mkValueAfterAction, mkWireResult); the prototype emitter
asserts CF methods (`MAnn` has no SB/SBR), so no file for them has been built;
(ii) gate item 5 has been checked against bsc's Verilog only on sysArithPipe.
Action shadowing is the most common reason for the flat theorem: 5 designs,
including 4 of the 5 large h264/MPEG4 modules. Admitting them needs a
`Cert.arg` that drives a self-SBR port with its last caller's argument (next
round).

**Calibrating item 6** (kernel time of one scan segment, three footprint
shapes; the scan definitions are unchanged by the revision, so the cost
reviewer's runs in `r7-cost1/logs/summary.txt` apply):

| uses in one segment | creg shape (rule i reads and writes its own register) | two reads per rule, read-only registers, S = 8,192 | B50k (random multi-child) |
|---|---|---|---|
| 2,048 | 13.9 s (6.8 ms/use) | 39.2–41.4 s (19–20 ms/use) | — |
| 4,096 | 20.4 s (5.0 ms/use) | 94.1 s (23 ms/use), 1.84 GB | — |
| 5,000 | — | — | 17–27 s (3.4–5.4 ms/use) |
| 8,192 | 31.7 s (3.9 ms/use), 5.22 GB | 359 s (44 ms/use), 3.28 GB | — |

Within one segment the two-reads shape grows ×2.3 and then ×3.8 per doubling;
the cause is not found. At 4,096 uses every probed shape stays within about
25 ms of kernel time per use, so the bound is 4,096 (round 7's first table had
8,192). At S = 8,192 and 4,096-use segments the full-state checkpoints hold
at most 2 slot records per use; the segment rule max(2,000, S) uses, not the
rule codes, is what keeps the mkCregInlineBlowup shape's checkpoint data
linear (fixed 1,024-rule segments: ×5.8, ×4.6, ×4.3 per doubling). At the
gate's corner (S = 8,192, 16,384 rules, 4 segments) the checkpoint literals are
1.68 / 6.6 / 20.0 / 34.8 MB for 0 / 2,048 / 4,096 / 8,192 codes; at 4,096
rules every rule coded costs 2.5 % more kernel time at 5.4× the data.

**Cost of the scan**: task-chain probe at 10,240 / 20,480 / 40,960 / 81,920
statements: 14.8 / 30.4 / 65.6 / 139.6 s wall, 10.0 / 20.9 / 46.6 / 99.0 s
kernel, 1.05 / 1.53 / 2.58 / 4.69 GB (cost piece); the reviewer's rerun of
the last two: 62.8 / 154.3 s wall, 43 / 101 s kernel (the last doubling ×2.05–
2.46 in wall time, ×2.1–2.35 in kernel time, depending on load). B50k 322.9 s
wall, 234 s kernel, 8.29 GB. The working-set checkpoint design is still not
proved.

## 11. Task traces

Unchanged: each module's statements, numbered in rank order, are the ports of
one `Tasks n` child; `Realizes`' last conjunct compares the hardware's calls
with the run's, per module. Incompleteness (fail safe): two task-bearing
methods exported CF, a task-bearing method called twice, statements not in
rank order. A task-bearing exported method also picks up every task-bearing
rule as a hit through the chain slot, which can trip `h3` (admission, not
soundness; the corpus mirror has no tasks).

## 12. Replay

Per module, `leanchecker` without `--fresh` for incremental builds; the final
verdict for a design is one `leanchecker --fresh` of the top file
(`build/all.log`).

| file | elaborate | `leanchecker` | `leanchecker --fresh` |
|---|---|---|---|
| sysArithPipe (tasks) | 177.5 s wall, 170.6 s user, 2.70 GB (load 53) | 35.0 s, 1.30 GB | 107.9 s, 1.36 GB |
| mkBufferControl (obligation only) | 45.0 s wall, 44.4 s user, 1.13 GB (load 36) | 11.9 s, 0.57 GB | 90.2 s, 0.60 GB |
| toy `ExTopRealizes` | 0.6 s, 0.48 GB | — | 87.6 s, 0.51 GB |

## 13. sysArithPipe end to end

`e2e/tools/mke2e.py` (one emitter for both modules) reads the dump and the
printed APackage and writes `e2e/SysArithPipe.lean`: 227 rules, 83 children,
1,859 sites including 690 task statements, 10 muxes, two `mutually_exclusive`
groups (51 and 45 members; 94 checks, now O(96) data), `OblU` through
`certU`, `Acyc` and `top : RealizesU …`, standard three axioms. sysArithPipe
needs no conditional read in any mode, so its `trU` and `HQ` are trivial: the
skipped-value machinery runs only in mkBufferControl and the toy examples. With
no conditional site, the synthetic split of §7 does not arise
here.

| | modular, tasks | modular, no tasks | today's flat (`quadfix/shape/final-fix`) | task-agreement flat (`quadfix/task/fix6`) |
|---|---|---|---|---|
| elaboration (this revision, `build/all.log`, `e2e/flat7/r8.log`) | 177.5 s wall, 170.6 s user, 2.70 GB (load 53) | 111.6 s wall, 101.5 s user, 1.98 GB (load 31) | 35.8 s wall, 25.8 s user, 1.38 GB (load 48) | 175.0 s wall, 116.3 s user, 2.51 GB (load 46) |
| elaboration (cost review r7-1, load 13–19, the same files but for the revision's changes) | 100.7 s wall, 100.3 s user, 2.87 GB | 57.9 s wall, 57.6 s user, 2.11 GB | 15.3 s wall, 15.0 s user, 1.45 GB | 62.7 s wall, 62.1 s user, 2.65 GB |
| `skel` | not emitted (atom test: 42.0 s user here, 23.8 s in the review) | not emitted | included | included |
| checks task traces | yes, 690 | no | no | yes |

Ratios by user time, this revision's runs: against today's file 6.6× with tasks
and 3.9× without; against the task-agreement file 1.5×. With `skel` added to the
modular side: 8.2×, 5.6×, 1.8×. The cost review's runs at load 13–19 give 6.7×,
3.8×, 1.6× (8.3×, 5.4×, 2.0× with `skel`). The task-agreement flat file is the
fair comparison for the file with tasks. The section heartbeats
(load-independent) of the revised file are 1.4 % below round 7's first file.
Round 7's first table quoted a run at load 60 (127.3 s); its open issue "final
emitter slower" was load and is closed.

**Segments.** `e2e/SysArithPipeSeg.lean` is the same module with the scan in
4 segments of at most 600 uses (`E2E_SEG_USES=600`; 3 checkpoints):
322.8 s wall, 188.6 s user, 2.29 GB (load 52), `obl` and `top` proved.
`SysArithPipe60.lean` (8 segments of at
most 128 uses) is the regression test in `build.sh`.

**Before and after the group form**, the same run's section heartbeats
(`MARK=1`, `tools_r7a/` against `tools/`): `guards` 60 → 38 Mhb (the check
rules' literal lists), `chains` 56 → 48 Mhb; every other section within 1 %.

## 14. mkBufferControl end to end

The same emitter, extended: 9 exported methods (Fin nM, pairwise CF, results
checked by `resM`), 18 rules (with task statements), 60 children (43 Reg,
11 FIFO2 from the library, 1 SizedFIFO and 3 RegFile assumed, one
`Tasks 18`), 303 sites, 85 conditional calls, 2 conditional (a) reads under
their reach, 5 readiness reads under their ready literal, 12 escaped pairs
with 50 dynamic facts, three urgency blockers on `RL_adjustingFreeSlots`,
34 muxes whose selectors carry the users' call conditions (93 member pairs,
40 by caller-exclusivity facts).

**Proved**: `MkBufferControl.obl : OblU (F ops E)` from `Hyps` and
`Unconfirmed`. Standard three axioms. 45.0 s wall, 44.4 s user, 1.13 GB (load 36).

- `Hyps`: disjointness; 4 readiness facts and 1 invariance fact, each
  confirmed by the site tool for the formula stated (site values: `rdy_0`
  over both return lists' site values with `inv_1`; `rdy_2`, `rdy_4` over
  their own site value, invariance by `inv_self`; `rdy_5` over unconditional
  reads); 50 escape facts, each confirmed by the solver for its site pair
  and the conditions stated; 39 caller-exclusivity facts.
- `Unconfirmed` (2 facts): `rdy_3`, the readiness fact of `RL_inputing /
  shortTermPicList_returnList.first`, which the site tool refutes; `cx_35`,
  the caller exclusivity of `lockInterLoads.write` by `RL_unlockInterLoads`
  and `RL_goToNextFrame`, which is action shadowing.
- `F_sites` by `rfl`: each step takes its skipped values only through
  `runSites`.
- Its two conditional (a) reads are split on their site condition itself
  (the same opaque operator), so a too-strong (a) condition could not fail in
  this file; its readiness splits are checked only through the stated facts
  (§7, trust point 1).

Round 7's first file had stated three readiness facts over a formula no tool
had checked (an opaque operator over the fan-in with every conditional read
removed) and labelled them confirmed; one of them (`rdy_0`) is satisfiable
against the guard under that reading (soundness review r7-1 M1). Those are
replaced by the site-value facts above.

**Not proved: `Acyc`, `built`, `top`.** In the synthetic netlist the address
of a shared RegFile `sub` copy is a mux selected by its users' WILL_FIRE, and
those users' guards read the copy's result (`shortTermPicList_rfile_rf` copy
0 for `removing`/`searching`/`listingAll`; `longTermPicList_rfile_rf` copy 1;
`refPicList_rf` copy 2 for the two inter-request rules): a combinational
cycle the rank pass reports. bsc's real netlist cannot have that cycle, so it
selects the address otherwise; the generator needs `aState`'s select signals,
which the dump does not carry (next round). The emitter then emits the
obligation only.

Because of the two unconfirmed facts the module is off path A under the gate
in the default mode, and the file is a structure and cost probe: its
`Unconfirmed` argument is explicit in `obl`'s statement.

| | modular (obligation only) | today's flat | task-agreement flat |
|---|---|---|---|
| elaboration (this revision) | 45.0 s wall, 44.4 s user, 1.13 GB (load 36) | 63.1 s wall, 51.7 s user, 1.43 GB (load 73) | 85.7 s wall, 67.7 s user, 1.59 GB (load 49) |
| ratio by user time, without / with `skel` (31.7 s user here; 19.4 s in the review) | — | 0.9× / 1.5× | 0.7× / 1.1× |

These runs were at unequal load (36 for the modular file, 49–73 for the flat
files), which flatters the modular side. Load-independent: the revised file's
heartbeats are 467 Mhb against 451 for round 7's first file (+3.5 %, nearly all
in `hq`, the site-value invariance proofs; back to back at one load, 40.7 s
against 42.2 s user). Scaling the cost review's one-load runs (28.0 s user for
the first file, load 13–19) by that +3.5 % gives about 1.0× / 1.55× today's file
and 0.69× / 1.16× the task-agreement file, without / with `skel`. The
task-agreement flat file is the fair comparison (the module carries 18 task
statements). The modular file still lacks `Acyc`, `top` (about 5 % of
sysArithPipe's file) and the consistency check the flat file has
(`fail_if_success bv_decide`).

## 15. Cost, recomputed

From the two modules and the probes (per-unit slopes are user time):

| item | measured | where |
|---|---|---|
| a site (key, table entry, facts, `HQ`) | 0.049–0.056 s (sysArithPipe without tasks) to 0.092–0.098 s (mkBufferControl, the port probe); memory ≈ 1.4–1.7 MB per site where steps are small (sysArithPipe 2.6–2.9 GB for 1,859 sites; mkBufferControl 1.1 GB for 303 sites over a 0.5 GB base); a large step costs more (Em row) | §13, §14, `emit/port7/` |
| a task statement | 0.49 Mhb ≈ 40–45 ms | cost review r6-2 minor 1 |
| a mux member of a `mutually_exclusive` group | linear (`SufAsm`, no per-member data): chain probe below; port probe guards and chains ×2.0 per doubling | `scan/chain8/`, `emit/port7/` |
| a mux member under a non-suffix check | O(members + check entries) by cover sets (C13): the checks' own data is quadratic when they are | `scan/chain7/` |
| an escaped pair | ≈ 1.2 ms per site of its first step, plus one fact per bad site pair | `scan/esc7/` |
| a method (mkBufferControl, 9) | per-method `resM` leaf, `HQ`/`RY` leaves, `cMk`/`aMk` per user | §14 |
| the scan | per segment ≤ 4,096 uses, ≤ 25 ms/use on every probed shape (§10) | §10 |
| one large step (Em series) | 1,025 / 2,049 / 3,073-site step (`Em512`/`Em1024`/`Em1536`): 132.5 / 293.2 / 476.3 s user, 3.60 / 8.13 / 12.9 GB (the first two the cost review's runs; `Em1536` at load 21); a 4,097-site step (`Em2048`) did not complete: exit 1 after 1,251 s wall, 812 s user, 294 s system, 16.9 GB peak RSS, under the 24 GB address-space cap (load 45–60; the log kept no error line) | `emit/out/` |
| `skel` | not emitted; atom test 23.8 s (sysArithPipe), 19.4 s (mkBufferControl), user time | cost review r7-1 |

**Mux chains, whole file** (round 7's first table quoted only the chain
section):

| members | cover sets (`chain7/`, the review's runs: whole file wall / user, RSS) | groups (`chain8/`, this revision: whole file wall / user, RSS; chain section) |
|---|---|---|
| 256 | 5.15 / 4.07 s, 0.76 GB | 0.28 / 0.20 s, 0.47 GB; chain section 0.02 s (load 23.83), kernel 10 ms |
| 512 | 19.8 / 15.0 s, 1.41 GB | 0.48 / 0.37 s, 0.48 GB; chain section 0.05 s (load 23.83), kernel 30 ms |
| 1,024 | 74.9 / 56.4 s, 3.91 GB | 1.04 / 0.61 s, 0.49 GB; chain section 0.14 s (load 23.83), kernel 0.12 s |
| 2,048 | — | 1.79 / 1.09 s, 0.51 GB; chain section 0.43 s (load 23.83), kernel 0.41 s |
| 4,096 | — | 2.51 / 1.89 s, 0.54 GB; chain section 0.31 s (load 28.01), kernel 0.29 s |
| 8,192 | — | 3.28 / 3.05 s, 0.61 GB; chain section 0.48 s (load 28.01), kernel 0.47 s |

The cover-set form grows ×3.8 per doubling (its check data is u²/2 entries);
the group form's file is the member list itself.

**Port probe** (`emit/port7/genport7.py`, the cost reviewer's r7cost probe on
the current emitter: one register written by U rules, bsc-shaped suffix
checks; whole file, wall / user, RSS, and the `guards` section):

| users | sites | before (review, load 9–35) | after (this revision) | `guards` before → after |
|---|---|---|---|---|
| 103 | 309 | 28.0 / 26.6 s, 1.15 GB | 34.2 / 30.6 s, 1.07 GB (load 41) | 4.50 s → 1.93 s, 21 Mhb |
| 206 | 618 | 64.7 / 60.4 s, 2.06 GB | 67.6 / 59.9 s, 1.66 GB (load 60) | 14.8 s → 5.29 s, 42 Mhb |
| 412 | 1,236 | 167.9 / 153.3 s, 5.48 GB | 147.2 / 126.9 s, 3.66 GB (load 36) | 53.6 s → 8.20 s, 83 Mhb |

(The probe files were emitted before the chain's member list became a
definition, C17; that change only shortens the chain section.)

User time ×1.96 and ×2.12 per doubling (was ×2.3 and ×2.5); the `guards`
heartbeats ×2.0 and ×2.0 (the wall times of these runs are inflated by load
36–60). Memory still grows ×1.55, ×2.2.

**Estimates for other designs** (single-module slopes, not measured; a range
from both modules' per-site slopes; memory from sysArithPipe's ≈ 1.5 MB per
site plus the largest step at the Em slope):

- sysFloatTest (3,858 sites, 614 statements, 988 mux members, largest 111;
  flat in the default mode anyway): 190–380 s for sites, 26 s for
  statements, chains a few seconds, plus `skel` (≈ 186 s): ≈ 400–590 s,
  2.4–3.5× today's 170 s; ≈ 6–8 GB.
- sysConflictFreeOKLarge (6,667 sites, 2,050 statements, one step of 2,562
  uses): 330–650 s for sites, 90 s for statements, plus `skel`; ≈ 10–16 GB (its sites at the small-step rate, its 2,562-site step between `Em1024` and `Em1536`; not built, near the 24 GB cap at the top of the range).
- sysBRAM0Test (85 muxes, 394 members, largest 206): chains linear now; its
  3,432 uses fit one segment.

## 16. Primitives

| primitive | theorems |
|---|---|
| `RegN.v` (`RegUN`, `RegA` map here), `CRegN5.v` | `reg_realizes`, `creg_realizes n`, `creg_causal n` |
| `RWire`, `RWire0`, `BypassWire*` | `rwire*_realizes`, `*_causal` |
| `FIFO2.v` (`vMk2FIFOF`, guarded) | `fifo2_realizes`, `fifo2_causal`, `fifo2_aliased` |
| `FIFO20.v` | `fifo20_realizes`, `fifo20_causal`, `fifo20_aliased` |
| `Tasks n` | `tasks_realizes`, `tasks_causal`, `tasks_aliased` |

Not covered (assumed leaves, explicit `Leaves` fields): FIFOL1, SizedFIFO
(with FIFO2's interface, plus its `Aliased`), RegFile (`RF.X`: upd and five
`sub` copies with a path from each copy's address to its result), ConfigRegN,
the BRAMs, Counter, Probe. `Leaves` is not shown inhabited: a wrong assumed
schedule or path table makes `top` vacuous, not false. A generated child's
`RDY_m` is read as a port (`exactRet`, `aliased_toModuleU`; `Ex.RdyPort`).

## 17. Examples and negatives

`Example.lean`: the toy hierarchy, `Asm`, `WireEx`, `AliasEx`, `Fsm`,
`Show`; (n) `UOrder` (`obl_each`, `not_oblU`); (o) `RdyPort`. Negatives (a)–(m)
as in round 6, `TaskRev.not_realizes`, `ScanExLate.no_escape`,
`h3_catches`, `ScanTasks.no_chain`, `not_okT`, `no_pairFacts`,
`no_pairFacts_all` (no pair facts for `Pbad` in rank order whenever rules 0
and 1 fire, tying `no_chain` to `h4`), `EqvTest.no_trans`, `no_trans_cong`.

## 18. Emission trust points

No Lean check sees these:

1. `comb`'s fidelity to bsc's netlist (`aState`): enables, muxes, RDY,
   outputs, task conditions and arguments. In the e2e files the netlist is
   synthetic (opaque operators over the real fan-in, uses of conditional
   reads split on their reach); an (a) read's split condition and its site
   condition are the same operator, so a too-strong (a) condition cannot fail
   there, and a readiness split is faithful only if its `R` covers where the
   read matters, which the site tool's facts are about.
2. Completeness of `onehot`: every AND-OR mux listed.
3. `Mod.asm`'s content: only bsc's assertion list (a `mutually_exclusive`
   group as `SufAsm` over its members, asserted by the generator to be
   exactly bsc's checks).
4. The `Tasks` child numbered in bsc's Verilog order (rank order required).
5. The library model chosen by Verilog module name and checked against the
   instance's `VModInfo`.
6. A BVI interface's `hasEn`, `alwaysEn`, `alwaysRdy` and `path`.
7. **Every skipped value is the step's own** is now a Lean fact about the
   file: `F_sites : F ops E = withSitesX (F ops E) ssM resM ssR resR` by
   `rfl`, where the site lists and results receive the method's argument but
   not the skipped values (`EmitLemmas.withSitesX`). What remains trusted is
   that the site lists are the design's (item 1).

## 19. Not proved, and open issues

1. **No generator yet**: `e2e/tools/mke2e.py` is a prototype over the dump
   and the APackage.
2. **The model-to-design step** (§7) is an argument outside Lean; it assumes
   reads at skipped sites are side-effect free.
3. **mkBufferControl's `Acyc`/`top`**: blocked by the synthetic RegFile copy
   address muxes; needs bsc's real select signals for shared value-method
   copies (next round).
4. **Action shadowing** (two callers of a self-SBR port in one cycle) fails
   safe; it keeps 5 corpus designs off path A (§10). Last-caller arguments for
   self-SBR ports with two callers: next round.
5. `skel` and the consistency check are not emitted.
6. Path B is defined, not built.
7. Working-set checkpoints are not proved; full-state checkpoints remain,
   bounded by the gate's slot, segment and code limits. The within-segment
   growth of the two-reads shape is unexplained; the segment bound keeps it
   within about 25 ms per use.
8. Conditions over hardware wires: not expressible; sysFloatTest's 10 facts
   stay at risk in the default and `-no-aggressive-conditions` modes.
9. The 28 corpus designs without site conditions were not recompiled in the
   three other modes; 10 path-A designs export non-CF method pairs, which the
   prototype emitter does not emit (§10).
10. `oblU_weakens` derives results and the call superset; legality,
    readiness and `u`-independence of the extra calls are the extra
    obligations weakening adds.
11. A method called twice shares its skipped values (argued in §7, no
    theorem about the real design).
12. More primitives, RST, multiple clocks, multi-cycle traces; `Leaves` not
    shown inhabited; value-returning tasks' results are free outputs.
13. **One large step**: the Em series' memory grows ×2.26 from a 1,025- to a
2,049-site step and ×1.59 from 2,049 to 3,073 sites; a 4,097-site step does not
complete under the cap. The cause is not found; the gate bounds a step at 3,072
sites (corpus maximum 2,562).
14. A mux whose members lie in two large groups gets the larger group's pairs
    from `SufAsm` and proves the others' pairs one by one (`meG`); none occurs
    in the two modules.
15. (a)-read conditions over conditional reads (site-value form with an
    invariance fact) are designed but not exercised; the emitter asserts
    there are none.
16. Kernel time is not separated in the e2e measurements; the per-site and
    per-segment memory growth (×2.2 per doubling at the top of the port and
    Em probes) is unexplained.

## 20. Review items and where they are fixed

| review item | fix | evidence |
|---|---|---|
| soundness r6-2 M1: order not fixed before `u` | `ParentU`/`ModuleU`, `OblU` (τP and σC before `u`), `CertU` with `trU`, `RealizesU`; generated files prove `OblU` | `Modular.lean` §2, `Ex.UOrder`, both e2e files |
| soundness r6-2 M2: the always-ready `first` drops readiness | removed (Ravi); the requested wider orientation search superseded by decision (b), not done | §7 |
| soundness r6-2 minors 1–6 | as in round 7's first table: `oblU_weakens`, `ParentU.at`, `no_pairFacts`, `exactRet`/`RdyPort`, mkBufferControl | §2, §7, §16, §17 |
| cost r6-2 M1–M4, minors | `covB`, tables split, rule codes, mkBufferControl, the gate | §8, §10, §14, §15 |
| **soundness r7-1 M1**: readiness verdict kept cyclic literals; three facts mislabelled; `Unconfirmed` incomplete | site-value form in the site tool (readiness + invariance, `EncS`), `R` over site values in the file (`evs`, `inv_self`, `inv_n`), each fact labelled by the verdict for its formula; 22 of 24 recount; mkBufferControl's `Unconfirmed` now the refuted readiness fact and the shadowing fact | §7, §14, `sites/rule.md` §11, `e2e/MkBufferControl.lean` |
| soundness r7-1 minor 1: trust point 7 | `withSitesX`/`withRuleSites`, `F_sites` by `rfl` in every e2e file | §18 item 7 |
| minor 2: the synthetic split hides the (a) check | stated beside trust point 1 | §7, §18, `e2e/README.md` |
| minor 3: escape labels design-global | each escape fact checked by the solver for its pair and stated conditions | §6, §14 |
| minor 4: wording | §7 for readiness reads; `rule.md` opening bullet and §3 item 2 | §7, `sites/rule.md` |
| minor 5: orientation search not recorded | recorded as superseded | §7, this table |
| minor 6: carried over | disclosed (`Leaves`, trivial `trU` in sysArithPipe, kernel time) | §13, §16, §19 |
| **cost r7-1 M1**: mux coverage quadratic | `SufAsm` groups (O(u) data, `amoChain_of_grp`, `amoChain_of_cover_grp`, `meG`), check rules' guards over the group list; whole-file numbers | §8, §15, `emit/conventions.md` C17 |
| **cost r7-1 M2**: gate bounds | segment bound recalibrated to 4,096 uses on three shapes; segmentation in the emitter, tested on a 4-segment sysArithPipe and an 8-segment 60-rule file; a per-step site bound from Em2048; corpus maxima corrected | §10, §13, §15 |
| cost minor 1: `skel` beside mkBufferControl's ratio | quoted | §14 |
| minor 2: load-60 run | re-measured; ratios restated | §13 |
| minor 3: codes grow slot records | restated; bound on codes | §10 |
| minor 4: estimates | ranges from both modules, memory | §15 |
| minor 5: checkpoint growth | the segment rule, stated | §10 |
| minor 6: unexercised designs | stated beside the outcome | §10 |
| minor 7: task-chain doubling | quoted as a range | §10 |
| minor 8: mismatches | chain figures whole-file, `emit/out/Em512`/`Em1024` regenerated (byte-identical to the reviewer's), port probe in `emit/port7/` | §15 |
| round 6 and earlier | as in `prev-r6/design.md` §19 | — |
