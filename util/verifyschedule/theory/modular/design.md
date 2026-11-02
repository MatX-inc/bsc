# Round 5: the modular proof (revision 3)

Each module bsc compiles gets a contract, `Realizes`, stated over its own
interface: its method ports and the annotations it exports. A module built
from child instances meets its contract when two things hold:

- every child meets its own contract and respects its output aliases;
- the module meets a per-module obligation, `Obl`, which mentions the
  children only through their interfaces.

The composition theorem is proved once in Lean. Induction over the instance
tree then gives the top module's contract. Revision 3 adds the second half
of the top theorem: **every input has a hardware cycle** (`Causal`), so the
contract can no longer be vacuous. A module gets it from its children's and
from a decided acyclicity certificate (`Acyc`). Without the certificate the
module is not `Built`.

The top theorem now reads (`Built.top`):

```lean
theorem Built.top [Inhabited V] {X : Iface} {Mo : Module V X} (h : Built X Mo) :
    Realizes Mo ∧ ∀ s w, Mo.clean s = s → ∃ o s' g, Mo.hw s w o s' g
```

`Realizes` covers the hardware cycles in which the assertions hold
(`Mo.ok g`, an explicit hypothesis). `Mo.ok` unfolds to the conjunction of
each instance's `asm` on its own signals.

§17 maps each item of both round-3 reviews to its fix.

## Files (all in this directory)

| file | what | lines | check |
|---|---|---|---|
| `Modular.lean` | the theory (below) | 3,912 | 17.2 s, 0.76 GB |
| `Example.lean` | the toy hierarchy, `Asm`, `WireEx`, `AliasEx`, `Fsm`; negatives (a)–(j), `FsmNo` | 2,351 | 19.1 s, 0.91 GB |
| `Scale.lean` | 4096 children and 4096 rules indexed by `Fin n` | 170 | 0.5 s, 0.50 GB |
| `split/` | the toy as per-module files (§10) | 471 | 0.2–2.0 s each; `leanchecker --fresh` 44 s |
| `tools/mksites.py`, `probe/Sites{4,16,32,64,128}.lean` | conditional sites as a first-order site list (§7, §11) | — | `probe/sites-times.txt` |
| `tools/mkcond.py`, `probe/Cond{4,16,32}.lean` | the same probe with nested `callIf`s (for comparison) | — | §11 |
| `tools/atomize2.py`, `tools/runatom.sh`, `atom/` | the abstract-value test of six corpus designs with EqLaw chains (§4) | — | `atom/results*.txt` |
| `tools/portscan.py`, `tools/portscan.out` | revision 2's footprint scan over the corpus (§9) | — | — |
| `prev/`, `prev2/` | revisions 1 and 2 | — | — |

Everything is core Lean 4.35: no Mathlib, and no `sorry`, `admit`, `axiom`,
`native_decide`, `implemented_by` or `unsafe`. Every Lean run used
`prlimit --as=25769803776`, `timeout`, `-j1` and `-DElab.async=false`, one
process at a time.

    lean -o build/Modular.olean -i build/Modular.ilean Modular.lean
    LEAN_PATH=build lean Example.lean
    LEAN_PATH=build lean Scale.lean
    LEAN_PATH=build lean probe/Sites64.lean

Every `#print axioms` lists at most `propext`, `Classical.choice` and
`Quot.sound`. The exceptions are `Ex.Asm.skel` and the theorems built on it,
which also list `Ex.Asm.skel._native.bv_decide.ax_7`, as generated files do
today.

`Modular.lean` contains:

- the model: `Ann` (with `same`, `rdyRet`, `path`), `Module` (with `Sig`,
  `ok`, `clean_idem`), `Legal`, `Drives`, `Realizes`, `Causal`, `Aliased`;
- `Parent`, `toModule`, `Obl`, `compose`;
- `Acyc`, `Acyc.ofMoore`, `causal_toModule`, `Built`, `Built.realizes`,
  `Built.causal`, `Built.top`;
- `Realizes.restrict`, `Causal.restrict`, `Aliased.restrict`;
- primitives: CReg n, Reg, the wire model and bsc's eleven wire imports,
  FIFO2 with 8 ports, FIFO20, each with `_realizes` and `_causal`, and
  FIFO2/FIFO20 with `_aliased`;
- `place_ok`, `Cert`, `Env`, `EnvC`, `obl_of_cert`;
- emission support: `EqC`, `amoChain_of_law`; `Prog.callIf`,
  `trace_callIf`, `Within`, `Seq`, `NoFail`, `calls_iff_any`; site lists
  `Site`, `runSites`, `envHw`, `trace_runSites`, `envHw_at`,
  `calls_sites`, `mem_proj_callsOf`, `grpT_sites`; `okT_of_static`,
  `h4_of_static`.

## 1. The model and the contract

```lean
structure Ann (M : Type) where
  prec sbr conf : M → M → Bool         -- SB/SBR, SBR, C (as before)
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
  Sig : Type                                        -- the cycle's internal signals
  hw : S → Wires X.M V → Outs X.M V → S → Sig → Prop -- the Verilog's cycles, no assertions
  ok : Sig → Prop                                   -- the assertions below the module hold

def Realizes (Mo : Module V X) : Prop :=
  ∀ s σ w o s' g, Mo.clean s = s → Legal X.ann σ → Drives X.ann w σ → Mo.hw s w o s' g → Mo.ok g →
    (∀ c ∈ σ.flatten, X.ann.alwaysRdy c.1 = false → o.rdy c.1 = true) →
    ∃ τ fin, groups τ = σ ∧ Exec Mo o s τ fin ∧ s' = Mo.clean fin

def Causal (Mo : Module V X) : Prop :=
  ∃ out : Mo.S → Wires X.M V → Outs X.M V,
    (∀ s w w' q, (∀ p, X.ann.path p q = true → AgreeW w w' p) → AgreeO (out s w) (out s w') q) ∧
    (∀ s w, Mo.clean s = s → ∃ s' g, Mo.hw s w (out s w) s' g)

def Aliased (toB : V → Bool) (Mo : Module V X) : Prop :=
  ∀ s w o s' g, Mo.clean s = s → Mo.hw s w o s' g → AliasOK toB X.ann o
```

What changed from revision 2:

- **The hardware relation no longer contains the assertions.** `hw` is the
  Verilog's cycles with their internal signals `g`, and `Realizes` assumes
  `Mo.ok g` explicitly. A reader of the top statement sees the excluded
  cycles as `Mo.ok`, which unfolds to each instance's `asm` on its signals
  (`Ex.Asm.ok_iff` shows it for `Asm`).
- **`Causal`** says what `Realizes` cannot: the outputs are a function of
  the state and of the inputs on declared paths, and every input has a
  cycle (§5).
- **`Aliased`** says that a module's outputs respect its declared aliases
  (§7, FIFO guards).
- **`clean_idem`** is a field; every primitive and `toModule` prove it.

Revision 1's choices stand: argument-less value methods are one port read
any number of times; ports with arguments have one port per copy, and
`share` makes a copy's calls agree; a self-SBR action port drives the last
call's argument; readiness is assumed at the cycle's start and enabledness
is proved at the call's position.

## 2. Built modules and the obligation

```lean
structure Parent (V : Type) where
  I : Type; decI; kids; kids_nodup; kids_all
  CI : I → Iface;  X : Iface;  PR : Type
  body : X.M → V → Prog I (fun j => (CI j).M) V
  rbody : PR → Prog I (fun j => (CI j).M) V
  comb : ((j : I) → Outs (CI j).M V) → Wires X.M V → ((j : I) → Wires (CI j).M V) × Outs X.M V
  asm onehot : ((j : I) → Outs (CI j).M V) → Wires X.M V → Prop
  toB : V → Bool := …    -- the design's Bool reading of a 1-bit value (for children's aliases)
```

`P.toModule impl`: its state is the children's states; its signals are
`PSig` (the module's input wires, its children's outputs `oc` and inputs
`wc`, and each child's own signals); its hardware is

```lean
hw s w o s' g := g.w = w ∧ (∀ j, (impl j).hw (s j) (g.wc j) (g.oc j) (s' j) (g.sub j)) ∧
  (∀ j, (g.wc j).en = ((P.comb g.oc w).1 j).en) ∧ o.rdy = (P.comb g.oc w).2.rdy ∧
  (P.onehot g.oc w → g.wc = (P.comb g.oc w).1 ∧ o = (P.comb g.oc w).2)
ok g := P.asm g.oc g.w ∧ ∀ j, (impl j).ok (g.sub j)
```

```lean
def Obl (P : Parent V) : Prop :=
  ∀ oc w σ, P.asm oc w → (∀ j, AliasOK P.toB (P.CI j).ann (oc j)) → Legal P.X.ann σ → Drives P.X.ann w σ →
    (∀ c ∈ σ.flatten, P.X.ann.alwaysRdy c.1 = false → (P.comb oc w).2.rdy c.1 = true) →
    P.onehot oc w ∧ ∃ τP, groups τP = σ ∧ (∀ e ∈ τP, StepOK P oc (P.comb oc w).2 e) ∧
      ∀ j, Legal (P.CI j).ann (sigmaJ P oc τP j) ∧ Drives (P.CI j).ann ((P.comb oc w).1 j) (sigmaJ P oc τP j) ∧
           ∀ c ∈ (sigmaJ P oc τP j).flatten, (P.CI j).ann.alwaysRdy c.1 = false → (oc j).rdy c.1 = true
```

The only change is the alias hypothesis: the children's outputs are free,
but they respect their interfaces' aliases.

## 3. The composition and hierarchy theorems (proved once)

```lean
theorem compose (P : Parent V) (impl : (j : P.I) → Module V (P.CI j))
    (hC : ∀ j, Realizes (impl j)) (hA : ∀ j, Aliased P.toB (impl j)) (hO : Obl P) :
    Realizes (P.toModule impl)

theorem causal_toModule [Inhabited V] (P : Parent V) (impl : (j : P.I) → Module V (P.CI j))
    (hC : ∀ j, Causal (impl j)) (A : Acyc P) : Causal (P.toModule impl)

inductive Built : (X : Iface) → Module V X → Prop
  | leaf (Mo : Module V X) : Realizes Mo → Causal Mo → Built X Mo
  | node (P : Parent V) (impl : (j : P.I) → Module V (P.CI j)) :
      (∀ j, Built (P.CI j) (impl j)) → (∀ j, Aliased P.toB (impl j)) → Obl P → Acyc P →
      Built P.X (P.toModule impl)

theorem Built.realizes (h : Built X Mo) : Realizes Mo
theorem Built.causal [Inhabited V] (h : Built X Mo) : Causal Mo
theorem Built.top [Inhabited V] (h : Built X Mo) :
    Realizes Mo ∧ ∀ s w, Mo.clean s = s → ∃ o s' g, Mo.hw s w o s' g
```

A BVI leaf now needs both its assumed contract and its assumed causality:
`Built.leaf bvi hR hC`. Its `path` table comes from its `VModInfo`, so a
wrong path table is part of the assumed contract.

## 4. Values, operators and constants (integration review 2, B1)

**Decision: constants and their comparison are an operator with one law.**

```lean
structure EqC (V : Type) where
  eqc : V → Nat → Bool          -- x == k, for a constant k already truncated to x's width
  law : ∀ x k k', k ≠ k' → eqc x k = true → eqc x k' = true → False

theorem amoChain_of_law (E : EqC V) (x : V) (ks : List Nat) (acc : Bool) :
    ks.Nodup → (acc = true → ∀ k ∈ ks, E.eqc x k = false) → amoChain acc (ks.map (E.eqc x))
```

- bsc's operators satisfy the law: on `(n : Nat) × BitVec n`,
  `eqc ⟨n, x⟩ k := x.toNat == k`.
- The generator writes every comparison with a constant as `E.eqc x k`, and
  for each operand compared with two or more constants it states one chain
  (`amoChain`, prefix ORs, linear, as today's `disjointChains`), proved from
  the law and handed to `skel`.
- The top theorem takes `E : EqC V` in place of a fact per pair.
- Lean evidence: `Ex.Fsm` is a two-state FSM whose rules fire on `st == 0`
  and `st == 1` with an AND-OR mux, as `StmtFSM` produces. `Fsm.skel` is one
  `bv_decide` with the chain as the only fact relating the two atoms, and
  `Fsm.obl` holds for every `V`, operators and `EqC`. Without the law,
  `FsmNo.not_obl` shows the obligation false.

**Corpus test** (`tools/atomize2.py`, `atom/`). Every `i.F == «k»` (an
input) and every `v.F == «k»` (a view field) becomes a free Bool. Then the
script adds what the modular plan emits:

- one EqLaw chain per operand: per input field, and per view field and view
  instance;
- the congruences lifted to the atoms. Where a hypothesis equates two
  values (`A = B`, such as a view's read agreeing with the hardware's), it
  also equates their comparisons with each constant. In the modular plan
  these are not hypotheses at all: a read is the child's output wire on both
  sides.

The theorems after `schedule_sound` (the exported orders) are dropped.

| design | atoms (input / view) | chain facts | lifted facts | result |
|---|---|---|---|---|
| sysArithPipe | 161 / 0 | 134 | 2 | **passes**, 25.0 s, 2.6 GB (failed in 210 s without chains) |
| mkBufferControl | 114 / 54 | 543 | 204 | passes, 22.9 s, 1.6 GB (view-form comparisons included) |
| mkDeblockFilter | 67 / 16 (+6 other) | 148 | 46 | passes, 17.0 s, 1.6 GB (partial, see below) |
| sysFloatTest | 398 / 0 | 378 | 2 | passes, 185.7 s, 11.0 GB |
| mkMCR | 35 / 0 | 24 | 0 | passes, 9.6 s, 1.3 GB |
| sysBasic | 32 / 0 | 31 | 0 | passes, 3.0 s, 0.9 GB |

What the test does not cover yet:

- **Computed operands.** Comparisons of a computed value with constants,
  such as mkDeblockFilter's case tables (`indexA i == «6'd0»`, …: 988
  occurrences), stay interpreted. Under `EqC` they are atoms with one chain
  per operand term, the same mechanism.
- **Comparisons between two values.** They stay interpreted: 2 to 11 per
  design. Under an abstract `V` each would be an atom `eqv x y`. The plan is
  to add `eqv` to `EqC` with reflexivity, symmetry, transitivity and
  `eqv x y → eqc x k → eqc y k`, stated as facts only for the operand pairs
  that occur. It is not yet tested; open issue 2.

**Unchanged from revision 2.** One abstract `V`, each module's operators a
parameter (`Mod.Ops V`), and `bv_decide` on the Boolean skeleton with data
comparisons as atoms. Data equalities close by `simp` once the selector is
known. bsc's solver verdicts stay hypotheses (`Mod.Hyps ops`).

## 5. Non-vacuity, assertions, parallel muxes (soundness review 2)

**Causal totality, proved once.** The parent's hardware is a fixpoint
through its children. A combinational loop through a child's path (a wire, a
CReg port, a module with combinational methods) can empty it, and with it
every ancestor's. `Ex.Loop` is the reviewer's probe: `Loop.obl` holds, from
a certificate, yet `Loop.hw_empty` shows no hardware cycle exists.

```lean
structure Acyc (P : Parent V) where
  D   : (j : P.I) → (P.CI j).M → List (Σ j, (P.CI j).M)   -- children's outputs child j's input p reads
  Dw  : (j : P.I) → (P.CI j).M → List P.X.M               -- parent inputs it reads
  Do  : P.X.M → List (Σ j, (P.CI j).M);  Dwo : P.X.M → List P.X.M   -- same, for the parent's outputs
  dep  : comb's child inputs depend only on D, Dw      -- proved per port by simp
  depo : comb's outputs depend only on Do, Dwo
  rk : (Σ j, (P.CI j).M) → Nat;  N : Nat;  rk_lt : ∀ y, rk y < N
  rk_ok : ∀ j q p, (P.CI j).ann.path p q = true → ∀ y ∈ D j p, rk y < rk ⟨j, q⟩   -- no loop
  reach : (Σ j, (P.CI j).M) → List P.X.M               -- parent inputs each child output reaches
  reach_w, reach_o : reach is closed along D, Dw and the children's paths
  path_w, path_o : the parent's declared paths contain what its outputs reach
```

- `causal_toModule` evaluates the fixpoint by `N` rounds of iteration.
  `iterOC_stable` proves that after more rounds than its rank, an output no
  longer changes. `iterOC_reach` proves that an output depends only on the
  parent inputs it reaches.
- Hardware data are taken as `comb`'s, which `hw` allows whether or not
  `onehot` holds.
- For a parent whose children have no paths, `Acyc.ofMoore` needs only the
  read lists and `dep`/`depo`.
- `Ex.WireEx` has a real path: `t <= w.wget` and `w.wset(r)` in one cycle
  through bsc's `vMkRWire`. Its certificate ranks `r.read ↦ 0` and
  `w.wget ↦ 1`, and `WireEx.top` gives both halves.
- `Ex.Loop.no_acyc : Acyc P → False`: in the probe, `w.wset`'s enable reads
  `w.whas`, and the wire's path `wset → whas` would need
  `rk whas < rk whas`.
- **Primitives:** `creg_causal` (paths `wr k → rd j` for `k < j`),
  `wire_causal` (`wset → wget, whas`), `fifo2_causal` (no paths), and,
  through `Causal.restrict`, all eleven wire imports and FIFO20.
- **BVI:** causality is assumed with the contract (§3).

**Assertions are visible (soundness minor 1).** `asm` is no longer inside
`hw`. The generator emits each module's `asm` as a named definition built
only from bsc's assertion list (`Mod.asm`), and sets `P.asm := Mod.asm`.

**The consistency check (soundness minor 2, integration minor 7).** Today's
`fail_if_success` refutes all hypotheses together. The modular file does the
same over `Mod.asm`, the instantiated solver verdicts (`Mod.Hyps` at the
module's atoms) and the EqLaw chains. `Ex.Fsm.chain_consistent` shows the
pattern. Where feasible, a solver verdict is also discharged on `BitVec` by
`bv_decide`, so that `Hyps ops` is a theorem for bsc's operators.

**Parallel muxes, and the precondition of "Booleans are exact" (soundness
minor 3).**

- `Cert.onehot` now assumes only `EnvC`: the assertions, the aliases and the
  exported C pairs. It never assumes the parent's own RDY, which `comb`
  computes.
- `hw` fixes enables and RDY to `comb`'s even when `onehot` fails. That
  matches the Verilog only if no Boolean in `comb` depends on a value that
  passes through an AND-OR mux. The generator checks this on the netlist:
  every `comb` Boolean's fan-in contains no parallel-mux output. Values
  routed through a child are fine, since `wc.arg` and `oc` are existential.

**Reset.** RST is not modelled: the theorems cover cycles out of reset.
FIFO2's CLR is modelled.

## 6. The headline theorem of a generated module file

Obligation file `<Mod>.lean`:

```lean
variable {V : Type} (ops : Mod.Ops V) (E : EqC V)
theorem Mod.obl (hyps : Mod.Hyps ops) : Obl (Mod.P ops E) := obl_of_cert _ (Mod.cert ops E hyps)
def Mod.acyc : Acyc (Mod.P ops E) := …           -- read lists from the netlist, ranks, reach
theorem Mod.realizes_of (hyps) (impl : (j : Fin Mod.nK) → Module V (Mod.CI j))
    (hC : ∀ j, Realizes (impl j)) (hA : ∀ j, Aliased ops.toB (impl j)) :
    Realizes ((Mod.P ops E).toModule impl) := compose _ impl hC hA (Mod.obl ops E hyps)
```

`<Mod>Realizes.lean`:

```lean
theorem Mod.built [Inhabited V] (env : Mod.Env V) : Built Mod.X (Mod.mod env) :=
  Built.node (Mod.P env.ops env.E) (Mod.impl env) (…children's `built`, by kind…)
    (…children's `aliased`…) (Mod.obl env.ops env.E env.hyps) (Mod.acyc env.ops env.E)
theorem Mod.top [Inhabited V] (env : Mod.Env V) := (Mod.built env).top
```

`Mod.Env V` is a bundled record (integration minor 2):

- the module's `ops`, `E` and `hyps`;
- each child module type's `Env`, by kind, so the module-type DAG unfolds
  into a tree of records;
- each BVI child's module, contract and causality.

The driver builds it once per design. In the toy: `Ex.Top.top`,
`Ex.Asm.top`, `Ex.WireEx.top`, `Ex.AliasEx.built`, and
`split/ExTopRealizes.lean`.

## 7. What a generated module file emits

**Interface file `<Mod>Iface.lean`:** `M := Fin nM` with an `abbrev` per
port; `ann` as per-method partner tables, with `path` from bsc's path
analysis of the module (§11) and `same`/`rdyRet` empty; `X : Iface`.

**Obligation file `<Mod>.lean`:**

| item | content | source today |
|---|---|---|
| `nK`, `CI` | children by kind | `dChildren` |
| `PR := Fin nR` | rules in schedule order | rule steps |
| `Ops`, `E`, `Hyps` | operators; constants (`EqC`); solver verdicts as `∀ oc w` facts | `Oracles` |
| `sites`, `res` per step | the step as a site list (below) | `stReads`, `stCalls`, `stGuard`, `stOutputs` |
| `body`, `rbody` | `runSites u res sites 0 u` | — |
| `comb` | enables, argument muxes, WILL_FIRE, outputs, RDY | section 4 |
| `Mod.asm` | bsc's assertion list only, per cycle | `Assertions` |
| `onehot` | every parallel mux's selectors, as a chain | `Hw.oneHot` |
| `P : Parent V` | the above, `toB := ops.toB` | — |
| `fp`, `users` | per step and per child port, as tables | `Run.fp` |
| `skel` | one `bv_decide`: every per-cycle Boolean fact under `Env`, with the EqLaw chains and exception chains as hypotheses | `schedule_sound` |
| `cert : Cert P` | fields from `skel`'s conjuncts and the scan (§9) | — |
| `acyc : Acyc P` | the netlist's fan-in per child input and output, ranks, reach | new |
| `obl`, `realizes_of` | §6 | — |

**Steps as site lists (integration review 2, M1).**

```lean
structure Site (I) (CM : I → Type) (V) where
  j : I;  m : CM j;  cond : (Nat → V) → Bool;  arg : (Nat → V) → V
def runSites (u) (res) : List (Site I CM V) → Nat → (Nat → V) → Prog I CM V   -- a chain of callIf
theorem trace_runSites : SitesOK ss n →
    trace oc (runSites u res ss n ρ) = (callsOf (envHw u oc ss n ρ) ss, res (envHw u oc ss n ρ))
theorem envHw_at : SitesOK ss n → envHw u oc ss n ρ (n + i) =
    if ss[i].cond (envHw …) then (oc ss[i].j).ret ss[i].m else u (n + i)
theorem calls_sites : Calls P (callsOf ρ ss) j cm ↔ ∃ s ∈ ss.filter (onPort j cm), s.cond ρ = true
```

- A step is a list of sites: each a read or an action call, its child and
  port, its condition and its argument. Both are functions of the read
  environment `ρ`, where `ρ n` is the value site `n` returned, or its
  skipped value `u n`.
  - Today's `stReads` (with use conditions) and `stCalls` (`clCond`,
    `clArgs`) map onto it directly.
  - The environment replaces long parameter lists (integration minor 5).
- **Per design:** one `ReadsBefore` fact per site, proved by one rewrite per
  read (`sitesOK`).
- **Once in the library:**
  - the trace (`trace_runSites`): all conditions are read at one
    environment, the hardware's;
  - each environment value (`envHw_at`);
  - the per-port reflection (`calls_sites`, `mem_proj_callsOf`);
  - the footprint (`callsOf_ports`) and one-caller (`grpT_sites`) facts.
- **No per-design proof substitutes a value into a continuation.** In the
  nested-`callIf` form (`Prog.callIf`, `trace_callIf`, `Seq`, `NoFail`,
  `calls_iff_any`, also in the library) that substitution costs quadratic
  term work (§11).
- **Conditional reads: the contextual rewrite** (§8, `Congruence`). A value
  read under `c` is `if c then (oc j).ret m else u n` (`envHw_at`). Each use
  under its condition is rewritten:
  - `c && f (if c then a else u) = c && f a`, by `cases c`;
  - `if_pos` on data, under the site's condition (`Sites*.arg_r*`).

  One rewrite per use site. A use that depends on the skipped value cannot
  be proved for every `u`. The use-condition bugs found in mkDeblockFilter
  and mkMCR are therefore caught.

**FIFO guards and output aliases (integration review 2, M3).** FIFO2 and
FIFOL1 drive several ports from one wire.

- `fifoAnn` declares the aliases:
  - `same notFull i_notFull`;
  - `same notEmpty i_notEmpty`;
  - `rdyRet enq notFull`, `rdyRet enq i_notFull`;
  - `rdyRet deq notEmpty`, `rdyRet deq i_notEmpty`;
  - `rdyRet first notEmpty`, `rdyRet first i_notEmpty`.
- `fifo2_aliased` proves them for a Bool reading that inverts the FIFO's
  flag encoding. `Obl` and `Env` assume them for every child.
- **Convention.** `comb` uses the netlist's one wire, named after the port
  the `.ba` gives it (`i_notFull`). Each step's site calls the port it reads
  for ordering: `notFull` for an explicit read, `i_notFull` where bsc reads
  the implicit condition.
- `Ex.AliasEx` is the pattern. `canPut` reads `notFull` explicitly, and
  `comb` returns the `i_notFull` wire and uses `toB` of it as `put`'s RDY.
  The certificate relates the three through the aliases.
- A guard read that bsc merged for two methods (`first`'s and `deq`'s
  `i_notEmpty`) is the conjunction of both RDYs, `rdy first && rdy deq`;
  mapped to one alone, `rdyR` fails for the other (soundness minor 4).

**Other conventions** (unchanged): dummy arguments `ops.z`; system-task
reads are sites under their reach conditions; library children's
annotations come from the `.ba` as schedule statements and are checked
against the library's by `decide` (`fifoAnn_sched`); multi-argument ports
take one tuple value `ops.tup x y`, with the parent and the child model
applying the same projections (integration minor 4).

## 8. What changes versus today's emitted file

| today | modular | where it goes |
|---|---|---|
| `State` fields `«c$EN_m»`, `«c$m_1»`; `Agree` | child wires `(comb oc w).1 j`; `Drives` | `Cert.en`, `Cert.arg` |
| `illegal_access`, per-step checks | `Legal` of each child's induced sequence | `grp*`, `mm`, `mmg`, `h1`–`h4`, `place_ok` |
| `guard_violated` | `StepOK` | `resM`, `resR` |
| `View`, `View.at_<step>` | every read is the child's output wire `oc j` | — |
| `Congruence`: view reads | gone (one wire); conditional reads by the contextual rewrite (§7) | per use site |
| `Congruence`: application, twins | shared definitions; a `congrArg` lemma where two forms are unavoidable | — |
| `chAlias` | `Ann.same`, `Ann.rdyRet`; `Aliased` | `fifo2_aliased`, `Env` |
| `Primitives` | library contracts and causality | `Built.leaf` |
| `Environment` | the contract's hypotheses; `Env` | `Realizes`, `Cert` |
| `Oracles` | `Hyps ops`, `∀ oc w` | `skel` |
| constant comparisons on tokens | `EqC.eqc`; EqLaw chains | `skel` |
| `Assertions` | `Mod.asm`, visible in `Realizes` as `Mo.ok` | `toModule.ok` |
| `Hw.oneHot` | `Parent.onehot`, from `EnvC` | `Cert.onehot` |
| `fail_if_success` | over `asm`, instantiated `Hyps` and chains | §5 |
| `disjointChains` (exceptions) | exception groups as `amoChain`s (§9) | `h4_of_static` |
| none (the flat theorem computes the hardware) | `Acyc`; `Causal` | `Built.top` |
| `Run.*`, `CF.orders` | subsumed: `Realizes` covers every legal order | `place_ok` |

**Generator encoding (`Run.Eff`).**

- **Not needed:** `Run.Eff` and round 4's `Gen.serial` stack, the views and
  `@<step>` inputs, and `CF.orders`.
- **Needed:**
  - per step, the site list;
  - per step, the footprint as `(child, port)` entries, which is the site
    list's ports (`callsOf_ports`);
  - per module, the netlist's fan-in for `Acyc`.

  All three come from fields `Step` already has, together with `aState`'s
  dependency information.

## 9. All orders, and the scan behind the certificate (integration review 2, M2)

The parent has no state of its own, and each child's contract covers every
legal order of its calls. `place_ok` and `obl_of_cert` turn the per-pair
fields into `Obl`, once.

**Exceptions are at-most-one groups.** bsc's disjoint guards, urgency
blocking and `mutually_exclusive` are cliques of steps that never fire
together, as today's `disjointChains` finds them. The generator emits:

- per group, one `amoChain` over the steps' WILL_FIRE (or Avail for
  methods), as a `skel` conjunct under `Env`;
- per step, its group mask.

`h4_of_static` (proved) is the lemma that consumes them:

```lean
theorem h4_of_static (rules fire fp okP) (hok : okP implies Follow) (hfp : traces lie in fp)
    (grp : P.PR → P.PR → Bool)
    (hgrp : ∀ oc w r1 r2, Env P oc w → grp r1 r2 = true → fire oc w r1 = true → fire oc w r2 = true → r1 = r2)
    (hscan : ∀ r1 r2, r1 ≠ r2 → grp r1 r2 = false → ¬ [r2, r1] <+ rules →
      ∀ j a b, ⟨j, a⟩ ∈ fp r1 → ⟨j, b⟩ ∈ fp r2 → okP j a b = true) :
    the `h4` field of `Cert`
```

`okT_of_static` is the key step for every pair field: a static table on
footprints that implies `Follow` gives the dynamic `OkT`.

**The scan, specified with exceptions.** The inputs are `Tab`s:

- per step, its footprint;
- per child kind, `mayFollow[p]` and `mayGroup[p]` as bit masks;
- per method, `follow[m]`, `group[m]` and `sbrOrC[m]`;
- per step, `excl[s]`: the steps in a common exception group, as a bit set
  of steps.

The scan's state, per child instance `j`:

- `used_j[p]`: the bit set of **steps** (not ports) that used port `p` so
  far;
- `reach_j[p]`: the methods whose calls must precede the next user of `p`.

The pass:

1. **`grp`, `mm`, `mmg`:** as in revision 2.
2. **`h4`:** at rule `r`, for each port `p` it uses on `j`, the conflicting
   earlier steps are the OR of `used_j[q]` over the ports `q` with
   `¬mayFollow[q][p]`. After removing `excl[r]`, none may remain. Then `r`
   adds itself to `used_j[p]`. This is `hscan`, linear in uses × ⌈steps/64⌉.
3. **`hit`, `h3`:** as in revision 2. A rule's `hit` ORs only non-excluded
   users.

`cert_of_scan` is the soundness of this pass. It is stated in this
document, and `h4_of_static` is proved; the scan's proof that it decides
`hscan` and the `h1`–`h3` analogues is open issue 1. Until it lands, the pair
fields are case splits: `h4` alone has R² cases, which is 624,100 for
sysFloatTest. So **the modular path covers only small modules until step 3
of the PR sequence (§17)**, and today's flat theorem stays the default.

**Counts** (revision 2's `portscan.py`, exceptions as pairs; integration
review 2 re-ran it without exceptions, which gave about 40,000 `h4`
failures). With exceptions the static scan fails only 30 `h4` pairs in 5
designs. These become dynamic facts on the traces, which carry the use
conditions and assertions.

## 10. How the files compose (tested on the toy)

- **Per module, three files:** `<Mod>Iface.lean`, `<Mod>.lean` (imports the
  library and the children's interface files only) and `<Mod>Realizes.lean`
  (imports its own obligation and the children's `Realizes` files). They
  form a DAG, linear in size, and one schedlean run per module writes all
  three.
- **BVI interfaces** are keyed by the Verilog module name plus a hash of the
  `VModInfo` (ports, schedule, paths), as `<vName>_<hash>Iface`: the
  generator knows a child only as `vName vmi`, and two packages may import
  one Verilog module with different schedules (integration minor 3).
- **The build** is `lean -o` with `LEAN_PATH` in dependency order, every run
  with `-DElab.async=false`.
- **The final verdict** is `leanchecker --fresh <Top>Realizes`, mandatory.

| file (`split/`) | imports | wall | RSS |
|---|---|---|---|
| `ExLib` | Modular | 0.32 s | 0.51 GB |
| `ExChildIface` | Modular | 0.22 s | 0.50 GB |
| `ExChild` (obligation, `acyc`) | ExLib, ExChildIface | 1.88 s | 0.54 GB |
| `ExChildRealizes` | ExChild | 0.19 s | 0.50 GB |
| `ExTop` (obligation, `acyc`) | ExLib, ExChildIface (not ExChild) | 2.02 s | 0.55 GB |
| `ExTopRealizes` | ExTop, ExChildRealizes | 0.20 s | 0.50 GB |
| `leanchecker --fresh ExTopRealizes` | — | 44.3 s | 0.54 GB |

## 11. Cost

- **Indices.** Children, rules and ports are `Fin n` or inductives;
  `kids := List.finRange n`. `Scale.lean` (4096 registers and rules,
  including its `Acyc` for every `n` and `top4096`) checks in 0.5 s.
- **Facts.** `en`, `arg` and `alw` range over each port's users; `rdy*` and
  `enV` over each step's trace; their total is the total uses.
- **`bv_decide`.** One per module (`skel`), with the chains as hypotheses.
- **`Acyc`.**
  - The read lists are the netlist's fan-in: linear.
  - `dep` is one `simp` per child input and parent output.
  - `rk_ok` and `reach_*` are decided per path edge. Paths are few: CReg n
    has n(n-1)/2, a wire 2.
- **Conditional sites** (integration review 2, M1). One rule with `2k`
  conditional sites: a conditional read of `a_i` and a conditional write of
  `r_i` whose condition and data use the value read.
  - `probe/Sites<k>` is the site-list form. Each per-port fact is a
    separate lemma about its own site; only selecting that site (a `rfl` on
    `sites.filter (onPort j cm)`) still scans the list.
  - `probe/Cond<k>` is the nested-`callIf` form.

  | k (sites) | Sites: wall, RSS | Cond: wall, RSS |
  |---|---|---|
  | 4 (9) | 1.3 s, 0.55 GB | 1.6 s, 0.54 GB |
  | 16 (33) | 4.8 s, 0.67 GB | 13.8 s, 0.68 GB |
  | 32 (65) | 12.6 s, 0.83 GB | 58.3 s, 1.13 GB |
  | 64 (129) | 33.8 s, 1.21 GB | — |
  | 128 (257) | 118.8 s, 2.00 GB | — |

  - **Comparison.** The site-list form is 4.6× faster at k = 32. Its
    growth per doubling is k^1.4 (32→64) to k^1.8 (64→128), against k^2.1
    for the nested form (16→32), whose per-port `simp` walks the whole
    trace.
  - **Not yet linear.**
    - What remains is per-port list scans: each per-port `rfl` evaluates
      `sites.filter (onPort j cm)` over all `2k+1` sites.
    - `envHw_at` looks up `sites[i]` in O(i).
    - The `match` dispatch is over k literals.
  - **The fix,** for the generator PR:
    - index the site list by a `Tab` (balanced lookups);
    - check a per-port site-index table once against it, by `decide` on the
      sites' static fields;
    - dispatch through the `Tab`.

    Open issue 3.
  - **The corpus:** steps with 16 or more conditional sites occur in eight
    designs. The largest are sysConflictFreeOKLarge (1,024 sites), mkAes
    (136) and sysTrafficBRAM (133).
- **Scan.** §9; linear in uses × ⌈steps/64⌉ once `cert_of_scan` exists.

## 12. Primitives

| primitive | interface | theorems |
|---|---|---|
| `RegN.v` (`mkReg`); `RegUN.v`, `RegA.v` map here (same behaviour out of reset) | read CF read, read SB write, write SBR write | `reg_realizes`, `creg_causal 1` |
| `CRegN5.v` (`mkCReg`); `CRegUN5.v`, `CRegA5.v` map here | ports SBR across; paths `wr k → rd j`, `k < j` | `creg_realizes n`, `creg_causal n` |
| `RWire.v`: `vMkRWire`, `vMkRWireSBR`, `vMkUnsafeRWire`, `vMkUnsafeRWireSBR` | wset, wget, whas; paths `wset → wget, whas` | `rwire_realizes`, `rwire_causal` |
| `RWire0.v`: `vMkRWire0` (`mkPulseWire`), `vMkRWire0SBR`, Unsafe variants | wset, whas; path `wset → whas` | `rwire0_realizes`, `rwire0_causal` |
| `BypassWire.v`: `vMkBypassWire` | wset (always enabled) SBR wget; path | `bypass_realizes`, `bypass_causal` |
| `BypassWire.v`: `vMkUnsafeBypassWire` | wset SB wget (soundness minor 5) | `ubypass_realizes`, `ubypass_causal` |
| `BypassWire0.v`: `vMkBypassWire0` | wset (always enabled, no data) | `bypass0_realizes`, `bypass0_causal` |
| `FIFO2.v`: `vMk2FIFOF` | 8 ports, bsc's schedule (`fifoAnn_sched`), aliases (§7), no paths | `fifo2_realizes`, `fifo2_causal`, `fifo2_aliased` |
| `FIFO20.v`: `vMk2FIFOF0` | the same without `first` | `fifo20_realizes`, `fifo20_causal`, `fifo20_aliased` |

**FIFO2's `i_` ports read the cycle's start** (soundness minor 4). Their
atomic reads return the start-of-cycle flags, the semantics bsc's
`i_… CF enq/deq` annotation implies. It is not BSV's literal
one-rule-at-a-time reading of an explicit `i_notFull` call after an `enq`.
ConfigReg will need the same. The README's "What the file proves" should
gain this sentence (the README is outside this directory, so it is not
edited here): *"A read of a FIFO's `i_notEmpty` or `i_notFull`, and of a
`ConfigReg`, returns its value at the start of the cycle, whatever runs
before it in the cycle; bsc schedules these reads conflict-free with the
FIFO's actions on that basis."*

**Not covered yet:** unguarded FIFOs, FIFOL1, SizedFIFO, SizedFIFO0,
RegFile, the BRAMs, ConfigRegN, Counter and Probe. Until proved, each is a
leaf with an assumed contract and causality.

## 13. bsc features and their cases

| feature | treatment |
|---|---|
| port copies (`mtCopies`) | one port per copy; `share` within a copy |
| argument-less value methods | one port, read any number of times; dummy argument `ops.z` |
| split methods (`dMethodSteps`) | one port; the site list branches on the split conditions |
| always_enabled | `hasEn = false`, `alwaysEn = true`, **and `alwaysRdy = true`** (bsc proves always_enabled implies always_ready); `Cert.alw`; `NoAlw.not_obl`, `AlwEnq.not_obl` |
| always_ready | `alwaysRdy = true`; the module proves its guard holds anyway; `AlwRdy.not_obl` |
| `enabled_when_ready` | bsc sets the method's enable to its RDY (`AAddScheduleDefs.handleEnableWhenReady`); `comb` reads `EN := RDY`; fails safe in `Obl` if the generator omits it (soundness minor 6) |
| `-unsafe-always-ready` | bsc's always_ready proof becomes a warning, and always_enabled then implies `EN := RDY`; the modular proof still requires the guard, so a design relying on the flag fails safe |
| action-value methods | `ret` compared like a value method's; C with themselves (a BVI one that is not must be rejected) |
| two C methods sharing a child port | `Env`'s exported-C part; `Asm`'s `put1`/`put2` |
| parallel (AND-OR) muxes | `Parent.onehot`, `Cert.onehot` from `EnvC` (§5) |
| user assertions, run-time checks | `Mod.asm`, visible as `Mo.ok` in `Realizes` |
| solver verdicts | `Hyps ops`, also in the consistency check |
| constants | `EqC`, EqLaw chains (§4) |
| use conditions | sites under their conditions; skipped values `u`; the contextual rewrite (§7) |
| shared output wires | `Ann.same`, `Ann.rdyRet`, `Aliased` (§7) |
| wires, CRegs | library contracts with paths; `Acyc` ranks the loops away (§5) |
| combinational methods of generated modules | `Ann.path` from bsc's path analysis, checked by the parent's own `Acyc` (`path_w`, `path_o`) |
| action shadowing | not yet (open issue 4) |
| BVI | `Built.leaf` with an assumed contract and causality; keyed by Verilog name and `VModInfo` hash |
| system tasks | not modelled; their reads are sites under the reach conditions |
| reset | cycles out of reset; FIFO2's CLR is modelled |
| multiple clocks | per domain, with other domains' ports as free wires; not done |

## 14. The example

Toy hierarchy, proved for every `V` and `ToyOps V`:
`mkTop ── c : mkChild ── a, b : Reg` and `mkTop ── t : Reg`.

- **`mkChild`** exports `getA SB setA`, `getB SBR setA` (`move` lies between
  them), `getA CF getB` and `setA C setA`. It has `Child.obl`,
  `Child.acyc` and `Child.built`.
- **`mkTop`** has the rules `rr` and `rw` and the value methods `get` and
  `peek`. It has `Top.obl`, `Top.acyc`, `Top.built`, `Top.realizes` and
  `Top.top`.
- **`Asm`** has an assertion, two AND-OR muxes, C methods sharing
  `q.enq`, and one `bv_decide` (`skel`, from `EnvC`). Its theorems are
  `Asm.top` and `Asm.ok_iff` (the excluded cycles, in words).
- **`WireEx`** has a child with a combinational path, ranked (§5):
  `WireEx.top`.
- **`AliasEx`** has FIFO aliases (§7): `AliasEx.obl`, `AliasEx.built`.
- **`Fsm`** has constants with `EqC` and the chain (§4): `Fsm.skel`,
  `Fsm.obl`, `Fsm.chain_consistent`.

Negative cases, each a theorem that the obligation, the contract or the
certificate is false:

| case | what fails | theorems |
|---|---|---|
| (a) `ChildSB` | `getB SB setA`: `move` sandwiched | `not_obl`, `no_cert`, `not_realizes` |
| (b) `PutBad` | the mux drives the wrong caller's argument | `not_obl` |
| (c) `Order` | `peek CF setP` against `getA SB setA` | `not_obl` |
| (d) `NoRdy` | a CAN_FIRE without the child FIFO's RDY | `not_obl` |
| (e) `ShareArg` | two callers, one copy, different arguments | `not_obl` |
| (f) `NoAlw` | a BypassWire's `wset` not called every cycle | `not_obl` |
| (g) `AsmNo` | `Asm` without its assertion | `not_obl` |
| (h) `Loop` | a loop through an RWire: `Obl` holds, no cycle exists | `obl`, `hw_empty`, `no_acyc` |
| (i) `AlwEnq` | an always_enabled method enqueueing into a FIFO that may be full | `not_obl` |
| (j) `AlwRdy` | an always_ready method whose guard can fail | `not_obl` |
| `FsmNo` | FSM states compared without the law | `not_obl` |

## 15. Emission trust points (soundness minor 8)

No Lean check sees these three; the generator must get them right, as
today's must for `Hw.*`, `Hw.oneHot` and `Assertions`:

1. **`comb`'s fidelity to bsc's netlist** (`aState`): enables, muxes, RDY,
   outputs. A wrong `comb` proves the wrong hardware.
2. **Completeness of `onehot`:** every AND-OR mux of `comb` that is read as
   a priority mux must list its selectors. A missing mux is unsound, not
   fail-safe.
3. **`Mod.asm`'s content:** only bsc's assertion list. A non-assertion fact
   put there silently excludes cycles.

Two further facts are checked but rest on the generator's input:

- **The `Acyc` read lists** are checked by `dep` (a wrong list fails).
- **A BVI child's `path` table** is part of its assumed contract.

## 16. Open issues

1. **`cert_of_scan`.** Specified in §9, with exceptions as groups.
   `h4_of_static` and `okT_of_static` are proved; the scan's own soundness
   is not. Until then the pair fields are case splits, quadratic in a
   module's steps.
2. **Comparisons between values, and computed operands** (§4). Computed
   operands use the same `EqC` chains but are untested on the corpus.
   `eqv` with its laws is designed but not tested.
3. **Linear site lists** (§11). The site-list form is 4.6× faster than the
   nested form at k = 32 but grows as about k^1.8, from list scans. `Tab`
   indexing is the plan.
4. **Action shadowing.** Order such callers by mux priority in
   `placeOrder`'s rank, and state `arg` for the last caller. UrgencyBlocking
   and MethodRelations need it.
5. **Heterogeneous cost evidence** (integration minor 1). Still missing: a
   per-kind `portIdx`, a decided-table-to-`Cert` lemma, and a 256–1024-step
   module built from a real dump.
6. **More primitives** (§12), **unguarded FIFOs**, **RST**.
7. **The generator.** Everything in §7 is a plan; no emitted file exists
   yet.
8. **Multi-cycle.** `clean_idem` is in place; the trace theorem by induction
   on cycles is not written.

## 17. Review items (round 3) and where they are fixed

| review item | fix | evidence |
|---|---|---|
| S major: vacuity | `Ann.path`, `Causal`, `Acyc`, `causal_toModule`, `Built.leaf/node` carry it, `Built.causal`, `Built.top`; every primitive `_causal`; BVI causality an explicit leaf argument | `Ex.Loop.no_acyc`, `Ex.WireEx.top`, `Scale.top4096` |
| S minor 1: assertions hidden | `hw` without `asm`; `Mo.ok` in `Realizes`; `Mod.asm` named | `Asm.ok_iff` |
| S minor 2: consistency check | `fail_if_success` over `asm`, instantiated `Hyps`, chains (pattern shown); discharging `Hyps` on `BitVec` where feasible is planned | `Fsm.chain_consistent` |
| S minor 3: Booleans exact | `Cert.onehot` from `EnvC` (done); the generator's fan-in check (§5) is planned | — |
| S minor 4: FIFO2 `i_` semantics; merged `i_notEmpty` | README sentence (§12); `rdy first && rdy deq` (§7) | — |
| S minor 5: `vMkUnsafeBypassWire`; reset variants | `UnsafeBypassWire`, `ubypass_*`; mapping (§12) | — |
| S minor 6: `enabled_when_ready`, `-unsafe-always-ready`, `alwaysRdy` for always_enabled | §13 | `AlwEnq` |
| S minor 7: `clean_idem` | `Module.clean_idem` | all modules |
| S minor 8: trust points | §15 | — |
| S minor 9: negatives | `AlwEnq`, `AlwRdy`, `Loop` | (h)–(j) |
| I blocker B1: constants | `EqC`, `amoChain_of_law`; `Fsm`, `FsmNo`; six corpus designs pass with chains, view-form included | §4 table |
| I major M1: conditional sites | `Prog.callIf`, `trace_callIf`, `Seq`, `NoFail`, `calls_iff_any`; first-order `Site`/`runSites` with `trace_runSites`, `envHw_at`, `calls_sites`; contextual rewrite | `probe/Sites*`, `probe/Cond*`, §11 |
| I major M2: scan exceptions | groups as `amoChain`s; per-port step bit sets; `h4_of_static`; modular path for small modules until `cert_of_scan` | §9 |
| I major M3: aliases | `Ann.same`, `Ann.rdyRet`, `AliasOK`, `Aliased`, `Obl`/`Env` hypotheses, `compose` premise, `fifo2_aliased`, `fifo20_aliased` | `Ex.AliasEx` |
| I minor 1: heterogeneous cost | partly (`Sites` probe); open issue 5 | — |
| I minor 2–4: parameters, BVI keys, tuples | `Mod.Env` record (§6); Verilog name and `VModInfo` hash (§10); `ops.tup` (§7) | — |
| I minor 5: parameter lists | the site list's read environment `ρ` | `probe/Sites*` |
| I minor 6: shadowing | open issue 4 | — |
| I minor 7: weaker check | as S minor 2 | — |
| I minor 8: large `Ops` | split records as `Inputs` is split today | — |

**Upstream PR sequence.**

1. The library: `Modular.lean`, split into core (model, compose, `Acyc`),
   primitives, certificate, and emission support (sites, `EqC`).
2. Emission of interface and obligation files for **small modules only**:
   site lists, `skel` with chains, case-split pair fields, `Acyc` from the
   netlist.
3. `cert_of_scan` with exception groups, and `Tab`-indexed site lists.
   After this the modular path can take every module.
4. `<Mod>Realizes.lean`, the `Mod.Env` records and the `leanchecker` step in
   the driver.
5. More primitives.

Today's flat theorem stays the default until step 3 lands.
