/-
Modular.lean: the modular schedule-soundness theory, proved once.

A module is known to its parent by its interface: its method ports and the
scheduling annotations it exports (CF, SB, SBR, C).  This file defines

* the module model (`Module`): atomic one-at-a-time semantics of methods and
  rules, a cycle-end function (wires forget), the hardware of one clock
  cycle as a relation driven by the wires of the methods' ports, with the
  cycle's internal signals, and the assertions (`ok`) on those signals;
* `Legal`: a parent's call sequence in one cycle (one list of calls per
  parent rule, in order) obeys the exported annotations;
* `Drives`: the method wires (enables, argument buses) are that sequence's
  projection;
* `Realizes`: the module contract.  For every legal call sequence driven on
  its ports, the hardware's outputs and next state are those of a
  one-at-a-time run that keeps the calls in order, keeps each caller's calls
  together and interleaves some of the module's own rules;
* `Parent`: a module built from child instances, whose rules and methods are
  programs calling child methods (`Prog`), whose hardware is the children's
  hardware plus combinational logic (`comb`), with data wires constrained
  only when its parallel muxes are one-hot (`onehot`), and whose assertions
  (`asm`) are part of `ok`, which `Realizes` assumes in so many words;
* `Obl`: the per-module obligation, stated on the children's *interfaces*
  only;
* skipped values: a generated step reads some sites only under a condition
  and otherwise continues with a skipped value.  `ParentU` gives each step
  its own (`u : Step → Nat → V`), `ModuleU` a module whose one-at-a-time
  steps take those of every module below, and `OblU`/`RealizesU` fix the
  order, the children's call sequences and the run *before* the skipped
  values, which the every-assignment argument needs; `oblU_of_cert` gives
  `OblU` from a certificate whose rules, firing and hits do not mention
  them (`CertU`);
* `compose`: children that realize their contracts plus a family that meets
  its obligation give a module that realizes its contract; `Built.realizes`:
  by induction over the instance tree, the top module realizes its contract;
* `Causal` (non-vacuity): a module's outputs are a function of its state and
  of the inputs on its declared combinational paths, and its hardware has a
  cycle for every input.  Proved for every primitive; `causal_toModule`
  gives it to a parent from its children's and a decided acyclicity
  certificate (`Acyc`), and `Built.causal` to the whole instance tree;
* output aliases (`Ann.same`, `Ann.rdyRet`, `Aliased`): ports that share one
  output wire, as FIFO2's `notFull`, `i_notFull` and `enq`'s RDY do; a
  generated module's ready output read as a port (`Parent.exactRet`,
  `aliased_toModuleU`);
* system tasks: a module's task statements are the ports of a `Tasks` child
  whose log the contract compares, per module, with the hardware's task
  calls (`Module.L`, `log`, `trc`; `tasks_realizes`); task calls inside
  interface methods are not statements (bsc G0020), so they are not compared;
* primitive contracts: `CReg n` (and `Reg = CReg 1`); the wire model and
  bsc's eleven wire imports, each with its exact ports and schedule
  (`Realizes.restrict`); `FIFO2` with `vMk2FIFOF`'s eight ports and schedule
  (checked against bsc's schedule statements), and `FIFO20`;
* `obl_of_cert`: the obligation from a per-cycle certificate of finite facts
  (per step, per child port over its users, and per pair of steps), with the
  parent-level all-orders placement of rules proved once (`place_ok`); `obl_of_certL` the same with
  last-caller ports (`CertL`: a self-SBR port carries the argument of the step last in `lp`);
* emission support: constants and their law (`EqC`, `amoChain_of_law`),
  conditional calls and reads (`Prog.callIf`, `trace_callIf`, `Within`,
  `calls_iff_any`), and footprint-level ordering facts (`okT_of_static`,
  `h4_of_static`).

Values are one abstract type `V`; a generated module takes its datapath
operators as parameters, so its theorems hold for every interpretation.

Only core Lean.
-/
set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false
open scoped List

namespace Modular

/-! ## Annotations, legality, wires -/

/-- What a module exports about its method ports.  For two ports `p q`:

* `prec p q`: p SB q or p SBR q, so no call of `p` may come after a call of
  `q` (only for `p ≠ q`);
* `sbr p q`: p SBR q, so no one caller may call both (for `p = q`: no caller
  calls `p` twice, as for a register's `write SBR write`);
* `conf p q`: p C q, so not both are called (for `p = q`: at most one call);
* `share m`: every call of `m` in a cycle carries the same argument (a value
  method, whose argument is one bus);
* `hasEn m`: the port has an enable wire;
* `alwaysEn m`: the method is always_enabled and must be called every cycle;
* `alwaysRdy m`: the method is always ready (always_ready, or a primitive's
  port with no ready signal), so a caller need not check its RDY;
* `same p q`: `p`'s and `q`'s results are one output wire;
* `rdyRet a p`: `a`'s RDY is the wire whose one-bit value `p` returns (FIFO2's
  `FULL_N` is `enq`'s RDY and the result of `notFull` and `i_notFull`);
* `path p q`: a combinational path from `p`'s inputs (enable, arguments) to
  `q`'s outputs (RDY, result), as bsc's `path` statements and `VModInfo`
  list them.

The last three default to none.  For a generated module they are checked
(`Acyc`); for an imported Verilog module they are part of its assumed
contract. -/
structure Ann (M : Type) where
  prec : M → M → Bool
  sbr : M → M → Bool
  conf : M → M → Bool
  share : M → Bool
  hasEn : M → Bool
  alwaysEn : M → Bool
  alwaysRdy : M → Bool
  same : M → M → Bool := fun _ _ => false
  rdyRet : M → M → Bool := fun _ _ => false
  path : M → M → Bool := fun _ _ => false

variable {V : Type}

/-- Call `c2` may come later in the cycle than call `c1`. -/
def Ann.Follow {M : Type} (A : Ann M) (c1 c2 : M × V) : Prop :=
  (c1.1 ≠ c2.1 → A.prec c2.1 c1.1 = false) ∧ A.conf c1.1 c2.1 = false ∧ A.conf c2.1 c1.1 = false ∧
  (c1.1 = c2.1 → A.share c1.1 = true → c1.2 = c2.2)

/-- One caller may make call `c1` and then call `c2`. -/
def Ann.Group {M : Type} (A : Ann M) (c1 c2 : M × V) : Prop :=
  A.Follow c1 c2 ∧ A.sbr c1.1 c2.1 = false ∧ A.sbr c2.1 c1.1 = false

/-- A cycle's call sequence `σ`, one list of calls per caller (parent rule or
    parent method), in order, obeys the annotations `A`. -/
structure Legal {M : Type} (A : Ann M) (σ : List (List (M × V))) : Prop where
  order : ∀ c1 c2, [c1, c2] <+ σ.flatten → A.Follow c1 c2
  group : ∀ g ∈ σ, ∀ c1 c2, [c1, c2] <+ g → A.Group c1 c2
  always : ∀ m, A.alwaysEn m = true → ∃ x, (m, x) ∈ σ.flatten

/-- The wires a parent drives into a module's ports in one cycle. -/
structure Wires (M V : Type) where
  en : M → Bool
  arg : M → V

/-- The wires a module drives out: ready signals and results. -/
structure Outs (M V : Type) where
  rdy : M → Bool
  ret : M → V

/-- Two input wire sets agree at port `p`. -/
def AgreeW {M : Type} (w w' : Wires M V) (p : M) : Prop := w.en p = w'.en p ∧ w.arg p = w'.arg p

/-- Two output wire sets agree at port `q`. -/
def AgreeO {M : Type} (o o' : Outs M V) (q : M) : Prop := o.rdy q = o'.rdy q ∧ o.ret q = o'.ret q

theorem Outs.ext' {M : Type} {o o' : Outs M V} (h : ∀ q, AgreeO o o' q) : o = o' := by
  cases o; cases o'
  congr
  · funext q; exact (h q).1
  · funext q; exact (h q).2

/-- The outputs respect the interface's aliases: results of one wire are
    equal, and an RDY that is a one-bit result's wire is its Bool reading
    (`toB`, the design's reading of a one-bit value). -/
def AliasOK {M : Type} (toB : V → Bool) (A : Ann M) (o : Outs M V) : Prop :=
  (∀ p q, A.same p q = true → o.ret p = o.ret q) ∧ (∀ a p, A.rdyRet a p = true → o.rdy a = toB (o.ret p))

/-- The argument of the last call of `m`. -/
def lastArg {M : Type} [DecidableEq M] (m : M) : List (M × V) → Option V
  | [] => none
  | c :: l => match lastArg m l with
    | some y => some y
    | none => if c.1 = m then some c.2 else none

/-- The wires are the projection of the call sequence: an enabled port is
    exactly a called one, and a called port's argument bus carries the last
    call's argument.  Uncalled ports' arguments, and the enables of ports
    without one, are unconstrained. -/
structure Drives {M : Type} [DecidableEq M] (A : Ann M) (w : Wires M V) (σ : List (List (M × V))) : Prop where
  en : ∀ m, A.hasEn m = true → (w.en m = true ↔ ∃ x, (m, x) ∈ σ.flatten)
  arg : ∀ m x, lastArg m σ.flatten = some x → w.arg m = x

/-! ## Modules and the contract -/

/-- A module's interface: its method ports and exported annotations. -/
structure Iface where
  M : Type
  decM : DecidableEq M
  ann : Ann M

attribute [instance] Iface.decM

/-- A system-task call (`$display`, `$write`, `$finish`, ...): its statement's
    index in its module's Verilog, and its arguments (one tuple value). -/
abbrev TaskCall (V : Type) := Nat × V

/-- A module implementing interface `X`. -/
structure Module (V : Type) (X : Iface) where
  /-- State, as the one-at-a-time semantics sees it. -/
  S : Type
  /-- Rules (for a built module: its own and all its descendants'). -/
  R : Type
  /-- `meth m x s v s'`: calling `m` with argument `x` in state `s` may return
      `v` and leave `s'`; no such `v s'` when `m` is not enabled. -/
  meth : X.M → V → S → V → S → Prop
  /-- `rule r s s'`: rule `r` may fire in `s` and leave `s'`. -/
  rule : R → S → S → Prop
  /-- The end of a cycle (a wire forgets its value). -/
  clean : S → S
  clean_idem : ∀ s, clean (clean s) = clean s
  /-- The internal signals of a cycle (for a built module: its input wires,
      its children's wires and theirs, recursively). -/
  Sig : Type
  /-- One clock cycle of the hardware, as the Verilog has it: from state `s`,
      with input wires `w`, the outputs may be `o`, the next state `s'` and
      the internal signals `g`. -/
  hw : S → Wires X.M V → Outs X.M V → S → Sig → Prop
  /-- The assertions (user `mutually_exclusive`/`conflict_free` and bsc's
      run-time checks) of the module and of every module below it hold on
      the cycle's signals. -/
  ok : Sig → Prop
  /-- The task logs of this module and the modules below it, one per module
      with system tasks (its `Tasks` child).  None by default. -/
  L : Type := Empty
  /-- The task calls a one-at-a-time run has made so far this cycle, per log. -/
  log : S → L → List (TaskCall V) := fun _ _ => []
  /-- The task calls the hardware makes in a cycle, per log, in statement order. -/
  trc : Sig → L → List (TaskCall V) := fun _ _ => []
  /-- A cycle starts with empty logs. -/
  log_clean : ∀ s l, log (clean s) l = [] := by intros; rfl

section Exec
variable {X : Iface} (Mo : Module V X) (o : Outs X.M V)

/-- One caller's calls, one at a time, each returning the hardware's result. -/
inductive ExecG : Mo.S → List (X.M × V) → Mo.S → Prop
  | nil (s : Mo.S) : ExecG s [] s
  | cons {s s1 s2 : Mo.S} {m : X.M} {x : V} {l : List (X.M × V)} :
      Mo.meth m x s (o.ret m) s1 → ExecG s1 l s2 → ExecG s ((m, x) :: l) s2

/-- A one-at-a-time run: callers' groups of calls (`inl`) and rules (`inr`). -/
inductive Exec : Mo.S → List (List (X.M × V) ⊕ Mo.R) → Mo.S → Prop
  | nil (s : Mo.S) : Exec s [] s
  | grp {s s1 s2 : Mo.S} {g : List (X.M × V)} {τ : List (List (X.M × V) ⊕ Mo.R)} :
      ExecG Mo o s g s1 → Exec s1 τ s2 → Exec s (.inl g :: τ) s2
  | rule {s s1 s2 : Mo.S} {r : Mo.R} {τ : List (List (X.M × V) ⊕ Mo.R)} :
      Mo.rule r s s1 → Exec s1 τ s2 → Exec s (.inr r :: τ) s2

end Exec

/-- The callers' groups of a run, in order. -/
def groups {α β : Type} : List (α ⊕ β) → List α
  | [] => []
  | .inl a :: l => a :: groups l
  | .inr _ :: l => groups l

/-- **The module contract.**  From every state a cycle can start in, for every
    legal call sequence `σ` driven on the ports (`Drives`) whose calls the
    hardware says are ready, and every hardware cycle with those wires in
    which the assertions hold (`Mo.ok g`), some one-at-a-time run `τ` with
    `groups τ = σ` (the calls in order, each caller's calls together, the
    module's rules between callers only) has every step enabled, every call
    returning the hardware's result, and ends in a state whose cycle end is
    the hardware's next state.  For each module with system tasks (each
    log `l`), the hardware's task calls (enabled statements, arguments,
    statement order) are that module's calls in the run, in run order.

    Different modules' task calls are not ordered against each other: in
    Verilog each synthesized module's tasks run in its own always block, and
    blocks on one clock edge run in an unspecified order, so output and
    shared-state tasks (an unseeded `$random`) may interleave differently.
    Bluesim should follow the full one-at-a-time order; that is a property
    of the Bluesim back end, outside this proof.

    Task calls inside interface methods (bsc warning G0020) are not `Tasks` statements in
    generated modules, so this agreement does not cover them; task calls in rules it does. -/
def Realizes {X : Iface} (Mo : Module V X) : Prop :=
  ∀ (s : Mo.S) (σ : List (List (X.M × V))) (w : Wires X.M V) (o : Outs X.M V) (s' : Mo.S) (g : Mo.Sig),
    Mo.clean s = s → Legal X.ann σ → Drives X.ann w σ → Mo.hw s w o s' g → Mo.ok g →
    (∀ c ∈ σ.flatten, X.ann.alwaysRdy c.1 = false → o.rdy c.1 = true) →
    ∃ (τ : List (List (X.M × V) ⊕ Mo.R)) (fin : Mo.S), groups τ = σ ∧ Exec Mo o s τ fin ∧ s' = Mo.clean fin ∧
      ∀ l, Mo.trc g l = Mo.log fin l

/-- **Causal totality** (non-vacuity): the outputs are a function `out` of
    the state and the inputs, depending on port `q`'s outputs only through
    the inputs of ports with a path to `q`; and from every state a cycle can
    start in, the hardware has a cycle for every input, with those outputs. -/
def Causal {X : Iface} (Mo : Module V X) : Prop :=
  ∃ out : Mo.S → Wires X.M V → Outs X.M V,
    (∀ s w w' q, (∀ p, X.ann.path p q = true → AgreeW w w' p) → AgreeO (out s w) (out s w') q) ∧
    (∀ s w, Mo.clean s = s → ∃ s' g, Mo.hw s w (out s w) s' g)

/-- Every hardware cycle respects the interface's output aliases. -/
def Aliased {X : Iface} (toB : V → Bool) (Mo : Module V X) : Prop :=
  ∀ s w o s' g, Mo.clean s = s → Mo.hw s w o s' g → AliasOK toB X.ann o

/-- A module whose interface declares no aliases respects them. -/
theorem aliased_of_none {X : Iface} (toB : V → Bool) (Mo : Module V X)
    (h1 : ∀ p q, X.ann.same p q = false) (h2 : ∀ a p, X.ann.rdyRet a p = false) : Aliased toB Mo :=
  fun _ _ _ _ _ _ _ => ⟨fun p q h => by { rw [h1] at h; cases h }, fun a p h => by { rw [h2] at h; cases h }⟩

/-- The same module, exporting annotations `A` instead. -/
def Module.reann {X : Iface} (Mo : Module V X) (A : Ann X.M) : Module V ⟨X.M, X.decM, A⟩ :=
  { S := Mo.S, R := Mo.R, meth := Mo.meth, rule := Mo.rule, clean := Mo.clean,
    clean_idem := Mo.clean_idem, Sig := Mo.Sig, hw := Mo.hw, ok := Mo.ok,
    L := Mo.L, log := Mo.log, trc := Mo.trc, log_clean := Mo.log_clean }

theorem execG_reann {X : Iface} (Mo : Module V X) (A : Ann X.M) (o : Outs X.M V) :
    ∀ (s t : Mo.S) (g : List (X.M × V)), ExecG Mo o s g t → ExecG (Mo.reann A) o s g t := by
  intro s t g h
  induction h with
  | nil s => exact ExecG.nil (Mo := Mo.reann A) (o := o) s
  | cons hm _ ih => exact ExecG.cons (Mo := Mo.reann A) hm ih

theorem exec_reann {X : Iface} (Mo : Module V X) (A : Ann X.M) (o : Outs X.M V) :
    ∀ (s t : Mo.S) (τ : List (List (X.M × V) ⊕ Mo.R)), Exec Mo o s τ t → Exec (Mo.reann A) o s τ t := by
  intro s t τ h
  induction h with
  | nil s => exact Exec.nil (Mo := Mo.reann A) (o := o) s
  | grp hg _ ih => exact Exec.grp (Mo := Mo.reann A) (execG_reann Mo A o _ _ _ hg) ih
  | rule hr _ ih => exact Exec.rule (Mo := Mo.reann A) hr ih

/-- A module with stricter annotations still realizes its contract. -/
theorem Realizes.mono {X : Iface} (Mo : Module V X) (A : Ann X.M)
    (hL : ∀ σ : List (List (X.M × V)), Legal A σ → Legal X.ann σ)
    (hD : ∀ (w : Wires X.M V) (σ : List (List (X.M × V))), Drives A w σ → Drives X.ann w σ)
    (hR : ∀ m, A.alwaysRdy m = X.ann.alwaysRdy m)
    (h : Realizes Mo) : Realizes (Mo.reann A) := by
  intro s σ w o s' g hc hl hd hh hok hr
  obtain ⟨τ, fin, h1, h2, h3, h4⟩ := h s σ w o s' g hc (hL σ hl) (hD w σ hd) hh hok
    (fun c hc' h' => hr c hc' ((hR c.1).trans h'))
  exact ⟨τ, fin, h1, exec_reann Mo A o s fin τ h2, h3, h4⟩

/-! ## Built modules -/

/-- What a parent's rule or method does, atomically: call child `j`'s port
    `m` with argument `x`, continue with its result; finish with a result;
    or fail (a guard is false). -/
inductive Prog (I : Type) (CM : I → Type) (V : Type) where
  | ret : V → Prog I CM V
  | fail : Prog I CM V
  | call (j : I) (m : CM j) (x : V) (k : V → Prog I CM V) : Prog I CM V

/-- **A conditional call or read** (a site under its condition): when `b`,
    call child `j`'s port `m` with `x` and continue with its result; else
    continue with `u`, the skipped site's value.  A generated step is a
    chain of these, so its trace is one term (`trace_callIf`) and its facts
    are linear in its sites, not exponential in its conditions. -/
def Prog.callIf {I : Type} {CM : I → Type} (b : Bool) (j : I) (m : CM j) (x u : V)
    (k : V → Prog I CM V) : Prog I CM V :=
  if b then .call j m x k else k u

/-- Update one component of a dependent function. -/
def upd {I : Type} [DecidableEq I] {S : I → Type} (s : (j : I) → S j) (j : I) (x : S j) :
    (j' : I) → S j' :=
  fun j' => if h : j' = j then h ▸ x else s j'

theorem upd_same {I : Type} [DecidableEq I] {S : I → Type} (s : (j : I) → S j) (j : I) (x : S j) :
    upd s j x j = x := by simp [upd]

theorem upd_ne {I : Type} [DecidableEq I] {S : I → Type} (s : (j : I) → S j) {j j' : I} (x : S j)
    (h : j' ≠ j) : upd s j x j' = s j' := by simp [upd, h]

theorem upd_upd {I : Type} [DecidableEq I] {S : I → Type} (s : (j : I) → S j) (j : I) (x y : S j) :
    upd (upd s j x) j y = upd s j y := by
  funext j'
  by_cases h : j' = j
  · subst h; simp [upd_same]
  · simp [upd_ne _ _ h]

/-- A module built from child instances.  The children are known here only
    by their interfaces `CI j`; `toModule` takes implementations. -/
structure Parent (V : Type) where
  I : Type
  decI : DecidableEq I
  kids : List I
  kids_nodup : kids.Nodup
  kids_all : ∀ j, j ∈ kids
  CI : I → Iface
  /-- The parent's own interface (methods and exported annotations). -/
  X : Iface
  /-- The parent's own rules. -/
  PR : Type
  body : X.M → V → Prog I (fun j => (CI j).M) V
  rbody : PR → Prog I (fun j => (CI j).M) V
  /-- The parent's combinational logic: from the children's outputs and the
      parent's input wires, the children's input wires and the parent's
      outputs (CAN_FIRE/WILL_FIRE, enables, argument muxes, results, RDY).
      A parallel (AND-OR) mux on data values is read here as a priority
      mux. -/
  comb : ((j : I) → Outs (CI j).M V) → Wires X.M V → ((j : I) → Wires (CI j).M V) × Outs X.M V
  /-- The per-cycle assumptions on this module's logic: the user's
      `mutually_exclusive` and `conflict_free` assertions and bsc's other
      run-time checks.  A cycle in which one fails is not covered (bsc
      reports it in simulation).  `True` when there are none. -/
  asm : ((j : I) → Outs (CI j).M V) → Wires X.M V → Prop
  /-- The one-hot conditions of `comb`'s parallel muxes on data values.
      Reading such a mux as a priority mux is exact only when they hold;
      `toModule` leaves the data wires unconstrained otherwise, so the
      obligation must prove them.  `True` when there are none. -/
  onehot : ((j : I) → Outs (CI j).M V) → Wires X.M V → Prop
  /-- The design's Bool reading of a one-bit value (for children's output
      aliases, `Ann.rdyRet`). -/
  toB : V → Bool := fun _ => false
  /-- Ports whose result is a wire `comb` computes without a data mux (a method's ready
      output `RDY_m`, read as a port), so the hardware drives it exactly. -/
  exactRet : X.M → Bool := fun _ => false

attribute [instance] Parent.decI

/-- A built module's internal signals: its input wires, its children's outputs and inputs,
    and each child's own signals.  It mentions interfaces only, not programs. -/
structure PSig (V : Type) (X : Iface) (I : Type) (CI : I → Iface) (Sg : I → Type) where
  w : Wires X.M V
  oc : (j : I) → Outs (CI j).M V
  wc : (j : I) → Wires (CI j).M V
  sub : (j : I) → Sg j

section Built
variable (P : Parent V) (impl : (j : P.I) → Module V (P.CI j))

/-- A program run atomically on the children's states. -/
inductive RunP : Prog P.I (fun j => (P.CI j).M) V → ((j : P.I) → (impl j).S) → V →
    ((j : P.I) → (impl j).S) → Prop
  | ret (v : V) (s : (j : P.I) → (impl j).S) : RunP (.ret v) s v s
  | call {j : P.I} {m : (P.CI j).M} {x : V} {k : V → Prog P.I (fun j => (P.CI j).M) V}
      {s : (j : P.I) → (impl j).S} {s1 : (impl j).S} {v r : V} {s2 : (j : P.I) → (impl j).S} :
      (impl j).meth m x (s j) v s1 → RunP (k v) (upd s j s1) r s2 → RunP (.call j m x k) s r s2

/-- The module a parent and its children's implementations make.  Its state
    is the children's states, its rules its own and its children's.

    Its hardware: the children's hardware, with the children's outputs `oc`
    and inputs `wc` connected through `comb`.  Boolean wires (enables, RDY)
    are exactly `comb`'s.  The data wires (child arguments, results) are
    `comb`'s when the parallel muxes' selectors are one-hot, and otherwise
    unconstrained: an AND-OR of several data values is not modelled.  Its
    assertions: its own `asm` on its signals, and its children's.  Its task
    logs: its children's, each kept apart (its own tasks are its `Tasks`
    child's). -/
@[reducible] def Parent.toModule : Module V P.X where
  S := (j : P.I) → (impl j).S
  R := P.PR ⊕ (Σ j, (impl j).R)
  meth m x s v s' := RunP P impl (P.body m x) s v s'
  rule r s s' := match r with
    | .inl r => ∃ v, RunP P impl (P.rbody r) s v s'
    | .inr ⟨j, r⟩ => ∃ t, (impl j).rule r (s j) t ∧ s' = upd s j t
  clean s := fun j => (impl j).clean (s j)
  clean_idem s := funext fun j => (impl j).clean_idem (s j)
  Sig := PSig V P.X P.I P.CI (fun j => (impl j).Sig)
  hw s w o s' g := g.w = w ∧ (∀ j, (impl j).hw (s j) (g.wc j) (g.oc j) (s' j) (g.sub j)) ∧
    (∀ j, (g.wc j).en = ((P.comb g.oc w).1 j).en) ∧ o.rdy = (P.comb g.oc w).2.rdy ∧
    (∀ m, P.exactRet m = true → o.ret m = (P.comb g.oc w).2.ret m) ∧
    (P.onehot g.oc w → g.wc = (P.comb g.oc w).1 ∧ o = (P.comb g.oc w).2)
  ok g := P.asm g.oc g.w ∧ ∀ j, (impl j).ok (g.sub j)
  L := Σ j, (impl j).L
  log s l := (impl l.1).log (s l.1) l.2
  trc g l := (impl l.1).trc (g.sub l.1) l.2
  log_clean s l := (impl l.1).log_clean (s l.1) l.2

end Built

/-! ## Skipped values, fixed after the run

A step reads some sites only under a condition and otherwise continues with a skipped
value.  The every-assignment argument (set each skipped value to what the real read
returns at its position) needs the run fixed before the skipped values, at every level.
`ParentU` gives each step its own skipped values; `ModuleU`'s steps take those of every
module below; `RealizesU` chooses the run before them. -/

/-- A module whose one-at-a-time steps take skipped values `u : U`; its hardware does not. -/
structure ModuleU (V : Type) (X : Iface) where
  U : Type
  S : Type
  R : Type
  meth : U → X.M → V → S → V → S → Prop
  rule : U → R → S → S → Prop
  clean : S → S
  clean_idem : ∀ s, clean (clean s) = clean s
  Sig : Type
  hw : S → Wires X.M V → Outs X.M V → S → Sig → Prop
  ok : Sig → Prop
  L : Type := Empty
  log : S → L → List (TaskCall V) := fun _ _ => []
  trc : Sig → L → List (TaskCall V) := fun _ _ => []
  log_clean : ∀ s l, log (clean s) l = [] := by intros; rfl

/-- The module at skipped values `u`. -/
@[reducible] def ModuleU.at {X : Iface} (Mo : ModuleU V X) (u : Mo.U) : Module V X where
  S := Mo.S
  R := Mo.R
  meth := Mo.meth u
  rule := Mo.rule u
  clean := Mo.clean
  clean_idem := Mo.clean_idem
  Sig := Mo.Sig
  hw := Mo.hw
  ok := Mo.ok
  L := Mo.L
  log := Mo.log
  trc := Mo.trc
  log_clean := Mo.log_clean

/-- The hardware alone, for `Causal` and `Aliased` (they read no one-at-a-time step). -/
@[reducible] def ModuleU.hwm {X : Iface} (Mo : ModuleU V X) : Module V X where
  S := Mo.S
  R := Empty
  meth _ _ _ _ _ := False
  rule r _ _ := r.elim
  clean := Mo.clean
  clean_idem := Mo.clean_idem
  Sig := Mo.Sig
  hw := Mo.hw
  ok := Mo.ok
  L := Mo.L
  log := Mo.log
  trc := Mo.trc
  log_clean := Mo.log_clean

/-- A module without skipped values (a primitive, a BVI leaf, a module off path A). -/
@[reducible] def Module.toU {X : Iface} (Mo : Module V X) : ModuleU V X where
  U := Unit
  S := Mo.S
  R := Mo.R
  meth _ := Mo.meth
  rule _ := Mo.rule
  clean := Mo.clean
  clean_idem := Mo.clean_idem
  Sig := Mo.Sig
  hw := Mo.hw
  ok := Mo.ok
  L := Mo.L
  log := Mo.log
  trc := Mo.trc
  log_clean := Mo.log_clean

/-- **The contract with skipped values**: the run `τ` is chosen before them, and at every
    assignment `u` it runs the module at `u` to the hardware's next state and task calls. -/
def RealizesU {X : Iface} (Mo : ModuleU V X) : Prop :=
  ∀ (s : Mo.S) (σ : List (List (X.M × V))) (w : Wires X.M V) (o : Outs X.M V) (s' : Mo.S) (g : Mo.Sig),
    Mo.clean s = s → Legal X.ann σ → Drives X.ann w σ → Mo.hw s w o s' g → Mo.ok g →
    (∀ c ∈ σ.flatten, X.ann.alwaysRdy c.1 = false → o.rdy c.1 = true) →
    ∃ τ : List (List (X.M × V) ⊕ Mo.R), groups τ = σ ∧ ∀ u : Mo.U, ∃ fin : Mo.S,
      Exec (Mo.at u) o s τ fin ∧ s' = Mo.clean fin ∧ ∀ l, Mo.trc g l = Mo.log fin l

theorem Module.toU_at {X : Iface} (Mo : Module V X) (u : Unit) : Mo.toU.at u = Mo := rfl

theorem realizesU_toU {X : Iface} (Mo : Module V X) (h : Realizes Mo) : RealizesU Mo.toU := by
  intro s σ w o s' g hc hl hd hh hok hr
  obtain ⟨τ, fin, h1, h2, h3, h4⟩ := h s σ w o s' g hc hl hd hh hok hr
  exact ⟨τ, h1, fun _ => ⟨fin, h2, h3, h4⟩⟩

/-- The run fixed before `u` is in particular a run at each `u`. -/
theorem RealizesU.at {X : Iface} {Mo : ModuleU V X} (h : RealizesU Mo) (u : Mo.U) : Realizes (Mo.at u) := by
  intro s σ w o s' g hc hl hd hh hok hr
  obtain ⟨τ, h1, h2⟩ := h s σ w o s' g hc hl hd hh hok hr
  obtain ⟨fin, h3, h4, h5⟩ := h2 u
  exact ⟨τ, fin, h1, h3, h4, h5⟩

theorem Causal.toU {X : Iface} {Mo : Module V X} (h : Causal Mo) : Causal Mo.toU.hwm := h

theorem Aliased.toU {X : Iface} {toB : V → Bool} {Mo : Module V X} (h : Aliased toB Mo) :
    Aliased toB Mo.toU.hwm := h

/-- A parent whose programs take their step's skipped values (`body m x v`, `rbody r v`). -/
structure ParentU (V : Type) where
  I : Type
  decI : DecidableEq I
  kids : List I
  kids_nodup : kids.Nodup
  kids_all : ∀ j, j ∈ kids
  CI : I → Iface
  X : Iface
  PR : Type
  body : X.M → V → (Nat → V) → Prog I (fun j => (CI j).M) V
  rbody : PR → (Nat → V) → Prog I (fun j => (CI j).M) V
  comb : ((j : I) → Outs (CI j).M V) → Wires X.M V → ((j : I) → Wires (CI j).M V) × Outs X.M V
  asm : ((j : I) → Outs (CI j).M V) → Wires X.M V → Prop
  onehot : ((j : I) → Outs (CI j).M V) → Wires X.M V → Prop
  toB : V → Bool := fun _ => false
  exactRet : X.M → Bool := fun _ => false

attribute [instance] ParentU.decI

/-- A family's steps; `u : F.Step → Nat → V` gives each step its own skipped values. -/
abbrev ParentU.Step (F : ParentU V) : Type := F.X.M ⊕ F.PR

/-- The parent at skipped values `u`: step `s` sees `u s` only, so no two steps share one. -/
@[reducible] def ParentU.at (F : ParentU V) (u : F.Step → Nat → V) : Parent V where
  I := F.I
  decI := F.decI
  kids := F.kids
  kids_nodup := F.kids_nodup
  kids_all := F.kids_all
  CI := F.CI
  X := F.X
  PR := F.PR
  body m x := F.body m x (u (.inl m))
  rbody r := F.rbody r (u (.inr r))
  comb := F.comb
  asm := F.asm
  onehot := F.onehot
  toB := F.toB
  exactRet := F.exactRet

/-- The part of a family that reads no program (for `Acyc`, `Env`, `Av`). -/
@[reducible] def ParentU.base (F : ParentU V) : Parent V where
  I := F.I
  decI := F.decI
  kids := F.kids
  kids_nodup := F.kids_nodup
  kids_all := F.kids_all
  CI := F.CI
  X := F.X
  PR := F.PR
  body _ _ := .fail
  rbody _ := .fail
  comb := F.comb
  asm := F.asm
  onehot := F.onehot
  toB := F.toB
  exactRet := F.exactRet

/-- A parent without skipped values, as a family. -/
@[reducible] def Parent.toU (P : Parent V) : ParentU V where
  I := P.I
  decI := P.decI
  kids := P.kids
  kids_nodup := P.kids_nodup
  kids_all := P.kids_all
  CI := P.CI
  X := P.X
  PR := P.PR
  body m x _ := P.body m x
  rbody r _ := P.rbody r
  comb := P.comb
  asm := P.asm
  onehot := P.onehot
  toB := P.toB
  exactRet := P.exactRet

theorem Parent.toU_at (P : Parent V) (u : P.toU.Step → Nat → V) : P.toU.at u = P := rfl

/-- The module a family builds; at `u` it is `(F.at u.1).toModule` of the children at `u.2`. -/
@[reducible] def ParentU.toModuleU (F : ParentU V) (impl : (j : F.I) → ModuleU V (F.CI j)) :
    ModuleU V F.X where
  U := (F.Step → Nat → V) × ((j : F.I) → (impl j).U)
  S := (j : F.I) → (impl j).S
  R := F.PR ⊕ (Σ j, (impl j).R)
  meth u := ((F.at u.1).toModule (fun j => (impl j).at (u.2 j))).meth
  rule u := ((F.at u.1).toModule (fun j => (impl j).at (u.2 j))).rule
  clean s := fun j => (impl j).clean (s j)
  clean_idem s := funext fun j => (impl j).clean_idem (s j)
  Sig := PSig V F.X F.I F.CI (fun j => (impl j).Sig)
  hw s w o s' g := g.w = w ∧ (∀ j, (impl j).hw (s j) (g.wc j) (g.oc j) (s' j) (g.sub j)) ∧
    (∀ j, (g.wc j).en = ((F.comb g.oc w).1 j).en) ∧ o.rdy = (F.comb g.oc w).2.rdy ∧
    (∀ m, F.exactRet m = true → o.ret m = (F.comb g.oc w).2.ret m) ∧
    (F.onehot g.oc w → g.wc = (F.comb g.oc w).1 ∧ o = (F.comb g.oc w).2)
  ok g := F.asm g.oc g.w ∧ ∀ j, (impl j).ok (g.sub j)
  L := Σ j, (impl j).L
  log s l := (impl l.1).log (s l.1) l.2
  trc g l := (impl l.1).trc (g.sub l.1) l.2
  log_clean s l := (impl l.1).log_clean (s l.1) l.2

theorem ParentU.toModuleU_at (F : ParentU V) (impl : (j : F.I) → ModuleU V (F.CI j))
    (u : (F.toModuleU impl).U) :
    (F.toModuleU impl).at u = (F.at u.1).toModule (fun j => (impl j).at (u.2 j)) := rfl

/-! ## The per-module obligation -/

section Obl

/-- A program unfolded with the hardware's results: its calls in order, and
    its result (`none`: a guard failed). -/
def trace {I : Type} {CM : I → Type} (oc : (j : I) → Outs (CM j) V) :
    Prog I CM V → List (Σ j, CM j × V) × Option V
  | .ret v => ([], some v)
  | .fail => ([], none)
  | .call j m x k => ((⟨j, (m, x)⟩ :: (trace oc (k ((oc j).ret m))).1), (trace oc (k ((oc j).ret m))).2)

/-- **The trace of a conditional site**: the call when `b`, then the rest,
    which sees the result when `b` and the skipped value otherwise.  One
    term for both cases, so a chain of `n` sites unfolds to `n` such terms. -/
theorem trace_callIf {I : Type} {CM : I → Type} (oc : (j : I) → Outs (CM j) V) (b : Bool) (j : I)
    (m : CM j) (x u : V) (k : V → Prog I CM V) :
    trace oc (Prog.callIf b j m x u k) =
      ((if b then [⟨j, (m, x)⟩] else []) ++ (trace oc (k (if b then (oc j).ret m else u))).1,
       (trace oc (k (if b then (oc j).ret m else u))).2) := by
  cases b <;> rfl

/-- The calls on child `j`. -/
def proj {I : Type} [DecidableEq I] {CM : I → Type} (j : I) : List (Σ j, CM j × V) → List (CM j × V)
  | [] => []
  | ⟨j', c⟩ :: l => if h : j' = j then (h ▸ c) :: proj j l else proj j l

theorem proj_nil {I : Type} {inst : DecidableEq I} {CM : I → Type} (j : I) :
    proj (V := V) (CM := CM) j [] = [] := rfl

theorem proj_cons_self {I : Type} {inst : DecidableEq I} {CM : I → Type} (j : I) (c : CM j × V)
    (l : List (Σ j, CM j × V)) : proj j (⟨j, c⟩ :: l) = c :: proj j l := by simp [proj]

theorem proj_cons_ne {I : Type} {inst : DecidableEq I} {CM : I → Type} {j j' : I} (c : CM j' × V)
    (l : List (Σ j, CM j × V)) (h : j' ≠ j) : proj j (⟨j', c⟩ :: l) = proj j l := by simp [proj, h]

theorem proj_append {I : Type} {inst : DecidableEq I} {CM : I → Type} (j : I) :
    ∀ (l1 l2 : List (Σ j, CM j × V)), proj j (l1 ++ l2) = proj j l1 ++ proj j l2
  | [], l2 => rfl
  | ⟨j', c⟩ :: l1, l2 => by
    by_cases h : j' = j
    · subst h; simp [proj_cons_self, proj_append j' l1 l2]
    · simp only [List.cons_append, proj_cons_ne _ _ h, proj_append j l1 l2]

theorem proj_flatMap {I : Type} {inst : DecidableEq I} {CM : I → Type} {α : Type} (j : I)
    (f : α → List (Σ j, CM j × V)) :
    ∀ (l : List α), proj j (l.flatMap f) = (l.map (fun a => proj j (f a))).flatten
  | [] => rfl
  | a :: l => by simp [List.flatMap_cons, proj_append, proj_flatMap j f l]

variable (P : Parent V)

/-- The child calls of a parent step: a caller's group of parent method
    calls (`inl`), or a parent rule (`inr`). -/
def stepTrace (oc : (j : P.I) → Outs (P.CI j).M V) : List (P.X.M × V) ⊕ P.PR →
    List (Σ j, (P.CI j).M × V)
  | .inl g => g.flatMap (fun c => (trace oc (P.body c.1 c.2)).1)
  | .inr r => (trace oc (P.rbody r)).1

/-- Child `j`'s call sequence induced by a run of parent steps: one group per
    parent step (a parent step's calls on `j` are one caller's). -/
def sigmaJ (oc : (j : P.I) → Outs (P.CI j).M V) (τP : List (List (P.X.M × V) ⊕ P.PR)) (j : P.I) :
    List (List ((P.CI j).M × V)) :=
  τP.map (fun e => proj j (stepTrace P oc e))

/-- A parent step, unfolded with the hardware's results, keeps its guards and
    returns the parent's hardware results. -/
def StepOK (oc : (j : P.I) → Outs (P.CI j).M V) (o : Outs P.X.M V) :
    List (P.X.M × V) ⊕ P.PR → Prop
  | .inl g => ∀ c ∈ g, (trace oc (P.body c.1 c.2)).2 = some (o.ret c.1)
  | .inr r => (trace oc (P.rbody r)).2.isSome = true

/-- **The per-module obligation**, on the children's interfaces only.  For
    every value of the children's outputs that respects their output
    aliases, and of the parent's input wires, that satisfies the module's
    assertions, and every legal call sequence on the parent's ports that
    those wires drive and that the parent's hardware says is ready: the
    parallel muxes' selectors are one-hot, and some order `τP` of the
    callers' groups and parent rules, unfolded with the hardware's values,
    keeps every guard and returns the parent's outputs, and induces on each
    child a call sequence that is legal for the child's annotations, is
    driven by the parent's enables and argument muxes, and calls only ports
    the child says are ready. -/
def Obl : Prop :=
  ∀ (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) (σ : List (List (P.X.M × V))),
    P.asm oc w → (∀ j, AliasOK P.toB (P.CI j).ann (oc j)) → Legal P.X.ann σ → Drives P.X.ann w σ →
    (∀ c ∈ σ.flatten, P.X.ann.alwaysRdy c.1 = false → (P.comb oc w).2.rdy c.1 = true) →
    P.onehot oc w ∧ ∃ τP : List (List (P.X.M × V) ⊕ P.PR), groups τP = σ ∧
      (∀ e ∈ τP, StepOK P oc (P.comb oc w).2 e) ∧
      ∀ j, Legal (P.CI j).ann (sigmaJ P oc τP j) ∧
        Drives (P.CI j).ann ((P.comb oc w).1 j) (sigmaJ P oc τP j) ∧
        ∀ c ∈ (sigmaJ P oc τP j).flatten, (P.CI j).ann.alwaysRdy c.1 = false → (oc j).rdy c.1 = true

/-- **The obligation with skipped values**: the order `τP` and every child's call sequence
    `σC j` are chosen before `u`; at every `u` the steps keep their guards, return the
    parent's outputs and make exactly those calls. -/
def OblU (F : ParentU V) : Prop :=
  ∀ (oc : (j : F.I) → Outs (F.CI j).M V) (w : Wires F.X.M V) (σ : List (List (F.X.M × V))),
    F.asm oc w → (∀ j, AliasOK F.toB (F.CI j).ann (oc j)) → Legal F.X.ann σ → Drives F.X.ann w σ →
    (∀ c ∈ σ.flatten, F.X.ann.alwaysRdy c.1 = false → (F.comb oc w).2.rdy c.1 = true) →
    F.onehot oc w ∧ ∃ (τP : List (List (F.X.M × V) ⊕ F.PR)) (σC : (j : F.I) → List (List ((F.CI j).M × V))),
      groups τP = σ ∧
      (∀ j, Legal (F.CI j).ann (σC j) ∧ Drives (F.CI j).ann ((F.comb oc w).1 j) (σC j) ∧
        ∀ c ∈ (σC j).flatten, (F.CI j).ann.alwaysRdy c.1 = false → (oc j).rdy c.1 = true) ∧
      ∀ u : F.Step → Nat → V, (∀ e ∈ τP, StepOK (F.at u) oc (F.comb oc w).2 e) ∧
        ∀ j, sigmaJ (F.at u) oc τP j = σC j

/-- A parent without skipped values meets the family obligation when it meets its own. -/
theorem oblU_of_obl (P : Parent V) (h : Obl P) : OblU P.toU := by
  intro oc w σ h1 h2 h3 h4 h5
  obtain ⟨hoh, τP, hg, hs, hj⟩ := h oc w σ h1 h2 h3 h4 h5
  exact ⟨hoh, τP, fun j => sigmaJ P oc τP j, hg, hj, fun _ => ⟨hs, fun _ => rfl⟩⟩

end Obl

/-! ## The composition theorem -/

section Compose
variable {X : Iface} (Mo : Module V X) (o : Outs X.M V)

theorem execG_append : ∀ {s t : Mo.S} (l1 l2 : List (X.M × V)),
    ExecG Mo o s (l1 ++ l2) t → ∃ u, ExecG Mo o s l1 u ∧ ExecG Mo o u l2 t
  | s, t, [], l2, h => ⟨s, .nil s, h⟩
  | s, t, (m, x) :: l1, l2, h => by
    cases h with
    | cons h1 h2 =>
      obtain ⟨u, hu1, hu2⟩ := execG_append l1 l2 h2
      exact ⟨u, .cons h1 hu1, hu2⟩

theorem execG_append' : ∀ {s u t : Mo.S} (l1 l2 : List (X.M × V)),
    ExecG Mo o s l1 u → ExecG Mo o u l2 t → ExecG Mo o s (l1 ++ l2) t
  | s, u, t, [], l2, h1, h2 => by cases h1; exact h2
  | s, u, t, (m, x) :: l1, l2, h1, h2 => by
    cases h1 with
    | cons a b => exact .cons a (execG_append' l1 l2 b h2)

theorem execG_nil_eq {s t : Mo.S} (h : ExecG Mo o s [] t) : s = t := by cases h; rfl

theorem exec_append : ∀ {s u t : Mo.S} (τ1 τ2 : List (List (X.M × V) ⊕ Mo.R)),
    Exec Mo o s τ1 u → Exec Mo o u τ2 t → Exec Mo o s (τ1 ++ τ2) t
  | s, u, t, [], τ2, h1, h2 => by cases h1; exact h2
  | s, u, t, .inl g :: τ1, τ2, h1, h2 => by
    cases h1 with
    | grp a b => exact .grp a (exec_append τ1 τ2 b h2)
  | s, u, t, .inr r :: τ1, τ2, h1, h2 => by
    cases h1 with
    | rule a b => exact .rule a (exec_append τ1 τ2 b h2)

theorem exec_split : ∀ {s t : Mo.S} (τ1 τ2 : List (List (X.M × V) ⊕ Mo.R)),
    Exec Mo o s (τ1 ++ τ2) t → ∃ u, Exec Mo o s τ1 u ∧ Exec Mo o u τ2 t
  | s, t, [], τ2, h => ⟨s, .nil s, h⟩
  | s, t, .inl g :: τ1, τ2, h => by
    cases h with
    | grp a b =>
      obtain ⟨u, h1, h2⟩ := exec_split τ1 τ2 b
      exact ⟨u, .grp a h1, h2⟩
  | s, t, .inr r :: τ1, τ2, h => by
    cases h with
    | rule a b =>
      obtain ⟨u, h1, h2⟩ := exec_split τ1 τ2 b
      exact ⟨u, .rule a h1, h2⟩

end Compose

theorem groups_append {α β : Type} : ∀ (l1 l2 : List (α ⊕ β)), groups (l1 ++ l2) = groups l1 ++ groups l2
  | [], l2 => rfl
  | .inl a :: l1, l2 => by simp [groups, groups_append l1 l2]
  | .inr b :: l1, l2 => by simp [groups, groups_append l1 l2]

theorem groups_map_inr {α β : Type} : ∀ (ρ : List β), groups (ρ.map (Sum.inr (α := α))) = []
  | [] => rfl
  | b :: ρ => by simp [groups, groups_map_inr ρ]

theorem groups_map_inl {α β : Type} : ∀ (σ : List α), groups (σ.map (Sum.inl (β := β))) = σ
  | [] => rfl
  | a :: σ => by simp [groups, groups_map_inl σ]

/-- A run with no groups is rules only. -/
theorem groups_nil_rules {α β : Type} : ∀ (τ : List (α ⊕ β)), groups τ = [] → ∃ ρ : List β, τ = ρ.map .inr
  | [], _ => ⟨[], rfl⟩
  | .inl a :: τ, h => by simp [groups] at h
  | .inr b :: τ, h => by
    obtain ⟨ρ, hρ⟩ := groups_nil_rules τ h
    exact ⟨b :: ρ, by simp [hρ]⟩

/-- A run splits at its first group. -/
theorem groups_cons_split {α β : Type} : ∀ (τ : List (α ⊕ β)) (g : α) (gs : List α),
    groups τ = g :: gs → ∃ (ρ : List β) (post : List (α ⊕ β)), τ = ρ.map .inr ++ .inl g :: post ∧ groups post = gs
  | [], g, gs, h => by simp [groups] at h
  | .inl a :: τ, g, gs, h => by
    simp only [groups, List.cons.injEq] at h
    obtain ⟨rfl, rfl⟩ := h
    exact ⟨[], τ, rfl, rfl⟩
  | .inr b :: τ, g, gs, h => by
    obtain ⟨ρ, post, h1, h2⟩ := groups_cons_split τ g gs h
    exact ⟨b :: ρ, post, by simp [h1], h2⟩

/-- A run's rules before its first group, and what follows that group. -/
def leadR {α β : Type} : List (α ⊕ β) → List β × List (α ⊕ β)
  | [] => ([], [])
  | .inl _ :: l => ([], l)
  | .inr b :: l => (b :: (leadR l).1, (leadR l).2)

theorem leadR_split {α β : Type} : ∀ (τ : List (α ⊕ β)) (g : α) (gs : List α), groups τ = g :: gs →
    τ = (leadR τ).1.map .inr ++ .inl g :: (leadR τ).2 ∧ groups (leadR τ).2 = gs
  | [], g, gs, h => by simp [groups] at h
  | .inl a :: τ, g, gs, h => by
    simp only [groups, List.cons.injEq] at h
    obtain ⟨rfl, rfl⟩ := h
    exact ⟨rfl, rfl⟩
  | .inr b :: τ, g, gs, h => by
    obtain ⟨h1, h2⟩ := leadR_split τ g gs h
    refine ⟨?_, h2⟩
    simp only [leadR, List.map_cons, List.cons_append]
    exact congrArg _ h1

theorem leadR_nil {α β : Type} : ∀ (τ : List (α ⊕ β)), groups τ = [] → τ = (leadR τ).1.map .inr
  | [], _ => rfl
  | .inl a :: τ, h => by simp [groups] at h
  | .inr b :: τ, h => by
    simp only [leadR, List.map_cons]
    exact congrArg _ (leadR_nil τ h)

section ComposeP
variable (P : Parent V) (impl : (j : P.I) → Module V (P.CI j))

/-- A program unfolded with the hardware's results runs atomically, if each
    child runs its share of the calls with those results. -/
theorem runP_of_trace (oc : (j : P.I) → Outs (P.CI j).M V) :
    ∀ (p : Prog P.I (fun j => (P.CI j).M) V) (a b : (j : P.I) → (impl j).S) (v : V),
      (trace oc p).2 = some v →
      (∀ j, ExecG (impl j) (oc j) (a j) (proj j (trace oc p).1) (b j)) →
      RunP P impl p a v b
  | .ret v', a, b, v, h1, h2 => by
    simp only [trace, Option.some.injEq] at h1
    subst h1
    have : a = b := funext fun j => by
      have := h2 j
      simp only [trace, proj_nil] at this
      exact execG_nil_eq _ _ this
    subst this
    exact .ret _ _
  | .fail, a, b, v, h1, h2 => by simp [trace] at h1
  | .call j0 m x k, a, b, v, h1, h2 => by
    simp only [trace] at h1 h2
    have h0 := h2 j0
    rw [proj_cons_self] at h0
    cases h0 with
    | cons hm hrest =>
      rename_i s1
      apply RunP.call hm
      apply runP_of_trace oc (k ((oc j0).ret m)) (upd a j0 s1) b v h1
      intro j
      by_cases hj : j = j0
      · subst hj; rw [upd_same]; exact hrest
      · rw [upd_ne _ _ hj]
        have := h2 j
        rwa [proj_cons_ne _ _ (Ne.symm hj)] at this

/-- Choose, per child, a witness of a per-child existential. -/
theorem choose_fun {I : Type} {B : I → Type} {Q : (j : I) → B j → Prop} (h : ∀ j, ∃ b, Q j b) :
    ∃ f : (j : I) → B j, ∀ j, Q j (f j) :=
  Classical.axiomOfChoice h

/-- A caller's group of parent method calls runs atomically. -/
theorem execG_parent (oc : (j : P.I) → Outs (P.CI j).M V) (o : Outs P.X.M V) :
    ∀ (g : List (P.X.M × V)) (a b : (j : P.I) → (impl j).S),
      (∀ c ∈ g, (trace oc (P.body c.1 c.2)).2 = some (o.ret c.1)) →
      (∀ j, ExecG (impl j) (oc j) (a j) (proj j (g.flatMap (fun c => (trace oc (P.body c.1 c.2)).1))) (b j)) →
      ExecG (P.toModule impl) o a g b
  | [], a, b, _, h => by
    have : a = b := funext fun j => by
      have := h j
      simp only [List.flatMap_nil, proj_nil] at this
      exact execG_nil_eq _ _ this
    subst this; exact .nil _
  | (m, x) :: g, a, b, h1, h2 => by
    have hsplit : ∀ j, ∃ u : (impl j).S,
        ExecG (impl j) (oc j) (a j) (proj j (trace oc (P.body m x)).1) u ∧
        ExecG (impl j) (oc j) u (proj j (g.flatMap (fun c => (trace oc (P.body c.1 c.2)).1))) (b j) := by
      intro j
      have := h2 j
      rw [List.flatMap_cons, proj_append] at this
      exact execG_append _ _ _ _ this
    obtain ⟨u, hu⟩ := choose_fun hsplit
    refine .cons ?_ (execG_parent oc o g u b (fun c hc => h1 c (List.mem_cons_of_mem _ hc)) (fun j => (hu j).2))
    exact runP_of_trace P impl oc (P.body m x) a u (o.ret m) (h1 (m, x) (List.mem_cons_self ..))
      (fun j => (hu j).1)

/-- Child `j`'s rules, run inside the parent. -/
theorem exec_child_rules (o : Outs P.X.M V) (oc : (j : P.I) → Outs (P.CI j).M V) (j : P.I) :
    ∀ (ρ : List (impl j).R) (a : (j : P.I) → (impl j).S) (t : (impl j).S),
      Exec (impl j) (oc j) (a j) (ρ.map .inr) t →
      Exec (P.toModule impl) o a (ρ.map (fun r => .inr (.inr ⟨j, r⟩))) (upd a j t)
  | [], a, t, h => by
    cases h
    have : upd a j (a j) = a := funext fun j' => by
      by_cases hj : j' = j
      · subst hj; rw [upd_same]
      · rw [upd_ne _ _ hj]
    rw [this]; exact .nil _
  | r :: ρ, a, t, h => by
    cases h with
    | rule hr hrest =>
      rename_i s1
      have ih := exec_child_rules o oc j ρ (upd a j s1) t (by rw [upd_same]; exact hrest)
      rw [upd_upd] at ih
      exact .rule ⟨s1, hr, rfl⟩ ih

/-- Children's rules, child by child, as steps of the parent. -/
def kidRules {I G PR : Type} {Rj : I → Type} (kids : List I) (ρ : (j : I) → List (Rj j)) :
    List (G ⊕ (PR ⊕ Σ j, Rj j)) :=
  kids.flatMap (fun j => (ρ j).map (fun r => .inr (.inr ⟨j, r⟩)))

/-- **The merged run**, from the parent's steps and the children's runs alone: before each
    parent step, each child's rules up to its group for that step. -/
def mergeL {I G PR : Type} {Gj Rj : I → Type} (kids : List I) :
    List (G ⊕ PR) → ((j : I) → List (Gj j ⊕ Rj j)) → List (G ⊕ (PR ⊕ Σ j, Rj j))
  | [], τs => kidRules kids (fun j => (leadR (τs j)).1)
  | e :: τP, τs => kidRules kids (fun j => (leadR (τs j)).1) ++
      Sum.map id Sum.inl e :: mergeL kids τP (fun j => (leadR (τs j)).2)

theorem groups_kidRules {I G PR : Type} {Rj : I → Type} (ρ : (j : I) → List (Rj j)) :
    ∀ (kids : List I), groups (kidRules (G := G) (PR := PR) kids ρ) = []
  | [] => rfl
  | j :: kids => by
    have h : ∀ (l : List (Rj j)),
        groups ((l.map (fun r => (Sum.inr (Sum.inr ⟨j, r⟩) : G ⊕ (PR ⊕ Σ j, Rj j)))) ++
          kidRules kids ρ) = [] := by
      intro l
      induction l with
      | nil => exact groups_kidRules ρ kids
      | cons _ _ ih => simpa [groups] using ih
    exact h (ρ j)

/-- The merged run keeps the parent's groups, whatever the children's runs. -/
theorem groups_mergeL {I G PR : Type} {Gj Rj : I → Type} (kids : List I) :
    ∀ (τP : List (G ⊕ PR)) (τs : (j : I) → List (Gj j ⊕ Rj j)), groups (mergeL kids τP τs) = groups τP
  | [], τs => groups_kidRules _ kids
  | e :: τP, τs => by
    simp only [mergeL]
    rw [groups_append, groups_kidRules, List.nil_append]
    cases e with
    | inl g => simp [groups, groups_mergeL kids τP]
    | inr r => simp [groups, groups_mergeL kids τP]

/-- The children's rules `ρ j`, run inside the parent, child by child. -/
theorem exec_kidRules (o : Outs P.X.M V) (oc : (j : P.I) → Outs (P.CI j).M V)
    (b : (j : P.I) → (impl j).S) :
    ∀ (L : List P.I), L.Nodup → ∀ (a : (j : P.I) → (impl j).S) (ρ : (j : P.I) → List (impl j).R),
      (∀ j ∈ L, Exec (impl j) (oc j) (a j) ((ρ j).map .inr) (b j)) →
      Exec (P.toModule impl) o a (kidRules (G := List (P.X.M × V)) (PR := P.PR) L ρ)
        (fun j => if j ∈ L then b j else a j)
  | [], _, a, ρ, _ => by
    simp only [kidRules, List.flatMap_nil, List.not_mem_nil, ite_false]; exact .nil _
  | j0 :: L, hnd, a, ρ, h => by
    have h1 := exec_child_rules P impl o oc j0 (ρ j0) a (b j0) (h j0 (List.mem_cons_self ..))
    have hnd' := List.nodup_cons.mp hnd
    have h2 := exec_kidRules o oc b L hnd'.2 (upd a j0 (b j0)) ρ (fun j hj => by
      have hne : j ≠ j0 := fun e => hnd'.1 (e ▸ hj)
      rw [upd_ne _ _ hne]
      exact h j (List.mem_cons_of_mem _ hj))
    have e : (fun j => if j ∈ L then b j else upd a j0 (b j0) j) =
        (fun j => if j ∈ j0 :: L then b j else a j) := funext fun j => by
      by_cases hj : j = j0
      · subst hj; simp [hnd'.1, upd_same]
      · simp [hj, upd_ne _ _ hj]
    rw [← e]
    simp only [kidRules, List.flatMap_cons]
    exact exec_append _ _ _ _ h1 h2

/-- **The children's runs merge into one run of the parent**, along `mergeL`: child rules
    go between parent steps, never inside one. -/
theorem merge_exec (oc : (j : P.I) → Outs (P.CI j).M V) (o : Outs P.X.M V) :
    ∀ (τP : List (List (P.X.M × V) ⊕ P.PR)) (a fin : (j : P.I) → (impl j).S)
      (τs : (j : P.I) → List (List ((P.CI j).M × V) ⊕ (impl j).R)),
      (∀ j, groups (τs j) = sigmaJ P oc τP j) →
      (∀ j, Exec (impl j) (oc j) (a j) (τs j) (fin j)) →
      (∀ e ∈ τP, StepOK P oc o e) →
      Exec (P.toModule impl) o a (mergeL P.kids τP τs) fin
  | [], a, fin, τs, hg, hx, _ => by
    have hr : ∀ j ∈ P.kids, Exec (impl j) (oc j) (a j) ((leadR (τs j)).1.map .inr) (fin j) := by
      intro j _
      have hx' := hx j
      rwa [leadR_nil (τs j) (by rw [hg j]; rfl)] at hx'
    have h := exec_kidRules P impl o oc fin P.kids P.kids_nodup a (fun j => (leadR (τs j)).1) hr
    have e : (fun j => if j ∈ P.kids then fin j else a j) = fin := funext fun j => by
      simp [P.kids_all j]
    rw [e] at h
    exact h
  | e :: τP, a, fin, τs, hg, hx, hst => by
    have hsp : ∀ j, ∃ q : (impl j).S × (impl j).S,
        Exec (impl j) (oc j) (a j) ((leadR (τs j)).1.map .inr) q.1 ∧
        ExecG (impl j) (oc j) q.1 (proj j (stepTrace P oc e)) q.2 ∧
        Exec (impl j) (oc j) q.2 (leadR (τs j)).2 (fin j) := by
      intro j
      obtain ⟨h1, -⟩ := leadR_split (τs j) _ _ (by rw [hg j]; rfl)
      have hx' := hx j
      rw [h1] at hx'
      obtain ⟨u, hu1, hu2⟩ := exec_split _ _ _ _ hx'
      cases hu2 with
      | grp hg' hrest =>
        rename_i v
        exact ⟨(u, v), hu1, hg', hrest⟩
    obtain ⟨f, hf⟩ := choose_fun hsp
    have hτ1 := exec_kidRules P impl o oc (fun j => (f j).1) P.kids P.kids_nodup a
      (fun j => (leadR (τs j)).1) (fun j _ => (hf j).1)
    have e1 : (fun j => if j ∈ P.kids then (f j).1 else a j) = (fun j => (f j).1) :=
      funext fun j => by simp [P.kids_all j]
    rw [e1] at hτ1
    have hτ2 := merge_exec oc o τP (fun j => (f j).2) fin (fun j => (leadR (τs j)).2)
      (fun j => (leadR_split (τs j) _ _ (by rw [hg j]; rfl)).2) (fun j => (hf j).2.2)
      (fun e' he' => hst e' (List.mem_cons_of_mem _ he'))
    simp only [mergeL]
    match e, hst, hf with
    | .inl g, hst, hf =>
      have hstep : ExecG (P.toModule impl) o (fun j => (f j).1) g (fun j => (f j).2) :=
        execG_parent P impl oc o g _ _ (hst (.inl g) (List.mem_cons_self ..)) (fun j => (hf j).2.1)
      exact exec_append _ _ _ _ hτ1 (.grp hstep hτ2)
    | .inr r, hst, hf =>
      have hok := hst (.inr r) (List.mem_cons_self ..)
      simp only [StepOK, Option.isSome_iff_exists] at hok
      obtain ⟨v, hv⟩ := hok
      have hrun : RunP P impl (P.rbody r) (fun j => (f j).1) v (fun j => (f j).2) :=
        runP_of_trace P impl oc (P.rbody r) _ _ v hv (fun j => (hf j).2.1)
      exact exec_append _ _ _ _ hτ1 (.rule ⟨v, hrun⟩ hτ2)

end ComposeP

/-- **The composition theorem.**  If every child realizes its contract and respects its
    output aliases, and the family meets its obligation, the module realizes its contract
    with the run fixed before every skipped value; each child's task log stays its own. -/
theorem compose (F : ParentU V) (impl : (j : F.I) → ModuleU V (F.CI j)) (hC : ∀ j, RealizesU (impl j))
    (hA : ∀ j, Aliased F.toB (impl j).hwm) (hO : OblU F) : RealizesU (F.toModuleU impl) := by
  intro s σ w o s' g hcl hL hD hhw hok hrdy
  obtain ⟨hgw, hchild, -, hrd, -, hdata⟩ := hhw
  obtain ⟨hasm, hokc⟩ := hok
  rw [hgw] at hasm
  have hal : ∀ j, AliasOK F.toB (F.CI j).ann (g.oc j) := fun j =>
    hA j (s j) (g.wc j) (g.oc j) (s' j) (g.sub j) (congrFun hcl j) (hchild j)
  have hrdy' : ∀ c ∈ σ.flatten, F.X.ann.alwaysRdy c.1 = false → (F.comb g.oc w).2.rdy c.1 = true :=
    fun c hc h => hrd ▸ hrdy c hc h
  obtain ⟨hoh, τP, σC, hgP, hσC, hu⟩ := hO g.oc w σ hasm hal hL hD hrdy'
  obtain ⟨hwc, ho⟩ := hdata hoh
  subst ho
  -- each child's run comes from its call sequence alone, before any skipped value
  have hch : ∀ j, ∃ τj : List (List ((F.CI j).M × V) ⊕ (impl j).R), groups τj = σC j ∧
      ∀ uj : (impl j).U, ∃ fin : (impl j).S, Exec ((impl j).at uj) (g.oc j) (s j) τj fin ∧
        s' j = (impl j).clean fin ∧ ∀ l, (impl j).trc (g.sub j) l = (impl j).log fin l := by
    intro j
    have hcj := hchild j
    rw [hwc] at hcj
    exact hC j (s j) (σC j) ((F.comb g.oc w).1 j) (g.oc j) (s' j) (g.sub j) (congrFun hcl j)
      (hσC j).1 (hσC j).2.1 hcj (hokc j) (hσC j).2.2
  obtain ⟨τs, hτs⟩ := choose_fun hch
  refine ⟨mergeL F.kids τP τs, (groups_mergeL F.kids τP τs).trans hgP, fun u => ?_⟩
  obtain ⟨fin, hfin⟩ := choose_fun (fun j => (hτs j).2 (u.2 j))
  refine ⟨fin, ?_, funext fun j => (hfin j).2.1, fun l => (hfin l.1).2.2 l.2⟩
  exact merge_exec (F.at u.1) (fun j => (impl j).at (u.2 j)) g.oc (F.comb g.oc w).2 τP s fin τs
    (fun j => (hτs j).1.trans ((hu u.1).2 j).symm) (fun j => (hfin j).1) (hu u.1).1

/-! ## Non-vacuity: causal totality, proved once

`Realizes` says nothing about a module whose hardware relation is empty, and
a parent's relation is a fixpoint through its children: a combinational
loop through a child's path (a wire, a CReg) can empty it, and with it every
ancestor's.  `Causal` rules this out.  A parent gets it from its children's
and an acyclicity certificate (`Acyc`), whose data a generated file emits
from the netlist and whose checks are decided: which children's outputs and
parent inputs each piece of `comb` reads, a rank on the children's output
ports that every read through a child's path decreases, and the parent
inputs each child output depends on, contained in the parent's declared
paths.  `causal_toModule` evaluates the fixpoint by iteration
(`iterOC`), stable after as many rounds as there are ranks. -/

section Acyclic
variable (P : Parent V)

/-- **The acyclicity certificate** of a parent: which signals each piece of
    `comb` reads, a rank on the children's outputs, and the parent inputs
    each child output depends on. -/
structure Acyc where
  /-- the children's outputs that child `j`'s input `p` reads -/
  D : (j : P.I) → (P.CI j).M → List (Σ j, (P.CI j).M)
  /-- the parent's inputs that child `j`'s input `p` reads -/
  Dw : (j : P.I) → (P.CI j).M → List P.X.M
  /-- the children's outputs and parent inputs that the parent's output `q` reads -/
  Do : P.X.M → List (Σ j, (P.CI j).M)
  Dwo : P.X.M → List P.X.M
  dep : ∀ oc oc' w w' j p, (∀ y ∈ D j p, AgreeO (oc y.1) (oc' y.1) y.2) → (∀ p' ∈ Dw j p, AgreeW w w' p') →
    AgreeW ((P.comb oc w).1 j) ((P.comb oc' w').1 j) p
  depo : ∀ oc oc' w w' q, (∀ y ∈ Do q, AgreeO (oc y.1) (oc' y.1) y.2) → (∀ p' ∈ Dwo q, AgreeW w w' p') →
    AgreeO (P.comb oc w).2 (P.comb oc' w').2 q
  /-- a rank on the children's output ports, below `N` -/
  rk : (Σ j, (P.CI j).M) → Nat
  N : Nat
  rk_lt : ∀ y, rk y < N
  /-- every read through a child's path is of a lower rank (no loop) -/
  rk_ok : ∀ j q p, (P.CI j).ann.path p q = true → ∀ y ∈ D j p, rk y < rk ⟨j, q⟩
  /-- the parent inputs a child output depends on -/
  reach : (Σ j, (P.CI j).M) → List P.X.M
  reach_w : ∀ j q p, (P.CI j).ann.path p q = true → ∀ p' ∈ Dw j p, p' ∈ reach ⟨j, q⟩
  reach_o : ∀ j q p, (P.CI j).ann.path p q = true → ∀ y ∈ D j p, ∀ p' ∈ reach y, p' ∈ reach ⟨j, q⟩
  /-- the parent's declared paths contain its real ones -/
  path_w : ∀ q, ∀ p ∈ Dwo q, P.X.ann.path p q = true
  path_o : ∀ q, ∀ y ∈ Do q, ∀ p ∈ reach y, P.X.ann.path p q = true

variable {P} (A : Acyc P) (impl : (j : P.I) → Module V (P.CI j))
  (out : (j : P.I) → (impl j).S → Wires (P.CI j).M V → Outs (P.CI j).M V)
  (hout : ∀ j s w w' q, (∀ p, (P.CI j).ann.path p q = true → AgreeW w w' p) → AgreeO (out j s w) (out j s w') q)
  (v0 : V)

/-- The children's outputs after `k` rounds of evaluation. -/
def iterOC (s : (j : P.I) → (impl j).S) (w : Wires P.X.M V) : Nat → (j : P.I) → Outs (P.CI j).M V
  | 0 => fun j => out j (s j) ⟨fun _ => false, fun _ => v0⟩
  | k + 1 => fun j => out j (s j) ((P.comb (iterOC s w k) w).1 j)

include A hout in
/-- After more rounds than an output's rank, it no longer changes. -/
theorem iterOC_stable (s : (j : P.I) → (impl j).S) (w : Wires P.X.M V) :
    ∀ n (y : Σ j, (P.CI j).M), A.rk y < n → ∀ k k', n ≤ k → n ≤ k' →
      AgreeO (iterOC impl out v0 s w k y.1) (iterOC impl out v0 s w k' y.1) y.2
  | 0, _, h, _, _, _, _ => absurd h (Nat.not_lt_zero _)
  | n + 1, ⟨j, q⟩, h, k, k', hk, hk' => by
    obtain ⟨a, rfl⟩ : ∃ a, k = a + 1 := ⟨k - 1, by omega⟩
    obtain ⟨b, rfl⟩ : ∃ b, k' = b + 1 := ⟨k' - 1, by omega⟩
    show AgreeO (out j (s j) ((P.comb (iterOC impl out v0 s w a) w).1 j))
      (out j (s j) ((P.comb (iterOC impl out v0 s w b) w).1 j)) q
    apply hout j (s j)
    intro p hp
    apply A.dep
    · intro y hy
      have := A.rk_ok j q p hp y hy
      exact iterOC_stable s w n y (by omega) a b (by omega) (by omega)
    · intro p' _; exact ⟨rfl, rfl⟩

include hout in
/-- A child output depends only on the parent inputs it reaches. -/
theorem iterOC_reach (s : (j : P.I) → (impl j).S) (w w' : Wires P.X.M V) :
    ∀ k (y : Σ j, (P.CI j).M), (∀ p ∈ A.reach y, AgreeW w w' p) →
      AgreeO (iterOC impl out v0 s w k y.1) (iterOC impl out v0 s w' k y.1) y.2
  | 0, _, _ => ⟨rfl, rfl⟩
  | k + 1, ⟨j, q⟩, h => by
    show AgreeO (out j (s j) ((P.comb (iterOC impl out v0 s w k) w).1 j))
      (out j (s j) ((P.comb (iterOC impl out v0 s w' k) w').1 j)) q
    apply hout j (s j)
    intro p hp
    apply A.dep
    · intro y hy
      exact iterOC_reach s w w' k y (fun p' hp' => h p' (A.reach_o j q p hp y hy p' hp'))
    · intro p' hp'
      exact h p' (A.reach_w j q p hp p' hp')

/-- The children's outputs at the fixpoint. -/
def fixOC (s : (j : P.I) → (impl j).S) (w : Wires P.X.M V) : (j : P.I) → Outs (P.CI j).M V :=
  iterOC impl out v0 s w A.N

include hout in
theorem fixOC_eq (s : (j : P.I) → (impl j).S) (w : Wires P.X.M V) (j : P.I) :
    fixOC A impl out v0 s w j = out j (s j) ((P.comb (fixOC A impl out v0 s w) w).1 j) := by
  apply Outs.ext'
  intro q
  exact iterOC_stable A impl out hout v0 s w A.N ⟨j, q⟩ (A.rk_lt _) A.N (A.N + 1) (Nat.le_refl _) (by omega)

end Acyclic

/-- An acyclicity certificate for a parent whose children have no
    combinational paths (registers, FIFOs, path-free modules): no rank or
    reach facts, only what `comb` reads, and the parent's own paths (the
    parent inputs its outputs read). -/
def Acyc.ofMoore (P : Parent V) (D : (j : P.I) → (P.CI j).M → List (Σ j, (P.CI j).M))
    (Dw : (j : P.I) → (P.CI j).M → List P.X.M) (Do : P.X.M → List (Σ j, (P.CI j).M)) (Dwo : P.X.M → List P.X.M)
    (dep : ∀ oc oc' w w' j p, (∀ y ∈ D j p, AgreeO (oc y.1) (oc' y.1) y.2) → (∀ p' ∈ Dw j p, AgreeW w w' p') →
      AgreeW ((P.comb oc w).1 j) ((P.comb oc' w').1 j) p)
    (depo : ∀ oc oc' w w' q, (∀ y ∈ Do q, AgreeO (oc y.1) (oc' y.1) y.2) → (∀ p' ∈ Dwo q, AgreeW w w' p') →
      AgreeO (P.comb oc w).2 (P.comb oc' w').2 q)
    (hC : ∀ j p q, (P.CI j).ann.path p q = false) (hW : ∀ q, ∀ p ∈ Dwo q, P.X.ann.path p q = true) : Acyc P where
  D := D
  Dw := Dw
  Do := Do
  Dwo := Dwo
  dep := dep
  depo := depo
  rk _ := 0
  N := 1
  rk_lt _ := Nat.zero_lt_one
  rk_ok j q p h := by rw [hC] at h; cases h
  reach _ := []
  reach_w j q p h := by rw [hC] at h; cases h
  reach_o j q p h := by rw [hC] at h; cases h
  path_w := hW
  path_o q y _ p hp := by simp at hp

/-- Outputs of an interface without aliases respect them. -/
theorem aliasOK_of_none {M : Type} (toB : V → Bool) (A : Ann M) (o : Outs M V)
    (h1 : ∀ p q, A.same p q = false) (h2 : ∀ a p, A.rdyRet a p = false) : AliasOK toB A o :=
  ⟨fun p q h => by { rw [h1] at h; cases h }, fun a p h => by { rw [h2] at h; cases h }⟩

/-- **A parent of causal children with an acyclicity certificate is causal**
    (for an inhabited value type, from which the evaluation starts). -/
theorem causal_toModule [Inhabited V] (P : Parent V) (impl : (j : P.I) → Module V (P.CI j))
    (hC : ∀ j, Causal (impl j)) (A : Acyc P) : Causal (P.toModule impl) := by
  obtain ⟨out, hout⟩ := choose_fun hC
  have hc : ∀ j s w w' q, (∀ p, (P.CI j).ann.path p q = true → AgreeW w w' p) →
      AgreeO (out j s w) (out j s w') q := fun j => (hout j).1
  refine ⟨fun s w => (P.comb (fixOC A impl out default s w) w).2, ?_, ?_⟩
  · intro s w w' q hq
    apply A.depo
    · intro y hy
      exact iterOC_reach A impl out hc default s w w' A.N y (fun p hp => hq p (A.path_o q y hy p hp))
    · intro p hp
      exact hq p (A.path_w q p hp)
  · intro s w hcl
    have hj : ∀ j, ∃ q : (impl j).S × (impl j).Sig,
        (impl j).hw (s j) ((P.comb (fixOC A impl out default s w) w).1 j) (fixOC A impl out default s w j)
          q.1 q.2 := by
      intro j
      obtain ⟨s'j, gj, h⟩ := (hout j).2 (s j) ((P.comb (fixOC A impl out default s w) w).1 j) (congrFun hcl j)
      refine ⟨(s'j, gj), ?_⟩
      rw [fixOC_eq A impl out hc default s w j]
      exact h
    obtain ⟨f, hf⟩ := choose_fun hj
    exact ⟨fun j => (f j).1, ⟨w, fixOC A impl out default s w, (P.comb (fixOC A impl out default s w) w).1,
      fun j => (f j).2⟩, rfl, hf, fun _ => rfl, rfl, fun _ _ => rfl, fun _ => ⟨rfl, rfl⟩⟩

/-- The instance tree: leaves are modules with a contract and causal
    totality (a primitive's, proved below, or an imported Verilog module's,
    assumed), nodes are families that meet their obligations (`OblU`, the
    order before the skipped values), with an acyclicity certificate, over
    built children that respect their output aliases.  An assumed synthesized leaf must export
    every action method C with itself (bsc's exported self relation is no guarantee). -/
inductive Built : (X : Iface) → ModuleU V X → Prop
  | leaf {X : Iface} (Mo : Module V X) : Realizes Mo → Causal Mo → Built X Mo.toU
  | node (F : ParentU V) (impl : (j : F.I) → ModuleU V (F.CI j)) :
      (∀ j, Built (F.CI j) (impl j)) → (∀ j, Aliased F.toB (impl j).hwm) → OblU F → Acyc F.base →
      Built F.X (F.toModuleU impl)

/-- **The hierarchy theorem.**  A built module realizes its contract, with one run fixed
    before the skipped values of every module in the tree. -/
theorem Built.realizes {X : Iface} {Mo : ModuleU V X} (h : Built X Mo) : RealizesU Mo := by
  induction h with
  | leaf Mo h _ => exact realizesU_toU Mo h
  | node F impl _ hA hO _ ih => exact compose F impl ih hA hO

/-- A family of causal children with an acyclicity certificate is causal (its hardware
    reads no program, so `causal_toModule` applies to its `u`-free part). -/
theorem causal_toModuleU [Inhabited V] (F : ParentU V) (impl : (j : F.I) → ModuleU V (F.CI j))
    (hC : ∀ j, Causal (impl j).hwm) (A : Acyc F.base) : Causal (F.toModuleU impl).hwm :=
  causal_toModule F.base (fun j => (impl j).hwm) hC A

/-- **And its contract is not vacuous**: it has a hardware cycle for every
    input, from every state a cycle can start in. -/
theorem Built.causal [Inhabited V] {X : Iface} {Mo : ModuleU V X} (h : Built X Mo) : Causal Mo.hwm := by
  induction h with
  | leaf Mo _ h => exact h
  | node F impl _ _ _ A ih => exact causal_toModuleU F impl ih A

/-- **The top theorem, in full**: every hardware cycle in which the
    assertions hold agrees with a one-at-a-time run (for every legal call
    sequence on the ports), and every input has a hardware cycle.  `Mo.ok`
    unfolds to the conjunction of each instance's `asm` on its signals.
    The run is fixed before the skipped values of every module in the tree,
    and agrees with the hardware at every assignment of them.

    The agreement includes each module's system-task calls (condition,
    arguments, order), compared per module: Verilog runs each synthesized
    module's tasks in its own always block, in an unspecified order on one
    clock edge, so different modules' calls are not ordered against each
    other.  Bluesim should follow the full one-at-a-time order; that is a
    property of the Bluesim back end, outside this proof.  Task calls inside
    interface methods (bsc G0020) are not statements, so they are not compared. -/
theorem Built.top [Inhabited V] {X : Iface} {Mo : ModuleU V X} (h : Built X Mo) :
    RealizesU Mo ∧ ∀ s w, Mo.clean s = s → ∃ o s' g, Mo.hw s w o s' g := by
  refine ⟨h.realizes, fun s w hc => ?_⟩
  obtain ⟨out, -, ht⟩ := h.causal
  obtain ⟨s', g, hg⟩ := ht s w hc
  exact ⟨out s w, s', g, hg⟩

/-- An acyclicity certificate reads only `comb` and the interfaces, so a parent's moves to
    its family. -/
def Acyc.toU {P : Parent V} (A : Acyc P) : Acyc P.toU.base := { A with }

/-- **A built module respects its output aliases** when each aliased result is an exact
    port and `comb` computes the alias (a ready output: `rdy a = toB (ret p)`). -/
theorem aliased_toModuleU (F : ParentU V) (impl : (j : F.I) → ModuleU V (F.CI j)) (toB : V → Bool)
    (hs : ∀ p q, F.X.ann.same p q = true → F.exactRet p = true ∧ F.exactRet q = true ∧
      ∀ oc w, (F.comb oc w).2.ret p = (F.comb oc w).2.ret q)
    (hr : ∀ a p, F.X.ann.rdyRet a p = true → F.exactRet p = true ∧
      ∀ oc w, (F.comb oc w).2.rdy a = toB ((F.comb oc w).2.ret p)) :
    Aliased toB (F.toModuleU impl).hwm := by
  intro s w o s' g _ hh
  obtain ⟨hgw, -, -, hrd, hex, -⟩ := hh
  refine ⟨fun p q hpq => ?_, fun a p hap => ?_⟩
  · obtain ⟨hp, hq, he⟩ := hs p q hpq
    rw [hex p hp, hex q hq, he]
  · obtain ⟨hp, he⟩ := hr a p hap
    rw [hrd, hex p hp, he]

section Lists

theorem sublist_flatten_pair {α : Type} {a b : α} : ∀ (L : List (List α)), [a, b] <+ L.flatten →
    (∃ l ∈ L, [a, b] <+ l) ∨ (∃ l1 l2, [l1, l2] <+ L ∧ a ∈ l1 ∧ b ∈ l2)
  | [], h => by simp at h
  | l :: L, h => by
    rw [List.flatten_cons] at h
    obtain ⟨p1, p2, e, h1, h2⟩ := List.sublist_append_iff.mp h
    rcases p1 with _ | ⟨x, _ | ⟨y, _ | ⟨z, p1⟩⟩⟩
    · simp only [List.nil_append] at e
      subst e
      rcases sublist_flatten_pair L h2 with ⟨l', hl', hs⟩ | ⟨l1, l2, hs, ha, hb⟩
      · exact .inl ⟨l', List.mem_cons_of_mem _ hl', hs⟩
      · exact .inr ⟨l1, l2, hs.trans (List.sublist_cons_self _ _), ha, hb⟩
    · simp only [List.singleton_append, List.cons.injEq] at e
      obtain ⟨e1, e2⟩ := e
      subst e1 e2
      have ha : a ∈ l := List.singleton_sublist.mp h1
      obtain ⟨l2, hl2, hb2⟩ := List.mem_flatten.mp (List.singleton_sublist.mp h2)
      exact .inr ⟨l, l2, List.cons_sublist_cons.mpr (List.singleton_sublist.mpr hl2), ha, hb2⟩
    · simp only [List.cons_append, List.nil_append, List.cons.injEq] at e
      obtain ⟨e1, e2, e3⟩ := e
      subst e1 e2
      exact .inl ⟨l, List.mem_cons_self .., h1⟩
    · simp at e

theorem sublist_map_pair {α β : Type} {f : α → β} {a b : β} {L : List α} (h : [a, b] <+ L.map f) :
    ∃ x y, [x, y] <+ L ∧ a = f x ∧ b = f y := by
  obtain ⟨l', hl', e⟩ := List.sublist_map_iff.mp h
  rcases l' with _ | ⟨x, _ | ⟨y, _ | ⟨z, l'⟩⟩⟩ <;> simp at e
  obtain ⟨e1, e2⟩ := e
  exact ⟨x, y, hl', e1, e2⟩

theorem either_sublist {α : Type} {a b : α} : ∀ {l : List α}, a ∈ l → b ∈ l → a ≠ b → [a, b] <+ l ∨ [b, a] <+ l
  | c :: l, ha, hb, hne => by
    rcases List.mem_cons.mp ha with rfl | ha'
    · rcases List.mem_cons.mp hb with rfl | hb'
      · exact absurd rfl hne
      · exact .inl (List.cons_sublist_cons.mpr (List.singleton_sublist.mpr hb'))
    · rcases List.mem_cons.mp hb with rfl | hb'
      · exact .inr (List.cons_sublist_cons.mpr (List.singleton_sublist.mpr ha'))
      · rcases either_sublist ha' hb' hne with h | h
        · exact .inl (h.trans (List.sublist_cons_self _ _))
        · exact .inr (h.trans (List.sublist_cons_self _ _))

theorem nodup_pair_both {α : Type} {a b : α} : ∀ {l : List α}, l.Nodup → [a, b] <+ l → [b, a] <+ l → False
  | [], _, h, _ => by simp at h
  | c :: l, hnd, h1, h2 => by
    have hc := List.nodup_cons.mp hnd
    have m : ∀ {x y : α}, [x, y] <+ c :: l → ([x, y] <+ l) ∨ (x = c ∧ y ∈ l) := by
      intro x y h
      rcases List.sublist_cons_iff.mp h with h | ⟨r, e, h⟩
      · exact .inl h
      · simp only [List.cons.injEq] at e
        obtain ⟨e1, e2⟩ := e
        subst e2
        exact .inr ⟨e1, List.singleton_sublist.mp h⟩
    rcases m h1 with h1' | ⟨e1, hb⟩ <;> rcases m h2 with h2' | ⟨e2, ha⟩
    · exact nodup_pair_both hc.2 h1' h2'
    · have : b ∈ l := h1'.subset (by simp)
      exact hc.1 (e2 ▸ this)
    · have : a ∈ l := h2'.subset (by simp)
      exact hc.1 (e1 ▸ this)
    · exact hc.1 (e1 ▸ ha)

theorem flatMap_congr' {α β : Type} {f g : α → List β} {l : List α} (h : ∀ a ∈ l, f a = g a) :
    l.flatMap f = l.flatMap g := by
  rw [List.flatMap_def, List.flatMap_def, List.map_congr_left h]

theorem groups_flatMap {α β γ : Type} (f : γ → List (α ⊕ β)) :
    ∀ (L : List γ), groups (L.flatMap f) = L.flatMap (fun x => groups (f x))
  | [] => rfl
  | x :: L => by simp [List.flatMap_cons, groups_append, groups_flatMap f L]

theorem groups_map_sum {α α' β : Type} (f : α → α') :
    ∀ (L : List (α ⊕ β)), groups (L.map (Sum.map f id)) = (groups L).map f
  | [] => rfl
  | .inl a :: L => by simp [groups, groups_map_sum f L]
  | .inr b :: L => by simp [groups, groups_map_sum f L]

theorem getD_mem_flatten {α : Type} : ∀ (σ : List (List α)) (k : Nat) (c : α), c ∈ σ.getD k [] → c ∈ σ.flatten
  | [], k, c, h => by simp at h
  | g :: σ, 0, c, h => by
    rw [List.getD_cons_zero] at h
    rw [List.flatten_cons]; exact List.mem_append_left _ h
  | g :: σ, k + 1, c, h => by
    rw [List.getD_cons_succ] at h
    rw [List.flatten_cons]; exact List.mem_append_right _ (getD_mem_flatten σ k c h)

theorem getD_mem {α : Type} : ∀ (σ : List (List α)) (k : Nat), k < σ.length → σ.getD k [] ∈ σ
  | [], k, h => by simp at h
  | g :: σ, 0, _ => by simp
  | g :: σ, k + 1, h => by
    rw [List.getD_cons_succ]
    exact List.mem_cons_of_mem _ (getD_mem σ k (by simp at h; omega))

theorem mem_flatten_getD {α : Type} : ∀ (σ : List (List α)) (c : α), c ∈ σ.flatten →
    ∃ k, k < σ.length ∧ c ∈ σ.getD k []
  | [], c, h => by simp at h
  | g :: σ, c, h => by
    rw [List.flatten_cons] at h
    rcases List.mem_append.mp h with h | h
    · exact ⟨0, by simp, by simpa using h⟩
    · obtain ⟨k, hk, hc⟩ := mem_flatten_getD σ c h
      exact ⟨k + 1, by simp; omega, by rw [List.getD_cons_succ]; exact hc⟩

theorem pair_getD {α : Type} : ∀ (σ : List (List α)) (k1 k2 : Nat) (c c' : α), k1 < k2 →
    c ∈ σ.getD k1 [] → c' ∈ σ.getD k2 [] → [c, c'] <+ σ.flatten
  | [], k1, k2, c, c', _, h, _ => by simp at h
  | g :: σ, 0, k2 + 1, c, c', _, h1, h2 => by
    rw [List.getD_cons_zero] at h1
    rw [List.getD_cons_succ] at h2
    rw [List.flatten_cons]
    exact List.Sublist.append (List.singleton_sublist.mpr h1)
      (List.singleton_sublist.mpr (getD_mem_flatten σ k2 c' h2))
  | g :: σ, k1 + 1, k2 + 1, c, c', hlt, h1, h2 => by
    rw [List.getD_cons_succ] at h1 h2
    rw [List.flatten_cons]
    exact (pair_getD σ k1 k2 c c' (by omega) h1 h2).trans (List.sublist_append_right _ _)
  | g :: σ, k1, 0, c, c', hlt, _, _ => by omega

theorem range_map_getD {α : Type} : ∀ (σ : List (List α)), (List.range σ.length).map (fun k => σ.getD k []) = σ
  | [] => rfl
  | g :: σ => by
    rw [List.length_cons, List.range_succ_eq_map, List.map_cons, List.map_map, List.getD_cons_zero]
    congr 1
    have : ((fun k => (g :: σ).getD k []) ∘ Nat.succ) = (fun k => σ.getD k []) := by
      funext k; simp [List.getD_cons_succ]
    rw [this, range_map_getD σ]

def maxSucc : List Nat → Nat
  | [] => 0
  | k :: l => max (k + 1) (maxSucc l)

theorem le_maxSucc : ∀ (l : List Nat) (k : Nat), k ∈ l → k + 1 ≤ maxSucc l
  | [], k, h => by simp at h
  | a :: l, k, h => by
    simp only [maxSucc]
    rcases List.mem_cons.mp h with rfl | h
    · omega
    · have := le_maxSucc l k h; omega

theorem maxSucc_spec : ∀ (l : List Nat), maxSucc l = 0 ∨ ∃ k ∈ l, maxSucc l = k + 1
  | [] => .inl rfl
  | a :: l => by
    simp only [maxSucc]
    rcases maxSucc_spec l with h | ⟨k, hk, h⟩
    · exact .inr ⟨a, List.mem_cons_self .., by omega⟩
    · by_cases hle : k + 1 ≤ a + 1
      · exact .inr ⟨a, List.mem_cons_self .., by omega⟩
      · exact .inr ⟨k, List.mem_cons_of_mem _ hk, by omega⟩

theorem maxSucc_le : ∀ (l : List Nat) (K : Nat), (∀ k ∈ l, k < K) → maxSucc l ≤ K
  | [], K, _ => Nat.zero_le _
  | a :: l, K, h => by
    simp only [maxSucc]
    have h1 := h a (List.mem_cons_self ..)
    have h2 := maxSucc_le l K (fun k hk => h k (List.mem_cons_of_mem _ hk))
    omega

theorem pair_flatten_map {α β : Type} (f : α → List β) {e1 e2 : α} {x y : β} :
    ∀ {L : List α}, [e1, e2] <+ L → x ∈ f e1 → y ∈ f e2 → [x, y] <+ (L.map f).flatten
  | [], h, _, _ => by simp at h
  | e :: L, h, hx, hy => by
    rw [List.map_cons, List.flatten_cons]
    rcases List.sublist_cons_iff.mp h with h | ⟨r, e', h⟩
    · exact (pair_flatten_map f h hx hy).trans (List.sublist_append_right _ _)
    · simp only [List.cons.injEq] at e'
      obtain ⟨rfl, rfl⟩ := e'
      have he2 : e2 ∈ L := List.singleton_sublist.mp h
      exact List.Sublist.append (List.singleton_sublist.mpr hx)
        (List.singleton_sublist.mpr (List.mem_flatten.mpr ⟨f e2, List.mem_map.mpr ⟨e2, he2, rfl⟩, hy⟩))

theorem pair_sub_two {α : Type} {a b c d : α} (h : [a, b] <+ [c, d]) : a = c ∧ b = d := by
  rcases List.sublist_cons_iff.mp h with h | ⟨r, e, h⟩
  · have := h.length_le; simp at this
  · simp only [List.cons.injEq] at e
    obtain ⟨rfl, rfl⟩ := e
    exact ⟨rfl, List.singleton_sublist.mp h |> List.mem_singleton.mp⟩

theorem pair_sub_single {α : Type} {a b c : α} : ¬ [a, b] <+ [c] := fun h => by
  have := h.length_le; simp at this

theorem pair_sub_nil {α : Type} {a b : α} : ¬ [a, b] <+ ([] : List α) := fun h => by
  have := h.length_le; simp at this

end Lists

/-! ## Primitive contracts

Each primitive is modelled from its Verilog in `src/Verilog`, as the
hardware relation `hw`, next to its one-at-a-time semantics; its contract is
proved for every legal call sequence. -/

section Prim

theorem lastArg_mem {M : Type} [DecidableEq M] (m : M) :
    ∀ (l : List (M × V)) (x : V), lastArg m l = some x → (m, x) ∈ l
  | [], x, h => by simp [lastArg] at h
  | c :: l, x, h => by
    simp only [lastArg] at h
    split at h
    · rename_i y hy
      simp only [Option.some.injEq] at h
      subst h
      exact List.mem_cons_of_mem _ (lastArg_mem m l y hy)
    · split at h
      · rename_i hc
        simp only [Option.some.injEq] at h
        subst h hc
        exact List.mem_cons_self ..
      · simp at h

theorem lastArg_none {M : Type} [DecidableEq M] (m : M) :
    ∀ (l : List (M × V)), lastArg m l = none → ∀ x, (m, x) ∉ l
  | [], _, x, h => by simp at h
  | c :: l, h, x, hx => by
    simp only [lastArg] at h
    split at h
    · simp at h
    · rename_i hn
      split at h
      · simp at h
      · rename_i hc
        rcases List.mem_cons.mp hx with e | e
        · exact hc (by rw [← e])
        · exact lastArg_none m l hn x e

/-- A module without rules runs its groups as one list of calls. -/
theorem exec_map_inl {X : Iface} (Mo : Module V X) (o : Outs X.M V) :
    ∀ (σ : List (List (X.M × V))) (s t : Mo.S), ExecG Mo o s σ.flatten t → Exec Mo o s (σ.map .inl) t
  | [], s, t, h => by
    have := execG_nil_eq Mo o h
    subst this; exact .nil _
  | g :: σ, s, t, h => by
    rw [List.flatten_cons] at h
    obtain ⟨u, h1, h2⟩ := execG_append Mo o _ _ h
    exact .grp h1 (exec_map_inl Mo o σ u t h2)

theorem sub_pair_left {α : Type} {p q : List α} {c d : α} (h : c ∈ p) : [c, d] <+ p ++ d :: q :=
  List.Sublist.append (List.singleton_sublist.mpr h) (List.cons_sublist_cons.mpr (List.nil_sublist _))

theorem sub_pair_right {α : Type} {p q : List α} {c d : α} (h : c ∈ q) : [d, c] <+ p ++ d :: q :=
  (List.cons_sublist_cons.mpr (List.singleton_sublist.mpr h)).trans (List.sublist_append_right _ _)

/-- Splitting a filter by a disjunction whose second part never precedes the first. -/
theorem filter_split {α : Type} (p q r : α → Bool) (hr : ∀ a, r a = (p a || q a))
    (hpq : ∀ a, p a = true → q a = true → False) :
    ∀ (L : List α), (∀ a b, [a, b] <+ L → q a = true → p b = true → False) →
      L.filter r = L.filter p ++ L.filter q
  | [], _ => rfl
  | a :: L, h => by
    have ih := filter_split p q r hr hpq L (fun x y hxy => h x y (hxy.trans (List.sublist_cons_self _ _)))
    by_cases hp : p a = true
    · have hq : q a = false := by
        cases e : q a
        · rfl
        · exact absurd (hpq a hp e) id
      simp [List.filter_cons, hr, hp, hq, ih]
    · have hp' : p a = false := by simpa using hp
      by_cases hq : q a = true
      · have hnil : L.filter p = [] := by
          rw [List.filter_eq_nil_iff]
          intro b hb hpb
          exact h a b (List.cons_sublist_cons.mpr (List.singleton_sublist.mpr hb)) hq (by simpa using hpb)
        simp [List.filter_cons, hr, hp', hq, ih, hnil]
      · have hq' : q a = false := by simpa using hq
        simp [List.filter_cons, hr, hp', hq', ih]

/-! ### CReg n (and Reg) -/

/-- The ports of an n-port CReg (`CRegN5.v`): port `i`'s read and write. -/
inductive CRegM (n : Nat) where
  | rd (i : Fin n)
  | wr (i : Fin n)
  deriving DecidableEq

namespace CRegM
def port {n : Nat} : CRegM n → Nat
  | rd i => i.val
  | wr i => i.val
def isWr {n : Nat} : CRegM n → Bool
  | rd _ => false
  | wr _ => true
@[simp] theorem port_rd {n : Nat} (i : Fin n) : (rd i).port = i.val := rfl
@[simp] theorem port_wr {n : Nat} (i : Fin n) : (wr i).port = i.val := rfl
@[simp] theorem isWr_rd {n : Nat} (i : Fin n) : (rd i).isWr = false := rfl
@[simp] theorem isWr_wr {n : Nat} (i : Fin n) : (wr i).isWr = true := rfl
end CRegM

/-- bsc's annotations for a CReg: on one port, read CF read, read SB write,
    write SBR write; everything on a lower port SBR everything on a higher one.
    No ready signals; only writes have enables.  Paths: port `k`'s write to
    port `j`'s read for `k < j` (`CRegN5.v`'s `Q_OUT` chain). -/
def cregAnn (n : Nat) : Ann (CRegM n) where
  prec p q := decide (p.port < q.port) || (decide (p.port = q.port) && !p.isWr && q.isWr)
  sbr p q := decide (p.port ≠ q.port) || (p.isWr && q.isWr)
  conf _ _ := false
  share m := !m.isWr
  hasEn m := m.isWr
  alwaysEn _ := false
  alwaysRdy _ := true
  path p q := p.isWr && !q.isWr && decide (p.port < q.port)

abbrev CReg.X (n : Nat) : Iface := ⟨CRegM n, inferInstance, cregAnn n⟩

/-- The value port `k` reads (`Q_OUT_k`): the register with the writes of the
    ports below `k` applied, `Q_OUT_{k+1} = EN_k ? D_IN_k : Q_OUT_k`. -/
def cval {n : Nat} (s : V) (w : Wires (CRegM n) V) : Nat → V
  | 0 => s
  | k + 1 => if h : k < n then (if w.en (.wr ⟨k, h⟩) then w.arg (.wr ⟨k, h⟩) else cval s w k) else cval s w k

/-- An n-port CReg.  Atomically a register; in hardware, `CRegN5.v`'s chain
    of muxes, and the next state is `Q_OUT_n`. -/
@[reducible] def CReg (n : Nat) : Module V (CReg.X n) where
  S := V
  R := Empty
  meth := fun (m : CRegM n) x s v s' => match m with
    | .rd _ => v = s ∧ s' = s
    | .wr _ => s' = x
  rule := fun r => r.elim
  clean s := s
  clean_idem _ := rfl
  Sig := Unit
  hw s w o s' _ := (∀ i : Fin n, o.ret (.rd i) = cval s w i) ∧ s' = cval s w n
  ok _ := True

/-- The register after the writes of a list of calls. -/
def foldW {n : Nat} (t : V) : List (CRegM n × V) → V
  | [] => t
  | (.wr _, x) :: l => foldW x l
  | (.rd _, _) :: l => foldW t l

theorem foldW_append {n : Nat} : ∀ (t : V) (l1 l2 : List (CRegM n × V)), foldW t (l1 ++ l2) = foldW (foldW t l1) l2
  | t, [], l2 => rfl
  | t, (.wr _, x) :: l1, l2 => foldW_append x l1 l2
  | t, (.rd _, _) :: l1, l2 => foldW_append t l1 l2

theorem foldW_filter {n : Nat} : ∀ (t : V) (l : List (CRegM n × V)), foldW t (l.filter (fun c => c.1.isWr)) = foldW t l
  | t, [] => rfl
  | t, (.wr _, x) :: l => by
    rw [List.filter_cons_of_pos (by rfl)]; exact foldW_filter x l
  | t, (.rd _, _) :: l => by
    rw [List.filter_cons_of_neg (by simp)]; exact foldW_filter t l

theorem foldW_only {n : Nat} (k : Fin n) : ∀ (t : V) (l : List (CRegM n × V)),
    foldW t (l.filter (fun c => decide (c.1 = .wr k))) = (lastArg (.wr k) l).getD t
  | t, [] => rfl
  | t, (m, x) :: l => by
    have ih := foldW_only k x l
    have ih' := foldW_only k t l
    by_cases hm : m = .wr k
    · subst hm
      simp only [List.filter_cons, decide_true, ite_true, foldW, lastArg]
      rw [ih]
      cases lastArg (.wr k) l <;> rfl
    · simp only [List.filter_cons, hm, decide_false, Bool.false_eq_true, ite_false, lastArg]
      rw [ih']
      cases lastArg (.wr k) l <;> simp [hm]

theorem execG_creg {n : Nat} (o : Outs (CRegM n) V) :
    ∀ (L : List (CRegM n × V)) (t : V),
      (∀ p (i : Fin n) x q, L = p ++ (.rd i, x) :: q → o.ret (.rd i) = foldW t p) →
      ExecG (CReg (V := V) n) o t L (foldW t L)
  | [], t, _ => .nil _
  | (.rd i, x) :: L, t, h => by
    have h0 := h [] i x L rfl
    refine .cons (s1 := t) ⟨h0, rfl⟩ ?_
    exact execG_creg o L t (fun p i' x' q e => by
      have := h ((.rd i, x) :: p) i' x' q (by rw [e]; rfl)
      simpa [foldW] using this)
  | (.wr i, x) :: L, t, h => by
    have hm : (CReg (V := V) n).meth (.wr i) x t (o.ret (.wr i)) x := rfl
    refine .cons hm ?_
    exact execG_creg o L x (fun p i' x' q e => by
      have := h ((.wr i, x) :: p) i' x' q (by rw [e]; rfl)
      simpa [foldW] using this)

/-- The writes before port `k`, in call order. -/
def wlt {n : Nat} (k : Nat) (c : CRegM n × V) : Bool := c.1.isWr && decide (c.1.port < k)

section CRegFacts
variable {n : Nat} {σ : List (List (CRegM n × V))} {w : Wires (CRegM n) V}

theorem creg_order (hL : Legal (cregAnn n) σ) {c1 c2 : CRegM n × V} (h : [c1, c2] <+ σ.flatten) :
    c1.1 ≠ c2.1 → ¬ (c2.1.port < c1.1.port) ∧ ¬ (c2.1.port = c1.1.port ∧ c2.1.isWr = false ∧ c1.1.isWr = true) := by
  intro hne
  have := (hL.order c1 c2 h).1 hne
  simp only [cregAnn, Bool.or_eq_false_iff, decide_eq_false_iff_not, Bool.and_eq_false_iff,
    Bool.not_eq_false'] at this
  refine ⟨this.1, ?_⟩
  rintro ⟨e1, e2, e3⟩
  rcases this.2 with (h1 | h1) | h1
  · exact h1 e1
  · rw [e2] at h1; cases h1
  · rw [e3] at h1; cases h1

theorem cval_eq (hL : Legal (cregAnn n) σ) (hD : Drives (cregAnn n) w σ) (s : V) :
    ∀ k, k ≤ n → cval s w k = foldW s (σ.flatten.filter (wlt k))
  | 0, _ => by
    have : σ.flatten.filter (wlt (n := n) (V := V) 0) = [] := by
      rw [List.filter_eq_nil_iff]; intro c _; simp [wlt]
    rw [this]; rfl
  | k + 1, hk => by
    have hkn : k < n := by omega
    have ih := cval_eq hL hD s k (by omega)
    have hsplit : σ.flatten.filter (wlt (k + 1)) =
        σ.flatten.filter (wlt k) ++ σ.flatten.filter (fun c => decide (c.1 = .wr ⟨k, hkn⟩)) := by
      apply filter_split
      · intro c
        rcases c with ⟨m, x⟩
        cases m with
        | rd j => simp [wlt]
        | wr j =>
          simp only [wlt, CRegM.isWr_wr, CRegM.port_wr, Bool.true_and, CRegM.wr.injEq]
          by_cases hj : j.val = k
          · have : j = ⟨k, hkn⟩ := Fin.ext hj
            simp [this]
          · have : j ≠ ⟨k, hkn⟩ := fun e => hj (by rw [e])
            simp only [this, decide_false, Bool.or_false]
            congr 1; apply propext; omega
      · intro c h1 h2
        simp only [wlt, decide_eq_true_eq, Bool.and_eq_true] at h1 h2
        rw [h2] at h1; simp at h1
      · intro a b hab ha hb
        simp only [decide_eq_true_eq] at ha
        simp only [wlt, Bool.and_eq_true, decide_eq_true_eq] at hb
        have hne : a.1 ≠ b.1 := by
          intro e; rw [← e, ha] at hb; simp at hb
        have := (creg_order hL hab hne).1
        rw [ha, CRegM.port_wr] at this
        simp only [Fin.val_mk] at this; omega
    rw [hsplit, foldW_append, ← ih, foldW_only]
    simp only [cval, hkn, dite_true]
    cases hl : lastArg (CRegM.wr ⟨k, hkn⟩) σ.flatten with
    | some x =>
      have hen : w.en (.wr ⟨k, hkn⟩) = true := ((hD.en _ rfl).mpr ⟨x, lastArg_mem _ _ x hl⟩)
      simp [hen, hD.arg _ x hl]
    | none =>
      have hen : w.en (.wr ⟨k, hkn⟩) = false := by
        cases e : w.en (.wr ⟨k, hkn⟩)
        · rfl
        · obtain ⟨x, hx⟩ := (hD.en _ rfl).mp e
          exact absurd hx (lastArg_none _ _ hl x)
      simp [hen]

theorem creg_read (hL : Legal (cregAnn n) σ) (hD : Drives (cregAnn n) w σ) (s : V)
    (p q : List (CRegM n × V)) (i : Fin n) (x : V) (e : σ.flatten = p ++ (.rd i, x) :: q) :
    foldW s p = cval s w i := by
  rw [cval_eq hL hD s i (Nat.le_of_lt i.isLt), e, List.filter_append, List.filter_cons]
  have hq : q.filter (wlt i) = [] := by
    rw [List.filter_eq_nil_iff]
    intro c hc
    rcases c with ⟨m, y⟩
    cases m with
    | rd j => simp [wlt]
    | wr j =>
      have hs := sub_pair_right (p := p) (d := (CRegM.rd i, x)) hc
      rw [← e] at hs
      have := (creg_order hL hs (by simp)).1
      simp at this
      simp only [wlt, CRegM.isWr_wr, CRegM.port_wr, Bool.true_and, Bool.not_eq_true]
      exact decide_eq_false (by omega)
  have hp : p.filter (wlt i) = p.filter (fun c => c.1.isWr) := by
    apply List.filter_congr
    intro c hc
    rcases c with ⟨m, y⟩
    cases m with
    | rd j => simp [wlt]
    | wr j =>
      have hs := sub_pair_left (q := q) (d := (CRegM.rd i, x)) hc
      rw [← e] at hs
      have := creg_order hL hs (by simp)
      simp at this
      simp only [wlt, CRegM.isWr_wr, CRegM.port_wr, Bool.true_and]
      exact decide_eq_true (by omega)
  simp only [wlt, CRegM.isWr_rd, Bool.false_and, Bool.false_eq_true, ite_false, hq, List.append_nil]
  rw [hp, foldW_filter]

theorem creg_final (hL : Legal (cregAnn n) σ) (hD : Drives (cregAnn n) w σ) (s : V) :
    foldW s σ.flatten = cval s w n := by
  rw [cval_eq hL hD s n (Nat.le_refl n)]
  have : σ.flatten.filter (wlt n) = σ.flatten.filter (fun c => c.1.isWr) := by
    apply List.filter_congr
    intro c _
    rcases c with ⟨m, y⟩
    cases m with
    | rd j => simp [wlt]
    | wr j => simp [wlt, j.isLt]
  rw [this, foldW_filter]

end CRegFacts

/-- **CReg n realizes its contract.** -/
theorem creg_realizes (n : Nat) : Realizes (CReg (V := V) n) := by
  intro s σ w o s' _ _ hL hD hhw _ _
  obtain ⟨hret, hs'⟩ := hhw
  refine ⟨σ.map .inl, foldW s σ.flatten, groups_map_inl σ, ?_, ?_, fun l => nomatch l⟩
  · apply exec_map_inl
    apply execG_creg o σ.flatten s
    intro p i x q e
    rw [hret i]
    exact (creg_read hL hD s p q i x e).symm
  · exact hs'.trans (creg_final hL hD s).symm

/-- `Q_OUT_k` depends only on the writes of the ports below `k`. -/
theorem cval_agree {n : Nat} (s : V) (w w' : Wires (CRegM n) V) :
    ∀ k, (∀ i : Fin n, i.val < k → AgreeW w w' (.wr i)) → cval s w k = cval s w' k
  | 0, _ => rfl
  | k + 1, h => by
    have ih := cval_agree s w w' k (fun i hi => h i (by omega))
    by_cases hk : k < n
    · obtain ⟨h1, h2⟩ := h ⟨k, hk⟩ (by simp)
      simp only [cval, hk, dite_true, h1, h2, ih]
    · simp only [cval, hk, dite_false, ih]

/-- A CReg's outputs: `Q_OUT_k` on port `k`'s read. -/
def cregOut {n : Nat} (s : V) (w : Wires (CRegM n) V) : Outs (CRegM n) V :=
  ⟨fun _ => true, fun m => match m with | .rd i => cval s w i | .wr _ => s⟩

/-- **CReg n is causal**, with paths from lower ports' writes to higher
    ports' reads. -/
theorem creg_causal (n : Nat) : Causal (CReg (V := V) n) :=
  ⟨cregOut, fun s w w' q hq => by
    cases q with
    | rd i => exact ⟨rfl, cval_agree s w w' i (fun i' hi => hq (.wr i') (by simp [cregAnn, hi]))⟩
    | wr i => exact ⟨rfl, rfl⟩,
   fun s w _ => ⟨cval s w n, (), fun _ => rfl, rfl⟩⟩

/-- A register (`RegN.v`) is a 1-port CReg: `read CF read`, `read SB write`,
    `write SBR write`. -/
abbrev Reg.X : Iface := CReg.X 1
abbrev Reg : Module V Reg.X := CReg 1
abbrev Reg.read : CRegM 1 := .rd 0
abbrev Reg.write : CRegM 1 := .wr 0

theorem reg_realizes : Realizes (Reg (V := V)) := creg_realizes 1

/-- The same register exporting the stricter `write C write`. -/
def regAnnC : Ann (CRegM 1) := { cregAnn 1 with conf := fun p q => p.isWr && q.isWr }

theorem reg_realizes_C : Realizes ((Reg (V := V)).reann regAnnC) := by
  apply Realizes.mono Reg regAnnC ?_ ?_ (fun _ => rfl) reg_realizes
  · intro σ h
    refine ⟨fun c1 c2 hs => ?_, fun g hg c1 c2 hs => ?_, h.always⟩
    · have := h.order c1 c2 hs
      exact ⟨this.1, rfl, rfl, this.2.2.2⟩
    · have := h.group g hg c1 c2 hs
      exact ⟨⟨this.1.1, rfl, rfl, this.1.2.2.2⟩, this.2.1, this.2.2⟩
  · intro w σ h; exact ⟨h.en, h.arg⟩

/-! ### Restricting a module to some of its ports -/

/-- `Mo` seen through a map of ports `f`: the parent may call only the ports
    in `f`'s image.  The other ports' enables are low, and their arguments
    and outputs are not observed.  A primitive with fewer ports than its
    model (bsc's `RWire0` against the wire model) is the model restricted. -/
def Module.restrict {X : Iface} (Mo : Module V X) (X' : Iface) (f : X'.M → X.M) : Module V X' where
  S := Mo.S
  R := Mo.R
  meth m x s v s' := Mo.meth (f m) x s v s'
  rule := Mo.rule
  clean := Mo.clean
  clean_idem := Mo.clean_idem
  Sig := Mo.Sig
  hw s w o s' g := ∃ (wX : Wires X.M V) (oX : Outs X.M V), Mo.hw s wX oX s' g ∧
    (∀ m, wX.en (f m) = w.en m) ∧ (∀ m, wX.arg (f m) = w.arg m) ∧
    (∀ p, (∀ m, f m ≠ p) → wX.en p = false) ∧
    (∀ m, o.rdy m = oX.rdy (f m)) ∧ (∀ m, o.ret m = oX.ret (f m))
  ok := Mo.ok
  L := Mo.L
  log := Mo.log
  trc := Mo.trc
  log_clean := Mo.log_clean

/-- `X'` is `X` restricted along `f`, with annotations at least as strict. -/
structure Restricts (X X' : Iface) (f : X'.M → X.M) : Prop where
  inj : ∀ a b, f a = f b → a = b
  prec : ∀ a b, X.ann.prec (f a) (f b) = true → X'.ann.prec a b = true
  sbr : ∀ a b, X.ann.sbr (f a) (f b) = true → X'.ann.sbr a b = true
  conf : ∀ a b, X.ann.conf (f a) (f b) = true → X'.ann.conf a b = true
  share : ∀ a, X.ann.share (f a) = true → X'.ann.share a = true
  hasEn : ∀ a, X'.ann.hasEn a = X.ann.hasEn (f a)
  alwaysEn : ∀ a, X'.ann.alwaysEn a = X.ann.alwaysEn (f a)
  alwaysRdy : ∀ a, X'.ann.alwaysRdy a = true → X.ann.alwaysRdy (f a) = true
  outside : ∀ p, (∀ m, f m ≠ p) → X.ann.alwaysEn p = false
  path : ∀ a b, X.ann.path (f a) (f b) = true → X'.ann.path a b = true

section Restrict
variable {X X' : Iface} {f : X'.M → X.M}

/-- A call of `X'`'s port, as a call of `X`'s. -/
def liftC (f : X'.M → X.M) (c : X'.M × V) : X.M × V := (f c.1, c.2)

theorem lastArg_map (hinj : ∀ a b, f a = f b → a = b) (m : X'.M) :
    ∀ (l : List (X'.M × V)), lastArg (f m) (l.map (liftC f)) = lastArg m l
  | [] => rfl
  | c :: l => by
    have ih := lastArg_map hinj m l
    show (match lastArg (f m) (l.map (liftC f)) with
      | some y => some y
      | none => if (liftC f c).1 = f m then some (liftC f c).2 else none) =
      (match lastArg m l with
      | some y => some y
      | none => if c.1 = m then some c.2 else none)
    rw [ih]
    cases lastArg m l with
    | some y => rfl
    | none =>
      simp only [liftC]
      by_cases h : c.1 = m
      · simp [h]
      · have : f c.1 ≠ f m := fun e => h (hinj _ _ e)
        simp [h, this]

theorem flatten_map_map {α β : Type} (g : α → β) :
    ∀ (L : List (List α)), (L.map (List.map g)).flatten = L.flatten.map g
  | [] => rfl
  | l :: L => by simp only [List.map_cons, List.flatten_cons, List.map_append, flatten_map_map g L]

theorem execG_restrict (Mo : Module V X) (o : Outs X'.M V) (oX : Outs X.M V)
    (ho : ∀ m, o.ret m = oX.ret (f m)) :
    ∀ (g : List (X'.M × V)) (s t : Mo.S), ExecG Mo oX s (g.map (liftC f)) t → ExecG (Mo.restrict X' f) o s g t
  | [], s, t, h => by cases h; exact .nil _
  | (m, x) :: g, s, t, h => by
    rw [List.map_cons] at h
    cases h with
    | cons h1 h2 =>
      rename_i s1
      have h1' : (Mo.restrict X' f).meth m x s (o.ret m) s1 := by
        show Mo.meth (f m) x s (o.ret m) s1
        rw [ho]; exact h1
      exact .cons h1' (execG_restrict Mo o oX ho g s1 t h2)

theorem exec_restrict (Mo : Module V X) (o : Outs X'.M V) (oX : Outs X.M V)
    (ho : ∀ m, o.ret m = oX.ret (f m)) :
    ∀ (τ : List (List (X.M × V) ⊕ Mo.R)) (σ' : List (List (X'.M × V))) (s t : Mo.S),
      groups τ = σ'.map (List.map (liftC f)) → Exec Mo oX s τ t →
      ∃ τ' : List (List (X'.M × V) ⊕ (Mo.restrict X' f).R), groups τ' = σ' ∧
        Exec (Mo.restrict X' f) o s τ' t
  | [], σ', s, t, hg, h => by
    cases h
    cases σ' with
    | nil => exact ⟨[], rfl, .nil _⟩
    | cons _ _ => simp [groups] at hg
  | .inl g :: τ, σ', s, t, hg, h => by
    cases h with
    | grp h1 h2 =>
      cases σ' with
      | nil => simp [groups] at hg
      | cons g' σ'' =>
        simp only [groups, List.map_cons, List.cons.injEq] at hg
        obtain ⟨rfl, hg⟩ := hg
        obtain ⟨τ', h3, h4⟩ := exec_restrict Mo o oX ho τ σ'' _ t hg h2
        exact ⟨.inl g' :: τ', by simp [groups, h3], .grp (execG_restrict Mo o oX ho g' _ _ h1) h4⟩
  | .inr r :: τ, σ', s, t, hg, h => by
    cases h with
    | rule h1 h2 =>
      obtain ⟨τ', h3, h4⟩ := exec_restrict Mo o oX ho τ σ' _ t hg h2
      exact ⟨.inr r :: τ', by simp [groups, h3], .rule h1 h4⟩

/-- **A module restricted to some of its ports, with annotations at least as
    strict, still realizes its contract.** -/
theorem Realizes.restrict (Mo : Module V X) (hX : Restricts X X' f) (h : Realizes Mo) :
    Realizes (Mo.restrict X' f) := by
  intro s σ' w o s' g hc hL hD hhw hok hrdy
  obtain ⟨wX, oX, hX', hen, harg, hout, hrd, hret⟩ := hhw
  obtain ⟨σ, hσ⟩ : ∃ σ : List (List (X.M × V)), σ = σ'.map (List.map (liftC f)) := ⟨_, rfl⟩
  have hfl : σ.flatten = σ'.flatten.map (liftC f) := by rw [hσ]; exact flatten_map_map _ σ'
  have hpre : ∀ c ∈ σ.flatten, ∃ a ∈ σ'.flatten, c = liftC f a := by
    intro c hc; rw [hfl] at hc; obtain ⟨a, ha, rfl⟩ := List.mem_map.mp hc; exact ⟨a, ha, rfl⟩
  have fol : ∀ a b : X'.M × V, X'.ann.Follow a b → X.ann.Follow (liftC f a) (liftC f b) := by
    intro a b hf
    show (f a.1 ≠ f b.1 → X.ann.prec (f b.1) (f a.1) = false) ∧ X.ann.conf (f a.1) (f b.1) = false ∧
      X.ann.conf (f b.1) (f a.1) = false ∧ (f a.1 = f b.1 → X.ann.share (f a.1) = true → a.2 = b.2)
    refine ⟨fun hne => ?_, ?_, ?_, fun he hs => ?_⟩
    · have hab : a.1 ≠ b.1 := fun e => hne (by rw [e])
      cases e : X.ann.prec (f b.1) (f a.1)
      · rfl
      · have := hf.1 hab; rw [hX.prec _ _ e] at this; cases this
    · cases e : X.ann.conf (f a.1) (f b.1)
      · rfl
      · have := hf.2.1; rw [hX.conf _ _ e] at this; cases this
    · cases e : X.ann.conf (f b.1) (f a.1)
      · rfl
      · have := hf.2.2.1; rw [hX.conf _ _ e] at this; cases this
    · exact hf.2.2.2 (hX.inj _ _ he) (hX.share _ hs)
  have grp : ∀ a b : X'.M × V, X'.ann.Group a b → X.ann.Group (liftC f a) (liftC f b) := by
    intro a b hg
    show X.ann.Follow (liftC f a) (liftC f b) ∧ X.ann.sbr (f a.1) (f b.1) = false ∧
      X.ann.sbr (f b.1) (f a.1) = false
    refine ⟨fol a b hg.1, ?_, ?_⟩
    · cases e : X.ann.sbr (f a.1) (f b.1)
      · rfl
      · have := hg.2.1; rw [hX.sbr _ _ e] at this; cases this
    · cases e : X.ann.sbr (f b.1) (f a.1)
      · rfl
      · have := hg.2.2; rw [hX.sbr _ _ e] at this; cases this
  have hL' : Legal X.ann σ := by
    refine ⟨fun c1 c2 hs => ?_, fun g hg c1 c2 hs => ?_, fun p hp => ?_⟩
    · rw [hfl] at hs
      obtain ⟨a, b, hab, rfl, rfl⟩ := sublist_map_pair hs
      exact fol a b (hL.order a b hab)
    · rw [hσ] at hg
      obtain ⟨g', hg', rfl⟩ := List.mem_map.mp hg
      obtain ⟨a, b, hab, rfl, rfl⟩ := sublist_map_pair hs
      exact grp a b (hL.group g' hg' a b hab)
    · by_cases him : ∃ m, f m = p
      · obtain ⟨m, rfl⟩ := him
        obtain ⟨x, hx⟩ := hL.always m (by rw [hX.alwaysEn]; exact hp)
        exact ⟨x, by rw [hfl]; exact List.mem_map.mpr ⟨(m, x), hx, rfl⟩⟩
      · have := hX.outside p (fun m e => him ⟨m, e⟩); rw [this] at hp; cases hp
  have hD' : Drives X.ann wX σ := by
    refine ⟨fun p hp => ?_, fun p x hx => ?_⟩
    · by_cases him : ∃ m, f m = p
      · obtain ⟨m, rfl⟩ := him
        rw [hen m, hD.en m (by rw [hX.hasEn]; exact hp), hfl]
        constructor
        · rintro ⟨x, hx⟩; exact ⟨x, List.mem_map.mpr ⟨(m, x), hx, rfl⟩⟩
        · rintro ⟨x, hx⟩
          obtain ⟨⟨m', x'⟩, hm', e⟩ := List.mem_map.mp hx
          simp only [liftC, Prod.mk.injEq] at e
          obtain ⟨e1, rfl⟩ := e
          exact ⟨x', by rw [← hX.inj _ _ e1]; exact hm'⟩
      · rw [hout p (fun m e => him ⟨m, e⟩)]
        simp only [Bool.false_eq_true, false_iff, not_exists]
        intro x hx
        obtain ⟨a, _, e⟩ := hpre _ hx
        exact him ⟨a.1, by simp only [liftC, Prod.mk.injEq] at e; exact e.1.symm⟩
    · have hm := lastArg_mem p _ x hx
      obtain ⟨a, _, e⟩ := hpre _ hm
      simp only [liftC, Prod.mk.injEq] at e
      obtain ⟨rfl, -⟩ := e
      rw [hfl, lastArg_map hX.inj] at hx
      rw [harg]; exact hD.arg _ x hx
  have hrdy' : ∀ c ∈ σ.flatten, X.ann.alwaysRdy c.1 = false → oX.rdy c.1 = true := by
    intro c hc hn
    obtain ⟨a, ha, rfl⟩ := hpre c hc
    have hn' : X'.ann.alwaysRdy a.1 = false := by
      cases e : X'.ann.alwaysRdy a.1
      · rfl
      · have := hX.alwaysRdy _ e; simp only [liftC] at hn; rw [this] at hn; cases hn
    have := hrdy a ha hn'; rw [hrd] at this; exact this
  obtain ⟨τ, fin, hg, hex, hs', htr⟩ := h s σ wX oX s' g hc hL' hD' hX' hok hrdy'
  obtain ⟨τ', hg', hex'⟩ := exec_restrict Mo o oX hret τ σ' s fin (hg.trans hσ) hex
  exact ⟨τ', fin, hg', hex', hs', htr⟩

open Classical in
/-- `X'`'s wires as `X`'s: the ports outside `f`'s image idle. -/
noncomputable def liftW [Inhabited V] (f : X'.M → X.M) (w : Wires X'.M V) : Wires X.M V :=
  ⟨fun p => if hp : ∃ m, f m = p then w.en (Classical.choose hp) else false,
   fun p => if hp : ∃ m, f m = p then w.arg (Classical.choose hp) else default⟩

/-- **A restricted causal module is causal**: the ports outside the image
    are held idle. -/
theorem Causal.restrict [Inhabited V] (Mo : Module V X) (hX : Restricts X X' f) (h : Causal Mo) :
    Causal (Mo.restrict X' f) := by
  obtain ⟨out, hc, ht⟩ := h
  have hl : ∀ (w : Wires X'.M V) m, (liftW f w).en (f m) = w.en m ∧ (liftW f w).arg (f m) = w.arg m := by
    intro w m
    have hp : ∃ m', f m' = f m := ⟨m, rfl⟩
    have e := hX.inj _ _ (Classical.choose_spec hp)
    refine ⟨?_, ?_⟩ <;> simp only [liftW, dite_eq_left hp, e]
  have hn : ∀ (w : Wires X'.M V) p, (∀ m, f m ≠ p) → (liftW f w).en p = false ∧ (liftW f w).arg p = default := by
    intro w p hp
    have : ¬ ∃ m, f m = p := fun ⟨m, e⟩ => hp m e
    refine ⟨?_, ?_⟩ <;> simp only [liftW, dite_eq_right this]
  refine ⟨fun s w => ⟨fun m => (out s (liftW f w)).rdy (f m), fun m => (out s (liftW f w)).ret (f m)⟩, ?_, ?_⟩
  · intro s w w' q hq
    have := hc s (liftW f w) (liftW f w') (f q) (fun p hp => by
      by_cases him : ∃ m, f m = p
      · obtain ⟨a, rfl⟩ := him
        obtain ⟨h1, h2⟩ := hq a (hX.path a q hp)
        exact ⟨(hl w a).1.trans (h1.trans (hl w' a).1.symm), (hl w a).2.trans (h2.trans (hl w' a).2.symm)⟩
      · have hp' : ∀ m, f m ≠ p := fun m e => him ⟨m, e⟩
        exact ⟨(hn w p hp').1.trans (hn w' p hp').1.symm, (hn w p hp').2.trans (hn w' p hp').2.symm⟩)
    exact this
  · intro s w hcl
    obtain ⟨s', g, hg⟩ := ht s (liftW f w) hcl
    exact ⟨s', g, liftW f w, out s (liftW f w), hg, fun m => (hl w m).1, fun m => (hl w m).2,
      fun p hp => (hn w p hp).1, fun _ => rfl, fun _ => rfl⟩

/-- A restriction keeps the aliases it declares. -/
theorem Aliased.restrict (toB : V → Bool) (Mo : Module V X) (h : Aliased toB Mo)
    (hs : ∀ a b, X'.ann.same a b = true → X.ann.same (f a) (f b) = true)
    (hr : ∀ a p, X'.ann.rdyRet a p = true → X.ann.rdyRet (f a) (f p) = true) :
    Aliased toB (Mo.restrict X' f) := by
  intro s w o s' g hc hhw
  obtain ⟨wX, oX, hX', -, -, -, hrd, hret⟩ := hhw
  obtain ⟨h1, h2⟩ := h s wX oX s' g hc hX'
  exact ⟨fun a b hab => by rw [hret, hret]; exact h1 _ _ (hs a b hab),
    fun a p hap => by rw [hrd, hret]; exact h2 _ _ (hr a p hap)⟩

end Restrict

/-! ### The wire family (RWire, RWire0/PulseWire, BypassWire and variants) -/

/-- A wire's ports: `wset` (action), `wget` and `whas` (values). -/
inductive WireM where
  | wset | wget | whas
  deriving DecidableEq

/-- The most permissive annotations the wire model realizes: `wset` before
    the reads (SB: one caller may set and then read), `wset` any number of
    times (the last call's value is read), reads CF reads.  `always`: `wset`
    has no enable and is called every cycle (`(*inhigh*)`).  bsc's imports
    (`PreludeBSV.bsv`) export stricter annotations, and some fewer ports;
    each is this model restricted (`Realizes.restrict`), below. -/
def wireAnn (always : Bool) : Ann WireM where
  prec p q := p == .wset && q != .wset
  sbr _ _ := false
  conf _ _ := false
  share m := m != .wset
  hasEn m := m == .wset && !always
  alwaysEn m := m == .wset && always
  alwaysRdy _ := true
  path p q := p == .wset && q != .wset

abbrev Wire.X (always : Bool) : Iface := ⟨WireM, inferInstance, wireAnn always⟩

/-- The wire model.  Atomically, a value set this cycle (`none` at a cycle's
    start; `wget` of an unset wire is any value); in hardware `RWire.v`
    (`BypassWire.v` when `always`): `WGET = WVAL`, `WHAS = WSET`, no state. -/
@[reducible] def Wire (ofBool : Bool → V) (always : Bool) : Module V (Wire.X always) where
  S := Option V
  R := Empty
  meth := fun (m : WireM) x s v s' => match m with
    | .wset => s' = some x
    | .wget => (∀ d, s = some d → v = d) ∧ s' = s
    | .whas => v = ofBool s.isSome ∧ s' = s
  rule := fun r => r.elim
  clean _ := none
  clean_idem _ := rfl
  Sig := Unit
  hw s w o s' _ := o.ret .wget = w.arg .wset ∧ o.ret .whas = ofBool (always || w.en .wset) ∧ s' = none
  ok _ := True

theorem execG_reads (ofBool : Bool → V) (always : Bool) (o : Outs WireM V) :
    ∀ (q : List (WireM × V)) (t : Option V), (∀ c ∈ q, c.1 ≠ .wset) →
      (∀ d, t = some d → o.ret .wget = d) → o.ret .whas = ofBool t.isSome →
      ExecG (Wire ofBool always) o t q t
  | [], t, _, _, _ => .nil _
  | (m, x) :: q, t, h1, h2, h3 => by
    have ih := execG_reads ofBool always o q t (fun c hc => h1 c (List.mem_cons_of_mem _ hc)) h2 h3
    cases m with
    | wset => exact absurd rfl (h1 _ (List.mem_cons_self ..))
    | wget => exact .cons (s1 := t) ⟨h2, rfl⟩ ih
    | whas => exact .cons (s1 := t) ⟨h3, rfl⟩ ih

theorem execG_wsets (ofBool : Bool → V) (always : Bool) (o : Outs WireM V) :
    ∀ (A : List (WireM × V)) (t : Option V), (∀ c ∈ A, c.1 = .wset) → ∃ u, ExecG (Wire ofBool always) o t A u
  | [], t, _ => ⟨t, .nil _⟩
  | (m, x) :: A, t, h => by
    have hm : m = .wset := h _ (List.mem_cons_self ..)
    subst hm
    obtain ⟨u, hu⟩ := execG_wsets ofBool always o A (some x) (fun c hc => h c (List.mem_cons_of_mem _ hc))
    have h1 : (Wire ofBool always).meth WireM.wset x t (o.ret WireM.wset) (some x) := rfl
    exact ⟨u, .cons h1 hu⟩

/-- The last call of `m` splits a list. -/
theorem lastArg_split {M : Type} [DecidableEq M] (m : M) :
    ∀ (l : List (M × V)) (x : V), lastArg m l = some x → ∃ A B, l = A ++ (m, x) :: B ∧ ∀ c ∈ B, c.1 ≠ m
  | [], x, h => by simp [lastArg] at h
  | c :: l, x, h => by
    simp only [lastArg] at h
    cases hl : lastArg m l with
    | some y =>
      simp only [hl, Option.some.injEq] at h
      subst h
      obtain ⟨A, B, e, hB⟩ := lastArg_split m l y hl
      exact ⟨c :: A, B, by rw [e]; rfl, hB⟩
    | none =>
      simp only [hl] at h
      split at h
      · rename_i hc
        simp only [Option.some.injEq] at h
        subst h
        refine ⟨[], l, ?_, fun c' hc' e => lastArg_none m l hl c'.2 (by rw [← e]; exact hc')⟩
        rcases c with ⟨m', x'⟩
        simp only at hc
        subst hc
        rfl
      · simp at h

/-- **The wire model realizes its contract** (with the permissive annotations). -/
theorem wire_realizes (ofBool : Bool → V) (always : Bool) : Realizes (Wire ofBool always) := by
  intro s σ w o s' _ hc hL hD hhw _ _
  obtain ⟨hget, hhas, hs'⟩ := hhw
  have hs : s = none := hc.symm
  subst hs
  refine ⟨σ.map .inl, ?_⟩
  cases hla : lastArg WireM.wset σ.flatten with
  | some x =>
    obtain ⟨A, B, e, hB⟩ := lastArg_split _ _ x hla
    have hx : (WireM.wset, x) ∈ σ.flatten := by rw [e]; simp
    -- everything before the last `wset` is a `wset`
    have hA : ∀ c ∈ A, c.1 = .wset := by
      intro c hcA
      have hs := sub_pair_left (q := B) (d := (WireM.wset, x)) hcA
      rw [← e] at hs
      have hf := hL.order c (WireM.wset, x) hs
      rcases c with ⟨m, y⟩
      cases m with
      | wset => rfl
      | wget => have h1 := hf.1 (fun h => by cases h); simp [Wire.X, wireAnn] at h1
      | whas => have h1 := hf.1 (fun h => by cases h); simp [Wire.X, wireAnn] at h1
    have hen : (always || w.en .wset) = true := by
      cases always
      · simp only [Bool.false_or]; exact (hD.en _ rfl).mpr ⟨x, hx⟩
      · rfl
    obtain ⟨u, hu⟩ := execG_wsets ofBool always o A none hA
    refine ⟨some x, groups_map_inl σ, exec_map_inl _ _ σ _ _ ?_, hs', fun l => nomatch l⟩
    rw [e]
    have hm : (Wire ofBool always).meth WireM.wset x u (o.ret WireM.wset) (some x) := rfl
    refine execG_append' _ _ _ _ hu (.cons hm ?_)
    apply execG_reads ofBool always o B (some x) hB
    · intro d hd
      simp only [Option.some.injEq] at hd
      rw [hget, hD.arg _ x hla, hd]
    · rw [hhas, hen]; rfl
  | none =>
    have hq : ∀ c ∈ σ.flatten, c.1 ≠ .wset := by
      intro c hcm hc
      exact lastArg_none _ _ hla c.2 (by rw [← hc]; exact hcm)
    have hset : ¬ ∃ x, (WireM.wset, x) ∈ σ.flatten := fun ⟨x, hx⟩ => lastArg_none _ _ hla x hx
    have halw : always = false := by
      cases always
      · rfl
      · obtain ⟨x, hx⟩ := hL.always .wset rfl
        exact absurd ⟨x, hx⟩ hset
    subst halw
    have hen : w.en .wset = false := by
      cases e : w.en .wset
      · rfl
      · exact absurd ((hD.en _ rfl).mp e) hset
    refine ⟨none, groups_map_inl σ, exec_map_inl _ _ σ _ _ ?_, hs', fun l => nomatch l⟩
    apply execG_reads ofBool false o _ none hq
    · intro d hd; cases hd
    · rw [hhas, hen]; rfl

/-- **The wire model is causal**: `WGET = WVAL`, `WHAS = WSET`. -/
theorem wire_causal (ofBool : Bool → V) (always : Bool) : Causal (Wire ofBool always) :=
  ⟨fun _ w => ⟨fun _ => true, fun m => match m with
      | .wget => w.arg .wset | .whas => ofBool (always || w.en .wset) | .wset => ofBool false⟩,
   fun s w w' q hq => by
    cases q with
    | wset => exact ⟨rfl, rfl⟩
    | wget => obtain ⟨-, h2⟩ := hq .wset rfl; exact ⟨rfl, h2⟩
    | whas => obtain ⟨h1, -⟩ := hq .wset rfl; exact ⟨rfl, by simp only [h1]⟩,
   fun s w _ => ⟨none, (), rfl, rfl, rfl⟩⟩

/-! bsc's wire imports (`PreludeBSV.bsv`), each with its exact ports and
schedule, as the wire model restricted:

| import | ports | schedule | here |
|---|---|---|---|
| `vMkRWire` | wset, wget, whas | wset SBR (wget, whas), wset C wset | `RWire ofBool true false` |
| `vMkRWireSBR` | wset, wget, whas | wset SBR (wget, whas), wset SBR wset | `RWire ofBool true true` |
| `vMkUnsafeRWire` | wset, wget, whas | wset SB (wget, whas), wset C wset | `RWire ofBool false false` |
| `vMkUnsafeRWireSBR` | wset, wget, whas | wset SB (wget, whas), wset SBR wset | `RWire ofBool false true` |
| `vMkRWire0` (`mkPulseWire`) | wset (no data), whas | wset SBR whas, wset C wset | `RWire0 ofBool true false` |
| `vMkRWire0SBR` (`mkPulseWireOR`) | wset, whas | wset SBR whas, wset SBR wset | `RWire0 ofBool true true` |
| `vMkUnsafeRWire0`, `…0SBR` | wset, whas | wset SB whas, … | `RWire0 ofBool false _` |
| `vMkBypassWire` | wset (inhigh), wget | wset SBR wget, wset C wset | `BypassWire ofBool` |
| `vMkBypassWire0` | wset (inhigh, no data) | wset C wset | `BypassWire0 ofBool` |
| `vMkUnsafeBypassWire` | wset (inhigh), wget | wset SB wget, wset C wset | `UnsafeBypassWire ofBool` |

A port without data (`RWire0`'s `wset`) still takes an argument in the
model; the hardware ignores it, so any value the parent passes is fine. -/

/-- `vMkRWire` and variants: `sbrRead` (wset SBR, not SB, the reads) and
    `multi` (wset SBR wset, not C). -/
def rwireAnn (sbrRead multi : Bool) : Ann WireM where
  prec p q := p == .wset && q != .wset
  sbr p q := (sbrRead && ((p == .wset && q != .wset) || (q == .wset && p != .wset))) ||
    (multi && p == .wset && q == .wset)
  conf p q := !multi && p == .wset && q == .wset
  share m := m != .wset
  hasEn m := m == .wset
  alwaysEn _ := false
  alwaysRdy _ := true
  path p q := p == .wset && q != .wset

abbrev RWire.X (sbrRead multi : Bool) : Iface := ⟨WireM, inferInstance, rwireAnn sbrRead multi⟩
abbrev RWire (ofBool : Bool → V) (sbrRead multi : Bool) : Module V (RWire.X sbrRead multi) :=
  (Wire ofBool false).restrict (RWire.X sbrRead multi) id

theorem rwire_restricts (sbrRead multi : Bool) : Restricts (Wire.X false) (RWire.X sbrRead multi) id :=
  ⟨fun _ _ h => h,
    fun a b h => by cases a <;> cases b <;> simp_all [wireAnn, rwireAnn],
    fun a b h => by simp [wireAnn] at h, fun a b h => by simp [wireAnn] at h,
    fun a h => by cases a <;> simp_all [wireAnn, rwireAnn],
    fun a => by cases a <;> rfl, fun a => by cases a <;> rfl, fun _ _ => rfl,
    fun p h => absurd rfl (h p), fun a b h => h⟩

theorem rwire_realizes (ofBool : Bool → V) (sbrRead multi : Bool) : Realizes (RWire ofBool sbrRead multi) :=
  Realizes.restrict _ (rwire_restricts sbrRead multi) (wire_realizes ofBool false)

theorem rwire_causal [Inhabited V] (ofBool : Bool → V) (sbrRead multi : Bool) :
    Causal (RWire ofBool sbrRead multi) :=
  Causal.restrict _ (rwire_restricts sbrRead multi) (wire_causal ofBool false)

/-- `vMkRWire0`'s ports: `wset` (no data) and `whas`. -/
inductive Wire0M where
  | wset | whas
  deriving DecidableEq

def Wire0M.toW : Wire0M → WireM
  | .wset => .wset
  | .whas => .whas

def rwire0Ann (sbrRead multi : Bool) : Ann Wire0M where
  prec p q := p == .wset && q == .whas
  sbr p q := (sbrRead && p != q) || (multi && p == .wset && q == .wset)
  conf p q := !multi && p == .wset && q == .wset
  share m := m == .whas
  hasEn m := m == .wset
  alwaysEn _ := false
  alwaysRdy _ := true
  path p q := p == .wset && q == .whas

abbrev RWire0.X (sbrRead multi : Bool) : Iface := ⟨Wire0M, inferInstance, rwire0Ann sbrRead multi⟩
abbrev RWire0 (ofBool : Bool → V) (sbrRead multi : Bool) : Module V (RWire0.X sbrRead multi) :=
  (Wire ofBool false).restrict (RWire0.X sbrRead multi) Wire0M.toW

theorem rwire0_restricts (sbrRead multi : Bool) :
    Restricts (Wire.X false) (RWire0.X sbrRead multi) Wire0M.toW :=
  ⟨fun a b h => by cases a <;> cases b <;> (first | rfl | cases h),
    fun a b h => by cases a <;> cases b <;> simp_all [Wire0M.toW, wireAnn, rwire0Ann],
    fun a b h => by simp [wireAnn] at h, fun a b h => by simp [wireAnn] at h,
    fun a h => by cases a <;> simp_all [Wire0M.toW, wireAnn, rwire0Ann],
    fun a => by cases a <;> rfl, fun a => by cases a <;> rfl, fun _ _ => rfl,
    fun p h => by cases p <;> first | rfl | exact absurd rfl (h .wset) | exact absurd rfl (h .whas),
    fun a b h => by cases a <;> cases b <;> simp_all [Wire0M.toW, wireAnn, rwire0Ann]⟩

theorem rwire0_realizes (ofBool : Bool → V) (sbrRead multi : Bool) :
    Realizes (RWire0 ofBool sbrRead multi) :=
  Realizes.restrict _ (rwire0_restricts sbrRead multi) (wire_realizes ofBool false)

theorem rwire0_causal [Inhabited V] (ofBool : Bool → V) (sbrRead multi : Bool) :
    Causal (RWire0 ofBool sbrRead multi) :=
  Causal.restrict _ (rwire0_restricts sbrRead multi) (wire_causal ofBool false)

/-- `vMkBypassWire`'s ports: `wset` (always enabled) and `wget`. -/
inductive BypM where
  | wset | wget
  deriving DecidableEq

def BypM.toW : BypM → WireM
  | .wset => .wset
  | .wget => .wget

def bypassAnn : Ann BypM where
  prec p q := p == .wset && q == .wget
  sbr p q := p != q
  conf p q := p == .wset && q == .wset
  share m := m == .wget
  hasEn _ := false
  alwaysEn m := m == .wset
  alwaysRdy _ := true
  path p q := p == .wset && q == .wget

abbrev BypassWire.X : Iface := ⟨BypM, inferInstance, bypassAnn⟩
abbrev BypassWire (ofBool : Bool → V) : Module V BypassWire.X :=
  (Wire ofBool true).restrict BypassWire.X BypM.toW

theorem bypass_restricts (A : Ann BypM) (hA : A = bypassAnn ∨ A = { bypassAnn with sbr := fun _ _ => false }) :
    Restricts (Wire.X true) ⟨BypM, inferInstance, A⟩ BypM.toW := by
  rcases hA with rfl | rfl <;>
  exact ⟨fun a b h => by cases a <;> cases b <;> (first | rfl | cases h),
    fun a b h => by cases a <;> cases b <;> simp_all [BypM.toW, wireAnn, bypassAnn],
    fun a b h => by simp [wireAnn] at h, fun a b h => by simp [wireAnn] at h,
    fun a h => by cases a <;> simp_all [BypM.toW, wireAnn, bypassAnn],
    fun a => by cases a <;> rfl, fun a => by cases a <;> rfl, fun _ _ => rfl,
    fun p h => by cases p <;> first | rfl | exact absurd rfl (h .wset) | exact absurd rfl (h .wget),
    fun a b h => by cases a <;> cases b <;> simp_all [BypM.toW, wireAnn, bypassAnn]⟩

theorem bypass_realizes (ofBool : Bool → V) : Realizes (BypassWire ofBool) :=
  Realizes.restrict _ (bypass_restricts _ (.inl rfl)) (wire_realizes ofBool true)

theorem bypass_causal [Inhabited V] (ofBool : Bool → V) : Causal (BypassWire ofBool) :=
  Causal.restrict _ (bypass_restricts _ (.inl rfl)) (wire_causal ofBool true)

/-- `vMkUnsafeBypassWire` (`PreludeBSV.bsv`, `mkUnsafeBypassWire`): `BypassWire.v` with
    `wset SB wget` instead of SBR. -/
def bypassUAnn : Ann BypM := { bypassAnn with sbr := fun _ _ => false }

abbrev UnsafeBypassWire.X : Iface := ⟨BypM, inferInstance, bypassUAnn⟩
abbrev UnsafeBypassWire (ofBool : Bool → V) : Module V UnsafeBypassWire.X :=
  (Wire ofBool true).restrict UnsafeBypassWire.X BypM.toW

theorem ubypass_realizes (ofBool : Bool → V) : Realizes (UnsafeBypassWire ofBool) :=
  Realizes.restrict _ (bypass_restricts _ (.inr rfl)) (wire_realizes ofBool true)

theorem ubypass_causal [Inhabited V] (ofBool : Bool → V) : Causal (UnsafeBypassWire ofBool) :=
  Causal.restrict _ (bypass_restricts _ (.inr rfl)) (wire_causal ofBool true)

/-- `vMkBypassWire0`'s one port: `wset` (always enabled, no data). -/
inductive Byp0M where
  | wset
  deriving DecidableEq

def bypass0Ann : Ann Byp0M where
  prec _ _ := false
  sbr _ _ := false
  conf _ _ := true
  share _ := false
  hasEn _ := false
  alwaysEn _ := true
  alwaysRdy _ := true

abbrev BypassWire0.X : Iface := ⟨Byp0M, inferInstance, bypass0Ann⟩
abbrev BypassWire0 (ofBool : Bool → V) : Module V BypassWire0.X :=
  (Wire ofBool true).restrict BypassWire0.X (fun _ => .wset)

theorem bypass0_restricts : Restricts (Wire.X true) BypassWire0.X (fun _ => .wset) :=
  ⟨fun a b _ => by cases a; cases b; rfl,
    fun a b h => by simp [wireAnn] at h, fun a b h => by simp [wireAnn] at h, fun _ _ _ => rfl,
    fun a h => by simp [wireAnn] at h, fun _ => rfl, fun _ => rfl, fun _ _ => rfl,
    fun p h => by cases p <;> first | rfl | exact absurd rfl (h .wset),
    fun a b h => by simp [wireAnn] at h⟩

theorem bypass0_realizes (ofBool : Bool → V) : Realizes (BypassWire0 ofBool) :=
  Realizes.restrict _ bypass0_restricts (wire_realizes ofBool true)

theorem bypass0_causal [Inhabited V] (ofBool : Bool → V) : Causal (BypassWire0 ofBool) :=
  Causal.restrict _ bypass0_restricts (wire_causal ofBool true)

end Prim

/-! ## The obligation from a certificate

`obl_of_cert` reduces a parent's obligation, which quantifies over every
legal call sequence, to finitely many facts per cycle: per parent method and
per parent rule (guards, results, readiness, the child calls each makes),
per pair of them (their child calls may follow one another, by the
children's annotations), and the enables and argument muxes.  The order of
the parent's steps is built once for all sequences (`placeOrder`): each
rule that fires goes right after the last caller's group that must precede
it (`hit`), and `place_ok` proves, once, that every pair of steps in that
order may follow one another.  This is the parent-level all-orders theorem;
unlike round 4's, it needs no commutation of state, because the children's
contracts already cover every legal order of their calls. -/


/-- A run contains its groups, in order. -/
theorem groups_sublist {α β : Type} : ∀ (τ : List (α ⊕ β)), (groups τ).map .inl <+ τ
  | [] => List.Sublist.slnil
  | .inl a :: τ => by simp only [groups, List.map_cons]; exact (groups_sublist τ).cons_cons _
  | .inr b :: τ => by simp only [groups]; exact (groups_sublist τ).cons _

theorem inl_mem_of_groups {α β : Type} {τ : List (α ⊕ β)} {g : α} (h : g ∈ groups τ) : .inl g ∈ τ :=
  (groups_sublist τ).subset (List.mem_map.mpr ⟨g, h, rfl⟩)

section Place
variable {R : Type}

/-- Where a rule that fires goes: after the last of the `K` callers' groups
    that must precede it (`hit`), or first. -/
def slot (K : Nat) (hit : Nat → R → Bool) (r : R) : Nat :=
  maxSucc ((List.range K).filter (fun k => hit k r))

/-- Slot `k`: the rules placed there, in rank order, then group `k`. -/
def bucket (K : Nat) (rs : List R) (hit : Nat → R → Bool) (k : Nat) : List (Nat ⊕ R) :=
  (rs.filter (fun r => slot K hit r == k)).map .inr ++ (if k < K then [.inl k] else [])

/-- **The order of a cycle's parent steps**, for `K` callers' groups (in the
    order the grandparent calls them) and the rules that fire (`rs`, in the
    parent's rank order). -/
def placeOrder (K : Nat) (rs : List R) (hit : Nat → R → Bool) : List (Nat ⊕ R) :=
  (List.range (K + 1)).flatMap (bucket K rs hit)

theorem slot_le (K : Nat) (hit : Nat → R → Bool) (r : R) : slot K hit r ≤ K :=
  maxSucc_le _ K (fun k hk => List.mem_range.mp (List.mem_filter.mp hk).1)

theorem slot_hit {K : Nat} {hit : Nat → R → Bool} {r : R} {k : Nat} (hk : k < K) (h : hit k r = true) :
    k + 1 ≤ slot K hit r :=
  le_maxSucc _ k (List.mem_filter.mpr ⟨List.mem_range.mpr hk, h⟩)

theorem slot_pos {K : Nat} {hit : Nat → R → Bool} {r : R} (h : slot K hit r ≠ 0) :
    ∃ k, k < K ∧ hit k r = true ∧ slot K hit r = k + 1 := by
  rcases maxSucc_spec ((List.range K).filter (fun k => hit k r)) with h' | ⟨k, hk, e⟩
  · exact absurd h' h
  · have := List.mem_filter.mp hk
    exact ⟨k, List.mem_range.mp this.1, this.2, e⟩

theorem mem_bucket {K : Nat} {rs : List R} {hit : Nat → R → Bool} {k : Nat} {x : Nat ⊕ R}
    (h : x ∈ bucket K rs hit k) :
    (∃ r, x = .inr r ∧ r ∈ rs ∧ slot K hit r = k) ∨ (x = .inl k ∧ k < K) := by
  unfold bucket at h
  rcases List.mem_append.mp h with h | h
  · obtain ⟨r, hr, rfl⟩ := List.mem_map.mp h
    have := List.mem_filter.mp hr
    exact .inl ⟨r, rfl, this.1, by simpa using this.2⟩
  · split at h
    · rename_i hk
      simp only [List.mem_singleton] at h
      exact .inr ⟨h, hk⟩
    · simp at h

theorem placeOrder_groups (K : Nat) (rs : List R) (hit : Nat → R → Bool) :
    groups (placeOrder K rs hit) = List.range K := by
  unfold placeOrder
  rw [groups_flatMap]
  have hb : ∀ k, groups (bucket K rs hit k) = if k < K then [k] else [] := by
    intro k
    unfold bucket
    rw [groups_append, groups_map_inr]
    split <;> rfl
  simp only [hb]
  rw [List.range_succ, List.flatMap_append]
  simp only [Nat.lt_irrefl, ite_false, List.flatMap_cons, List.flatMap_nil, List.append_nil]
  have : ∀ (l : List Nat), (∀ k ∈ l, k < K) → l.flatMap (fun k => if k < K then [k] else []) = l := by
    intro l hl
    induction l with
    | nil => rfl
    | cons a l ih =>
      rw [List.flatMap_cons, if_pos (hl a (List.mem_cons_self ..)),
        ih (fun k hk => hl k (List.mem_cons_of_mem _ hk))]
      rfl
  exact this _ (fun k hk => List.mem_range.mp hk)

theorem placeOrder_mem {K : Nat} {rs : List R} {hit : Nat → R → Bool} {x : Nat ⊕ R}
    (h : x ∈ placeOrder K rs hit) : (∃ r, x = .inr r ∧ r ∈ rs) ∨ (∃ k, x = .inl k ∧ k < K) := by
  unfold placeOrder at h
  obtain ⟨k, _, hk⟩ := List.mem_flatMap.mp h
  rcases mem_bucket hk with ⟨r, e, hr, _⟩ | ⟨e, hk'⟩
  · exact .inl ⟨r, e, hr⟩
  · exact .inr ⟨k, e, hk'⟩

theorem mem_placeOrder_inr {K : Nat} {rs : List R} {hit : Nat → R → Bool} {r : R} (h : r ∈ rs) :
    (.inr r : Nat ⊕ R) ∈ placeOrder K rs hit := by
  unfold placeOrder
  refine List.mem_flatMap.mpr ⟨slot K hit r, List.mem_range.mpr (Nat.lt_succ_of_le (slot_le K hit r)), ?_⟩
  unfold bucket
  exact List.mem_append_left _ (List.mem_map.mpr ⟨r, List.mem_filter.mpr ⟨h, by simp⟩, rfl⟩)

/-- **Every pair of steps in `placeOrder` may follow one another**, given:
    groups may follow in the grandparent's order (`Hgg`); a group a rule may
    not precede hits it (`H1`); hits pass along rules that must follow
    (`H2`); a group that may not precede a rule comes after every group
    hitting the rule (`H3`); two rules that may not run in rank order are
    ranked the other way (`H4`). -/
theorem place_ok (K : Nat) (rs : List R) (hnd : rs.Nodup) (hit : Nat → R → Bool)
    (OK : Nat ⊕ R → Nat ⊕ R → Prop)
    (Hgg : ∀ k1 k2, k1 < k2 → k2 < K → OK (.inl k1) (.inl k2))
    (H1 : ∀ k r, k < K → r ∈ rs → ¬ OK (.inr r) (.inl k) → hit k r = true)
    (H2 : ∀ k r1 r2, k < K → r1 ∈ rs → r2 ∈ rs → hit k r1 = true → ¬ OK (.inr r2) (.inr r1) → hit k r2 = true)
    (H3 : ∀ k k' r, k < K → k' < K → r ∈ rs → hit k r = true → ¬ OK (.inl k') (.inr r) → k < k')
    (H4 : ∀ r1 r2, r1 ∈ rs → r2 ∈ rs → r1 ≠ r2 → ¬ OK (.inr r1) (.inr r2) → [r2, r1] <+ rs) :
    ∀ x y, [x, y] <+ placeOrder K rs hit → OK x y := by
  intro x y hxy
  rw [placeOrder, List.flatMap_def] at hxy
  rcases sublist_flatten_pair _ hxy with ⟨l, hl, hs⟩ | ⟨l1, l2, hs, hx, hy⟩
  · obtain ⟨k, hk, rfl⟩ := List.mem_map.mp hl
    unfold bucket at hs
    obtain ⟨p1, p2, e, h1, h2⟩ := List.sublist_append_iff.mp hs
    rcases p1 with _ | ⟨a, _ | ⟨b, _ | ⟨c, p1⟩⟩⟩
    · simp only [List.nil_append] at e
      subst e
      have := h2.length_le
      split at this <;> simp at this
    · simp only [List.singleton_append, List.cons.injEq] at e
      obtain ⟨rfl, rfl⟩ := e
      obtain ⟨r, hr, rfl⟩ := List.mem_map.mp (List.singleton_sublist.mp h1)
      have hr' := List.mem_filter.mp hr
      have hsl : slot K hit r = k := by simpa using hr'.2
      split at h2
      · rename_i hkK
        have hy : y = .inl k := by simpa using h2
        subst hy
        apply Classical.byContradiction
        intro hn
        have := slot_hit hkK (H1 k r hkK hr'.1 hn)
        omega
      · simp at h2
    · simp only [List.cons_append, List.nil_append, List.cons.injEq] at e
      obtain ⟨rfl, rfl, rfl⟩ := e
      obtain ⟨r1, r2, hrs, rfl, rfl⟩ := sublist_map_pair h1
      have hrs' : [r1, r2] <+ rs := hrs.trans List.filter_sublist
      have hne : r1 ≠ r2 := by
        intro e; subst e
        have := hnd.sublist hrs'
        simp at this
      apply Classical.byContradiction
      intro hn
      exact nodup_pair_both hnd hrs' (H4 r1 r2 (hrs'.subset (by simp)) (hrs'.subset (by simp)) hne hn)
    · simp at e
  · obtain ⟨k1, k2, hks, rfl, rfl⟩ := sublist_map_pair hs
    have hlt : k1 < k2 := by
      have := List.pairwise_lt_range.sublist hks
      simpa using this
    have hk2 : k2 < K + 1 := List.mem_range.mp (hks.subset (by simp))
    rcases mem_bucket hx with ⟨r1, rfl, hr1, hs1⟩ | ⟨rfl, hk1K⟩ <;>
      rcases mem_bucket hy with ⟨r2, rfl, hr2, hs2⟩ | ⟨rfl, hk2K⟩
    · -- two rules in different slots
      apply Classical.byContradiction
      intro hn
      obtain ⟨k, hkK, hh, hsk⟩ := slot_pos (K := K) (hit := hit) (r := r2) (by omega)
      have := slot_hit hkK (H2 k r2 r1 hkK hr2 hr1 hh hn)
      omega
    · -- a rule, then a later group
      apply Classical.byContradiction
      intro hn
      have := slot_hit hk2K (H1 k2 r1 hk2K hr1 hn)
      omega
    · -- a group, then a rule in a later slot
      apply Classical.byContradiction
      intro hn
      obtain ⟨k, hkK, hh, hsk⟩ := slot_pos (K := K) (hit := hit) (r := r2) (by omega)
      have := H3 k k1 r2 hkK hk1K hr2 hh hn
      omega
    · exact Hgg k1 k2 hlt hk2K

end Place

section Cert
variable (P : Parent V)

/-- Every child call of `t1` may be followed by every child call of `t2`. -/
def OkT (t1 t2 : List (Σ j, (P.CI j).M × V)) : Prop :=
  ∀ j, ∀ a ∈ proj j t1, ∀ b ∈ proj j t2, (P.CI j).ann.Follow a b

/-- Every child call of `t1` and every child call of `t2` may be one caller's. -/
def TogT (t1 t2 : List (Σ j, (P.CI j).M × V)) : Prop :=
  ∀ j, ∀ a ∈ proj j t1, ∀ b ∈ proj j t2, (P.CI j).ann.Group a b

/-- The child calls of `t` may be one caller's, in their order. -/
def GrpT (t : List (Σ j, (P.CI j).M × V)) : Prop :=
  ∀ j, ∀ a b, [a, b] <+ proj j t → (P.CI j).ann.Group a b

/-- `t` calls child `j`'s port `cm`. -/
def Calls (t : List (Σ j, (P.CI j).M × V)) (j : P.I) (cm : (P.CI j).M) : Prop := ∃ x, (cm, x) ∈ proj j t

/-- A parent method called with its port's argument, unfolded with the hardware's values. -/
def trM (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) (m : P.X.M) :=
  trace oc (P.body m (w.arg m))

def trR (oc : (j : P.I) → Outs (P.CI j).M V) (r : P.PR) := trace oc (P.rbody r)

/-- A method may be called this cycle: it is always ready or ready, and
    enabled if it has an enable. -/
def Avail (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) (m : P.X.M) : Prop :=
  (P.X.ann.alwaysRdy m = true ∨ (P.comb oc w).2.rdy m = true) ∧ (P.X.ann.hasEn m = true → w.en m = true)

/-- A method the wires say is called: enabled, or always enabled. -/
def called (w : Wires P.X.M V) (m : P.X.M) : Bool := (P.X.ann.hasEn m && w.en m) || P.X.ann.alwaysEn m

/-- A parent's steps: its methods and its rules. -/
abbrev Step := P.X.M ⊕ P.PR

/-- A step's child calls, unfolded with the hardware's values (a method with
    its port's argument). -/
def trS (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) : Step P → List (Σ j, (P.CI j).M × V)
  | .inl m => (trM P oc w m).1
  | .inr r => (trR P oc r).1

/-- A step that makes its calls this cycle: a called method, or a rule that fires. -/
def On (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) : Step P → Prop
  | .inl m => called P w m = true
  | .inr r => fire oc w r = true

/-- A step that may make its calls this cycle: an available method, or a rule
    that fires. -/
def Av (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) : Step P → Prop
  | .inl m => Avail P oc w m
  | .inr r => fire oc w r = true

/-- What every cycle the obligation covers guarantees, as seen on the wires:
    the module's assertions hold, the children's outputs respect their
    aliases, every called method is available, and no two methods exported
    `C` are both called.  `obl_of_cert` derives it from the contract's
    hypotheses (`Legal`, `Drives`, readiness), so every per-cycle fact of
    the certificate may assume it. -/
def Env (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) : Prop :=
  P.asm oc w ∧ (∀ j, AliasOK P.toB (P.CI j).ann (oc j)) ∧ (∀ m, called P w m = true → Avail P oc w m) ∧
  (∀ m m', m ≠ m' → called P w m = true → called P w m' = true → P.X.ann.conf m m' = false)

/-- The part of `Env` that does not read the parent's RDY outputs: the
    one-hot conditions of `comb`'s muxes are proved from it, so that RDY,
    which `comb` computes, never justifies its own muxes. -/
def EnvC (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) : Prop :=
  P.asm oc w ∧ (∀ j, AliasOK P.toB (P.CI j).ann (oc j)) ∧
  (∀ m m', m ≠ m' → called P w m = true → called P w m' = true → P.X.ann.conf m m' = false)

theorem Env.toC {P : Parent V} {oc : (j : P.I) → Outs (P.CI j).M V} {w : Wires P.X.M V} (h : Env P oc w) :
    EnvC P oc w := ⟨h.1, h.2.1, h.2.2.2⟩

/-- **The per-cycle certificate** a parent's file proves (by `bv_decide` and
    decided tables, as today): the rules in rank order, which fire (WILL_FIRE),
    which callers' methods must precede which rules (`hit`), each step's
    static footprint (the child ports it may call) and each child port's
    users, and the facts.  Every per-cycle fact assumes `Env`.  The facts
    about child ports (`en`, `arg`, `alw`) range over the port's users only,
    and those about a step's calls (`rdy*`, `enV`) over its trace, so their
    total size is the total number of child-port uses. -/
structure CertB where
  rules : List P.PR
  rules_nodup : rules.Nodup
  fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool
  hit : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.X.M → P.PR → Bool
  /-- static footprints (the child ports a step may call) and each port's users -/
  fp : Step P → List (Σ j, (P.CI j).M)
  users : (j : P.I) → (P.CI j).M → List (Step P)
  users_ok : ∀ s, ∀ p ∈ fp s, s ∈ users p.1 p.2
  fp_ok : ∀ oc w s, ∀ c ∈ trS P oc w s, (⟨c.1, c.2.1⟩ : Σ j, (P.CI j).M) ∈ fp s
  /-- each parent method is C with itself or takes one argument per cycle -/
  selfAnn : ∀ m, P.X.ann.conf m m = true ∨ P.X.ann.share m = true
  fireIn : ∀ oc w r, fire oc w r = true → r ∈ rules
  /-- guards and results (today's `schedule_sound`, per step) -/
  resM : ∀ oc w m, Env P oc w → Avail P oc w m → (trM P oc w m).2 = some ((P.comb oc w).2.ret m)
  resR : ∀ oc w r, Env P oc w → fire oc w r = true → (trR P oc r).2.isSome = true
  /-- the child ports a step calls are ready (CAN_FIRE includes their RDY) -/
  rdyM : ∀ oc w m, Env P oc w → Avail P oc w m → ∀ c ∈ (trM P oc w m).1,
    (P.CI c.1).ann.alwaysRdy c.2.1 = false → (oc c.1).rdy c.2.1 = true
  rdyR : ∀ oc w r, Env P oc w → fire oc w r = true → ∀ c ∈ (trR P oc r).1,
    (P.CI c.1).ann.alwaysRdy c.2.1 = false → (oc c.1).rdy c.2.1 = true
  /-- each step's own child calls may be one caller's -/
  grpM : ∀ oc w m, Env P oc w → Avail P oc w m → GrpT P (trM P oc w m).1
  grpR : ∀ oc w r, Env P oc w → fire oc w r = true → GrpT P (trR P oc r).1
  /-- the parent's exported annotations are sound for its child calls -/
  mm : ∀ oc w m1 m2, Env P oc w → Avail P oc w m1 → Avail P oc w m2 →
    P.X.ann.Follow (m1, w.arg m1) (m2, w.arg m2) → OkT P (trM P oc w m1).1 (trM P oc w m2).1
  mmg : ∀ oc w m1 m2, Env P oc w → Avail P oc w m1 → Avail P oc w m2 →
    P.X.ann.Group (m1, w.arg m1) (m2, w.arg m2) → TogT P (trM P oc w m1).1 (trM P oc w m2).1
  /-- placement of rules: the scan's reach facts -/
  h1 : ∀ oc w m r, Env P oc w → Avail P oc w m → fire oc w r = true →
    ¬ OkT P (trR P oc r).1 (trM P oc w m).1 → hit oc w m r = true
  h2 : ∀ oc w m r1 r2, Env P oc w → Avail P oc w m → fire oc w r1 = true → fire oc w r2 = true →
    hit oc w m r1 = true → ¬ OkT P (trR P oc r2).1 (trR P oc r1).1 → hit oc w m r2 = true
  /-- a method hitting a rule that some method `m'` may not precede is
      exported before `m'` and never in one caller with it (SBR), or is
      never called with it (C) -/
  h3 : ∀ oc w m m' r, Env P oc w → Avail P oc w m → Avail P oc w m' → fire oc w r = true →
    hit oc w m r = true → ¬ OkT P (trM P oc w m').1 (trR P oc r).1 →
    m ≠ m' ∧ ((P.X.ann.prec m m' = true ∧ P.X.ann.sbr m m' = true) ∨
      P.X.ann.conf m m' = true ∨ P.X.ann.conf m' m = true)
  h4 : ∀ oc w r1 r2, Env P oc w → fire oc w r1 = true → fire oc w r2 = true → r1 ≠ r2 →
    ¬ OkT P (trR P oc r1).1 (trR P oc r2).1 → [r2, r1] <+ rules
  /-- enables, per child port over its users: on exactly when a step that
      makes its calls calls the port -/
  en : ∀ oc w j cm, Env P oc w → (P.CI j).ann.hasEn cm = true →
    (((P.comb oc w).1 j).en cm = true ↔ ∃ s ∈ users j cm, On P fire oc w s ∧ Calls P (trS P oc w s) j cm)
  /-- a method without an enable calls no port with one -/
  enV : ∀ oc w m, Env P oc w → P.X.ann.hasEn m = false → P.X.ann.alwaysEn m = false → Avail P oc w m →
    ∀ c ∈ (trM P oc w m).1, (P.CI c.1).ann.hasEn c.2.1 = false
  /-- always_enabled child ports are called every cycle -/
  alw : ∀ oc w j cm, Env P oc w → (P.CI j).ann.alwaysEn cm = true →
    ∃ s ∈ users j cm, On P fire oc w s ∧ Calls P (trS P oc w s) j cm
  /-- the parallel muxes' selectors are one-hot (from the assertions, the
      aliases and the exported C pairs only) -/
  onehot : ∀ oc w, EnvC P oc w → P.onehot oc w

/-- **The per-cycle certificate**: `CertB` and today's argument check (every call's argument
    on the port). -/
structure Cert extends CertB P where
  /-- argument muxes, per child port over its users: every call carries the
      mux's value -/
  arg : ∀ oc w j cm, Env P oc w → ∀ s ∈ users j cm, Av P fire oc w s →
    ∀ x, (cm, x) ∈ proj j (trS P oc w s) → ((P.comb oc w).1 j).arg cm = x

/-- **The certificate with last-caller ports** `lc` (self-SBR, with an enable): such a port carries
    the argument of the step last in `lp`; every other port keeps today's check (`argN`). -/
structure CertL extends CertB P where
  lc : (j : P.I) → (P.CI j).M → Bool
  lc_ok : ∀ j cm, lc j cm = true → (P.CI j).ann.sbr cm cm = true ∧ (P.CI j).ann.hasEn cm = true
  lp : Step P → Nat
  /-- on a last-caller port, `lp` orders rules by rank and puts methods before rules -/
  lpRR : ∀ oc r1 r2 j cm, lc j cm = true → Calls P (trR P oc r1).1 j cm → Calls P (trR P oc r2).1 j cm →
    [r1, r2] <+ rules → lp (.inr r1) < lp (.inr r2)
  lpMR : ∀ oc w m r j cm, lc j cm = true → Calls P (trM P oc w m).1 j cm → Calls P (trR P oc r).1 j cm →
    lp (.inl m) < lp (.inr r)
  /-- a method and a rule on one last-caller port: the method's caller comes first -/
  lcMR : ∀ oc w m r j cm, Env P oc w → Avail P oc w m → fire oc w r = true → lc j cm = true →
    Calls P (trM P oc w m).1 j cm → Calls P (trR P oc r).1 j cm → hit oc w m r = true
  /-- two rules on one last-caller port: the later one is hit wherever the earlier one is -/
  lcRR : ∀ oc w m r1 r2 j cm, Env P oc w → Avail P oc w m → fire oc w r1 = true → fire oc w r2 = true →
    lc j cm = true → Calls P (trR P oc r1).1 j cm → Calls P (trR P oc r2).1 j cm →
    lp (.inr r1) < lp (.inr r2) → hit oc w m r1 = true → hit oc w m r2 = true
  /-- two methods on one last-caller port: exported so that the later never precedes the earlier -/
  lcMM : ∀ oc w m1 m2 j cm, Env P oc w → Avail P oc w m1 → Avail P oc w m2 → lc j cm = true →
    Calls P (trM P oc w m1).1 j cm → Calls P (trM P oc w m2).1 j cm → lp (.inl m1) < lp (.inl m2) →
    ¬ P.X.ann.Follow (m2, w.arg m2) (m1, w.arg m1)
  /-- every other port: every call carries the mux's value -/
  argN : ∀ oc w j cm, Env P oc w → lc j cm = false → ∀ s ∈ users j cm, Av P fire oc w s →
    ∀ x, (cm, x) ∈ proj j (trS P oc w s) → ((P.comb oc w).1 j).arg cm = x
  /-- a last-caller port carries the argument of the last step in `lp` that calls it -/
  argL : ∀ oc w j cm, Env P oc w → lc j cm = true → ∀ s ∈ users j cm, On P fire oc w s →
    ∀ x, (cm, x) ∈ proj j (trS P oc w s) →
    (∀ s' ∈ users j cm, lp s < lp s' → On P fire oc w s' → ¬ Calls P (trS P oc w s') j cm) →
    ((P.comb oc w).1 j).arg cm = x

/-- Today's certificate is a last-caller certificate without last-caller ports. -/
def Cert.toL (C : Cert P) : CertL P where
  toCertB := C.toCertB
  lc _ _ := false
  lc_ok _ _ h := by cases h
  lp _ := 0
  lpRR _ _ _ _ _ h := by cases h
  lpMR _ _ _ _ _ _ h := by cases h
  lcMR _ _ _ _ _ _ _ _ _ h := by cases h
  lcRR _ _ _ _ _ _ _ _ _ _ _ h := by cases h
  lcMM _ _ _ _ _ _ _ _ _ h := by cases h
  argN oc w j cm hE _ := C.arg oc w j cm hE
  argL _ _ _ _ _ h := by cases h

theorem okT_flatMap_left {α : Type} (A : List α) (f : α → List (Σ j, (P.CI j).M × V))
    (t : List (Σ j, (P.CI j).M × V)) (h : ∀ a ∈ A, OkT P (f a) t) : OkT P (A.flatMap f) t := by
  intro j x hx y hy
  rw [proj_flatMap] at hx
  obtain ⟨l, hl, hxl⟩ := List.mem_flatten.mp hx
  obtain ⟨a, ha, rfl⟩ := List.mem_map.mp hl
  exact h a ha j x hxl y hy

theorem okT_flatMap_right {α : Type} (B : List α) (g : α → List (Σ j, (P.CI j).M × V))
    (t : List (Σ j, (P.CI j).M × V)) (h : ∀ b ∈ B, OkT P t (g b)) : OkT P t (B.flatMap g) := by
  intro j x hx y hy
  rw [proj_flatMap] at hy
  obtain ⟨l, hl, hyl⟩ := List.mem_flatten.mp hy
  obtain ⟨b, hb, rfl⟩ := List.mem_map.mp hl
  exact h b hb j x hx y hyl

theorem not_okT_left {α : Type} (A : List α) (f : α → List (Σ j, (P.CI j).M × V))
    (t : List (Σ j, (P.CI j).M × V)) (h : ¬ OkT P (A.flatMap f) t) : ∃ a ∈ A, ¬ OkT P (f a) t :=
  Classical.byContradiction fun hn => h (okT_flatMap_left P A f t fun a ha =>
    Classical.byContradiction fun hn' => hn ⟨a, ha, hn'⟩)

theorem not_okT_right {α : Type} (B : List α) (g : α → List (Σ j, (P.CI j).M × V))
    (t : List (Σ j, (P.CI j).M × V)) (h : ¬ OkT P t (B.flatMap g)) : ∃ b ∈ B, ¬ OkT P t (g b) :=
  Classical.byContradiction fun hn => h (okT_flatMap_right P B g t fun b hb =>
    Classical.byContradiction fun hn' => hn ⟨b, hb, hn'⟩)

theorem grpT_flatMap {α : Type} (A : List α) (f : α → List (Σ j, (P.CI j).M × V))
    (h1 : ∀ a ∈ A, GrpT P (f a)) (h2 : ∀ a b, [a, b] <+ A → TogT P (f a) (f b)) : GrpT P (A.flatMap f) := by
  intro j x y hxy
  rw [proj_flatMap] at hxy
  rcases sublist_flatten_pair _ hxy with ⟨l, hl, hs⟩ | ⟨l1, l2, hs, hx, hy⟩
  · obtain ⟨a, ha, rfl⟩ := List.mem_map.mp hl
    exact h1 a ha j x y hs
  · obtain ⟨a, b, hab, rfl, rfl⟩ := sublist_map_pair hs
    exact h2 a b hab j x hx y hy

/-- Legality of the induced child sequences from pairs of steps (the trace
    projection, proved once). -/
theorem legal_of_pairs {E : Type} (tr : E → List (Σ j, (P.CI j).M × V)) (τ : List E) (j : P.I)
    (hp : ∀ e1 e2, [e1, e2] <+ τ → OkT P (tr e1) (tr e2)) (hg : ∀ e ∈ τ, GrpT P (tr e))
    (ha : ∀ cm, (P.CI j).ann.alwaysEn cm = true → ∃ x, (cm, x) ∈ (τ.map (fun e => proj j (tr e))).flatten) :
    Legal (P.CI j).ann (τ.map (fun e => proj j (tr e))) := by
  refine ⟨fun c1 c2 h => ?_, fun g hgm c1 c2 h => ?_, ha⟩
  · rcases sublist_flatten_pair _ h with ⟨l, hl, hs⟩ | ⟨l1, l2, hs, hx, hy⟩
    · obtain ⟨e, he, rfl⟩ := List.mem_map.mp hl
      exact (hg e he j c1 c2 hs).1
    · obtain ⟨e1, e2, he, rfl, rfl⟩ := sublist_map_pair hs
      exact hp e1 e2 he j c1 hx c2 hy
  · obtain ⟨e, he, rfl⟩ := List.mem_map.mp hgm
    exact hg e he j c1 c2 h

theorem lastArg_isSome {M : Type} [DecidableEq M] (m : M) :
    ∀ (l : List (M × V)) (x : V), (m, x) ∈ l → ∃ y, lastArg m l = some y
  | [], x, h => by simp at h
  | c :: l, x, h => by
    simp only [lastArg]
    cases hl : lastArg m l with
    | some y => exact ⟨y, rfl⟩
    | none =>
      rcases List.mem_cons.mp h with e | e
      · subst e; exact ⟨x, by simp⟩
      · obtain ⟨y, hy⟩ := lastArg_isSome m l x e
        rw [hl] at hy; cases hy

/-- In a legal, driven sequence, every call of a port that is C with itself
    or shares its argument carries the port's argument. -/
theorem call_arg {M : Type} [DecidableEq M] {A : Ann M} {w : Wires M V} {σ : List (List (M × V))}
    (hL : Legal A σ) (hD : Drives A w σ) (hs : ∀ m, A.conf m m = true ∨ A.share m = true) :
    ∀ c ∈ σ.flatten, c.2 = w.arg c.1 := by
  intro c hc
  obtain ⟨y, hy⟩ := lastArg_isSome c.1 σ.flatten c.2 hc
  rw [hD.arg c.1 y hy]
  have hy' := lastArg_mem c.1 σ.flatten y hy
  by_cases e : c = (c.1, y)
  · rw [e]
  · have hfol : A.Follow c (c.1, y) ∨ A.Follow (c.1, y) c := by
      rcases either_sublist hc hy' e with h | h
      · exact .inl (hL.order _ _ h)
      · exact .inr (hL.order _ _ h)
    rcases hs c.1 with h | h
    · rcases hfol with hf | hf
      · have := hf.2.1; simp only at this; rw [h] at this; cases this
      · have := hf.2.1; simp only at this; rw [h] at this; cases this
    · rcases hfol with hf | hf
      · exact hf.2.2.2 rfl h
      · exact (hf.2.2.2 rfl h).symm

/-- A call on child `j` in the projection is a call of the list. -/
theorem mem_proj {I : Type} {inst : DecidableEq I} {CM : I → Type} (j : I) :
    ∀ (l : List (Σ j, CM j × V)) (c : CM j × V), c ∈ proj j l → (⟨j, c⟩ : Σ j, CM j × V) ∈ l
  | [], c, h => by simp [proj] at h
  | ⟨j', c'⟩ :: l, c, h => by
    by_cases hj : j' = j
    · subst hj
      rw [proj_cons_self] at h
      rcases List.mem_cons.mp h with e | e
      · subst e; exact List.mem_cons_self ..
      · exact List.mem_cons_of_mem _ (mem_proj j' l c e)
    · rw [proj_cons_ne _ _ hj] at h
      exact List.mem_cons_of_mem _ (mem_proj j l c h)

/-- The order `obl_of_cert` builds from the rules, which fire and the hits alone:
    `placeOrder` over the callers' groups. -/
def orderOf {M R : Type} (rules : List R) (fireB : R → Bool) (hitB : M → R → Bool)
    (σ : List (List (M × V))) : List (List (M × V) ⊕ R) :=
  (placeOrder σ.length (rules.filter fireB) (fun k r => (σ.getD k []).any (fun c => hitB c.1 r))).map
    (Sum.map (fun k => σ.getD k []) id)

/-- Every method called in a legal, driven, ready sequence may be called. -/
theorem avail_of {oc : (j : P.I) → Outs (P.CI j).M V} {w : Wires P.X.M V} {σ : List (List (P.X.M × V))}
    (hD : Drives P.X.ann w σ)
    (hrdy : ∀ c ∈ σ.flatten, P.X.ann.alwaysRdy c.1 = false → (P.comb oc w).2.rdy c.1 = true) :
    ∀ c ∈ σ.flatten, Avail P oc w c.1 := fun c hc => by
  refine ⟨?_, fun he => (hD.en c.1 he).mpr ⟨c.2, hc⟩⟩
  cases h : P.X.ann.alwaysRdy c.1
  · exact .inr (hrdy c hc h)
  · exact .inl rfl

/-- The steps a position of `placeOrder` stands for: a caller group's methods, or a rule. -/
def posSteps (G : Nat → List (P.X.M × V)) : Nat ⊕ P.PR → List (Step P)
  | .inl k => (G k).map (fun c => .inl c.1)
  | .inr r => [.inr r]

/-- Two positions call every last-caller port in `lp` order. -/
def LcOk (lc : (j : P.I) → (P.CI j).M → Bool) (lp : Step P → Nat) (oc : (j : P.I) → Outs (P.CI j).M V)
    (w : Wires P.X.M V) (G : Nat → List (P.X.M × V)) (x y : Nat ⊕ P.PR) : Prop :=
  ∀ j cm, lc j cm = true → ∀ sx ∈ posSteps P G x, ∀ sy ∈ posSteps P G y,
    Calls P (trS P oc w sx) j cm → Calls P (trS P oc w sy) j cm → lp sx ≤ lp sy

theorem lastArg_append {M : Type} [DecidableEq M] (m : M) :
    ∀ (l1 l2 : List (M × V)), lastArg m (l1 ++ l2) = match lastArg m l2 with
      | some y => some y
      | none => lastArg m l1
  | [], l2 => by simp only [List.nil_append]; cases lastArg m l2 <;> rfl
  | c :: l1, l2 => by
    simp only [List.cons_append, lastArg]
    rw [lastArg_append m l1 l2]
    cases lastArg m l2 <;> rfl

/-- The last call of `m` in a flattened map lies at one position, after which no call of `m`. -/
theorem lastArg_flatten_split {M α : Type} [DecidableEq M] (m : M) (f : α → List (M × V)) :
    ∀ (L : List α) (x : V), lastArg m (L.map f).flatten = some x →
      ∃ pre y post, L = pre ++ y :: post ∧ (m, x) ∈ f y ∧ ∀ z ∈ post, ∀ v, (m, v) ∉ f z
  | [], x, h => by simp [lastArg] at h
  | a :: L, x, h => by
    rw [List.map_cons, List.flatten_cons, lastArg_append] at h
    cases hl : lastArg m (L.map f).flatten with
    | some y =>
      simp only [hl, Option.some.injEq] at h
      subst h
      obtain ⟨pre, y', post, e, hy, hp⟩ := lastArg_flatten_split m f L y hl
      exact ⟨a :: pre, y', post, by rw [e]; rfl, hy, hp⟩
    | none =>
      simp only [hl] at h
      refine ⟨[], a, L, rfl, lastArg_mem m _ x h, fun z hz v hv => lastArg_none m _ hl v ?_⟩
      exact List.mem_flatten.mpr ⟨f z, List.mem_map_of_mem hz, hv⟩

/-- **The obligation from a last-caller certificate, at its own order** (`orderOf`). -/
theorem obl_of_certL_at (C : CertL P) (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V)
    (σ : List (List (P.X.M × V))) (hasm : P.asm oc w) (hal : ∀ j, AliasOK P.toB (P.CI j).ann (oc j))
    (hL : Legal P.X.ann σ) (hD : Drives P.X.ann w σ)
    (hrdy : ∀ c ∈ σ.flatten, P.X.ann.alwaysRdy c.1 = false → (P.comb oc w).2.rdy c.1 = true) :
    P.onehot oc w ∧ groups (orderOf C.rules (C.fire oc w) (C.hit oc w) σ) = σ ∧
      (∀ e ∈ orderOf C.rules (C.fire oc w) (C.hit oc w) σ, StepOK P oc (P.comb oc w).2 e) ∧
      ∀ j, Legal (P.CI j).ann (sigmaJ P oc (orderOf C.rules (C.fire oc w) (C.hit oc w) σ) j) ∧
        Drives (P.CI j).ann ((P.comb oc w).1 j) (sigmaJ P oc (orderOf C.rules (C.fire oc w) (C.hit oc w) σ) j) ∧
        ∀ c ∈ (sigmaJ P oc (orderOf C.rules (C.fire oc w) (C.hit oc w) σ) j).flatten,
          (P.CI j).ann.alwaysRdy c.1 = false → (oc j).rdy c.1 = true := by
  have harg : ∀ c ∈ σ.flatten, c.2 = w.arg c.1 := call_arg hL hD C.selfAnn
  have havail : ∀ c ∈ σ.flatten, Avail P oc w c.1 := fun c hc => by
    refine ⟨?_, fun he => (hD.en c.1 he).mpr ⟨c.2, hc⟩⟩
    cases h : P.X.ann.alwaysRdy c.1
    · exact .inr (hrdy c hc h)
    · exact .inl rfl
  -- a called method is in σ
  have hcalled : ∀ m, called P w m = true → ∃ c ∈ σ.flatten, c.1 = m := by
    intro m hm
    unfold called at hm
    simp only [Bool.or_eq_true, Bool.and_eq_true] at hm
    rcases hm with ⟨h1, h2⟩ | h
    · obtain ⟨x, hx⟩ := (hD.en m h1).mp h2
      exact ⟨(m, x), hx, rfl⟩
    · obtain ⟨x, hx⟩ := hL.always m h
      exact ⟨(m, x), hx, rfl⟩
  -- the environment every per-cycle fact assumes
  have hE : Env P oc w := by
    refine ⟨hasm, hal, fun m hm => ?_, fun m m' hne hm hm' => ?_⟩
    · obtain ⟨c, hc, rfl⟩ := hcalled m hm; exact havail c hc
    · obtain ⟨c, hc, rfl⟩ := hcalled m hm
      obtain ⟨c', hc', rfl⟩ := hcalled m' hm'
      have hcc : c ≠ c' := fun e => hne (by rw [e])
      rcases either_sublist hc hc' hcc with h | h
      · exact (hL.order _ _ h).2.1
      · exact (hL.order _ _ h).2.2.1
  have htr : ∀ c ∈ σ.flatten, trace oc (P.body c.1 c.2) = trM P oc w c.1 := fun c hc => by
    unfold trM; rw [← harg c hc]
  let G : Nat → List (P.X.M × V) := fun k => σ.getD k []
  have hGσ : ∀ k c, c ∈ G k → c ∈ σ.flatten := fun k c h => getD_mem_flatten σ k c h
  let rs := C.rules.filter (C.fire oc w)
  have hrs : ∀ r, r ∈ rs ↔ C.fire oc w r = true := fun r =>
    ⟨fun h => (List.mem_filter.mp h).2, fun h => List.mem_filter.mpr ⟨C.fireIn oc w r h, h⟩⟩
  let hitK : Nat → P.PR → Bool := fun k r => (G k).any (fun c => C.hit oc w c.1 r)
  let toE : Nat ⊕ P.PR → List (P.X.M × V) ⊕ P.PR := Sum.map G id
  let tr := stepTrace P oc
  have trG : ∀ k, tr (toE (.inl k)) = (G k).flatMap (fun c => (trace oc (P.body c.1 c.2)).1) := fun k => rfl
  have trRr : ∀ r, tr (toE (.inr r)) = (trR P oc r).1 := fun r => rfl
  -- a group's trace, call by call, through the method traces
  have trGm : ∀ k, (G k).flatMap (fun c => (trace oc (P.body c.1 c.2)).1) =
      (G k).flatMap (fun c => (trM P oc w c.1).1) := fun k => by
    apply flatMap_congr'
    intro c hc; rw [htr c (hGσ k c hc)]
  let OK : Nat ⊕ P.PR → Nat ⊕ P.PR → Prop := fun x y => OkT P (tr (toE x)) (tr (toE y))
  have Hgg : ∀ k1 k2, k1 < k2 → k2 < σ.length → OK (.inl k1) (.inl k2) := by
    intro k1 k2 hlt _
    show OkT P (tr (toE (.inl k1))) (tr (toE (.inl k2)))
    rw [trG, trG, trGm, trGm]
    apply okT_flatMap_left; intro c hc
    apply okT_flatMap_right; intro c' hc'
    have hf := hL.order c c' (pair_getD σ k1 k2 c c' hlt hc hc')
    have hf' : P.X.ann.Follow (c.1, w.arg c.1) (c'.1, w.arg c'.1) := by
      rw [← harg c (hGσ _ _ hc), ← harg c' (hGσ _ _ hc')]; exact hf
    exact C.mm oc w c.1 c'.1 hE (havail c (hGσ _ _ hc)) (havail c' (hGσ _ _ hc')) hf'
  have H1 : ∀ k r, k < σ.length → r ∈ rs → ¬ OK (.inr r) (.inl k) → hitK k r = true := by
    intro k r _ hr hn
    change ¬ OkT P (tr (toE (.inr r))) (tr (toE (.inl k))) at hn
    rw [trRr, trG, trGm] at hn
    obtain ⟨c, hc, hn'⟩ := not_okT_right P _ _ _ hn
    exact List.any_eq_true.mpr ⟨c, hc, C.h1 oc w c.1 r hE (havail c (hGσ _ _ hc)) ((hrs r).mp hr) hn'⟩
  have H2 : ∀ k r1 r2, k < σ.length → r1 ∈ rs → r2 ∈ rs → hitK k r1 = true → ¬ OK (.inr r2) (.inr r1) →
      hitK k r2 = true := by
    intro k r1 r2 _ hr1 hr2 hh hn
    obtain ⟨c, hc, hh'⟩ := List.any_eq_true.mp hh
    exact List.any_eq_true.mpr ⟨c, hc, C.h2 oc w c.1 r1 r2 hE (havail c (hGσ _ _ hc)) ((hrs r1).mp hr1)
      ((hrs r2).mp hr2) hh' hn⟩
  have H3 : ∀ k k' r, k < σ.length → k' < σ.length → r ∈ rs → hitK k r = true → ¬ OK (.inl k') (.inr r) →
      k < k' := by
    intro k k' r hk hk' hr hh hn
    change ¬ OkT P (tr (toE (.inl k'))) (tr (toE (.inr r))) at hn
    rw [trRr, trG, trGm] at hn
    obtain ⟨c', hc', hn'⟩ := not_okT_left P _ _ _ hn
    obtain ⟨c, hc, hh'⟩ := List.any_eq_true.mp hh
    obtain ⟨hne, hcase⟩ := C.h3 oc w c.1 c'.1 r hE (havail c (hGσ _ _ hc)) (havail c' (hGσ _ _ hc'))
      ((hrs r).mp hr) hh' hn'
    apply Classical.byContradiction
    intro hlt
    rcases Nat.lt_or_eq_of_le (Nat.le_of_not_lt hlt) with hlt' | heq
    · -- c' in an earlier group than c
      have hf := hL.order c' c (pair_getD σ k' k c' c hlt' hc' hc)
      rcases hcase with ⟨hpr, -⟩ | hcf | hcf
      · have := hf.1 (Ne.symm hne); rw [hpr] at this; cases this
      · have := hf.2.2.1; rw [hcf] at this; cases this
      · have := hf.2.1; rw [hcf] at this; cases this
    · -- one caller
      subst heq
      have hne' : c ≠ c' := fun e => hne (by rw [e])
      have hgm := getD_mem σ k' hk'
      rcases either_sublist hc hc' hne' with h | h
      · have hg := hL.group _ hgm _ _ h
        rcases hcase with ⟨-, hsb⟩ | hcf | hcf
        · have := hg.2.1; rw [hsb] at this; cases this
        · have := hg.1.2.1; rw [hcf] at this; cases this
        · have := hg.1.2.2.1; rw [hcf] at this; cases this
      · have hg := hL.group _ hgm _ _ h
        rcases hcase with ⟨-, hsb⟩ | hcf | hcf
        · have := hg.2.2; rw [hsb] at this; cases this
        · have := hg.1.2.2.1; rw [hcf] at this; cases this
        · have := hg.1.2.1; rw [hcf] at this; cases this
  have H4 : ∀ r1 r2, r1 ∈ rs → r2 ∈ rs → r1 ≠ r2 → ¬ OK (.inr r1) (.inr r2) → [r2, r1] <+ rs := by
    intro r1 r2 hr1 hr2 hne hn
    have h' := List.Sublist.filter (C.fire oc w)
      (C.h4 oc w r1 r2 hE ((hrs r1).mp hr1) ((hrs r2).mp hr2) hne hn)
    rwa [List.filter_cons_of_pos ((hrs r2).mp hr2), List.filter_cons_of_pos ((hrs r1).mp hr1),
      List.filter_nil] at h'
  let order := placeOrder σ.length rs hitK
  -- the same placement keeps every last-caller port's calls in `lp` order
  let OK' : Nat ⊕ P.PR → Nat ⊕ P.PR → Prop := fun x y => OK x y ∧ LcOk P C.lc C.lp oc w G x y
  have memG : ∀ k s, s ∈ posSteps P G (.inl k) → ∃ c ∈ G k, s = .inl c.1 := by
    intro k s hs
    obtain ⟨c, hc, e⟩ := List.mem_map.mp hs
    exact ⟨c, hc, e.symm⟩
  have memR : ∀ r s, s ∈ posSteps P G (.inr r) → s = .inr r := fun r s hs => List.mem_singleton.mp hs
  have Hgg' : ∀ k1 k2, k1 < k2 → k2 < σ.length → OK' (.inl k1) (.inl k2) := by
    intro k1 k2 hlt hk2
    refine ⟨Hgg k1 k2 hlt hk2, fun j cm hlc sx hsx sy hsy hcx hcy => ?_⟩
    obtain ⟨c, hc, rfl⟩ := memG k1 sx hsx
    obtain ⟨c', hc', rfl⟩ := memG k2 sy hsy
    apply Nat.le_of_not_lt
    intro hgt
    have hf : P.X.ann.Follow (c.1, w.arg c.1) (c'.1, w.arg c'.1) := by
      rw [← harg c (hGσ _ _ hc), ← harg c' (hGσ _ _ hc')]
      exact hL.order c c' (pair_getD σ k1 k2 c c' hlt hc hc')
    exact C.lcMM oc w c'.1 c.1 j cm hE (havail c' (hGσ _ _ hc')) (havail c (hGσ _ _ hc)) hlc hcy hcx hgt hf
  have H1' : ∀ k r, k < σ.length → r ∈ rs → ¬ OK' (.inr r) (.inl k) → hitK k r = true := by
    intro k r hk hr hn
    apply Classical.byContradiction
    intro hh
    apply hn
    refine ⟨Classical.byContradiction fun ho => hh (H1 k r hk hr ho), fun j cm hlc sx hsx sy hsy hcx hcy => ?_⟩
    obtain rfl := memR r sx hsx
    obtain ⟨c, hc, rfl⟩ := memG k sy hsy
    exact absurd (List.any_eq_true.mpr ⟨c, hc, C.lcMR oc w c.1 r j cm hE (havail c (hGσ _ _ hc))
      ((hrs r).mp hr) hlc hcy hcx⟩) hh
  have H2' : ∀ k r1 r2, k < σ.length → r1 ∈ rs → r2 ∈ rs → hitK k r1 = true → ¬ OK' (.inr r2) (.inr r1) →
      hitK k r2 = true := by
    intro k r1 r2 hk hr1 hr2 hh hn
    apply Classical.byContradiction
    intro hh2
    apply hn
    refine ⟨Classical.byContradiction fun ho => hh2 (H2 k r1 r2 hk hr1 hr2 hh ho),
      fun j cm hlc sx hsx sy hsy hcx hcy => ?_⟩
    obtain rfl := memR r2 sx hsx
    obtain rfl := memR r1 sy hsy
    apply Nat.le_of_not_lt
    intro hlt
    obtain ⟨c, hc, hh'⟩ := List.any_eq_true.mp hh
    exact hh2 (List.any_eq_true.mpr ⟨c, hc, C.lcRR oc w c.1 r1 r2 j cm hE (havail c (hGσ _ _ hc))
      ((hrs r1).mp hr1) ((hrs r2).mp hr2) hlc hcy hcx hlt hh'⟩)
  have H3' : ∀ k k' r, k < σ.length → k' < σ.length → r ∈ rs → hitK k r = true → ¬ OK' (.inl k') (.inr r) →
      k < k' := by
    intro k k' r hk hk' hr hh hn
    apply Classical.byContradiction
    intro hlt
    apply hn
    refine ⟨Classical.byContradiction fun ho => hlt (H3 k k' r hk hk' hr hh ho),
      fun j cm hlc sx hsx sy hsy hcx hcy => ?_⟩
    obtain ⟨c, hc, rfl⟩ := memG k' sx hsx
    obtain rfl := memR r sy hsy
    exact Nat.le_of_lt (C.lpMR oc w c.1 r j cm hlc hcx hcy)
  have H4' : ∀ r1 r2, r1 ∈ rs → r2 ∈ rs → r1 ≠ r2 → ¬ OK' (.inr r1) (.inr r2) → [r2, r1] <+ rs := by
    intro r1 r2 hr1 hr2 hne hn
    rcases either_sublist hr2 hr1 (Ne.symm hne) with h | h
    · exact h
    · exfalso
      apply hn
      refine ⟨Classical.byContradiction fun ho => ?_, fun j cm hlc sx hsx sy hsy hcx hcy => ?_⟩
      · exact nodup_pair_both (C.rules_nodup.sublist List.filter_sublist) h (H4 r1 r2 hr1 hr2 hne ho)
      · obtain rfl := memR r1 sx hsx
        obtain rfl := memR r2 sy hsy
        exact Nat.le_of_lt (C.lpRR oc r1 r2 j cm hlc hcx hcy (h.trans List.filter_sublist))
  have hpairs' := place_ok σ.length rs (C.rules_nodup.sublist List.filter_sublist) hitK OK' Hgg' H1' H2' H3' H4'
  have hpairs : ∀ x y, [x, y] <+ order → OK x y := fun x y h => (hpairs' x y h).1
  have hinl : ∀ k, k < σ.length → (.inl k : Nat ⊕ P.PR) ∈ order := by
    intro k hk
    have : k ∈ groups order := by rw [placeOrder_groups σ.length rs hitK]; exact List.mem_range.mpr hk
    have hmem : ∀ (L : List (Nat ⊕ P.PR)), k ∈ groups L → (.inl k : Nat ⊕ P.PR) ∈ L := by
      intro L
      induction L with
      | nil => intro h; simp [groups] at h
      | cons a L ih =>
        intro h
        cases a with
        | inl a =>
          simp only [groups, List.mem_cons] at h
          rcases h with e | h
          · rw [e]; exact List.mem_cons_self ..
          · exact List.mem_cons_of_mem _ (ih h)
        | inr a => exact List.mem_cons_of_mem _ (ih h)
    exact hmem order this
  let τP := order.map toE
  have hτP : orderOf C.rules (C.fire oc w) (C.hit oc w) σ = τP := rfl
  rw [hτP]
  -- the child calls of the steps of τP
  have hsteps : ∀ e ∈ τP, ∀ j y, y ∈ proj j (tr e) →
      (∃ c ∈ σ.flatten, y ∈ proj j (trM P oc w c.1).1) ∨ (∃ r, C.fire oc w r = true ∧ y ∈ proj j (trR P oc r).1) := by
    intro e he j y hy
    obtain ⟨x, hx, rfl⟩ := List.mem_map.mp he
    rcases placeOrder_mem hx with ⟨r, rfl, hr⟩ | ⟨k, rfl, hk⟩
    · exact .inr ⟨r, (hrs r).mp hr, hy⟩
    · rw [trG, trGm, proj_flatMap] at hy
      obtain ⟨l, hl, hyl⟩ := List.mem_flatten.mp hy
      obtain ⟨c, hc, rfl⟩ := List.mem_map.mp hl
      exact .inl ⟨c, hGσ _ _ hc, hyl⟩
  have hflat : ∀ j y, y ∈ (sigmaJ P oc τP j).flatten ↔ ∃ e ∈ τP, y ∈ proj j (tr e) := by
    intro j y
    unfold sigmaJ
    rw [List.mem_flatten]
    constructor
    · rintro ⟨l, hl, hy⟩
      obtain ⟨e, he, rfl⟩ := List.mem_map.mp hl
      exact ⟨e, he, hy⟩
    · rintro ⟨e, he, hy⟩
      exact ⟨_, List.mem_map.mpr ⟨e, he, rfl⟩, hy⟩
  -- every call of σ, and every firing rule, is a step of τP
  have hcallIn : ∀ c ∈ σ.flatten, ∀ j y, y ∈ proj j (trM P oc w c.1).1 → y ∈ (sigmaJ P oc τP j).flatten := by
    intro c hc j y hy
    obtain ⟨k, hk, hck⟩ := mem_flatten_getD σ c hc
    rw [hflat]
    refine ⟨toE (.inl k), List.mem_map.mpr ⟨.inl k, ?_, rfl⟩, ?_⟩
    · have hg := placeOrder_groups σ.length rs hitK
      have : k ∈ groups order := by rw [hg]; exact List.mem_range.mpr hk
      clear hg
      have hmem : ∀ (L : List (Nat ⊕ P.PR)), k ∈ groups L → (.inl k : Nat ⊕ P.PR) ∈ L := by
        intro L
        induction L with
        | nil => intro h; simp [groups] at h
        | cons a L ih =>
          intro h
          cases a with
          | inl a =>
            simp only [groups, List.mem_cons] at h
            rcases h with e | h
            · rw [e]; exact List.mem_cons_self ..
            · exact List.mem_cons_of_mem _ (ih h)
          | inr a => exact List.mem_cons_of_mem _ (ih h)
      exact hmem order this
    · rw [trG, trGm, proj_flatMap]
      exact List.mem_flatten.mpr ⟨_, List.mem_map.mpr ⟨c, hck, rfl⟩, hy⟩
  have hruleIn : ∀ r, C.fire oc w r = true → ∀ j y, y ∈ proj j (trR P oc r).1 → y ∈ (sigmaJ P oc τP j).flatten := by
    intro r hr j y hy
    rw [hflat]
    exact ⟨toE (.inr r), List.mem_map.mpr ⟨.inr r, mem_placeOrder_inr ((hrs r).mpr hr), rfl⟩, hy⟩
  -- a step calling a port is among the port's users
  have inUsers : ∀ (j : P.I) (s : Step P) (y : (P.CI j).M × V), y ∈ proj j (trS P oc w s) → s ∈ C.users j y.1 :=
    fun j s y hy => C.users_ok s ⟨j, y.1⟩ (C.fp_ok oc w s ⟨j, y⟩ (mem_proj (CM := fun j => (P.CI j).M) j _ y hy))
  have hcovers : ∀ j cm, (∃ s ∈ C.users j cm, On P C.fire oc w s ∧ Calls P (trS P oc w s) j cm) →
      ∃ x, (cm, x) ∈ (sigmaJ P oc τP j).flatten := by
    rintro j cm ⟨s, -, hs, x, hx⟩
    rcases s with m | r
    · obtain ⟨c, hc, rfl⟩ := hcalled m hs
      exact ⟨x, hcallIn c hc j _ hx⟩
    · exact ⟨x, hruleIn r hs j _ hx⟩
  refine ⟨C.onehot oc w ⟨hE.1, hE.2.1, hE.2.2.2⟩, ?_, ?_, fun j => ⟨?_, ?_, ?_⟩⟩
  · -- the groups of τP are σ
    show groups (order.map (Sum.map G id)) = σ
    rw [groups_map_sum, placeOrder_groups]
    exact range_map_getD σ
  · -- guards and results
    intro e he
    obtain ⟨x, hx, rfl⟩ := List.mem_map.mp he
    rcases placeOrder_mem hx with ⟨r, rfl, hr⟩ | ⟨k, rfl, hk⟩
    · exact C.resR oc w r hE ((hrs r).mp hr)
    · intro c hc
      rw [htr c (hGσ _ _ hc)]
      exact C.resM oc w c.1 hE (havail c (hGσ _ _ hc))
  · -- legal for child j
    apply legal_of_pairs P tr τP j
    · intro e1 e2 h
      obtain ⟨x, y, hxy, rfl, rfl⟩ := sublist_map_pair h
      exact hpairs x y hxy
    · intro e he
      obtain ⟨x, hx, rfl⟩ := List.mem_map.mp he
      rcases placeOrder_mem hx with ⟨r, rfl, hr⟩ | ⟨k, rfl, hk⟩
      · exact C.grpR oc w r hE ((hrs r).mp hr)
      · show GrpT P (tr (toE (.inl k)))
        rw [trG, trGm]
        apply grpT_flatMap
        · intro c hc; exact C.grpM oc w c.1 hE (havail c (hGσ _ _ hc))
        · intro c c' hcc
          have hg := hL.group _ (getD_mem σ k hk) c c' hcc
          have hc : c ∈ G k := hcc.subset (by simp)
          have hc' : c' ∈ G k := hcc.subset (by simp)
          have hg' : P.X.ann.Group (c.1, w.arg c.1) (c'.1, w.arg c'.1) := by
            rw [← harg c (hGσ _ _ hc), ← harg c' (hGσ _ _ hc')]; exact hg
          exact C.mmg oc w c.1 c'.1 hE (havail c (hGσ _ _ hc)) (havail c' (hGσ _ _ hc')) hg'
    · intro cm hcm
      exact hcovers j cm (C.alw oc w j cm hE hcm)
  · -- driven for child j
    refine ⟨fun cm hcm => ?_, fun cm x hx => ?_⟩
    · rw [C.en oc w j cm hE hcm]
      constructor
      · exact hcovers j cm
      · rintro ⟨x, hx⟩
        obtain ⟨e, he, hy⟩ := (hflat j _).mp hx
        rcases hsteps e he j _ hy with ⟨c, hc, hy'⟩ | ⟨r, hr, hy'⟩
        · refine ⟨.inl c.1, inUsers j (.inl c.1) _ hy', ?_, x, hy'⟩
          show called P w c.1 = true
          unfold called
          cases hEn : P.X.ann.hasEn c.1
          · cases hA : P.X.ann.alwaysEn c.1
            · have := C.enV oc w c.1 hE hEn hA (havail c hc) _ (mem_proj j _ _ hy')
              change (P.CI j).ann.hasEn cm = false at this
              rw [hcm] at this; cases this
            · simp
          · simp only [Bool.true_and, Bool.or_eq_true]
            exact .inl ((hD.en c.1 hEn).mpr ⟨c.2, hc⟩)
        · exact ⟨.inr r, inUsers j (.inr r) _ hy', hr, x, hy'⟩
    · cases hlc : C.lc j cm
      · have hm := lastArg_mem cm _ x hx
        obtain ⟨e, he, hy⟩ := (hflat j _).mp hm
        rcases hsteps e he j _ hy with ⟨c, hc, hy'⟩ | ⟨r, hr, hy'⟩
        · exact C.argN oc w j cm hE hlc (.inl c.1) (inUsers j (.inl c.1) _ hy') (havail c hc) x hy'
        · exact C.argN oc w j cm hE hlc (.inr r) (inUsers j (.inr r) _ hy') hr x hy'
      · -- a last-caller port: the run's last call is the last caller's in `lp`
        obtain ⟨hsbr, hen⟩ := C.lc_ok j cm hlc
        have hsig : sigmaJ P oc τP j = order.map (fun e => proj j (tr (toE e))) := by
          show τP.map _ = _
          rw [List.map_map]; rfl
        rw [hsig] at hx
        obtain ⟨pre, y, post, hord, hy, hpost⟩ := lastArg_flatten_split cm (fun e => proj j (tr (toE e))) order x hx
        have hyo : y ∈ order := by rw [hord]; exact List.mem_append_right _ (List.mem_cons_self ..)
        obtain ⟨s, hsy, hsx, hon⟩ : ∃ s ∈ posSteps P G y, (cm, x) ∈ proj j (trS P oc w s) ∧ On P C.fire oc w s := by
          rcases placeOrder_mem hyo with ⟨r, rfl, hr⟩ | ⟨k, rfl, hk⟩
          · exact ⟨.inr r, List.mem_singleton_self _, hy, (hrs r).mp hr⟩
          · have hy2 : (cm, x) ∈ proj j ((G k).flatMap (fun c => (trM P oc w c.1).1)) := by
              rw [← trGm, ← trG]; exact hy
            rw [proj_flatMap] at hy2
            obtain ⟨l, hl, hxl⟩ := List.mem_flatten.mp hy2
            obtain ⟨c, hc, rfl⟩ := List.mem_map.mp hl
            refine ⟨.inl c.1, List.mem_map_of_mem (f := fun c => (Sum.inl c.1 : Step P)) hc, hxl, ?_⟩
            show called P w c.1 = true
            unfold called
            cases hEn : P.X.ann.hasEn c.1
            · cases hA : P.X.ann.alwaysEn c.1
              · have := C.enV oc w c.1 hE hEn hA (havail c (hGσ _ _ hc)) _ (mem_proj j _ _ hxl)
                change (P.CI j).ann.hasEn cm = false at this
                rw [hen] at this; cases this
              · simp
            · simp only [Bool.true_and, Bool.or_eq_true]
              exact .inl ((hD.en c.1 hEn).mpr ⟨c.2, hGσ _ _ hc⟩)
        apply C.argL oc w j cm hE hlc s (inUsers j s _ hsx) hon x hsx
        rintro s' - hlt hon' ⟨z, hz⟩
        obtain ⟨x', hx'o, hs'x', hz'⟩ : ∃ x' ∈ order, s' ∈ posSteps P G x' ∧ (cm, z) ∈ proj j (tr (toE x')) := by
          rcases s' with m' | r'
          · obtain ⟨c', hc', rfl⟩ := hcalled m' hon'
            obtain ⟨k', hk', hck'⟩ := mem_flatten_getD σ c' hc'
            refine ⟨.inl k', hinl k' hk', List.mem_map_of_mem (f := fun c => (Sum.inl c.1 : Step P)) hck', ?_⟩
            rw [trG, trGm, proj_flatMap]
            exact List.mem_flatten.mpr ⟨_, List.mem_map.mpr ⟨c', hck', rfl⟩, hz⟩
          · exact ⟨.inr r', mem_placeOrder_inr ((hrs r').mpr hon'), List.mem_singleton_self _, hz⟩
        rw [hord] at hx'o
        rcases List.mem_append.mp hx'o with hpre | hyp
        · have hle := (hpairs' x' y (by show [x', y] <+ order; rw [hord]; exact sub_pair_left hpre)).2 j cm hlc s' hs'x' s hsy ⟨z, hz⟩ ⟨x, hsx⟩
          exact Nat.lt_irrefl _ (Nat.lt_of_lt_of_le hlt hle)
        · rcases List.mem_cons.mp hyp with rfl | hpo
          · rcases placeOrder_mem hyo with ⟨r, rfl, -⟩ | ⟨k, rfl, hk⟩
            · rw [memR r s' hs'x', memR r s hsy] at hlt; exact Nat.lt_irrefl _ hlt
            · obtain ⟨c, hc, rfl⟩ := memG k s hsy
              obtain ⟨c', hc', rfl⟩ := memG k s' hs'x'
              by_cases e : c = c'
              · subst e; exact Nat.lt_irrefl _ hlt
              · rcases either_sublist hc hc' e with h | h
                · have hg : P.X.ann.Group (c.1, w.arg c.1) (c'.1, w.arg c'.1) := by
                    rw [← harg c (hGσ _ _ hc), ← harg c' (hGσ _ _ hc')]
                    exact hL.group _ (getD_mem σ k hk) c c' h
                  have := (C.mmg oc w c.1 c'.1 hE (havail c (hGσ _ _ hc)) (havail c' (hGσ _ _ hc')) hg j _ hsx _ hz).2.1
                  rw [hsbr] at this; cases this
                · have hg : P.X.ann.Group (c'.1, w.arg c'.1) (c.1, w.arg c.1) := by
                    rw [← harg c (hGσ _ _ hc), ← harg c' (hGσ _ _ hc')]
                    exact hL.group _ (getD_mem σ k hk) c' c h
                  have := (C.mmg oc w c'.1 c.1 hE (havail c' (hGσ _ _ hc')) (havail c (hGσ _ _ hc)) hg j _ hz _ hsx).2.1
                  rw [hsbr] at this; cases this
          · exact hpost x' hpo z hz'
  · -- ready
    intro c hc hnr
    obtain ⟨e, he, hy⟩ := (hflat j _).mp hc
    rcases hsteps e he j _ hy with ⟨c', hc', hy'⟩ | ⟨r, hr, hy'⟩
    · exact C.rdyM oc w c'.1 hE (havail c' hc') _ (mem_proj j _ _ hy') hnr
    · exact C.rdyR oc w r hE hr _ (mem_proj j _ _ hy') hnr


/-- **The obligation from a certificate, at the certificate's own order** (`orderOf`). -/
theorem obl_of_cert_at (C : Cert P) (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V)
    (σ : List (List (P.X.M × V))) (hasm : P.asm oc w) (hal : ∀ j, AliasOK P.toB (P.CI j).ann (oc j))
    (hL : Legal P.X.ann σ) (hD : Drives P.X.ann w σ)
    (hrdy : ∀ c ∈ σ.flatten, P.X.ann.alwaysRdy c.1 = false → (P.comb oc w).2.rdy c.1 = true) :
    P.onehot oc w ∧ groups (orderOf C.rules (C.fire oc w) (C.hit oc w) σ) = σ ∧
      (∀ e ∈ orderOf C.rules (C.fire oc w) (C.hit oc w) σ, StepOK P oc (P.comb oc w).2 e) ∧
      ∀ j, Legal (P.CI j).ann (sigmaJ P oc (orderOf C.rules (C.fire oc w) (C.hit oc w) σ) j) ∧
        Drives (P.CI j).ann ((P.comb oc w).1 j) (sigmaJ P oc (orderOf C.rules (C.fire oc w) (C.hit oc w) σ) j) ∧
        ∀ c ∈ (sigmaJ P oc (orderOf C.rules (C.fire oc w) (C.hit oc w) σ) j).flatten,
          (P.CI j).ann.alwaysRdy c.1 = false → (oc j).rdy c.1 = true :=
  obl_of_certL_at P C.toL oc w σ hasm hal hL hD hrdy

/-- **The obligation from a certificate.** -/
theorem obl_of_cert (C : Cert P) : Obl P := by
  intro oc w σ hasm hal hL hD hrdy
  obtain ⟨h1, h2, h3, h4⟩ := obl_of_cert_at P C oc w σ hasm hal hL hD hrdy
  exact ⟨h1, _, h2, h3, h4⟩

/-- **A certificate with skipped values**: the rules, which fire and the hits are fixed
    before `u`; at every `u` a `Cert` uses them, and a step that makes its calls makes the
    same calls at every `u` (so the children's call sequences are fixed too). -/
structure CertU (F : ParentU V) where
  rules : List F.PR
  fire : ((j : F.I) → Outs (F.CI j).M V) → Wires F.X.M V → F.PR → Bool
  hit : ((j : F.I) → Outs (F.CI j).M V) → Wires F.X.M V → F.X.M → F.PR → Bool
  cert : ∀ u : F.Step → Nat → V, Cert (F.at u)
  rules_eq : ∀ u, (cert u).rules = rules
  fire_eq : ∀ u, (cert u).fire = fire
  hit_eq : ∀ u, (cert u).hit = hit
  trU : ∀ (oc : (j : F.I) → Outs (F.CI j).M V) (w : Wires F.X.M V) (s : F.Step) (u u' : F.Step → Nat → V),
    Env F.base oc w → Av F.base fire oc w s → trS (F.at u) oc w s = trS (F.at u') oc w s

/-- **`CertU` with last-caller certificates**: rules, firing and hits fixed before `u`, and the
    same calls at every `u`. -/
structure CertUL (F : ParentU V) where
  rules : List F.PR
  fire : ((j : F.I) → Outs (F.CI j).M V) → Wires F.X.M V → F.PR → Bool
  hit : ((j : F.I) → Outs (F.CI j).M V) → Wires F.X.M V → F.X.M → F.PR → Bool
  cert : ∀ u : F.Step → Nat → V, CertL (F.at u)
  rules_eq : ∀ u, (cert u).rules = rules
  fire_eq : ∀ u, (cert u).fire = fire
  hit_eq : ∀ u, (cert u).hit = hit
  trU : ∀ (oc : (j : F.I) → Outs (F.CI j).M V) (w : Wires F.X.M V) (s : F.Step) (u u' : F.Step → Nat → V),
    Env F.base oc w → Av F.base fire oc w s → trS (F.at u) oc w s = trS (F.at u') oc w s

/-- **The family obligation from a last-caller certificate**: the order is `orderOf` of the fixed
    rules, firing and hits; `σC` is any one `u`'s calls. -/
theorem oblU_of_certL [Inhabited V] (F : ParentU V) (C : CertUL F) : OblU F := by
  intro oc w σ hasm hal hL hD hrdy
  have hat : ∀ u : F.Step → Nat → V, F.onehot oc w ∧ groups (orderOf C.rules (C.fire oc w) (C.hit oc w) σ) = σ ∧
      (∀ e ∈ orderOf C.rules (C.fire oc w) (C.hit oc w) σ, StepOK (F.at u) oc (F.comb oc w).2 e) ∧
      ∀ j, Legal (F.CI j).ann (sigmaJ (F.at u) oc (orderOf C.rules (C.fire oc w) (C.hit oc w) σ) j) ∧
        Drives (F.CI j).ann ((F.comb oc w).1 j) (sigmaJ (F.at u) oc (orderOf C.rules (C.fire oc w) (C.hit oc w) σ) j) ∧
        ∀ c ∈ (sigmaJ (F.at u) oc (orderOf C.rules (C.fire oc w) (C.hit oc w) σ) j).flatten,
          (F.CI j).ann.alwaysRdy c.1 = false → (oc j).rdy c.1 = true := by
    intro u
    have h := obl_of_certL_at (F.at u) (C.cert u) oc w σ hasm hal hL hD hrdy
    rw [C.rules_eq u, C.fire_eq u, C.hit_eq u] at h
    exact h
  let u0 : F.Step → Nat → V := fun _ _ => default
  let τP := orderOf C.rules (C.fire oc w) (C.hit oc w) σ
  have hE : Env F.base oc w := by
    have havail := avail_of F.base hD hrdy
    have hcalled : ∀ m, called F.base w m = true → ∃ c ∈ σ.flatten, c.1 = m := by
      intro m hm
      unfold called at hm
      simp only [Bool.or_eq_true, Bool.and_eq_true] at hm
      rcases hm with ⟨h1, h2⟩ | h
      · obtain ⟨x, hx⟩ := (hD.en m h1).mp h2
        exact ⟨(m, x), hx, rfl⟩
      · obtain ⟨x, hx⟩ := hL.always m h
        exact ⟨(m, x), hx, rfl⟩
    refine ⟨hasm, hal, fun m hm => ?_, fun m m' hne hm hm' => ?_⟩
    · obtain ⟨c, hc, rfl⟩ := hcalled m hm; exact havail c hc
    · obtain ⟨c, hc, rfl⟩ := hcalled m hm
      obtain ⟨c', hc', rfl⟩ := hcalled m' hm'
      have hcc : c ≠ c' := fun e => hne (by rw [e])
      rcases either_sublist hc hc' hcc with h | h
      · exact (hL.order _ _ h).2.1
      · exact (hL.order _ _ h).2.2.1
  -- every step of τP makes the same calls at every u
  have hsame : ∀ u, ∀ e ∈ τP, stepTrace (F.at u) oc e = stepTrace (F.at u0) oc e := by
    intro u e he
    obtain ⟨x, hx, rfl⟩ := List.mem_map.mp he
    rcases placeOrder_mem hx with ⟨r, rfl, hr⟩ | ⟨k, rfl, hk⟩
    · exact C.trU oc w (.inr r) u u0 hE (List.mem_filter.mp hr).2
    · show (σ.getD k []).flatMap (fun c => (trace oc (F.body c.1 c.2 (u (.inl c.1)))).1) =
        (σ.getD k []).flatMap (fun c => (trace oc (F.body c.1 c.2 (u0 (.inl c.1)))).1)
      apply flatMap_congr'
      intro c hc
      have hcσ := getD_mem_flatten σ k c hc
      have harg : c.2 = w.arg c.1 := call_arg hL hD (C.cert u0).selfAnn c hcσ
      rw [harg]
      exact C.trU oc w (.inl c.1) u u0 hE (avail_of F.base hD hrdy c hcσ)
  refine ⟨(hat u0).1, τP, fun j => sigmaJ (F.at u0) oc τP j, (hat u0).2.1, fun j => (hat u0).2.2.2 j,
    fun u => ⟨(hat u).2.2.1, fun j => ?_⟩⟩
  exact List.map_congr_left (fun e he => by rw [hsame u e he])


/-- Today's family certificate as a last-caller one. -/
def CertU.toL {F : ParentU V} (C : CertU F) : CertUL F where
  rules := C.rules
  fire := C.fire
  hit := C.hit
  cert u := (C.cert u).toL
  rules_eq u := C.rules_eq u
  fire_eq u := C.fire_eq u
  hit_eq u := C.hit_eq u
  trU := C.trU

/-- **The family obligation from a certificate whose order does not depend on `u`**: the
    order is `orderOf` of the fixed rules, firing and hits; `σC` is any one `u`'s calls. -/
theorem oblU_of_cert [Inhabited V] (F : ParentU V) (C : CertU F) : OblU F :=
  oblU_of_certL F C.toL

end Cert

/-! ## Primitive contracts, continued -/

section Prim2

/-! ### FIFO2 -/

/-- The ports of `FIFO2.v` as bsc imports it (`FIFOF_.bsv`, `vMk2FIFOF`). -/
inductive FifoM where
  | enq | deq | first | notEmpty | notFull | i_notEmpty | i_notFull | clear
  deriving DecidableEq

/-- `vMk2FIFOF`'s schedule (`FIFOF_.bsv`, checked against it by
    `fifoAnn_sched` below).  Readiness: `vMk2FIFOF` has no RDY ports; the
    guards of the `FIFOF` wrapper it is used through are its implicit
    conditions, `i_notFull` for `enq` and `i_notEmpty` for `deq` and
    `first`.  The model takes them as those methods' RDY (`FULL_N`,
    `EMPTY_N`), so a parent's guard read of `f.i_notFull` used as `enq`'s
    implicit condition is `(oc f).rdy enq`.  Guarded FIFOs only
    (`guarded = 1`).  Output aliases: `FULL_N` is `enq`'s RDY and the
    result of `notFull` and `i_notFull`; `EMPTY_N` is `deq`'s and `first`'s
    RDY and the result of `notEmpty` and `i_notEmpty`.  No combinational
    paths (`FIFO2.v`'s outputs are registers). -/
def fifoAnn : Ann FifoM where
  prec p q := match p, q with
    | .first, .deq | .first, .clear => true
    | .notEmpty, .deq | .notEmpty, .enq | .notEmpty, .clear => true
    | .notFull, .deq | .notFull, .enq | .notFull, .clear => true
    | .deq, .clear | .enq, .clear => true
    | _, _ => false
  sbr p q := match p, q with
    | .deq, .clear | .enq, .clear | .clear, .clear => true
    | _, _ => false
  conf p q := match p, q with
    | .enq, .enq | .deq, .deq => true
    | _, _ => false
  share m := match m with
    | .enq | .deq | .clear => false
    | _ => true
  hasEn m := match m with
    | .enq | .deq | .clear => true
    | _ => false
  alwaysEn _ := false
  alwaysRdy m := match m with
    | .enq | .deq | .first => false
    | _ => true
  same p q := match p, q with
    | .notEmpty, .i_notEmpty | .i_notEmpty, .notEmpty | .notFull, .i_notFull | .i_notFull, .notFull => true
    | _, _ => false
  rdyRet a p := match a, p with
    | .enq, .notFull | .enq, .i_notFull => true
    | .deq, .notEmpty | .deq, .i_notEmpty | .first, .notEmpty | .first, .i_notEmpty => true
    | _, _ => false

abbrev FIFO2.X : Iface := ⟨FifoM, inferInstance, fifoAnn⟩

/-! A schedule as bsc writes it (`(ps) rel (qs)`), and the annotations it
gives; pairs it does not list conflict, as in bsc.  A generated file states
each child's annotations this way, from the `.ba`, and checks by `decide`
that they are the library interface's (`fifoAnn_sched`). -/

inductive Rel where
  | CF | SB | SBR | C
  deriving DecidableEq

def schedRel {M : Type} [DecidableEq M] (sched : List (List M × Rel × List M)) (p q : M) : Option Rel :=
  (sched.find? (fun st => st.1.contains p && st.2.2.contains q)).map (·.2.1)

def Ann.ofSched {M : Type} [DecidableEq M] (sched : List (List M × Rel × List M))
    (share hasEn alwaysEn alwaysRdy : M → Bool) : Ann M where
  prec p q := decide (p ≠ q) && match schedRel sched p q with
    | some .SB | some .SBR => true
    | _ => false
  sbr p q := match schedRel sched p q with
    | some .SBR => true
    | _ => false
  conf p q := match schedRel sched p q, schedRel sched q p with
    | some .C, _ | _, some .C => true
    | none, none => true
    | _, _ => false
  share := share
  hasEn := hasEn
  alwaysEn := alwaysEn
  alwaysRdy := alwaysRdy

/-- `vMk2FIFOF`'s schedule statements, `FIFOF_.bsv` lines 156-166. -/
def fifo2Sched : List (List FifoM × Rel × List FifoM) :=
  [([.deq], .CF, [.enq, .i_notEmpty, .i_notFull]),
   ([.enq], .CF, [.deq, .first, .i_notEmpty, .i_notFull]),
   ([.first, .notEmpty, .notFull], .CF, [.first, .i_notEmpty, .i_notFull, .notEmpty, .notFull]),
   ([.i_notEmpty, .i_notFull], .CF, [.clear, .first, .i_notEmpty, .i_notFull, .notEmpty, .notFull]),
   ([.clear, .deq, .enq], .SBR, [.clear]),
   ([.first], .SB, [.clear, .deq]),
   ([.notEmpty, .notFull], .SB, [.clear, .deq, .enq]),
   ([.deq], .C, [.deq]),
   ([.enq], .C, [.enq])]

/-- The library's FIFO2 annotations are bsc's schedule. -/
theorem fifoAnn_sched (p q : FifoM) :
    fifoAnn.prec p q = (Ann.ofSched fifo2Sched fifoAnn.share fifoAnn.hasEn fifoAnn.alwaysEn fifoAnn.alwaysRdy).prec p q ∧
    fifoAnn.sbr p q = (Ann.ofSched fifo2Sched fifoAnn.share fifoAnn.hasEn fifoAnn.alwaysEn fifoAnn.alwaysRdy).sbr p q ∧
    fifoAnn.conf p q = (Ann.ofSched fifo2Sched fifoAnn.share fifoAnn.hasEn fifoAnn.alwaysEn fifoAnn.alwaysRdy).conf p q := by
  cases p <;> cases q <;> decide

/-- `FIFO2.v`'s registers `empty_reg` (high when not empty), `full_reg` (high
    when not full), `data0_reg`, `data1_reg`; and `EMPTY_N`, `FULL_N` at the
    cycle's start (`ne0`, `nf0`), which the `i_` reads return: they are CF
    with the actions, so they see the cycle's start whatever comes before
    them.  `ne0`, `nf0` are not registers; the cycle's end refreshes them. -/
structure F2 (V : Type) where
  ne : Bool
  nf : Bool
  d0 : V
  d1 : V
  ne0 : Bool
  nf0 : Bool

/-- What the registers hold.  (`empty_reg = full_reg = 0`, which reset never
    leaves, holds nothing; nothing is ready there but the reads and `clear`.) -/
def F2.contents (s : F2 V) : List V := if s.ne then (if s.nf then [s.d0] else [s.d0, s.d1]) else []

def F2.valid (s : F2 V) : Bool := s.ne || s.nf

/-- The cycle's end: the start-of-cycle flags become the current ones. -/
def F2.snap (s : F2 V) : F2 V := { s with ne0 := s.ne, nf0 := s.nf }

/-- `FIFO2.v`'s next state with `CLR` low.  Its `data0_reg` mux is an AND-OR
    select of `d0di`, `d0d1`, `d0h`, read here as a priority mux; that is
    exact because the selects are one-hot whenever the enables are ready
    (`f2_onehot`). -/
def f2next (s : F2 V) (E D : Bool) (x : V) : F2 V :=
  { s with
    ne := if E && !D then true else if D && !E then !s.nf else s.ne
    nf := if E && !D then !s.ne else if D && !E then true else s.nf
    d0 := if (E && !s.ne) || (E && D && s.nf) then x else if D && !s.nf then s.d1 else s.d0
    d1 := if E && s.ne then x else s.d1 }

/-- `CLR` wins over the flags (`FIFO2.v`: empty); the data registers load as
    without it. -/
def f2clr (s : F2 V) : F2 V := { s with ne := false, nf := true }

/-- Whenever the enables are ready, exactly one of `FIFO2.v`'s `d0di`,
    `d0d1`, `d0h` is set. -/
theorem f2_onehot (ne nf E D : Bool) (hE : E = true → nf = true) (hD : D = true → ne = true) :
    let d0di := (E && !ne) || (E && D && nf)
    let d0d1 := D && !nf
    let d0h := (!D && !E) || (!D && ne) || (!E && nf)
    (d0di && !d0d1 && !d0h) || (!d0di && d0d1 && !d0h) || (!d0di && !d0d1 && d0h) = true := by
  cases ne <;> cases nf <;> cases E <;> cases D <;> simp_all

/-- A 2-element FIFO.  Atomically, a queue of at most two values (the data
    registers' other contents are free); in hardware, `FIFO2.v` (RST high). -/
@[reducible] def FIFO2 (ofBool : Bool → V) : Module V FIFO2.X where
  S := F2 V
  R := Empty
  meth := fun (m : FifoM) x s v s' => match m with
    | .enq => s.nf = true ∧ s'.contents = s.contents ++ [x] ∧ s'.valid = true ∧ s'.ne0 = s.ne0 ∧ s'.nf0 = s.nf0
    | .deq => s.ne = true ∧ s'.contents = s.contents.tail ∧ s'.valid = true ∧ s'.ne0 = s.ne0 ∧ s'.nf0 = s.nf0
    | .first => s.ne = true ∧ v = s.d0 ∧ s' = s
    | .notEmpty => v = ofBool s.ne ∧ s' = s
    | .notFull => v = ofBool s.nf ∧ s' = s
    | .i_notEmpty => v = ofBool s.ne0 ∧ s' = s
    | .i_notFull => v = ofBool s.nf0 ∧ s' = s
    | .clear => s'.contents = [] ∧ s'.valid = true ∧ s'.ne0 = s.ne0 ∧ s'.nf0 = s.nf0
  rule := fun r => r.elim
  clean s := s.snap
  clean_idem _ := rfl
  Sig := Unit
  hw s w o s' _ := o.rdy .enq = s.nf ∧ o.rdy .deq = s.ne ∧ o.rdy .first = s.ne ∧
    o.ret .first = s.d0 ∧ o.ret .notEmpty = ofBool s.ne ∧ o.ret .notFull = ofBool s.nf ∧
    o.ret .i_notEmpty = ofBool s.ne ∧ o.ret .i_notFull = ofBool s.nf ∧
    s' = (if w.en .clear then f2clr (f2next s (w.en .enq) (w.en .deq) (w.arg .enq))
          else f2next s (w.en .enq) (w.en .deq) (w.arg .enq)).snap
  ok _ := True

/-- Which port may follow which (`fifoAnn.Follow`, on ports). -/
def okF : FifoM → FifoM → Bool
  | .enq, .enq | .deq, .deq => false
  | .enq, .notEmpty | .enq, .notFull | .deq, .notEmpty | .deq, .notFull => false
  | .clear, .notEmpty | .clear, .notFull => false
  | .deq, .first | .clear, .first => false
  | .clear, .enq | .clear, .deq => false
  | _, _ => true

theorem f2_fol {a b : FifoM} {y z : V} (h : fifoAnn.Follow (a, y) (b, z)) : okF a b = true := by
  obtain ⟨h1, h2, h3, -⟩ := h
  cases a <;> cases b <;> simp_all [fifoAnn, okF]

/-- A read is right in state `t`. -/
def F2ReadOk (ofBool : Bool → V) (o : Outs FifoM V) (t : F2 V) : FifoM → Prop
  | .first => t.ne = true ∧ o.ret .first = t.d0
  | .notEmpty => o.ret .notEmpty = ofBool t.ne
  | .notFull => o.ret .notFull = ofBool t.nf
  | .i_notEmpty => o.ret .i_notEmpty = ofBool t.ne0
  | .i_notFull => o.ret .i_notFull = ofBool t.nf0
  | _ => False

theorem execG_f2reads (ofBool : Bool → V) (o : Outs FifoM V) :
    ∀ (q : List (FifoM × V)) (t : F2 V), (∀ c ∈ q, F2ReadOk ofBool o t c.1) → ExecG (FIFO2 ofBool) o t q t
  | [], t, _ => .nil _
  | (m, y) :: q, t, h => by
    have ih := execG_f2reads ofBool o q t (fun c hc => h c (List.mem_cons_of_mem _ hc))
    have hm := h _ (List.mem_cons_self ..)
    cases m with
    | first => exact .cons (s1 := t) ⟨hm.1, hm.2, rfl⟩ ih
    | notEmpty => exact .cons (s1 := t) ⟨hm, rfl⟩ ih
    | notFull => exact .cons (s1 := t) ⟨hm, rfl⟩ ih
    | i_notEmpty => exact .cons (s1 := t) ⟨hm, rfl⟩ ih
    | i_notFull => exact .cons (s1 := t) ⟨hm, rfl⟩ ih
    | enq => exact hm.elim
    | deq => exact hm.elim
    | clear => exact hm.elim

theorem three_parts {α : Type} {A B1 B2 : List α} {a1 a2 c : α} :
    (c ∈ A → [c, a1] <+ A ++ a1 :: (B1 ++ a2 :: B2) ∧ [c, a2] <+ A ++ a1 :: (B1 ++ a2 :: B2)) ∧
    (c ∈ B1 → [a1, c] <+ A ++ a1 :: (B1 ++ a2 :: B2) ∧ [c, a2] <+ A ++ a1 :: (B1 ++ a2 :: B2)) ∧
    (c ∈ B2 → [a1, c] <+ A ++ a1 :: (B1 ++ a2 :: B2) ∧ [a2, c] <+ A ++ a1 :: (B1 ++ a2 :: B2)) := by
  have e : A ++ a1 :: (B1 ++ a2 :: B2) = (A ++ a1 :: B1) ++ a2 :: B2 := by simp
  refine ⟨fun h => ⟨sub_pair_left h, ?_⟩, fun h => ⟨sub_pair_right (List.mem_append_left _ h), ?_⟩,
    fun h => ⟨sub_pair_right (List.mem_append_right _ (List.mem_cons_of_mem _ h)), ?_⟩⟩
  · rw [e]; exact sub_pair_left (List.mem_append_left _ h)
  · rw [e]; exact sub_pair_left (List.mem_append_right _ (List.mem_cons_of_mem _ h))
  · rw [e]; exact sub_pair_right h

/-- **The run of a cycle without `clear`**, from the start state `s` to
    `FIFO2.v`'s next state. -/
theorem f2_run (ofBool : Bool → V) (o : Outs FifoM V) (s : F2 V) (E D : Bool) (x0 : V)
    (L : List (FifoM × V))
    (fol : ∀ c1 c2, [c1, c2] <+ L → okF c1.1 c2.1 = true)
    (hnoC : ∀ c ∈ L, c.1 ≠ .clear)
    (hEi : E = true ↔ ∃ x, (FifoM.enq, x) ∈ L) (hDi : D = true ↔ ∃ y, (FifoM.deq, y) ∈ L)
    (harg : ∀ x, (FifoM.enq, x) ∈ L → x0 = x)
    (rdyE : E = true → s.nf = true) (rdyD : D = true → s.ne = true)
    (rdyF : ∀ y, (FifoM.first, y) ∈ L → s.ne = true)
    (hfirst : o.ret .first = s.d0) (hne : o.ret .notEmpty = ofBool s.ne) (hnf : o.ret .notFull = ofBool s.nf)
    (hine : o.ret .i_notEmpty = ofBool s.ne0) (hinf : o.ret .i_notFull = ofBool s.nf0) :
    ExecG (FIFO2 ofBool) o s L (f2next s E D x0) := by
  -- a read at the start state: anything but an action
  have rd_s : ∀ (q : List (FifoM × V)), (∀ c ∈ q, c ∈ L) →
      (∀ c ∈ q, c.1 ≠ .enq ∧ c.1 ≠ .deq) → ExecG (FIFO2 ofBool) o s q s := by
    intro q hq hr
    apply execG_f2reads
    intro c hc
    obtain ⟨h1, h2⟩ := hr c hc
    have h3 := hnoC c (hq c hc)
    rcases c with ⟨m, y⟩
    cases m <;> simp only at h1 h2 h3 <;> try contradiction
    · exact ⟨rdyF y (hq _ hc), hfirst⟩
    · exact hne
    · exact hnf
    · exact hine
    · exact hinf
  -- `i_` reads anywhere: every state of the run keeps `s`'s start flags
  have rd_i : ∀ (q : List (FifoM × V)) (t : F2 V), t.ne0 = s.ne0 → t.nf0 = s.nf0 →
      (∀ c ∈ q, c.1 = .i_notEmpty ∨ c.1 = .i_notFull) → ExecG (FIFO2 ofBool) o t q t := by
    intro q t h0 h1 hq
    apply execG_f2reads
    intro c hc
    rcases hq c hc with e | e <;> rw [e]
    · show o.ret .i_notEmpty = ofBool t.ne0; rw [h0]; exact hine
    · show o.ret .i_notFull = ofBool t.nf0; rw [h1]; exact hinf
  by_cases hEx : ∃ x, (FifoM.enq, x) ∈ L <;> by_cases hDx : ∃ y, (FifoM.deq, y) ∈ L
  · -- enq and deq: the FIFO holds one value
    obtain ⟨x, hx⟩ := hEx
    obtain ⟨y, hy⟩ := hDx
    have hE : E = true := hEi.mpr ⟨x, hx⟩
    have hD : D = true := hDi.mpr ⟨y, hy⟩
    have hsne := rdyD hD
    have hsnf := rdyE hE
    subst hE hD
    rw [harg x hx]
    have hfin : f2next s true true x = { s with d0 := x, d1 := x } := by
      simp [f2next, hsne, hsnf]
    rw [hfin]
    obtain ⟨A, B, eL⟩ := List.append_of_mem hx
    have hyAB : (FifoM.deq, y) ∈ A ∨ (FifoM.deq, y) ∈ B := by
      rw [eL] at hy
      rcases List.mem_append.mp hy with h | h
      · exact .inl h
      · rcases List.mem_cons.mp h with h | h
        · cases h
        · exact .inr h
    rcases hyAB with hyA | hyB
    · -- deq before enq
      obtain ⟨A1, A2, eA⟩ := List.append_of_mem hyA
      subst eA
      have eL' : L = A1 ++ (FifoM.deq, y) :: (A2 ++ (FifoM.enq, x) :: B) := by rw [eL]; simp
      have tp := fun c => (three_parts (A := A1) (B1 := A2) (B2 := B) (a1 := (FifoM.deq, y))
        (a2 := (FifoM.enq, x)) (c := c))
      rw [eL']
      refine execG_append' _ _ _ _ (rd_s A1 (fun c hc => by rw [eL']; exact List.mem_append_left _ hc)
        (fun c hc => ?_)) ?_
      · obtain ⟨h1, h2⟩ := (tp c).1 hc
        rw [← eL'] at h1 h2
        have f1 := fol _ _ h1
        have f2 := fol _ _ h2
        rcases c with ⟨m, z⟩
        constructor <;> intro e <;> subst e <;> simp [okF] at f1 f2
      · have hm1 : (FIFO2 ofBool).meth .deq y s (o.ret .deq) ({ s with ne := false, nf := true } : F2 V) :=
          ⟨hsne, by simp [F2.contents, hsne, hsnf], rfl, rfl, rfl⟩
        have hm2 : (FIFO2 ofBool).meth .enq x ({ s with ne := false, nf := true } : F2 V) (o.ret .enq)
            ({ s with d0 := x, d1 := x } : F2 V) :=
          ⟨rfl, by simp [F2.contents, hsne, hsnf], by simp [F2.valid, hsnf], rfl, rfl⟩
        refine .cons hm1 (execG_append' _ _ _ _ (rd_i A2 _ rfl rfl ?_) (.cons hm2 (rd_i B _ rfl rfl ?_)))
        · intro c hc
          obtain ⟨h1, h2⟩ := (tp c).2.1 hc
          rw [← eL'] at h1 h2
          have f1 := fol _ _ h1
          have f2 := fol _ _ h2
          have f3 := hnoC c (by rw [eL']; simp [hc])
          rcases c with ⟨m, z⟩
          cases m <;> simp [okF] at f1 f2 f3 ⊢
        · intro c hc
          obtain ⟨h1, h2⟩ := (tp c).2.2 hc
          rw [← eL'] at h1 h2
          have f1 := fol _ _ h1
          have f2 := fol _ _ h2
          have f3 := hnoC c (by rw [eL']; simp [hc])
          rcases c with ⟨m, z⟩
          cases m <;> simp [okF] at f1 f2 f3 ⊢
    · -- enq before deq
      obtain ⟨B1, B2, eB⟩ := List.append_of_mem hyB
      subst eB
      have tp := fun c => (three_parts (A := A) (B1 := B1) (B2 := B2) (a1 := (FifoM.enq, x))
        (a2 := (FifoM.deq, y)) (c := c))
      have hm1 : (FIFO2 ofBool).meth .enq x s (o.ret .enq) ({ s with nf := false, d1 := x } : F2 V) :=
        ⟨hsnf, by simp [F2.contents, hsne, hsnf], by simp [F2.valid, hsne], rfl, rfl⟩
      have hm2 : (FIFO2 ofBool).meth .deq y ({ s with nf := false, d1 := x } : F2 V) (o.ret .deq)
          ({ s with d0 := x, d1 := x } : F2 V) :=
        ⟨hsne, by simp [F2.contents, hsne, hsnf], by simp [F2.valid, hsne], rfl, rfl⟩
      rw [eL]
      refine execG_append' _ _ _ _ (rd_s A (fun c hc => by rw [eL]; exact List.mem_append_left _ hc)
        (fun c hc => ?_)) (.cons hm1 (execG_append' _ _ _ _
          (execG_f2reads ofBool o B1 _ ?_) (.cons hm2 (rd_i B2 _ rfl rfl ?_))))
      · obtain ⟨h1, h2⟩ := (tp c).1 hc
        rw [← eL] at h1 h2
        have f1 := fol _ _ h1
        have f2 := fol _ _ h2
        rcases c with ⟨m, z⟩
        constructor <;> intro e <;> subst e <;> simp [okF] at f1 f2
      · -- between the two actions: `first`, which sees the head, and `i_` reads
        intro c hc
        obtain ⟨h1, h2⟩ := (tp c).2.1 hc
        rw [← eL] at h1 h2
        have f1 := fol _ _ h1
        have f2 := fol _ _ h2
        have f3 := hnoC c (by rw [eL]; simp [hc])
        rcases c with ⟨m, z⟩
        cases m <;> simp [okF] at f1 f2 f3 <;> simp [F2ReadOk, hsne, hfirst, hine, hinf]
      · intro c hc
        obtain ⟨h1, h2⟩ := (tp c).2.2 hc
        rw [← eL] at h1 h2
        have f1 := fol _ _ h1
        have f2 := fol _ _ h2
        have f3 := hnoC c (by rw [eL]; simp [hc])
        rcases c with ⟨m, z⟩
        cases m <;> simp [okF] at f1 f2 f3 ⊢
  · -- enq only
    obtain ⟨x, hx⟩ := hEx
    have hE : E = true := hEi.mpr ⟨x, hx⟩
    have hD : D = false := by
      cases e : D
      · rfl
      · exact absurd (hDi.mp e) hDx
    have hsnf := rdyE hE
    subst hE hD
    rw [harg x hx]
    obtain ⟨A, B, eL⟩ := List.append_of_mem hx
    have hnoD : ∀ c ∈ L, c.1 ≠ .deq := fun c hc e => hDx ⟨c.2, by rw [← e]; exact hc⟩
    have hm : (FIFO2 ofBool).meth .enq x s (o.ret .enq) (f2next s true false x) := by
      refine ⟨hsnf, ?_, ?_, rfl, rfl⟩
      · cases hn : s.ne <;> simp [F2.contents, f2next, hn, hsnf]
      · simp [F2.valid, f2next]
    rw [eL]
    refine execG_append' _ _ _ _ (rd_s A (fun c hc => by rw [eL]; exact List.mem_append_left _ hc)
      (fun c hc => ⟨?_, hnoD c (by rw [eL]; exact List.mem_append_left _ hc)⟩))
      (.cons hm (execG_f2reads ofBool o B _ ?_))
    · have f1 := fol _ _ (by rw [eL]; exact sub_pair_left hc : [c, (FifoM.enq, x)] <+ L)
      intro e; rcases c with ⟨m, z⟩; simp only at e; subst e; simp [okF] at f1
    · -- after the enq: `first` (the head is unchanged) and `i_` reads
      intro c hc
      have hcL : c ∈ L := by rw [eL]; exact List.mem_append_right _ (List.mem_cons_of_mem _ hc)
      have f1 := fol _ _ (by rw [eL]; exact sub_pair_right hc : [(FifoM.enq, x), c] <+ L)
      have f2 := hnoD c hcL
      have f3 := hnoC c hcL
      rcases c with ⟨m, z⟩
      cases m <;> simp [okF] at f1 f2 f3
      · have hsne := rdyF z hcL
        simp [F2ReadOk, f2next, hsne, hfirst]
      · simp [F2ReadOk, f2next, hine]
      · simp [F2ReadOk, f2next, hinf]
  · -- deq only
    obtain ⟨y, hy⟩ := hDx
    have hE : E = false := by
      cases e : E
      · rfl
      · exact absurd (hEi.mp e) hEx
    have hD : D = true := hDi.mpr ⟨y, hy⟩
    have hsne := rdyD hD
    subst hE hD
    obtain ⟨A, B, eL⟩ := List.append_of_mem hy
    have hnoE : ∀ c ∈ L, c.1 ≠ .enq := fun c hc e => hEx ⟨c.2, by rw [← e]; exact hc⟩
    have hm : (FIFO2 ofBool).meth .deq y s (o.ret .deq) (f2next s false true x0) := by
      refine ⟨hsne, ?_, ?_, rfl, rfl⟩
      · cases hn : s.nf <;> simp [F2.contents, f2next, hn, hsne]
      · simp [F2.valid, f2next]
    rw [eL]
    refine execG_append' _ _ _ _ (rd_s A (fun c hc => by rw [eL]; exact List.mem_append_left _ hc)
      (fun c hc => ⟨hnoE c (by rw [eL]; exact List.mem_append_left _ hc), ?_⟩))
      (.cons hm (rd_i B _ rfl rfl ?_))
    · have f1 := fol _ _ (by rw [eL]; exact sub_pair_left hc : [c, (FifoM.deq, y)] <+ L)
      intro e; rcases c with ⟨m, z⟩; simp only at e; subst e; simp [okF] at f1
    · intro c hc
      have hcL : c ∈ L := by rw [eL]; exact List.mem_append_right _ (List.mem_cons_of_mem _ hc)
      have f1 := fol _ _ (by rw [eL]; exact sub_pair_right hc : [(FifoM.deq, y), c] <+ L)
      have f2 := hnoE c hcL
      have f3 := hnoC c hcL
      rcases c with ⟨m, z⟩
      cases m <;> simp [okF] at f1 f2 f3 ⊢
  · -- no actions
    have hE : E = false := by
      cases e : E
      · rfl
      · exact absurd (hEi.mp e) hEx
    have hD : D = false := by
      cases e : D
      · rfl
      · exact absurd (hDi.mp e) hDx
    subst hE hD
    have : f2next s false false x0 = s := by rcases s with ⟨ne, nf, d0, d1, ne0, nf0⟩; simp [f2next]
    rw [this]
    apply rd_s _ (fun c hc => hc)
    intro c hc
    exact ⟨fun e => hEx ⟨c.2, by rw [← e]; exact hc⟩, fun e => hDx ⟨c.2, by rw [← e]; exact hc⟩⟩

/-- The first element with a property splits a list. -/
theorem first_split {α : Type} (p : α → Bool) :
    ∀ (l : List α), (∃ c ∈ l, p c = true) → ∃ A c B, l = A ++ c :: B ∧ p c = true ∧ ∀ a ∈ A, p a = false
  | [], h => by simp at h
  | a :: l, h => by
    cases hp : p a
    · obtain ⟨c, hc, hpc⟩ := h
      have hc' : c ∈ l := by
        rcases List.mem_cons.mp hc with e | e
        · subst e; rw [hp] at hpc; cases hpc
        · exact e
      obtain ⟨A, c', B, e, h1, h2⟩ := first_split p l ⟨c, hc', hpc⟩
      refine ⟨a :: A, c', B, by rw [e]; rfl, h1, fun b hb => ?_⟩
      rcases List.mem_cons.mp hb with e' | e'
      · subst e'; exact hp
      · exact h2 b e'
    · exact ⟨[], a, l, rfl, hp, fun b hb => by simp at hb⟩

/-- **FIFO2 realizes its contract**, in every state. -/
theorem fifo2_realizes (ofBool : Bool → V) : Realizes (FIFO2 ofBool) := by
  intro s σ w o s' _ hc hL hD hhw _ hrdy
  obtain ⟨hre, hrd, hrf, hfirst, hne, hnf, hine, hinf, hs'⟩ := hhw
  have hsn0 : s.ne0 = s.ne := by have := congrArg F2.ne0 hc; simp [F2.snap] at this; exact this.symm
  have hsn1 : s.nf0 = s.nf := by have := congrArg F2.nf0 hc; simp [F2.snap] at this; exact this.symm
  have fol : ∀ c1 c2, [c1, c2] <+ σ.flatten → okF c1.1 c2.1 = true := fun c1 c2 h => f2_fol (hL.order c1 c2 h)
  have rdyE : ∀ x, (FifoM.enq, x) ∈ σ.flatten → s.nf = true := fun x h => by
    have := hrdy _ h rfl; rw [hre] at this; exact this
  have rdyD : ∀ y, (FifoM.deq, y) ∈ σ.flatten → s.ne = true := fun y h => by
    have := hrdy _ h rfl; rw [hrd] at this; exact this
  have rdyF : ∀ y, (FifoM.first, y) ∈ σ.flatten → s.ne = true := fun y h => by
    have := hrdy _ h rfl; rw [hrf] at this; exact this
  have hEi : w.en .enq = true ↔ ∃ x, (FifoM.enq, x) ∈ σ.flatten := hD.en .enq rfl
  have hDi : w.en .deq = true ↔ ∃ y, (FifoM.deq, y) ∈ σ.flatten := hD.en .deq rfl
  have hCi : w.en .clear = true ↔ ∃ y, (FifoM.clear, y) ∈ σ.flatten := hD.en .clear rfl
  -- the one enq carries the bus's argument
  have harg : ∀ x, (FifoM.enq, x) ∈ σ.flatten → w.arg .enq = x := by
    intro x hx
    obtain ⟨y, hy⟩ := lastArg_isSome _ _ _ hx
    rw [hD.arg _ y hy]
    have hy' := lastArg_mem _ _ _ hy
    by_cases e : y = x
    · exact e
    · rcases either_sublist hy' hx (by simp [e]) with h | h
      · have := fol _ _ h; simp [okF] at this
      · have := fol _ _ h; simp [okF] at this
  have hine' : o.ret .i_notEmpty = ofBool s.ne0 := by rw [hsn0]; exact hine
  have hinf' : o.ret .i_notFull = ofBool s.nf0 := by rw [hsn1]; exact hinf
  refine ⟨σ.map .inl, ?_⟩
  by_cases hC : ∃ c ∈ σ.flatten, (c.1 == FifoM.clear) = true
  · -- a `clear`: after the first one, only `clear`s and `i_` reads
    obtain ⟨A, c, B, eL, hc1, hA⟩ := first_split (fun c : FifoM × V => c.1 == FifoM.clear) _ hC
    rcases c with ⟨m, y⟩
    simp only [beq_iff_eq] at hc1
    subst hc1
    have hAno : ∀ a ∈ A, a.1 ≠ .clear := fun a ha e => by have := hA a ha; simp [e] at this
    have hBk : ∀ b ∈ B, b.1 = .clear ∨ b.1 = .i_notEmpty ∨ b.1 = .i_notFull := by
      intro b hb
      have f := fol _ _ (by rw [eL]; exact sub_pair_right hb : [(FifoM.clear, y), b] <+ σ.flatten)
      rcases b with ⟨m, z⟩
      cases m <;> simp [okF] at f ⊢
    have inA : ∀ m', m' ≠ .clear → m' ≠ .i_notEmpty → m' ≠ .i_notFull → ∀ z, (m', z) ∈ σ.flatten → (m', z) ∈ A := by
      intro m' h1 h2 h3 z hz
      rw [eL] at hz
      rcases List.mem_append.mp hz with h | h
      · exact h
      · rcases List.mem_cons.mp h with h | h
        · cases h; exact absurd rfl h1
        · rcases hBk _ h with e | e | e <;> simp at e <;> contradiction
    have hAsub : ∀ a ∈ A, a ∈ σ.flatten := fun a ha => by rw [eL]; exact List.mem_append_left _ ha
    have hrun := f2_run ofBool o s (w.en .enq) (w.en .deq) (w.arg .enq) A
      (fun c1 c2 h => fol c1 c2 (by rw [eL]; exact h.trans (List.sublist_append_left _ _))) hAno
      (by rw [hEi]; exact ⟨fun ⟨x, hx⟩ => ⟨x, inA _ (by simp) (by simp) (by simp) x hx⟩,
          fun ⟨x, hx⟩ => ⟨x, hAsub _ hx⟩⟩)
      (by rw [hDi]; exact ⟨fun ⟨x, hx⟩ => ⟨x, inA _ (by simp) (by simp) (by simp) x hx⟩,
          fun ⟨x, hx⟩ => ⟨x, hAsub _ hx⟩⟩)
      (fun x hx => harg x (hAsub _ hx))
      (fun h => by obtain ⟨x, hx⟩ := hEi.mp h; exact rdyE x hx)
      (fun h => by obtain ⟨x, hx⟩ := hDi.mp h; exact rdyD x hx)
      (fun y hy => rdyF y (hAsub _ hy)) hfirst hne hnf hine' hinf'
    let n := f2next s (w.en .enq) (w.en .deq) (w.arg .enq)
    have hCe : w.en .clear = true := hCi.mpr ⟨y, by rw [eL]; simp⟩
    refine ⟨f2clr n, groups_map_inl σ, exec_map_inl _ _ σ _ _ ?_, ?_, fun l => nomatch l⟩
    · rw [eL]
      have hmc : (FIFO2 ofBool).meth .clear y n (o.ret .clear) (f2clr n) :=
        ⟨by simp [F2.contents, f2clr], by simp [F2.valid, f2clr], rfl, rfl⟩
      refine execG_append' _ _ _ _ hrun (.cons hmc ?_)
      -- the tail: `clear`s on the empty FIFO and `i_` reads
      have tail : ∀ q : List (FifoM × V), (∀ b ∈ q, b.1 = .clear ∨ b.1 = .i_notEmpty ∨ b.1 = .i_notFull) →
          ExecG (FIFO2 ofBool) o (f2clr n) q (f2clr n) := by
        intro q hq
        induction q with
        | nil => exact .nil _
        | cons b q ih =>
          have ih' := ih (fun b' hb' => hq b' (List.mem_cons_of_mem _ hb'))
          rcases b with ⟨m, z⟩
          rcases hq _ (List.mem_cons_self ..) with e | e | e <;> simp only at e <;> subst e
          · exact .cons (s1 := f2clr n) ⟨by simp [F2.contents, f2clr], by simp [F2.valid, f2clr], rfl, rfl⟩ ih'
          · exact .cons (s1 := f2clr n) ⟨by simp [f2clr, n, f2next, hine'], rfl⟩ ih'
          · exact .cons (s1 := f2clr n) ⟨by simp [f2clr, n, f2next, hinf'], rfl⟩ ih'
      exact tail B hBk
    · rw [hs', if_pos hCe]
  · -- no `clear`
    have hno : ∀ c ∈ σ.flatten, c.1 ≠ .clear := fun c hc e => hC ⟨c, hc, by simp [e]⟩
    have hCe : w.en .clear = false := by
      cases e : w.en .clear
      · rfl
      · obtain ⟨y, hy⟩ := hCi.mp e; exact absurd rfl (hno _ hy)
    refine ⟨f2next s (w.en .enq) (w.en .deq) (w.arg .enq), groups_map_inl σ, exec_map_inl _ _ σ _ _ ?_, ?_,
      fun l => nomatch l⟩
    · exact f2_run ofBool o s (w.en .enq) (w.en .deq) (w.arg .enq) σ.flatten fol hno hEi hDi harg
        (fun h => by obtain ⟨x, hx⟩ := hEi.mp h; exact rdyE x hx)
        (fun h => by obtain ⟨x, hx⟩ := hDi.mp h; exact rdyD x hx) rdyF hfirst hne hnf hine' hinf'
    · rw [hs', if_neg (by simp [hCe])]

/-- `vMk2FIFOF0` (`FIFO20.v`, a FIFO of no data): `FIFO2` without `first`. -/
inductive Fifo0M where
  | enq | deq | notEmpty | notFull | i_notEmpty | i_notFull | clear
  deriving DecidableEq

def Fifo0M.toF : Fifo0M → FifoM
  | .enq => .enq
  | .deq => .deq
  | .notEmpty => .notEmpty
  | .notFull => .notFull
  | .i_notEmpty => .i_notEmpty
  | .i_notFull => .i_notFull
  | .clear => .clear

/-- `vMk2FIFOF0`'s schedule (`FIFOF_.bsv` lines 181-189) is `vMk2FIFOF`'s
    without `first`. -/
def fifo0Ann : Ann Fifo0M where
  prec p q := fifoAnn.prec p.toF q.toF
  sbr p q := fifoAnn.sbr p.toF q.toF
  conf p q := fifoAnn.conf p.toF q.toF
  share p := fifoAnn.share p.toF
  hasEn p := fifoAnn.hasEn p.toF
  alwaysEn p := fifoAnn.alwaysEn p.toF
  alwaysRdy p := fifoAnn.alwaysRdy p.toF
  same p q := fifoAnn.same p.toF q.toF
  rdyRet a p := fifoAnn.rdyRet a.toF p.toF
  path p q := fifoAnn.path p.toF q.toF

abbrev FIFO20.X : Iface := ⟨Fifo0M, inferInstance, fifo0Ann⟩
abbrev FIFO20 (ofBool : Bool → V) : Module V FIFO20.X := (FIFO2 ofBool).restrict FIFO20.X Fifo0M.toF

theorem fifo20_restricts : Restricts FIFO2.X FIFO20.X Fifo0M.toF :=
  ⟨fun a b h => by cases a <;> cases b <;> (first | rfl | cases h),
    fun _ _ h => h, fun _ _ h => h, fun _ _ h => h, fun _ h => h, fun _ => rfl, fun _ => rfl, fun _ h => h,
    fun p h => by cases p <;> rfl, fun _ _ h => h⟩

theorem fifo20_realizes (ofBool : Bool → V) : Realizes (FIFO20 ofBool) :=
  Realizes.restrict _ fifo20_restricts (fifo2_realizes ofBool)

/-- FIFO2's outputs are its registers'. -/
def f2Out (ofBool : Bool → V) (s : F2 V) : Outs FifoM V :=
  ⟨fun p => match p with | .enq => s.nf | .deq => s.ne | .first => s.ne | _ => true,
   fun p => match p with
    | .first => s.d0
    | .notEmpty => ofBool s.ne
    | .i_notEmpty => ofBool s.ne
    | .notFull => ofBool s.nf
    | .i_notFull => ofBool s.nf
    | _ => s.d0⟩

/-- **FIFO2 is causal**, with no paths. -/
theorem fifo2_causal (ofBool : Bool → V) : Causal (FIFO2 ofBool) :=
  ⟨fun s _ => f2Out ofBool s, fun _ _ _ _ _ => ⟨rfl, rfl⟩,
   fun s w _ => ⟨_, (), rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩⟩

theorem fifo20_causal [Inhabited V] (ofBool : Bool → V) : Causal (FIFO20 ofBool) :=
  Causal.restrict _ fifo20_restricts (fifo2_causal ofBool)

/-- **FIFO2 respects its output aliases**, for a Bool reading that inverts
    the FIFO's encoding of its flags. -/
theorem fifo2_aliased (ofBool : Bool → V) (toB : V → Bool) (h : ∀ b, toB (ofBool b) = b) :
    Aliased toB (FIFO2 ofBool) := by
  intro s w o s' g _ hhw
  obtain ⟨hre, hrd, hrf, -, hne, hnf, hine, hinf, -⟩ := hhw
  refine ⟨fun p q hpq => ?_, fun a p hap => ?_⟩
  · cases p <;> cases q <;> simp [fifoAnn] at hpq <;> simp only [hne, hnf, hine, hinf]
  · cases a <;> cases p <;> simp [fifoAnn] at hap <;> simp only [hre, hrd, hrf, hne, hnf, hine, hinf, h]

theorem fifo20_aliased (ofBool : Bool → V) (toB : V → Bool) (h : ∀ b, toB (ofBool b) = b) :
    Aliased toB (FIFO20 ofBool) :=
  Aliased.restrict toB _ (fifo2_aliased ofBool toB h) (fun _ _ h => h) (fun _ _ h => h)

end Prim2

/-! ### System tasks

A module's task statements, in Verilog order, are the ports of one `Tasks n`
child; `comb` drives port `t` with statement `t`'s condition and arguments,
so the obligation checks them like any child call, and `t SB t'` (`t < t'`)
fixes their order.  A value-returning task's result is a free output. -/

section Tasks

/-- Statement `p` runs before statement `q` (`p SB q`); each runs at most
    once a cycle. -/
def taskAnn (n : Nat) : Ann (Fin n) where
  prec p q := decide (p < q)
  sbr _ _ := false
  conf p q := decide (p = q)
  share _ := false
  hasEn _ := true
  alwaysEn _ := false
  alwaysRdy _ := true

abbrev Tasks.X (n : Nat) : Iface := ⟨Fin n, inferInstance, taskAnn n⟩

/-- The hardware's task calls: the enabled statements in order, each with its
    arguments. -/
def taskTrace {n : Nat} (w : Wires (Fin n) V) : List (TaskCall V) :=
  (List.finRange n).filterMap (fun t => if w.en t then some (t.val, w.arg t) else none)

/-- A module's `n` task statements: a call appends to the cycle's log; the
    hardware's calls are `taskTrace` of the statements' wires. -/
@[reducible] def Tasks (n : Nat) : Module V (Tasks.X n) where
  S := List (TaskCall V)
  R := Empty
  meth := fun (t : Fin n) x s _ s' => s' = s ++ [(t.val, x)]
  rule := fun r => r.elim
  clean _ := []
  clean_idem _ := rfl
  Sig := Wires (Fin n) V
  hw _ w _ s' g := s' = [] ∧ g = w
  ok _ := True
  L := Unit
  log s _ := s
  trc g _ := taskTrace g
  log_clean _ _ := rfl

theorem execG_tasks {n : Nat} (o : Outs (Fin n) V) :
    ∀ (l : List (Fin n × V)) (s : List (TaskCall V)),
      ExecG (Tasks (V := V) n) o s l (s ++ l.map (fun c => (c.1.val, c.2)))
  | [], s => by rw [List.map_nil, List.append_nil]; exact .nil _
  | (t, x) :: l, s => by
    have ih := execG_tasks o l (s ++ [(t.val, x)])
    rw [List.append_assoc, List.singleton_append] at ih
    rw [List.map_cons]
    exact .cons rfl ih

/-- Two strictly increasing lists with the same members are equal. -/
theorem fin_sorted_eq {n : Nat} : ∀ (l1 l2 : List (Fin n)), l1.Pairwise (· < ·) → l2.Pairwise (· < ·) →
    (∀ x, x ∈ l1 ↔ x ∈ l2) → l1 = l2
  | [], [], _, _, _ => rfl
  | [], b :: _, _, _, h => by simpa using (h b).mpr (List.mem_cons_self ..)
  | a :: _, [], _, _, h => by simpa using (h a).mp (List.mem_cons_self ..)
  | a :: l1, b :: l2, h1, h2, h => by
    rw [List.pairwise_cons] at h1 h2
    have hab : a = b := by
      rcases List.mem_cons.mp ((h a).mp (List.mem_cons_self ..)) with e | e
      · exact e
      · rcases List.mem_cons.mp ((h b).mpr (List.mem_cons_self ..)) with e' | e'
        · exact e'.symm
        · exact absurd (Fin.lt_trans (h1.1 b e') (h2.1 a e)) (Fin.lt_irrefl _)
    subst hab
    rw [fin_sorted_eq l1 l2 h1.2 h2.2 (fun x => ⟨fun hx => ?_, fun hx => ?_⟩)]
    · rcases List.mem_cons.mp ((h x).mp (List.mem_cons_of_mem _ hx)) with e | e
      · subst e; exact absurd (h1.1 _ hx) (Fin.lt_irrefl _)
      · exact e
    · rcases List.mem_cons.mp ((h x).mpr (List.mem_cons_of_mem _ hx)) with e | e
      · subst e; exact absurd (h2.1 _ hx) (Fin.lt_irrefl _)
      · exact e

theorem filterMap_ite_eq {α β : Type} (p : α → Bool) (f : α → β) :
    ∀ (L : List α), L.filterMap (fun t => if p t then some (f t) else none) = (L.filter p).map f
  | [] => rfl
  | a :: L => by
    cases h : p a <;> simp [List.filterMap_cons, List.filter_cons, h, filterMap_ite_eq p f L]

/-- In a legal, driven sequence the statements' calls are the hardware's,
    in order. -/
theorem taskTrace_eq {n : Nat} {σ : List (List (Fin n × V))} {w : Wires (Fin n) V}
    (hL : Legal (taskAnn n) σ) (hD : Drives (taskAnn n) w σ) :
    taskTrace w = σ.flatten.map (fun c => (c.1.val, c.2)) := by
  have hsort : σ.flatten.Pairwise (fun a b => a.1 < b.1) := by
    rw [List.pairwise_iff_forall_sublist]
    intro a b hab
    have hf := hL.order a b hab
    have hne : a.1 ≠ b.1 := by
      intro e; have := hf.2.1; simp [taskAnn, e] at this
    have hnlt : ¬ b.1 < a.1 := by
      intro hlt; have := hf.1 hne; simp [taskAnn, hlt] at this
    have hv : a.1.val ≠ b.1.val := fun e => hne (Fin.ext e)
    have hv' : ¬ b.1.val < a.1.val := hnlt
    exact show a.1.val < b.1.val by omega
  have harg : ∀ c ∈ σ.flatten, c.2 = w.arg c.1 := call_arg hL hD (fun m => .inl (by simp [taskAnn]))
  have hkeys : σ.flatten.map Prod.fst = (List.finRange n).filter w.en := by
    apply fin_sorted_eq
    · exact List.pairwise_map.mpr hsort
    · exact (List.pairwise_lt_finRange n).filter _
    · intro t
      rw [List.mem_filter, List.mem_map]
      constructor
      · rintro ⟨c, hc, rfl⟩; exact ⟨List.mem_finRange _, (hD.en c.1 rfl).mpr ⟨c.2, hc⟩⟩
      · rintro ⟨-, he⟩; obtain ⟨x, hx⟩ := (hD.en t rfl).mp he; exact ⟨(t, x), hx, rfl⟩
  rw [taskTrace, filterMap_ite_eq w.en (fun t => (t.val, w.arg t)), ← hkeys, List.map_map]
  refine List.map_congr_left fun c hc => ?_
  show (c.1.val, w.arg c.1) = (c.1.val, c.2)
  rw [harg c hc]

/-- **The task statements realize their contract**: in every legal order,
    the calls are the hardware's, with its conditions and arguments. -/
theorem tasks_realizes (n : Nat) : Realizes (Tasks (V := V) n) := by
  intro s σ w o s' g hc hL hD hhw _ _
  obtain ⟨hs', hg⟩ := hhw
  have hs : s = [] := hc.symm
  subst hs hs' hg
  refine ⟨σ.map .inl, σ.flatten.map (fun c => (c.1.val, c.2)), groups_map_inl σ, ?_, rfl,
    fun _ => taskTrace_eq hL hD⟩
  apply exec_map_inl
  have := execG_tasks o σ.flatten []
  rwa [List.nil_append] at this

theorem tasks_causal [Inhabited V] (n : Nat) : Causal (Tasks (V := V) n) :=
  ⟨fun _ _ => ⟨fun _ => true, fun _ => default⟩, fun _ _ _ _ _ => ⟨rfl, rfl⟩, fun _ w _ => ⟨[], w, rfl, rfl⟩⟩

theorem tasks_aliased (toB : V → Bool) (n : Nat) : Aliased toB (Tasks (V := V) n) :=
  aliased_of_none _ _ (fun _ _ => rfl) (fun _ _ => rfl)

end Tasks


/-! ## Emission support

What a generated module file uses to keep its facts linear: constants with
their law (review integration 2, B1), conditional sites (M1), and ordering
facts from footprints, with exceptions as at-most-one groups (M2). -/

section Emit

/-! ### Constants -/

/-- Comparisons of a value with constants: `eqc x k` is `x == k`, for a
    constant `k` already truncated to `x`'s width.  bsc's operators satisfy
    the law (`eqc ⟨n, x⟩ k := x.toNat == k` on `(n : Nat) × BitVec n`), so a
    top theorem that assumes it says something about them. -/
structure EqC (V : Type) where
  eqc : V → Nat → Bool
  law : ∀ x k k', k ≠ k' → eqc x k = true → eqc x k' = true → False

/-- At most one of `bs` holds, as a chain: each is false when one before it
    holds (`acc`, the OR of those before).  Linear in `bs`, as today's
    `disjointChains`. -/
def amoChain : Bool → List Bool → Prop
  | _, [] => True
  | acc, b :: bs => (acc && b) = false ∧ amoChain (acc || b) bs

/-- **An operand's comparisons with distinct constants form an at-most-one
    chain**, from `EqC.law` alone.  A generated file states one per operand
    compared with two or more constants and hands it to `skel`. -/
theorem amoChain_of_law (E : EqC V) (x : V) : ∀ (ks : List Nat) (acc : Bool), ks.Nodup →
    (acc = true → ∀ k ∈ ks, E.eqc x k = false) → amoChain acc (ks.map (E.eqc x))
  | [], _, _, _ => trivial
  | k :: ks, acc, hnd, hacc => by
    have hk : k ∉ ks := (List.nodup_cons.mp hnd).1
    refine ⟨?_, amoChain_of_law E x ks _ (List.nodup_cons.mp hnd).2 (fun h k' hk' => ?_)⟩
    · cases acc with
      | false => rfl
      | true => simp [hacc rfl k (List.mem_cons_self ..)]
    · cases hb : E.eqc x k' with
      | false => rfl
      | true =>
        exfalso
        simp only [Bool.or_eq_true] at h
        rcases h with h1 | h1
        · have := hacc h1 k' (List.mem_cons_of_mem _ hk'); rw [hb] at this; cases this
        · exact E.law x k k' (fun e => hk (e ▸ hk')) h1 hb

/-! ### Conditional sites -/

/-- A program calls only ports in `F`, whatever the values it reads: proved
    site by site (`Within.callIf`), with no case split on conditions. -/
inductive Within {I : Type} {CM : I → Type} (F : List (Σ j, CM j)) : Prog I CM V → Prop
  | ret (v : V) : Within F (.ret v)
  | fail : Within F .fail
  | call {j : I} {m : CM j} {x : V} {k : V → Prog I CM V} :
      ⟨j, m⟩ ∈ F → (∀ v, Within F (k v)) → Within F (.call j m x k)

theorem Within.callIf {I : Type} {CM : I → Type} {F : List (Σ j, CM j)} {b : Bool} {j : I} {m : CM j}
    {x u : V} {k : V → Prog I CM V} (hm : ⟨j, m⟩ ∈ F) (hk : ∀ v, Within F (k v)) :
    Within F (Prog.callIf b j m x u k) := by
  unfold Prog.callIf
  cases b
  · exact hk u
  · exact .call hm hk

theorem Within.ite {I : Type} {CM : I → Type} {F : List (Σ j, CM j)} {b : Bool} {p q : Prog I CM V}
    (hp : Within F p) (hq : Within F q) : Within F (if b then p else q) := by
  cases b
  · exact hq
  · exact hp

/-- The calls of a trace lie in the program's footprint. -/
theorem trace_within {I : Type} {CM : I → Type} (F : List (Σ j, CM j)) (oc : (j : I) → Outs (CM j) V)
    {p : Prog I CM V} (h : Within F p) : ∀ c ∈ (trace oc p).1, (⟨c.1, c.2.1⟩ : Σ j, CM j) ∈ F := by
  induction h with
  | ret v => intro c hc; simp [trace] at hc
  | fail => intro c hc; simp [trace] at hc
  | call hm hk ih =>
    intro c hc
    simp only [trace, List.mem_cons] at hc
    rcases hc with rfl | hc
    · exact hm
    · exact ih _ c hc

/-- A program's calls, in order, form a sublist of the site list `L`,
    whatever the values it reads: proved site by site (`Seq.callIf`).  One
    pass gives both the footprint fact and, when no child occurs twice in
    `L`, the one-caller fact (`grpT_of_seq`). -/
inductive Seq {I : Type} {CM : I → Type} : List (Σ j, CM j) → Prog I CM V → Prop
  | ret (L : List (Σ j, CM j)) (v : V) : Seq L (.ret v)
  | fail (L : List (Σ j, CM j)) : Seq L .fail
  | call {L : List (Σ j, CM j)} {j : I} {m : CM j} {x : V} {k : V → Prog I CM V} :
      (∀ v, Seq L (k v)) → Seq (⟨j, m⟩ :: L) (.call j m x k)
  | skip {L : List (Σ j, CM j)} (y : Σ j, CM j) {p : Prog I CM V} : Seq L p → Seq (y :: L) p

theorem Seq.callIf {I : Type} {CM : I → Type} {L : List (Σ j, CM j)} {b : Bool} {j : I} {m : CM j}
    {x u : V} {k : V → Prog I CM V} (hk : ∀ v, Seq L (k v)) : Seq (⟨j, m⟩ :: L) (Prog.callIf b j m x u k) := by
  unfold Prog.callIf
  cases b
  · exact .skip _ (hk u)
  · exact .call hk

theorem trace_seq {I : Type} {CM : I → Type} (oc : (j : I) → Outs (CM j) V) {L : List (Σ j, CM j)}
    {p : Prog I CM V} (h : Seq L p) :
    (trace oc p).1.map (fun c => (⟨c.1, c.2.1⟩ : Σ j, CM j)) <+ L := by
  induction h with
  | ret L v => simp [trace]
  | fail L => simp [trace]
  | call hk ih => simp only [trace, List.map_cons]; exact (ih _).cons_cons _
  | skip y h ih => exact ih.cons _

/-- A program that never fails (no guard). -/
inductive NoFail {I : Type} {CM : I → Type} : Prog I CM V → Prop
  | ret (v : V) : NoFail (.ret v)
  | call {j : I} {m : CM j} {x : V} {k : V → Prog I CM V} : (∀ v, NoFail (k v)) → NoFail (.call j m x k)

theorem NoFail.callIf {I : Type} {CM : I → Type} {b : Bool} {j : I} {m : CM j} {x u : V}
    {k : V → Prog I CM V} (hk : ∀ v, NoFail (k v)) : NoFail (Prog.callIf b j m x u k) := by
  unfold Prog.callIf
  cases b
  · exact hk u
  · exact .call hk

theorem trace_noFail {I : Type} {CM : I → Type} (oc : (j : I) → Outs (CM j) V) {p : Prog I CM V}
    (h : NoFail p) : (trace oc p).2.isSome = true := by
  induction h with
  | ret v => rfl
  | call _ ih => exact ih _

theorem proj_nil_of_not_mem {I : Type} {inst : DecidableEq I} {CM : I → Type} (j : I) :
    ∀ (t : List (Σ j, CM j × V)), j ∉ t.map Sigma.fst → proj j t = []
  | [], _ => rfl
  | ⟨j', c⟩ :: t, h => by
    have hne : j' ≠ j := fun e => h (by simp [e])
    rw [proj_cons_ne _ _ hne]
    exact proj_nil_of_not_mem j t (fun h' => h (List.mem_cons_of_mem _ h'))

theorem proj_length_le {I : Type} {inst : DecidableEq I} {CM : I → Type} (j : I) :
    ∀ (t : List (Σ j, CM j × V)), (t.map Sigma.fst).Nodup → (proj j t).length ≤ 1
  | [], _ => by simp [proj]
  | ⟨j', c⟩ :: t, h => by
    have h' := List.nodup_cons.mp h
    by_cases e : j' = j
    · subst e
      rw [proj_cons_self, proj_nil_of_not_mem j' t h'.1]
      simp
    · rw [proj_cons_ne _ _ e]
      exact proj_length_le j t h'.2

theorem proj_ite {I : Type} {inst : DecidableEq I} {CM : I → Type} (j : I) (c : Prop) [Decidable c]
    (l : List (Σ j, CM j × V)) : proj j (if c then l else []) = if c then proj j l else [] := by
  split <;> rfl

theorem any_ite {α : Type} (p : α → Bool) (c : Prop) [Decidable c] (l : List α) :
    (if c then l else []).any p = (decide c && l.any p) := by
  split <;> simp_all

variable (P : Parent V)

/-- **The footprint fact from the steps' syntax** (`Cert.fp_ok`). -/
theorem fp_ok_of_within (fp : Step P → List (Σ j, (P.CI j).M))
    (hM : ∀ m x, Within (fp (.inl m)) (P.body m x)) (hR : ∀ r, Within (fp (.inr r)) (P.rbody r)) :
    ∀ oc w s, ∀ c ∈ trS P oc w s, (⟨c.1, c.2.1⟩ : Σ j, (P.CI j).M) ∈ fp s := by
  intro oc w s c hc
  rcases s with m | r
  · exact trace_within _ oc (hM m (w.arg m)) c hc
  · exact trace_within _ oc (hR r) c hc

/-- **The footprint fact from a site list.** -/
theorem fp_ok_of_seq (fp : Step P → List (Σ j, (P.CI j).M))
    (hM : ∀ m x, Seq (fp (.inl m)) (P.body m x)) (hR : ∀ r, Seq (fp (.inr r)) (P.rbody r)) :
    ∀ oc w s, ∀ c ∈ trS P oc w s, (⟨c.1, c.2.1⟩ : Σ j, (P.CI j).M) ∈ fp s := by
  intro oc w s c hc
  rcases s with m | r
  · exact (trace_seq oc (hM m (w.arg m))).subset (List.mem_map.mpr ⟨c, hc, rfl⟩)
  · exact (trace_seq oc (hR r)).subset (List.mem_map.mpr ⟨c, hc, rfl⟩)

/-- **One caller's calls from a site list in which no child occurs twice.** -/
theorem grpT_of_seq (oc : (j : P.I) → Outs (P.CI j).M V) {L : List (Σ j, (P.CI j).M)}
    {p : Prog P.I (fun j => (P.CI j).M) V} (h : Seq L p) (hL : (L.map Sigma.fst).Nodup) :
    GrpT P (trace oc p).1 := by
  intro j a b hab
  have hs := (trace_seq oc h).map Sigma.fst
  rw [List.map_map] at hs
  have hnd : ((trace oc p).1.map Sigma.fst).Nodup := hL.sublist (by simpa [Function.comp_def] using hs)
  have := hab.length_le
  have := proj_length_le (inst := inferInstance) j _ hnd
  simp at *
  omega

/-- **Calls as a Bool** (reflection): with `trace_callIf`, `proj_append`,
    `proj_ite` and `any_ite`, a step's calls of a port become a Boolean
    term in its sites' conditions, so the enable facts are `skel`
    conjuncts. -/
theorem calls_iff_any (t : List (Σ j, (P.CI j).M × V)) (j : P.I) (cm : (P.CI j).M) :
    Calls P t j cm ↔ (proj j t).any (fun c => decide (c.1 = cm)) = true := by
  unfold Calls
  constructor
  · rintro ⟨x, hx⟩
    exact List.any_eq_true.mpr ⟨(cm, x), hx, by simp⟩
  · intro h
    obtain ⟨c, hc, he⟩ := List.any_eq_true.mp h
    refine ⟨c.2, ?_⟩
    simp only [decide_eq_true_eq] at he
    rw [← he]
    exact hc

end Emit

/-! ### Steps as site lists (first order)

A generated step is a list of sites over a read environment `ρ` (`ρ n`: the
value site `n` returned, or its skipped value `u n`).  Its program is
`runSites`, a chain of `callIf`s.  The trace is proved once
(`trace_runSites`): the calls of the sites whose conditions hold, all read
at one environment, the hardware's (`envHw`), whose values are the
children's outputs or the skipped values (`envHw_at`).  So no per-design
proof substitutes values into continuations, and each per-port fact needs
only the sites on that port (`calls_callsOf`, `mem_proj_callsOf`). -/

section Sites

/-- A site: child `j`'s port `m`, called when `cond ρ`, with `arg ρ`. -/
structure Site (I : Type) (CM : I → Type) (V : Type) where
  j : I
  m : CM j
  cond : (Nat → V) → Bool
  arg : (Nat → V) → V

def setN (ρ : Nat → V) (n : Nat) (v : V) : Nat → V := fun i => if i = n then v else ρ i

variable {I : Type} {CM : I → Type} (u : Nat → V)

/-- The program of sites `n, n+1, …` with environment `ρ`, then the result. -/
def runSites (res : (Nat → V) → Option V) : List (Site I CM V) → Nat → (Nat → V) → Prog I CM V
  | [], _, ρ => match res ρ with | some v => .ret v | none => .fail
  | s :: ss, n, ρ => Prog.callIf (s.cond ρ) s.j s.m (s.arg ρ) (u n) (fun v => runSites res ss (n + 1) (setN ρ n v))

variable (oc : (j : I) → Outs (CM j) V)

/-- The hardware's environment: each site's value is the child's output when
    its condition holds, the skipped value otherwise. -/
def envHw : List (Site I CM V) → Nat → (Nat → V) → (Nat → V)
  | [], _, ρ => ρ
  | s :: ss, n, ρ => envHw ss (n + 1) (setN ρ n (if s.cond ρ then (oc s.j).ret s.m else u n))

/-- The calls of the sites whose conditions hold at `ρ`. -/
def callsOf (ρ : Nat → V) : List (Site I CM V) → List (Σ j, CM j × V)
  | [] => []
  | s :: ss => (if s.cond ρ then [⟨s.j, (s.m, s.arg ρ)⟩] else []) ++ callsOf ρ ss

/-- Site `s`, at position `n`, reads only the sites before it. -/
def ReadsBefore (n : Nat) (s : Site I CM V) : Prop :=
  ∀ ρ ρ' : Nat → V, (∀ i, i < n → ρ i = ρ' i) → s.cond ρ = s.cond ρ' ∧ s.arg ρ = s.arg ρ'

def SitesOK : List (Site I CM V) → Nat → Prop
  | [], _ => True
  | s :: ss, n => ReadsBefore n s ∧ SitesOK ss (n + 1)

theorem envHw_below : ∀ (ss : List (Site I CM V)) (n : Nat) (ρ : Nat → V) (i : Nat), i < n →
    envHw u oc ss n ρ i = ρ i
  | [], _, _, _, _ => rfl
  | s :: ss, n, ρ, i, h => by
    simp only [envHw]
    rw [envHw_below ss (n + 1) _ i (by omega)]
    simp [setN, Nat.ne_of_lt h]

/-- **The trace of a site list, proved once.** -/
theorem trace_runSites (res : (Nat → V) → Option V) : ∀ (ss : List (Site I CM V)) (n : Nat) (ρ : Nat → V),
    SitesOK ss n →
    trace oc (runSites u res ss n ρ) = (callsOf (envHw u oc ss n ρ) ss, res (envHw u oc ss n ρ))
  | [], n, ρ, _ => by
    simp only [runSites, envHw, callsOf]
    cases res ρ <;> rfl
  | s :: ss, n, ρ, ⟨hs, hss⟩ => by
    have e : envHw u oc (s :: ss) n ρ =
        envHw u oc ss (n + 1) (setN ρ n (if s.cond ρ then (oc s.j).ret s.m else u n)) := rfl
    have hag : ∀ i, i < n → ρ i = envHw u oc (s :: ss) n ρ i := by
      intro i hi
      rw [e, envHw_below u oc ss (n + 1) _ i (by omega)]
      simp [setN, Nat.ne_of_lt hi]
    obtain ⟨hc, ha⟩ := hs _ _ hag
    simp only [runSites, trace_callIf]
    rw [trace_runSites res ss (n + 1) _ hss, ← e]
    show _ = ((if s.cond (envHw u oc (s :: ss) n ρ) then [⟨s.j, (s.m, s.arg (envHw u oc (s :: ss) n ρ))⟩] else []) ++
      callsOf (envHw u oc (s :: ss) n ρ) ss, res (envHw u oc (s :: ss) n ρ))
    rw [hc, ha]

/-- **Each value of the hardware's environment**: the child's output when
    the site's condition holds there, the skipped value otherwise. -/
theorem envHw_at : ∀ (ss : List (Site I CM V)) (n : Nat) (ρ : Nat → V), SitesOK ss n →
    ∀ (i : Nat) (h : i < ss.length), envHw u oc ss n ρ (n + i) =
      if ss[i].cond (envHw u oc ss n ρ) then (oc ss[i].j).ret ss[i].m else u (n + i)
  | [], _, _, _, i, h => absurd h (Nat.not_lt_zero _)
  | s :: ss, n, ρ, ⟨hs, hss⟩, 0, h => by
    have e : envHw u oc (s :: ss) n ρ =
        envHw u oc ss (n + 1) (setN ρ n (if s.cond ρ then (oc s.j).ret s.m else u n)) := rfl
    have hag : ∀ i, i < n → ρ i = envHw u oc (s :: ss) n ρ i := by
      intro i hi
      rw [e, envHw_below u oc ss (n + 1) _ i (by omega)]
      simp [setN, Nat.ne_of_lt hi]
    have hc := (hs _ _ hag).1
    have h0 : envHw u oc (s :: ss) n ρ n = if s.cond ρ then (oc s.j).ret s.m else u n := by
      rw [e, envHw_below u oc ss (n + 1) _ n (by omega)]
      simp [setN]
    simp only [Nat.add_zero, List.getElem_cons_zero]
    rw [h0, hc]
  | s :: ss, n, ρ, ⟨hs, hss⟩, i + 1, h => by
    simp only [envHw, List.getElem_cons_succ]
    have := envHw_at ss (n + 1) (setN ρ n (if s.cond ρ then (oc s.j).ret s.m else u n)) hss i
      (by simp at h; omega)
    rw [show n + (i + 1) = n + 1 + i by omega, this]
    rfl

omit oc in
/-- Two environments give the same calls when every site taken in one is taken in the
    other with the same argument (per site, from value lemmas). -/
theorem callsOf_congr (ρ ρ' : Nat → V) : ∀ (ss : List (Site I CM V)),
    (∀ s ∈ ss, s.cond ρ = s.cond ρ' ∧ (s.cond ρ = true → s.arg ρ = s.arg ρ')) →
    callsOf ρ ss = callsOf ρ' ss
  | [], _ => rfl
  | s :: ss, h => by
    obtain ⟨hc, ha⟩ := h s (List.mem_cons_self ..)
    simp only [callsOf]
    rw [callsOf_congr ρ ρ' ss (fun s' hs' => h s' (List.mem_cons_of_mem _ hs')), ← hc]
    cases e : s.cond ρ
    · rfl
    · simp only [↓reduceIte, ha e]

/-- **A site list's calls do not depend on its skipped values** when every condition and
    argument reads only positions in `T`: below `n` (where `ρ`, `ρ'` agree) or always taken. -/
theorem callsOf_envHw_u (u' : Nat → V) (ss : List (Site I CM V)) (n : Nat) (ρ ρ' : Nat → V)
    (T : Nat → Prop) (h : SitesOK ss n)
    (hT : ∀ k, T k → k < n ∨ ∃ (i : Nat) (hi : i < ss.length), k = n + i ∧ ∀ ρ, ss[i].cond ρ = true)
    (hlo : ∀ k, k < n → ρ k = ρ' k)
    (hR : ∀ s ∈ ss, ∀ ρ1 ρ2 : Nat → V, (∀ k, T k → ρ1 k = ρ2 k) → s.cond ρ1 = s.cond ρ2 ∧ s.arg ρ1 = s.arg ρ2) :
    callsOf (envHw u oc ss n ρ) ss = callsOf (envHw u' oc ss n ρ') ss := by
  have hag : ∀ k, T k → envHw u oc ss n ρ k = envHw u' oc ss n ρ' k := by
    intro k hk
    rcases hT k hk with hk' | ⟨i, hi, rfl, hc⟩
    · rw [envHw_below u oc ss n ρ k hk', envHw_below u' oc ss n ρ' k hk']; exact hlo k hk'
    · rw [envHw_at u oc ss n ρ h i hi, envHw_at u' oc ss n ρ' h i hi, hc, hc]; rfl
  apply callsOf_congr
  intro s hs
  obtain ⟨h1, h2⟩ := hR s hs _ _ hag
  exact ⟨h1, fun _ => h2⟩

/-- **A site list's calls do not depend on its skipped values** when each site is safe: if the
    earlier sites agree on their conditions, and on their values where taken, then the site
    agrees on its condition, and on its argument where taken (review fix 2, checked per site). -/
theorem callsOf_envHw_safe (u' : Nat → V) (ss : List (Site I CM V)) (n : Nat) (ρ ρ' : Nat → V)
    (h : SitesOK ss n) (hlo : ∀ k, k < n → ρ k = ρ' k)
    (hS : ∀ (i : Nat) (hi : i < ss.length) (ρ1 ρ2 : Nat → V), (∀ k, k < n → ρ1 k = ρ2 k) →
      (∀ (j : Nat) (hj : j < i), ss[j].cond ρ1 = ss[j].cond ρ2 ∧
        (ss[j].cond ρ1 = true → ρ1 (n + j) = ρ2 (n + j))) →
      ss[i].cond ρ1 = ss[i].cond ρ2 ∧ (ss[i].cond ρ1 = true → ss[i].arg ρ1 = ss[i].arg ρ2)) :
    callsOf (envHw u oc ss n ρ) ss = callsOf (envHw u' oc ss n ρ') ss := by
  have hb : ∀ k, k < n → envHw u oc ss n ρ k = envHw u' oc ss n ρ' k := fun k hk => by
    rw [envHw_below u oc ss n ρ k hk, envHw_below u' oc ss n ρ' k hk]; exact hlo k hk
  have hP : ∀ i (hi : i < ss.length), ss[i].cond (envHw u oc ss n ρ) = ss[i].cond (envHw u' oc ss n ρ') ∧
      (ss[i].cond (envHw u oc ss n ρ) = true →
        envHw u oc ss n ρ (n + i) = envHw u' oc ss n ρ' (n + i) ∧
        ss[i].arg (envHw u oc ss n ρ) = ss[i].arg (envHw u' oc ss n ρ')) := by
    have hm : ∀ m i (hi : i < ss.length), i < m → ss[i].cond (envHw u oc ss n ρ) = ss[i].cond (envHw u' oc ss n ρ') ∧
        (ss[i].cond (envHw u oc ss n ρ) = true →
          envHw u oc ss n ρ (n + i) = envHw u' oc ss n ρ' (n + i) ∧
          ss[i].arg (envHw u oc ss n ρ) = ss[i].arg (envHw u' oc ss n ρ')) := by
      intro m
      induction m with
      | zero => intro i _ him; exact absurd him (Nat.not_lt_zero _)
      | succ m ih =>
        intro i hi him
        obtain ⟨hc, ha⟩ := hS i hi _ _ hb (fun j hj =>
          ⟨(ih j (by omega) (by omega)).1, fun hcj => ((ih j (by omega) (by omega)).2 hcj).1⟩)
        refine ⟨hc, fun hci => ⟨?_, ha hci⟩⟩
        rw [envHw_at u oc ss n ρ h i hi, envHw_at u' oc ss n ρ' h i hi, if_pos hci, if_pos (hc ▸ hci)]
    intro i hi
    exact hm (i + 1) i hi (Nat.lt_succ_self i)
  apply callsOf_congr
  intro s hs
  obtain ⟨i, hi, rfl⟩ := List.getElem_of_mem hs
  exact ⟨(hP i hi).1, fun hc => ((hP i hi).2 hc).2⟩

variable [∀ j, DecidableEq (CM j)] [DecidableEq I]

/-- Site `s` is on port `cm` of child `j`. -/
def onPort (j : I) (cm : CM j) (s : Site I CM V) : Bool :=
  if h : s.j = j then decide ((h ▸ s.m) = cm) else false

/-- **A port's calls, from its sites only.** -/
theorem mem_proj_callsOf (ρ : Nat → V) (j : I) (cm : CM j) (x : V) :
    ∀ (ss : List (Site I CM V)), (cm, x) ∈ proj j (callsOf ρ ss) →
      ∃ s ∈ ss.filter (onPort j cm), s.cond ρ = true ∧ s.arg ρ = x
  | [], h => by simp [callsOf, proj] at h
  | s :: ss, h => by
    simp only [callsOf, proj_append, List.mem_append] at h
    rcases h with h | h
    · split at h
      · rename_i hc
        rcases s with ⟨sj, sm, sc, sa⟩
        simp only at hc h ⊢
        by_cases hj : sj = j
        · subst hj
          simp [proj] at h
          obtain ⟨rfl, rfl⟩ := h
          exact ⟨⟨sj, cm, sc, sa⟩, by simp [List.filter_cons, onPort], hc, rfl⟩
        · simp [proj, hj] at h
      · simp [proj] at h
    · obtain ⟨s', hs', h1, h2⟩ := mem_proj_callsOf ρ j cm x ss h
      refine ⟨s', ?_, h1, h2⟩
      simp only [List.filter_cons]
      split
      · exact List.mem_cons_of_mem _ hs'
      · exact hs'

/-- A taken site's call is a call of its port. -/
theorem mem_proj_callsOf_of (ρ : Nat → V) (j : I) (cm : CM j) :
    ∀ (ss : List (Site I CM V)) (s : Site I CM V), s ∈ ss.filter (onPort j cm) → s.cond ρ = true →
      ∃ x, (cm, x) ∈ proj j (callsOf ρ ss)
  | [], s, h, _ => by simp at h
  | s0 :: ss, s, h, hc => by
    simp only [callsOf, proj_append, List.mem_append]
    simp only [List.filter_cons] at h
    split at h
    · rename_i ho
      rcases List.mem_cons.mp h with e | h
      · subst e
        rcases s with ⟨sj, sm, sc, sa⟩
        unfold onPort at ho
        simp only at ho hc
        split at ho
        · rename_i hj
          subst hj
          simp only [decide_eq_true_eq] at ho
          subst ho
          exact ⟨sa ρ, .inl (by simp [hc, proj])⟩
        · cases ho
      · obtain ⟨x, hx⟩ := mem_proj_callsOf_of ρ j cm ss s h hc
        exact ⟨x, .inr hx⟩
    · obtain ⟨x, hx⟩ := mem_proj_callsOf_of ρ j cm ss s h hc
      exact ⟨x, .inr hx⟩

omit [∀ j, DecidableEq (CM j)] [DecidableEq I] in
/-- The calls of a site list lie on its sites' ports. -/
theorem callsOf_ports (ρ : Nat → V) : ∀ (ss : List (Site I CM V)),
    (callsOf ρ ss).map (fun c => (⟨c.1, c.2.1⟩ : Σ j, CM j)) <+ ss.map (fun s => ⟨s.j, s.m⟩)
  | [] => by simp [callsOf]
  | s :: ss => by
    simp only [callsOf, List.map_append, List.map_cons]
    split
    · exact (callsOf_ports ρ ss).cons_cons _
    · exact (callsOf_ports ρ ss).cons _

end Sites

/-- **A family whose steps are site lists**: every skipped value is the step's own (`runSites
    v`), by construction.  Method `m` starts at site 1 with its argument at `ρ 0`; a rule at 0. -/
@[reducible] def ParentU.withSites (F : ParentU V) (ssM : F.X.M → List (Site F.I (fun j => (F.CI j).M) V))
    (resM : F.X.M → (Nat → V) → Option V) (ssR : F.PR → List (Site F.I (fun j => (F.CI j).M) V))
    (resR : F.PR → (Nat → V) → Option V) : ParentU V :=
  { F with
    body := fun m x v => runSites v (resM m) (ssM m) 1 (setN v 0 x)
    rbody := fun r v => runSites v (resR r) (ssR r) 0 v }

section Emit2
variable (P : Parent V)

/-- **A port's calls in a site-list step** (per-port enable facts). -/
theorem calls_sites (ρ : Nat → V) (ss : List (Site P.I (fun j => (P.CI j).M) V)) (j : P.I) (cm : (P.CI j).M) :
    Calls P (callsOf ρ ss) j cm ↔ ∃ s ∈ ss.filter (onPort j cm), s.cond ρ = true := by
  constructor
  · rintro ⟨x, hx⟩
    obtain ⟨s, hs, hc, -⟩ := mem_proj_callsOf ρ j cm x ss hx
    exact ⟨s, hs, hc⟩
  · rintro ⟨s, hs, hc⟩
    exact mem_proj_callsOf_of ρ j cm ss s hs hc

/-- The one-caller fact for a site-list step whose sites are on distinct children. -/
theorem grpT_sites (ρ : Nat → V) (ss : List (Site P.I (fun j => (P.CI j).M) V))
    (hnd : (ss.map (fun s => s.j)).Nodup) : GrpT P (callsOf ρ ss) := by
  intro j a b hab
  have hs := (callsOf_ports ρ ss).map Sigma.fst
  simp only [List.map_map] at hs
  have hnd' : ((callsOf ρ ss).map Sigma.fst).Nodup := hnd.sublist (by simpa [Function.comp_def] using hs)
  have := hab.length_le
  have := proj_length_le (inst := inferInstance) j _ hnd'
  simp at *
  omega

end Emit2

section Emit3
variable (P : Parent V)

/-! ### Ordering facts from footprints -/

/-- **A static port table gives the dynamic ordering fact**: traces lie in
    their footprints, so a table on footprints that implies `Follow` (for a
    `share` port, only when the argument terms agree) gives `OkT`. -/
theorem okT_of_static (okP : (j : P.I) → (P.CI j).M → (P.CI j).M → Bool)
    (hok : ∀ j (a b : (P.CI j).M × V), okP j a.1 b.1 = true → (P.CI j).ann.Follow a b)
    (t1 t2 : List (Σ j, (P.CI j).M × V)) (F1 F2 : List (Σ j, (P.CI j).M))
    (h1 : ∀ c ∈ t1, (⟨c.1, c.2.1⟩ : Σ j, (P.CI j).M) ∈ F1) (h2 : ∀ c ∈ t2, (⟨c.1, c.2.1⟩ : Σ j, (P.CI j).M) ∈ F2)
    (hs : ∀ j a b, (⟨j, a⟩ : Σ j, (P.CI j).M) ∈ F1 → (⟨j, b⟩ : Σ j, (P.CI j).M) ∈ F2 → okP j a b = true) :
    OkT P t1 t2 := by
  intro j a ha b hb
  exact hok j a b (hs j a.1 b.1 (h1 _ (mem_proj j t1 a ha)) (h2 _ (mem_proj j t2 b hb)))

/-- **`Cert.h4` from footprints, a static table and exception groups.**  Two
    firing rules whose footprints may not run in rank order are ranked the
    other way, or lie in a common exception group (`grp`): rules that never
    fire together, by an at-most-one fact on their WILL_FIRE that `skel`
    proves (bsc's disjoint guards, urgency, `mutually_exclusive`).  The
    static premise `hscan` is what the scan decides, linearly (design.md
    §9); this lemma is the part of `cert_of_scan` that consumes the
    exceptions. -/
theorem h4_of_static (rules : List P.PR) (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)
    (fp : P.PR → List (Σ j, (P.CI j).M)) (okP : (j : P.I) → (P.CI j).M → (P.CI j).M → Bool)
    (hok : ∀ j (a b : (P.CI j).M × V), okP j a.1 b.1 = true → (P.CI j).ann.Follow a b)
    (hfp : ∀ oc r, ∀ c ∈ (trR P oc r).1, (⟨c.1, c.2.1⟩ : Σ j, (P.CI j).M) ∈ fp r)
    (grp : P.PR → P.PR → Bool)
    (hgrp : ∀ oc w r1 r2, Env P oc w → grp r1 r2 = true → fire oc w r1 = true → fire oc w r2 = true → r1 = r2)
    (hscan : ∀ r1 r2, r1 ≠ r2 → grp r1 r2 = false → ¬ [r2, r1] <+ rules →
      ∀ j a b, (⟨j, a⟩ : Σ j, (P.CI j).M) ∈ fp r1 → (⟨j, b⟩ : Σ j, (P.CI j).M) ∈ fp r2 → okP j a b = true) :
    ∀ oc w r1 r2, Env P oc w → fire oc w r1 = true → fire oc w r2 = true → r1 ≠ r2 →
      ¬ OkT P (trR P oc r1).1 (trR P oc r2).1 → [r2, r1] <+ rules := by
  intro oc w r1 r2 hE h1 h2 hne hn
  cases hg : grp r1 r2 with
  | true => exact absurd (hgrp oc w r1 r2 hE hg h1 h2) hne
  | false =>
    apply Classical.byContradiction
    intro hsub
    exact hn (okT_of_static P okP hok _ _ (fp r1) (fp r2) (hfp oc r1) (hfp oc r2) (hscan r1 r2 hne hg hsub))

end Emit3

end Modular

#print axioms Modular.compose
#print axioms Modular.Built.realizes
#print axioms Modular.causal_toModule
#print axioms Modular.Built.causal
#print axioms Modular.Built.top
#print axioms Modular.Causal.restrict
#print axioms Modular.Aliased.restrict
#print axioms Modular.creg_causal
#print axioms Modular.wire_causal
#print axioms Modular.rwire_causal
#print axioms Modular.rwire0_causal
#print axioms Modular.bypass_causal
#print axioms Modular.bypass0_causal
#print axioms Modular.ubypass_realizes
#print axioms Modular.ubypass_causal
#print axioms Modular.fifo2_causal
#print axioms Modular.fifo20_causal
#print axioms Modular.fifo2_aliased
#print axioms Modular.fifo20_aliased
#print axioms Modular.amoChain_of_law
#print axioms Modular.trace_callIf
#print axioms Modular.trace_within
#print axioms Modular.fp_ok_of_within
#print axioms Modular.fp_ok_of_seq
#print axioms Modular.grpT_of_seq
#print axioms Modular.trace_noFail
#print axioms Modular.calls_iff_any
#print axioms Modular.okT_of_static
#print axioms Modular.h4_of_static
#print axioms Modular.trace_runSites
#print axioms Modular.envHw_at
#print axioms Modular.calls_sites
#print axioms Modular.mem_proj_callsOf
#print axioms Modular.grpT_sites
#print axioms Modular.Realizes.restrict
#print axioms Modular.creg_realizes
#print axioms Modular.reg_realizes
#print axioms Modular.reg_realizes_C
#print axioms Modular.wire_realizes
#print axioms Modular.rwire_realizes
#print axioms Modular.rwire0_realizes
#print axioms Modular.bypass_realizes
#print axioms Modular.bypass0_realizes
#print axioms Modular.fifo2_realizes
#print axioms Modular.fifo20_realizes
#print axioms Modular.fifoAnn_sched
#print axioms Modular.place_ok
#print axioms Modular.legal_of_pairs
#print axioms Modular.obl_of_cert

#print axioms Modular.tasks_realizes
#print axioms Modular.tasks_causal
#print axioms Modular.tasks_aliased
#print axioms Modular.Realizes.mono
#print axioms Modular.realizesU_toU
#print axioms Modular.RealizesU.at
#print axioms Modular.oblU_of_obl
#print axioms Modular.merge_exec
#print axioms Modular.groups_mergeL
#print axioms Modular.causal_toModuleU
#print axioms Modular.obl_of_cert_at
#print axioms Modular.oblU_of_cert
#print axioms Modular.obl_of_certL_at
#print axioms Modular.oblU_of_certL
#print axioms Modular.lastArg_flatten_split
#print axioms Modular.callsOf_congr
#print axioms Modular.callsOf_envHw_u
#print axioms Modular.aliased_toModuleU
#print axioms Modular.callsOf_envHw_safe
