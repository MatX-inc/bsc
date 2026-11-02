/-
Example.lean: a two-level hierarchy, composed end to end with `Modular`.

    mkTop ── c : mkChild ── a : Reg
         │              └─ b : Reg
         └─ t : Reg

* `mkChild` has internal state (registers `a`, `b`), a rule `move`
  (`b <= a`) and three methods: `getA`, `getB` (values), `setA x` (action).
  Its schedule is getB, move, getA, setA: `move` sits between `getB` and
  `setA`, so it exports `getB SBR setA` (not SB), `getA SB setA`,
  `getA CF getB`, `setA C setA`.
* `mkTop` has two rules, `rr` (`t <= c.getB`) and `rw` (`c.setA(inc c.getA)`,
  one rule calling two child methods), and two value methods `get` (`t`) and
  `peek` (`c.getA`), exported CF.

The values are an abstract type `V` with the design's operators as
parameters (`ToyOps`): every theorem holds for every `V` and every
interpretation of the operators.  Each module's obligation is proved from a
certificate (`Modular.Cert`, the facts a generated file emits), the
registers' contracts are proved in `Modular`, and `Top.realizes` is the top
module's contract, by `Built.realizes`.

`Asm` is a module with a user assertion (`mutually_exclusive up down`, as in
the repository's `AssertedSchedules`), two parallel muxes, and two methods
exported C that share a child port (`q.enq`).  Its certificate's Boolean
facts are proved by one `bv_decide` over an abstract `V` (`Asm.skel`).

Negative cases (each must fail to prove; here the failure is a theorem):
(a) `ChildSB`: `mkChild` exporting `getB SB setA`.  Its obligation is false
    (`ChildSB.not_obl`), so no certificate exists (`ChildSB.no_cert`), and
    the module itself violates the contract (`ChildSB.not_realizes`).
(b) `PutBad`: a parent whose argument mux for `c.setA` selects the wrong
    caller's argument (`PutBad.not_obl`); the right mux passes (`Put.obl`).
(c) `Order`: a parent exporting `peek CF setP` while the child requires
    `getA SB setA` (`Order.not_obl`); `peek SB setP` passes (`Order.obl`).
(d) `NoRdy`: a rule whose CAN_FIRE omits the child FIFO's RDY.
(e) `ShareArg`: two rules calling one copy of a child value method with
    different arguments.
(f) `NoAlw`: an always_enabled child port (`BypassWire.wset`) not called in
    every cycle.
(g) `AsmNo`: `Asm` without its assertion.
(h) `Loop`: a combinational loop through an RWire.  The obligation holds,
    but no hardware cycle exists, and no acyclicity certificate does
    (`Loop.no_acyc`), so the module is not `Built`.
(i) `AlwEnq`: an always_enabled method enqueueing into a FIFO that may be
    full.
(j) `AlwRdy`: an always_ready method whose guard can fail.

Revision 3 adds: `WireEx` (a child with a combinational path, ranked by its
acyclicity certificate; `WireEx.top` gives the contract and a hardware cycle
for every input), `AliasEx` (FIFO2's shared output wires: an explicit
`notFull` read against `comb`'s `i_notFull` wire), and `Fsm`/`FsmNo`
(constants as `EqC` with its law; the at-most-one chain is the only fact
relating the FSM's state comparisons).  Every built module now also carries
its acyclicity certificate (`acyc`), and `Top.top`, `Asm.top` give both
halves of `Built.top`.

System tasks: `Show` is a parent whose rule `show` is
`if (isOn c.getB) $display(c.getA)`, the display a port of its `Tasks 1`
child; `Show.top` is its contract with the task trace, `Show.display` the
trace end to end.  Negatives:
(k) `ShowLate`: the display's read of `c.getA` placed after a `c.setA` in
    the same rule.
(l) `ShowCond`: the display's enable omits its condition.
(m) `TaskOrder`: two displays whose statement order is the reverse of the
    order the child's annotations force.
`TaskRev.not_realizes`: a task module whose hardware prints in reverse
order violates the contract itself.

Build: `lean -o build/Modular.olean Modular.lean`, then
`LEAN_PATH=build lean Example.lean`.
-/
import Modular
import Std.Tactic.BVDecide
open Modular
open scoped List

set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false

namespace Ex

/-- Child calls, projected and listed: the part of a trace computation that
    does not depend on the design. -/
macro "pj" : tactic => `(tactic| simp only [proj_cons_self, proj_cons_ne, proj_nil, ne_eq, reduceCtorEq,
  not_false_eq_true, List.mem_cons, List.not_mem_nil, or_false, forall_eq_or_imp, forall_eq,
  implies_true, false_implies, and_true, true_and, false_and, and_false, false_or, Prod.mk.injEq] at *)

/-- The certificate's step predicates, unfolded. -/
macro "unf" : tactic => `(tactic| simp only [Calls, On, Av, trS, called, Avail] at *)

theorem fin1 (i : Fin 1) : i = 0 := by
  obtain ⟨k, hk⟩ := i; congr; omega

/-- The acyclicity certificate's read facts, unfolded: each hypothesis
    becomes equations between the two sides' signals. -/
macro "dep" : tactic => `(tactic| (simp only [List.mem_cons, List.mem_singleton, List.not_mem_nil, or_false,
  forall_eq_or_imp, forall_eq, implies_true, false_implies, AgreeO, AgreeW, true_and, and_true] at *))

/-- A register has no combinational paths. -/
theorem reg_nopath (p q : CRegM 1) : (cregAnn 1).path p q = false := by
  cases p <;> cases q <;> rename_i a b <;> rw [fin1 a, fin1 b] <;> rfl

/-- No aliases among a child's outputs. -/
macro "noal" : tactic => `(tactic| (intro j; (try cases j) <;> apply aliasOK_of_none <;> intros <;> rfl))

/-- The design's operators, abstract: the dummy argument of argument-less
    calls and the one datapath operator the toy uses. -/
structure ToyOps (V : Type) where
  z : V
  inc : V → V

/-- The register's write wires: enable and data; reads take the dummy. -/
def regW {V : Type} (z : V) (en : Bool) (d : V) : Wires (CRegM 1) V :=
  ⟨fun m => match m with | .wr _ => en | .rd _ => false, fun m => match m with | .wr _ => d | .rd _ => z⟩

/-! ## mkChild -/
namespace Child

/-- Instances: `a`, `b`, both `Reg`. -/
inductive K where
  | a | b
  deriving DecidableEq

inductive M where
  | getA | getB | setA
  deriving DecidableEq

inductive R where
  | move
  deriving DecidableEq

/-- The exported annotations, by method name (bsc's schedule output). -/
def ann : Ann M where
  prec p q := match p, q with
    | .getA, .setA => true   -- getA SB setA
    | .getB, .setA => true   -- getB SBR setA
    | _, _ => false
  sbr p q := match p, q with
    | .getB, .setA | .setA, .getB => true
    | _, _ => false
  conf p q := match p, q with
    | .setA, .setA => true   -- setA C setA
    | _, _ => false
  share m := match m with | .setA => false | _ => true
  hasEn m := match m with | .setA => true | _ => false
  alwaysEn _ := false
  alwaysRdy _ := true

abbrev X : Iface := ⟨M, inferInstance, ann⟩
abbrev CI : K → Iface := fun _ => Reg.X

section
variable {V : Type} (ops : ToyOps V)

/-- Methods, as calls of the children's ports. -/
def body : M → V → Prog K (fun j => (CI j).M) V
  | .getA, _ => .call .a Reg.read ops.z .ret
  | .getB, _ => .call .b Reg.read ops.z .ret
  | .setA, x => .call .a Reg.write x (fun _ => .ret ops.z)

/-- rule move: b <= a -/
def rbody : R → Prog K (fun j => (CI j).M) V
  | .move => .call .a Reg.read ops.z (fun v => .call .b Reg.write v (fun _ => .ret ops.z))

/-- The hardware: enables, argument muxes, outputs (WILL_FIRE_move = 1). -/
def comb (oc : (j : K) → Outs (CI j).M V) (w : Wires M V) : ((j : K) → Wires (CI j).M V) × Outs M V :=
  (fun j => match j with
    | .a => regW ops.z (w.en .setA) (w.arg .setA)
    | .b => regW ops.z true ((oc .a).ret Reg.read),
   ⟨fun _ => true, fun m => match m with
    | .getA => (oc .a).ret Reg.read
    | .getB => (oc .b).ret Reg.read
    | .setA => ops.z⟩)

abbrev P : Parent V where
  I := K
  decI := inferInstance
  kids := [.a, .b]
  kids_nodup := by decide
  kids_all := fun j => by cases j <;> decide
  CI := CI
  X := X
  PR := R
  body := body ops
  rbody := rbody ops
  comb := comb ops
  asm _ _ := True
  onehot _ _ := True

variable (oc : (j : K) → Outs (CI j).M V) (w : Wires M V)
theorem trM_getA : trM (P ops) oc w .getA = ([⟨.a, (Reg.read, ops.z)⟩], some ((oc .a).ret Reg.read)) := rfl
theorem trM_getB : trM (P ops) oc w .getB = ([⟨.b, (Reg.read, ops.z)⟩], some ((oc .b).ret Reg.read)) := rfl
theorem trM_setA : trM (P ops) oc w .setA = ([⟨.a, (Reg.write, w.arg .setA)⟩], some ops.z) := rfl
theorem trR_move : trR (P ops) oc .move =
    ([⟨.a, (Reg.read, ops.z)⟩, ⟨.b, (Reg.write, (oc .a).ret Reg.read)⟩], some ops.z) := rfl
end

/-- Each step's footprint: the child ports it may call. -/
def fp : M ⊕ R → List (Σ j, (CI j).M)
  | .inl .getA => [⟨.a, Reg.read⟩]
  | .inl .getB => [⟨.b, Reg.read⟩]
  | .inl .setA => [⟨.a, Reg.write⟩]
  | .inr .move => [⟨.a, Reg.read⟩, ⟨.b, Reg.write⟩]

/-- Each child port's users. -/
def users : (j : K) → (CI j).M → List (M ⊕ R)
  | .a, .rd _ => [.inl .getA, .inr .move]
  | .a, .wr _ => [.inl .setA]
  | .b, .rd _ => [.inl .getB]
  | .b, .wr _ => [.inr .move]

macro "tr" : tactic => `(tactic| ((try simp only [trM_getA, trM_getB, trM_setA, trR_move] at *); (try pj)))
macro "an" : tactic => `(tactic| simp [CI, Reg.X, CReg.X, cregAnn, Ann.Follow, Ann.Group, X, ann] at *)
macro "okt" : tactic => `(tactic| (intro j; cases j <;> (tr <;> an <;> done)))
macro "grpt" : tactic => `(tactic| (intro j a b h; cases j <;> tr <;>
  first | exact absurd h pair_sub_single | exact absurd h pair_sub_nil))

section
variable {V : Type} (ops : ToyOps V)

/-- The certificate: the rule list, which rules fire, which callers' methods
    must precede which rules (`getB` before `move`), the footprints, and the
    facts. -/
def cert : Cert (P ops) where
  rules := [.move]
  rules_nodup := by simp
  fire _ _ _ := true
  hit _ _ m _ := match m with | .getB => true | _ => false
  fp := fp
  users := users
  users_ok s := by cases s <;> rename_i x <;> cases x <;> simp [fp, users]
  fp_ok oc w s c hc := by
    cases s <;> rename_i x <;> cases x <;> unf <;> tr <;> (first | (rcases hc with rfl | rfl) | subst hc) <;>
      simp [fp]
  selfAnn m := by cases m <;> simp [X, ann]
  fireIn _ _ r _ := by cases r; simp
  resM oc w m _ _ := by cases m <;> rfl
  resR oc w r _ _ := by cases r; rfl
  rdyM oc w m _ _ c _ h := by an
  rdyR oc w r _ _ c _ h := by an
  grpM oc w m _ _ := by cases m <;> grpt
  grpR oc w r _ _ := by cases r; grpt
  mm oc w m1 m2 _ _ _ hf := by cases m1 <;> cases m2 <;> first | okt | (exfalso; an)
  mmg oc w m1 m2 _ _ _ hf := by
    cases m1 <;> cases m2 <;> first | (intro j; cases j <;> (tr <;> an <;> done)) | (exfalso; an)
  h1 oc w m r _ _ _ hn := by cases m <;> cases r <;> first | rfl | (exfalso; apply hn; okt)
  h2 oc w m r1 r2 _ _ _ _ hh _ := by cases r1; cases r2; exact hh
  h3 oc w m m' r _ _ _ _ hh hn := by
    cases m
    · exact absurd hh (by simp)
    · cases m' <;> cases r <;> first | (exfalso; apply hn; okt) | exact ⟨nofun, .inl ⟨rfl, rfl⟩⟩
    · exact absurd hh (by simp)
  h4 oc w r1 r2 _ _ _ hne := by cases r1; cases r2; exact absurd rfl hne
  en oc w j cm _ hcm := by
    cases j <;> cases cm with
    | rd i => an
    | wr i =>
      rw [fin1 i]
      simp [users, comb, regW, X, ann]
      unf; tr; simp [X, ann]
  enV oc w m _ h1 h2 _ c hc := by
    cases m <;> tr <;> simp [X, ann] at h1 <;> subst hc <;> rfl
  arg oc w j cm _ s hs _ x hx := by
    cases j <;> cases cm <;> simp only [users, List.mem_cons, List.not_mem_nil, or_false] at hs <;>
      (first | (rcases hs with rfl | rfl) | subst hs) <;> unf <;> tr <;>
      (rcases hx with ⟨-, rfl⟩; rfl)
  alw oc w j cm _ h := by an
  onehot _ _ _ := trivial

/-- **mkChild's obligation.** -/
theorem obl : Obl (P ops) := obl_of_cert (P ops) (cert ops)

/-- **mkChild's headline theorem, in the modular form**: for any
    implementations of its children's interfaces that realize theirs, the
    module realizes its own contract.  This is what a generated file states;
    it mentions the children only through `CI`. -/
theorem realizes_of (impl : (j : K) → Module V (CI j)) (hC : ∀ j, Realizes (impl j)) :
    Realizes ((P ops).toModule impl) :=
  compose (P ops) impl hC (fun j => aliased_of_none _ _ (fun _ _ => rfl) (fun _ _ => rfl)) (obl ops)

/-- What `comb` reads (the netlist's fan-in), for the acyclicity
    certificate: `b`'s write data reads `a`; `a`'s write reads `setA`'s
    wires; `getA`, `getB` return the registers.  The registers have no
    paths, so no ranks are needed. -/
def acyc : Acyc (P ops) :=
  Acyc.ofMoore (P ops)
    (fun j p => match j, p with | .b, .wr _ => [⟨.a, Reg.read⟩] | _, _ => [])
    (fun j p => match j, p with | .a, .wr _ => [.setA] | _, _ => [])
    (fun q => match q with | .getA => [⟨.a, Reg.read⟩] | .getB => [⟨.b, Reg.read⟩] | .setA => [])
    (fun _ => [])
    (by intro oc oc' w w' j p hD hW; cases j <;> cases p <;> dep <;> simp [comb, regW, *])
    (by intro oc oc' w w' q hD hW; cases q <;> dep <;> simp [comb, *])
    (fun j p q => reg_nopath p q) (fun _ _ h => by simp at h)

/-- mkChild, built from two registers. -/
abbrev impl : (j : K) → Module V (CI j) := fun _ => Reg
abbrev mod : Module V X := (P ops).toModule impl

theorem built : Built X (mod ops) :=
  Built.node (P ops) impl (fun _ => Built.leaf _ reg_realizes (creg_causal 1))
    (fun _ => aliased_of_none _ _ (fun _ _ => rfl) (fun _ _ => rfl)) (obl ops) (acyc ops)

theorem realizes : Realizes (mod ops) := (built ops).realizes

end
end Child


/-- The toy's operators on `Nat`, for the counterexamples. -/
def natOps : ToyOps Nat := ⟨0, (· + 1)⟩

/-- A register's read may not follow its write. -/
theorem reg_rw {V : Type} (u v : V) : ¬ (cregAnn 1).Follow (CRegM.wr 0, u) (CRegM.rd 0, v) := by
  intro h; have := h.1 (by simp); simp [cregAnn] at this

theorem mem_groups_of_inl {α β : Type} : ∀ {τ : List (α ⊕ β)} {g : α}, .inl g ∈ τ → g ∈ groups τ
  | [], g, h => by simp at h
  | .inl a :: τ, g, h => by
    rcases List.mem_cons.mp h with e | e
    · cases e; simp [groups]
    · simp [groups, mem_groups_of_inl e]
  | .inr b :: τ, g, h => by
    rcases List.mem_cons.mp h with e | e
    · cases e
    · simp [groups, mem_groups_of_inl e]

/-! ## (a) mkChild exporting `getB SB setA`: the rule `move` is sandwiched -/
namespace ChildSB
open Child (K M R CI impl)

def annSB : Ann M := { Child.ann with sbr := fun _ _ => false }
abbrev XSB : Iface := ⟨M, inferInstance, annSB⟩

abbrev P : Parent Nat :=
  { Child.P natOps with X := XSB }

/-- One caller calls `getB` and then `setA 5`, legal under SB. -/
def g : List (M × Nat) := [(.getB, 0), (.setA, 5)]

theorem legal_g : Legal P.X.ann [g] := by
  refine ⟨fun c1 c2 hs => ?_, fun g' hg c1 c2 hs => ?_, fun m hm => by cases m <;> simp [annSB, Child.ann] at hm⟩
  · simp only [g, List.flatten_cons, List.flatten_nil, List.append_nil] at hs
    obtain ⟨rfl, rfl⟩ := pair_sub_two hs
    simp [Ann.Follow, annSB, Child.ann]
  · simp only [List.mem_singleton] at hg
    subst hg
    simp only [g] at hs
    obtain ⟨rfl, rfl⟩ := pair_sub_two hs
    simp [Ann.Group, Ann.Follow, annSB, Child.ann]

def w5 : Wires M Nat := ⟨fun m => match m with | .setA => true | _ => false,
  fun m => match m with | .setA => 5 | _ => 0⟩

theorem drives_g : Drives P.X.ann w5 [g] := by
  refine ⟨fun m hm => ?_, fun m x hx => ?_⟩
  · cases m <;> simp [annSB, Child.ann] at hm
    simp [w5, g]
  · cases m <;> simp [g, lastArg] at hx <;> subst hx <;> rfl

/-- **The obligation is false.**  The hardware fires `move`, so `b.write` is
    enabled and some step of any parent order is `move`.  Before the
    caller's group it puts `b.read` after `b.write`; after it, `a.read`
    after `a.write`: both break a register's `read SB write`. -/
theorem not_obl : ¬ Obl P := by
  intro h
  let oc : (j : K) → Outs (CI j).M Nat := fun _ => ⟨fun _ => true, fun _ => 0⟩
  obtain ⟨-, τP, hg, _, hj⟩ := h oc w5 [g] trivial (by noal) legal_g drives_g (fun c _ h => by simp [annSB, Child.ann] at h)
  have hgin : (.inl g : List (M × Nat) ⊕ R) ∈ τP := inl_mem_of_groups (by rw [hg]; simp)
  obtain ⟨x, hx⟩ := ((hj .b).2.1.en (.wr 0) rfl).mp rfl
  obtain ⟨l, hl, hxl⟩ := List.mem_flatten.mp hx
  obtain ⟨e, he, rfl⟩ := List.mem_map.mp hl
  have trg_b : proj K.b (stepTrace P oc (.inl g)) = [(Reg.read, 0)] := rfl
  have trg_a : proj K.a (stepTrace P oc (.inl g)) = [(Reg.write, 5)] := rfl
  have trm_b : proj K.b (stepTrace P oc (.inr .move)) = [(Reg.write, 0)] := rfl
  have trm_a : proj K.a (stepTrace P oc (.inr .move)) = [(Reg.read, 0)] := rfl
  have hmove : e = .inr .move := by
    rcases e with g' | r
    · have : g' = g := by
        have := mem_groups_of_inl he
        rw [hg] at this; simpa using this
      subst this
      rw [trg_b] at hxl; cases List.mem_singleton.mp hxl
    · cases r; rfl
  subst hmove
  rcases either_sublist he hgin (by simp) with hs | hs
  · exact reg_rw _ _ ((hj .b).1.order _ _ (pair_flatten_map (fun e => proj K.b (stepTrace P oc e)) hs
      (x := (Reg.write, 0)) (y := (Reg.read, 0)) (by rw [trm_b]; exact List.mem_singleton.mpr rfl) (by rw [trg_b]; exact List.mem_singleton.mpr rfl)))
  · exact reg_rw _ _ ((hj .a).1.order _ _ (pair_flatten_map (fun e => proj K.a (stepTrace P oc e)) hs
      (x := (Reg.write, 5)) (y := (Reg.read, 0)) (by rw [trg_a]; exact List.mem_singleton.mpr rfl) (by rw [trm_a]; exact List.mem_singleton.mpr rfl)))

/-- Hence no certificate exists: the generated file would not check. -/
theorem no_cert : Cert P → False := fun C => not_obl (obl_of_cert P C)

/-! The module itself violates the contract: not just the proof method. -/

abbrev mod : Module Nat XSB := P.toModule impl

def gSB : List (M × Nat) := [(.getB, 0), (.setA, 2)]

/-- One rule step: `move` copies `a` into `b`. -/
theorem move_step {s s1 : mod.S} {r : mod.R} (h : mod.rule r s s1) : s1 .a = s .a ∧ s1 .b = s .a := by
  rcases r with r | ⟨j, r⟩
  · cases r
    obtain ⟨v, hv⟩ := h
    change RunP P impl (.call .a Reg.read 0 (fun v => .call .b Reg.write v (fun _ => .ret 0))) s v s1 at hv
    cases hv with
    | call h1 h2 =>
      obtain ⟨e1, e2⟩ := h1
      cases h2 with
      | call h3 h4 =>
        cases h4
        subst e1 e2
        refine ⟨?_, ?_⟩
        · rw [upd_ne _ _ (by decide), upd_same]
        · rw [upd_same]; exact h3
  · exact r.elim

/-- After any run of rules: `a` unchanged, `b` unchanged or copied from `a`. -/
theorem rules_inv (o : Outs M Nat) : ∀ (ρ : List mod.R) (s t : mod.S), Exec mod o s (ρ.map .inr) t →
    t .a = s .a ∧ (t .b = s .b ∨ t .b = s .a)
  | [], s, t, h => by
    have : s = t := by cases h; rfl
    subst this; exact ⟨rfl, .inl rfl⟩
  | r :: ρ, s, t, h => by
    cases h with
    | rule hr hrest =>
      rename_i s1
      obtain ⟨ha, hb⟩ := move_step hr
      obtain ⟨ha', hb'⟩ := rules_inv o ρ s1 t hrest
      refine ⟨ha'.trans ha, .inr ?_⟩
      rcases hb' with e | e
      · exact e.trans hb
      · exact e.trans ha

/-- `a = 1`, `b = 0`; one caller calls `getB` then `setA 2`.  The hardware
    returns `getB = 0` and ends with `a = 2`, `b = 1` (move fired).  No
    one-at-a-time run does both: `move` before the caller makes `getB`
    return 1; after it, `b` ends 2. -/
theorem not_realizes : ¬ Realizes mod := by
  intro h
  let s : mod.S := fun j => match j with | .a => (1 : Nat) | .b => (0 : Nat)
  let w : Wires M Nat := ⟨fun m => match m with | .setA => true | _ => false,
    fun m => match m with | .setA => 2 | _ => 0⟩
  let oc : (j : K) → Outs (CI j).M Nat := fun j => ⟨fun _ => true, fun _ => s j⟩
  let s' : mod.S := fun j => match j with | .a => (2 : Nat) | .b => (1 : Nat)
  let g : mod.Sig := ⟨w, oc, (P.comb oc w).1, fun _ => ()⟩
  have hhw : mod.hw s w (P.comb oc w).2 s' g :=
    ⟨rfl, fun j => by cases j <;> exact ⟨fun i => by rw [fin1 i]; rfl, by rfl⟩,
      fun _ => rfl, rfl, fun _ => ⟨rfl, rfl⟩⟩
  have hL : Legal XSB.ann [gSB] := by
    refine ⟨fun c1 c2 hs => ?_, fun g' hg c1 c2 hs => ?_, fun m hm => by cases m <;> simp [annSB, Child.ann] at hm⟩
    · simp only [gSB, List.flatten_cons, List.flatten_nil, List.append_nil] at hs
      obtain ⟨rfl, rfl⟩ := pair_sub_two hs
      simp [Ann.Follow, annSB, Child.ann]
    · simp only [List.mem_singleton] at hg
      subst hg
      simp only [gSB] at hs
      obtain ⟨rfl, rfl⟩ := pair_sub_two hs
      simp [Ann.Group, Ann.Follow, annSB, Child.ann]
  have hD : Drives XSB.ann w [gSB] := by
    refine ⟨fun m hm => ?_, fun m x hx => ?_⟩
    · cases m <;> simp [annSB, Child.ann] at hm
      simp [w, gSB]
    · cases m <;> simp [gSB, lastArg] at hx <;> subst hx <;> rfl
  obtain ⟨τ, fin, hg, hex, hs', -⟩ := h s [gSB] w _ s' g rfl hL hD hhw ⟨trivial, fun _ => trivial⟩
    (fun c _ h => by simp [annSB, Child.ann] at h)
  obtain ⟨ρ, post, rfl, hpost⟩ := groups_cons_split τ gSB [] hg
  obtain ⟨ρ', rfl⟩ := groups_nil_rules post hpost
  obtain ⟨u, h1, h2⟩ := exec_split _ _ _ _ hex
  cases h2 with
  | grp hgx hrest =>
    rename_i v
    obtain ⟨hua, hub⟩ := rules_inv _ ρ s u h1
    obtain ⟨hfa, hfb⟩ := rules_inv _ ρ' v fin hrest
    -- the caller's group: getB returns the hardware's 0, then setA 2
    rw [show gSB = [(.getB, 0), (.setA, 2)] from rfl] at hgx
    cases hgx with
    | cons m1 r1 =>
      rename_i u1
      cases r1 with
      | cons m2 r2 =>
        rename_i u2
        have hv := execG_nil_eq _ _ r2
        subst hv
        change RunP P impl (.call .b Reg.read 0 .ret) u _ u1 at m1
        change RunP P impl (.call .a Reg.write 2 (fun _ => .ret 0)) u1 _ u2 at m2
        cases m1 with
        | call k1 k2 =>
          obtain ⟨e1, e2⟩ := k1
          cases k2
          cases m2 with
          | call k3 k4 =>
            cases k4
            have hb0 : u .b = 0 := e1.symm.trans (by rfl)
            have hfin : fin .b = 0 ∨ fin .b = 2 := by
              rcases hfb with e | e
              · left; rw [e, upd_ne _ _ (by decide), upd_same, e2, hb0]
              · right; rw [e, upd_same]; exact k3
            have hb : (1 : Nat) = fin .b := congrFun hs' .b
            rcases hfin with e | e <;> rw [e] at hb <;> exact absurd hb (by decide)

end ChildSB

/-! ## mkTop -/
namespace Top

/-- Instances: `c : mkChild`, `t : Reg`. -/
inductive K where
  | c | t
  deriving DecidableEq

inductive M where
  | get | peek
  deriving DecidableEq

inductive R where
  | rr | rw
  deriving DecidableEq

/-- Two value methods, conflict-free. -/
def ann : Ann M where
  prec _ _ := false
  sbr _ _ := false
  conf _ _ := false
  share _ := true
  hasEn _ := false
  alwaysEn _ := false
  alwaysRdy _ := true

abbrev X : Iface := ⟨M, inferInstance, ann⟩
abbrev CI : K → Iface
  | .c => Child.X
  | .t => Reg.X

/-- mkChild's input wires: `setA`'s enable and data; value ports take the dummy. -/
def childW {V : Type} (z : V) (en : Bool) (d : V) : Wires Child.M V :=
  ⟨fun m => match m with | .setA => en | _ => false, fun m => match m with | .setA => d | _ => z⟩

section
variable {V : Type} (ops : ToyOps V)

def body : M → V → Prog K (fun j => (CI j).M) V
  | .get, _ => .call .t Reg.read ops.z .ret
  | .peek, _ => .call .c Child.M.getA ops.z .ret

/-- rule rr: t <= c.getB;  rule rw: c.setA(inc c.getA) -/
def rbody : R → Prog K (fun j => (CI j).M) V
  | .rr => .call .c Child.M.getB ops.z (fun v => .call .t Reg.write v (fun _ => .ret ops.z))
  | .rw => .call .c Child.M.getA ops.z (fun v => .call .c Child.M.setA (ops.inc v) (fun _ => .ret ops.z))

/-- WILL_FIRE_rr = WILL_FIRE_rw = 1. -/
def comb (oc : (j : K) → Outs (CI j).M V) (w : Wires M V) : ((j : K) → Wires (CI j).M V) × Outs M V :=
  (fun j => match j with
    | .c => childW ops.z true (ops.inc ((oc .c).ret Child.M.getA))
    | .t => regW ops.z true ((oc .c).ret Child.M.getB),
   ⟨fun _ => true, fun m => match m with
    | .get => (oc .t).ret Reg.read
    | .peek => (oc .c).ret Child.M.getA⟩)

abbrev P : Parent V where
  I := K
  decI := inferInstance
  kids := [.c, .t]
  kids_nodup := by decide
  kids_all := fun j => by cases j <;> decide
  CI := CI
  X := X
  PR := R
  body := body ops
  rbody := rbody ops
  comb := comb ops
  asm _ _ := True
  onehot _ _ := True

variable (oc : (j : K) → Outs (CI j).M V) (w : Wires M V)
theorem trM_get : trM (P ops) oc w .get = ([⟨.t, (Reg.read, ops.z)⟩], some ((oc .t).ret Reg.read)) := rfl
theorem trM_peek : trM (P ops) oc w .peek = ([⟨.c, (.getA, ops.z)⟩], some ((oc .c).ret .getA)) := rfl
theorem trR_rr : trR (P ops) oc .rr =
    ([⟨.c, (.getB, ops.z)⟩, ⟨.t, (Reg.write, (oc .c).ret .getB)⟩], some ops.z) := rfl
theorem trR_rw : trR (P ops) oc .rw =
    ([⟨.c, (.getA, ops.z)⟩, ⟨.c, (.setA, ops.inc ((oc .c).ret .getA))⟩], some ops.z) := rfl
end

def fp : M ⊕ R → List (Σ j, (CI j).M)
  | .inl .get => [⟨.t, Reg.read⟩]
  | .inl .peek => [⟨.c, Child.M.getA⟩]
  | .inr .rr => [⟨.c, Child.M.getB⟩, ⟨.t, Reg.write⟩]
  | .inr .rw => [⟨.c, Child.M.getA⟩, ⟨.c, Child.M.setA⟩]

def users : (j : K) → (CI j).M → List (M ⊕ R)
  | .c, .getA => [.inl .peek, .inr .rw]
  | .c, .getB => [.inr .rr]
  | .c, .setA => [.inr .rw]
  | .t, .rd _ => [.inl .get]
  | .t, .wr _ => [.inr .rr]

macro "tr" : tactic => `(tactic| ((try simp only [trM_get, trM_peek, trR_rr, trR_rw] at *); (try pj)))
macro "an" : tactic => `(tactic| simp [CI, Reg.X, CReg.X, cregAnn, Ann.Follow, Ann.Group, X, ann,
  Child.X, Child.ann] at *)
macro "okt" : tactic => `(tactic| (intro j; cases j <;> (tr <;> an <;> done)))
macro "grpt" : tactic => `(tactic| (intro j a b h; cases j <;> tr <;>
  first | exact absurd h pair_sub_single | exact absurd h pair_sub_nil |
    (have hp := pair_sub_two h; rcases hp with ⟨h1, h2⟩; subst h1 h2; an)))

section
variable {V : Type} (ops : ToyOps V)

/-- The certificate.  `get` must precede `rr` (`t.read SB t.write`) and so
    `rw` (`rw` may not precede `rr`: `getB SBR setA`); `peek` must precede `rw`
    (`getA SB setA`). -/
def cert : Cert (P ops) where
  rules := [.rr, .rw]
  rules_nodup := by simp
  fire _ _ _ := true
  hit _ _ m r := match m, r with
    | .get, _ => true
    | .peek, .rw => true
    | .peek, .rr => false
  fp := fp
  users := users
  users_ok s := by cases s <;> rename_i x <;> cases x <;> simp [fp, users]
  fp_ok oc w s c hc := by
    cases s <;> rename_i x <;> cases x <;> unf <;> tr <;> (first | (rcases hc with rfl | rfl) | subst hc) <;>
      simp [fp]
  selfAnn m := .inr rfl
  fireIn _ _ r _ := by cases r <;> simp
  resM oc w m _ _ := by cases m <;> rfl
  resR oc w r _ _ := by cases r <;> rfl
  rdyM oc w m _ _ c hc h := by cases m <;> tr <;> subst hc <;> an
  rdyR oc w r _ _ c hc h := by cases r <;> tr <;> (rcases hc with rfl | rfl) <;> an
  grpM oc w m _ _ := by cases m <;> grpt
  grpR oc w r _ _ := by cases r <;> grpt
  mm oc w m1 m2 _ _ _ _ := by cases m1 <;> cases m2 <;> okt
  mmg oc w m1 m2 _ _ _ _ := by cases m1 <;> cases m2 <;> (intro j; cases j <;> (tr <;> an <;> done))
  h1 oc w m r _ _ _ hn := by cases m <;> cases r <;> first | rfl | (exfalso; apply hn; okt)
  h2 oc w m r1 r2 _ _ _ _ hh hn := by
    cases m <;> cases r1 <;> cases r2 <;> first | rfl | (simp at hh; done) | (exfalso; apply hn; okt)
  h3 oc w m m' r _ _ _ _ _ hn := by
    exfalso; apply hn; cases m' <;> cases r <;> okt
  h4 oc w r1 r2 _ _ _ hne hn := by
    cases r1 <;> cases r2 <;> first | exact absurd rfl hne | exact List.Sublist.refl _ | (exfalso; apply hn; okt)
  en oc w j cm _ hcm := by
    cases j
    · cases cm <;> first | (an; done) | skip
      simp [users, comb, childW]; unf; tr; simp
    · cases cm with
      | rd i => an
      | wr i =>
        rw [fin1 i]
        simp [users, comb, regW]; unf; tr; simp
  enV oc w m _ _ _ _ c hc := by cases m <;> tr <;> subst hc <;> rfl
  arg oc w j cm _ s hs _ x hx := by
    cases j <;> cases cm <;> simp only [users, List.mem_cons, List.not_mem_nil, or_false] at hs <;>
      (first | (rcases hs with rfl | rfl) | subst hs) <;> unf <;> tr <;>
      first | (rcases hx with ⟨-, rfl⟩; rfl) | (simp at hx; done) |
        (rcases hx with ⟨-, rfl⟩ | ⟨-, rfl⟩ <;> rfl)
  alw oc w j cm _ h := by cases j <;> cases cm <;> an
  onehot _ _ _ := trivial

/-- **mkTop's obligation.** -/
theorem obl : Obl (P ops) := obl_of_cert (P ops) (cert ops)

abbrev impl : (j : K) → Module V (CI j)
  | .c => Child.mod ops
  | .t => Reg

abbrev mod : Module V X := (P ops).toModule (impl ops)

/-- What `comb` reads: `c.setA`'s data reads `c.getA`, `t`'s write data
    reads `c.getB`; `get` and `peek` return `t` and `c.getA`.  mkChild
    exports no paths (`Child.X`'s `path` is empty, as its own certificate
    checks), so no ranks are needed. -/
def acyc : Acyc (P ops) :=
  Acyc.ofMoore (P ops)
    (fun j p => match j, p with
      | .c, .setA => [⟨.c, Child.M.getA⟩] | .t, .wr _ => [⟨.c, Child.M.getB⟩] | _, _ => [])
    (fun _ _ => [])
    (fun q => match q with | .get => [⟨.t, Reg.read⟩] | .peek => [⟨.c, Child.M.getA⟩])
    (fun _ => [])
    (by intro oc oc' w w' j p hD hW; cases j <;> cases p <;> dep <;> simp [comb, regW, childW, *])
    (by intro oc oc' w w' q hD hW; cases q <;> dep <;> simp [comb, *])
    (fun j p q => by cases j <;> first | exact reg_nopath p q | rfl) (fun _ _ h => by simp at h)

/-- The instance tree. -/
theorem built : Built X (mod ops) :=
  Built.node (P ops) (impl ops) (fun j => match j with
    | .c => Child.built ops
    | .t => Built.leaf _ reg_realizes (creg_causal 1))
    (fun j => aliased_of_none _ _ (fun p q => by cases j <;> rfl) (fun a p => by cases j <;> rfl))
    (obl ops) (acyc ops)

/-- **The top module realizes its contract**, for every value type and every
    interpretation of the operators: from the registers' contracts and the
    two modules' obligations. -/
theorem realizes : Realizes (mod ops) := (built ops).realizes

/-- **And it is not vacuous**: every input has a hardware cycle, from every
    state a cycle can start in (`Built.top`, from the registers' causality
    and the two acyclicity certificates). -/
theorem top [Inhabited V] : Realizes (mod ops) ∧
    ∀ s w, (mod ops).clean s = s → ∃ o s' g, (mod ops).hw s w o s' g := (built ops).top

end
end Top


/-! ## (b) The argument mux: two callers of `c.setA`

A method `put x` calls `c.setA x`; the rule `rw` calls `c.setA (c.getA + 1)`
and does not fire when `put` is enabled.  The right mux selects `put`'s
argument when `EN_put`; the wrong one selects it when `rw` fires. -/
namespace Put

inductive K where
  | c
  deriving DecidableEq

inductive M where
  | put
  deriving DecidableEq

inductive R where
  | rw
  deriving DecidableEq

def ann : Ann M where
  prec _ _ := false
  sbr _ _ := false
  conf _ _ := true
  share _ := false
  hasEn _ := true
  alwaysEn _ := false
  alwaysRdy _ := true

abbrev X : Iface := ⟨M, inferInstance, ann⟩
abbrev CI : K → Iface := fun _ => Child.X
abbrev Pg := Prog K (fun j => (CI j).M) Nat

def body : M → Nat → Pg
  | .put, x => .call .c .setA x (fun _ => .ret 0)

def rbody : R → Pg
  | .rw => .call .c .getA 0 (fun v => .call .c .setA (v + 1) (fun _ => .ret 0))

/-- `good`: the mux selects put's argument when `EN_put` (right);
    otherwise when `WILL_FIRE_rw = !EN_put` (the wrong caller). -/
def comb (good : Bool) (oc : (j : K) → Outs (CI j).M Nat) (w : Wires M Nat) :
    ((j : K) → Wires (CI j).M Nat) × Outs M Nat :=
  (fun _ => Top.childW 0 true (if (if good then w.en .put else !w.en .put) then w.arg .put
      else (oc .c).ret .getA + 1),
   ⟨fun _ => true, fun _ => 0⟩)

abbrev P (good : Bool) : Parent Nat where
  I := K
  decI := inferInstance
  kids := [.c]
  kids_nodup := by decide
  kids_all := fun j => by cases j; decide
  CI := CI
  X := X
  PR := R
  body := body
  rbody := rbody
  comb := comb good
  asm _ _ := True
  onehot _ _ := True

section
variable (oc : (j : (P true).I) → Outs ((P true).CI j).M Nat) (w : Wires (P true).X.M Nat)
theorem trM_put : trM (P true) oc w .put = ([⟨.c, (.setA, w.arg .put)⟩], some 0) := rfl
theorem trR_rw : trR (P true) oc .rw =
    ([⟨.c, (.getA, 0)⟩, ⟨.c, (.setA, (oc .c).ret .getA + 1)⟩], some 0) := rfl
end

def fp : M ⊕ R → List (Σ j, (CI j).M)
  | .inl .put => [⟨.c, .setA⟩]
  | .inr .rw => [⟨.c, .getA⟩, ⟨.c, .setA⟩]

def users : (j : K) → (CI j).M → List (M ⊕ R)
  | .c, .setA => [.inl .put, .inr .rw]
  | .c, .getA => [.inr .rw]
  | .c, .getB => []

macro "tr" : tactic => `(tactic| ((try simp only [trM_put, trR_rw] at *); (try pj)))
macro "an" : tactic => `(tactic| simp [CI, Ann.Follow, Ann.Group, X, ann, Child.X, Child.ann] at *)

/-- With the right mux: WILL_FIRE_rw = !EN_put, so `put` and `rw` never
    both run and nothing needs placing. -/
def cert : Cert (P true) where
  rules := [.rw]
  rules_nodup := by decide
  fire _ w _ := !w.en .put
  hit _ _ _ _ := false
  fp := fp
  users := users
  users_ok s := by cases s <;> rename_i x <;> cases x <;> decide
  fp_ok oc w s c hc := by
    cases s <;> rename_i x <;> cases x <;> unf <;> tr <;> (first | (rcases hc with rfl | rfl) | subst hc) <;>
      simp [fp]
  selfAnn _ := .inl rfl
  fireIn _ _ r _ := by cases r; simp
  resM oc w m _ _ := by cases m; rfl
  resR oc w r _ _ := by cases r; rfl
  rdyM oc w m _ _ c _ h := by an
  rdyR oc w r _ _ c _ h := by an
  grpM oc w m _ _ := by
    cases m; intro j a b h; cases j; tr; exact absurd h pair_sub_single
  grpR oc w r _ _ := by
    cases r; intro j a b h; cases j; tr; obtain ⟨rfl, rfl⟩ := pair_sub_two h; an
  mm oc w m1 m2 _ _ _ hf := by cases m1; cases m2; exfalso; an
  mmg oc w m1 m2 _ _ _ hf := by cases m1; cases m2; exfalso; an
  h1 oc w m r _ ha hr _ := by
    cases m; have := ha.2 rfl; simp [this] at hr
  h2 oc w m r1 r2 _ _ _ _ hh _ := by simp at hh
  h3 oc w m m' r _ _ _ _ hh _ := by simp at hh
  h4 oc w r1 r2 _ _ _ hne := by cases r1; cases r2; exact absurd rfl hne
  en oc w j cm _ hcm := by
    cases j; cases cm <;> first | (an; done) | skip
    simp only [users, List.mem_cons, List.not_mem_nil, or_false, exists_eq_or_imp, exists_eq_left]
    unf; tr
    cases he : w.en .put <;> simp [he, comb, Top.childW, X, ann]
  enV oc w m _ h1 _ _ := by cases m; simp [X, ann] at h1
  arg oc w j cm _ s hs hav x hx := by
    cases j; cases cm <;> simp only [users, List.mem_cons, List.not_mem_nil, or_false] at hs <;>
      (first | (rcases hs with rfl | rfl) | subst hs | skip) <;> unf <;> tr
    · rcases hx with ⟨-, rfl⟩; rfl
    · have he := hav.2 rfl
      rcases hx with ⟨-, rfl⟩; simp [comb, Top.childW, he]
    · have hn : w.en .put = false := by simpa using hav
      rcases hx with ⟨-, rfl⟩; simp [comb, Top.childW, hn]
  alw oc w j cm _ h := by cases j; cases cm <;> an
  onehot _ _ _ := trivial

theorem obl : Obl (P true) := obl_of_cert (P true) cert

end Put

namespace PutBad
open Put (K M R CI)

/-- **The wrong mux fails.**  `put 5` with `c.getA = 1`: the mux drives
    `c.setA`'s data with `rw`'s 2, but the only legal sequence for `c`
    calls `setA` once, with 5. -/
theorem not_obl : ¬ Obl (Put.P false) := by
  intro h
  let oc : (j : K) → Outs (CI j).M Nat := fun _ => ⟨fun _ => true, fun _ => 1⟩
  let w : Wires M Nat := ⟨fun _ => true, fun _ => 5⟩
  let g : List (M × Nat) := [(.put, 5)]
  have hL : Legal (Put.P false).X.ann [g] :=
    ⟨fun c1 c2 hs => absurd hs pair_sub_single, fun g' hg c1 c2 hs => by
      simp only [List.mem_singleton] at hg; subst hg; exact absurd hs pair_sub_single,
     fun m hm => by simp [Put.ann] at hm⟩
  have hD : Drives (Put.P false).X.ann w [g] :=
    ⟨fun m _ => by cases m; simp [w, g], fun m x hx => by cases m; simp [g, lastArg] at hx; exact hx⟩
  obtain ⟨-, τP, hg, _, hj⟩ := h oc w [g] trivial (by noal) hL hD (fun c _ h => by simp [Put.ann] at h)
  have hgin : (.inl g : List (M × Nat) ⊕ R) ∈ τP := inl_mem_of_groups (by rw [hg]; simp)
  have h5 : (Child.M.setA, 5) ∈ (sigmaJ (Put.P false) oc τP .c).flatten :=
    List.mem_flatten.mpr ⟨_, List.mem_map.mpr ⟨_, hgin, rfl⟩, List.mem_singleton.mpr rfl⟩
  obtain ⟨y, hy⟩ := lastArg_isSome _ _ _ h5
  have h2 : y = 2 := ((hj .c).2.1.arg _ y hy).symm
  subst h2
  have hy' := lastArg_mem _ _ _ hy
  rcases either_sublist h5 hy' (by simp) with hs | hs
  · have := ((hj .c).1.order _ _ hs).2.1; simp [Child.ann] at this
  · have := ((hj .c).1.order _ _ hs).2.1; simp [Child.ann] at this

end PutBad

/-! ## (c) A parent order the child's annotations forbid

`peek` calls `c.getA`, `setP x` calls `c.setA x`.  The child exports
`getA SB setA`, so the parent must export `peek SB setP` (`Order.P true`);
exporting `peek CF setP` (`Order.P false`) fails. -/
namespace Order

inductive K where
  | c
  deriving DecidableEq

inductive M where
  | peek | setP
  deriving DecidableEq

def ann (sb : Bool) : Ann M where
  prec p q := match p, q with | .peek, .setP => sb | _, _ => false
  sbr _ _ := false
  conf p q := match p, q with | .setP, .setP => true | _, _ => false
  share m := match m with | .peek => true | _ => false
  hasEn m := match m with | .setP => true | _ => false
  alwaysEn _ := false
  alwaysRdy _ := true

abbrev X (sb : Bool) : Iface := ⟨M, inferInstance, ann sb⟩
abbrev CI : K → Iface := fun _ => Child.X
abbrev Pg := Prog K (fun j => (CI j).M) Nat

def body : M → Nat → Pg
  | .peek, _ => .call .c .getA 0 .ret
  | .setP, x => .call .c .setA x (fun _ => .ret 0)

def comb (oc : (j : K) → Outs (CI j).M Nat) (w : Wires M Nat) : ((j : K) → Wires (CI j).M Nat) × Outs M Nat :=
  (fun _ => Top.childW 0 (w.en .setP) (w.arg .setP),
   ⟨fun _ => true, fun m => match m with | .peek => (oc .c).ret .getA | .setP => 0⟩)

abbrev P (sb : Bool) : Parent Nat where
  I := K
  decI := inferInstance
  kids := [.c]
  kids_nodup := by decide
  kids_all := fun j => by cases j; decide
  CI := CI
  X := X sb
  PR := Empty
  body := body
  rbody := fun r => r.elim
  comb := comb
  asm _ _ := True
  onehot _ _ := True

section
variable (oc : (j : (P true).I) → Outs ((P true).CI j).M Nat) (w : Wires (P true).X.M Nat)
theorem trM_peek : trM (P true) oc w .peek = ([⟨.c, (.getA, 0)⟩], some ((oc .c).ret .getA)) := rfl
theorem trM_setP : trM (P true) oc w .setP = ([⟨.c, (.setA, w.arg .setP)⟩], some 0) := rfl
end

def fp : M ⊕ Empty → List (Σ j, (CI j).M)
  | .inl .peek => [⟨.c, .getA⟩]
  | .inl .setP => [⟨.c, .setA⟩]

def users : (j : K) → (CI j).M → List (M ⊕ Empty)
  | .c, .getA => [.inl .peek]
  | .c, .setA => [.inl .setP]
  | .c, .getB => []

macro "tr" : tactic => `(tactic| ((try simp only [trM_peek, trM_setP] at *); (try pj)))
macro "an" : tactic => `(tactic| simp [CI, Ann.Follow, Ann.Group, X, ann, Child.X, Child.ann] at *)

def cert : Cert (P true) where
  rules := []
  rules_nodup := List.nodup_nil
  fire _ _ _ := false
  hit _ _ _ _ := false
  fp := fp
  users := users
  users_ok s := by rcases s with m | r; cases m <;> decide; exact r.elim
  fp_ok oc w s c hc := by
    rcases s with m | r
    · cases m <;> unf <;> tr <;> subst hc <;> simp [fp]
    · exact r.elim
  selfAnn m := by cases m <;> decide
  fireIn _ _ r _ := r.elim
  resM oc w m _ _ := by cases m <;> rfl
  resR oc w r _ := r.elim
  rdyM oc w m _ _ c _ h := by an
  rdyR oc w r _ := r.elim
  grpM oc w m _ _ := by cases m <;> (intro j a b h; cases j; tr; exact absurd h pair_sub_single)
  grpR oc w r _ := r.elim
  mm oc w m1 m2 _ _ _ hf := by
    cases m1 <;> cases m2 <;> first | (intro j; cases j; tr; an; done) | (exfalso; an)
  mmg oc w m1 m2 _ _ _ hf := by
    cases m1 <;> cases m2 <;> first | (intro j; cases j; tr; an; done) | (exfalso; an)
  h1 oc w m r _ _ hr := by simp at hr
  h2 oc w m r1 r2 _ _ hr := by simp at hr
  h3 oc w m m' r _ _ _ hr := by simp at hr
  h4 oc w r1 r2 _ hr := by simp at hr
  en oc w j cm _ hcm := by
    cases j; cases cm <;> first | (an; done) | skip
    simp only [users, List.mem_cons, List.not_mem_nil, or_false, exists_eq_or_imp, exists_eq_left]
    unf; tr; simp [comb, Top.childW, ann]
  enV oc w m _ h1 h2 _ c hc := by
    cases m <;> tr <;> simp_all [ann, Child.ann]
  arg oc w j cm _ s hs _ x hx := by
    cases j; cases cm <;> simp only [users, List.mem_cons, List.not_mem_nil, or_false] at hs <;>
      (first | subst hs | skip) <;> unf <;> tr
    · rcases hx with ⟨-, rfl⟩; rfl
    · rcases hx with ⟨-, rfl⟩; rfl
  alw oc w j cm _ h := by cases j; cases cm <;> an
  onehot _ _ _ := trivial

/-- With `peek SB setP`, the obligation holds. -/
theorem obl : Obl (P true) := obl_of_cert (P true) cert

/-- **With `peek CF setP` it fails**: `setP 5` then `peek` is legal for the
    parent but makes `c.getA` follow `c.setA`. -/
theorem not_obl : ¬ Obl (P false) := by
  intro h
  let oc : (j : K) → Outs (CI j).M Nat := fun _ => ⟨fun _ => true, fun _ => 0⟩
  let w : Wires M Nat := ⟨fun m => match m with | .setP => true | _ => false,
    fun m => match m with | .setP => 5 | _ => 0⟩
  let g1 : List (M × Nat) := [(.setP, 5)]
  let g2 : List (M × Nat) := [(.peek, 0)]
  have hL : Legal (P false).X.ann [g1, g2] := by
    refine ⟨fun c1 c2 hs => ?_, fun g' hg c1 c2 hs => ?_, fun m hm => by cases m <;> simp [ann] at hm⟩
    · simp only [g1, g2, List.flatten_cons, List.flatten_nil, List.append_nil, List.singleton_append] at hs
      obtain ⟨rfl, rfl⟩ := pair_sub_two hs
      simp [Ann.Follow, ann]
    · simp only [List.mem_cons, List.not_mem_nil, or_false] at hg
      rcases hg with rfl | rfl <;> exact absurd hs pair_sub_single
  have hD : Drives (P false).X.ann w [g1, g2] := by
    refine ⟨fun m hm => ?_, fun m x hx => ?_⟩
    · cases m <;> simp [ann] at hm
      simp [w, g1, g2]
    · cases m <;> simp [g1, g2, lastArg] at hx <;> subst hx <;> rfl
  obtain ⟨-, τP, hg, _, hj⟩ := h oc w [g1, g2] trivial (by noal) hL hD (fun c _ h => by simp [ann] at h)
  have hs : [(.inl g1 : List (M × Nat) ⊕ Empty), .inl g2] <+ τP := by
    have := groups_sublist τP
    rw [hg] at this
    exact this
  have := (hj .c).1.order _ _ (pair_flatten_map (fun e => proj K.c (stepTrace (P false) oc e)) hs
    (x := (.setA, 5)) (y := (.getA, 0)) (List.mem_singleton.mpr rfl) (List.mem_singleton.mpr rfl))
  have := this.1 (by simp)
  simp [Child.ann] at this

end Order


/-! ## A module with a user assertion and parallel muxes

As in the repository's `AssertedSchedules.bsv`: rule `up` (`cnt <= inc cnt`
when `isOne mode`) and rule `down` (`cnt <= dec cnt` when `isMax cnt`) are
asserted `mutually_exclusive`, so bsc fires each on its own CAN_FIRE and
drives `cnt`'s data with an AND-OR mux.  The assertion is not a fact of the
logic (`mode` and `cnt` are independent), so it is a per-cycle assumption
(`asm`), not a hypothesis of the obligation.  Methods `put1 x` and `put2 x`,
exported `put1 C put2`, both enqueue into the FIFO `q`, whose data is an
AND-OR mux of the two arguments.  Both muxes are read as priority muxes;
the certificate proves them one-hot, from the assertion and from the
exported C pair. -/
namespace Asm

/-- The operators the module uses, abstract. -/
structure Ops (V : Type) where
  z : V
  inc : V → V
  dec : V → V
  isOne : V → Bool
  isMax : V → Bool
  toB : V → Bool

inductive K where
  | cnt | mode | q
  deriving DecidableEq

inductive M where
  | put1 | put2
  deriving DecidableEq

inductive R where
  | up | down
  deriving DecidableEq

/-- `put1 C put2`, and each C with itself; guarded by `q`'s RDY. -/
def ann : Ann M where
  prec _ _ := false
  sbr _ _ := false
  conf _ _ := true
  share _ := false
  hasEn _ := true
  alwaysEn _ := false
  alwaysRdy _ := false

abbrev X : Iface := ⟨M, inferInstance, ann⟩
abbrev CI : K → Iface
  | .cnt => Reg.X
  | .mode => Reg.X
  | .q => FIFO2.X

section
variable {V : Type} (ops : Ops V)

def body : M → V → Prog K (fun j => (CI j).M) V
  | _, x => .call .q FifoM.enq x (fun _ => .ret ops.z)

def rbody : R → Prog K (fun j => (CI j).M) V
  | .up => .call .mode Reg.read ops.z (fun m => if ops.isOne m then
      .call .cnt Reg.read ops.z (fun c => .call .cnt Reg.write (ops.inc c) (fun _ => .ret ops.z)) else .fail)
  | .down => .call .cnt Reg.read ops.z (fun c => if ops.isMax c then
      .call .cnt Reg.write (ops.dec c) (fun _ => .ret ops.z) else .fail)

/-- WILL_FIRE = CAN_FIRE for both rules (bsc relies on the assertion). -/
def fire (oc : (j : K) → Outs (CI j).M V) (w : Wires M V) : R → Bool
  | .up => ops.isOne ((oc .mode).ret Reg.read)
  | .down => ops.isMax ((oc .cnt).ret Reg.read)

/-- The hardware.  `cnt`'s data and `q`'s data are AND-OR muxes in bsc's
    Verilog, read here as priority muxes. -/
def comb (oc : (j : K) → Outs (CI j).M V) (w : Wires M V) : ((j : K) → Wires (CI j).M V) × Outs M V :=
  (fun j => match j with
    | .cnt => regW ops.z (ops.isOne ((oc .mode).ret Reg.read) || ops.isMax ((oc .cnt).ret Reg.read))
        (if ops.isOne ((oc .mode).ret Reg.read) then ops.inc ((oc .cnt).ret Reg.read)
          else ops.dec ((oc .cnt).ret Reg.read))
    | .mode => regW ops.z false ops.z
    | .q => ⟨fun p => match p with | .enq => w.en .put1 || w.en .put2 | _ => false,
             fun p => match p with | .enq => if w.en .put1 then w.arg .put1 else w.arg .put2 | _ => ops.z⟩,
   ⟨fun _ => (oc .q).rdy FifoM.enq, fun _ => ops.z⟩)

abbrev P : Parent V where
  I := K
  decI := inferInstance
  kids := [.cnt, .mode, .q]
  kids_nodup := by decide
  kids_all := fun j => by cases j <;> decide
  CI := CI
  X := X
  PR := R
  body := body ops
  rbody := rbody ops
  comb := comb ops
  -- `mutually_exclusive up down`
  asm oc _ := (ops.isOne ((oc .mode).ret Reg.read) && ops.isMax ((oc .cnt).ret Reg.read)) = false
  -- the selectors of `q`'s and `cnt`'s data muxes
  onehot oc w := (w.en .put1 && w.en .put2) = false ∧
    (ops.isOne ((oc .mode).ret Reg.read) && ops.isMax ((oc .cnt).ret Reg.read)) = false
  toB := ops.toB

variable (oc : (j : K) → Outs (CI j).M V) (w : Wires M V)

theorem trM_put (m : M) : trM (P ops) oc w m = ([⟨.q, (FifoM.enq, w.arg m)⟩], some ops.z) := by
  cases m <;> rfl
theorem trR_up (h : ops.isOne ((oc .mode).ret Reg.read) = true) : trR (P ops) oc .up =
    ([⟨.mode, (Reg.read, ops.z)⟩, ⟨.cnt, (Reg.read, ops.z)⟩,
      ⟨.cnt, (Reg.write, ops.inc ((oc .cnt).ret Reg.read))⟩], some ops.z) := by
  simp [trR, rbody, trace, h]
theorem trR_up' (h : ops.isOne ((oc .mode).ret Reg.read) = false) : trR (P ops) oc .up =
    ([⟨.mode, (Reg.read, ops.z)⟩], none) := by
  simp [trR, rbody, trace, h]
theorem trR_down (h : ops.isMax ((oc .cnt).ret Reg.read) = true) : trR (P ops) oc .down =
    ([⟨.cnt, (Reg.read, ops.z)⟩, ⟨.cnt, (Reg.write, ops.dec ((oc .cnt).ret Reg.read))⟩], some ops.z) := by
  simp [trR, rbody, trace, h]
theorem trR_down' (h : ops.isMax ((oc .cnt).ret Reg.read) = false) : trR (P ops) oc .down =
    ([⟨.cnt, (Reg.read, ops.z)⟩], none) := by
  simp [trR, rbody, trace, h]

/-- **The Boolean skeleton of the certificate, by one `bv_decide`** over an
    abstract `V`: the data comparisons `isOne _`, `isMax _` and the enables
    are atoms.  From the environment (the assertion, and `put1 C put2`):
    both muxes' selectors are one-hot, and whichever step calls a port, its
    selector is the one on.  A generated file proves its module's skeleton
    this way, in one call, and each certificate field picks its conjunct. -/
theorem skel (hE : EnvC (P ops) oc w) :
    (w.en .put1 && w.en .put2) = false ∧
    (ops.isOne ((oc .mode).ret Reg.read) && ops.isMax ((oc .cnt).ret Reg.read)) = false ∧
    (ops.isMax ((oc .cnt).ret Reg.read) = true → ops.isOne ((oc .mode).ret Reg.read) = false) ∧
    (w.en .put2 = true → w.en .put1 = false) := by
  obtain ⟨hasm, -, hC⟩ := hE
  have h12 := hC .put1 .put2 nofun
  change ((true && w.en .put1) || false) = true → ((true && w.en .put2) || false) = true → true = false at h12
  change (ops.isOne ((oc .mode).ret Reg.read) && ops.isMax ((oc .cnt).ret Reg.read)) = false at hasm
  bv_decide
end

def fp : M ⊕ R → List (Σ j, (CI j).M)
  | .inl _ => [⟨.q, FifoM.enq⟩]
  | .inr .up => [⟨.mode, Reg.read⟩, ⟨.cnt, Reg.read⟩, ⟨.cnt, Reg.write⟩]
  | .inr .down => [⟨.cnt, Reg.read⟩, ⟨.cnt, Reg.write⟩]

def users : (j : K) → (CI j).M → List (M ⊕ R)
  | .q, .enq => [.inl .put1, .inl .put2]
  | .cnt, .rd _ => [.inr .up, .inr .down]
  | .cnt, .wr _ => [.inr .up, .inr .down]
  | .mode, .rd _ => [.inr .up]
  | _, _ => []

macro "lst" : tactic => `(tactic| simp only [List.mem_cons, List.not_mem_nil, or_false] at *)

section
variable {V : Type} (ops : Ops V)

def cert : Cert (P ops) where
  rules := [.up, .down]
  rules_nodup := by simp
  fire := fire ops
  hit _ _ _ _ := false
  fp := fp
  users := users
  users_ok s := by cases s <;> rename_i x <;> cases x <;> simp [fp, users]
  fp_ok oc w s c hc := by
    rcases s with m | r
    · simp only [trS, trM_put, List.mem_singleton] at hc; subst hc; simp [fp]
    · cases r
      · cases h : ops.isOne ((oc .mode).ret Reg.read)
        · simp only [trS, trR_up' ops oc h, List.mem_singleton] at hc; subst hc; simp [fp]
        · simp only [trS, trR_up ops oc h] at hc; lst
          rcases hc with rfl | rfl | rfl <;> simp [fp]
      · cases h : ops.isMax ((oc .cnt).ret Reg.read)
        · simp only [trS, trR_down' ops oc h, List.mem_singleton] at hc; subst hc; simp [fp]
        · simp only [trS, trR_down ops oc h] at hc; lst
          rcases hc with rfl | rfl <;> simp [fp]
  selfAnn _ := .inl rfl
  fireIn _ _ r _ := by cases r <;> simp
  resM oc w m _ _ := by rw [trM_put]; rfl
  resR oc w r _ hr := by
    cases r
    · rw [trR_up ops oc hr]; rfl
    · rw [trR_down ops oc hr]; rfl
  rdyM oc w m _ ha c hc h := by
    rw [trM_put, List.mem_singleton] at hc
    subst hc
    rcases ha.1 with h1 | h1
    · simp [X, ann] at h1
    · exact h1
  rdyR oc w r _ hr c hc h := by
    cases r
    · rw [trR_up ops oc hr] at hc; lst
      rcases hc with rfl | rfl | rfl <;> simp [Reg.X, CReg.X, cregAnn] at h
    · rw [trR_down ops oc hr] at hc; lst
      rcases hc with rfl | rfl <;> simp [Reg.X, CReg.X, cregAnn] at h
  grpM oc w m _ _ := by
    intro j a b h; rw [trM_put] at h
    cases j <;> pj <;> first | exact absurd h pair_sub_single | exact absurd h pair_sub_nil
  grpR oc w r _ hr := by
    intro j a b h
    cases r
    · rw [trR_up ops oc hr] at h
      cases j <;> pj <;> first | exact absurd h pair_sub_single | exact absurd h pair_sub_nil |
        (obtain ⟨rfl, rfl⟩ := pair_sub_two h; simp [Reg.X, CReg.X, cregAnn, Ann.Group, Ann.Follow])
    · rw [trR_down ops oc hr] at h
      cases j <;> pj <;> first | exact absurd h pair_sub_single | exact absurd h pair_sub_nil |
        (obtain ⟨rfl, rfl⟩ := pair_sub_two h; simp [Reg.X, CReg.X, cregAnn, Ann.Group, Ann.Follow])
  mm oc w m1 m2 _ _ _ hf := by exfalso; simp [Ann.Follow, X, ann] at hf
  mmg oc w m1 m2 _ _ _ hf := by exfalso; simp [Ann.Group, Ann.Follow, X, ann] at hf
  h1 oc w m r _ _ hr hn := by
    exfalso; apply hn
    intro j a ha b hb
    rw [trM_put] at hb
    cases r
    · rw [trR_up ops oc hr] at ha
      cases j <;> pj
    · rw [trR_down ops oc hr] at ha
      cases j <;> pj
  h2 oc w m r1 r2 _ _ _ _ hh _ := by simp at hh
  h3 oc w m m' r _ _ _ _ hh _ := by simp at hh
  h4 oc w r1 r2 hE h1 h2 hne _ := by
    have := (skel ops oc w hE.toC).2.1
    cases r1 <;> cases r2
    · exact absurd rfl hne
    · simp only [fire] at h1 h2; simp [h1, h2] at this
    · simp only [fire] at h1 h2; simp [h1, h2] at this
    · exact absurd rfl hne
  en oc w j cm hE hcm := by
    cases j <;> cases cm
    case cnt.rd i => simp [Reg.X, CReg.X, cregAnn] at hcm
    case mode.rd i => simp [Reg.X, CReg.X, cregAnn] at hcm
    case cnt.wr i =>
      rw [fin1 i]
      simp only [users, List.mem_cons, List.not_mem_nil, or_false, exists_eq_or_imp, exists_eq_left]
      unf
      cases h1 : ops.isOne ((oc .mode).ret Reg.read) <;> cases h2 : ops.isMax ((oc .cnt).ret Reg.read) <;>
        simp [comb, regW, fire, h1, h2, trR_up ops oc, trR_up' ops oc, trR_down ops oc, trR_down' ops oc,
          proj_cons_self, proj_cons_ne]
    case mode.wr i => simp [users, comb, regW]
    all_goals first | (simp [FIFO2.X, fifoAnn] at hcm; done) | skip
    case q.enq =>
      simp only [users, List.mem_cons, List.not_mem_nil, or_false, exists_eq_or_imp, exists_eq_left]
      unf
      simp [comb, trM_put, X, ann, proj_cons_self]
    all_goals simp [users, comb]
  enV oc w m _ h1 _ _ := by simp [X, ann] at h1
  arg oc w j cm hE s hs hav x hx := by
    have sk := skel ops oc w hE.toC
    cases j <;> cases cm <;> simp only [users] at hs <;> lst
    case cnt.rd i =>
      rcases hs with rfl | rfl <;> unf <;> simp only [fire] at hav
      · rw [trR_up ops oc hav] at hx; pj; rcases hx with ⟨-, rfl⟩; rfl
      · rw [trR_down ops oc hav] at hx; pj; rcases hx with ⟨-, rfl⟩; rfl
    case cnt.wr i =>
      rcases hs with rfl | rfl <;> unf <;> simp only [fire] at hav
      · rw [trR_up ops oc hav] at hx; pj
        rcases hx with ⟨-, rfl⟩; simp [comb, regW, hav]
      · rw [trR_down ops oc hav] at hx; pj
        rcases hx with ⟨-, rfl⟩; simp [comb, regW, sk.2.2.1 hav]
    case mode.rd i =>
      subst hs; unf; simp only [fire] at hav
      rw [trR_up ops oc hav] at hx; pj; rcases hx with ⟨-, rfl⟩; rfl
    case q.enq =>
      rcases hs with rfl | rfl <;> unf
      · rw [trM_put] at hx; pj; rcases hx with ⟨-, rfl⟩
        simp [comb, hav.2 rfl]
      · rw [trM_put] at hx; pj; rcases hx with ⟨-, rfl⟩
        simp [comb, sk.2.2.2 (hav.2 rfl)]
  alw oc w j cm _ h := by cases j <;> cases cm <;> simp [Reg.X, CReg.X, cregAnn, FIFO2.X, fifoAnn] at h
  onehot oc w hE := ⟨(skel ops oc w hE).1, (skel ops oc w hE).2.1⟩

/-- **The module's obligation**, for every `V` and every interpretation of
    its operators. -/
theorem obl : Obl (P ops) := obl_of_cert (P ops) (cert ops)

abbrev impl (ofBool : Bool → V) : (j : K) → Module V (CI j)
  | .cnt => Reg
  | .mode => Reg
  | .q => FIFO2 ofBool

abbrev mod (ofBool : Bool → V) : Module V X := (P ops).toModule (impl ofBool)

/-- What `comb` reads: `cnt`'s write reads `mode` and `cnt`; `q.enq` reads
    the methods' wires; RDY reads `q`'s RDY.  The children have no paths. -/
def acyc : Acyc (P ops) :=
  Acyc.ofMoore (P ops)
    (fun j p => match j, p with
      | .cnt, .wr _ => [⟨.mode, Reg.read⟩, ⟨.cnt, Reg.read⟩] | _, _ => [])
    (fun j p => match j, p with | .q, .enq => [.put1, .put2] | _, _ => [])
    (fun _ => [⟨.q, FifoM.enq⟩])
    (fun _ => [])
    (by
      intro oc oc' w w' j p hD hW
      cases j <;> cases p <;> dep <;> simp [comb, regW, *])
    (by intro oc oc' w w' q hD hW; dep; simp [comb, *])
    (fun j p q => by cases j <;> first | exact reg_nopath p q | rfl) (fun _ _ h => by simp at h)

/-- The instance tree, for a FIFO encoding of flags that the design's Bool
    reading inverts. -/
theorem built (ofBool : Bool → V) (hB : ∀ b, ops.toB (ofBool b) = b) : Built X (mod ops ofBool) :=
  Built.node (P ops) (impl ofBool) (fun j => match j with
    | .cnt => Built.leaf _ reg_realizes (creg_causal 1)
    | .mode => Built.leaf _ reg_realizes (creg_causal 1)
    | .q => Built.leaf _ (fifo2_realizes ofBool) (fifo2_causal ofBool))
    (fun j => match j with
      | .cnt => aliased_of_none _ _ (fun _ _ => rfl) (fun _ _ => rfl)
      | .mode => aliased_of_none _ _ (fun _ _ => rfl) (fun _ _ => rfl)
      | .q => fifo2_aliased ofBool ops.toB hB)
    (obl ops) (acyc ops)

/-- **It realizes its contract, in every cycle in which the assertion
    holds**, and every input has a hardware cycle. -/
theorem top [Inhabited V] (ofBool : Bool → V) (hB : ∀ b, ops.toB (ofBool b) = b) :
    Realizes (mod ops ofBool) ∧
      ∀ s w, (mod ops ofBool).clean s = s → ∃ o s' g, (mod ops ofBool).hw s w o s' g :=
  (built ops ofBool hB).top

/-- The excluded cycles, in so many words: `ok` is the module's assertion
    on its signals (its children have none). -/
theorem ok_iff (ofBool : Bool → V) (g : (mod ops ofBool).Sig) :
    (mod ops ofBool).ok g ↔
      (ops.isOne ((g.oc .mode).ret Reg.read) && ops.isMax ((g.oc .cnt).ret Reg.read)) = false := by
  constructor
  · exact fun h => h.1
  · intro h; exact ⟨h, fun j => by cases j <;> trivial⟩

end

/-- The assertion is satisfiable: `bv_decide` cannot refute it (today's
    `fail_if_success` check, unchanged). -/
theorem asm_consistent : True := by
  fail_if_success
    (have : ∀ (V : Type) (ops : Ops V) (oc : (j : K) → Outs (CI j).M V) (w : Wires M V),
        (P ops).asm oc w → False := by
      intro V ops oc w h
      change (ops.isOne ((oc .mode).ret Reg.read) && ops.isMax ((oc .cnt).ret Reg.read)) = false at h
      bv_decide)
  trivial

/-- The operators on `Nat`, as `AssertedSchedules.bsv` has them. -/
def natOps : Ops Nat := ⟨0, (· + 1), (· - 1), (· == 1), (· == 7), (· == 1)⟩

/-- A hardware cycle in which the assertion holds. -/
theorem asm_sat : ∃ (oc : (j : K) → Outs (CI j).M Nat) (w : Wires M Nat), (P natOps).asm oc w :=
  ⟨fun _ => ⟨fun _ => true, fun _ => 0⟩, ⟨fun _ => false, fun _ => 0⟩, rfl⟩

end Asm

/-! ## (g) Without its assertion the module fails -/
namespace AsmNo
open Asm (K M R CI)

abbrev P : Parent Nat := { Asm.P Asm.natOps with asm := fun _ _ => True }

/-- With `mode = 1` and `cnt = 7` both rules fire: `cnt`'s data mux is not
    one-hot. -/
theorem not_obl : ¬ Obl P := by
  intro h
  obtain ⟨hoh, -⟩ := h (fun j => ⟨fun _ => true, fun _ => match j with | .cnt => 7 | .mode => 1 | .q => 1⟩)
    ⟨fun _ => false, fun _ => 0⟩ [] trivial
    (fun j => by
      cases j
      · exact aliasOK_of_none _ _ _ (fun _ _ => rfl) (fun _ _ => rfl)
      · exact aliasOK_of_none _ _ _ (fun _ _ => rfl) (fun _ _ => rfl)
      · exact ⟨fun p q h => rfl, fun a p h => by cases a <;> cases p <;> simp [fifoAnn] at h <;> rfl⟩)
    ⟨fun _ _ h => by simp at h, fun _ hg => by simp at hg, fun m hm => by simp [Asm.X, Asm.ann] at hm⟩
    ⟨fun m _ => by simp, fun m x hx => by simp [lastArg] at hx⟩ (fun _ h => by simp at h)
  exact absurd hoh.2 (by decide)

end AsmNo

/-- No methods. -/
def noAnn : Ann Empty where
  prec _ _ := false
  sbr _ _ := false
  conf _ _ := false
  share _ := false
  hasEn _ := false
  alwaysEn _ := false
  alwaysRdy _ := true
abbrev NoX : Iface := ⟨Empty, inferInstance, noAnn⟩

theorem legal_nil {M : Type} {V : Type} (A : Ann M) (h : ∀ m, A.alwaysEn m = false) : Legal (V := V) A [] :=
  ⟨fun _ _ h => by simp at h, fun _ hg => by simp at hg, fun m hm => by rw [h] at hm; cases hm⟩

/-! ## (d) A CAN_FIRE that omits a child's RDY -/
namespace NoRdy

/-- rule push: `q.enq 0` with CAN_FIRE = 1, without `q`'s RDY (FULL_N). -/
abbrev P : Parent Nat where
  I := Unit
  decI := inferInstance
  kids := [()]
  kids_nodup := by decide
  kids_all := fun j => by cases j; decide
  CI := fun _ => FIFO2.X
  X := NoX
  PR := Unit
  body := fun m => m.elim
  rbody := fun _ => .call () FifoM.enq 0 (fun _ => .ret 0)
  comb := fun _ _ => (fun _ => ⟨fun p => p == .enq, fun _ => 0⟩, ⟨fun m => m.elim, fun m => m.elim⟩)
  asm _ _ := True
  onehot _ _ := True

/-- With `q` full the rule still enqueues, which the FIFO's contract does
    not allow. -/
theorem not_obl : ¬ Obl P := by
  intro h
  obtain ⟨-, τP, -, -, hj⟩ := h (fun _ => ⟨fun _ => false, fun _ => 0⟩) ⟨fun m => m.elim, fun m => m.elim⟩ []
    trivial (fun _ => ⟨fun p q h => by cases p <;> cases q <;> simp [fifoAnn] at h <;> rfl,
      fun a p h => by cases a <;> cases p <;> simp [fifoAnn] at h <;> rfl⟩)
    (legal_nil _ (fun m => m.elim)) ⟨fun m => m.elim, fun m => m.elim⟩ (fun _ h => by simp at h)
  obtain ⟨x, hx⟩ := ((hj ()).2.1.en FifoM.enq rfl).mp rfl
  exact Bool.false_ne_true ((hj ()).2.2 _ hx rfl)

end NoRdy

/-! ## (e) Two callers of one copy of a value method, with different arguments -/
namespace ShareArg

/-- A child value method with an argument, one copy (`share`; CF with
    itself), like a `RegFile`'s `sub` port. -/
def subAnn : Ann Unit where
  prec _ _ := false
  sbr _ _ := false
  conf _ _ := false
  share _ := true
  hasEn _ := false
  alwaysEn _ := false
  alwaysRdy _ := true
abbrev SubX : Iface := ⟨Unit, inferInstance, subAnn⟩

inductive K where
  | s | t1 | t2
  deriving DecidableEq

inductive R where
  | r1 | r2
  deriving DecidableEq

abbrev CI : K → Iface
  | .s => SubX
  | .t1 => Reg.X
  | .t2 => Reg.X

/-- r1: `t1 <= s.sub(1)`; r2: `t2 <= s.sub(2)`, both on one copy. -/
abbrev P : Parent Nat where
  I := K
  decI := inferInstance
  kids := [.s, .t1, .t2]
  kids_nodup := by decide
  kids_all := fun j => by cases j <;> decide
  CI := CI
  X := NoX
  PR := R
  body := fun m => m.elim
  rbody := fun r => match r with
    | .r1 => .call .s () 1 (fun v => .call .t1 Reg.write v (fun _ => .ret 0))
    | .r2 => .call .s () 2 (fun v => .call .t2 Reg.write v (fun _ => .ret 0))
  comb := fun oc _ => (fun j => match j with
      | .s => ⟨fun _ => false, fun _ => 1⟩
      | .t1 => regW 0 true ((oc .s).ret ())
      | .t2 => regW 0 true ((oc .s).ret ()),
    ⟨fun m => m.elim, fun m => m.elim⟩)
  asm _ _ := True
  onehot _ _ := True

theorem not_obl : ¬ Obl P := by
  intro h
  let oc : (j : K) → Outs (CI j).M Nat := fun _ => ⟨fun _ => true, fun _ => 0⟩
  obtain ⟨-, τP, -, -, hj⟩ := h oc ⟨fun m => m.elim, fun m => m.elim⟩ []
    trivial (by noal) (legal_nil _ (fun m => m.elim)) ⟨fun m => m.elim, fun m => m.elim⟩ (fun _ h => by simp at h)
  have nogrp : ∀ (g : List (Empty × Nat)) (j : K), proj j (stepTrace P oc (.inl g)) = [] := by
    intro g j; cases g with
    | nil => rfl
    | cons c _ => exact c.1.elim
  -- `t1.write` and `t2.write` are enabled, and only `r1`, `r2` call them
  have in1 : (.inr .r1 : List (Empty × Nat) ⊕ R) ∈ τP := by
    obtain ⟨x, hx⟩ := ((hj .t1).2.1.en (.wr 0) rfl).mp rfl
    obtain ⟨l, hl, hxl⟩ := List.mem_flatten.mp hx
    obtain ⟨e, he, rfl⟩ := List.mem_map.mp hl
    rcases e with g | r
    · rw [nogrp] at hxl; cases hxl
    · cases r
      · exact he
      · have : proj K.t1 (stepTrace P oc (.inr .r2)) = [] := rfl
        rw [this] at hxl; cases hxl
  have in2 : (.inr .r2 : List (Empty × Nat) ⊕ R) ∈ τP := by
    obtain ⟨x, hx⟩ := ((hj .t2).2.1.en (.wr 0) rfl).mp rfl
    obtain ⟨l, hl, hxl⟩ := List.mem_flatten.mp hx
    obtain ⟨e, he, rfl⟩ := List.mem_map.mp hl
    rcases e with g | r
    · rw [nogrp] at hxl; cases hxl
    · cases r
      · have : proj K.t2 (stepTrace P oc (.inr .r1)) = [] := rfl
        rw [this] at hxl; cases hxl
      · exact he
  have p1 : proj K.s (stepTrace P oc (.inr .r1)) = [((), 1)] := rfl
  have p2 : proj K.s (stepTrace P oc (.inr .r2)) = [((), 2)] := rfl
  rcases either_sublist in1 in2 (by simp) with hs | hs
  · have := ((hj .s).1.order _ _ (pair_flatten_map (fun e => proj K.s (stepTrace P oc e)) hs
      (x := ((), 1)) (y := ((), 2)) (by rw [p1]; simp) (by rw [p2]; simp))).2.2.2 rfl rfl
    cases this
  · have := ((hj .s).1.order _ _ (pair_flatten_map (fun e => proj K.s (stepTrace P oc e)) hs
      (x := ((), 2)) (y := ((), 1)) (by rw [p2]; simp) (by rw [p1]; simp))).2.2.2 rfl rfl
    cases this

end ShareArg

/-! ## (f) An always_enabled child port not called in every cycle -/
namespace NoAlw

inductive K where
  | b | r
  deriving DecidableEq

abbrev CI : K → Iface
  | .b => BypassWire.X
  | .r => Reg.X

/-- rule drive: when `r = 0`, `b.wset(r)`.  `b`'s `wset` is always enabled
    (`mkBypassWire`), but nothing calls it when `r ≠ 0`. -/
abbrev P : Parent Nat where
  I := K
  decI := inferInstance
  kids := [.b, .r]
  kids_nodup := by decide
  kids_all := fun j => by cases j <;> decide
  CI := CI
  X := NoX
  PR := Unit
  body := fun m => m.elim
  rbody := fun _ => .call .r Reg.read 0 (fun v => if v = 0 then .call .b BypM.wset v (fun _ => .ret 0) else .fail)
  comb := fun oc _ => (fun j => match j with
      | .b => ⟨fun _ => false, fun _ => (oc .r).ret Reg.read⟩
      | .r => regW 0 false 0,
    ⟨fun m => m.elim, fun m => m.elim⟩)
  asm _ _ := True
  onehot _ _ := True

theorem not_obl : ¬ Obl P := by
  intro h
  let oc : (j : K) → Outs (CI j).M Nat := fun _ => ⟨fun _ => true, fun _ => 1⟩
  obtain ⟨-, τP, -, -, hj⟩ := h oc ⟨fun m => m.elim, fun m => m.elim⟩ []
    trivial (by noal) (legal_nil _ (fun m => m.elim)) ⟨fun m => m.elim, fun m => m.elim⟩ (fun _ h => by simp at h)
  obtain ⟨x, hx⟩ := (hj .b).1.always BypM.wset rfl
  obtain ⟨l, hl, hxl⟩ := List.mem_flatten.mp hx
  obtain ⟨e, he, rfl⟩ := List.mem_map.mp hl
  rcases e with g | u
  · cases g with
    | nil => cases hxl
    | cons c _ => exact c.1.elim
  · have : proj K.b (stepTrace P oc (.inr u)) = [] := rfl
    rw [this] at hxl; cases hxl

end NoAlw

/-! ## A child with a combinational path: an RWire (soundness review 2)

`r : Reg`, `w : RWire` (bsc's `vMkRWire`: `wset SBR wget`, path
`WVAL → WGET`), `t : Reg`.  Rule `drive` sets the wire from `r`; rule `use`
writes `t` from the wire in the same cycle.  `t`'s data reads `w.wget`,
which (through the wire's path) reads `w.wset`'s data, which reads `r`: the
ranks `r.read ↦ 0`, `w.wget ↦ 1` order the evaluation, so the module is
causal (`WireEx.top`). -/
namespace WireEx

inductive K where
  | r | w | t
  deriving DecidableEq

inductive R where
  | drive | use
  deriving DecidableEq

abbrev CI : K → Iface
  | .r => Reg.X
  | .w => RWire.X true false
  | .t => Reg.X

def wireW {V : Type} (en : Bool) (d z : V) : Wires WireM V :=
  ⟨fun p => match p with | .wset => en | _ => false, fun p => match p with | .wset => d | _ => z⟩

section
variable {V : Type} (z : V)

def rbody : R → Prog K (fun j => (CI j).M) V
  | .drive => .call .r Reg.read z (fun v => .call .w WireM.wset v (fun _ => .ret z))
  | .use => .call .w WireM.wget z (fun v => .call .t Reg.write v (fun _ => .ret z))

def comb (oc : (j : K) → Outs (CI j).M V) (w : Wires Empty V) : ((j : K) → Wires (CI j).M V) × Outs Empty V :=
  (fun j => match j with
    | .r => regW z false z
    | .w => wireW true ((oc .r).ret Reg.read) z
    | .t => regW z true ((oc .w).ret WireM.wget),
   ⟨fun m => m.elim, fun m => m.elim⟩)

abbrev P : Parent V where
  I := K
  decI := inferInstance
  kids := [.r, .w, .t]
  kids_nodup := by decide
  kids_all := fun j => by cases j <;> decide
  CI := CI
  X := NoX
  PR := R
  body := fun m => m.elim
  rbody := rbody z
  comb := comb z
  asm _ _ := True
  onehot _ _ := True

variable (oc : (j : K) → Outs (CI j).M V)
theorem trR_drive : trR (P z) oc .drive =
    ([⟨.r, (Reg.read, z)⟩, ⟨.w, (WireM.wset, (oc .r).ret Reg.read)⟩], some z) := rfl
theorem trR_use : trR (P z) oc .use =
    ([⟨.w, (WireM.wget, z)⟩, ⟨.t, (Reg.write, (oc .w).ret WireM.wget)⟩], some z) := rfl
end

def fp : Empty ⊕ R → List (Σ j, (CI j).M)
  | .inl m => m.elim
  | .inr .drive => [⟨.r, Reg.read⟩, ⟨.w, WireM.wset⟩]
  | .inr .use => [⟨.w, WireM.wget⟩, ⟨.t, Reg.write⟩]

def users : (j : K) → (CI j).M → List (Empty ⊕ R)
  | .r, .rd _ => [.inr .drive]
  | .w, .wset => [.inr .drive]
  | .w, .wget => [.inr .use]
  | .t, .wr _ => [.inr .use]
  | _, _ => []

macro "tr" : tactic => `(tactic| ((try simp only [trR_drive, trR_use] at *); (try pj)))
macro "an" : tactic => `(tactic| simp [CI, Reg.X, CReg.X, cregAnn, RWire.X, rwireAnn, Ann.Follow, Ann.Group] at *)
macro "okt" : tactic => `(tactic| (intro j; cases j <;> (tr <;> an <;> done)))

section
variable {V : Type} (z : V)

/-- The certificate: `drive` before `use` (`wset SBR wget`). -/
def cert : Cert (P z) where
  rules := [.drive, .use]
  rules_nodup := by simp
  fire _ _ _ := true
  hit _ _ m _ := m.elim
  fp := fp
  users := users
  users_ok s := by rcases s with m | r; exact m.elim; cases r <;> simp [fp, users]
  fp_ok oc w s c hc := by
    rcases s with m | r
    · exact m.elim
    · cases r <;> unf <;> tr <;> (rcases hc with rfl | rfl) <;> simp [fp]
  selfAnn m := m.elim
  fireIn _ _ r _ := by cases r <;> simp
  resM _ _ m := m.elim
  resR oc w r _ _ := by cases r <;> rfl
  rdyM _ _ m := m.elim
  rdyR oc w r _ _ c hc h := by cases r <;> tr <;> (rcases hc with rfl | rfl) <;> an
  grpM _ _ m := m.elim
  grpR oc w r _ _ := by
    cases r <;> (intro j a b h; cases j <;> tr <;> first | exact absurd h pair_sub_single | exact absurd h pair_sub_nil)
  mm _ _ m := m.elim
  mmg _ _ m := m.elim
  h1 _ _ m := m.elim
  h2 _ _ m := m.elim
  h3 _ _ m := m.elim
  h4 oc w r1 r2 _ _ _ hne hn := by
    cases r1 <;> cases r2
    · exact absurd rfl hne
    · exfalso; apply hn; okt
    · exact List.Sublist.refl _
    · exact absurd rfl hne
  en oc w j cm _ hcm := by
    cases j <;> cases cm <;> (try (an; done)) <;> (try (rename_i i; rw [fin1 i])) <;>
      (simp only [users, List.mem_cons, List.not_mem_nil, or_false, exists_eq_or_imp, exists_eq_left]; unf; tr;
       simp [comb, regW, wireW])
  enV _ _ m := m.elim
  arg oc w j cm _ s hs _ x hx := by
    cases j <;> cases cm <;> simp only [users, List.mem_cons, List.not_mem_nil, or_false] at hs <;>
      (first | subst hs | skip) <;> unf <;> tr <;> (try (rcases hx with ⟨-, rfl⟩; rfl)) <;> simp at hs
  alw oc w j cm _ h := by cases j <;> cases cm <;> an
  onehot _ _ _ := trivial

theorem obl : Obl (P z) := obl_of_cert (P z) (cert z)

/-- The acyclicity certificate, with ranks: `w`'s path from `wset` to `wget`
    reads `r` (rank 0) below `w.wget` (rank 1). -/
def acyc : Acyc (P z) where
  D j p := match j, p with
    | .w, .wset => [⟨.r, Reg.read⟩]
    | .t, .wr _ => [⟨.w, WireM.wget⟩]
    | _, _ => []
  Dw _ _ := []
  Do q := q.elim
  Dwo q := q.elim
  dep := by intro oc oc' w w' j p hD hW; cases j <;> cases p <;> dep <;> simp [comb, regW, wireW, *]
  depo := by intro oc oc' w w' q; exact q.elim
  rk y := match y with | ⟨.w, .wget⟩ => 1 | ⟨.w, .whas⟩ => 1 | _ => 0
  N := 2
  rk_lt y := by
    rcases y with ⟨j, p⟩; cases j <;> cases p <;> (show _ < 2; first | exact Nat.zero_lt_two | exact Nat.one_lt_two)
  rk_ok j q p h y hy := by
    cases j
    · rw [reg_nopath] at h; cases h
    · cases q <;> cases p <;> simp [rwireAnn] at h <;> simp at hy <;> subst hy <;> exact Nat.zero_lt_one
    · rw [reg_nopath] at h; cases h
  reach _ := []
  reach_w _ _ _ _ p' hp' := by simp at hp'
  reach_o _ _ _ _ _ _ p' hp' := by simp at hp'
  path_w q := q.elim
  path_o q := q.elim

abbrev impl (ofBool : Bool → V) : (j : K) → Module V (CI j)
  | .r => Reg
  | .w => RWire ofBool true false
  | .t => Reg

abbrev mod (ofBool : Bool → V) : Module V NoX := (P z).toModule (impl ofBool)

theorem built [Inhabited V] (ofBool : Bool → V) : Built NoX (mod z ofBool) :=
  Built.node (P z) (impl ofBool) (fun j => match j with
    | .r => Built.leaf _ reg_realizes (creg_causal 1)
    | .w => Built.leaf _ (rwire_realizes ofBool true false) (rwire_causal ofBool true false)
    | .t => Built.leaf _ reg_realizes (creg_causal 1))
    (fun j => aliased_of_none _ _ (fun p q => by cases j <;> rfl) (fun a p => by cases j <;> rfl))
    (obl z) (acyc z)

/-- **Realizes, and every input has a hardware cycle**, through the wire's path. -/
theorem top [Inhabited V] (ofBool : Bool → V) : Realizes (mod z ofBool) ∧
    ∀ s w, (mod z ofBool).clean s = s → ∃ o s' g, (mod z ofBool).hw s w o s' g :=
  (built z ofBool).top
end
end WireEx

/-! ## (h) A combinational loop through a wire: the obligation holds, the
acyclicity certificate cannot exist

The parent's one rule calls `w.wset`, but its `comb` (a transcription slip)
makes `wset`'s enable `!whas`, and `RWire.v` has `WHAS = WSET`.  The
obligation holds (`Loop.obl`, from a certificate), yet the hardware has no
cycle at all (`Loop.hw_empty`): its `Realizes` would say nothing.  The
acyclicity certificate, which `Built.node` requires, is impossible
(`Loop.no_acyc`): `w.wset`'s enable reads `w.whas`, and the wire's path
`wset → whas` would need `rk whas < rk whas`. -/
namespace Loop

abbrev CI : Unit → Iface := fun _ => RWire.X true false

abbrev P : Parent Bool where
  I := Unit
  decI := inferInstance
  kids := [()]
  kids_nodup := by decide
  kids_all := fun j => by cases j; decide
  CI := CI
  X := NoX
  PR := Unit
  body := fun m => m.elim
  rbody := fun _ => .call () WireM.wset false (fun _ => .ret false)
  comb := fun oc _ => (fun _ => ⟨fun p => match p with | .wset => !((oc ()).ret WireM.whas) | _ => false,
      fun _ => false⟩, ⟨fun m => m.elim, fun m => m.elim⟩)
  asm _ _ := True
  onehot _ _ := True

theorem trR_r (oc : (j : Unit) → Outs (CI j).M Bool) (r : Unit) :
    trR P oc r = ([⟨(), (WireM.wset, false)⟩], some false) := rfl

def cert : Cert P where
  rules := [()]
  rules_nodup := by simp
  fire oc _ _ := !((oc ()).ret WireM.whas)
  hit _ _ m _ := m.elim
  fp s := match s with | .inl m => m.elim | .inr _ => [⟨(), WireM.wset⟩]
  users _ cm := match cm with | .wset => [.inr ()] | _ => []
  users_ok s p hp := by
    rcases s with m | r
    · exact m.elim
    · simp only [List.mem_singleton] at hp; subst hp; simp
  fp_ok oc w s c hc := by
    rcases s with m | r
    · exact m.elim
    · simp only [trS, trR_r, List.mem_singleton] at hc; subst hc; simp
  selfAnn m := m.elim
  fireIn _ _ r _ := by cases r; simp
  resM _ _ m := m.elim
  resR _ _ _ _ _ := rfl
  rdyM _ _ m := m.elim
  rdyR oc w r _ _ c hc h := by
    simp only [trR_r, List.mem_singleton] at hc; subst hc; simp [rwireAnn] at h
  grpM _ _ m := m.elim
  grpR oc w r _ _ := by
    intro j a b h; cases j
    simp only [trR_r, proj_cons_self, proj_nil] at h
    exact absurd h pair_sub_single
  mm _ _ m := m.elim
  mmg _ _ m := m.elim
  h1 _ _ m := m.elim
  h2 _ _ m := m.elim
  h3 _ _ m := m.elim
  h4 _ _ r1 r2 _ _ _ hne := by cases r1; cases r2; exact absurd rfl hne
  en oc w j cm _ hcm := by
    cases j
    cases cm
    · simp only [List.mem_singleton, exists_eq_left, On, Calls, trS, trR_r, proj_cons_self, proj_nil]
      simp
    · simp [rwireAnn] at hcm
    · simp [rwireAnn] at hcm
  enV _ _ m := m.elim
  arg oc w j cm _ s hs hav x hx := by
    cases j
    cases cm
    · simp only [List.mem_singleton] at hs; subst hs
      simp only [trS, trR_r, proj_cons_self, proj_nil, List.mem_singleton, Prod.mk.injEq] at hx
      obtain ⟨-, rfl⟩ := hx; rfl
    · simp at hs
    · simp at hs
  alw oc w j cm _ h := by cases cm <;> simp [rwireAnn] at h
  onehot _ _ _ := trivial

/-- The obligation holds. -/
theorem obl : Obl P := obl_of_cert P cert

abbrev impl : (j : Unit) → Module Bool (CI j) := fun _ => RWire id true false
abbrev mod : Module Bool NoX := P.toModule impl

/-- But no hardware cycle exists, from any state, for any input. -/
theorem hw_empty (s : mod.S) (w : Wires Empty Bool) (o : Outs Empty Bool) (s' : mod.S) (g : mod.Sig) :
    ¬ mod.hw s w o s' g := by
  rintro ⟨-, hc, hen, -, -⟩
  obtain ⟨wX, oX, ⟨-, hhas, -⟩, hen', -, -, -, hret⟩ := hc ()
  have e1 : (g.oc ()).ret WireM.whas = oX.ret WireM.whas := hret WireM.whas
  have e2 : wX.en WireM.wset = (g.wc ()).en WireM.wset := hen' WireM.wset
  have e3 := congrFun (hen ()) WireM.wset
  rw [hhas, Bool.false_or, e2, e3] at e1
  change (g.oc ()).ret WireM.whas = !((g.oc ()).ret WireM.whas) at e1
  revert e1
  cases (g.oc ()).ret WireM.whas <;> simp

/-- **No acyclicity certificate exists**, so the module is not `Built`. -/
theorem no_acyc : Acyc P → False := fun A => by
  obtain ⟨out, -, ht⟩ := causal_toModule P impl (fun _ => rwire_causal id true false) A
  obtain ⟨s', g, h⟩ := ht (fun _ => none) ⟨fun e => e.elim, fun e => e.elim⟩ rfl
  exact hw_empty _ _ _ _ _ h

end Loop

/-! ## (i) A parent exporting an always_enabled method it cannot always serve

`tick` is always_enabled (so always_ready, as bsc requires) and enqueues into
a FIFO.  In a cycle in which the FIFO is full, the caller still calls it:
the obligation fails (the FIFO call is not ready). -/
namespace AlwEnq

inductive M where
  | tick
  deriving DecidableEq

def ann : Ann M where
  prec _ _ := false
  sbr _ _ := false
  conf _ _ := true
  share _ := false
  hasEn _ := false
  alwaysEn _ := true
  alwaysRdy _ := true

abbrev P : Parent Nat where
  I := Unit
  decI := inferInstance
  kids := [()]
  kids_nodup := by decide
  kids_all := fun j => by cases j; decide
  CI := fun _ => FIFO2.X
  X := ⟨M, inferInstance, ann⟩
  PR := Empty
  body := fun _ _ => .call () FifoM.enq 0 (fun _ => .ret 0)
  rbody := fun r => r.elim
  comb := fun _ _ => (fun _ => ⟨fun p => p == .enq, fun _ => 0⟩, ⟨fun _ => true, fun _ => 0⟩)
  asm _ _ := True
  onehot _ _ := True

theorem not_obl : ¬ Obl P := by
  intro h
  let oc : (j : Unit) → Outs FifoM Nat := fun _ => ⟨fun _ => false, fun _ => 0⟩
  let w : Wires M Nat := ⟨fun _ => false, fun _ => 0⟩
  let g : List (M × Nat) := [(.tick, 0)]
  have hL : Legal P.X.ann [g] :=
    ⟨fun c1 c2 hs => absurd hs pair_sub_single, fun g' hg c1 c2 hs => by
      simp only [List.mem_singleton] at hg; subst hg; exact absurd hs pair_sub_single,
     fun m _ => ⟨0, by cases m; simp [g]⟩⟩
  have hD : Drives P.X.ann w [g] :=
    ⟨fun m hm => by simp [ann] at hm, fun m x hx => by cases m; simp [g, lastArg] at hx; simp [w, ← hx]⟩
  obtain ⟨-, τP, hg, -, hj⟩ := h oc w [g] trivial
    (fun _ => ⟨fun p q h => by cases p <;> cases q <;> simp [fifoAnn] at h <;> rfl,
      fun a p h => by cases a <;> cases p <;> simp [fifoAnn] at h <;> rfl⟩) hL hD (fun c _ h => by simp [ann] at h)
  have hgin : (.inl g : List (M × Nat) ⊕ Empty) ∈ τP := inl_mem_of_groups (by rw [hg]; simp)
  have trg : proj () (stepTrace P oc (.inl g)) = [(FifoM.enq, 0)] := rfl
  have hmem : (FifoM.enq, 0) ∈ (sigmaJ P oc τP ()).flatten :=
    List.mem_flatten.mpr ⟨_, List.mem_map.mpr ⟨_, hgin, rfl⟩, by rw [trg]; exact List.mem_singleton.mpr rfl⟩
  exact Bool.false_ne_true ((hj ()).2.2 _ hmem rfl)

end AlwEnq

/-! ## (j) An always_ready method whose guard can fail

`get` is exported always_ready but returns `r` only when `r ≠ 0`: with
`r = 0` a caller may call it, and the method's guard fails. -/
namespace AlwRdy

inductive M where
  | get
  deriving DecidableEq

def ann : Ann M where
  prec _ _ := false
  sbr _ _ := false
  conf _ _ := false
  share _ := true
  hasEn _ := false
  alwaysEn _ := false
  alwaysRdy _ := true

abbrev P : Parent Nat where
  I := Unit
  decI := inferInstance
  kids := [()]
  kids_nodup := by decide
  kids_all := fun j => by cases j; decide
  CI := fun _ => Reg.X
  X := ⟨M, inferInstance, ann⟩
  PR := Empty
  body := fun _ _ => .call () Reg.read 0 (fun v => if v = 0 then .fail else .ret v)
  rbody := fun r => r.elim
  comb := fun oc _ => (fun _ => regW 0 false 0, ⟨fun _ => true, fun _ => (oc ()).ret Reg.read⟩)
  asm _ _ := True
  onehot _ _ := True

theorem not_obl : ¬ Obl P := by
  intro h
  let oc : (j : Unit) → Outs (CRegM 1) Nat := fun _ => ⟨fun _ => true, fun _ => 0⟩
  let w : Wires M Nat := ⟨fun _ => false, fun _ => 0⟩
  let g : List (M × Nat) := [(.get, 0)]
  have hL : Legal P.X.ann [g] :=
    ⟨fun c1 c2 hs => absurd hs pair_sub_single, fun g' hg c1 c2 hs => by
      simp only [List.mem_singleton] at hg; subst hg; exact absurd hs pair_sub_single,
     fun m hm => by simp [ann] at hm⟩
  have hD : Drives P.X.ann w [g] :=
    ⟨fun m hm => by simp [ann] at hm, fun m x hx => by cases m; simp [g, lastArg] at hx; simp [w, ← hx]⟩
  obtain ⟨-, τP, hg, hst, -⟩ := h oc w [g] trivial (by noal) hL hD (fun c _ h => by simp [ann] at h)
  have hgin : (.inl g : List (M × Nat) ⊕ Empty) ∈ τP := inl_mem_of_groups (by rw [hg]; simp)
  have := hst _ hgin (M.get, 0) (List.mem_singleton.mpr rfl)
  simp [trace, oc] at this

end AlwRdy

/-! ## Output aliases (integration review 2, M3)

FIFO2's `FULL_N` is `enq`'s RDY and the result of `notFull` and
`i_notFull`; the `.ba` names the wire after `i_notFull`.  The parent's
`comb` (from the netlist) uses that one wire for `put`'s RDY and for
`canPut`'s result, while `canPut`'s body reads `notFull` explicitly (it must:
`notFull SB enq` orders it, `i_notFull CF enq` would not).  The
certificate relates the three through the FIFO's exported aliases
(`Env`, `fifo2_aliased`). -/
namespace AliasEx

inductive K where
  | q
  deriving DecidableEq

inductive M where
  | put | canPut
  deriving DecidableEq

structure Ops (V : Type) where
  z : V
  toB : V → Bool

/-- `canPut SB put` (from `notFull SB enq`), `put C put`. -/
def ann : Ann M where
  prec p q := match p, q with | .canPut, .put => true | _, _ => false
  sbr _ _ := false
  conf p q := match p, q with | .put, .put => true | _, _ => false
  share m := match m with | .canPut => true | _ => false
  hasEn m := match m with | .put => true | _ => false
  alwaysEn _ := false
  alwaysRdy m := match m with | .canPut => true | _ => false

abbrev X : Iface := ⟨M, inferInstance, ann⟩
abbrev CI : K → Iface := fun _ => FIFO2.X

section
variable {V : Type} (ops : Ops V)

def body : M → V → Prog K (fun j => (CI j).M) V
  | .put, x => .call .q FifoM.enq x (fun _ => .ret ops.z)
  | .canPut, _ => .call .q FifoM.notFull ops.z .ret

/-- `RDY_put` and `canPut` are the FIFO's `FULL_N`, named after `i_notFull`. -/
def comb (oc : (j : K) → Outs (CI j).M V) (w : Wires M V) : ((j : K) → Wires (CI j).M V) × Outs M V :=
  (fun _ => ⟨fun p => match p with | .enq => w.en .put | _ => false,
             fun p => match p with | .enq => w.arg .put | _ => ops.z⟩,
   ⟨fun m => match m with | .put => ops.toB ((oc .q).ret FifoM.i_notFull) | .canPut => true,
    fun m => match m with | .put => ops.z | .canPut => (oc .q).ret FifoM.i_notFull⟩)

abbrev P : Parent V where
  I := K
  decI := inferInstance
  kids := [.q]
  kids_nodup := by decide
  kids_all := fun j => by cases j; decide
  CI := CI
  X := X
  PR := Empty
  body := body ops
  rbody := fun r => r.elim
  comb := comb ops
  asm _ _ := True
  onehot _ _ := True
  toB := ops.toB

variable (oc : (j : K) → Outs (CI j).M V) (w : Wires M V)
theorem trM_put : trM (P ops) oc w .put = ([⟨.q, (FifoM.enq, w.arg .put)⟩], some ops.z) := rfl
theorem trM_canPut : trM (P ops) oc w .canPut =
    ([⟨.q, (FifoM.notFull, ops.z)⟩], some ((oc .q).ret FifoM.notFull)) := rfl
end

def fp : M ⊕ Empty → List (Σ j, (CI j).M)
  | .inl .put => [⟨.q, FifoM.enq⟩]
  | .inl .canPut => [⟨.q, FifoM.notFull⟩]

def users : (j : K) → (CI j).M → List (M ⊕ Empty)
  | .q, .enq => [.inl .put]
  | .q, .notFull => [.inl .canPut]
  | _, _ => []

macro "tr" : tactic => `(tactic| ((try simp only [trM_put, trM_canPut] at *); (try pj)))
macro "an" : tactic => `(tactic| simp [CI, FIFO2.X, fifoAnn, Ann.Follow, Ann.Group, X, ann] at *)

section
variable {V : Type} (ops : Ops V)

def cert : Cert (P ops) where
  rules := []
  rules_nodup := List.nodup_nil
  fire _ _ _ := false
  hit _ _ _ _ := false
  fp := fp
  users := users
  users_ok s := by rcases s with m | r; cases m <;> simp [fp, users]; exact r.elim
  fp_ok oc w s c hc := by
    rcases s with m | r
    · cases m <;> unf <;> tr <;> subst hc <;> simp [fp]
    · exact r.elim
  selfAnn m := by cases m <;> simp [ann]
  fireIn _ _ r _ := r.elim
  resM oc w m hE _ := by
    cases m
    · rfl
    · -- the explicit `notFull` read returns the `i_notFull` wire: one output
      rw [trM_canPut]
      simp [comb, (hE.2.1 .q).1 FifoM.notFull FifoM.i_notFull rfl]
  resR oc w r _ := r.elim
  rdyM oc w m hE ha c hc h := by
    cases m
    · -- `put`'s RDY is `FULL_N`, which is `enq`'s RDY
      rw [trM_put, List.mem_singleton] at hc; subst hc
      rcases ha.1 with h1 | h1
      · simp [ann] at h1
      · rw [(hE.2.1 .q).2 FifoM.enq FifoM.i_notFull rfl]; exact h1
    · rw [trM_canPut, List.mem_singleton] at hc; subst hc; simp [fifoAnn] at h
  rdyR oc w r _ := r.elim
  grpM oc w m _ _ := by cases m <;> (intro j a b h; cases j; tr; exact absurd h pair_sub_single)
  grpR oc w r _ := r.elim
  mm oc w m1 m2 _ _ _ hf := by
    cases m1 <;> cases m2 <;> first | (exfalso; an; done) | (intro j; cases j; tr; an)
  mmg oc w m1 m2 _ _ _ hf := by
    cases m1 <;> cases m2 <;> first | (exfalso; an; done) | (intro j; cases j; tr; an)
  h1 oc w m r _ _ hr := r.elim
  h2 oc w m r1 r2 _ _ hr := r1.elim
  h3 oc w m m' r _ _ _ hr := r.elim
  h4 oc w r1 r2 _ hr := r1.elim
  en oc w j cm _ hcm := by
    cases j; cases cm <;> first | (an; done) | skip
    all_goals simp only [users, List.mem_cons, List.not_mem_nil, or_false, exists_eq_or_imp, exists_eq_left,
      exists_false]
    all_goals (unf; tr; simp [comb, ann])
  enV oc w m _ h1 h2 _ c hc := by cases m <;> tr <;> simp_all [ann, fifoAnn]
  arg oc w j cm _ s hs _ x hx := by
    cases j; cases cm <;> simp only [users, List.mem_cons, List.not_mem_nil, or_false] at hs <;>
      (first | subst hs | skip) <;> unf <;> tr
    · rcases hx with ⟨-, rfl⟩; rfl
    · rcases hx with ⟨-, rfl⟩; rfl
  alw oc w j cm _ h := by cases j; cases cm <;> an
  onehot _ _ _ := trivial

theorem obl : Obl (P ops) := obl_of_cert (P ops) (cert ops)

def acyc : Acyc (P ops) :=
  Acyc.ofMoore (P ops) (fun _ _ => [])
    (fun j p => match j, p with | .q, .enq => [.put] | _, _ => [])
    (fun _ => [⟨.q, FifoM.i_notFull⟩]) (fun _ => [])
    (by intro oc oc' w w' j p hD hW; cases j <;> cases p <;> dep <;> simp [comb, *])
    (by intro oc oc' w w' q hD hW; cases q <;> dep <;> simp [comb, *])
    (fun j p q => rfl) (fun _ _ h => by simp at h)

theorem built [Inhabited V] (ofBool : Bool → V) (hB : ∀ b, ops.toB (ofBool b) = b) :
    Built X ((P ops).toModule (fun _ => FIFO2 ofBool)) :=
  Built.node (P ops) (fun _ => FIFO2 ofBool) (fun _ => Built.leaf _ (fifo2_realizes ofBool) (fifo2_causal ofBool))
    (fun _ => fifo2_aliased ofBool ops.toB hB) (obl ops) (acyc ops)
end
end AliasEx

/-! ## Constants (integration review 2, B1)

A two-state FSM in the style of `StmtFSM`: rule `go0` fires when
`st == 0` (sets `st <= 1`, `a <= x0`), rule `go1` when `st == 1` (sets
`st <= 0`, `a <= x1`).  bsc fires both on their CAN_FIRE and AND-ORs their
writes, relying on the states' disjointness, which it does not record as a
solver verdict.  Over an abstract `V` the two comparisons are independent
atoms; with constants as `EqC` and its law, the at-most-one chain
(`amoChain_of_law`) supplies the disjointness, and `skel` is one
`bv_decide`.  Without the law the obligation is false (`FsmNo`). -/
namespace Fsm

structure Ops (V : Type) where
  z : V
  k : Nat → V
  x0 : V
  x1 : V

inductive K where
  | st | a
  deriving DecidableEq

inductive R where
  | go0 | go1
  deriving DecidableEq

abbrev CI : K → Iface := fun _ => Reg.X

section
variable {V : Type} (ops : Ops V) (eqc : V → Nat → Bool)

def rbody : R → Prog K (fun j => (CI j).M) V
  | .go0 => .call .st Reg.read ops.z (fun v => if eqc v 0 then
      .call .st Reg.write (ops.k 1) (fun _ => .call .a Reg.write ops.x0 (fun _ => .ret ops.z)) else .fail)
  | .go1 => .call .st Reg.read ops.z (fun v => if eqc v 1 then
      .call .st Reg.write (ops.k 0) (fun _ => .call .a Reg.write ops.x1 (fun _ => .ret ops.z)) else .fail)

def fire (oc : (j : K) → Outs (CI j).M V) (w : Wires Empty V) : R → Bool
  | .go0 => eqc ((oc .st).ret Reg.read) 0
  | .go1 => eqc ((oc .st).ret Reg.read) 1

def comb (oc : (j : K) → Outs (CI j).M V) (w : Wires Empty V) : ((j : K) → Wires (CI j).M V) × Outs Empty V :=
  (fun j => match j with
    | .st => regW ops.z (eqc ((oc .st).ret Reg.read) 0 || eqc ((oc .st).ret Reg.read) 1)
        (if eqc ((oc .st).ret Reg.read) 0 then ops.k 1 else ops.k 0)
    | .a => regW ops.z (eqc ((oc .st).ret Reg.read) 0 || eqc ((oc .st).ret Reg.read) 1)
        (if eqc ((oc .st).ret Reg.read) 0 then ops.x0 else ops.x1),
   ⟨fun m => m.elim, fun m => m.elim⟩)

abbrev P : Parent V where
  I := K
  decI := inferInstance
  kids := [.st, .a]
  kids_nodup := by decide
  kids_all := fun j => by cases j <;> decide
  CI := CI
  X := NoX
  PR := R
  body := fun m => m.elim
  rbody := rbody ops eqc
  comb := comb ops eqc
  asm _ _ := True
  onehot oc _ := (eqc ((oc .st).ret Reg.read) 0 && eqc ((oc .st).ret Reg.read) 1) = false

variable (oc : (j : K) → Outs (CI j).M V)
theorem trR_go0 (h : eqc ((oc .st).ret Reg.read) 0 = true) : trR (P ops eqc) oc .go0 =
    ([⟨.st, (Reg.read, ops.z)⟩, ⟨.st, (Reg.write, ops.k 1)⟩, ⟨.a, (Reg.write, ops.x0)⟩], some ops.z) := by
  simp [trR, rbody, trace, h]
theorem trR_go0' (h : eqc ((oc .st).ret Reg.read) 0 = false) : trR (P ops eqc) oc .go0 =
    ([⟨.st, (Reg.read, ops.z)⟩], none) := by
  simp [trR, rbody, trace, h]
theorem trR_go1 (h : eqc ((oc .st).ret Reg.read) 1 = true) : trR (P ops eqc) oc .go1 =
    ([⟨.st, (Reg.read, ops.z)⟩, ⟨.st, (Reg.write, ops.k 0)⟩, ⟨.a, (Reg.write, ops.x1)⟩], some ops.z) := by
  simp [trR, rbody, trace, h]
theorem trR_go1' (h : eqc ((oc .st).ret Reg.read) 1 = false) : trR (P ops eqc) oc .go1 =
    ([⟨.st, (Reg.read, ops.z)⟩], none) := by
  simp [trR, rbody, trace, h]
end

/-- **The skeleton, with the EqLaw chain**: one `bv_decide`, whose atoms
    are the two comparisons; the chain (from `EqC.law`) is the only fact
    relating them. -/
theorem skel {V : Type} (E : EqC V) (v : V) : (E.eqc v 0 && E.eqc v 1) = false := by
  have h := amoChain_of_law E v [0, 1] false (by decide) (by simp)
  simp only [amoChain, List.map] at h
  obtain ⟨-, h, -⟩ := h
  bv_decide

/-- The chain is consistent: `bv_decide` cannot refute it (the
    `fail_if_success` check, now over the oracle facts and chains too). -/
theorem chain_consistent : True := by
  fail_if_success
    (have : ∀ (V : Type) (E : EqC V) (v : V), amoChain false [E.eqc v 0, E.eqc v 1] → False := by
      intro V E v h
      simp only [amoChain] at h
      obtain ⟨-, h, -⟩ := h
      bv_decide)
  trivial

def fp : Empty ⊕ R → List (Σ j, (CI j).M)
  | .inl m => m.elim
  | .inr _ => [⟨.st, Reg.read⟩, ⟨.st, Reg.write⟩, ⟨.a, Reg.write⟩]

def users : (j : K) → (CI j).M → List (Empty ⊕ R) := fun _ _ => [.inr .go0, .inr .go1]

section
variable {V : Type} (ops : Ops V) (E : EqC V)

def cert : Cert (P ops E.eqc) where
  rules := [.go0, .go1]
  rules_nodup := by simp
  fire := fire E.eqc
  hit _ _ m _ := m.elim
  fp := fp
  users := users
  users_ok s := by rcases s with m | r; exact m.elim; cases r <;> simp [fp, users]
  fp_ok oc w s c hc := by
    rcases s with m | r
    · exact m.elim
    · cases r
      · cases h : E.eqc ((oc .st).ret Reg.read) 0
        · simp only [trS, trR_go0' ops E.eqc oc h, List.mem_singleton] at hc; subst hc; simp [fp]
        · simp only [trS, trR_go0 ops E.eqc oc h, List.mem_cons, List.not_mem_nil, or_false] at hc
          rcases hc with rfl | rfl | rfl <;> simp [fp]
      · cases h : E.eqc ((oc .st).ret Reg.read) 1
        · simp only [trS, trR_go1' ops E.eqc oc h, List.mem_singleton] at hc; subst hc; simp [fp]
        · simp only [trS, trR_go1 ops E.eqc oc h, List.mem_cons, List.not_mem_nil, or_false] at hc
          rcases hc with rfl | rfl | rfl <;> simp [fp]
  selfAnn m := m.elim
  fireIn _ _ r _ := by cases r <;> simp
  resM _ _ m := m.elim
  resR oc w r _ hr := by
    cases r
    · rw [trR_go0 ops E.eqc oc hr]; rfl
    · rw [trR_go1 ops E.eqc oc hr]; rfl
  rdyM _ _ m := m.elim
  rdyR oc w r _ _ c _ h := by simp [cregAnn] at h
  grpM _ _ m := m.elim
  grpR oc w r _ hr := by
    intro j a b h
    cases r
    · rw [trR_go0 ops E.eqc oc hr] at h
      cases j <;> pj <;> first | exact absurd h pair_sub_single | exact absurd h pair_sub_nil |
        (obtain ⟨rfl, rfl⟩ := pair_sub_two h; simp [cregAnn, Ann.Group, Ann.Follow])
    · rw [trR_go1 ops E.eqc oc hr] at h
      cases j <;> pj <;> first | exact absurd h pair_sub_single | exact absurd h pair_sub_nil |
        (obtain ⟨rfl, rfl⟩ := pair_sub_two h; simp [cregAnn, Ann.Group, Ann.Follow])
  mm _ _ m := m.elim
  mmg _ _ m := m.elim
  h1 _ _ m := m.elim
  h2 _ _ m := m.elim
  h3 _ _ m := m.elim
  h4 oc w r1 r2 _ h1 h2 hne _ := by
    have := skel E ((oc .st).ret Reg.read)
    cases r1 <;> cases r2
    · exact absurd rfl hne
    · simp only [fire] at h1 h2; simp [h1, h2] at this
    · simp only [fire] at h1 h2; simp [h1, h2] at this
    · exact absurd rfl hne
  en oc w j cm _ hcm := by
    have sk := skel E ((oc .st).ret Reg.read)
    cases j <;> cases cm <;> (try (simp [cregAnn] at hcm; done)) <;> rename_i i <;> rw [fin1 i] <;>
      simp only [users, List.mem_cons, List.not_mem_nil, or_false, exists_eq_or_imp, exists_eq_left] <;> unf <;>
      cases h0 : E.eqc ((oc .st).ret Reg.read) 0 <;> cases h1 : E.eqc ((oc .st).ret Reg.read) 1 <;>
      simp [comb, regW, fire, h0, h1, trR_go0 ops E.eqc oc, trR_go0' ops E.eqc oc, trR_go1 ops E.eqc oc,
        trR_go1' ops E.eqc oc, proj_cons_self, proj_cons_ne]
  enV _ _ m := m.elim
  arg oc w j cm _ s hs hav x hx := by
    have sk := skel E ((oc .st).ret Reg.read)
    simp only [users, List.mem_cons, List.not_mem_nil, or_false] at hs
    rcases hs with rfl | rfl <;> unf <;> simp only [fire] at hav
    · rw [trR_go0 ops E.eqc oc hav] at hx
      cases j <;> pj <;> (rcases hx with ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ <;> simp_all [comb, regW])
    · rw [trR_go1 ops E.eqc oc hav] at hx
      have h0 : E.eqc ((oc .st).ret Reg.read) 0 = false := by simpa [hav] using sk
      cases j <;> pj <;> (rcases hx with ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩ <;> simp_all [comb, regW])
  alw oc w j cm _ h := by simp [cregAnn] at h
  onehot oc w _ := skel E ((oc .st).ret Reg.read)

/-- **The obligation, for every `V`, every operators and every `EqC`.** -/
theorem obl : Obl (P ops E.eqc) := obl_of_cert (P ops E.eqc) (cert ops E)
end
end Fsm

/-! Without the law, the two comparisons are unrelated: with every
comparison true, both rules fire and the obligation fails (the AND-OR mux
is not one-hot). -/
namespace FsmNo

def natOps : Fsm.Ops Nat := ⟨0, id, 5, 6⟩

theorem not_obl : ¬ Obl (Fsm.P natOps (fun _ _ => true)) := by
  intro h
  obtain ⟨hoh, -⟩ := h (fun _ => ⟨fun _ => true, fun _ => 0⟩) ⟨fun m => m.elim, fun m => m.elim⟩ []
    trivial (by noal) (legal_nil _ (fun m => m.elim)) ⟨fun m => m.elim, fun m => m.elim⟩ (fun _ h => by simp at h)
  exact absurd hoh (by decide)

end FsmNo

/-! ## System tasks (decision 5)

`mkShow` has children `c : mkChild` and `T : Tasks 1`, its one `$display`
statement.  Rule `show` is `if (isOn c.getB) $display(c.getA)`, rule `bump`
is `c.setA(inc c.getA)`.  `comb` drives `T`'s enable with the statement's
condition and its argument with `c.getA`.  `Show.top` is the contract with
the module's task trace (log `⟨.T, ()⟩`) in it, and `Show.hw_trace` spells
that trace out: the hardware prints `c.getA` exactly when `isOn c.getB`. -/
namespace Show

inductive K where
  | c | T
  deriving DecidableEq

inductive R where
  | show | bump
  deriving DecidableEq

abbrev CI : K → Iface
  | .c => Child.X
  | .T => Tasks.X 1

structure Ops (V : Type) where
  z : V
  inc : V → V
  isOn : V → Bool

section
variable {V : Type} (ops : Ops V)

/-- rule show: if (isOn c.getB) $display(c.getA);  rule bump: c.setA(inc c.getA) -/
def rbody : R → Prog K (fun j => (CI j).M) V
  | .show => .call .c Child.M.getB ops.z (fun b => .call .c Child.M.getA ops.z (fun a =>
      Prog.callIf (ops.isOn b) .T (0 : Fin 1) a ops.z (fun _ => .ret ops.z)))
  | .bump => .call .c Child.M.getA ops.z (fun v => .call .c Child.M.setA (ops.inc v) (fun _ => .ret ops.z))

/-- The display's enable is its condition (`good`), or wrongly WILL_FIRE_show alone. -/
def comb (good : Bool) (oc : (j : K) → Outs (CI j).M V) (w : Wires Empty V) :
    ((j : K) → Wires (CI j).M V) × Outs Empty V :=
  (fun j => match j with
    | .c => Top.childW ops.z true (ops.inc ((oc .c).ret Child.M.getA))
    | .T => ⟨fun _ => if good then ops.isOn ((oc .c).ret Child.M.getB) else true,
             fun _ => (oc .c).ret Child.M.getA⟩,
   ⟨fun m => m.elim, fun m => m.elim⟩)

abbrev P (good : Bool) : Parent V where
  I := K
  decI := inferInstance
  kids := [.c, .T]
  kids_nodup := by decide
  kids_all := fun j => by cases j <;> decide
  CI := CI
  X := NoX
  PR := R
  body := fun m => m.elim
  rbody := rbody ops
  comb := comb ops good
  asm _ _ := True
  onehot _ _ := True

variable (good : Bool) (oc : (j : K) → Outs (CI j).M V)
theorem trR_show_on (h : ops.isOn ((oc .c).ret .getB) = true) : trR (P ops good) oc .show =
    ([⟨.c, (.getB, ops.z)⟩, ⟨.c, (.getA, ops.z)⟩, ⟨.T, ((0 : Fin 1), (oc .c).ret .getA)⟩], some ops.z) := by
  simp [trR, rbody, trace, Prog.callIf, h]
theorem trR_show_off (h : ops.isOn ((oc .c).ret .getB) = false) : trR (P ops good) oc .show =
    ([⟨.c, (.getB, ops.z)⟩, ⟨.c, (.getA, ops.z)⟩], some ops.z) := by
  simp [trR, rbody, trace, Prog.callIf, h]
theorem trR_bump : trR (P ops good) oc .bump =
    ([⟨.c, (.getA, ops.z)⟩, ⟨.c, (.setA, ops.inc ((oc .c).ret .getA))⟩], some ops.z) := rfl
end

def fp : Empty ⊕ R → List (Σ j, (CI j).M)
  | .inl m => m.elim
  | .inr .show => [⟨.c, Child.M.getB⟩, ⟨.c, Child.M.getA⟩, ⟨.T, (0 : Fin 1)⟩]
  | .inr .bump => [⟨.c, Child.M.getA⟩, ⟨.c, Child.M.setA⟩]

def users : (j : K) → (CI j).M → List (Empty ⊕ R)
  | .c, .getA => [.inr .show, .inr .bump]
  | .c, .getB => [.inr .show]
  | .c, .setA => [.inr .bump]
  | .T, _ => [.inr .show]

macro "showan" : tactic => `(tactic| simp [CI, Child.X, Child.ann, Tasks.X, taskAnn, Ann.Follow, Ann.Group] at *)

section
variable {V : Type} (ops : Ops V)

/-- `show`'s calls may precede `bump`'s, whether or not it displays. -/
theorem okT_show_bump (oc : (j : K) → Outs (CI j).M V) :
    OkT (P ops true) (trR (P ops true) oc .show).1 (trR (P ops true) oc .bump).1 := by
  intro j a ha b hb
  rw [trR_bump] at hb
  cases hc : ops.isOn ((oc .c).ret .getB)
  · rw [trR_show_off ops true oc hc] at ha
    cases j <;> pj <;> (rcases ha with rfl | rfl) <;> (rcases hb with rfl | rfl) <;> showan
  · rw [trR_show_on ops true oc hc] at ha
    cases j <;> pj
    · rcases ha with rfl | rfl <;> (rcases hb with rfl | rfl) <;> showan

/-- The certificate: `show` before `bump` (`getA SB setA`); the display's
    enable and argument, per user, like any child port's. -/
def cert : Cert (P ops true) where
  rules := [.show, .bump]
  rules_nodup := by simp
  fire _ _ _ := true
  hit _ _ m _ := m.elim
  fp := fp
  users := users
  users_ok s := by rcases s with m | r; exact m.elim; cases r <;> simp [fp, users]
  fp_ok oc w s c hc := by
    rcases s with m | r
    · exact m.elim
    · cases r
      · cases h : ops.isOn ((oc .c).ret .getB)
        · simp only [trS, trR_show_off ops true oc h, List.mem_cons, List.not_mem_nil, or_false] at hc
          rcases hc with rfl | rfl <;> simp [fp]
        · simp only [trS, trR_show_on ops true oc h, List.mem_cons, List.not_mem_nil, or_false] at hc
          rcases hc with rfl | rfl | rfl <;> simp [fp]
      · simp only [trS, trR_bump, List.mem_cons, List.not_mem_nil, or_false] at hc
        rcases hc with rfl | rfl <;> simp [fp]
  selfAnn m := m.elim
  fireIn _ _ r _ := by cases r <;> simp
  resM _ _ m := m.elim
  resR oc w r _ _ := by
    cases r
    · cases h : ops.isOn ((oc .c).ret .getB)
      · rw [trR_show_off ops true oc h]; rfl
      · rw [trR_show_on ops true oc h]; rfl
    · rfl
  rdyM _ _ m := m.elim
  rdyR oc w r _ _ c _ h := by rcases c with ⟨j, cm, x⟩; cases j <;> showan
  grpM _ _ m := m.elim
  grpR oc w r _ _ := by
    intro j a b h
    cases r
    · cases hc : ops.isOn ((oc .c).ret .getB)
      · rw [trR_show_off ops true oc hc] at h
        cases j <;> pj <;> first | exact absurd h pair_sub_single | exact absurd h pair_sub_nil |
          (obtain ⟨rfl, rfl⟩ := pair_sub_two h; showan)
      · rw [trR_show_on ops true oc hc] at h
        cases j <;> pj <;> first | exact absurd h pair_sub_single | exact absurd h pair_sub_nil |
          (obtain ⟨rfl, rfl⟩ := pair_sub_two h; showan)
    · rw [trR_bump] at h
      cases j <;> pj <;> first | exact absurd h pair_sub_single | exact absurd h pair_sub_nil |
        (obtain ⟨rfl, rfl⟩ := pair_sub_two h; showan)
  mm _ _ m := m.elim
  mmg _ _ m := m.elim
  h1 _ _ m := m.elim
  h2 _ _ m := m.elim
  h3 _ _ m := m.elim
  h4 oc w r1 r2 _ _ _ hne hn := by
    cases r1 <;> cases r2
    · exact absurd rfl hne
    · exact absurd (okT_show_bump ops oc) hn
    · exact List.Sublist.refl _
    · exact absurd rfl hne
  en oc w j cm _ hcm := by
    cases j
    · cases cm
      · showan
      · showan
      · simp only [users, List.mem_cons, List.not_mem_nil, or_false, exists_eq_or_imp, exists_eq_left]
        unf
        simp [comb, Top.childW, trR_bump, proj_cons_self]
    · rw [fin1 cm]
      simp only [users, List.mem_cons, List.not_mem_nil, or_false, exists_eq_or_imp, exists_eq_left]
      unf
      cases hc : ops.isOn ((oc .c).ret .getB)
      · simp [comb, hc, trR_show_off ops true oc hc, proj_cons_ne, proj_nil]
      · simp [comb, hc, trR_show_on ops true oc hc, proj_cons_ne, proj_cons_self, proj_nil]
  enV _ _ m := m.elim
  arg oc w j cm _ s hs _ x hx := by
    cases j
    · cases cm <;> simp only [users, List.mem_cons, List.not_mem_nil, or_false] at hs
      · rcases hs with rfl | rfl <;> unf
        · cases hc : ops.isOn ((oc .c).ret .getB)
          · rw [trR_show_off ops true oc hc] at hx; pj; subst hx; rfl
          · rw [trR_show_on ops true oc hc] at hx; pj; subst hx; rfl
        · rw [trR_bump] at hx; pj; subst hx; rfl
      · subst hs; unf
        cases hc : ops.isOn ((oc .c).ret .getB)
        · rw [trR_show_off ops true oc hc] at hx; pj; subst hx; rfl
        · rw [trR_show_on ops true oc hc] at hx; pj; subst hx; rfl
      · subst hs; unf; rw [trR_bump] at hx; pj; subst hx; rfl
    · simp only [users, List.mem_cons, List.not_mem_nil, or_false] at hs
      subst hs; unf
      cases hc : ops.isOn ((oc .c).ret .getB)
      · rw [trR_show_off ops true oc hc] at hx; pj
      · rw [trR_show_on ops true oc hc] at hx; pj; rcases hx with ⟨-, rfl⟩; rfl
  alw oc w j cm _ h := by cases j <;> showan
  onehot _ _ _ := trivial

/-- **mkShow's obligation**: the display's condition and argument, and its
    place before `bump`'s write, are checked like any child call. -/
theorem obl : Obl (P ops true) := obl_of_cert (P ops true) (cert ops)

/-- What `comb` reads: `c.setA`'s data reads `c.getA`; the display's enable
    and argument read `c.getB` and `c.getA`.  No child has a path. -/
def acyc : Acyc (P ops true) :=
  Acyc.ofMoore (P ops true)
    (fun j p => match j, p with
      | .c, .setA => [⟨.c, Child.M.getA⟩]
      | .T, _ => [⟨.c, Child.M.getB⟩, ⟨.c, Child.M.getA⟩]
      | _, _ => [])
    (fun _ _ => []) (fun q => q.elim) (fun q => q.elim)
    (by
      intro oc oc' w w' j p hD hW
      cases j
      · cases p <;> dep <;> simp [comb, Top.childW, *]
      · dep; simp [comb, *])
    (by intro oc oc' w w' q; exact q.elim)
    (fun j p q => by cases j <;> rfl) (fun q => q.elim)

abbrev impl : (j : K) → Module V (CI j)
  | .c => Child.mod (⟨ops.z, ops.inc⟩ : ToyOps V)
  | .T => Tasks 1

abbrev mod : Module V NoX := (P ops true).toModule (impl ops)

theorem built [Inhabited V] : Built NoX (mod ops) :=
  Built.node (P ops true) (impl ops) (fun j => match j with
    | .c => Child.built _
    | .T => Built.leaf _ (tasks_realizes 1) (tasks_causal 1))
    (fun j => aliased_of_none _ _ (fun p q => by cases j <;> rfl) (fun a p => by cases j <;> rfl))
    (obl ops) (acyc ops)

/-- **mkShow realizes its contract, task trace included, and is not vacuous.** -/
theorem top [Inhabited V] : Realizes (mod ops) ∧
    ∀ s w, (mod ops).clean s = s → ∃ o s' g, (mod ops).hw s w o s' g := (built ops).top

/-- The hardware's trace of mkShow's own log: `c.getA` when `isOn c.getB`.
    `top` equates it with the run's calls of `T`. -/
theorem hw_trace (s : (mod ops).S) (w : Wires Empty V) (o : Outs Empty V) (s' : (mod ops).S)
    (g : (mod ops).Sig) (h : (mod ops).hw s w o s' g) :
    (mod ops).trc g ⟨.T, ()⟩ =
      if ops.isOn ((g.oc .c).ret .getB) then [(0, (g.oc .c).ret .getA)] else [] := by
  obtain ⟨-, hch, -, -, hd⟩ := h
  obtain ⟨hwc, -⟩ := hd trivial
  obtain ⟨-, hT⟩ := hch .T
  show taskTrace (g.sub .T) = _
  rw [hT, hwc]
  cases hc : ops.isOn ((g.oc .c).ret .getB) <;> simp [taskTrace, comb, hc, List.finRange]

/-- **The display, end to end**: in every covered cycle, a one-at-a-time run
    that agrees with the hardware makes exactly the hardware's display calls:
    one, of `c.getA`, when `isOn c.getB`, else none. -/
theorem display [Inhabited V] (s : (mod ops).S) (σ : List (List (Empty × V))) (w : Wires Empty V)
    (o : Outs Empty V) (s' : (mod ops).S) (g : (mod ops).Sig) (hc : (mod ops).clean s = s)
    (hL : Legal NoX.ann σ) (hD : Drives NoX.ann w σ) (hh : (mod ops).hw s w o s' g) (hok : (mod ops).ok g) :
    ∃ τ fin, groups τ = σ ∧ Exec (mod ops) o s τ fin ∧ s' = (mod ops).clean fin ∧
      (mod ops).log fin ⟨.T, ()⟩ =
        if ops.isOn ((g.oc .c).ret .getB) then [(0, (g.oc .c).ret .getA)] else [] := by
  obtain ⟨τ, fin, h1, h2, h3, h4⟩ := (top ops).1 s σ w o s' g hc hL hD hh hok (fun c _ _ => c.1.elim)
  exact ⟨τ, fin, h1, h2, h3, (h4 ⟨.T, ()⟩).symm.trans (hw_trace ops s w o s' g hh)⟩
end
end Show

/-! ## (k) The display's read placed after the child write

Rule `late` is `c.setA(c.getA + 1); $display(c.getA)` with the display's
read emitted after the write.  `getA SB setA` forbids it. -/
namespace ShowLate
open Show (K CI)

def rbody : Unit → Prog K (fun j => (CI j).M) Nat
  | () => .call .c Child.M.getA 0 (fun v => .call .c Child.M.setA (v + 1) (fun _ =>
      .call .c Child.M.getA 0 (fun a => .call .T (0 : Fin 1) a (fun _ => .ret 0))))

abbrev P : Parent Nat where
  I := K
  decI := inferInstance
  kids := [.c, .T]
  kids_nodup := by decide
  kids_all := fun j => by cases j <;> decide
  CI := CI
  X := NoX
  PR := Unit
  body := fun m => m.elim
  rbody := rbody
  comb := fun oc _ => (fun j => match j with
    | .c => Top.childW 0 true ((oc .c).ret Child.M.getA + 1)
    | .T => ⟨fun _ => true, fun _ => (oc .c).ret Child.M.getA⟩, ⟨fun m => m.elim, fun m => m.elim⟩)
  asm _ _ := True
  onehot _ _ := True

/-- The display is enabled, so `late` is in every order, and its own calls on
    `c` put `getA` after `setA`. -/
theorem not_obl : ¬ Obl P := by
  intro h
  let oc : (j : K) → Outs (CI j).M Nat := fun _ => ⟨fun _ => true, fun _ => 0⟩
  obtain ⟨-, τP, hg, -, hj⟩ := h oc ⟨fun m => m.elim, fun m => m.elim⟩ [] trivial (by noal)
    (legal_nil _ (fun m => m.elim)) ⟨fun m => m.elim, fun m => m.elim⟩ (fun _ h => by simp at h)
  obtain ⟨x, hx⟩ := ((hj .T).2.1.en (0 : Fin 1) rfl).mp rfl
  obtain ⟨l, hl, hxl⟩ := List.mem_flatten.mp hx
  obtain ⟨e, he, rfl⟩ := List.mem_map.mp hl
  rcases e with g | r
  · have := mem_groups_of_inl he; rw [hg] at this; simp at this
  · have hc : proj K.c (stepTrace P oc (.inr r)) =
        [(Child.M.getA, 0), (Child.M.setA, 1), (Child.M.getA, 0)] := rfl
    have hgrp := (hj .c).1.group _ (List.mem_map.mpr ⟨_, he, rfl⟩) (Child.M.setA, 1) (Child.M.getA, 0)
      (by rw [hc]; exact List.Sublist.cons _ (List.Sublist.refl _))
    have := hgrp.1.1 (by decide)
    simp [Child.X, Child.ann] at this

end ShowLate

/-! ## (l) The display fired under the wrong condition

`Show` with `T`'s enable WILL_FIRE_show alone, omitting `isOn c.getB`. -/
namespace ShowCond
open Show (K CI R)

def natOps : Show.Ops Nat := ⟨0, (· + 1), (· != 0)⟩

/-- With `c.getB = 0` no step calls the display, but its enable is on. -/
theorem not_obl : ¬ Obl (Show.P natOps false) := by
  intro h
  let oc : (j : K) → Outs (CI j).M Nat := fun _ => ⟨fun _ => true, fun _ => 0⟩
  obtain ⟨-, τP, hg, -, hj⟩ := h oc ⟨fun m => m.elim, fun m => m.elim⟩ [] trivial (by noal)
    (legal_nil _ (fun m => m.elim)) ⟨fun m => m.elim, fun m => m.elim⟩ (fun _ h => by simp at h)
  obtain ⟨x, hx⟩ := ((hj .T).2.1.en (0 : Fin 1) rfl).mp rfl
  obtain ⟨l, hl, hxl⟩ := List.mem_flatten.mp hx
  obtain ⟨e, he, rfl⟩ := List.mem_map.mp hl
  rcases e with g | r
  · have := mem_groups_of_inl he; rw [hg] at this; simp at this
  · have h0 : ∀ r : R, proj K.T (stepTrace (Show.P natOps false) oc (.inr r)) = [] := by
      intro r; cases r <;> rfl
    rw [h0] at hxl; simp at hxl

end ShowCond

/-! ## (m) Two displays in the wrong statement order

Rule `wr` is `c.setA(0); $display(0)` (statement 0), rule `rd` is
`$display(c.getA)` (statement 1).  The child forces `rd` before `wr`
(`getA SB setA`), so the run prints statement 1 first; the hardware prints
statement 0 first. -/
namespace TaskOrder
open Show (K)

inductive R where
  | wr | rd
  deriving DecidableEq

abbrev CI : K → Iface
  | .c => Child.X
  | .T => Tasks.X 2

def rbody : R → Prog K (fun j => (CI j).M) Nat
  | .wr => .call .c Child.M.setA 0 (fun _ => .call .T (0 : Fin 2) 0 (fun _ => .ret 0))
  | .rd => .call .c Child.M.getA 0 (fun a => .call .T (1 : Fin 2) a (fun _ => .ret 0))

abbrev P : Parent Nat where
  I := K
  decI := inferInstance
  kids := [.c, .T]
  kids_nodup := by decide
  kids_all := fun j => by cases j <;> decide
  CI := CI
  X := NoX
  PR := R
  body := fun m => m.elim
  rbody := rbody
  comb := fun oc _ => (fun j => match j with
    | .c => Top.childW 0 true 0
    | .T => ⟨fun _ => true, fun t => if t = 0 then 0 else (oc .c).ret Child.M.getA⟩,
    ⟨fun m => m.elim, fun m => m.elim⟩)
  asm _ _ := True
  onehot _ _ := True

/-- Both displays are enabled, so both rules are in every order: `wr` first
    breaks `getA SB setA`, `rd` first breaks the statement order. -/
theorem not_obl : ¬ Obl P := by
  intro h
  let oc : (j : K) → Outs (CI j).M Nat := fun _ => ⟨fun _ => true, fun _ => 0⟩
  obtain ⟨-, τP, hg, -, hj⟩ := h oc ⟨fun m => m.elim, fun m => m.elim⟩ [] trivial (by noal)
    (legal_nil _ (fun m => m.elim)) ⟨fun m => m.elim, fun m => m.elim⟩ (fun _ h => by simp at h)
  have hT0 : proj K.T (stepTrace P oc (.inr .wr)) = [((0 : Fin 2), 0)] := rfl
  have hT1 : proj K.T (stepTrace P oc (.inr .rd)) = [((1 : Fin 2), 0)] := rfl
  have hc0 : proj K.c (stepTrace P oc (.inr .wr)) = [(Child.M.setA, 0)] := rfl
  have hc1 : proj K.c (stepTrace P oc (.inr .rd)) = [(Child.M.getA, 0)] := rfl
  -- the step that calls statement `t` is in the order
  have hin : ∀ t : Fin 2, ∃ r, (.inr r : List (Empty × Nat) ⊕ R) ∈ τP ∧
      ∃ x, (t, x) ∈ proj K.T (stepTrace P oc (.inr r)) := by
    intro t
    obtain ⟨x, hx⟩ := ((hj .T).2.1.en t rfl).mp rfl
    obtain ⟨l, hl, hxl⟩ := List.mem_flatten.mp hx
    obtain ⟨e, he, rfl⟩ := List.mem_map.mp hl
    rcases e with g | r
    · have := mem_groups_of_inl he; rw [hg] at this; simp at this
    · exact ⟨r, he, x, hxl⟩
  obtain ⟨r0, h0, x0, hx0⟩ := hin 0
  obtain ⟨r1, h1, x1, hx1⟩ := hin 1
  have e0 : r0 = .wr := by
    cases r0
    · rfl
    · rw [hT1] at hx0; simp at hx0
  have e1 : r1 = .rd := by
    cases r1
    · rw [hT0] at hx1; simp at hx1
    · rfl
  subst e0 e1
  rcases either_sublist h0 h1 (by simp) with hs | hs
  · have := ((hj .c).1.order _ _ (pair_flatten_map (fun e => proj K.c (stepTrace P oc e)) hs
      (x := (Child.M.setA, 0)) (y := (Child.M.getA, 0)) (by rw [hc0]; simp) (by rw [hc1]; simp))).1 (by decide)
    simp [Child.X, Child.ann] at this
  · have := ((hj .T).1.order _ _ (pair_flatten_map (fun e => proj K.T (stepTrace P oc e)) hs
      (x := ((1 : Fin 2), 0)) (y := ((0 : Fin 2), 0)) (by rw [hT1]; simp) (by rw [hT0]; simp))).1 (by decide)
    simp [Tasks.X, taskAnn] at this

end TaskOrder

/-! ## The contract compares traces

`Tasks 2` with hardware that runs its two statements in reverse order does
not realize the contract: the trace part of `Realizes` has content. -/
namespace TaskRev

abbrev mod : Module Nat (Tasks.X 2) := { Tasks 2 with trc := fun g _ => (taskTrace g).reverse }

theorem execG_log (o : Outs (Fin 2) Nat) (s t : mod.S) (l : List (Fin 2 × Nat))
    (h : ExecG mod o s l t) : t = s ++ l.map (fun c => (c.1.val, c.2)) := by
  induction h with
  | nil s => simp
  | @cons s s1 s2 m x l hm _ ih =>
    have e : s1 = s ++ [(m.val, x)] := hm
    rw [ih, e]; simp

theorem exec_log (o : Outs (Fin 2) Nat) (s t : mod.S) (τ : List (List (Fin 2 × Nat) ⊕ mod.R))
    (h : Exec mod o s τ t) : t = s ++ (groups τ).flatten.map (fun c => (c.1.val, c.2)) := by
  induction h with
  | nil s => simp [groups]
  | @grp s s1 s2 g τ hg _ ih => rw [ih, execG_log o _ _ _ hg]; simp [groups]
  | @rule s s1 s2 r τ hr _ ih => exact r.elim

/-- One caller calls statement 0, then statement 1. -/
def σ2 : List (List (Fin 2 × Nat)) := [[(0, 5), (1, 6)]]
def w2 : Wires (Fin 2) Nat := ⟨fun _ => true, fun t => if t = 0 then 5 else 6⟩

theorem legal2 : Legal (taskAnn 2) σ2 := by
  refine ⟨fun c1 c2 hs => ?_, fun g hg c1 c2 hs => ?_, fun m hm => by simp [taskAnn] at hm⟩
  · simp only [σ2, List.flatten_cons, List.flatten_nil, List.append_nil] at hs
    obtain ⟨rfl, rfl⟩ := pair_sub_two hs
    simp [Ann.Follow, taskAnn]
  · simp only [σ2, List.mem_singleton] at hg
    subst hg
    obtain ⟨rfl, rfl⟩ := pair_sub_two hs
    simp [Ann.Group, Ann.Follow, taskAnn]

theorem drives2 : Drives (taskAnn 2) w2 σ2 := by
  refine ⟨fun m _ => ⟨fun _ => ⟨w2.arg m, ?_⟩, fun _ => rfl⟩, fun m x hx => ?_⟩
  · revert m; rw [Fin.forall_fin_two]
    exact ⟨fun _ _ => by simp [σ2, w2], fun _ _ => by simp [σ2, w2]⟩
  · have key : ∀ m : Fin 2, lastArg m σ2.flatten = some (w2.arg m) := by
      rw [Fin.forall_fin_two]; exact ⟨rfl, rfl⟩
    rw [key m] at hx; exact Option.some.inj hx

/-- The hardware prints statement 1 before statement 0; every run that
    agrees with it prints 0 before 1. -/
theorem not_realizes : ¬ Realizes mod := by
  intro h
  obtain ⟨τ, fin, hg, hex, -, htr⟩ := h [] σ2 w2 ⟨fun _ => true, fun _ => 0⟩ [] w2 rfl legal2 drives2
    ⟨rfl, rfl⟩ trivial (fun c _ h => by simp [taskAnn] at h)
  have hfin := exec_log _ _ _ _ hex
  rw [hg] at hfin
  have := htr ()
  rw [hfin] at this
  revert this; decide

end TaskRev


end Ex

#print axioms Ex.Child.obl
#print axioms Ex.Child.realizes
#print axioms Ex.ChildSB.not_obl
#print axioms Ex.ChildSB.no_cert
#print axioms Ex.ChildSB.not_realizes
#print axioms Ex.Top.obl
#print axioms Ex.Top.realizes
#print axioms Ex.Top.top
#print axioms Ex.Child.built
#print axioms Ex.Put.obl
#print axioms Ex.PutBad.not_obl
#print axioms Ex.Order.obl
#print axioms Ex.Order.not_obl
#print axioms Ex.Asm.skel
#print axioms Ex.Asm.obl
#print axioms Ex.Asm.built
#print axioms Ex.Asm.top
#print axioms Ex.Asm.ok_iff
#print axioms Ex.Asm.asm_sat
#print axioms Ex.AsmNo.not_obl
#print axioms Ex.NoRdy.not_obl
#print axioms Ex.ShareArg.not_obl
#print axioms Ex.NoAlw.not_obl
#print axioms Ex.WireEx.obl
#print axioms Ex.WireEx.built
#print axioms Ex.WireEx.top
#print axioms Ex.Loop.obl
#print axioms Ex.Loop.hw_empty
#print axioms Ex.Loop.no_acyc
#print axioms Ex.AlwEnq.not_obl
#print axioms Ex.AlwRdy.not_obl
#print axioms Ex.AliasEx.obl
#print axioms Ex.AliasEx.built
#print axioms Ex.Fsm.skel
#print axioms Ex.Fsm.obl
#print axioms Ex.FsmNo.not_obl
#print axioms Ex.Show.obl
#print axioms Ex.Show.built
#print axioms Ex.Show.top
#print axioms Ex.Show.hw_trace
#print axioms Ex.Show.display
#print axioms Ex.ShowLate.not_obl
#print axioms Ex.ShowCond.not_obl
#print axioms Ex.TaskOrder.not_obl
#print axioms Ex.TaskRev.not_realizes
