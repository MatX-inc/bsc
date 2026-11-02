/- ExChild: mkChild's obligation. Imports only its children's interfaces (Reg, in Modular). -/
import ExLib
import ExChildIface
open Modular
open scoped List
set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false
namespace Ex.Child
open Ex
/-- Instances: `a`, `b`, both `Reg`. -/
inductive K where
  | a | b
  deriving DecidableEq

inductive R where
  | move
  deriving DecidableEq

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

/-- What `comb` reads, for the acyclicity certificate (registers: no paths). -/
def acyc : Acyc (P ops) :=
  Acyc.ofMoore (P ops)
    (fun j p => match j, p with | .b, .wr _ => [⟨.a, Reg.read⟩] | _, _ => [])
    (fun j p => match j, p with | .a, .wr _ => [.setA] | _, _ => [])
    (fun q => match q with | .getA => [⟨.a, Reg.read⟩] | .getB => [⟨.b, Reg.read⟩] | .setA => [])
    (fun _ => [])
    (by intro oc oc' w w' j p hD hW; cases j <;> cases p <;> dep <;> simp [comb, regW, *])
    (by intro oc oc' w w' q hD hW; cases q <;> dep <;> simp [comb, *])
    (fun j p q => reg_nopath p q) (fun _ _ h => by simp at h)

end
end Ex.Child
