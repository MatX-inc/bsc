/- ExTop: mkTop's obligation. Imports mkChild's interface only, not its proof. -/
import ExLib
import ExChildIface
open Modular
open scoped List
set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false
namespace Ex.Top
open Ex
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

/-- What `comb` reads (mkChild exports no paths: its interface file says so,
    and its own certificate checks it). -/
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

end
end Ex.Top
