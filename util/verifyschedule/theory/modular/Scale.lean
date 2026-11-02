/-
Scale.lean: children and rules indexed by `Fin n` (review M3).

A parent with `n` register children and `n` rules, rule `r` doing
`c_r <= c_r` (a read and a write of its own register), as in
`mkCregInlineBlowup`'s 4096 identical rules and instances.  Children and
rules are `Fin n`; `kids := List.finRange n` with the library's
`List.nodup_finRange` and `List.mem_finRange`, so no file enumerates its
instances or rules to prove `kids_nodup`, `kids_all` or `rules_nodup`.  The
certificate is proved once for every `n`, and instantiated at 4096 below.

A heterogeneous design indexes the same way and reads per-instance data
(child kind, rule bodies, footprints, users) from tables (design.md §9).
-/
import Modular
open Modular
open scoped List

set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false

namespace Scale

def noAnn : Ann Empty where
  prec _ _ := false
  sbr _ _ := false
  conf _ _ := false
  share _ := false
  hasEn _ := false
  alwaysEn _ := false
  alwaysRdy _ := true
abbrev NoX : Iface := ⟨Empty, inferInstance, noAnn⟩

def regW {V : Type} (z : V) (en : Bool) (d : V) : Wires (CRegM 1) V :=
  ⟨fun m => match m with | .wr _ => en | .rd _ => false, fun m => match m with | .wr _ => d | .rd _ => z⟩

section
variable (n : Nat) {V : Type} (z : V)

abbrev P : Parent V where
  I := Fin n
  decI := inferInstance
  kids := List.finRange n
  kids_nodup := List.nodup_finRange n
  kids_all := List.mem_finRange
  CI := fun _ => Reg.X
  X := NoX
  PR := Fin n
  body := fun m => m.elim
  rbody := fun r => .call r Reg.read z (fun v => .call r Reg.write v (fun _ => .ret z))
  comb := fun oc _ => (fun j => regW z true ((oc j).ret Reg.read), ⟨fun m => m.elim, fun m => m.elim⟩)
  asm _ _ := True
  onehot _ _ := True

theorem trR_eq (oc : (j : Fin n) → Outs (CRegM 1) V) (r : Fin n) :
    trR (P n z) oc r = ([⟨r, (Reg.read, z)⟩, ⟨r, (Reg.write, (oc r).ret Reg.read)⟩], some z) := rfl

theorem proj_r (oc : (j : Fin n) → Outs (CRegM 1) V) (r j : Fin n) :
    proj j (trR (P n z) oc r).1 = if r = j then [(Reg.read, z), (Reg.write, (oc j).ret Reg.read)] else [] := by
  rw [trR_eq]
  by_cases h : r = j
  · subst h; simp [proj_cons_self, proj_nil]
  · simp [proj_cons_ne _ _ h, proj_nil, h]

theorem fin1 (i : Fin 1) : i = 0 := by obtain ⟨k, hk⟩ := i; congr; omega

def cert : Cert (P n z) where
  rules := List.finRange n
  rules_nodup := List.nodup_finRange n
  fire _ _ _ := true
  hit _ _ _ _ := false
  fp s := match s with
    | .inl m => m.elim
    | .inr r => [⟨r, Reg.read⟩, ⟨r, Reg.write⟩]
  users j _ := [.inr j]
  users_ok s p hp := by
    rcases s with m | r
    · exact m.elim
    · simp only [List.mem_cons, List.not_mem_nil, or_false] at hp
      rcases hp with rfl | rfl <;> simp
  fp_ok oc w s c hc := by
    rcases s with m | r
    · exact m.elim
    · simp only [trS, trR_eq, List.mem_cons, List.not_mem_nil, or_false] at hc
      rcases hc with rfl | rfl <;> simp
  selfAnn m := m.elim
  fireIn _ _ r _ := List.mem_finRange r
  resM oc w m := m.elim
  resR oc w r _ _ := rfl
  rdyM oc w m := m.elim
  rdyR oc w r _ _ c _ h := by simp [Reg.X, CReg.X, cregAnn] at h
  grpM oc w m := m.elim
  grpR oc w r _ _ := by
    intro j a b h
    rw [proj_r] at h
    split at h
    · obtain ⟨rfl, rfl⟩ := pair_sub_two h
      simp [Reg.X, CReg.X, cregAnn, Ann.Group, Ann.Follow]
    · exact absurd h pair_sub_nil
  mm oc w m := m.elim
  mmg oc w m := m.elim
  h1 oc w m := m.elim
  h2 oc w m := m.elim
  h3 oc w m := m.elim
  h4 oc w r1 r2 _ _ _ hne hn := by
    exfalso; apply hn
    intro j a ha b hb
    rw [proj_r] at ha hb
    split at ha <;> split at hb
    · rename_i h1 h2; exact absurd (h1.trans h2.symm) hne
    all_goals simp at ha hb
  en oc w j cm _ hcm := by
    cases cm with
    | rd i => simp [Reg.X, CReg.X, cregAnn] at hcm
    | wr i =>
      rw [fin1 i]
      simp only [List.mem_singleton, exists_eq_left, On, Calls, trS, proj_r, if_pos rfl]
      simp [regW]
  enV oc w m := m.elim
  arg oc w j cm _ s hs hav x hx := by
    simp only [List.mem_singleton] at hs
    subst hs
    simp only [trS, proj_r, if_pos rfl] at hx
    cases cm with
    | rd i => simp at hx; first | (obtain ⟨-, rfl⟩ := hx; rfl) | (subst hx; rfl)
    | wr i => simp at hx; first | (obtain ⟨-, rfl⟩ := hx; rfl) | (subst hx; rfl)
  alw oc w j cm _ h := by simp [Reg.X, CReg.X, cregAnn] at h
  onehot _ _ _ := trivial

theorem obl : Obl (P n z) := obl_of_cert (P n z) (cert n z)

abbrev mod : Module V NoX := (P n z).toModule (fun _ => Reg)

theorem reg_nopath (p q : CRegM 1) : (cregAnn 1).path p q = false := by
  cases p <;> cases q <;> rename_i a b <;> rw [fin1 a, fin1 b] <;> rfl

/-- Each register's write reads that register; no paths. -/
def acyc : Acyc (P n z) :=
  Acyc.ofMoore (P n z) (fun j p => match p with | .wr _ => [⟨j, Reg.read⟩] | .rd _ => [])
    (fun _ _ => []) (fun q => q.elim) (fun q => q.elim)
    (by
      intro oc oc' w w' j p hD hW
      cases p with
      | rd _ => exact ⟨rfl, rfl⟩
      | wr _ =>
        obtain ⟨-, h⟩ := hD ⟨j, Reg.read⟩ (List.mem_singleton.mpr rfl)
        exact ⟨rfl, by simp only [regW]; exact h⟩)
    (fun _ _ _ _ q => q.elim) (fun j p q => reg_nopath p q) (fun q => q.elim)

theorem built : Built NoX (mod n z) :=
  Built.node (P n z) (fun _ => Reg) (fun _ => Built.leaf _ reg_realizes (creg_causal 1))
    (fun _ => aliased_of_none _ _ (fun _ _ => rfl) (fun _ _ => rfl)) (obl n z) (acyc n z)

theorem realizes : Realizes (mod n z) := (built n z).realizes
end

/-- **4096 children and 4096 rules.** -/
theorem realizes4096 : Realizes (mod 4096 (0 : Nat)) := realizes 4096 0

/-- And it has a hardware cycle for every input (from 4096 register leaves
    and one acyclicity certificate, proved for every `n`). -/
theorem top4096 : Realizes (mod 4096 (0 : Nat)) ∧
    ∀ s w, (mod 4096 (0 : Nat)).clean s = s → ∃ o s' g, (mod 4096 (0 : Nat)).hw s w o s' g :=
  (built 4096 0).top

end Scale

#print axioms Scale.realizes4096
#print axioms Scale.top4096
