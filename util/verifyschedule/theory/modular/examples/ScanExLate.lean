/- An escaped pair: rule `late` reads `a` after `incA` writes it, but only when `incA` cannot
   fire; the scan fails without the escape and passes with its dynamic `OkT`. -/
import ScanEx

open Modular Scan ScanEx
open Emit (Tab)

namespace ScanExLate

variable {V : Type} (z : V) (inc : V → V) (tB : V → Bool)

/-- Rules: 0 `incA`, 1 `copy`, 2 `drain`, 3 `late`. -/
def rbody : Fin 4 → Prog (Fin 3) (fun j => (CI j).M) V
  | 0 => ScanEx.rbody z inc 0
  | 1 => ScanEx.rbody z inc 1
  | 2 => ScanEx.rbody z inc 2
  | 3 => .call (CM := fun j => (CI j).M) 2 FifoM.i_notEmpty z (fun e =>
          Prog.callIf (CM := fun j => (CI j).M) (tB e) 0 Reg.read z z (fun v =>
            .call (CM := fun j => (CI j).M) 1 Reg.write v (fun _ => .ret z)))

abbrev P : Parent V where
  I := Fin 3
  decI := inferInstance
  kids := List.finRange 3
  kids_nodup := List.nodup_finRange 3
  kids_all := List.mem_finRange
  CI := CI
  X := X
  PR := Fin 4
  body := body z
  rbody := rbody z inc tB
  comb := fun _ _ => (fun _ => ⟨fun _ => false, fun _ => z⟩, ⟨fun _ => false, fun _ => z⟩)
  asm := fun _ _ => True
  onehot := fun _ _ => True

/-- `incA` fires only when `f.i_notEmpty` reads false. -/
def fire (oc : (j : (P z inc tB).I) → Outs ((P z inc tB).CI j).M V) (_ : Wires (P z inc tB).X.M V)
    (r : (P z inc tB).PR) : Bool :=
  if r = 0 then !tB ((oc 2).ret FifoM.i_notEmpty) else true

/-- Rank positions: 0 `copy`, 1 `incA`, 2 `late`, 3 `drain`; `late` escapes `incA`. -/
def D : Design where
  nM := 2
  nR := 4
  d := 4
  fpM := .node 1 (.leaf [(2, 0, 0)]) (.leaf [(4, 1, 0)])
  fpR := .node 2 (.node 1 (.leaf [(0, 0, 0), (2, 0, 1)]) (.leaf [(0, 0, 0), (0, 0, 1)]))
                 (.node 3 (.leaf [(4, 1, 5), (0, 0, 0), (2, 0, 1)]) (.leaf [(4, 1, 2), (4, 1, 1), (0, 0, 1)]))
  kt := .node 1 (.leaf regKC) (.leaf fifoKC)
  nfI := .node 1 (.leaf 0) (.leaf 2)
  nfO := .node 1 (.leaf 0) (.leaf 2)
  ngI := .node 1 (.leaf 0) (.leaf 2)
  ngO := .node 1 (.leaf 0) (.leaf 2)
  h3ok := .leaf 0
  exM := .leaf 0
  exR := .leaf 0
  exMR := .leaf 0
  escR := .node 2 (.leaf 0) (.node 3 (.leaf 2) (.leaf 0))

theorem ok : D.Ok := D.ok_of_chain [] (show D.segLast 0 ST.init = true by decide +kernel)

/-- Without the escape the scan fails. -/
theorem no_escape : (({ D with escR := .leaf 0 } : Design).segLast 0 ST.init) = false := by decide +kernel

/-- Had `late` read `f.notEmpty` (SB `enq`), `put` would have to follow `late` and `get` precede
    it, with neither SBR nor C exported: the scan fails (`h3`). -/
def fpBad : Tab (List Ent) :=
  .node 2 (.node 1 (.leaf [(0, 0, 0), (2, 0, 1)]) (.leaf [(0, 0, 0), (0, 0, 1)]))
    (.node 3 (.leaf [(4, 1, 3), (0, 0, 0), (2, 0, 1)]) (.leaf [(4, 1, 2), (4, 1, 1), (0, 0, 1)]))

theorem h3_catches : (({ D with fpR := fpBad } : Design).segLast 0 ST.init) = false := by decide +kernel

def ruleAt (i : Nat) : Fin 4 := if i = 0 then 1 else if i = 1 then 0 else if i = 2 then 3 else 2

def rk : Fin 4 → Nat
  | 0 => 1
  | 1 => 0
  | 2 => 3
  | 3 => 2

def G : Glue (P z inc tB) D where
  mi := fun m => m.val
  ruleAt := ruleAt
  rk := rk
  base := fun j => if j = 0 then 0 else if j = 1 then 2 else 4
  kind := fun j => if j = 2 then 1 else 0
  code := code
  fx := fx
  z := z

theorem escR_get (i : Nat) : D.escR.get i = if i = 2 then 2 else 0 := by
  show (bif Nat.blt i 2 then _ else _) = _
  by_cases h1 : i < 2
  · rw [Nat.blt_eq.mpr h1]; simp only [Bool.cond_true, Tab.get]; simp only [show ¬ i = 2 by omega, ite_false]
  · have : Nat.blt i 2 = false := by cases h : Nat.blt i 2; rfl; exact absurd (Nat.blt_eq.mp h) h1
    rw [this]; simp only [Bool.cond_false, Tab.get]
    by_cases h2 : i < 3
    · rw [Nat.blt_eq.mpr h2]; simp only [Bool.cond_true, Tab.get]; simp only [show i = 2 by omega, ite_true]
    · have : Nat.blt i 3 = false := by cases h : Nat.blt i 3; rfl; exact absurd (Nat.blt_eq.mp h) h2
      rw [this]; simp only [Bool.cond_false, Tab.get]; simp only [show ¬ i = 2 by omega, ite_false]

/-- Ports' static `Follow`, whatever the arguments. -/
def okP (j : (P z inc tB).I) (a b : ((P z inc tB).CI j).M) : Bool :=
  folP ((P z inc tB).CI j).ann a b && (!decide (a = b) || !((P z inc tB).CI j).ann.share a)

theorem okP_follow : ∀ j (a b : ((P z inc tB).CI j).M × V), okP z inc tB j a.1 b.1 = true →
    ((P z inc tB).CI j).ann.Follow a b := by
  intro j a b h
  simp only [okP, Bool.and_eq_true, Bool.or_eq_true, Bool.not_eq_true', decide_eq_false_iff_not] at h
  obtain ⟨hf, hs⟩ := h
  simp only [folP, Bool.and_eq_true, Bool.or_eq_true, decide_eq_true_eq, Bool.not_eq_true'] at hf
  obtain ⟨⟨h11, h12⟩, h13⟩ := hf
  refine ⟨fun hne => ?_, h12, h13, fun he hsh => ?_⟩
  · rcases h11 with h | h
    · exact absurd h hne
    · exact h
  · rcases hs with h | h
    · exact absurd he h
    · rw [hsh] at h; cases h

/-- **The escaped pair's dynamic proof**: when `incA` fires, `late` skips its read of `a`. -/
theorem incA_late (oc : (j : (P z inc tB).I) → Outs ((P z inc tB).CI j).M V) (w : Wires (P z inc tB).X.M V)
    (hf : fire z inc tB oc w 0 = true) :
    OkT (P z inc tB) (trR (P z inc tB) oc 0).1 (trR (P z inc tB) oc 3).1 := by
  have he : tB ((oc 2).ret FifoM.i_notEmpty) = false := by simpa [fire] using hf
  refine okT_except (P z inc tB) (okP z inc tB) (okP_follow z inc tB) _ _
    [⟨0, Reg.read⟩, ⟨0, Reg.write⟩] [⟨2, FifoM.i_notEmpty⟩, ⟨0, Reg.read⟩, ⟨1, Reg.write⟩]
    (fun c hc => ?_) (fun c hc => ?_) (fun j a b h1 _ _ _ hc2 => ?_)
  · simp [trR, trace, P, rbody, ScanEx.rbody] at hc
    rcases hc with rfl | rfl <;> simp
  · simp [trR, trace, Prog.callIf, P, rbody, he] at hc
    rcases hc with rfl | rfl <;> simp
  · simp only [List.mem_cons, Sigma.mk.injEq, List.not_mem_nil, or_false] at h1
    obtain ⟨x, hx⟩ := hc2
    rcases h1 with ⟨rfl, -⟩ | ⟨rfl, -⟩ <;>
      simp [trR, trace, Prog.callIf, P, rbody, he, proj] at hx

theorem glueOK : GlueOK (P z inc tB) D (G z inc tB) (fire z inc tB) where
  mi_lt := fun m => by show m.val < 2; exact m.isLt
  rk_lt := fun r => (by decide : ∀ r : Fin 4, rk r < 4) r
  rk_at := fun r => (by decide : ∀ r : Fin 4, ruleAt (rk r) = r) r
  at_rk := by show ∀ i, i < 4 → rk (ruleAt i) = i; decide
  kind := fun j _ => match j with
    | 0 => reg_kind z
    | 1 => reg_kind z
    | 2 => fifo_kind z
  fpM := fun oc w m => match m with
    | 0 => List.Sublist.refl _
    | 1 => List.Sublist.refl _
  fpR := fun oc r => match r with
    | 0 => List.Sublist.refl _
    | 1 => List.Sublist.refl _
    | 2 => List.Sublist.refl _
    | 3 => by
      cases h : tB ((oc 2).ret FifoM.i_notEmpty)
      · simp [trR, trace, Prog.callIf, P, rbody, h, Glue.ent, G, code, regCode, fifoCode, D, Tab.get, rk]
      · simp [trR, trace, Prog.callIf, P, rbody, h, Glue.ent, G, code, regCode, fifoCode, D, Tab.get, rk]
  zM := fun oc w m c hc hf => match m with
    | 0 => by simp [trM, trace, P, body] at hc; subst hc; rfl
    | 1 => by simp [trM, trace, P, body] at hc; subst hc; simp [G, fx, code, fifoCode] at hf
  zR := fun oc r c hc hf => match r with
    | 0 => by
      simp [trR, trace, P, rbody, ScanEx.rbody] at hc
      rcases hc with rfl | rfl
      · rfl
      · simp [G, fx, code, regCode] at hf
    | 1 => by
      simp [trR, trace, P, rbody, ScanEx.rbody] at hc
      rcases hc with rfl | rfl
      · rfl
      · simp [G, fx, code, regCode] at hf
    | 2 => by
      simp [trR, trace, P, rbody, ScanEx.rbody] at hc
      rcases hc with rfl | rfl | rfl
      · rfl
      · rfl
      · simp [G, fx, code, regCode] at hf
    | 3 => by
      cases h : tB ((oc 2).ret FifoM.i_notEmpty) <;> simp [trR, trace, Prog.callIf, P, rbody, h] at hc
      · rcases hc with rfl | rfl
        · rfl
        · simp [G, fx, code, regCode] at hf
      · rcases hc with rfl | rfl | rfl
        · rfl
        · rfl
        · simp [G, fx, code, regCode] at hf
  nfI := fun m1 m2 h x y hf => by
    obtain ⟨rfl, rfl⟩ := tab2 m1 m2 h
    exact absurd hf.2.1 (by simp [P, X, pAnn])
  nfO := fun m1 m2 h x y hf => by
    obtain ⟨rfl, rfl⟩ := tab2 m2 m1 h
    exact absurd hf.2.1 (by simp [P, X, pAnn])
  ngI := fun m1 m2 h x y hf => by
    obtain ⟨rfl, rfl⟩ := tab2 m1 m2 h
    exact absurd hf.1.2.1 (by simp [P, X, pAnn])
  ngO := fun m1 m2 h x y hf => by
    obtain ⟨rfl, rfl⟩ := tab2 m2 m1 h
    exact absurd hf.1.2.1 (by simp [P, X, pAnn])
  h3ok := fun m m' h => by simp [D, Tab.get] at h
  exM := fun _ _ _ _ _ h => by simp [D, Tab.get] at h
  exR := fun _ _ _ _ _ h => by simp [D, Tab.get] at h
  exMR := fun _ _ _ _ _ h => by simp [D, Tab.get] at h
  esc := esc_of_pairs (P z inc tB) (G z inc tB) (fire z inc tB) (fun r => (by decide : ∀ r : Fin 4, ruleAt (rk r) = r) r)
    [(1, 2)] (fun p hp oc w _ h1 _ => by
      simp only [List.mem_singleton] at hp
      subst hp
      exact incA_late z inc tB oc w h1)
    (fun i => by
      rw [escR_get]
      by_cases h : i = 2
      · subst h; exact sub_of_subB (by decide)
      · rw [if_neg h]; intro k hk; simp at hk)
  kindC := fun _ h => by cases h
  chAnn := fun _ h => by cases h
  cmono := fun _ _ _ => Nat.le_refl 0
  cinM := fun _ _ _ _ _ h => by cases h
  cinR := fun _ _ _ _ h => by cases h
  cincM := fun _ _ _ _ _ _ h => by cases h
  cincR := fun _ _ _ _ _ h => by cases h
  chCode := fun _ h => by cases h

def pairs : PairFacts (P z inc tB) (G z inc tB).rules (fire z inc tB) :=
  pairFacts_of_scan (P z inc tB) ok (G z inc tB) (fire z inc tB) (glueOK z inc tB)

end ScanExLate

#print axioms ScanExLate.no_escape
#print axioms ScanExLate.h3_catches
#print axioms ScanExLate.incA_late
#print axioms ScanExLate.pairs
