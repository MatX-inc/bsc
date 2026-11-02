/- Task statements through the scan as a chain kind: a register `a` and a `Tasks 3` child `T`,
   three rules.  Positive: statement order follows rank order, every glue fact discharged.
   Negative: statement order against rank order; no chain intervals exist and `OkT` fails. -/
import Scan

open Modular Scan
open Emit (Tab)

namespace ScanTasks

variable {V : Type} (z : V) (inc : V → V)

def CI : Fin 2 → Iface
  | 0 => Reg.X
  | 1 => Tasks.X 3

def xAnn : Ann (Fin 0) where
  prec p _ := p.elim0
  sbr p _ := p.elim0
  conf p _ := p.elim0
  share p := p.elim0
  hasEn p := p.elim0
  alwaysEn p := p.elim0
  alwaysRdy p := p.elim0

abbrev X : Iface := ⟨Fin 0, inferInstance, xAnn⟩

abbrev CM := fun j : Fin 2 => (CI j).M

/-- Task port `t` of `T`. -/
def tk (t : Nat) (h : t < 3 := by decide) : (CI 1).M := (⟨t, h⟩ : Fin 3)

/-- Rules (rank = number): 0 reads `a` and prints it (statement `s0`), 1 prints twice
    (`s1`, `s2`), 2 increments `a`. -/
def rbody (s0 s1 s2 : Nat) (h0 : s0 < 3) (h1 : s1 < 3) (h2 : s2 < 3) : Fin 3 → Prog (Fin 2) CM V
  | 0 => .call (CM := CM) 0 Reg.read z (fun v => .call (CM := CM) 1 (tk s0 h0) v (fun _ => .ret z))
  | 1 => .call (CM := CM) 1 (tk s1 h1) z (fun _ => .call (CM := CM) 1 (tk s2 h2) z (fun _ => .ret z))
  | 2 => .call (CM := CM) 0 Reg.read z (fun v => .call (CM := CM) 0 Reg.write (inc v) (fun _ => .ret z))

abbrev P (s0 s1 s2 : Nat) (h0 : s0 < 3) (h1 : s1 < 3) (h2 : s2 < 3) : Parent V where
  I := Fin 2
  decI := inferInstance
  kids := List.finRange 2
  kids_nodup := List.nodup_finRange 2
  kids_all := List.mem_finRange
  CI := CI
  X := X
  PR := Fin 3
  body := fun m _ => m.elim0
  rbody := rbody z inc s0 s1 s2 h0 h1 h2
  comb := fun _ _ => (fun _ => ⟨fun _ => false, fun _ => z⟩, ⟨fun m => m.elim0, fun m => m.elim0⟩)
  asm := fun _ _ => True
  onehot := fun _ _ => True

/-- The positive parent: statements in rank order. -/
abbrev Pok := P z inc 0 1 2 (by decide) (by decide) (by decide)

def regKC : Tab KC := .node 1 (.leaf ⟨[1], [], [1], []⟩) (.leaf ⟨[], [0], [1], [0, 1]⟩)

/-- A chain kind's table: one code, nothing forward, everything backward. -/
def taskKC : Tab KC := .leaf ⟨[], [0], [], [0]⟩

/-- Slots: `a` 0–1, `T` 2 (one slot for all its ports). -/
def D : Design where
  nM := 0
  nR := 3
  d := 2
  fpM := .leaf []
  fpR := .node 1 (.leaf [(0, 0, 0), (2, 1, 0)]) (.node 2 (.leaf [(2, 1, 0), (2, 1, 0)]) (.leaf [(0, 0, 0), (0, 0, 1)]))
  kt := .node 1 (.leaf regKC) (.leaf taskKC)
  nfI := .leaf 0
  nfO := .leaf 0
  ngI := .leaf 0
  ngO := .leaf 0
  h3ok := .leaf 0
  exM := .leaf 0
  exR := .leaf 0
  exMR := .leaf 0
  escR := .leaf 0

theorem ok : D.Ok := D.ok_of_chain [] (show D.segLast 0 ST.init = true by decide +kernel)

def regCode : CRegM 1 → Nat
  | .rd _ => 0
  | .wr _ => 1

def code : (j : Fin 2) → (CI j).M → Nat
  | 0 => regCode
  | 1 => fun _ => 0

def pos : (j : Fin 2) → (CI j).M → Nat
  | 0 => fun _ => 0
  | 1 => fun t => (t : Fin 3).val

/-- Rule `g`'s statements lie in `[clo g, chi g)`: 0 in `[0, 1)`, 1 in `[1, 3)`, 2 in `[3, 3)`. -/
def clo (g : Nat) : Nat := if g = 0 then 0 else if g = 1 then 1 else 3
def chi (g : Nat) : Nat := if g = 0 then 1 else 3

def G : Glue (Pok z inc) D where
  mi := fun m => m.elim0
  ruleAt := fun i => if h : i < 3 then ⟨i, h⟩ else 0
  rk := fun r => r.val
  base := fun j => if j = 0 then 0 else 2
  kind := fun j => if j = 0 then 0 else 1
  code := code
  fx := fun κ c => κ == 0 && c == 0
  z := z
  ch := fun j => j == 1
  pos := pos
  clo := clo
  chi := chi

theorem reg_ports : ∀ p : CRegM 1, p ∈ [CRegM.rd 0, CRegM.wr 0]
  | .rd ⟨0, _⟩ => by simp
  | .wr ⟨0, _⟩ => by simp

theorem reg_kind (w : V) : KindOK (V := V) (cregAnn 1) regCode (fun c => regKC.get c) (fun c => c == 0) w :=
  kindOK_of_chk reg_ports (by decide) w

/-- Statement order is the `Tasks` annotation's order. -/
theorem task_group {n : Nat} {a b : Fin n × V} (h : a.1.val < b.1.val) : (taskAnn n).Group a b := by
  refine ⟨⟨fun _ => ?_, ?_, ?_, fun _ hs => by simp [taskAnn] at hs⟩, rfl, rfl⟩
  · simp only [taskAnn, decide_eq_false_iff_not]; exact fun h' => Nat.lt_asymm h h'
  · simp only [taskAnn, decide_eq_false_iff_not]; exact fun e => by rw [e] at h; exact Nat.lt_irrefl _ h
  · simp only [taskAnn, decide_eq_false_iff_not]; exact fun e => by rw [e] at h; exact Nat.lt_irrefl _ h

variable (fire : ((j : (Pok z inc).I) → Outs ((Pok z inc).CI j).M V) → Wires (Pok z inc).X.M V → (Pok z inc).PR → Bool)

theorem glueOK : GlueOK (Pok z inc) D (G z inc) fire where
  mi_lt m := m.elim0
  rk_lt r := r.isLt
  rk_at r := by simp [G]
  at_rk i hi := by simp [G, show i < 3 from hi]
  kind j h := match j with
    | 0 => reg_kind z
    | 1 => by simp [G] at h
  kindC j h := match j with
    | 0 => by simp [G] at h
    | 1 => kindOut_chain (fun _ => rfl) (by show (0 : Nat) ∈ [0]; simp) (by show (0 : Nat) ∈ [0]; simp)
  chAnn j h a b hlt := match j with
    | 0 => by simp [G] at h
    | 1 => task_group hlt
  cmono g1 g2 h := by show chi g1 ≤ clo g2; unfold chi clo; split <;> split <;> (try split) <;> omega
  cinM _ _ m := m.elim0
  cinR oc r c hc hch := match r with
    | 0 => by
      simp [trR, trace, Pok, P, rbody] at hc
      rcases hc with rfl | rfl
      · simp [G] at hch
      · simp [G, clo, chi, pos, tk, D]
    | 1 => by
      simp [trR, trace, Pok, P, rbody] at hc
      rcases hc with rfl | rfl
      · simp [G, clo, chi, pos, tk, D]
      · simp [G, clo, chi, pos, tk, D]
    | 2 => by
      simp [trR, trace, Pok, P, rbody] at hc
      rcases hc with rfl | rfl <;> simp [G] at hch
  cincM _ _ m := m.elim0
  cincR oc r j a b hj hab := match r, j with
    | 0, 0 => by simp [G] at hj
    | 1, 0 => by simp [G] at hj
    | 2, 0 => by simp [G] at hj
    | 0, 1 => by
      have e : proj (1 : Fin 2) (trR (Pok z inc) oc 0).1 = [(tk 0, (oc 0).ret Reg.read)] := by
        simp [trR, trace, Pok, P, rbody, proj]
      rw [e] at hab; exact (pair_sub_single hab).elim
    | 1, 1 => by
      have e : proj (1 : Fin 2) (trR (Pok z inc) oc 1).1 = [(tk 1, z), (tk 2, z)] := by
        simp [trR, trace, Pok, P, rbody, proj]
      rw [e] at hab
      obtain ⟨rfl, rfl⟩ := pair_sub_two hab
      simp [G, pos, tk]
    | 2, 1 => by
      have e : proj (1 : Fin 2) (trR (Pok z inc) oc 2).1 = [] := by
        simp [trR, trace, Pok, P, rbody, proj]
      rw [e] at hab; exact (pair_sub_nil hab).elim
  chCode j h p := match j with
    | 0 => by simp [G] at h
    | 1 => rfl
  fpM _ _ m := m.elim0
  fpR oc r := match r with
    | 0 => List.Sublist.refl _
    | 1 => List.Sublist.refl _
    | 2 => List.Sublist.refl _
  zM _ _ m := m.elim0
  zR oc r c hc hf := match r with
    | 0 => by
      simp [trR, trace, Pok, P, rbody] at hc
      rcases hc with rfl | rfl
      · rfl
      · simp [G] at hf
    | 1 => by
      simp [trR, trace, Pok, P, rbody] at hc
      rcases hc with rfl | rfl <;> simp [G] at hf
    | 2 => by
      simp [trR, trace, Pok, P, rbody] at hc
      rcases hc with rfl | rfl
      · rfl
      · simp [G, code, regCode] at hf
  nfI m := m.elim0
  nfO m := m.elim0
  ngI m := m.elim0
  ngO m := m.elim0
  h3ok m := m.elim0
  exM _ _ m := m.elim0
  exR := fun _ _ _ _ _ h => by simp [D, Tab.get] at h
  exMR _ _ m := m.elim0
  esc := fun _ _ _ _ _ h => by simp [D, Tab.get] at h

/-- **The pair fields, with the task statements checked as a chain.** -/
def pairs : PairFacts (Pok z inc) (G z inc).rules fire :=
  pairFacts_of_scan (Pok z inc) ok (G z inc) fire (glueOK z inc fire)

/-! ### Negative: statements against rank order (rule 0 prints `s2`, rule 1 `s1`, `s0`) -/

abbrev Pbad := P z inc 2 1 0 (by decide) (by decide) (by decide)

/-- No chain intervals fit: rule 0's statement 2 would have to precede rule 1's statement 1. -/
theorem no_chain : ¬ ∃ lo hi : Nat → Nat, (∀ g1 g2, g1 < g2 → hi g1 ≤ lo g2) ∧
    (lo 0 ≤ 2 ∧ 2 < hi 0) ∧ (lo 1 ≤ 1 ∧ 1 < hi 1) := by
  rintro ⟨lo, hi, hm, h0, h1⟩
  have := hm 0 1 (by decide)
  omega

/-- And the pair genuinely conflicts: rule 1's calls may not follow rule 0's. -/
theorem not_okT (oc : (j : (Pbad z inc).I) → Outs ((Pbad z inc).CI j).M V) :
    ¬ OkT (Pbad z inc) (trR (Pbad z inc) oc 0).1 (trR (Pbad z inc) oc 1).1 := by
  intro h
  have ha : ((tk 2 : Fin 3), (oc 0).ret Reg.read) ∈ proj (1 : Fin 2) (trR (Pbad z inc) oc 0).1 := by
    simp [trR, trace, Pbad, P, rbody, proj, tk]
  have hb : ((tk 1 : Fin 3), z) ∈ proj (1 : Fin 2) (trR (Pbad z inc) oc 1).1 := by
    simp [trR, trace, Pbad, P, rbody, proj, tk]
  have := (h 1 _ ha _ hb).1 (show (tk 2 : Fin 3) ≠ tk 1 by decide)
  simp [CI, taskAnn, tk] at this

end ScanTasks

#print axioms ScanTasks.ok
#print axioms ScanTasks.glueOK
#print axioms ScanTasks.pairs
#print axioms ScanTasks.no_chain
#print axioms ScanTasks.not_okT
