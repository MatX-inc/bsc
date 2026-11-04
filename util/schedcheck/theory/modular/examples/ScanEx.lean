/- `pairFacts_of_scan` on a concrete parent (registers `a`, `b`, a FIFO `f`; three rules, two
   methods): every glue fact discharged, the scan decided by the kernel. -/
import Scan

open Modular Scan
open Emit (Tab)

namespace ScanEx

variable {V : Type} (z : V) (inc : V → V)

def CI : Fin 3 → Iface
  | 0 => Reg.X
  | 1 => Reg.X
  | 2 => FIFO2.X

/-- `get` (value) and `put` (action, C with itself). -/
def pAnn : Ann (Fin 2) where
  prec _ _ := false
  sbr _ _ := false
  conf p q := p == 1 && q == 1
  share p := p == 0
  hasEn p := p == 1
  alwaysEn _ := false
  alwaysRdy _ := false

abbrev X : Iface := ⟨Fin 2, inferInstance, pAnn⟩

/-- Rules: 0 `incA`, 1 `copy`, 2 `drain`. -/
def rbody : Fin 3 → Prog (Fin 3) (fun j => (CI j).M) V
  | 0 => .call (CM := fun j => (CI j).M) 0 Reg.read z (fun v => .call (CM := fun j => (CI j).M) 0 Reg.write (inc v) (fun _ => .ret z))
  | 1 => .call (CM := fun j => (CI j).M) 0 Reg.read z (fun v => .call (CM := fun j => (CI j).M) 1 Reg.write v (fun _ => .ret z))
  | 2 => .call (CM := fun j => (CI j).M) 2 FifoM.first z (fun v =>
          .call (CM := fun j => (CI j).M) 2 FifoM.deq z (fun _ => .call (CM := fun j => (CI j).M) 0 Reg.write v (fun _ => .ret z)))

def body : Fin 2 → V → Prog (Fin 3) (fun j => (CI j).M) V
  | 0, _ => .call (CM := fun j => (CI j).M) 1 Reg.read z (fun v => .ret v)
  | 1, x => .call (CM := fun j => (CI j).M) 2 FifoM.enq x (fun _ => .ret z)

abbrev P : Parent V where
  I := Fin 3
  decI := inferInstance
  kids := List.finRange 3
  kids_nodup := List.nodup_finRange 3
  kids_all := List.mem_finRange
  CI := CI
  X := X
  PR := Fin 3
  body := body z
  rbody := rbody z inc
  comb := fun _ _ => (fun _ => ⟨fun _ => false, fun _ => z⟩, ⟨fun _ => false, fun _ => z⟩)
  asm := fun _ _ => True
  onehot := fun _ _ => True

/-! The scan's input, as a generator would write it. -/

def regKC : Tab KC := .node 1 (.leaf ⟨[1], [], [1], []⟩) (.leaf ⟨[], [0], [1], [0, 1]⟩)

def fifoKC : Tab KC :=
  .node 4 (.node 2 (.node 1 (.leaf ⟨[0, 7], [0, 3, 4], [0, 7], [0, 3, 4, 7]⟩)
                            (.leaf ⟨[1, 7], [1, 2, 3, 4], [1, 7], [1, 2, 3, 4, 7]⟩))
                   (.node 3 (.leaf ⟨[1, 7], [], [1, 7], []⟩) (.leaf ⟨[0, 1, 7], [], [0, 1, 7], []⟩)))
          (.node 6 (.node 5 (.leaf ⟨[0, 1, 7], [], [0, 1, 7], []⟩) (.leaf ⟨[], [], [], []⟩))
                   (.node 7 (.leaf ⟨[], [], [], []⟩) (.leaf ⟨[], [0, 1, 2, 3, 4], [0, 1, 7], [0, 1, 2, 3, 4, 7]⟩)))

/-- Slots: `a` 0–1, `b` 2–3, `f` 4–11.  Rank positions: 0 `copy`, 1 `incA`, 2 `drain`. -/
def D : Design where
  nM := 2
  nR := 3
  d := 4
  fpM := .node 1 (.leaf [(2, 0, 0)]) (.leaf [(4, 1, 0)])
  fpR := .node 1 (.leaf [(0, 0, 0), (2, 0, 1)]) (.node 2 (.leaf [(0, 0, 0), (0, 0, 1)]) (.leaf [(4, 1, 2), (4, 1, 1), (0, 0, 1)]))
  kt := .node 1 (.leaf regKC) (.leaf fifoKC)
  nfI := .node 1 (.leaf 0) (.leaf 2)
  nfO := .node 1 (.leaf 0) (.leaf 2)
  ngI := .node 1 (.leaf 0) (.leaf 2)
  ngO := .node 1 (.leaf 0) (.leaf 2)
  h3ok := .leaf 0
  exM := .leaf 0
  exR := .leaf 0
  exMR := .leaf 0
  escR := .leaf 0

/-- **The scan passes** (one kernel check, no checkpoint). -/
theorem ok : D.Ok := D.ok_of_chain [] (show D.segLast 0 ST.init = true by decide +kernel)

def regCode : CRegM 1 → Nat
  | .rd _ => 0
  | .wr _ => 1

def fifoCode : FifoM → Nat
  | .enq => 0 | .deq => 1 | .first => 2 | .notEmpty => 3 | .notFull => 4
  | .i_notEmpty => 5 | .i_notFull => 6 | .clear => 7

def code : (j : Fin 3) → (CI j).M → Nat
  | 0 => regCode
  | 1 => regCode
  | 2 => fifoCode

def ruleAt (i : Nat) : Fin 3 := if i = 0 then 1 else if i = 1 then 0 else 2

def rk : Fin 3 → Nat
  | 0 => 1
  | 1 => 0
  | 2 => 2

/-- Flagged (argument-less, every call carries `z`): a register's read, all
    FIFO ports but `enq`. -/
def fx (κ c : Nat) : Bool := if κ = 0 then c == 0 else c != 0

def G : Glue (P z inc) D where
  mi := fun m => m.val
  ruleAt := ruleAt
  rk := rk
  base := fun j => if j = 0 then 0 else if j = 1 then 2 else 4
  kind := fun j => if j = 2 then 1 else 0
  code := code
  fx := fx
  z := z

theorem reg_ports : ∀ p : CRegM 1, p ∈ [CRegM.rd 0, CRegM.wr 0]
  | .rd ⟨0, _⟩ => by simp
  | .wr ⟨0, _⟩ => by simp

theorem fifo_ports : ∀ p : FifoM, p ∈ [FifoM.enq, .deq, .first, .notEmpty, .notFull, .i_notEmpty, .i_notFull, .clear] := by
  intro p; cases p <;> simp

theorem reg_kind (w : V) : KindOK (V := V) (cregAnn 1) regCode (fun c => regKC.get c) (fx 0) w :=
  kindOK_of_chk reg_ports (by decide) w

theorem fifo_kind (w : V) : KindOK (V := V) fifoAnn fifoCode (fun c => fifoKC.get c) (fx 1) w :=
  kindOK_of_chk fifo_ports (by decide) w

theorem tab2 : ∀ m1 m2 : Fin 2, ((Tab.node 1 (.leaf 0) (.leaf 2) : Tab Nat).get m2.val).testBit m1.val = true →
    m1 = 1 ∧ m2 = 1 := by decide

variable (fire : ((j : (P z inc).I) → Outs ((P z inc).CI j).M V) → Wires (P z inc).X.M V → (P z inc).PR → Bool)

theorem glueOK : GlueOK (P z inc) D (G z inc) fire where
  mi_lt := fun m => by show m.val < 2; exact m.isLt
  rk_lt := fun r => (by decide : ∀ r : Fin 3, rk r < 3) r
  rk_at := fun r => (by decide : ∀ r : Fin 3, ruleAt (rk r) = r) r
  at_rk := by show ∀ i, i < 3 → rk (ruleAt i) = i; decide
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
  zM := fun oc w m c hc hf => match m with
    | 0 => by simp [trM, trace, P, body] at hc; subst hc; rfl
    | 1 => by simp [trM, trace, P, body] at hc; subst hc; simp [G, fx, code, fifoCode] at hf
  zR := fun oc r c hc hf => match r with
    | 0 => by
      simp [trR, trace, P, rbody] at hc
      rcases hc with rfl | rfl
      · rfl
      · simp [G, fx, code, regCode] at hf
    | 1 => by
      simp [trR, trace, P, rbody] at hc
      rcases hc with rfl | rfl
      · rfl
      · simp [G, fx, code, regCode] at hf
    | 2 => by
      simp [trR, trace, P, rbody] at hc
      rcases hc with rfl | rfl | rfl
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
  esc := fun _ _ _ _ _ h => by simp [D, Tab.get] at h
  kindC := fun _ h => by cases h
  chAnn := fun _ h => by cases h
  cmono := fun _ _ _ => Nat.le_refl 0
  cinM := fun _ _ _ _ _ h => by cases h
  cinR := fun _ _ _ _ h => by cases h
  cincM := fun _ _ _ _ _ _ h => by cases h
  cincR := fun _ _ _ _ _ h => by cases h
  chCode := fun _ h => by cases h

/-- **The pair fields of the toy, from the scan.** -/
def pairs : PairFacts (P z inc) (G z inc).rules fire := pairFacts_of_scan (P z inc) ok (G z inc) fire (glueOK z inc fire)

end ScanEx

#print axioms ScanEx.ok
#print axioms ScanEx.glueOK
#print axioms ScanEx.pairs
