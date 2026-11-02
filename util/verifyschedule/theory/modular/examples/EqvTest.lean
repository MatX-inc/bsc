/- Comparisons between two values (integration review 3, minor 8): `eqv` with its laws, each
   law stated as a Boolean fact only for the operand pairs and triples that occur, then
   `bv_decide` over the atoms.  Transitivity is needed: without it the fact is false. -/
import Modular
import Std.Tactic.BVDecide

open Modular

namespace EqvTest

/-- Value/value comparisons with their laws, beside the module's constants `E` (bsc's `==` on
    bit vectors has them). -/
structure EqV (V : Type) (E : EqC V) where
  eqv : V → V → Bool
  refl : ∀ x, eqv x x = true
  symm : ∀ x y, eqv x y = eqv y x
  trans : ∀ x y z, eqv x y = true → eqv y z = true → eqv x z = true
  /-- a comparison with one of the module's constants is the same for equal values -/
  cong : ∀ x y k, eqv x y = true → E.eqc x k = E.eqc y k

/-- Transitivity at one triple, as a Boolean fact for `bv_decide`. -/
theorem trans_fact {V : Type} {E : EqC V} (Q : EqV V E) (x y z : V) : (!(Q.eqv x y && Q.eqv y z) || Q.eqv x z) = true := by
  cases h1 : Q.eqv x y
  · simp
  · cases h2 : Q.eqv y z
    · simp
    · simp [Q.trans x y z h1 h2]

/-- The skeleton: a parallel mux selects on `x == y` and on `y == z && x != z`. -/
theorem skel (a b c : Bool) (htr : (!(a && b) || c) = true) : (a && (b && !c)) = false := by
  bv_decide

/-- **The mux is one-hot**, for every value type and every lawful `eqv`. -/
theorem onehot {V : Type} {E : EqC V} (Q : EqV V E) (x y z : V) : (Q.eqv x y && (Q.eqv y z && !Q.eqv x z)) = false :=
  skel _ _ _ (trans_fact Q x y z)

/-- **Without transitivity the fact is false**: a reflexive, symmetric comparison (distance at
    most one) on `Nat` makes both selectors true at 0, 1, 2. -/
theorem no_trans : ¬ ∀ (eqv : Nat → Nat → Bool), (∀ x, eqv x x = true) → (∀ x y, eqv x y = eqv y x) →
    ∀ x y z, (eqv x y && (eqv y z && !eqv x z)) = false := by
  intro h
  have := h (fun x y => decide (x ≤ y + 1) && decide (y ≤ x + 1)) (fun x => by simp) (fun x y => by
    simp only [Bool.and_comm]) 0 1 2
  simp at this

end EqvTest

#print axioms EqvTest.onehot
#print axioms EqvTest.no_trans
