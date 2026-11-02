/- ExLib: helpers every generated file uses (would live in the generic library). -/
import Modular
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

/-- The acyclicity certificate's read facts, unfolded. -/
macro "dep" : tactic => `(tactic| (simp only [List.mem_cons, List.mem_singleton, List.not_mem_nil, or_false,
  forall_eq_or_imp, forall_eq, implies_true, false_implies, AgreeO, AgreeW, true_and, and_true] at *))

/-- A register has no combinational paths. -/
theorem reg_nopath (p q : CRegM 1) : (cregAnn 1).path p q = false := by
  cases p <;> cases q <;> rename_i a b <;> rw [fin1 a, fin1 b] <;> rfl

/-- The design's operators, abstract: the dummy argument of argument-less
    calls and the one datapath operator the toy uses. -/
structure ToyOps (V : Type) where
  z : V
  inc : V → V

/-- The register's write wires: enable and data; reads take the dummy. -/
def regW {V : Type} (z : V) (en : Bool) (d : V) : Wires (CRegM 1) V :=
  ⟨fun m => match m with | .wr _ => en | .rd _ => false, fun m => match m with | .wr _ => d | .rd _ => z⟩

end Ex
