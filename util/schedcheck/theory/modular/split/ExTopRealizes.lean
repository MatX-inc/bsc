/- ExTopRealizes: mkTop realizes its contract.  Imports its own obligation and the
   Realizes file of each child module (here mkChild's): the files form a DAG, one per module. -/
import ExTop
import ExChildRealizes
open Modular
open scoped List
set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false
namespace Ex.Top
open Ex
section
variable {V : Type} (ops : ToyOps V)

abbrev impl : (j : K) → ModuleU V (CI j)
  | .c => Child.mod ops
  | .t => Reg.toU

abbrev mod : ModuleU V X := (P ops).toU.toModuleU (impl ops)

/-- The instance tree below mkTop: the children's trees, by name. -/
theorem built : Built X (mod ops) :=
  Built.node (P ops).toU (impl ops) (fun j => match j with
    | .c => Child.built ops
    | .t => Built.leaf _ reg_realizes (creg_causal 1))
    (fun j => aliased_of_none _ _ (fun p q => by cases j <;> rfl) (fun a p => by cases j <;> rfl))
    (oblU_of_obl _ (obl ops)) (acyc ops).toU

/-- **The top module realizes its contract.** -/
theorem realizes : RealizesU (mod ops) := (built ops).realizes
end
end Ex.Top

#print axioms Ex.Top.realizes
