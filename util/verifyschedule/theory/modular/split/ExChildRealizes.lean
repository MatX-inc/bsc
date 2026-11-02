/- ExChildRealizes: mkChild realizes its contract, from its obligation and its children's
   (the registers', in the library).  Imports its own obligation only. -/
import ExChild
open Modular
open scoped List
set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false
namespace Ex.Child
open Ex
section
variable {V : Type} (ops : ToyOps V)
/-- mkChild, built from two registers. -/
abbrev impl : (j : K) → Module V (CI j) := fun _ => Reg
abbrev mod : Module V X := (P ops).toModule impl

/-- The instance tree below mkChild. -/
theorem built : Built X (mod ops) :=
  Built.node (P ops) impl (fun _ => Built.leaf _ reg_realizes (creg_causal 1))
    (fun _ => aliased_of_none _ _ (fun _ _ => rfl) (fun _ _ => rfl)) (obl ops) (acyc ops)
theorem realizes : Realizes (mod ops) := (built ops).realizes
end
end Ex.Child
