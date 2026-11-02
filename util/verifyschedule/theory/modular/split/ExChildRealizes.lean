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
/-- mkChild, built from two registers (it has no skipped values: `Parent.toU`). -/
abbrev impl : (j : K) → ModuleU V (CI j) := fun _ => Reg.toU
abbrev mod : ModuleU V X := (P ops).toU.toModuleU impl

/-- The instance tree below mkChild. -/
theorem built : Built X (mod ops) :=
  Built.node (P ops).toU impl (fun _ => Built.leaf _ reg_realizes (creg_causal 1))
    (fun _ => aliased_of_none _ _ (fun _ _ => rfl) (fun _ _ => rfl)) (oblU_of_obl _ (obl ops)) (acyc ops).toU
theorem realizes : RealizesU (mod ops) := (built ops).realizes
end
end Ex.Child
