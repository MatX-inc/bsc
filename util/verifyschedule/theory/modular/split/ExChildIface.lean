/- ExChildIface: mkChild's interface (methods, exported annotations). What a parent imports. -/
import Modular
open Modular
open scoped List
set_option autoImplicit false
set_option linter.unusedVariables false
set_option linter.unusedSimpArgs false
namespace Ex.Child
inductive M where
  | getA | getB | setA
  deriving DecidableEq

/-- The exported annotations, by method name (bsc's schedule output). -/
def ann : Ann M where
  prec p q := match p, q with
    | .getA, .setA => true   -- getA SB setA
    | .getB, .setA => true   -- getB SBR setA
    | _, _ => false
  sbr p q := match p, q with
    | .getB, .setA | .setA, .getB => true
    | _, _ => false
  conf p q := match p, q with
    | .setA, .setA => true   -- setA C setA
    | _, _ => false
  share m := match m with | .setA => false | _ => true
  hasEn m := match m with | .setA => true | _ => false
  alwaysEn _ := false
  alwaysRdy _ := true

abbrev X : Iface := ⟨M, inferInstance, ann⟩
end Ex.Child
