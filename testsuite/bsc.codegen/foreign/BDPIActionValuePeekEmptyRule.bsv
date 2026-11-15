import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekEmptyRule ();
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the condition of a rule is used even when the rule has no action
   rule never (peekGet(g) == 0);
   endrule
endmodule
