import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekRuleCond ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the peeked result of my_count is in the condition of the rule
   rule go (peekGet(g) == 3);
      let x <- g.get;
      r <= x;
   endrule
endmodule
