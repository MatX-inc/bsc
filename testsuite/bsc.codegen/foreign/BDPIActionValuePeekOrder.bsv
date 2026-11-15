import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekOrder ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Get#(Bit#(32)) g2 = toGet(my_count(1));
   Get#(Bool) g1 = toGet(my_flag(2));

   // two results used by one rule: the errors are in the order of the
   // calls in the source, not in the order the rule reads them
   rule go;
      r <= peekGet(g1) ? peekGet(g2) : 0;
   endrule
endmodule
