import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekNoFold ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the evaluator does not fold p ^ p (nor does it for a register), so
   // the result is used
   rule go;
      let p = peekGet(g);
      r <= (p ^ p) + 1;
   endrule
endmodule
