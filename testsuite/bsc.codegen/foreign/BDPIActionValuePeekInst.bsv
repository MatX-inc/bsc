import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekInst ();
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the peeked result of my_count is an argument of an instantiation
   Reg#(Bit#(32)) r <- mkReg(peekGet(g));

   rule go;
      let x <- g.get;
      r <= x;
   endrule
endmodule
