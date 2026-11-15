import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekOnce ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // one read used twice is one value, and the difference is 0
   rule go;
      let p = peekGet(g);
      r <= p - p;
   endrule
endmodule
