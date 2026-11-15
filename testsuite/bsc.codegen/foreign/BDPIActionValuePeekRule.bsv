import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekRule ();
   Reg#(Bit#(32)) r1 <- mkReg(0);
   Reg#(Bit#(32)) r2 <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the peeked result of my_count reaches the elaborated rule only in an
   // arm of a condition that is never true, which later optimizations may
   // remove; it is used all the same
   rule r;
      r2 <= (r1 == 0 && r1 != 0) ? peekGet(g) : 0;
   endrule
endmodule
