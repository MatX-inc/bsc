import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekModIf ();
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the module instantiated depends on the peeked result of my_count,
   // which elaboration must know
   Reg#(Bit#(32)) r <- (peekGet(g) == 0) ? mkReg(0) : mkRegU;

   rule go;
      r <= r + 1;
   endrule
endmodule
