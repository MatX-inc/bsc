import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekString ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the message must be a static String, which the if on the peeked
   // result is not
   String s = (peekGet(g) == 0) ? "a" : "b";
   messageM(s);

   rule go;
      r <= 1;
   endrule
endmodule
