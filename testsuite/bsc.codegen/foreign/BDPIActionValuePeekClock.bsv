import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekClock ();
   Clock c1 <- exposeCurrentClock;
   Clock c2 = noClock;
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the clock of the register must be static, which the if on the peeked
   // result is not
   Clock c = (peekGet(g) == 0) ? c1 : c2;
   Reg#(Bit#(32)) r <- mkReg(0, clocked_by c);
endmodule
