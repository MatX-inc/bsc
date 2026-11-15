import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekTwiceRun ();
   Reg#(Bit#(32)) r <- mkReg(0);
   ActionValue#(Bit#(32)) c = my_count(1);

   // but the two executions of a call in one read are different calls, and
   // their results are used
   Get#(Bit#(32)) g = toGet(actionvalue
                               let a <- c;
                               let b <- c;
                               return a - b;
                            endactionvalue);

   rule go;
      r <= peekGet(g);
   endrule
endmodule
