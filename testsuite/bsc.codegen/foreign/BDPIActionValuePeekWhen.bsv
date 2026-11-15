import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekWhen ();
   Reg#(Bit#(32)) r <- mkReg(0);

   // the when on the result of my_flag in the action is a condition on a
   // value that the action makes, which when cannot lift, whether or not
   // the action runs
   Get#(Bit#(32)) g = toGet(actionvalue
                               let b <- my_flag(1);
                               when(b, noAction);
                               return r;
                            endactionvalue);

   rule go;
      r <= peekGet(g) + 1;
   endrule
endmodule
