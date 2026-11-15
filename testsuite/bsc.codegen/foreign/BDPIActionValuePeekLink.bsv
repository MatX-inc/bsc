import GetPut::*;

// an imported function whose name is not that of the C function
import "BDPI" c_counter = function ActionValue#(Bit#(32)) next_count (Bit#(8) step);

(* synthesize *)
module mkBDPIActionValuePeekLink ();
   Reg#(Bit#(32)) r <- mkReg(0);

   // the value does not use the result of the call, so there is nothing
   // to report
   Get#(Bit#(32)) g = toGet(actionvalue
                               let n <- next_count(1);
                               return r;
                            endactionvalue);

   rule go;
      r <= peekGet(g);
   endrule
endmodule
