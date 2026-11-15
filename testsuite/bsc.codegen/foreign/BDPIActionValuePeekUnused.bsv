import GetPut::*;

import "BDPI" function ActionValue#(Bit#(32)) my_count (Bit#(8) step);
import "BDPI" function ActionValue#(Bool) my_flag (Bit#(8) x);
import "BDPI" function Action my_display (Bit#(8) x);

(* synthesize *)
module mkBDPIActionValuePeekUnused ();
   Reg#(Bit#(32)) r <- mkReg(42);

   // peekGet reads the value of g.get without running it.  The action
   // calls my_count and my_flag, whose results the value does not use,
   // and my_display, which has no result: nothing to report.
   Get#(Bit#(32)) g = toGet(actionvalue
                               let n <- my_count(1);
                               let b <- my_flag(1);
                               my_display(3);
                               return r;
                            endactionvalue);

   rule go;
      let p = peekGet(g);
      let x <- g.get;
      $display("peek %0d get %0d", p, x);
      $finish(0);
   endrule
endmodule
