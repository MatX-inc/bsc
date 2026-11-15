import Vector::*;

import "BDPI" function ActionValue#(Bit#(32)) my_count (Bit#(8) step);

(* synthesize *)
module mkBDPIActionValueLetBound ();
   Reg#(Bool) first <- mkReg(True);

   // each execution of a let-bound ActionValue is its own call
   rule r1 (first);
      ActionValue#(Bit#(32)) next = my_count(1);
      Vector#(3, Bit#(32)) v <- replicateM(next);
      $display("%0d %0d %0d", v[0], v[1], v[2]);
      first <= False;
   endrule

   // and so is each execution of a function of no arguments that runs it
   function ActionValue#(Bit#(32)) count_tens();
      actionvalue
         Bit#(32) i <- my_count(1);
         return (i * 10);
      endactionvalue
   endfunction

   rule r2 (!first);
      let a <- count_tens();
      let b <- count_tens();
      $display("%0d %0d", a, b);
      $finish(0);
   endrule
endmodule
