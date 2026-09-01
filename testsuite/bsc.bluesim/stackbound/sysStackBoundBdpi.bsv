// A design importing a foreign (BDPI) C function.  Imported C is
// compiled outside the stack-usage scheme, so no sound static stack
// bound exists for this model: bk_stack_depth_bound() must return
// the documented "no bound available" value 0.

import "BDPI" function Bit#(32) stackbound_id(Bit#(32) x);

(* synthesize *)
module sysStackBoundBdpi();
   Reg#(Bit#(32)) x <- mkReg(0);

   rule bump;
      x <= stackbound_id(x) + 1;
   endrule

   rule done (x == 100);
      $display("x=%0d", x);
      $finish(0);
   endrule
endmodule
