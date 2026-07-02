// The OVL assertion checkers have pure-BSV fallbacks, so designs using
// them can simulate without the OVL Verilog library (always in Bluesim;
// in Verilog simulation with the selection macro defined).

import OVLAssertions::*;

(* synthesize *)
module sysTbOvl(Empty);
   let defaults = mkOVLDefaults;
   defaults.msg = "n must stay below 5";
   AssertTest_IFC#(Bool) a <- bsv_assert_always(defaults);

   Reg#(Bit#(8)) n <- mkReg(0);
   rule step;
      n <= n + 1;
      a.test(n < 5);
      $display("cycle %0d", n);
      if (n == 8) $finish(0);
   endrule
endmodule
