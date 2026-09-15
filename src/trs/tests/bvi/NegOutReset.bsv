// R2 negative: MORE THAN ONE output reset (v1.2 supports exactly one
// per import -- the derived-reset network keys one node per generator
// instance).
interface Ifc;
   interface Reset rout;
   interface Reset rout2;
   method Bit#(8) get();
endinterface

import "BVI" OutRst2 =
module mkOutRst2(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   output_reset rout(RST_OUT);
   output_reset rout2(RST_OUT2);
   method OUT get();
   schedule get CF get;
endmodule

(* synthesize *)
module sysNegOutReset();
   let dut <- mkOutRst2;
   rule r;
      $display("%0d", dut.get());
      $finish(0);
   endrule
endmodule
