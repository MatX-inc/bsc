// R2 negative: a (*reg*) input argument -- latched-on-arrival is legal
// BVI but interacts with probe-volatile args; refused in v1.
interface Ifc;
   method Action put(Bit#(8) x);
endinterface

import "BVI" RegArg =
module mkRegArg(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method put((* reg *)IN) enable(EN);
   schedule put C put;
endmodule

(* synthesize *)
module sysNegVPregArg();
   let dut <- mkRegArg;
   rule r;
      dut.put(1);
      $finish(0);
   endrule
endmodule
