// R2 negative: a GATED output clock (v1.2 supports ungated output
// clocks; a generated gate is a second sampled level the derived-clock
// network does not model yet).
interface Ifc;
   interface Clock cout;
   method Bit#(8) get();
endinterface

import "BVI" OutClkG =
module mkOutClkG(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   output_clock cout(CLK_OUT, CLK_GATE_OUT);
   method OUT get();
   schedule get CF get;
endmodule

(* synthesize *)
module sysNegOutClock();
   let dut <- mkOutClkG;
   rule r;
      $display("%0d", dut.get());
      $finish(0);
   endrule
endmodule
