// R2 negative: an Action method clocked by no_clock -- there is no
// edge to commit its effects; refused.
interface Ifc;
   method Action poke(Bit#(8) x);
endinterface

import "BVI" NoClk =
module mkNoClk(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method poke(IN) enable(EN) clocked_by(no_clock);
endmodule

(* synthesize *)
module sysNegClocklessAction();
   let dut <- mkNoClk;
   rule r;
      dut.poke(1);
      $finish(0);
   endrule
endmodule
