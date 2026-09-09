// R5 battery: a gated input clock -- the raw oscillator + gate LEVEL
// port contract.  The gate toggles at cycle granularity; the model
// counts only gated-on posedges.
import Clocks :: *;

interface Ifc;
   method Bit#(8) cnt();
endinterface

import "BVI" GateCnt =
module mkGateCnt(Ifc);
   default_clock clk(CLK, CLK_GATE);
   default_reset rst(RST_N);
   method CNT cnt();
   schedule cnt CF cnt;
endmodule

(* synthesize *)
module sysPosGate();
   GatedClockIfc g <- mkGatedClockFromCC(True);
   Reset grst <- mkAsyncResetFromCR(2, g.new_clk);
   Ifc dut <- mkGateCnt(clocked_by g.new_clk, reset_by grst);
   Reg#(Bit#(4)) n <- mkReg(0);

   rule drive;
      // gate ON for two cycles, OFF for two, repeating
      g.setGateCond(n[1] == 0);
      n <= n + 1;
      if (n == 11) $finish(0);
   endrule

   Reg#(Bit#(2)) w <- mkReg(0, clocked_by g.new_clk, reset_by grst);
   rule show;
      // warm-up: skip the pre-reset display slot, where the 4-state
      // oracle reads x and two-state trs reads 0 (defined divergence)
      if (w < 2) w <= w + 1;
      else $display("cnt=%0d", dut.cnt());
   endrule
endmodule
