// R5 battery: reset behavior -- startup reset value visible before any
// write, then a MID-RUN reset assertion (mkReset) restoring it.
import Clocks :: *;

interface Ifc;
   method Action put(Bit#(8) x);
   method Bit#(8) get();
endinterface

import "BVI" RstReg =
module mkRstReg(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method put(IN) enable(EN);
   method OUT get();
   schedule get SB put;
   schedule get CF get;
   schedule put C put;
endmodule

(* synthesize *)
module sysPosRst();
   Clock clk <- exposeCurrentClock;
   MakeResetIfc mr <- mkReset(2, True, clk);
   Ifc dut <- mkRstReg(reset_by mr.new_rst);
   Reg#(Bit#(4)) n <- mkReg(0);

   rule show;
      $display("get=%h", dut.get());
   endrule

   rule step;
      n <= n + 1;
      if (n < 4) dut.put({4'h5, n});
      if (n == 5) mr.assertReset();   // mid-run reset pulse
      if (n == 11) $finish(0);
   endrule
endmodule
