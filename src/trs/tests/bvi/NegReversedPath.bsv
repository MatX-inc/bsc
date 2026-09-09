// R2 negative: a declared path whose reader is scheduled BEFORE the
// influencer (get SB put, path put->get): the reader would see
// pre-drive values where the netlist reads the settled fixed point.
interface Ifc;
   method Action put(Bit#(8) x);
   method Bit#(8) get();
endinterface

import "BVI" PathMod =
module mkPathMod(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method put(IN) enable(EN_put);
   method OUT get();
   path(IN, OUT);
   schedule get SB put;
   schedule put C put;
   schedule get CF get;
endmodule

(* synthesize *)
module sysNegReversedPath();
   let dut <- mkPathMod;
   rule r1;
      dut.put(1);
   endrule
   rule r2;
      $display("%0d", dut.get());
      $finish(0);
   endrule
endmodule
