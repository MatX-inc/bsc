// R2 negative: a declared cross-method path whose influencer is CF
// (unordered) with the reader -- the netlist reads the settled fixed
// point while replay order is arbitrary; refused.
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
   schedule put CF get;
   schedule put C put;
   schedule get CF get;
endmodule

(* synthesize *)
module sysNegCFPath();
   let dut <- mkPathMod;
   rule r1;
      dut.put(1);
   endrule
   rule r2;
      $display("%0d", dut.get());
      $finish(0);
   endrule
endmodule
