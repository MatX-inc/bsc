// R2 negative: a module Port argument driven by a dynamic (register)
// value -- ports are driven once at construction, so only constants
// are accepted.
interface Ifc;
   method Bit#(8) get();
endinterface

import "BVI" DynPort =
module mkDynPort#(Bit#(8) v)(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   port PV = v;
   method OUT get();
   schedule get CF get;
endmodule

(* synthesize *)
module sysNegDynPort();
   Reg#(Bit#(8)) r <- mkReg(7);
   let dut <- mkDynPort(r);
   rule show;
      $display("%0d", dut.get());
      $finish(0);
   endrule
endmodule
