// R5 battery: $time inside the model.  trs passes the kernel time
// straight through (the Bluesim timebase); the two flows differ only
// at the STARTUP instant (the Verilog main's first edge lands at t=1
// where Bluesim's lands at t=0 -- an engine-timebase fact, not a BVI
// one), so the model guards its display with $time > 5: from the
// second edge on, both flows print identical times (10, 20, 30).
interface Ifc;
   method Action go();
endinterface

import "BVI" Timely =
module mkTimely(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method go() enable(GO);
   schedule go C go;
endmodule

(* synthesize *)
module sysPosTime();
   let dut <- mkTimely;
   Reg#(Bit#(3)) n <- mkReg(0);
   rule r;
      dut.go();
      n <= n + 1;
      if (n == 3) $finish(0);
   endrule
endmodule
