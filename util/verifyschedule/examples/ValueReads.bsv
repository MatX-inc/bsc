// Value methods of an imported child that may not be read in one order.
//  * TwoMod has v1 SB v2: a read of v1 after a read of v2 in the cycle is
//    out of order and sees an arbitrary value, but only when the earlier
//    read mattered, that is when the value it read reached what its rule did
//  * b runs first (a writes q, which b reads) and writes x from v2 only when
//    mode < 3; a writes y from v1 only when mode > 9.  So whenever a's read
//    matters, b's did not, which bsc's solver confirms of the two
//    conditions, and a sees what the hardware does
//
// Compile: bsc -u -verilog -elab ValueReads.bsv
interface Two;
   method Bit#(8) v1;
   method Bit#(8) v2;
endinterface

import "BVI" TwoMod =
module mkTwo(Two);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method V1 v1();
   method V2 v2();
   schedule v1 CF v1;
   schedule v2 CF v2;
   schedule v1 SB v2;
endmodule

(* synthesize *)
module mkValueReads(Empty);
   Two           t    <- mkTwo;
   Reg#(Bit#(4)) mode <- mkReg(0);
   Reg#(Bit#(8)) q    <- mkReg(0);
   Reg#(Bit#(8)) x    <- mkReg(0);
   Reg#(Bit#(8)) y    <- mkReg(0);

   rule b;
      y <= q;
      if (mode < 3) x <= t.v2;
   endrule

   rule a;
      q <= q + 1;
      if (mode > 9) y <= t.v1;
   endrule
endmodule
