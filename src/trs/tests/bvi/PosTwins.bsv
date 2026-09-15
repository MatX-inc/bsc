// R5 battery: two instances with the SAME parameter valuation (one
// verilate class, shared .so, independent VerilatedContexts) plus a
// third with a DIFFERENT valuation (its own class).
interface Counter;
   method Action bump(Bit#(8) amt);
   method Bit#(8) read();
endinterface

import "BVI" BviCounter =
module mkBviCounter(Counter);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method bump(bump_amt) enable(EN_bump) ready(RDY_bump);
   method count read();
   schedule read CF read;
   schedule read SB bump;
   schedule bump C bump;
endmodule

(* synthesize *)
module sysPosTwins();
   Counter a <- mkBviCounter;
   Counter b <- mkBviCounter;
   Counter c <- mkBviCounter;
   Reg#(Bit#(3)) n <- mkReg(0);

   rule step;
      a.bump(1);
      b.bump(2);
      if (n[0] == 1) c.bump(5);
      $display("a=%0d b=%0d c=%0d", a.read(), b.read(), c.read());
      n <= n + 1;
      if (n == 5) $finish(0);
   endrule
endmodule
