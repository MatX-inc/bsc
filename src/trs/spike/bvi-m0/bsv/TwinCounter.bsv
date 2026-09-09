// iverilog-oracle twin of the counter fixture: the same BviCounter.v,
// imported as a real BVI, driven by rules whose per-cycle prints must
// match the harness scenario's pre-edge value series.
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
module sysTwinCounter();
   Counter c <- mkBviCounter;
   Reg#(Bit#(3)) n <- mkReg(0);

   rule show;
      $display("count=%0d", c.read());
   endrule
   rule step;
      c.bump(3);
      n <= n + 1;
      if (n == 3) $finish(0);
   endrule
endmodule
