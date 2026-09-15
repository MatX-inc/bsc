// R2 positive: the M0 counter imported as a real BVI, exported to .bir
// (the runtime lands at R4; this gates export + contract emission).
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
module sysPosCounter();
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
