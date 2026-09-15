// iverilog-oracle twin of the shadow fixture: self-SBR AV with the
// ACCEPTED consumption pattern -- only the schedule-last caller keeps
// its result.  Per-cycle prints must match the harness scenario.
interface Echo;
   method ActionValue#(Bit#(8)) m(Bit#(8) x);
   method Bit#(8) peek();
endinterface

import "BVI" BviEcho =
module mkBviEcho(Echo);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method OUT m(IN) enable(EN);
   method LAST peek();
   schedule m SBR m;
   schedule peek CF (peek, m);
endmodule

(* synthesize *)
module sysTwinShadow();
   Echo e <- mkBviEcho;
   Reg#(Bit#(3)) n <- mkReg(0);

   rule ra;                       // earlier caller: result DISCARDED
      let _ <- e.m(10);
   endrule
   rule rb;                       // schedule-last caller: consumes
      let v <- e.m(20);
      $display("v=%0d last=%0d", v, e.peek());
   endrule
   rule step;
      n <= n + 1;
      if (n == 2) $finish(0);
   endrule
endmodule
