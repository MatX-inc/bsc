// R5 battery: constant Port argument + (*inhigh*) always-enabled
// Action + clockless value method, in one import.
interface Ifc;
   (* always_enabled *)
   method Action tick(Bit#(8) x);
   method Bit#(8) plus1(Bit#(8) v);
   method Bit#(16) total();
endinterface

import "BVI" Mix =
module mkMix#(Bit#(8) base)(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   port BASE = base;
   method tick(TICK_IN) enable((*inhigh*) IGNORE);
   method PLUS1 plus1(PIN) clocked_by(no_clock);
   method TOT total();
   schedule total SB tick;
   schedule tick C tick;
   schedule total CF total;
   schedule plus1 CF (plus1, tick, total);
endmodule

(* synthesize *)
module sysPosMix();
   let dut <- mkMix(8'd100);
   Reg#(Bit#(4)) n <- mkReg(0);

   rule step;
      dut.tick({4'b0, n});
      $display("tot=%0d p1=%0d", dut.total(), dut.plus1({4'b0, n}));
      n <= n + 1;
      if (n == 5) $finish(0);
   endrule
endmodule
