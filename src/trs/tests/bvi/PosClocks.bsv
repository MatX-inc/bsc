// R5 battery: two input clocks with COINCIDENT posedges + a one-flop
// crossing register -- the NBA-batching golden.  Both clocks are the
// same waveform (period 10, first edge 5) via mkAbsoluteClock, so every
// posedge is coincident: dreg must capture the OLD sreg each cycle.
import Clocks :: *;

interface Ifc;
   method Action put(Bit#(8) x);
   method Bit#(8) get();
endinterface

import "BVI" CrossReg =
module mkCrossReg#(Clock dclk)(Ifc);
   default_clock sclk(SCLK);
   default_reset rst(RST_N);
   input_clock dclk(DCLK) = dclk;
   method put(IN) enable(EN);
   method OUT get() clocked_by(dclk);
   schedule put C put;
   schedule get CF get;
   schedule get CF put;
endmodule

(* synthesize *)
module sysPosClocks();
   Clock dclk <- mkAbsoluteClock(5, 10);
   Reset drst <- mkAsyncResetFromCR(2, dclk);
   let dut <- mkCrossReg(dclk);
   Reg#(Bit#(4)) n <- mkReg(0);
   Reg#(Bit#(4)) m <- mkReg(0, clocked_by dclk, reset_by drst);

   rule feed;
      dut.put({4'b0, n});
      n <= n + 1;
   endrule

   rule watch (m < 8);
      $display("out=%0d", dut.get());
      m <= m + 1;
      if (m == 7) $finish(0);
   endrule
endmodule
