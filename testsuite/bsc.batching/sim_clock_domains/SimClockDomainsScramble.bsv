package SimClockDomainsScramble;

import Clocks :: *;

// The probe's clocks and crossing registers, instantiated in the opposite
// order under the same names: compiled before the probe in a batch, this
// module creates the instance names the probe's domains are keyed by in
// the other order.
(* synthesize *)
module mkSimClockDomainsScramble();

   Clock clkC <- mkAbsoluteClock(7, 6);
   Reset rstC <- mkAsyncResetFromCR(2, clkC);

   Clock clkA <- mkAbsoluteClock(15, 10);
   Reset rstA <- mkAsyncResetFromCR(3, clkA);

   ClockDividerIfc divClock <- mkClockDividerOffset(8, 4, clocked_by clkA, reset_by rstA);
   Clock clkB = divClock.slowClock;
   Reset rstB <- mkAsyncReset(3, rstA, clkB);

   Reg#(UInt#(16)) cToCC <- mkSyncRegToCC(0, clkC, rstC);
   Reg#(UInt#(16)) ccToC <- mkSyncRegFromCC(0, clkC);
   Reg#(UInt#(16)) bToA <- mkSyncRegToFast(0, divClock, rstB);
   Reg#(UInt#(16)) aToB <- mkSyncRegToSlow(0, divClock, rstB);

   rule tick;
      ccToC <= cToCC;
   endrule

   rule aplus;
      aToB <= bToA;
   endrule

endmodule

endpackage
