import Clocks :: *;

// Three clock domains besides the default one, created in this order:
// clkA, then clkB divided from clkA, then clkC.  Each has a register and
// a rule, and values cross between them through synchronizing registers.

(* synthesize *)
module sysSimClockDomains();

   Clock clkA <- mkAbsoluteClock(15, 10);
   Reset rstA <- mkAsyncResetFromCR(3, clkA);

   ClockDividerIfc divClock <- mkClockDividerOffset(8, 4, clocked_by clkA, reset_by rstA);
   Clock clkB = divClock.slowClock;
   Reset rstB <- mkAsyncReset(3, rstA, clkB);

   Clock clkC <- mkAbsoluteClock(7, 6);
   Reset rstC <- mkAsyncResetFromCR(2, clkC);

   Reg#(UInt#(16)) creg <- mkReg(0);
   Reg#(UInt#(16)) areg <- mkReg(0, clocked_by clkA, reset_by rstA);
   Reg#(UInt#(16)) breg <- mkReg(0, clocked_by clkB, reset_by rstB);
   Reg#(UInt#(16)) xreg <- mkReg(0, clocked_by clkC, reset_by rstC);

   Reg#(UInt#(16)) aToB <- mkSyncRegToSlow(0, divClock, rstB);
   Reg#(UInt#(16)) bToA <- mkSyncRegToFast(0, divClock, rstB);
   Reg#(UInt#(16)) ccToC <- mkSyncRegFromCC(0, clkC);
   Reg#(UInt#(16)) cToCC <- mkSyncRegToCC(0, clkC, rstC);

   rule tick;
      creg <= creg + 1;
      ccToC <= creg;
      $display("cc %0d %0d", creg, cToCC);
      if (creg >= 200) $finish(0);
   endrule

   rule aplus;
      areg <= areg + 1;
      aToB <= areg;
      $display("a %0d %0d", areg, bToA);
   endrule

   rule bplus;
      breg <= breg + 1;
      bToA <= breg;
      $display("b %0d %0d", breg, aToB);
   endrule

   rule cplus;
      xreg <= xreg + 1;
      cToCC <= xreg;
      $display("c %0d %0d", xreg, ccToC);
   endrule

endmodule
