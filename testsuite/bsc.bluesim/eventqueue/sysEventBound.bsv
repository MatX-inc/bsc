import Clocks::*;

// A busy multi-clock, multi-reset design used to validate the exposed
// maximum event-queue depth (bk_max_event_queue_depth): the
// accompanying host harness (host_eventqueue.cxx) runs it with the
// event-queue capacity set EXACTLY to the exposed bound, so any
// under-estimate in the codegen formula aborts the simulation.
//
// It exercises every kind of event source the bound accounts for:
// the default clock and reset, a periodic generated clock (ClockGen),
// a logic-driven clock (MakeClock), a divided clock (ClockDivider),
// reset synchronizers in each derived domain (InitialReset,
// SyncResetA) and a logic-controlled reset asserted mid-run
// (MakeResetA).
(* synthesize *)
module sysEventBound();

   // Domain A: a periodic generated clock whose period slides
   // against the default clock's edges
   Clock clkA <- mkAbsoluteClock(3, 7);
   Reset rstA <- mkInitialReset(2, clocked_by clkA);
   Reg#(UInt#(16)) countA <- mkReg(0, clocked_by clkA, reset_by rstA);

   rule incr_a;
      countA <= countA + 1;
   endrule

   // Domain B: a logic-driven clock with the waveform /1\2/2\3
   // shaped from the default domain
   Reg#(UInt#(3)) shape <- mkReg(0);
   MakeClockIfc#(Bool) mc <- mkClock(False, True);
   Clock clkB = mc.new_clk;
   Reset rstB <- mkAsyncResetFromCR(2, clkB);
   Reg#(UInt#(16)) countB <- mkReg(0, clocked_by clkB, reset_by rstB);

   rule shape_b;
      mc.setClockValue(shape == 0 || shape == 3 || shape == 4);
      shape <= shape + 1;
   endrule

   rule incr_b;
      countB <= countB + 1;
   endrule

   // Domain C: a divided clock
   ClockDividerIfc divC <- mkClockDivider(3);
   Clock clkC = divC.slowClock;
   Reset rstC <- mkAsyncResetFromCR(2, clkC);
   Reg#(UInt#(16)) countC <- mkReg(0, clocked_by clkC, reset_by rstC);

   rule incr_c;
      countC <= countC + 1;
   endrule

   // Default domain: a logic-controlled reset asserted mid-run, so
   // deferred reset events flow while all the clocks are busy
   Clock defClk <- exposeCurrentClock;
   MakeResetIfc mr <- mkReset(2, False, defClk);
   Reg#(UInt#(16)) countR <- mkReg(0, reset_by mr.new_rst);

   Reg#(UInt#(16)) cyc <- mkReg(0);

   rule count_cycles;
      cyc <= cyc + 1;
   endrule

   rule incr_r;
      countR <= countR + 1;
   endrule

   rule mid_reset (cyc == 20 || cyc == 21);
      mr.assertReset();
   endrule

   rule done (cyc == 100);
      $display("cycles = %0d", cyc);
      $display("countR = %0d", countR);
      $finish(0);
   endrule

endmodule
