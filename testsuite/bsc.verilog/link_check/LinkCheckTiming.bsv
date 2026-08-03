// Designs for the link-time needs-timing facts.
//
// sysLCTiming instantiates a delay-based clock generator (ClockGen.v via
// mkAbsoluteClock), which only simulates under Verilator's --timing:
// bsc records the fact in the generated .v header and the verilator
// builder refuses the link (and the -check-only check) by default.
//
// sysLCRstSync uses only reset synchronizers (SyncReset.v, SyncResetA.v,
// MakeReset*.v): their initial blocks are confined to translate_off
// regions and their synthesizable bodies are plain flops, which
// two-state --no-timing verilator simulates exactly, so no fact is
// recorded and nothing is refused.
//
// sysLCClockDiv uses mkClockDivider (ClockDiv.v, #0 in its edge
// generator): recorded and refused.  sysLCDisClk clocks a register by
// primMakeDisabledClock (MakeClock.v, delay-free): clean.
//
// sysLCInitRst uses mkInitialReset (InitialReset.v), whose whole body
// is translate_off (sim-only): recorded and refused.

import Clocks::*;

(* synthesize *)
module sysLCTiming();
   Clock c <- mkAbsoluteClock(5, 10);
   Reg#(UInt#(8)) r <- mkReg(0);
   rule tick;
      r <= r + 1;
   endrule
endmodule

(* synthesize *)
module sysLCRstSync();
   Clock clk <- exposeCurrentClock;
   Reset rst <- exposeCurrentReset;
   Reset rSync  <- mkSyncReset(2, rst, clk);
   Reset rAsync <- mkAsyncReset(2, rst, clk);
   MakeResetIfc mr <- mkReset(2, True, clk);
   Reg#(UInt#(8)) c1 <- mkReg(0, reset_by rSync);
   Reg#(UInt#(8)) c2 <- mkReg(0, reset_by rAsync);
   Reg#(UInt#(8)) c3 <- mkReg(0, reset_by mr.new_rst);
   rule t1; c1 <= c1 + 1; endrule
   rule t2; c2 <= c2 + 1; endrule
   rule t3; c3 <= c3 + 1; endrule
endmodule

(* synthesize *)
module sysLCClockDiv();
   ClockDividerIfc div <- mkClockDivider(2);
   Reg#(UInt#(8)) r <- mkRegU(clocked_by div.slowClock);
   rule tick;
      r <= r + 1;
   endrule
endmodule

// primMakeDisabledClock is MakeClock-based (delay-free): no fact, no lock.
(* synthesize *)
module sysLCDisClk();
   Reg#(UInt#(8)) r <- mkRegU(clocked_by primMakeDisabledClock);
   rule tick;
      r <= r + 1;
   endrule
endmodule

(* synthesize *)
module sysLCInitRst();
   Reset r0 <- mkInitialReset(2);
   Reg#(UInt#(8)) c1 <- mkReg(0, reset_by r0);
   rule t1; c1 <= c1 + 1; endrule
endmodule
