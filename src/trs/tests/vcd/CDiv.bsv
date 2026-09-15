import Clocks::*;

(* synthesize *)
module sysCDiv ();
   ClockDividerIfc div <- mkClockDivider(3);
   Clock slow = div.slowClock;
   Reset sr <- mkAsyncResetFromCR(2, slow);
   GatedClockIfc g <- mkGatedClockFromCC(True);
   Reg#(UInt#(8)) fast <- mkReg(0);
   Reg#(UInt#(8)) cnt <- mkReg(0, clocked_by slow, reset_by sr);

   rule tickf;
      fast <= fast + 1;
      g.setGateCond(pack(fast)[0] == 0);
   endrule

   rule ticks_;
      cnt <= cnt + 1;
   endrule

   rule done (fast == 20);
      $finish(0);
   endrule
endmodule
