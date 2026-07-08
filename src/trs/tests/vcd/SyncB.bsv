import Clocks::*;

(* synthesize *)
module sysSyncB ();
   Clock c2 <- mkAbsoluteClock(3, 7);
   Reset r2 <- mkAsyncResetFromCR(2, c2);
   Reg#(UInt#(8)) fast <- mkReg(0);
   Clock cc <- exposeCurrentClock;
   Reset cr <- exposeCurrentReset;
   SyncBitIfc#(Bit#(1)) sb <- mkSyncBit(cc, cr, c2);
   SyncPulseIfc sp <- mkSyncPulse(cc, cr, c2);
   Reg#(UInt#(8)) got <- mkReg(0, clocked_by c2, reset_by r2);

   rule tickf;
      fast <= fast + 1;
      sb.send(pack(fast)[0]);
   endrule

   rule pulse (fast % 4 == 0);
      sp.send();
   endrule

   rule recv (sp.pulse());
      got <= got + 1;
   endrule

   rule done (fast == 15);
      $finish(0);
   endrule
endmodule
