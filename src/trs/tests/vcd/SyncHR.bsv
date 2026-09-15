import Clocks::*;

(* synthesize *)
module sysSyncHR ();
   Clock c2 <- mkAbsoluteClock(3, 7);
   Reset r2 <- mkAsyncResetFromCR(2, c2);
   Clock cc <- exposeCurrentClock;
   Reset cr <- exposeCurrentReset;
   Reg#(UInt#(8)) fast <- mkReg(0);
   Reg#(Bit#(8)) sr <- mkSyncReg(0, cc, cr, c2);

   rule tickf;
      fast <= fast + 1;
   endrule

   rule sendr (fast % 5 == 1);
      sr <= pack(fast);
   endrule

   rule done (fast == 18);
      $finish(0);
   endrule
endmodule
