import Clocks::*;

(* synthesize *)
module sysMClk ();
   Clock c2 <- mkAbsoluteClock(3, 7);
   Reset r2 <- mkAsyncResetFromCR(2, c2);
   Reg#(UInt#(8)) fast <- mkReg(0);
   Reg#(UInt#(8)) slow <- mkReg(0, clocked_by c2, reset_by r2);

   rule tickf;
      fast <= fast + 1;
   endrule

   rule ticks_;
      slow <= slow + 1;
   endrule

   rule done (fast == 15);
      $finish(0);
   endrule
endmodule
