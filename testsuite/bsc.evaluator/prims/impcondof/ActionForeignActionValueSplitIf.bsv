import FIFO::*;

(* synthesize *)
module sysActionForeignActionValueSplitIf();

   FIFO#(File) f1 <- mkFIFO;
   FIFO#(Bool) f2 <- mkFIFO;
   FIFO#(Bool) f3 <- mkFIFO;
   Reg#(Bool) c <- mkReg(True);

   // splitIf makes the action it chooses with toPrimAction, which runs it:
   // $fgetc is called there, and its result decides the condition, but
   // impCondOf drops the action, so the call is not part of the design
   Action p =
      action
         splitIf(c,
                 action
                    let x <- $fgetc(f1.first);
                    if (x == 0) f2.enq(True); else f3.enq(False);
                 endaction,
                 noAction);
      endaction;

   rule r;
      $display("cond p = %d", impCondOf(p));
   endrule

endmodule
