import FIFO::*;

(* synthesize *)
module sysActionForeignActionValueSplitIfRun();

   FIFO#(File) f1 <- mkFIFO;
   FIFO#(Bool) f2 <- mkFIFO;
   FIFO#(Bool) f3 <- mkFIFO;
   Reg#(Bool) c <- mkReg(True);

   // splitIf makes the action it chooses with toPrimAction, which makes
   // the call once for every execution: rule q runs the action, but rule
   // r reads its condition with impCondOf, which does not run it, so the
   // result is used where the call has not run
   Action p =
      action
         splitIf(c,
                 action
                    let x <- $fgetc(f1.first);
                    if (x == 0) f2.enq(True); else f3.enq(False);
                 endaction,
                 noAction);
      endaction;

   rule q;
      p;
   endrule

   rule r;
      $display("cond p = %d", impCondOf(p));
   endrule

endmodule
