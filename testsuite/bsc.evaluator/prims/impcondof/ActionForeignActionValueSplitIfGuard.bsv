import FIFO::*;

(* synthesize *)
module sysActionForeignActionValueSplitIfGuard();

   FIFO#(File) f1 <- mkFIFO;
   FIFO#(Bool) f2 <- mkFIFO;
   FIFO#(Bool) f3 <- mkFIFO;
   Reg#(Bool) c <- mkReg(True);

   // the condition of rule q is that of the action that its body runs,
   // which depends on the result of $fgetc: the condition is evaluated
   // before the action runs, so the result is used where the call has not
   // run
   Action p =
      action
         splitIf(c,
                 action
                    let x <- $fgetc(f1.first);
                    if (x == 0) f2.enq(True); else f3.enq(False);
                 endaction,
                 noAction);
      endaction;

   rule q (impCondOf(p));
      p;
   endrule

endmodule
