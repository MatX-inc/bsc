import FIFO::*;

(* synthesize *)
module sysActionForeignActionValue();

   FIFO#(File) f1 <- mkFIFO;
   FIFO#(Bool) f2 <- mkFIFO;
   FIFO#(Bool) f3 <- mkFIFO;
   Reg#(Bool) c <- mkReg(True);

   Action a =
      action
         let x <- $fgetc(f1.first);
         f2.enq(x == 0);
      endaction;

   // impCondOf does not run the action, so $fgetc is not called and its
   // result does not exist: a condition that depends on it (an if, a
   // selection) uses a result that does not exist, an error
   Action b =
      action
         let x <- $fgetc(f1.first);
         if (x == 0) f2.enq(True); else f3.enq(False);
      endaction;

   // (also after another action, whose world -keep-method-conds wraps)
   Action m =
      action
         f1.deq;
         let x <- $fgetc(f1.first);
         if (x == 0) f2.enq(True); else f3.enq(False);
      endaction;

   Action s =
      action
         let x <- $fgetc(f1.first);
         FIFO#(Bool) fs[2] = {f2, f3};
         fs[pack(x)[0]].enq(True);
      endaction;

   // a condition that does not depend on the result is fine
   Action d =
      action
         let x <- $fgetc(f1.first);
         if (c) f2.enq(x == 0); else f3.enq(x == 0);
      endaction;

   // and two results are two calls (an error for each)
   Action e =
      action
         let x <- $fgetc(f1.first);
         let y <- $fgetc(f1.first);
         if (x == y) f2.enq(True); else f3.enq(False);
      endaction;

   Action n =
      action
         let x <- $fgetc(f1.first);
         let y <- $fgetc(f1.first);
         if (x != y) f2.enq(True); else f3.enq(False);
      endaction;

   rule r;
      $display("cond a = %d", impCondOf(a));
      $display("cond b = %d", impCondOf(b));
      $display("cond m = %d", impCondOf(m));
      $display("cond s = %d", impCondOf(s));
      $display("cond d = %d", impCondOf(d));
      $display("cond e = %d", impCondOf(e));
      $display("cond n = %d", impCondOf(n));
   endrule

endmodule
