import FIFO::*;

(* synthesize *)
module sysActionForeignActionValueWhenSelf();

   FIFO#(File) f1 <- mkFIFO;
   FIFO#(Bool) f2 <- mkFIFO;

   // a result is equal to itself, whatever it is: the when lifts True
   Action w =
      action
         let x <- $fgetc(f1.first);
         when(x == x, f2.enq(True));
      endaction;

   // and so is the same computation on it, made twice
   Action v =
      action
         let x <- $fgetc(f1.first);
         when(x + 1 == x + 1 && truncate(x) == Int#(8)'(truncate(x)),
              f2.enq(True));
      endaction;

   rule r;
      $display("cond w = %d", impCondOf(w));
      $display("cond v = %d", impCondOf(v));
   endrule

endmodule
