import FIFO::*;

(* synthesize *)
module sysActionForeignActionValueWhenFold();

   FIFO#(File) f1 <- mkFIFO;
   FIFO#(Bool) f2 <- mkFIFO;

   // the result takes the path of a dynamic value: the evaluator folds
   // each of these conditions to True whatever the result is, so when
   // lifts True
   Action w =
      action
         let x <- $fgetc(f1.first);
         when((x * 0) == 0 && (x & 0) == 0 && x == x + 0, f2.enq(True));
      endaction;

   rule r;
      $display("cond w = %d", impCondOf(w));
   endrule

endmodule
