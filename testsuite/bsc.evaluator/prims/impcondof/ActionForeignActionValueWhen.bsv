import FIFO::*;

(* synthesize *)
module sysActionForeignActionValueWhen();

   FIFO#(File) f1 <- mkFIFO;
   FIFO#(Bool) f2 <- mkFIFO;

   Action w =
      action
         let x <- $fgetc(f1.first);
         when(x != 0, f2.enq(True));
      endaction;

   rule r;
      $display("cond w = %d", impCondOf(w));
   endrule

endmodule
