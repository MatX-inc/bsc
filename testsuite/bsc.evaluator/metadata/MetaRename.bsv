import FIFO::*;

// A renamed instance port: the enqueue and dequeue of a FIFO become
// port defs in method notation (q$enq_1, q$EN_enq, q$EN_deq), which
// aRenameIO renames to the Verilog port names (q$D_IN, q$ENQ, q$DEQ).
(* synthesize *)
module sysMetaRename(Empty);
   FIFO#(UInt#(8)) q <- mkFIFO;
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(0);

   rule fill;
      q.enq(a);
      a <= a + 1;
   endrule

   rule drain;
      b <= b + q.first;
      q.deq;
   endrule
endmodule
