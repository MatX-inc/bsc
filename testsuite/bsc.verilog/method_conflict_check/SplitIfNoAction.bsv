import FIFO::*;

interface SplitIfNoAction;
   method Action a(Bool sel);
   method Action b;
endinterface

(* synthesize *)
(* conflict_free = "" *)
module mkSplitIfNoAction (SplitIfNoAction);
   FIFO#(Bit#(8)) f <- mkFIFO;
   method Action a(Bool sel);
      if (sel) f.enq(1);
      else f.enq(2);
   endmethod
   method Action b;
      f.enq(3);
   endmethod
endmodule
