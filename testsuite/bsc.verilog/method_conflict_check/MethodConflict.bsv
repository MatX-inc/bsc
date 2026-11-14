import FIFO::*;

interface MethodConflict;
   method Action a;
   method Action b;
endinterface

(* synthesize *)
module mkMethodConflict (MethodConflict);
   FIFO#(Bit#(8)) f <- mkFIFO;
   method Action a; f.enq(1); endmethod
   method Action b; f.enq(2); endmethod
endmodule
