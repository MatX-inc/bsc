import FIFO::*;

interface NoConflict;
   method Action a;
   method Action b;
endinterface

(* synthesize *)
module mkNoConflict (NoConflict);
   FIFO#(Bit#(8)) f <- mkFIFO;
   FIFO#(Bit#(8)) g <- mkFIFO;
   method Action a; f.enq(1); endmethod
   method Action b; g.enq(2); endmethod
endmodule
