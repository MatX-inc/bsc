// Two methods that conflict through a shared resource: both call the same
// FIFO's enq, and enq conflicts with itself, so the schedule marks a C b.
//
// Note two methods writing the same register would NOT conflict -- bsc
// sequences those SB and warns about shadowing instead.
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
