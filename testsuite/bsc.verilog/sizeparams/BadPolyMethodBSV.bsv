package BadPolyMethodBSV;

import FIFO::*;

(* synthesize *)
module mkBadPolyMethodBSV(FIFO#(a)) provisos (Bits#(a, sa));
   FIFO#(a) f <- mkFIFO;
   return f;
endmodule

endpackage
