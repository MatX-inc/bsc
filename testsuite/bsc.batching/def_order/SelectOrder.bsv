package SelectOrder;

import Vector::*;

// Two dynamic selects from the same vector, one per index register, added
// together: each select is generated as its own always block and neither
// depends on the other, so a tie-break decides which block comes first.
// Elaboration creates k's select before j's whatever the process compiled
// before.
(* synthesize *)
module mkSelectOrder(Empty);
   Vector#(4, Reg#(UInt#(8))) va <- replicateM(mkReg(0));
   Reg#(UInt#(2)) k <- mkReg(0);
   Reg#(UInt#(2)) j <- mkReg(1);
   Reg#(UInt#(8)) s <- mkRegU;
   rule step;
      s <= va[k] + va[j];
      k <= k + 1;
      j <= j + 1;
   endrule
endmodule

endpackage
