package EnableOrderTop;

import EnableOrderScrambleB::*;
import EnableOrderScrambleA::*;
import EnableOrder::*;

// Batch order B, A, probe: w2's names are interned before w1's.
(* synthesize *)
module mkEnableOrderTop(Empty);
   EnableOrderScrambleB b <- mkEnableOrderScrambleB;
   EnableOrderScrambleA a <- mkEnableOrderScrambleA;
   EnableOrder p <- mkEnableOrder;
endmodule

endpackage
