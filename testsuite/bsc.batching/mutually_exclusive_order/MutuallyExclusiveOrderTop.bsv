package MutuallyExclusiveOrderTop;

import MutuallyExclusiveOrderScramble::*;
import MutuallyExclusiveOrder::*;

// Batch order: the scrambler, then the probe.
(* synthesize *)
module mkMutuallyExclusiveOrderTop(Empty);
   Empty s <- mkMutuallyExclusiveOrderScramble;
   Empty p <- sysMutuallyExclusiveOrder;
endmodule

endpackage
