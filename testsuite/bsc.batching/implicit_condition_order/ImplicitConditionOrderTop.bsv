package ImplicitConditionOrderTop;

import ImplicitConditionOrderScramble::*;
import ImplicitConditionOrder::*;

// Batch order: the scrambler, then the probe.
(* synthesize *)
module mkImplicitConditionOrderTop(Empty);
   Empty s <- mkImplicitConditionOrderScramble;
   Peek p <- sysImplicitConditionOrder;
endmodule

endpackage
