package SelectOrderTopBA;

import SelectOrderScrambleB::*;
import SelectOrderScrambleA::*;
import SelectOrder::*;

// Batch order B, A, probe: j's generated names are interned before k's.
(* synthesize *)
module mkSelectOrderTopBA(Empty);
   Empty b <- mkSelectOrderScrambleB;
   Empty a <- mkSelectOrderScrambleA;
   Empty p <- mkSelectOrder;
endmodule

endpackage
