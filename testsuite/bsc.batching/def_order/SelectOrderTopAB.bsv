package SelectOrderTopAB;

import SelectOrderScrambleA::*;
import SelectOrderScrambleB::*;
import SelectOrder::*;

// Batch order A, B, probe: k's generated names are interned before j's.
(* synthesize *)
module mkSelectOrderTopAB(Empty);
   Empty a <- mkSelectOrderScrambleA;
   Empty b <- mkSelectOrderScrambleB;
   Empty p <- mkSelectOrder;
endmodule

endpackage
