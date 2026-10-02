package ArbOrderTop;

import ArbOrderScrambleB::*;
import ArbOrderScrambleA::*;
import ArbOrder::*;

// Batch order B, A, probe: the index defs of read2..read6 are interned
// before read1's.
(* synthesize *)
module mkArbOrderTop(Empty);
   Empty b <- mkArbOrderScrambleB;
   Empty a <- mkArbOrderScrambleA;
   Empty p <- mkArbOrder;
endmodule

endpackage
