package ImplicitConditionOrderScramble;

import FIFO::*;

// The probe's five instance names declared the other way round: sideQ,
// outQ, inQ, p2, p1.  A batch parses this package before the probe, so
// these names are interned in this order and p2 sorts before p1.  On the
// compiler before the fix that is what moved: the two RDY_pop conjuncts
// are the same selector on two state variables and sort by Ord Id on p1
// and p2, while the FIFO conjuncts were heap references, which sort by
// allocation number and after any other term.
(* synthesize *)
module mkImplicitConditionOrderScramble(Empty);
   FIFO#(Bit#(8)) sideQ <- mkFIFO;
   FIFO#(Bit#(8)) outQ <- mkFIFO;
   FIFO#(Bit#(8)) inQ <- mkFIFO;
   FIFO#(Bit#(8)) p2 <- mkFIFO;
   FIFO#(Bit#(8)) p1 <- mkFIFO;
endmodule

endpackage
