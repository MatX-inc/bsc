package ForeignOrderTop;

import ForeignOrderScrambleB::*;
import ForeignOrderScrambleA::*;
import ForeignOrder::*;

(* synthesize *)
module mkForeignOrderTop(Empty);
   Empty b <- mkForeignOrderScrambleB;
   Empty a <- mkForeignOrderScrambleA;
   Empty p <- mkForeignOrder;
endmodule

endpackage
