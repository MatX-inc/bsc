package InlinedRegisterOrderTop;

import InlinedRegisterOrderScramble::*;
import InlinedRegisterOrder::*;

// Batch order: the scrambler, then the probe.  Both take their clocks and
// resets as arguments; this module passes its own through.
(* synthesize *)
module mkInlinedRegisterOrderTop #(Clock clkZ, Reset rstZ,
                                   Clock clkM, Reset rstM,
                                   Reset rstA2) (Empty);
   Empty  s <- mkInlinedRegisterOrderScramble(clkM, rstM, clkZ, rstZ, rstA2);
   Counts p <- mkInlinedRegisterOrder(clkZ, rstZ, clkM, rstM, rstA2);
endmodule

endpackage
