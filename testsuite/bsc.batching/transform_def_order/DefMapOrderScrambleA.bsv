package DefMapOrderScrambleA;

import FIFO::*;

// Rule rone's first conditional call is to an instance named fa, so
// compiling this module creates the name COND_RL_rone_fa_enq_1.
(* synthesize *)
module mkDefMapOrderScrambleA(Empty);
   FIFO#(Bit#(8)) fa <- mkFIFO;
   FIFO#(Bit#(8)) fy <- mkFIFO;
   FIFO#(Bit#(8)) fz <- mkFIFO;
   Reg#(Bit#(2)) s <- mkReg(0);
   Reg#(Bit#(8)) x <- mkReg(0);
   rule rone;
      case (s)
         0: fa.enq(x);
         1: fy.enq(x);
         default: fz.enq(x);
      endcase
   endrule
endmodule

endpackage
