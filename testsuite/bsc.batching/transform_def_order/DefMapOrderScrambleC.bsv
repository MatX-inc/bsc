package DefMapOrderScrambleC;

import FIFO::*;

// Rule rone's third conditional call is to an instance named fc, so
// compiling this module creates the name COND_RL_rone_fc_enq_3 (the other
// two, on fx and fy, are names the probe does not use).
(* synthesize *)
module mkDefMapOrderScrambleC(Empty);
   FIFO#(Bit#(8)) fx <- mkFIFO;
   FIFO#(Bit#(8)) fy <- mkFIFO;
   FIFO#(Bit#(8)) fc <- mkFIFO;
   Reg#(Bit#(2)) s <- mkReg(0);
   Reg#(Bit#(8)) x <- mkReg(0);
   rule rone;
      case (s)
         0: fx.enq(x);
         1: fy.enq(x);
         default: fc.enq(x);
      endcase
   endrule
endmodule

endpackage
