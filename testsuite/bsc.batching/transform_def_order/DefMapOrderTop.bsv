package DefMapOrderTop;

import DefMapOrderScrambleC::*;
import DefMapOrderScrambleA::*;
import DefMapOrder::*;

// Batch order C, A, probe: COND_RL_rone_fc_enq_3 is interned first and
// COND_RL_rone_fa_enq_1 second, before the probe makes its three, so in
// Ord Id the probe's COND_ names run fc, fa, fb instead of fa, fb, fc.
(* synthesize *)
module mkDefMapOrderTop(Empty);
   Empty c <- mkDefMapOrderScrambleC;
   Empty a <- mkDefMapOrderScrambleA;
   DefMapOrder p <- mkDefMapOrder;
   Reg#(Bit#(8)) x <- mkReg(0);
   rule drive;
      p.put(x);
   endrule
endmodule

endpackage
