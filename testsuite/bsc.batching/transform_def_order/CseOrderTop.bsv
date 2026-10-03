package CseOrderTop;

import CseOrderScrambleB::*;
import CseOrderScrambleA::*;
import CseOrder::*;

// Batch order B, A, probe: put_zz and put_zy are interned before put_aa
// and put_ab, the reverse of the order the probe's own source names them
// in.
(* synthesize *)
module mkCseOrderTop(Empty);
   CseOrderScrambleB b <- mkCseOrderScrambleB;
   CseOrderScrambleA a <- mkCseOrderScrambleA;
   CseOrder p <- mkCseOrder;
   Reg#(Bit#(8)) x <- mkReg(0);
   rule drive;
      b.put(x, x);
      a.put(x, x);
      p.put(x, x, x, x);
   endrule
endmodule

endpackage
