package SelectOrderScrambleA;

import Vector::*;

// The probe with j renamed m: the same shape, so the generated names of
// k's select (and of k's hidden read) are the same strings as the
// probe's, interned here when this package is compiled after ScrambleB.
(* synthesize *)
module mkSelectOrderScrambleA(Empty);
   Vector#(4, Reg#(UInt#(8))) va <- replicateM(mkReg(0));
   Reg#(UInt#(2)) k <- mkReg(0);
   Reg#(UInt#(2)) m <- mkReg(1);
   Reg#(UInt#(8)) s <- mkRegU;
   rule step;
      s <= va[k] + va[m];
      k <= k + 1;
      m <= m + 1;
   endrule
endmodule

endpackage
