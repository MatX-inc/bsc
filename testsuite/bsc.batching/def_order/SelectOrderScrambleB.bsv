package SelectOrderScrambleB;

import Vector::*;

// The probe with k renamed n: the same shape, so the generated names of
// j's select (and of j's hidden read) are the same strings as the
// probe's, interned here when this package is compiled before ScrambleA.
(* synthesize *)
module mkSelectOrderScrambleB(Empty);
   Vector#(4, Reg#(UInt#(8))) va <- replicateM(mkReg(0));
   Reg#(UInt#(2)) n <- mkReg(0);
   Reg#(UInt#(2)) j <- mkReg(1);
   Reg#(UInt#(8)) s <- mkRegU;
   rule step;
      s <= va[n] + va[j];
      n <= n + 1;
      j <= j + 1;
   endrule
endmodule

endpackage
