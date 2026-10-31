// A Bit#(1) element of a vector compared with zero, as a condition.
// "bv[idx] == 0" is one negation of the selected bit, which the
// evaluator keeps after the selection rather than pushing it into
// every element: the generated Verilog selects the un-negated bit
// and swaps the arms of the mux.

import Vector::*;

(* synthesize *)
module sysNotSel(Empty);
   Vector#(16, Reg#(Bit#(1))) bv <- replicateM(mkRegU);
   Reg#(Bit#(4)) idx <- mkRegU;
   Reg#(int) a <- mkRegU;
   Reg#(int) b <- mkRegU;
   Reg#(int) sel <- mkRegU;

   rule r;
      sel <= (bv[idx] == 0) ? a : b;
   endrule
endmodule
