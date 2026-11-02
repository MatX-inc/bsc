// Two defs the transform stage merges: the evaluator keeps a def for
// each of the two selections (each is read twice), one of them nested
// under its own condition again, which the transform's context rewrite
// reduces to the other, so its CSE finds the two defs equal and merges
// them.
interface MergeIfc;
   method UInt#(8) value();
endinterface

(* synthesize *)
module sysMetaMerge(MergeIfc);
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(0);
   Reg#(UInt#(8)) c <- mkReg(0);
   Reg#(UInt#(8)) d <- mkReg(0);
   Reg#(UInt#(8)) e <- mkReg(0);
   Reg#(UInt#(8)) f <- mkReg(0);

   rule step;
      Bool big = a > b;
      UInt#(8) x = big ? a : b;
      UInt#(8) y = big ? (big ? a : d) : b;
      c <= x;
      d <= x + 1;
      e <= y;
      f <= y + 1;
   endrule

   method UInt#(8) value();
      return c;
   endmethod
endmodule
