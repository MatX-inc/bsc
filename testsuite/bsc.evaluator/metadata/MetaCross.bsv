import MetaCrossLib::*;

// The same function called from this package and from inside its own
// package: a use site against a definition site on the chain.
interface CrossIfc;
   method UInt#(8) value();
endinterface

(* synthesize *)
module sysMetaCross(CrossIfc);
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(0);

   rule direct;
      a <= incr(scale(a));
   endrule

   rule indirect;
      b <= viaScale(b);
   endrule

   method UInt#(8) value();
      return a + b;
   endmethod
endmodule
