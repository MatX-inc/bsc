// A def inlined by the inline stage: the sum is built once, under one
// rule, and read by one expression, so iInline substitutes its def into
// the def of that expression and the ledger records the inlining.
interface InlineIfc;
   method UInt#(8) value();
endinterface

(* synthesize *)
module sysMetaInline(InlineIfc);
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(0);
   Reg#(UInt#(8)) c <- mkReg(0);

   rule step;
      UInt#(8) s = a + b;
      c <= s * 3;
   endrule

   method UInt#(8) value();
      return c;
   endmethod
endmodule
