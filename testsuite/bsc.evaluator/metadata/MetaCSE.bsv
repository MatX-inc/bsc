// The same expression built under two rules: the rebuild's CSE keeps one
// def for both cells, each cell with its own chain.
function UInt#(8) incr(UInt#(8) x);
   return x + 1;
endfunction

interface CSEIfc;
   method UInt#(8) value();
endinterface

(* synthesize *)
module sysMetaCSE(CSEIfc);
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(0);
   Reg#(UInt#(8)) c <- mkReg(0);

   rule up;
      c <= incr(a + b);
   endrule

   rule down;
      a <= incr(a + b);
   endrule

   method UInt#(8) value();
      return c;
   endmethod
endmodule
