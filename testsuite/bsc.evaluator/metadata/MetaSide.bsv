import Vector::*;

// A small module for the evaluator metadata side file.  The argument of
// a function call is heaped, so twice(a + b) under two rules and a method
// builds three equal cells for the one expression, which the rebuild's
// CSE identifies as one def standing for three contexts, in one alias
// class; the arguments of mix are strict operands beside each other, so
// each survives as a def bound to its parameter; the literal argument
// of plus7 is a constant cell the rebuild folds into the def that uses
// it; the static loop counts its iterations; the registers'
// instantiations are roots.
interface SideIfc;
   method UInt#(8) sum();
   method Action put(UInt#(8) x);
endinterface

function UInt#(8) twice(UInt#(8) x);
   return x + x;
endfunction

function UInt#(8) plus7(UInt#(8) x);
   return twice(x) + 7;
endfunction

function Bit#(8) mix(Bit#(8) x, Bit#(8) y);
   return (x << 1) + y;
endfunction

(* synthesize *)
module sysMetaSide(SideIfc);
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(0);
   Vector#(3, Reg#(UInt#(8))) vs <- replicateM(mkReg(0));
   Reg#(Bit#(8)) c <- mkReg(0);

   rule r1;
      a <= twice(a + b);
   endrule

   rule r2 (b != 7);
      b <= twice(a + b);
   endrule

   rule r3;
      for (Integer i = 0; i < 3; i = i + 1)
         vs[i] <= plus7(vs[i]);
   endrule

   rule r4;
      c <= mix(pack(a) + 1, pack(b) ^ 3);
   endrule

   method UInt#(8) sum();
      return twice(a + b);
   endmethod

   method Action put(UInt#(8) x);
      a <= x;
   endmethod
endmodule
