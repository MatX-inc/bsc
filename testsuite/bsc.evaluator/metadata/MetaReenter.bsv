import Vector::*;

// A loop re-entered from inside its own body: zipWith over a vector of
// vectors runs the library's zipWith loop again when a row is forced,
// so one static loop site is on a chain twice, as two dynamic instances
// with the inner element loop's frames between them.
function UInt#(8) incr(UInt#(8) x);
   return x + 1;
endfunction

function UInt#(8) add(UInt#(8) a, UInt#(8) b);
   return a + b;
endfunction

function Vector#(2, UInt#(8)) addRow(Vector#(2, UInt#(8)) x, Vector#(2, UInt#(8)) y);
   return zipWith(add, x, y);
endfunction

interface ReenterIfc;
   method UInt#(8) total();
endinterface

(* synthesize *)
module sysMetaReenter(ReenterIfc);
   Vector#(2, Vector#(2, Reg#(UInt#(8)))) a <- replicateM(replicateM(mkReg(0)));
   Vector#(2, Vector#(2, Reg#(UInt#(8)))) b <- replicateM(replicateM(mkReg(1)));

   rule tick;
      Vector#(2, Vector#(2, UInt#(8))) sums =
         zipWith(addRow, map(readVReg, a), map(readVReg, b));
      a[0][0] <= incr(sums[0][0]);
      a[0][1] <= incr(sums[0][1]);
      a[1][0] <= incr(sums[1][0]);
      a[1][1] <= incr(sums[1][1]);
   endrule

   method UInt#(8) total();
      return a[0][0] + a[1][1];
   endmethod
endmodule
