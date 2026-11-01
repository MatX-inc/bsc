// Two top-level functions recursive through each other: one component
// of the package's definition graph, so both are recursion points.
function UInt#(8) stepA(Integer n, UInt#(8) x);
   return (n == 0) ? x : stepB(n - 1, x + 1);
endfunction

function UInt#(8) stepB(Integer n, UInt#(8) x);
   return (n == 0) ? x : stepA(n - 1, x + x);
endfunction

interface MutualIfc;
   method UInt#(8) value();
endinterface

(* synthesize *)
module sysMetaMutual(MutualIfc);
   Reg#(UInt#(8)) acc <- mkReg(1);

   rule tick;
      acc <= stepA(3, acc);
   endrule

   method UInt#(8) value();
      return acc;
   endmethod
endmodule
