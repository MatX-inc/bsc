import Vector::*;

// A local recursive binding, a primFix loop: the argument of each
// recursive call is built in the iteration that receives it.
function UInt#(8) incr(UInt#(8) x);
   return x + 1;
endfunction

interface FixIfc;
   method UInt#(8) value();
endinterface

(* synthesize *)
module sysMetaFix(FixIfc);
   Vector#(3, Reg#(UInt#(8))) regs <- replicateM(mkReg(0));
   Reg#(UInt#(8)) acc <- mkReg(0);

   function UInt#(8) addRegs(Integer n, UInt#(8) x);
      return (n == 0) ? x : addRegs(n - 1, x + regs[n - 1]);
   endfunction

   rule tick;
      acc <= incr(addRegs(3, acc));
   endrule

   method UInt#(8) value();
      return acc;
   endmethod
endmodule
