import Vector::*;

// A mapped vector: each element is built in its own iteration of the
// map's loop, and the element's chain carries that iteration.
function UInt#(8) twice(UInt#(8) x);
   return x + x;
endfunction

function UInt#(8) incr(UInt#(8) x);
   return x + 1;
endfunction

interface VecMapIfc;
   method UInt#(8) sum();
endinterface

(* synthesize *)
module sysMetaVecMap(VecMapIfc);
   Vector#(3, Reg#(UInt#(8))) regs <- replicateM(mkReg(0));

   rule tick;
      Vector#(3, UInt#(8)) vals = map(twice, readVReg(regs));
      regs[0] <= incr(vals[0]);
      regs[1] <= incr(vals[1]);
      regs[2] <= incr(vals[2]);
   endrule

   method UInt#(8) sum();
      return regs[0] + regs[1] + regs[2];
   endmethod
endmodule
