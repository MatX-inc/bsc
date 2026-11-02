import Vector::*;

// Two nested static loops: the inner loop's iterations are nested under
// the outer loop's iteration that ran them, each loop with its own count.
interface NestedIfc;
   method UInt#(8) total();
endinterface

(* synthesize *)
module sysMetaNested(NestedIfc);
   Vector#(2, Vector#(2, Reg#(UInt#(8)))) grid <- replicateM(replicateM(mkReg(0)));

   rule tick;
      grid[0][0] <= grid[0][0] + 1;
   endrule

   method UInt#(8) total();
      UInt#(8) s = 0;
      for (Integer i = 0; i < 2; i = i + 1)
         for (Integer j = 0; j < 2; j = j + 1)
            s = s + grid[i][j];
      return s;
   endmethod
endmodule
