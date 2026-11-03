// A module parameter: the Verilog parameter stands for no record of
// the table (the join pins it among the unresolved identifiers, as it
// pins the clock and the reset), while the def that reads it resolves
// as any other.
interface ParamIfc;
   method UInt#(8) out();
endinterface

(* synthesize *)
module sysMetaParam#(parameter UInt#(8) k)(ParamIfc);
   Reg#(UInt#(8)) r <- mkReg(0);

   rule bump;
      r <= r + k;
   endrule

   method UInt#(8) out() = r;
endmodule
