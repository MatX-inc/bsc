package ParamInoutBSV;

import Connectable::*;

interface DualInoutIfc#(numeric type a, numeric type b);
   interface Inout#(Bit#(a)) busA;
   interface Inout#(Bit#(TAdd#(a, b))) busSum;
endinterface

(* synthesize *)
module mkParamInoutConnect #(Inout#(Bit#(w)) left, Inout#(Bit#(w)) right) (Empty)
   provisos (Add#(1, _w, w));
   mkConnection(left, right);
endmodule

(* synthesize *)
module mkDualInoutThru #(Inout#(Bit#(a)) inA, Inout#(Bit#(TAdd#(a, b))) inSum)
                       (DualInoutIfc#(a, b))
   provisos (Add#(1, _a, a), Add#(1, _b, b), Add#(1, _s, TAdd#(a, b)));
   interface busA   = inA;
   interface busSum = inSum;
endmodule

endpackage
