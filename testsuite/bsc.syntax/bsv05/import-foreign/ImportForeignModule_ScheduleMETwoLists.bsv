// The ME scheduling annotation is not supported, also with two lists

interface Ifc;
   method Action a(Bit#(8) x);
   method Action b(Bit#(8) x);
endinterface

import "BVI" VMod =
module mkMod (Ifc);
   method a(A_IN) enable(EN_A);
   method b(B_IN) enable(EN_B);
   schedule (a) ME (b);
   schedule (a, b) C (a, b);
endmodule
