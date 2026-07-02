interface Two;
   method Action a();
   method Action b();
endinterface

// a and b write the same register, so they conflict; the import
// declares them CF, which this module cannot refine
(* synthesize *)
module mkTwoModel(Two);
   Reg#(Bit#(8)) r <- mkReg(0);
   method Action a(); r <= r + 1; endmethod
   method Action b(); r <= r + 2; endmethod
endmodule

import "BVI" VendorTwo =
module mkVendorTwo(Two);
   default_clock clk(CLK, (*unused*) CLK_GATE);
   default_reset rst(RST_N);
   method a() enable(EN_A);
   method b() enable(EN_B);
   schedule (a) CF (b);
   schedule (a) C (a);
   schedule (b) C (b);
   fallback mkTwoModel;
endmodule
