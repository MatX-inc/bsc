interface Cnt;
   method Action incr();
endinterface

// takes no arguments, but the import has a parameter
(* synthesize *)
module mkCntModel(Cnt);
   Reg#(Bit#(8)) r <- mkReg(0);
   method Action incr(); r <= r + 1; endmethod
endmodule

import "BVI" VendorCnt =
module mkVendorCnt#(Bit#(8) k)(Cnt);
   default_clock clk(CLK, (*unused*) CLK_GATE);
   default_reset rst(RST_N);
   parameter K = k;
   method incr() enable(EN_incr);
   schedule (incr) C (incr);
   fallback mkCntModel;
endmodule
