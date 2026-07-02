interface Cnt;
   method Action incr();
endinterface

// not marked (* synthesize *)
module mkCntModel(Cnt);
   Reg#(Bit#(8)) r <- mkReg(0);
   method Action incr(); r <= r + 1; endmethod
endmodule

import "BVI" VendorCnt =
module mkVendorCnt(Cnt);
   default_clock clk(CLK, (*unused*) CLK_GATE);
   default_reset rst(RST_N);
   method incr() enable(EN_incr);
   schedule (incr) C (incr);
   fallback mkCntModel;
endmodule
