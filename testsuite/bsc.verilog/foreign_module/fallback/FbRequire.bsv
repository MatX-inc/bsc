interface Cnt;
   method Action incr();
endinterface

import "BVI" VendorCnt =
module mkVendorCnt(Cnt);
   default_clock clk(CLK, (*unused*) CLK_GATE);
   default_reset rst(RST_N);
   method incr() enable(EN_incr);
   schedule (incr) C (incr);
endmodule

(* synthesize *)
module sysFbRequire(Empty);
   Cnt c <- mkVendorCnt;
   rule step;
      c.incr;
   endrule
endmodule
