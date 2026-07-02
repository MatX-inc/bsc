interface Cnt;
   method Action incr();
endinterface

interface Other;
   method Bool b();
endinterface

(* synthesize *)
module mkOtherModel(Other);
   method b = True;
endmodule

import "BVI" VendorCnt =
module mkVendorCnt(Cnt);
   default_clock clk(CLK, (*unused*) CLK_GATE);
   default_reset rst(RST_N);
   method incr() enable(EN_incr);
   schedule (incr) C (incr);
   fallback mkOtherModel;
endmodule
