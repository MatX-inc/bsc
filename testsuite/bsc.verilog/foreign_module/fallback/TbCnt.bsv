// A foreign module import with a pure-BSV fallback: the Verilog backend
// emits one shared instantiation under an ifdef that selects the module
// name, and Bluesim resolves the instance to the fallback's module.

interface Cnt;
   method Action incr();
   method Bit#(8) value();
endinterface

(* synthesize *)
module mkCntModel(Cnt);
   Reg#(Bit#(8)) r <- mkReg(0);
   method Action incr(); r <= r + 1; endmethod
   method value = r;
endmodule

import "BVI" VendorCnt =
module mkVendorCnt(Cnt);
   default_clock clk(CLK, (*unused*) CLK_GATE);
   default_reset rst(RST_N);
   method incr() enable(EN_incr);
   method VAL value();
   schedule (value) CF (value);
   schedule (incr) C (incr);
   schedule (value) SB (incr);
   fallback mkCntModel;
endmodule

(* synthesize *)
module sysTbCnt(Empty);
   Cnt c <- mkVendorCnt;
   Reg#(Bit#(8)) n <- mkReg(0);
   rule step;
      c.incr;
      n <= n + 1;
      $display("cycle %0d cnt %0d", n, c.value);
      if (n == 10) $finish(0);
   endrule
endmodule
