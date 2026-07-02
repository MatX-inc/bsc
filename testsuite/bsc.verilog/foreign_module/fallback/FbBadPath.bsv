interface PathIfc;
   method Bit#(8) f(Bit#(8) x);
endinterface

// f's result depends combinationally on its argument, but the import
// declares no paths
(* synthesize *)
module mkPathModel(PathIfc);
   method f(x) = x + 1;
endmodule

import "BVI" VendorPath =
module mkVendorPath(PathIfc);
   default_clock clk(CLK, (*unused*) CLK_GATE);
   no_reset;
   method F f(X);
   schedule (f) CF (f);
   fallback mkPathModel;
endmodule
