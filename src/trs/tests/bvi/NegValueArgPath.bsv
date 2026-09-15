// R2 negative: a cross-method path sourced from a VALUE method's
// argument: value args have no selection event (guards probe before
// firing is known), so a losing probe's args can sit in the shadow
// vector at a frontier; refused.
interface Ifc;
   method Bit#(8) f(Bit#(8) x);
   method Bit#(8) g();
endinterface

import "BVI" VArg =
module mkVArg(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method FOUT f(FIN);
   method GOUT g();
   path(FIN, GOUT);
   schedule f SB g;
   schedule f CF f;
   schedule g CF g;
endmodule

(* synthesize *)
module sysNegValueArgPath();
   let dut <- mkVArg;
   rule r;
      $display("%0d %0d", dut.f(1), dut.g());
      $finish(0);
   endrule
endmodule
