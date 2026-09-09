// R2 negative: two value methods reading the SAME physical output port
// (aliasing that can hide undeclared cross-method paths); refused.
interface Ifc;
   method Bit#(8) a();
   method Bit#(8) b();
endinterface

import "BVI" Shared =
module mkShared(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method OUT a();
   method OUT b();
   schedule a CF a;
   schedule b CF b;
   schedule a CF b;
endmodule

(* synthesize *)
module sysNegSharedOut();
   let dut <- mkShared;
   rule r;
      $display("%0d %0d", dut.a(), dut.b());
      $finish(0);
   endrule
endmodule
