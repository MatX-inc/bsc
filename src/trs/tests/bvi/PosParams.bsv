// R5 battery: typed parameters -- signed decimal, 96-bit sized hex,
// string, real -- through the -G bake; the display fires from the
// model itself so serialization must be SEMANTICALLY exact.
interface Ifc;
   method Action go();
endinterface

import "BVI" ParamShow =
module mkParamShow#(Integer sp, Bit#(96) wp, String st, Real rp)(Ifc);
   parameter SIGNED_P = sp;
   parameter WIDE_P = wp;
   parameter STR_P = st;
   parameter REAL_P = rp;
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method go() enable(GO);
   schedule go C go;
endmodule

(* synthesize *)
module sysPosParams();
   let dut <- mkParamShow(-5, 96'hDEADBEEF_00112233_44556677,
                          "hello world", 2.5);
   rule fire;
      dut.go();
   endrule
endmodule
