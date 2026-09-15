// R5 battery: plusargs visible inside the model ($test$plusargs and
// $value$plusargs), passed at construction via the shim's argv.
interface Ifc;
   method Action go();
endinterface

import "BVI" Plussy =
module mkPlussy(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method go() enable(GO);
   schedule go C go;
endmodule

(* synthesize *)
module sysPosPlus();
   let dut <- mkPlussy;
   rule r;
      dut.go();
   endrule
endmodule
