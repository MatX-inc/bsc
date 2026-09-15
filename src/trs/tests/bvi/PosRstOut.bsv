// Output-reset positive (v1.2a): the import generates a reset that a
// downstream register is reset_by.  The stretcher asserts for two
// cycles after go(), so the derived reset asserts AND deasserts
// mid-run.  Oracle: the same BSV under the Verilog flow (iverilog).
interface RstStretchIfc;
   method Action go();
   method Bit#(2) state();
   interface Reset rst_out;
endinterface

import "BVI" RstStretch =
module mkRstStretch(RstStretchIfc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method go() enable(GO);
   method STATE state();
   output_reset rst_out(RST_OUT) clocked_by(clk);
   schedule state CF state;
   schedule state SB go;
   schedule go C go;
endmodule

(* synthesize *)
module sysPosRstOut();
   RstStretchIfc rs <- mkRstStretch;
   Reg#(Bit#(8)) held <- mkReg(0, reset_by rs.rst_out);
   Reg#(Bit#(8)) n <- mkReg(0);

   rule step;
      n <= n + 1;
      $display("c=%0d state=%0d held=%0d", n, rs.state(), held);
      if (n == 2) rs.go();
      if (n == 12) $finish(0);
   endrule
   rule bump;
      held <= held + 1;
   endrule
endmodule
