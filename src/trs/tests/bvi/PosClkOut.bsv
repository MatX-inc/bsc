// Output-clock positive (v1.2b): the import divides its input clock by
// two; a register and rule clocked by the derived clock advance at the
// divided rate.  Oracle: the same BSV under the Verilog flow (iverilog).
interface DivClkIfc;
   interface Clock clk_out;
   method Bit#(8) cnt();
endinterface

import "BVI" DivClk =
module mkDivClk(DivClkIfc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   output_clock clk_out(CLK_OUT);
   method CNT cnt();
   schedule cnt CF cnt;
endmodule

(* synthesize *)
module sysPosClkOut();
   DivClkIfc d <- mkDivClk;
   Reg#(Bit#(8)) s <- mkRegU(clocked_by d.clk_out);

   rule slowstep;
      s <= s + 1;
      $display("slow=%0d", s);
      if (s == 9) $finish(0);
   endrule
endmodule
