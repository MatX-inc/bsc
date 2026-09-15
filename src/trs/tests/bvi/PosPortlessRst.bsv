// R5: a PORTLESS input reset declared before a ported one.  The
// portless form is an association only -- "the parent needs to
// associate a reset with methods, but the reset is not used
// internally" (BSV ref guide) -- so it has no Verilog port and nothing
// to drive.  It still arrives as a Reset instantiation argument, and
// the contract carries only the ported resets, so the two sides must
// agree on which argument each contract reset is: get that wrong and
// RST2_N is never driven and b is silently never cleared.
import Clocks :: *;

interface Ifc;
   method Action put(Bit#(8) x);
   method Bit#(8) geta();
   method Bit#(8) getb();
endinterface

import "BVI" TwoRst =
module mkTwoRst#(Reset rd, Reset r2)(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   input_reset dead() = rd;
   input_reset rst2(RST2_N) = r2;
   method put(IN) enable(EN);
   method OUTA geta();
   method OUTB getb();
   schedule (geta, getb) SB put;
   schedule put C put;
   schedule geta CF (geta, getb);
   schedule getb CF getb;
endmodule

(* synthesize *)
module sysPosPortlessRst();
   Clock clk <- exposeCurrentClock;
   MakeResetIfc mr  <- mkReset(1, True, clk);
   MakeResetIfc mrd <- mkReset(1, True, clk);
   Ifc dut <- mkTwoRst(mrd.new_rst, mr.new_rst);
   Reg#(Bit#(4)) n <- mkReg(0);

   rule show;
      $display("a=%h b=%h", dut.geta(), dut.getb());
   endrule

   rule step;
      n <= n + 1;
      // same timing as PosTwoRst: the puts land after rst2 deasserts,
      // so the pulse at n==7 is observable as b returning to 22
      if (n > 2 && n < 6) dut.put({4'h4, n});
      if (n == 7) mr.assertReset();
      if (n == 9) $finish(0);
   endrule
endmodule
