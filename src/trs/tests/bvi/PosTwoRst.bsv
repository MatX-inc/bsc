// R5 battery: TWO reset inputs on one import -- pins the reset-arg
// ordinal mapping (instantiation Reset args -> contract resets).  The
// second reset pulses mid-run and must clear only register b.
import Clocks :: *;

interface Ifc;
   method Action put(Bit#(8) x);
   method Bit#(8) geta();
   method Bit#(8) getb();
endinterface

import "BVI" TwoRst =
module mkTwoRst#(Reset r2)(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
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
module sysPosTwoRst();
   Clock clk <- exposeCurrentClock;
   MakeResetIfc mr <- mkReset(1, True, clk);
   Ifc dut <- mkTwoRst(mr.new_rst);
   Reg#(Bit#(4)) n <- mkReg(0);

   rule show;
      $display("a=%h b=%h", dut.geta(), dut.getb());
   endrule

   rule step;
      n <= n + 1;
      // the puts have to land AFTER rst2 deasserts: mkReset(1, True)
      // starts asserted and holds a cycle past the parent's deassert, so
      // a put at n<3 is swallowed and b never leaves its reset value --
      // which makes a wrong second-reset mapping invisible.
      if (n > 2 && n < 6) dut.put({4'h4, n});
      if (n == 7) mr.assertReset();
      if (n == 9) $finish(0);
   endrule
endmodule
