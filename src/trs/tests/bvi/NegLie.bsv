// R5 battery: the lying import (undeclared IN -> PEEK path).  The
// contract this BVI declares is clean -- the LIE is in the Verilog --
// so export and link accept it; TRS_BVI_CHECK=observe catches it at
// runtime with a sound witness.
interface Ifc;
   method Action put(Bit#(8) x);
   method Bit#(8) peek();
endinterface

import "BVI" Liar =
module mkLiar(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method put(IN) enable(EN);
   method PEEK peek();
   schedule peek SB put;
   schedule peek CF peek;
   schedule put C put;
endmodule

(* synthesize *)
module sysNegLie();
   let dut <- mkLiar;
   Reg#(Bit#(4)) n <- mkReg(0);
   rule show;
      $display("peek=%0d", dut.peek());
   endrule
   rule step;
      dut.put({4'b0, n});
      n <= n + 1;
      if (n == 5) $finish(0);
   endrule
endmodule
