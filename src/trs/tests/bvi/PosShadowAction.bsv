// R2 positive: self-SBR Action (no result consumption): replacement is
// register-like and unconditionally sound; accepted with no condition.
interface Ifc;
   method Action put(Bit#(8) x);
   method Bit#(8) peek();
endinterface

import "BVI" Store =
module mkStore(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method put(IN) enable(EN);
   method PEEK peek();
   schedule put SBR put;
   schedule peek SB put;
   schedule peek CF peek;
endmodule

(* synthesize *)
module sysPosShadowAction();
   Reg#(Bit#(2)) cyc <- mkReg(0);
   let dut <- mkStore;
   rule rA;
      dut.put(10);
   endrule
   rule rB;
      dut.put(20);
   endrule
   rule show;
      $display("peek=%0d", dut.peek());
      cyc <= cyc + 1;
      if (cyc == 2) $finish(0);
   endrule
endmodule
