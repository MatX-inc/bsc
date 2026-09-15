// R2 positive: self-SBR ActionValue whose result is consumed by a
// caller that is mutually exclusive with every later caller -- the
// accepted half of the atomic-read condition (design section 4.2).
interface Ifc;
   method ActionValue#(Bit#(8)) m(Bit#(8) x);
endinterface

import "BVI" Echo =
module mkEcho(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method OUT m(IN) enable(EN);
   schedule m SBR m;
endmodule

(* synthesize *)
module sysPosShadowFixed();
   Reg#(Bit#(8)) a <- mkReg(0);
   Reg#(Bit#(2)) cyc <- mkReg(0);
   let dut <- mkEcho;

   rule rA (cyc == 0);            // consumer: exclusive with rB
      let v <- dut.m(10);
      a <= v;
   endrule

   rule rB (cyc == 1);            // later caller, disjoint predicate
      let w <- dut.m(20);
      $display("rB saw %0d", w);
   endrule

   rule step;
      cyc <= cyc + 1;
      if (cyc == 2) begin
         $display("a = %0d", a);
         $finish(0);
      end
   endrule
endmodule
