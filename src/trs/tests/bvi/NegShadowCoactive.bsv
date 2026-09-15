// R2 negative: self-SBR ActionValue result consumed by a caller that
// can fire together with a LATER caller of the same method -- the read
// is not atomic with the last call, so the export refuses (the
// executable witness for this divergence is the shadow-witness pair in
// the design record: Verilog stores 21 where atomic semantics says 11).
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
module sysNegShadowCoactive();
   Reg#(Bit#(8)) a <- mkReg(0);
   Reg#(Bit#(2)) cyc <- mkReg(0);
   let dut <- mkEcho;

   rule rA (cyc == 0);            // consumes, but rB also fires
      let v <- dut.m(10);
      a <= v;
   endrule

   rule rB (cyc == 0);            // coactive later caller
      let w <- dut.m(20);
      $display("rB saw %0d", w);
   endrule

   rule step;
      cyc <= cyc + 1;
      if (cyc == 1) begin
         $display("a = %0d", a);
         $finish(0);
      end
   endrule
endmodule
