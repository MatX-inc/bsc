// Registers on three clocks, named so that the first register on each
// clock puts the clocks in an order that is neither the clock ports' order
// (CLK, clkZ, clkM) nor the clock names' sort order (CLK, clkM, clkZ): the
// first register is on clkM, the next on the default clock, the next on
// clkZ.  The default clock also carries a second reset, and the
// asynchronous-reset registers repeat the three clocks in the opposite
// order.  With inlined registers (the default) each clock, and each reset
// within a clock, gets its own group of statements under "handling of
// inlined registers"; those groups must come out in the order of the
// registers in the module's instance list, which the Verilog lists by name.

interface Counts;
   method UInt#(8) m;
   method UInt#(8) a;
   method UInt#(8) a2;
   method UInt#(8) z;
endinterface

(* synthesize *)
module mkInlinedRegisterOrder #(Clock clkZ, Reset rstZ,
                                Clock clkM, Reset rstM,
                                Reset rstA2) (Counts);

   // asynchronous-reset registers: clkZ, then the default clock, then clkM
   Reg#(UInt#(8)) a1_z  <- mkRegA(0, clocked_by clkZ, reset_by rstZ);
   Reg#(UInt#(8)) a2_a  <- mkRegA(0);
   Reg#(UInt#(8)) a3_m  <- mkRegA(0, clocked_by clkM, reset_by rstM);

   // synchronous-reset registers: clkM, then the default clock, then clkZ
   Reg#(UInt#(8)) n1_m  <- mkReg(0, clocked_by clkM, reset_by rstM);
   Reg#(UInt#(8)) n2_a  <- mkReg(0);
   Reg#(UInt#(8)) n3_z  <- mkReg(0, clocked_by clkZ, reset_by rstZ);

   // a second reset on the default clock, met after the first
   Reg#(UInt#(8)) n4_a2 <- mkReg(0, reset_by rstA2);

   // registers without a reset, on the two argument clocks
   Reg#(UInt#(8)) u5_m  <- mkRegU(clocked_by clkM, reset_by noReset);
   Reg#(UInt#(8)) u6_z  <- mkRegU(clocked_by clkZ, reset_by noReset);

   rule tick_m;
      n1_m <= n1_m + 1;
      u5_m <= n1_m;
      a3_m <= a3_m + 2;
   endrule

   rule tick_a;
      n2_a <= n2_a + 1;
      a2_a <= a2_a + 2;
   endrule

   rule tick_a2;
      n4_a2 <= n4_a2 + 3;
   endrule

   rule tick_z;
      n3_z <= n3_z + 1;
      u6_z <= n3_z;
      a1_z <= a1_z + 2;
   endrule

   method m  = n1_m + u5_m + a3_m;
   method a  = n2_a + a2_a;
   method a2 = n4_a2;
   method z  = n3_z + u6_z + a1_z;

endmodule
