package InlinedRegisterOrderScramble;

// The probe's clock and reset arguments with clkM and rstM declared before
// clkZ and rstZ, and a register on each: compiled before the probe in a
// batch, this module's wrapper creates the port names CLK_clkM and
// RST_N_rstM before CLK_clkZ and RST_N_rstZ.  (Every wrapper creates CLK
// and RST_N, the default clock and reset's, before its argument ports, so
// a batch cannot move those; -reverse-intern-order does.)
(* synthesize *)
module mkInlinedRegisterOrderScramble #(Clock clkM, Reset rstM,
                                        Clock clkZ, Reset rstZ,
                                        Reset rstA2) (Empty);
   Reg#(UInt#(8)) s_m  <- mkReg(0, clocked_by clkM, reset_by rstM);
   Reg#(UInt#(8)) s_z  <- mkReg(0, clocked_by clkZ, reset_by rstZ);
   Reg#(UInt#(8)) s_a2 <- mkReg(0, reset_by rstA2);
   Reg#(UInt#(8)) s_a  <- mkReg(0);

   rule tick_m;  s_m  <= s_m  + 1; endrule
   rule tick_z;  s_z  <= s_z  + 1; endrule
   rule tick_a2; s_a2 <= s_a2 + 1; endrule
   rule tick_a;  s_a  <= s_a  + 1; endrule
endmodule

endpackage
