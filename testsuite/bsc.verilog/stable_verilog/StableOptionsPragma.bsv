// The source compile applies this module's options pragma. Replay must
// receive -keep-fires explicitly to produce the same Verilog.
(* synthesize *)
(* options = "-keep-fires" *)
module sysStableOptionsPragma();
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(1);
   rule r1 (a < 100); a <= a + b; endrule
   rule r2 (b < 50); b <= b + 1; endrule
endmodule
