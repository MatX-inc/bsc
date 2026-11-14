(* synthesize *)
module sysIfSame2();
   Reg#(Bit#(8)) r <- mkReg(0);
   Reg#(Bool) c <- mkReg(False);
   Reg#(Bool) d <- mkReg(False);
   rule bothGuarded;
      Bit#(8) v = (r[0] == 1) ? when(c, 8'd5) : when(d, 8'd5);
      $display("both %0d", v);
   endrule
   rule bothGuardedDiff;
      Bit#(8) v = (r[0] == 1) ? when(c, 8'd5) : when(d, 8'd6);
      $display("diff %0d", v);
   endrule
endmodule
