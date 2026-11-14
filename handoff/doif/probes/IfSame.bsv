// An if whose arms evaluate to the same constant, where only the then arm
// carries an implicit condition (IExpand doIf's equal-arm shortcut).
(* synthesize *)
module sysIfSame();
   Reg#(Bit#(8)) r <- mkReg(0);
   Reg#(Bool) c <- mkReg(False);
   rule sameConst;
      Bit#(8) v = (r[0] == 1) ? when(c, 8'd5) : 8'd5;
      $display("sameConst %0d", v);
   endrule
   rule diffConst;
      Bit#(8) v = (r[0] == 1) ? when(c, 8'd5) : 8'd6;
      $display("diffConst %0d", v);
   endrule
   rule inc;
      r <= r + 1;
   endrule
endmodule
