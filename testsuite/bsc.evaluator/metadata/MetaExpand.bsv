// A def inlined by aExpand: the conversion names a def for each
// compound subexpression of the rule, and the backend's expansion
// substitutes the single-use defs into the one that uses them, down to
// the register's write port, whose record then stands for them all.
(* synthesize *)
module sysMetaExpand(Empty);
   Reg#(Bit#(8)) a <- mkReg(0);
   Reg#(Bit#(8)) b <- mkReg(0);
   Reg#(Bit#(8)) c <- mkReg(0);

   rule step;
      Bit#(8) x = a ^ b;
      Bit#(8) y = x & c;
      a <= y | 8'h0f;
   endrule
endmodule
