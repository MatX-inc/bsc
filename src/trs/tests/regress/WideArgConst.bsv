// Wide (>64-bit) module arguments must reach compiled bodies with all
// their limbs: port_consts once stored a single u64 and the width filter
// dropped wide params entirely, so the port fallthrough folded them to
// 0/1 (sysWideModArgPortTest ran compiled to a silent empty run).
(* synthesize *)
module sysWideArgConst();
   let x <- mkWideArgConstSub(65'h1_0000_0000_0000_0003, 2);
endmodule

(* synthesize *)
module mkWideArgConstSub#(parameter Bit#(65) val1, Bit#(65) val2)(Reg#(Bit#(65)));
   Reg#(Bool) started <- mkReg(False);
   Reg#(Bit#(65)) sum <- mkReg(val1);
   Reg#(Bit#(65)) cnt <- mkRegU;

   rule start (!started);
      cnt <= val1 + val2;
      started <= True;
   endrule

   rule count (started && (cnt > 65'h1_0000_0000_0000_0000));
      $display("Count: %x", cnt);
      cnt <= cnt - 1;
   endrule

   rule finish (started && (cnt <= 65'h1_0000_0000_0000_0000));
      $finish(0);
   endrule

   return sum;
endmodule
