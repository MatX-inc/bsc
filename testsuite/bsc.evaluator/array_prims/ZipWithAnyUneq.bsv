import Vector::*;

// zipWithAny with different-size vectors: the result takes the length
// of the shorter input, in either argument order.  (The longer-first
// order used to crash the compiler with an array bounds exception.)

(* synthesize *)
module sysZipWithAnyUneq(Empty);

  Reg#(Bit#(8)) cycle <- mkReg(0);

  Vector#(4, Bit#(8)) a = genWith(fromInteger);
  Vector#(2, Bit#(8)) b = genWith(compose(fromInteger, \+ (16)));

  function Bit#(8) add2(Bit#(8) x, Bit#(8) y) = x + y;

  Vector#(2, Bit#(8)) z1 = zipWithAny(add2, a, b);
  Vector#(2, Bit#(8)) z2 = zipWithAny(add2, b, a);

  rule show;
    $display("Z c=%0d z1_0=%0h z1_1=%0h z2_0=%0h z2_1=%0h",
             cycle, z1[0], z1[1], z2[0], z2[1]);
  endrule

  rule tick;
    cycle <= cycle + 1;
    if (cycle == 4) $finish(0);
  endrule

endmodule
