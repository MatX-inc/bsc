import Vector::*;

// toVector of a conditional list: the PrimIf spine must be pushed into
// the conversion (selecting each arm and muxing the arrays), not
// rejected.  (This used to be an internalError in the evaluator.)

(* synthesize *)
module sysListToVectorIf(Empty);

  Reg#(Bool) b <- mkReg(False);
  Reg#(Bit#(8)) cycle <- mkReg(0);

  List#(Bit#(8)) xs = Cons(8'h1, Cons(8'h2, Nil));
  List#(Bit#(8)) ys = Cons(8'h3, Cons(8'h4, Nil));
  Vector#(2, Bit#(8)) v = toVector(b ? xs : ys);

  rule show;
    $display("T c=%0d v0=%0h v1=%0h", cycle, v[0], v[1]);
  endrule

  rule tick;
    b <= !b;
    cycle <= cycle + 1;
    if (cycle == 3) $finish(0);
  endrule

endmodule
