import List::*;

// A select index that is a compile-time mux of constants must be
// pushed into the branches (selecting each and muxing the results),
// like array selection, rather than failing.

(* synthesize *)
module sysListSelectDyn(Empty);

  Reg#(Bit#(8)) cycle <- mkReg(0);
  Reg#(Bool) b <- mkReg(False);

  List#(Bit#(8)) xs = Cons(8'h10, Cons(8'h21, Cons(8'h32, Nil)));

  rule show;
    $display("D c=%0d v=%0h", cycle, xs[b ? 1 : 2]);
  endrule

  rule tick;
    b <= !b;
    cycle <= cycle + 1;
    if (cycle == 4) $finish(0);
  endrule

endmodule
