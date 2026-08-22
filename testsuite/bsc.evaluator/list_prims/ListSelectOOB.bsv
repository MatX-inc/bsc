import List::*;

// Selecting past the end of a list with a static index must be a
// compile-time error (as the recursive (!!) reported), not a silent
// undefined value.

(* synthesize *)
module sysListSelectOOB(Empty);

  List#(Bit#(8)) xs = Cons(8'h1, Cons(8'h2, Cons(8'h3, Nil)));
  Reg#(Bit#(8)) r <- mkRegU;

  rule go;
    r <= xs[5];
  endrule

endmodule
