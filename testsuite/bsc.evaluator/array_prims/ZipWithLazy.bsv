import Vector::*;

// zipWith cells must stay lazy: an element that would error if forced
// is fine as long as it is never consumed (matching map and the old
// loop-based zipWith).

(* synthesize *)
module sysZipWithLazy(Empty);

  Reg#(Bit#(8)) r <- mkRegU;

  Vector#(2, Bit#(8)) a = replicate(8'h1);
  Vector#(2, Bit#(8)) bad =
    cons(8'h2, cons(error("this element must never be forced"), nil));
  Vector#(2, Bit#(8)) z = zipWith( \+ , a, bad);

  rule go;
    r <= z[0];
  endrule

endmodule
