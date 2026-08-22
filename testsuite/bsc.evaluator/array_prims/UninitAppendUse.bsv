import Vector::*;

// Consuming an element from the undefined half of an appended vector
// yields a don't-care value (newVector semantics), not an error.

(* synthesize *)
module sysUninitAppendUse(Empty);

  Reg#(Bit#(8)) r <- mkRegU;

  Vector#(2, Bit#(8)) u = newVector;
  Vector#(2, Bit#(8)) w = replicate(8'h5);
  Vector#(4, Bit#(8)) c = append(u, w);

  rule go;
    r <= c[0];
  endrule

endmodule
