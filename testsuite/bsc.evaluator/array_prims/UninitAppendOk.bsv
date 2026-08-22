import Vector::*;

// Appending a newVector (undefined elements) is fine: cells are copied
// lazily, so the undefined elements are only don't-cares if consumed,
// matching the old loop-based append.

(* synthesize *)
module sysUninitAppendOk(Empty);

  Reg#(Bit#(8)) r <- mkRegU;

  Vector#(2, Bit#(8)) u = newVector;
  Vector#(2, Bit#(8)) w = replicate(8'h5);
  Vector#(4, Bit#(8)) c = append(u, w);

  rule go;
    r <= c[2] + c[3];
  endrule

endmodule
