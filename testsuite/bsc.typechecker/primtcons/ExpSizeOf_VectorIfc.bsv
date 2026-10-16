// GitHub Issue #313 (Bluespec Inc Bug 1917): resolve the SizeOf in an interface
// vector's length before generating its field metadata.  The compiler and
// generic wrapper must both traverse the two subinterfaces.

import Vector::*;

// Keep the type function here rather than spelling the length as a literal.
typedef SizeOf#(Bit#(2)) Length;

typedef Bit#(1) Data;

interface Example;
  interface Vector#(Length, WriteOnly#(Data)) example;
endinterface

(* synthesize *)
module mkExample (Example);
  Vector#(Length, Reg#(Data)) ex <- replicateM(mkRegU);
  interface example = ?;
endmodule
