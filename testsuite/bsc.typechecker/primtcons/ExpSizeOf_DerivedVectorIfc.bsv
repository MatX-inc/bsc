import Vector::*;

// Wrapper generation must see the local derived instance, not just instances
// imported from libraries.  This length is unavailable before deriving.
typedef struct { Bit#(2) bits; } Width deriving (Bits);
typedef SizeOf#(Width) Length;

interface Example;
  interface Vector#(Length, WriteOnly#(Bit#(1))) example;
endinterface

(* synthesize *)
module mkDerivedVectorIfc (Example);
  interface example = ?;
endmodule
