import Vector::*;

// This instance is derived after wrapper generation, so its SizeOf cannot yet
// be used to construct interface-vector metadata.  Report a source type error
// rather than generating a descriptor with the wrong number of leaves.
typedef struct { Bit#(2) bits; } Width deriving (Bits);
typedef SizeOf#(Width) Length;

interface Example;
  interface Vector#(Length, WriteOnly#(Bit#(1))) example;
endinterface

(* synthesize *)
module mkVectorIfcUnresolved (Example);
  interface example = ?;
endmodule
