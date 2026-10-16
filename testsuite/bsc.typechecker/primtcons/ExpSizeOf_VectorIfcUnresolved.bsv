import Vector::*;

// There is deliberately no Bits instance for Width.  An unresolved interface
// vector length must produce a source error, not malformed field metadata.
typedef struct { Bit#(2) bits; } Width;
typedef SizeOf#(Width) Length;

interface Example;
  interface Vector#(Length, WriteOnly#(Bit#(1))) example;
endinterface

(* synthesize *)
module mkVectorIfcUnresolved (Example);
  interface example = ?;
endmodule
