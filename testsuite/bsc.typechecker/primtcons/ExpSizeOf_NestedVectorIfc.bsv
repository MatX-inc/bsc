import Vector::*;
import ListN::*;

typedef SizeOf#(Bit#(2)) Rows;
typedef TAdd#(1, SizeOf#(Bit#(1))) Columns;

interface Example;
  interface Vector#(Rows, ListN#(Columns, WriteOnly#(Bit#(1)))) lanes;
  method Vector#(Columns, Bit#(8)) samples();
endinterface

(* synthesize *)
module mkNestedVectorIfc (Example);
  interface lanes = ?;
  method samples = Vector::replicate(8'hA5);
endmodule

// BVI keeps the whole-Bits policy for an ordinary vector method result.
interface PackedVector;
  method Vector#(Columns, Bit#(8)) read();
endinterface

import "BVI" PackedVector =
module mkPackedVector (PackedVector);
  default_clock no_clock;
  no_reset;
  method OUT read();
  schedule read CF read;
endmodule
