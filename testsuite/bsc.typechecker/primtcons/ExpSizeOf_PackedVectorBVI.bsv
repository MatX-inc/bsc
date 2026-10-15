import Vector::*;

interface Cell;
  method Bit#(1) get();
endinterface

instance Bits#(Cell, 1);
  function Bit#(1) pack(Cell x) = x.get;
  function Cell unpack(Bit#(1) x);
    return interface Cell;
      method get = x;
    endinterface;
  endfunction
endinstance

// BVI treats both kinds of method as packed values.  It must not require a
// concrete vector length to construct the interface-field descriptor.
interface PackedVector#(numeric type n);
  method Vector#(n, Cell) value();
  method Vector#(n, Cell) echo(Bit#(1) x);
endinterface

import "BVI" PackedVector =
module mkPackedVector (PackedVector#(n));
  default_clock no_clock;
  no_reset;
  parameter N = valueOf(n);
  method VALUE value();
  method ECHO echo(IN);
  schedule (value, echo) CF (value, echo);
endmodule

// An argument-taking synthesized method also remains packed.  Its length can
// be resolved during typechecking, after this local Bits instance is derived.
typedef struct { Bit#(2) width; } Width deriving (Bits);
typedef SizeOf#(Width) MethodLength;

interface PackedMethod;
  method Vector#(MethodLength, Cell) echo(Bit#(1) x);
endinterface

(* synthesize *)
module mkPackedVectorMethod (PackedMethod);
  method echo(x) = replicate(unpack(x));
endmodule
