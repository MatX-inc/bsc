package Kinded;

// Type parameters annotated with their kinds ("numeric type n" beside a
// plain "type a") are an IdPKind in the typechecked package, the one IdK
// form a .bo never holds, so the .bcheck writer needs an arm of its own
// for it.  An interface, a struct and a typeclass carry one each.

interface Shifter#(type a, numeric type n);
   method a shift(a x);
endinterface

typedef struct { Bit#(n) value; } Word#(numeric type n) deriving (Bits, Eq);

typeclass Widen#(type a, numeric type n)
   dependencies (a determines n);
   function Bit#(n) widen(a x);
endtypeclass

instance Widen#(Bit#(8), 16);
   function widen(x) = zeroExtend(x);
endinstance

module mkShifter(Shifter#(Bit#(n), k));
   method shift(x) = x << valueOf(k);
endmodule

endpackage
