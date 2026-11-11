package FieldAssocBSV;

// The field type needs SizeOf#(t), so the Bits proviso stays: no warning.

typedef struct { Bit#(SizeOf#(t)) raw; } PackedT#(type t) deriving (Bits);

function PackedT#(t) copyIt(PackedT#(t) p) provisos (Bits#(t, sz));
   PackedT#(t) q = PackedT { raw: p.raw };
   return q;
endfunction

endpackage
