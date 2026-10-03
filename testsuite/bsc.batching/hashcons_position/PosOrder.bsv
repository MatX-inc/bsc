package PosOrder;

// Ground types that the scrambler also writes, so that in the batch their
// hash-consed nodes are first built while the scrambler is parsed.  The
// .bo's signature carries these types with positions; they must be this
// file's positions in both compiles.
import Vector::*;

typedef Vector#(4, Bit#(8)) Row;

function Maybe#(Bit#(8)) firstOf(Row r);
   return tagged Valid r[0];
endfunction

function Tuple2#(Bool, Bit#(8)) tagIt(Bit#(8) x);
   return tuple2(x != 0, x);
endfunction

endpackage
