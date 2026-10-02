package SerOrder;

// Every Bits#(t, n) predicate the typechecker satisfies records
// SizeOf#(t) = n in the package's associated-type-function cache, a map the
// .bo carries, keyed on (SizeOf, [t]).  Two struct types give two entries
// whose keys compare by the structs' names, so the map's order, and with it
// the bytes of the .bo, once followed the names' intern order.  No printer
// shows the cache: the difference is in the Hash line of the dump alone.
typedef struct { Bit#(8) x; } SerAlpha deriving (Bits, Eq);
typedef struct { Bit#(4) y; } SerBeta deriving (Bits, Eq);

function Bit#(12) serOrder(SerAlpha a, SerBeta b);
   return {pack(a), pack(b)};
endfunction

endpackage
