// deriving-generated code must not trigger pattern warnings, even though
// e.g. the unpack function for a non-power-of-2 enum is incomplete
typedef enum { X, Y, Z, W, V } Five deriving (Bits, Eq, Bounded, FShow);

typedef enum { P, Q, R } Three deriving (Bits, Eq, Bounded, FShow);

typedef union tagged {
   void JustOne;
} Single deriving (Bits, Eq, FShow);

typedef union tagged {
   void None;
   Bit#(8) Some;
} Opt deriving (Bits, Eq, FShow);

typedef struct {
   Five f;
   Three t;
} Pair deriving (Bits, Eq, FShow);

// the ".Tag" selection sugar generates an intentionally partial match
function Bit#(8) dotTag(Opt o);
   return o.Some;
endfunction
