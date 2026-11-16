package GuardFuel;

typedef enum { FuelA, FuelB } FuelTag deriving (Bits, Eq);

// The first arm is impossible.  Normal fuel proves it redundant; a tiny
// budget must abandon the conclusion conservatively.
function Bit#(1) impossibleGuard(FuelTag tag, Bool g);
   return (case (tag) matches
              FuelA &&& (g && (!g)): 0;
              default: 1;
           endcase);
endfunction

endpackage
