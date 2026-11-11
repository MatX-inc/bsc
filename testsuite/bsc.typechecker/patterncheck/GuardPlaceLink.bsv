package GuardPlaceLink;

// A guard over a pattern-bound variable denotes the value at that pattern
// place.  Arms whose patterns constrain the same place must combine with
// the guard: these matches are exhaustive and must not warn.

// The guarded arm takes the True payload; the later arms cover the rest.
function Bit#(2) payloadGuard(Maybe#(Bool) m);
   return (case (m) matches
              tagged Valid .b &&& b: 0;
              tagged Valid False: 1;
              tagged Invalid: 2;
           endcase);
endfunction

// The same linkage at the scrutinee root.
function Bit#(1) rootGuard(Bool b);
   return (case (b) matches
              True: 0;
              .x &&& (!x): 1;
           endcase);
endfunction

typedef struct {
   Bool flag;
   Bit#(2) v;
} S deriving (Bits, Eq);

// The same linkage at a struct field place.
function Bit#(2) fieldGuard(S s);
   return (case (s) matches
              S { flag: .f, v: .* } &&& f: 0;
              S { flag: False, v: .* }: 1;
           endcase);
endfunction

endpackage
