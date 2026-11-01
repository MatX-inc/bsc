package GuardRedundant;

typedef enum { RA, RB } RedundantGuardTag deriving (Bits, Eq);

// The second occurrence of the same pattern and guard is redundant.
function Bit#(3) repeatedGuard(RedundantGuardTag tag, Bool g);
   return (case (tag) matches
              RA &&& g: 0;
              RA &&& g: 1;
              default: 2;
           endcase);
endfunction

// A False guard can never select its arm.
function Bit#(3) falseGuard(RedundantGuardTag tag);
   return (case (tag) matches
              RA &&& False: 3;
              default: 4;
           endcase);
endfunction

// True is equivalent to an unguarded arm and shadows the later guarded arm.
function Bit#(3) trueShadows(RedundantGuardTag tag, Bool g);
   return (case (tag) matches
              RA &&& True: 5;
              RA &&& g: 6;
              RB: 7;
           endcase);
endfunction

// Complementary guards together shadow a later unguarded arm.
function Bit#(3) complementsShadow(RedundantGuardTag tag, Bool g);
   return (case (tag) matches
              RA &&& g: 0;
              RA &&& (!g): 1;
              RA: 2;
              RB: 3;
           endcase);
endfunction

endpackage
