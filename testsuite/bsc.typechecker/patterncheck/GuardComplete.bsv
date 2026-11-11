package GuardComplete;

typedef enum { GA, GB } GuardTag deriving (Bits, Eq);

// Complementary guards cover both valuations of g for the GA arm.
function Bit#(3) complementary(GuardTag tag, Bool g);
   return (case (tag) matches
              GA &&& g: 0;
              GA &&& (!g): 1;
              GB: 2;
           endcase);
endfunction

// Negation of a conjunction is complementary to the conjunction itself.
function Bit#(3) complementaryConjunction(GuardTag tag, Bool g, Bool h);
   return (case (tag) matches
              GA &&& (g && h): 3;
              GA &&& (!(g && h)): 4;
              GB: 5;
           endcase);
endfunction

// Negation of a disjunction exercises the De Morgan product path.
function Bit#(3) complementaryDisjunction(GuardTag tag, Bool g, Bool h);
   return (case (tag) matches
              GA &&& (g || h): 6;
              GA &&& (!(g || h)): 7;
              GB: 0;
           endcase);
endfunction

endpackage
