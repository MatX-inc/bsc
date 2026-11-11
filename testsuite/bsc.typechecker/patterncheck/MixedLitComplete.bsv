// A mixed literal constrains only its known low slices.  The high four bits
// are unconstrained, so these two masks cover all 256 values of Bit#(8).
function Bool maskComplete(Bit#(8) x);
   return (case (x) matches
              4'b0???: False;
              4'b1???: True;
           endcase);
endfunction

// The masks overlap, but the second arm still has values not covered by the
// first (bit 3 is one and bit 2 is zero).
function Bit#(2) maskPartialOverlapUseful(Bit#(4) x);
   return (case (x) matches
              4'b0???: 0;
              4'b?0??: 1;
              default: 2;
           endcase);
endfunction

// Coverage in the second column is correlated with the first-column mask.
function Bool maskMultiColumn(Bit#(4) x, Bool b);
   return (case (tuple2(x, b)) matches
              { 4'b0???, False }: False;
              { 4'b0???, True  }: True;
              { 4'b1???, .*    }: b;
           endcase);
endfunction
