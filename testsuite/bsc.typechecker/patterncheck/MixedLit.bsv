// Literal patterns with wildcard digits (mixed literals) would need
// mask-aware analysis; matches containing them are not checked, since
// the masks alone can make a match complete.
function Bit#(4) maskComplete(Bit#(4) x);
   return (case (x) matches
              4'b0???: 0;
              4'b1???: 1;
           endcase);
endfunction

function Bit#(4) maskPartial(Bit#(4) x);
   return (case (x) matches
              4'b1?01: 0;
           endcase);
endfunction
