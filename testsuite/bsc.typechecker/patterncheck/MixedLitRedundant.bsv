// 3 is inside 0???, while 8 is not.
function Bit#(2) exactUnderMask(Bit#(8) x);
   return (case (x) matches
              4'b0???: 0;
              8'd3: 1;
              8'd8: 2;
              default: 3;
           endcase);
endfunction

// These three masks cover the domain, making the final all-wild mask dead.
function Bit#(2) maskUnionComplete(Bit#(4) x);
   return (case (x) matches
              4'b0???: 0;
              4'b10??: 1;
              4'b11??: 2;
              4'b????: 3;
           endcase);
endfunction

// Pin hex mixed-literal rendering in the redundant-row diagnostic.  The
// second mask is a subset of the first and must print with both '?' digits.
function Bool hexSubmask(Bit#(16) x);
   return (case (x) matches
              16'h0???: False;
              16'h00??: True;
              default: True;
           endcase);
endfunction

// And likewise pin octal rendering (one wildcard digit, not zero).
function Bool octalSubmask(Bit#(9) x);
   return (case (x) matches
              9'o0??: False;
              9'o00?: True;
              default: True;
           endcase);
endfunction
