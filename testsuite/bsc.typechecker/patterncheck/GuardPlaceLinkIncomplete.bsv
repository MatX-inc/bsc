package GuardPlaceLinkIncomplete;

// A guard over a pattern-bound payload covers only one payload value; the
// witness names the exact value that remains uncovered.
function Bit#(2) stillIncomplete(Maybe#(Bool) m);
   return (case (m) matches
              tagged Valid .b &&& b: 0;
              tagged Invalid: 1;
           endcase);
endfunction

endpackage
