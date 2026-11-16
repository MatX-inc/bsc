// A literal pattern outside the type's domain is only diagnosed at
// elaboration; the checker must abandon the analysis rather than
// draw wrong conclusions from it.
function Bit#(2) f(Bit#(1) x);
   return (case (x) matches
              0: 2'd1;
              1: 2'd2;
              5: 2'd3;
           endcase);
endfunction

function Bit#(2) g(Bit#(1) x);
   return (case (x) matches
              0: 2'd0;
              5: 2'd1;
              .*: 2'd2;
           endcase);
endfunction

// widths too large to enumerate are treated as unbounded (and must
// not blow up compile time building a 2^n-sized domain)
function Bit#(2) h(Bit#(1000000) x);
   return (case (x) matches
              0: 2'd1;
              .*: 2'd2;
           endcase);
endfunction
