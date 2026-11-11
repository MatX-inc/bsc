// A literal match over a wide Bit type must be analyzed through the
// unbounded path.  The finite path reasons with bit-cubes whose cost
// grows with the column width, so treating a wide domain as finite would
// spend the whole analysis budget on big-integer cube arithmetic (a
// width in this band hung the compiler before the domain was capped).
// The wildcard makes the match exhaustive, so no warning is expected;
// the point of the test is that it completes quickly.
function Bit#(2) f(Bit#(4096) x);
   return (case (x) matches
              0: 2'd0;
              1: 2'd1;
              .*: 2'd2;
           endcase);
endfunction
