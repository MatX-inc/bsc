package IncludedCaseSameOwner;

// These are two distinct case expressions despite having the same included
// source syntax, position and enclosing definition owner.  Both must warn.
function Tuple2#(Bool, Bool) includedCaseSameOwner(Maybe#(Bool) m);
   return tuple2(
`include "IncludedCaseExpr.bsvh"
      ,
`include "IncludedCaseExpr.bsvh"
   );
endfunction

endpackage
