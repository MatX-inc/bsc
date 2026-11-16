package IncludedCase;

// Keep the definition position in this file while the case expression's
// position comes from the include file.  Pattern warnings must follow the
// source construct, not require both positions to name the same file.
function Bool includedCase(Maybe#(Bool) m);
`include "IncludedCaseBody.bsvh"
endfunction

endpackage
