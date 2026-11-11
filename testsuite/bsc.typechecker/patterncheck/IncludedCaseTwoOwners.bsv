package IncludedCaseTwoOwners;

// The same included source text under distinct owners represents two source
// matches; owner-keyed obligation deduplication must retain both warnings.
function Bool includedCase1(Maybe#(Bool) m);
`include "IncludedCaseBody.bsvh"
endfunction

function Bool includedCase2(Maybe#(Bool) m);
`include "IncludedCaseBody.bsvh"
endfunction

endpackage
