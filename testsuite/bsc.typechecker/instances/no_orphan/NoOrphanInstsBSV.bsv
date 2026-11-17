package NoOrphanInstsBSV;

import NoOrphanClassesBSV::*;

// Each of these is an orphan instance of a typeclass not declared orphan
// (T0127)

instance NoOrphanBSV#(UInt#(n));
   function noOrphanBSV(x) = x == 0;
endinstance

instance CoherentNoOrphanBSV#(UInt#(n));
   function coherentNoOrphanBSV(x) = x == 0;
endinstance

instance IncoherentNoOrphanBSV#(UInt#(n));
   function incoherentNoOrphanBSV(x) = x == 0;
endinstance

endpackage
