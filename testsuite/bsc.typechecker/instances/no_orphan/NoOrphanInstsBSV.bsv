package NoOrphanInstsBSV;

import NoOrphanClassesBSV::*;

// Each of these is an orphan instance of a no_orphan class (T0127)

instance NoOrphanBSV#(UInt#(n));
   function noOrphanBSV(x) = x == 0;
endinstance

instance CoherentNoOrphanBSV#(UInt#(n));
   function coherentNoOrphanBSV(x) = x == 0;
endinstance

instance NoOrphanCoherentBSV#(UInt#(n));
   function noOrphanCoherentBSV(x) = x == 0;
endinstance

instance IncoherentNoOrphanBSV#(UInt#(n));
   function incoherentNoOrphanBSV(x) = x == 0;
endinstance

instance NoOrphanIncoherentBSV#(UInt#(n));
   function noOrphanIncoherentBSV(x) = x == 0;
endinstance

endpackage
