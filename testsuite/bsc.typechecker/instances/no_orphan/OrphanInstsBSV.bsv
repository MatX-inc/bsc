package OrphanInstsBSV;

import OrphanClassesBSV::*;

// Each of these is an orphan instance of a typeclass declared orphan,
// which is allowed with a warning (T0127)

instance OrphanBSV#(UInt#(n));
   function orphanBSV(x) = x == 0;
endinstance

instance CoherentOrphanBSV#(UInt#(n));
   function coherentOrphanBSV(x) = x == 0;
endinstance

instance OrphanCoherentBSV#(UInt#(n));
   function orphanCoherentBSV(x) = x == 0;
endinstance

instance IncoherentOrphanBSV#(UInt#(n));
   function incoherentOrphanBSV(x) = x == 0;
endinstance

instance OrphanIncoherentBSV#(UInt#(n));
   function orphanIncoherentBSV(x) = x == 0;
endinstance

endpackage
