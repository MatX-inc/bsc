package NoOrphanClassesBSV;

// Typeclasses not declared "orphan", with and without a coherence annotation

typeclass NoOrphanBSV#(type a);
   function Bool noOrphanBSV(a x);
endtypeclass

typeclass coherent CoherentNoOrphanBSV#(type a);
   function Bool coherentNoOrphanBSV(a x);
endtypeclass

typeclass incoherent IncoherentNoOrphanBSV#(type a);
   function Bool incoherentNoOrphanBSV(a x);
endtypeclass

// An instance in the class's own package is not an orphan
instance NoOrphanBSV#(Bool);
   function noOrphanBSV(x) = x;
endinstance

endpackage
