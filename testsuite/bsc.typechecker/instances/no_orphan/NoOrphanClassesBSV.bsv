package NoOrphanClassesBSV;

// "no_orphan" alone, and in either order with a coherence annotation

typeclass no_orphan NoOrphanBSV#(type a);
   function Bool noOrphanBSV(a x);
endtypeclass

typeclass coherent no_orphan CoherentNoOrphanBSV#(type a);
   function Bool coherentNoOrphanBSV(a x);
endtypeclass

typeclass no_orphan coherent NoOrphanCoherentBSV#(type a);
   function Bool noOrphanCoherentBSV(a x);
endtypeclass

typeclass incoherent no_orphan IncoherentNoOrphanBSV#(type a);
   function Bool incoherentNoOrphanBSV(a x);
endtypeclass

typeclass no_orphan incoherent NoOrphanIncoherentBSV#(type a);
   function Bool noOrphanIncoherentBSV(a x);
endtypeclass

// An instance in the class's own package is not an orphan
instance NoOrphanBSV#(Bool);
   function noOrphanBSV(x) = x;
endinstance

endpackage
