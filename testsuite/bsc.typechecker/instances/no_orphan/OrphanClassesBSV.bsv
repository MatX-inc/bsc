package OrphanClassesBSV;

// "orphan" alone, and in either order with a coherence annotation

typeclass orphan OrphanBSV#(type a);
   function Bool orphanBSV(a x);
endtypeclass

typeclass coherent orphan CoherentOrphanBSV#(type a);
   function Bool coherentOrphanBSV(a x);
endtypeclass

typeclass orphan coherent OrphanCoherentBSV#(type a);
   function Bool orphanCoherentBSV(a x);
endtypeclass

typeclass incoherent orphan IncoherentOrphanBSV#(type a);
   function Bool incoherentOrphanBSV(a x);
endtypeclass

typeclass orphan incoherent OrphanIncoherentBSV#(type a);
   function Bool orphanIncoherentBSV(a x);
endtypeclass

endpackage
