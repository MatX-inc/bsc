package NoOrphanTwiceBSV;

// Each annotation may be given at most once
typeclass no_orphan coherent no_orphan NoOrphanTwiceBSV#(type a);
   function Bool noOrphanTwiceBSV(a x);
endtypeclass

endpackage
