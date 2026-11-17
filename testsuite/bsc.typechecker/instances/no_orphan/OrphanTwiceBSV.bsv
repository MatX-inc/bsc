package OrphanTwiceBSV;

// Each annotation may be given at most once
typeclass orphan coherent orphan OrphanTwiceBSV#(type a);
   function Bool orphanTwiceBSV(a x);
endtypeclass

endpackage
