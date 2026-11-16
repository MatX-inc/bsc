package SuperclassInstanceMissing_Classes;

typeclass Super#(type a);
   function Bool superMeth(a x);
endtypeclass

typeclass Sub#(type a) provisos (Super#(a));
   function Bool subMeth(a x);
endtypeclass

endpackage
