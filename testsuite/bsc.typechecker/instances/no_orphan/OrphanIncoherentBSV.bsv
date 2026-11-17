package OrphanIncoherentBSV;

import Real::*;

// "incoherent" keeps its meaning next to "orphan", in either order:
// the eqLiteral functions are accepted only because the classes are
// incoherent

typeclass incoherent orphan EqIntegerFirst#(type a);
   function Bool eqIntegerFirst(a x, Integer i);
endtypeclass

instance EqIntegerFirst#(a) provisos (Literal#(a), Eq#(a));
   function eqIntegerFirst(x, i) = x == fromInteger(i);
endinstance

instance EqIntegerFirst#(Real);
   function eqIntegerFirst(r, i) = trunc(r) == i;
endinstance

function Bool eqLiteralFirst(a x, Integer i) provisos (Literal#(a), Eq#(a));
   return eqIntegerFirst(x, i);
endfunction

typeclass orphan incoherent EqIntegerSecond#(type a);
   function Bool eqIntegerSecond(a x, Integer i);
endtypeclass

instance EqIntegerSecond#(a) provisos (Literal#(a), Eq#(a));
   function eqIntegerSecond(x, i) = x == fromInteger(i);
endinstance

instance EqIntegerSecond#(Real);
   function eqIntegerSecond(r, i) = trunc(r) == i;
endinstance

function Bool eqLiteralSecond(a x, Integer i) provisos (Literal#(a), Eq#(a));
   return eqIntegerSecond(x, i);
endfunction

endpackage
