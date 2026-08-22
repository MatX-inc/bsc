import List::*;
import Assert::*;

// Value semantics of the list primitives and the List functions that
// route through them, checked at elaboration.  Order-sensitive
// encodings pin element order, traversal direction and argument order.

function Integer add3(Integer x) = x + 3;
function Integer encL(Integer acc, Integer x) = acc * 100 + x;
function Integer encR(Integer x, Integer acc) = acc * 100 + x;
function Integer tenAB(Integer a, Integer b) = a * 10 + b;
function Bool gt2(Integer x) = x > 2;

(* synthesize *)
module sysListValueSemantics();

  List#(Integer) nilI = Nil;
  List#(Integer) l123 = Cons(1, Cons(2, Cons(3, Nil)));
  List#(Integer) l45  = Cons(4, Cons(5, Nil));

  // ---- map: values, order, length ----
  List#(Integer) m = List::map(add3, l123);
  staticAssert(length(m) == 3, "map length");
  staticAssert(m[0] == 4 && m[1] == 5 && m[2] == 6, "map values");
  staticAssert(length(List::map(add3, nilI)) == 0, "map empty");

  // ---- append: first argument first ----
  List#(Integer) ap = List::append(l123, l45);
  staticAssert(length(ap) == 5, "append length");
  staticAssert(ap[0] == 1 && ap[1] == 2 && ap[2] == 3 &&
               ap[3] == 4 && ap[4] == 5, "append order");
  List#(Integer) apn = List::append(nilI, l45);
  staticAssert(apn[0] == 4 && apn[1] == 5, "append empty left");
  List#(Integer) apm = List::append(l45, nilI);
  staticAssert(apm[0] == 4 && apm[1] == 5, "append empty right");

  // ---- concat: outer order, embedded empty lists ----
  List#(Integer) cc = List::concat(Cons(l123, Cons(nilI, Cons(l45, Nil))));
  staticAssert(length(cc) == 5, "concat length");
  staticAssert(cc[0] == 1 && cc[1] == 2 && cc[2] == 3 &&
               cc[3] == 4 && cc[4] == 5, "concat order");

  // ---- length ----
  staticAssert(length(nilI) == 0, "length empty");
  staticAssert(length(l123) == 3, "length");

  // ---- select: every index ----
  staticAssert(l123[0] == 1 && l123[1] == 2 && l123[2] == 3, "select values");
  staticAssert(primListSelect(l123, 2) == 3, "primListSelect value");

  // ---- fold directions ----
  staticAssert(List::foldl(encL, 1, l123) == 1010203, "foldl order");
  staticAssert(List::foldr(encR, 1, l123) == 1030201, "foldr order");
  staticAssert(List::foldl1(encL, l123) == 10203, "foldl1 order");
  staticAssert(List::foldr1(encR, l123) == 30201, "foldr1 order");
  staticAssert(List::foldl(encL, 42, nilI) == 42, "foldl empty");
  staticAssert(List::foldr(encR, 42, nilI) == 42, "foldr empty");

  // ---- zipWith: pairing, argument order, min length ----
  List#(Integer) z1 = List::zipWith(tenAB, l123, l45);
  staticAssert(length(z1) == 2, "zipWith min length");
  staticAssert(z1[0] == 14 && z1[1] == 25, "zipWith values");
  List#(Integer) z2 = List::zipWith(tenAB, l45, l123);
  staticAssert(z2[0] == 41 && z2[1] == 52, "zipWith swapped values");
  staticAssert(length(List::zipWith(tenAB, nilI, l45)) == 0, "zipWith empty");

  // ---- zip / unzip ----
  List#(Tuple2#(Integer, Integer)) zp = List::zip(l123, l45);
  staticAssert(length(zp) == 2, "zip length");
  staticAssert(tpl_1(zp[1]) == 2 && tpl_2(zp[1]) == 5, "zip values");
  Tuple2#(List#(Integer), List#(Integer)) uz = List::unzip(zp);
  staticAssert(tpl_1(uz)[1] == 2 && tpl_2(uz)[1] == 5, "unzip values");

  // ---- update (drives listPrimNum: zipWith against an infinite list) ----
  List#(Integer) up = List::update(l123, 1, 99);
  staticAssert(up[0] == 1 && up[1] == 99 && up[2] == 3, "update values");
  staticAssert(length(up) == 3, "update length");

  // ---- reverse ----
  List#(Integer) rv = List::reverse(l123);
  staticAssert(rv[0] == 3 && rv[1] == 2 && rv[2] == 1, "reverse values");

  // ---- elem / any / all ----
  staticAssert(List::elem(2, l123), "elem present");
  staticAssert(!List::elem(9, l123), "elem absent");
  staticAssert(List::any(gt2, l123), "any true");
  staticAssert(!List::all(gt2, l123), "all false");
  staticAssert(List::all(gt2, l45), "all true");

endmodule
