import Assert::*;

// Value semantics of the array primitives, checked at elaboration.
// Every assertion uses asymmetric data and order-sensitive encodings
// (acc*100 + x), so element order, traversal direction and argument
// order are all pinned, not just the multiset of results.
// Arrays are built with primListToArray and observed with
// primArrayToList / list selection, which the conversion tests
// (ListArrayConvert) validate independently.

function Integer sq(Integer i) = i * i;
function Integer add3(Integer x) = x + 3;

// order-sensitive fold encodings
function Integer encL(Integer acc, Integer x) = acc * 100 + x;
function Integer encR(Integer x, Integer acc) = acc * 100 + x;

// order-sensitive zip encoding
function Integer tenAB(Integer a, Integer b) = a * 10 + b;

(* synthesize *)
module sysArrayValueSemantics();

  List#(Integer) nilI = Nil;
  List#(Integer) l123 = Cons(1, Cons(2, Cons(3, Nil)));
  List#(Integer) l45  = Cons(4, Cons(5, Nil));

  let a123 = primListToArray(l123);
  let a45  = primListToArray(l45);
  let a0   = primListToArray(nilI);

  // ---- primArrayGenWith: values and order ----
  let g  = primArrayGenWith(5, sq);
  let gl = primArrayToList(g);
  staticAssert(arrayLength(g) == 5, "genWith length");
  staticAssert(gl[0] == 0 && gl[1] == 1 && gl[2] == 4 && gl[3] == 9 && gl[4] == 16,
               "genWith values");

  // ---- primArrayGenWith of size 0 ----
  let g0 = primArrayGenWith(0, sq);
  staticAssert(arrayLength(g0) == 0, "genWith 0 length");

  // ---- primArrayMap: values, order, length ----
  let m  = primArrayMap(add3, a123);
  let ml = primArrayToList(m);
  staticAssert(arrayLength(m) == 3, "map length");
  staticAssert(ml[0] == 4 && ml[1] == 5 && ml[2] == 6, "map values");

  // ---- primArrayFoldL: left-to-right, acc on the left ----
  staticAssert(primArrayFoldL(encL, 1, a123) == 1010203, "foldl order");

  // ---- primArrayFoldR: right-to-left, acc on the right ----
  staticAssert(primArrayFoldR(encR, 1, a123) == 1030201, "foldr order");

  // ---- folds over an empty array return the seed ----
  staticAssert(primArrayFoldL(encL, 42, a0) == 42, "foldl empty");
  staticAssert(primArrayFoldR(encR, 42, a0) == 42, "foldr empty");

  // ---- primArrayZipWith: pairing, argument order, min length ----
  let z1  = primArrayZipWith(tenAB, a123, a45);
  let z1l = primArrayToList(z1);
  staticAssert(arrayLength(z1) == 2, "zipWith min length");
  staticAssert(z1l[0] == 14 && z1l[1] == 25, "zipWith values");

  let z2  = primArrayZipWith(tenAB, a45, a123);
  let z2l = primArrayToList(z2);
  staticAssert(arrayLength(z2) == 2, "zipWith swapped length");
  staticAssert(z2l[0] == 41 && z2l[1] == 52, "zipWith swapped values");

  // ---- primArrayAppend: first argument's elements first ----
  let ap  = primArrayAppend(a123, a45);
  let apl = primArrayToList(ap);
  staticAssert(arrayLength(ap) == 5, "append length");
  staticAssert(apl[0] == 1 && apl[1] == 2 && apl[2] == 3 &&
               apl[3] == 4 && apl[4] == 5, "append order");

  // ---- append with an empty side is the other side ----
  let apn = primArrayToList(primArrayAppend(a0, a45));
  staticAssert(apn[0] == 4 && apn[1] == 5, "append empty left");
  let apm = primArrayToList(primArrayAppend(a45, a0));
  staticAssert(apm[0] == 4 && apm[1] == 5, "append empty right");

  // ---- primArrayConcat: outer order, ragged rows, empty rows ----
  let rows = primListToArray(Cons(a123, Cons(a0, Cons(a45, Nil))));
  let c    = primArrayConcat(rows);
  let cl   = primArrayToList(c);
  staticAssert(arrayLength(c) == 5, "concat ragged length");
  staticAssert(cl[0] == 1 && cl[1] == 2 && cl[2] == 3 &&
               cl[3] == 4 && cl[4] == 5, "concat ragged order");

  // ---- primArrayReverse: odd, single, empty ----
  let rv = primArrayToList(primArrayReverse(a123));
  staticAssert(rv[0] == 3 && rv[1] == 2 && rv[2] == 1, "reverse odd");
  let r1 = primArrayToList(primArrayReverse(primListToArray(Cons(7, Nil))));
  staticAssert(r1[0] == 7, "reverse single");
  staticAssert(arrayLength(primArrayReverse(a0)) == 0, "reverse empty");

  // ---- round trips preserve order ----
  let rt = primArrayToList(primListToArray(l123));
  staticAssert(rt[0] == 1 && rt[1] == 2 && rt[2] == 3, "list-array round trip");

endmodule
