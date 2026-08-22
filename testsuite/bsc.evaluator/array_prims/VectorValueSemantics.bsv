import Vector::*;
import Assert::*;

// Value semantics of the Vector functions that sit on the array
// primitives, checked at elaboration.  Order-sensitive encodings pin
// element order, traversal direction and (for fold) the exact shape of
// the halving tree, which user code relies on for logic depth.

function Integer addOne(Integer i) = i + 1;
function Integer add3(Integer x) = x + 3;
function Integer encL(Integer acc, Integer x) = acc * 100 + x;
function Integer encR(Integer x, Integer acc) = acc * 100 + x;
function Integer encT(Integer a, Integer b) = a * 1000 + b;
function Integer f3(Integer a, Integer b, Integer c) = a * 100 + b * 10 + c;
function Bool gt2(Integer x) = x > 2;

(* synthesize *)
module sysVectorValueSemantics();

  // genWith: [1,2,3]
  Vector#(3, Integer) v123 = genWith(addOne);
  staticAssert(v123[0] == 1 && v123[1] == 2 && v123[2] == 3, "genWith values");

  Vector#(3, Integer) v456 = map(add3, v123);
  staticAssert(v456[0] == 4 && v456[1] == 5 && v456[2] == 6, "map values");

  Vector#(3, Integer) v789 = map(add3, v456);

  // ---- fold directions ----
  staticAssert(foldl(encL, 1, v123) == 1010203, "foldl order");
  staticAssert(foldr(encR, 1, v123) == 1030201, "foldr order");
  staticAssert(foldl1(encL, v123) == 10203, "foldl1 order");
  staticAssert(foldr1(encR, v123) == 30201, "foldr1 order");

  // folds over an empty vector return the seed
  Vector#(0, Integer) v0 = nil;
  staticAssert(foldl(encL, 42, v0) == 42, "foldl empty");
  staticAssert(foldr(encR, 42, v0) == 42, "foldr empty");

  // ---- fold: balanced halving tree shape ----
  Vector#(1, Integer) w1 = cons(9, nil);
  staticAssert(fold(encT, w1) == 9, "fold 1");
  Vector#(2, Integer) w2 = cons(1, cons(2, nil));
  staticAssert(fold(encT, w2) == 1002, "fold 2");
  staticAssert(fold(encT, v123) == 1002003, "fold 3");
  Vector#(5, Integer) w5 = cons(1, cons(2, cons(3, cons(4, cons(5, nil)))));
  staticAssert(fold(encT, w5) == 1005004005, "fold 5");

  // ---- scans: full contents, length n+1 ----
  Vector#(4, Integer) sl = scanl(encL, 5, v123);
  staticAssert(sl[0] == 5 && sl[1] == 501 && sl[2] == 50102 && sl[3] == 5010203,
               "scanl values");
  Vector#(4, Integer) sr = scanr(encR, 5, v123);
  staticAssert(sr[0] == 5030201 && sr[1] == 50302 && sr[2] == 503 && sr[3] == 5,
               "scanr values");

  // ---- append: first argument first ----
  Vector#(2, Integer) v45x = cons(4, cons(5, nil));
  Vector#(5, Integer) ap = append(v123, v45x);
  staticAssert(ap[0] == 1 && ap[1] == 2 && ap[2] == 3 && ap[3] == 4 && ap[4] == 5,
               "append order");

  // ---- concat: row-major, first row first ----
  Vector#(2, Vector#(3, Integer)) vv = cons(v123, cons(v456, nil));
  Vector#(6, Integer) cc = concat(vv);
  staticAssert(cc[0] == 1 && cc[1] == 2 && cc[2] == 3 &&
               cc[3] == 4 && cc[4] == 5 && cc[5] == 6, "concat order");

  // ---- reverse ----
  Vector#(3, Integer) rv = reverse(v123);
  staticAssert(rv[0] == 3 && rv[1] == 2 && rv[2] == 1, "reverse values");

  // ---- take from the front, drop from the front ----
  Vector#(2, Integer) tk = take(v123);
  staticAssert(tk[0] == 1 && tk[1] == 2, "take values");
  Vector#(2, Integer) dr = drop(v123);
  staticAssert(dr[0] == 2 && dr[1] == 3, "drop values");

  // ---- zip family ----
  Vector#(3, Tuple2#(Integer, Integer)) zp = zip(v123, v456);
  staticAssert(tpl_1(zp[1]) == 2 && tpl_2(zp[1]) == 5, "zip values");

  Tuple2#(Vector#(3, Integer), Vector#(3, Integer)) uz = unzip(zp);
  staticAssert(tpl_1(uz)[2] == 3 && tpl_2(uz)[2] == 6, "unzip values");

  Vector#(3, Integer) zw3 = zipWith3(f3, v123, v456, v789);
  staticAssert(zw3[0] == 147 && zw3[1] == 258 && zw3[2] == 369, "zipWith3 values");

  // ---- elem / any / all, including the empty cases ----
  staticAssert(elem(2, v123), "elem present");
  staticAssert(!elem(9, v123), "elem absent");
  staticAssert(any(gt2, v123), "any true");
  staticAssert(!any(gt2, tk), "any false");
  staticAssert(all(gt2, v456), "all true");
  staticAssert(!all(gt2, v123), "all false");
  staticAssert(!elem(1, v0), "elem empty");
  staticAssert(!any(gt2, v0), "any empty");
  staticAssert(all(gt2, v0), "all empty");

  // ---- pack/unpack: element 0 in the low bits ----
  Vector#(3, Bit#(8)) vb = cons(8'h01, cons(8'h02, cons(8'h03, nil)));
  staticAssert(pack(vb) == 24'h030201, "pack layout");
  Vector#(3, Bit#(8)) vu = unpack(24'h0A0B0C);
  staticAssert(vu[0] == 8'h0C && vu[1] == 8'h0B && vu[2] == 8'h0A, "unpack layout");

  // ---- toList preserves order ----
  List#(Integer) tl = toList(v123);
  staticAssert(tl[0] == 1 && tl[1] == 2 && tl[2] == 3, "toList order");

endmodule
