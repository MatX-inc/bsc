import List::*;
import Assert::*;

// primListZipWith must walk its FIRST list's spine and only consult
// the second list when the first has a Cons: the Prelude's listPrimNum
// (which List::update and dynamic list selection are built on) zips a
// finite list against an infinite index list, so evaluating the second
// spine eagerly would exhaust the steps budget here.

function List#(Integer) nats(Integer n);
  return Cons(n, nats(n + 1));
endfunction

function Integer encXI(Integer x, Integer i) = x * 100 + i;

(* synthesize *)
module sysInfiniteZip();

  List#(Integer) finite = Cons(10, Cons(20, Cons(30, Nil)));

  // direct primitive, infinite second argument
  List#(Integer) z = primListZipWith(encXI, finite, nats(0));
  staticAssert(length(z) == 3, "prim zip infinite length");
  staticAssert(z[0] == 1000 && z[1] == 2001 && z[2] == 3002,
               "prim zip infinite values");

  // library zipWith and zip over the same shape
  List#(Integer) z2 = List::zipWith(encXI, finite, nats(5));
  staticAssert(z2[0] == 1005 && z2[1] == 2006 && z2[2] == 3007,
               "zipWith infinite values");

  List#(Tuple2#(Integer, Integer)) zp = List::zip(finite, nats(1));
  staticAssert(length(zp) == 3, "zip infinite length");
  staticAssert(tpl_1(zp[2]) == 30 && tpl_2(zp[2]) == 3, "zip infinite values");

endmodule
