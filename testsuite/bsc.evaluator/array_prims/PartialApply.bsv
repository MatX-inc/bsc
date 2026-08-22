import Vector::*;

// Test partial application of array primitives.
// Each primitive that takes >1 value arg must handle being partially applied
// (falling through to bldAp' rather than crashing in the do* handler).
// Each partial application is used more than once to verify reusability.

function Bit#(8) inc(Bit#(8) x);
  return x + 1;
endfunction

function Bit#(8) addPair(Bit#(8) a, Bit#(8) b);
  return a + b;
endfunction

function Bit#(8) sumFn(Bit#(8) acc, Bit#(8) x);
  return acc + x;
endfunction

(* synthesize *)
module sysPartialApply(Empty);

  Vector#(4, Bit#(8)) v1 = genWith(fromInteger);
  Vector#(4, Bit#(8)) v2 = replicate(10);

  // --- Partial application of map ---
  let mapInc = map(inc);
  Vector#(4, Bit#(8)) m1 = mapInc(v1);
  Vector#(4, Bit#(8)) m2 = mapInc(v2);

  // --- Partial application of foldl ---
  let sumOf = foldl(sumFn, 0);
  Bit#(8) s1 = sumOf(v1);
  Bit#(8) s2 = sumOf(v2);

  // --- Partial application of foldr ---
  let xorOf = foldr(\^ , 0);
  Bit#(8) x1 = xorOf(v1);
  Bit#(8) x2 = xorOf(v2);

  // --- Partial application of zipWith ---
  let addVecs = zipWith(addPair);
  Vector#(4, Bit#(8)) z1 = addVecs(v1, v2);
  Vector#(4, Bit#(8)) z2 = addVecs(v2, v2);

  // --- Partial application of append ---
  Vector#(2, Bit#(8)) va = replicate(100);
  Vector#(2, Bit#(8)) vb = replicate(200);
  let appendVa = append(va);
  Vector#(4, Bit#(8)) a1 = appendVa(vb);
  Vector#(4, Bit#(8)) a2 = appendVa(replicate(50));

  rule test;
    $display("map1:    %0d %0d %0d %0d", m1[0], m1[1], m1[2], m1[3]);
    $display("map2:    %0d %0d %0d %0d", m2[0], m2[1], m2[2], m2[3]);
    $display("foldl:   %0d %0d", s1, s2);
    $display("foldr:   %0d %0d", x1, x2);
    $display("zip1:    %0d %0d %0d %0d", z1[0], z1[1], z1[2], z1[3]);
    $display("zip2:    %0d %0d %0d %0d", z2[0], z2[1], z2[2], z2[3]);
    $display("append1: %0d %0d %0d %0d", a1[0], a1[1], a1[2], a1[3]);
    $display("append2: %0d %0d %0d %0d", a2[0], a2[1], a2[2], a2[3]);
    $finish(0);
  endrule

endmodule
