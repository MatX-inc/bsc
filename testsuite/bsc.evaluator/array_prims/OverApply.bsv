import Vector::*;

// Test over-application of array primitives with polymorphic return types.
// When select/foldl/foldr return a function, trailing args must be forwarded.
// Tests both direct over-application (arg in same spine) and stored reuse.

function Bit#(8) addOne(Bit#(8) x);
  return x + 1;
endfunction

function Bit#(8) timesTwo(Bit#(8) x);
  return x * 2;
endfunction

function Bit#(8) addTen(Bit#(8) x);
  return x + 10;
endfunction

typedef function Bit#(8) f(Bit#(8) x) Fn;

function Fn mkFn(Integer i);
  case (i)
    0: return addOne;
    1: return timesTwo;
    2: return addTen;
    default: return addOne;
  endcase
endfunction

function Fn composeLR(Fn acc, Fn f);
  return compose(f, acc);
endfunction

function Fn composeRL(Fn f, Fn acc);
  return compose(f, acc);
endfunction

(* synthesize *)
module sysOverApply(Empty);

  Vector#(3, Fn) fns = genWith(mkFn);
  Vector#(3, Fn) ops = genWith(mkFn);

  // --- Direct over-application: extra arg on the same spine ---
  Bit#(8) sel_d1  = (select(fns, 1))(10);
  Bit#(8) sel_d2  = (select(fns, 1))(7);
  Bit#(8) foldl_d1 = (foldl(composeLR, id, ops))(5);
  Bit#(8) foldl_d2 = (foldl(composeLR, id, ops))(0);
  Bit#(8) foldr_d1 = (foldr(composeRL, id, ops))(5);
  Bit#(8) foldr_d2 = (foldr(composeRL, id, ops))(0);

  // --- Stored result applied multiple times ---
  Fn sel_fn = select(fns, 1);
  Bit#(8) sel_s1 = sel_fn(10);
  Bit#(8) sel_s2 = sel_fn(7);

  Fn composed = foldl(composeLR, id, ops);
  Bit#(8) foldl_s1 = composed(5);
  Bit#(8) foldl_s2 = composed(0);

  Fn composedR = foldr(composeRL, id, ops);
  Bit#(8) foldr_s1 = composedR(5);
  Bit#(8) foldr_s2 = composedR(0);

  rule test;
    $display("direct:  %0d %0d %0d %0d %0d %0d", sel_d1, sel_d2, foldl_d1, foldl_d2, foldr_d1, foldr_d2);
    $display("select:  %0d %0d", sel_s1, sel_s2);
    $display("foldl:   %0d %0d", foldl_s1, foldl_s2);
    $display("foldr:   %0d %0d", foldr_s1, foldr_s2);
    $finish(0);
  endrule

endmodule
