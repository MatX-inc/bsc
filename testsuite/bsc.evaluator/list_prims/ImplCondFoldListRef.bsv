import FIFOF::*;
import List::*;

// Reference implementation of foldl/foldr using explicit head/tail recursion.
// Must produce identical sorted output to sysImplCondFoldList.out.expected.
// Validates that implicit conditions propagate through BSC's own recursive
// evaluation, not just through primListFoldL/R.

function Bool isNonZero(Bit#(8) x) = (x != 0);
function Bool isEven(Bit#(8) x) = (x[0] == 0);
function Bool isMatch(Bit#(8) x) = (x == 8'h00);
function Bool orAcc(Bool acc, Bool x) = acc || x;
function Bool andAcc(Bool acc, Bool x) = acc && x;

function b foldlRef(function b f(b acc, a x), b init, List#(a) xs);
  if (isNull(xs))
    return init;
  else
    return foldlRef(f, f(init, List::head(xs)), List::tail(xs));
endfunction

function b foldrRef(function b f(a x, b acc), b init, List#(a) xs);
  if (isNull(xs))
    return init;
  else
    return f(List::head(xs), foldrRef(f, init, List::tail(xs)));
endfunction

function List#(b) mapRef(function b f(a x), List#(a) xs);
  if (isNull(xs))
    return Nil;
  else
    return Cons(f(List::head(xs)), mapRef(f, List::tail(xs)));
endfunction

(* synthesize *)
module sysImplCondFoldListRef(Empty);

  FIFOF#(Bit#(8)) f0 <- mkFIFOF;
  FIFOF#(Bit#(8)) f1 <- mkFIFOF;
  FIFOF#(Bit#(8)) f2 <- mkFIFOF;
  FIFOF#(Bit#(8)) f3 <- mkFIFOF;
  Reg#(Bit#(8)) cycle <- mkReg(0);

  rule fill_0 (cycle[0] == 1);
    f0.enq(0 * 16 + zeroExtend(cycle[3:0]));
  endrule
  rule fill_1 (cycle[1] == 1);
    f1.enq(1 * 16 + zeroExtend(cycle[3:0]));
  endrule
  rule fill_2 (cycle[2] == 1);
    f2.enq(2 * 16 + zeroExtend(cycle[3:0]));
  endrule
  rule fill_3 (cycle[3] == 1);
    f3.enq(3 * 16 + zeroExtend(cycle[3:0]));
  endrule

  rule observe;
    List#(Bit#(8)) vals = Cons(f0.first, Cons(f1.first, Cons(f2.first, Cons(f3.first, Nil))));

    Bit#(8) r = foldrRef(\^ , 0, vals);
    Bit#(8) l = foldlRef(\^ , 0, vals);

    Bool a = foldlRef(orAcc,  False, mapRef(isNonZero, vals));
    Bool b = foldlRef(andAcc, True,  mapRef(isEven,    vals));
    Bool c = foldlRef(orAcc,  False, mapRef(isMatch,   vals));

    $display("FR c=%0d v=%0h", cycle, r);
    $display("FL c=%0d v=%0h", cycle, l);
    $display("AN c=%0d v=%b", cycle, a);
    $display("AL c=%0d v=%b", cycle, b);
    $display("EL c=%0d v=%b", cycle, c);

    f0.deq;
    f1.deq;
    f2.deq;
    f3.deq;
  endrule

  rule tick;
    cycle <= cycle + 1;
    if (cycle == 32) $finish(0);
  endrule

endmodule
