import FIFOF::*;
import List::*;

// Reference: zipWith via explicit head/tail recursion (no primitives).
// Must produce identical sorted output to sysImplCondZipList.out.expected.

function List#(c) zipWithRef(function c f(a x, b y), List#(a) xs, List#(b) ys);
  if (isNull(xs) || isNull(ys))
    return Nil;
  else
    return Cons(f(List::head(xs), List::head(ys)),
                zipWithRef(f, List::tail(xs), List::tail(ys)));
endfunction

function a selectRef(List#(a) xs, Integer n);
  if (n == 0) return List::head(xs);
  else        return selectRef(List::tail(xs), n - 1);
endfunction

(* synthesize *)
module sysImplCondZipListRef(Empty);

  FIFOF#(Bit#(8)) fa0 <- mkFIFOF;
  FIFOF#(Bit#(8)) fa1 <- mkFIFOF;
  FIFOF#(Bit#(8)) fa2 <- mkFIFOF;
  FIFOF#(Bit#(8)) fa3 <- mkFIFOF;

  FIFOF#(Bit#(8)) fb0 <- mkFIFOF;
  FIFOF#(Bit#(8)) fb1 <- mkFIFOF;
  FIFOF#(Bit#(8)) fb2 <- mkFIFOF;
  FIFOF#(Bit#(8)) fb3 <- mkFIFOF;

  Reg#(Bit#(8)) cycle <- mkReg(0);

  rule fill_a0 (cycle[0] == 1); fa0.enq(0 + zeroExtend(cycle[3:0])); endrule
  rule fill_a1 (cycle[1] == 1); fa1.enq(1 + zeroExtend(cycle[3:0])); endrule
  rule fill_a2 (cycle[2] == 1); fa2.enq(2 + zeroExtend(cycle[3:0])); endrule
  rule fill_a3 (cycle[3] == 1); fa3.enq(3 + zeroExtend(cycle[3:0])); endrule

  rule fill_b0 (cycle[2] == 1); fb0.enq(0 * 16 + zeroExtend(cycle[3:0])); endrule
  rule fill_b1 (cycle[3] == 1); fb1.enq(1 * 16 + zeroExtend(cycle[3:0])); endrule
  rule fill_b2 (cycle[0] == 1); fb2.enq(2 * 16 + zeroExtend(cycle[3:0])); endrule
  rule fill_b3 (cycle[1] == 1); fb3.enq(3 * 16 + zeroExtend(cycle[3:0])); endrule

  List#(Bit#(8)) avals = Cons(fa0.first, Cons(fa1.first, Cons(fa2.first, Cons(fa3.first, Nil))));
  List#(Bit#(8)) bvals = Cons(fb0.first, Cons(fb1.first, Cons(fb2.first, Cons(fb3.first, Nil))));
  List#(Bit#(8)) zipped = zipWithRef(\+ , avals, bvals);

  rule observe_0;
    $display("Z0 c=%0d v=%0h", cycle, selectRef(zipped, 0));
    fa0.deq; fb0.deq;
  endrule
  rule observe_1;
    $display("Z1 c=%0d v=%0h", cycle, selectRef(zipped, 1));
    fa1.deq; fb1.deq;
  endrule
  rule observe_2;
    $display("Z2 c=%0d v=%0h", cycle, selectRef(zipped, 2));
    fa2.deq; fb2.deq;
  endrule
  rule observe_3;
    $display("Z3 c=%0d v=%0h", cycle, selectRef(zipped, 3));
    fa3.deq; fb3.deq;
  endrule

  rule tick;
    cycle <= cycle + 1;
    if (cycle == 32) $finish(0);
  endrule

endmodule
