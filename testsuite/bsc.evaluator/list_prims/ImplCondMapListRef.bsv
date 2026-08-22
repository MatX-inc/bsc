import FIFOF::*;
import List::*;

// Reference implementation of map using explicit head/tail recursion.
// Must produce identical sorted output to sysImplCondMapList.out.expected.
// Validates that implicit conditions propagate through BSC's own recursive
// evaluation, not just through primListMap.

function List#(b) mapRef(function b f(a x), List#(a) xs);
  if (isNull(xs))
    return Nil;
  else
    return Cons(f(List::head(xs)), mapRef(f, List::tail(xs)));
endfunction

function a selectRef(List#(a) xs, Integer n);
  if (n == 0)
    return List::head(xs);
  else
    return selectRef(List::tail(xs), n - 1);
endfunction

(* synthesize *)
module sysImplCondMapListRef(Empty);

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

  List#(Bit#(8)) vals = Cons(f0.first, Cons(f1.first, Cons(f2.first, Cons(f3.first, Nil))));
  List#(Bit#(8)) mapped = mapRef(invert, vals);

  rule observe_0;
    $display("M0 c=%0d v=%0h", cycle, selectRef(mapped, 0));
    f0.deq;
  endrule

  rule observe_1;
    $display("M1 c=%0d v=%0h", cycle, selectRef(mapped, 1));
    f1.deq;
  endrule

  rule observe_2;
    $display("M2 c=%0d v=%0h", cycle, selectRef(mapped, 2));
    f2.deq;
  endrule

  rule observe_3;
    $display("M3 c=%0d v=%0h", cycle, selectRef(mapped, 3));
    f3.deq;
  endrule

  rule tick;
    cycle <= cycle + 1;
    if (cycle == 32) $finish(0);
  endrule

endmodule
