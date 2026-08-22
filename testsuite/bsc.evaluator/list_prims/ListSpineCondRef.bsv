import FIFOF::*;
import List::*;

// Reference implementation of sysListSpineCond using explicit head/tail
// recursion instead of the list primitives.  Must produce identical
// sorted output to sysListSpineCond.out.expected.

function List#(a) appendRef(List#(a) xs, List#(a) ys);
  if (isNull(xs))
    return ys;
  else
    return Cons(List::head(xs), appendRef(List::tail(xs), ys));
endfunction

function List#(a) concatRef(List#(List#(a)) xss);
  if (isNull(xss))
    return Nil;
  else
    return appendRef(List::head(xss), concatRef(List::tail(xss)));
endfunction

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

function Integer lengthRef(List#(a) xs);
  if (isNull(xs))
    return 0;
  else
    return 1 + lengthRef(List::tail(xs));
endfunction

(* synthesize *)
module sysListSpineCondRef(Empty);

  FIFOF#(Bit#(8)) f0 <- mkFIFOF;
  FIFOF#(Bit#(8)) f1 <- mkFIFOF;
  Reg#(Bit#(8)) cycle <- mkReg(0);

  rule fill_0 (cycle[0] == 1);
    f0.enq(zeroExtend(cycle[3:0]));
  endrule
  rule fill_1 (cycle[1] == 1);
    f1.enq(16 + zeroExtend(cycle[3:0]));
  endrule

  Bit#(8) sel = f0.first;

  List#(Bit#(8)) xs = (sel[0] == 1) ? Cons(8'hA0, Cons(8'hA1, Nil))
                                    : Cons(8'hB0, Cons(8'hB1, Nil));
  List#(Bit#(8)) ys = Cons(f1.first, Cons(8'hC1, Nil));

  List#(Bit#(8)) app = appendRef(xs, ys);
  List#(Bit#(8)) cat = concatRef(Cons(xs, Cons(ys, Nil)));
  List#(Bit#(8)) mpd = mapRef(invert, xs);

  rule obs_app0;
    $display("A0 c=%0d v=%0h", cycle, selectRef(app, 0));
    f0.deq;
  endrule

  rule obs_app3;
    $display("A3 c=%0d v=%0h", cycle, selectRef(app, 3));
  endrule

  rule obs_cat2;
    $display("K2 c=%0d v=%0h", cycle, selectRef(cat, 2));
    f1.deq;
  endrule

  rule obs_map1;
    $display("M1 c=%0d v=%0h", cycle, selectRef(mpd, 1));
  endrule

  rule obs_len;
    $display("L c=%0d l=%0d n=%0d", cycle, lengthRef(app), lengthRef(cat));
  endrule

  rule tick;
    cycle <= cycle + 1;
    if (cycle == 32) $finish(0);
  endrule

endmodule
