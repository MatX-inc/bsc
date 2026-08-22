import FIFOF::*;
import List::*;

// Test spine (not per-element) implicit condition propagation through
// primListAppend, primListConcat, and primListMap.  The list structure
// itself is chosen by a condition that carries an implicit condition
// (f0.first), so the spine walk must keep that condition on every node
// it guards.  Must produce identical sorted output to ListSpineCondRef.

(* synthesize *)
module sysListSpineCond(Empty);

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

  // PrimIf spine: the choice of spine depends on a value with an
  // implicit condition (f0.notEmpty).
  List#(Bit#(8)) xs = (sel[0] == 1) ? Cons(8'hA0, Cons(8'hA1, Nil))
                                    : Cons(8'hB0, Cons(8'hB1, Nil));
  // Element condition from f1 on the head of ys.
  List#(Bit#(8)) ys = Cons(f1.first, Cons(8'hC1, Nil));

  List#(Bit#(8)) app = append(xs, ys);
  List#(Bit#(8)) cat = concat(Cons(xs, Cons(ys, Nil)));
  List#(Bit#(8)) mpd = map(invert, xs);

  // Element 0 of append: a mux over the xs branches, so it needs
  // f0's condition (via the mux selector).
  rule obs_app0;
    $display("A0 c=%0d v=%0h", cycle, app[0]);
    f0.deq;
  endrule

  // Element 3 of append: both xs branches have equal shape, so the
  // spine merges and this pure ys element needs no condition.
  rule obs_app3;
    $display("A3 c=%0d v=%0h", cycle, app[3]);
  endrule

  // Element 2 of concat: head of ys, needs both f0's (spine) and f1's
  // (element) conditions.
  rule obs_cat2;
    $display("K2 c=%0d v=%0h", cycle, cat[2]);
    f1.deq;
  endrule

  // Element 1 of map over the conditional spine: needs f0's condition.
  rule obs_map1;
    $display("M1 c=%0d v=%0h", cycle, mpd[1]);
  endrule

  // length walks the whole (merged) spine: both branches have length
  // 2 so the count is static and unconditional.
  rule obs_len;
    $display("L c=%0d l=%0d n=%0d", cycle, length(app), length(cat));
  endrule

  rule tick;
    cycle <= cycle + 1;
    if (cycle == 32) $finish(0);
  endrule

endmodule
