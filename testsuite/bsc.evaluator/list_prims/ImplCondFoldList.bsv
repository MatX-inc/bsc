import FIFOF::*;

// Test implicit condition propagation through primListFoldL/FoldR.
// Must produce identical sorted output to ImplCondFoldVec.
//
// All fold operations touch every element, so the rule fires only when
// ALL FIFOFs have data.

function Bool isNonZero(Bit#(8) x) = (x != 0);
function Bool isEven(Bit#(8) x) = (x[0] == 0);
function Bool isMatch(Bit#(8) x) = (x == 8'h00);
function Bool orAcc(Bool acc, Bool x) = acc || x;
function Bool andAcc(Bool acc, Bool x) = acc && x;

(* synthesize *)
module sysImplCondFoldList(Empty);

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

    Bit#(8) r = primListFoldR(\^ , 0, vals);
    Bit#(8) l = primListFoldL(\^ , 0, vals);

    // any = fold (||) (map p xs)
    Bool a = primListFoldL(orAcc, False, primListMap(isNonZero, vals));
    // all = fold (&&) (map p xs)
    Bool b = primListFoldL(andAcc, True, primListMap(isEven, vals));
    // elem = any (== target) xs
    Bool c = primListFoldL(orAcc, False, primListMap(isMatch, vals));

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
