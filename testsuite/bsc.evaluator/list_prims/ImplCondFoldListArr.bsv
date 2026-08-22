import FIFOF::*;

// Array-based reference: convert list -> array, apply primArrayFoldL/R and primArrayMap.
// Must produce identical sorted output to sysImplCondFoldList.out.expected.
// Validates primListToArray condition propagation as a bonus.

function Bool isNonZero(Bit#(8) x) = (x != 0);
function Bool isEven(Bit#(8) x) = (x[0] == 0);
function Bool isMatch(Bit#(8) x) = (x == 8'h00);
function Bool orAcc(Bool acc, Bool x) = acc || x;
function Bool andAcc(Bool acc, Bool x) = acc && x;

(* synthesize *)
module sysImplCondFoldListArr(Empty);

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

    Bit#(8) r = primArrayFoldR(\^ , 0, primListToArray(vals));
    Bit#(8) l = primArrayFoldL(\^ , 0, primListToArray(vals));

    Bool a = primArrayFoldL(orAcc,  False, primArrayMap(isNonZero, primListToArray(vals)));
    Bool b = primArrayFoldL(andAcc, True,  primArrayMap(isEven,    primListToArray(vals)));
    Bool c = primArrayFoldL(orAcc,  False, primArrayMap(isMatch,   primListToArray(vals)));

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
