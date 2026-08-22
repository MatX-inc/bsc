import Vector::*;
import FIFOF::*;

// Test implicit condition propagation through Vector fold/any/all/elem.
// Companion: ImplCondFoldSL.bsv — must produce identical output.
//
// All these operations touch every element, so the rule fires only when
// ALL FIFOFs have data.

(* synthesize *)
module sysImplCondFoldVec(Empty);

  Vector#(4, FIFOF#(Bit#(8))) fs <- replicateM(mkFIFOF);
  Reg#(Bit#(8)) cycle <- mkReg(0);

  for (Integer i = 0; i < 4; i = i + 1) begin
    rule fill (cycle[i] == 1);
      fs[i].enq(fromInteger(i) * 16 + zeroExtend(cycle[3:0]));
    endrule
  end

  rule observe;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;

    Bit#(8) r = foldr(\^ , 0, vals);
    Bit#(8) l = foldl(\^ , 0, vals);

    function Bool isNonZero(Bit#(8) x) = (x != 0);
    function Bool isEven(Bit#(8) x) = (x[0] == 0);
    Bool a = any(isNonZero, vals);
    Bool b = all(isEven, vals);
    Bool c = elem(8'h00, vals);

    $display("FR c=%0d v=%0h", cycle, r);
    $display("FL c=%0d v=%0h", cycle, l);
    $display("AN c=%0d v=%b", cycle, a);
    $display("AL c=%0d v=%b", cycle, b);
    $display("EL c=%0d v=%b", cycle, c);

    for (Integer j = 0; j < 4; j = j + 1)
      fs[j].deq;
  endrule

  rule tick;
    cycle <= cycle + 1;
    if (cycle == 32) $finish(0);
  endrule

endmodule
