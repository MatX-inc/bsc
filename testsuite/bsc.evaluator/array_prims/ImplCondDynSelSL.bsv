import Vector::*;
import FIFOF::*;
import SimpleList::*;

// Test implicit condition propagation through SimpleList dynamic select/update.
// Reference implementation: must produce identical output to ImplCondDynSelVec.

(* synthesize *)
module sysImplCondDynSelSL(Empty);

  Vector#(4, FIFOF#(Bit#(8))) fs <- replicateM(mkFIFOF);
  Reg#(Bit#(8)) cycle <- mkReg(0);
  Reg#(UInt#(2)) idx <- mkReg(0);

  for (Integer i = 0; i < 4; i = i + 1) begin
    rule fill (cycle[i] == 1);
      fs[i].enq(fromInteger(i) * 16 + zeroExtend(cycle[3:0]));
    endrule
  end

  rule observe_sel;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;
    SimpleList#(Bit#(8)) tvals = vectorToSL(vals);

    $display("DS c=%0d i=%0d v=%0h", cycle, idx, tvals[idx]);
    fs[idx].deq;
  endrule

  rule observe_upd;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;
    SimpleList#(Bit#(8)) tvals = vectorToSL(vals);

    SimpleList#(Bit#(8)) updated = slUpdate(tvals, idx, 8'hFF);
    $display("DU c=%0d i=%0d v=%0h", cycle, idx, updated[0]);
  endrule

  // Static update (Integer index) then static select via SimpleList
  rule observe_supd;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;
    SimpleList#(Bit#(8)) tvals_s = vectorToSL(vals);
    SimpleList#(Bit#(8)) supdated = slStaticUpdate(tvals_s, 2, 8'hFF);
    $display("SU c=%0d v=%0h", cycle, supdated[0]);
  endrule

  rule tick;
    cycle <= cycle + 1;
    idx <= idx + 1;
    if (cycle == 32) $finish(0);
  endrule

endmodule
