import Vector::*;
import FIFOF::*;

// Test implicit condition propagation through dynamic select and update.
// Companion: ImplCondDynSelSL.bsv — must produce identical output.
//
// Dynamic select with runtime index creates a mux over all element conditions.
// Dynamic update then static select tests condition propagation through update.

(* synthesize *)
module sysImplCondDynSelVec(Empty);

  Vector#(4, FIFOF#(Bit#(8))) fs <- replicateM(mkFIFOF);
  Reg#(Bit#(8)) cycle <- mkReg(0);
  Reg#(UInt#(2)) idx <- mkReg(0);

  for (Integer i = 0; i < 4; i = i + 1) begin
    rule fill (cycle[i] == 1);
      fs[i].enq(fromInteger(i) * 16 + zeroExtend(cycle[3:0]));
    endrule
  end

  // Dynamic select: result condition depends on which element idx points to
  rule observe_sel;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;

    $display("DS c=%0d i=%0d v=%0h", cycle, idx, vals[idx]);
    fs[idx].deq;
  endrule

  // Dynamic update then static select: update element idx, read element 0
  rule observe_upd;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;

    Vector#(4, Bit#(8)) updated = update(vals, idx, 8'hFF);
    $display("DU c=%0d i=%0d v=%0h", cycle, idx, updated[0]);
  endrule

  // Static update (Integer index) then static select: update element 2, read element 0
  rule observe_supd;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;
    Vector#(4, Bit#(8)) supdated = update(vals, 2, 8'hFF);
    $display("SU c=%0d v=%0h", cycle, supdated[0]);
  endrule

  rule tick;
    cycle <= cycle + 1;
    idx <= idx + 1;
    if (cycle == 32) $finish(0);
  endrule

endmodule
