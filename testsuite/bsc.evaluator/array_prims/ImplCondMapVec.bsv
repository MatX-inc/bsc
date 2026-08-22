import Vector::*;
import FIFOF::*;

// Test per-element implicit condition propagation through Vector map.
// Companion: ImplCondMapSL.bsv (same test using SimpleList — must produce identical output).
//
// Source vector built via imperative assignment (fs[i].first), not map.
// Only the map-under-test uses array primitives.

(* synthesize *)
module sysImplCondMapVec(Empty);

  Vector#(4, FIFOF#(Bit#(8))) fs <- replicateM(mkFIFOF);
  Reg#(Bit#(8)) cycle <- mkReg(0);

  for (Integer i = 0; i < 4; i = i + 1) begin
    rule fill (cycle[i] == 1);
      fs[i].enq(fromInteger(i) * 16 + zeroExtend(cycle[3:0]));
    endrule
  end

  for (Integer i = 0; i < 4; i = i + 1) begin
    rule observe;
      // Imperative construction — each element carries only its FIFO's condition
      Vector#(4, Bit#(8)) vals = newVector;
      for (Integer j = 0; j < 4; j = j + 1)
        vals[j] = fs[j].first;

      // Primitive under test
      Vector#(4, Bit#(8)) mapped = map(invert, vals);

      $display("M%0d c=%0d v=%0h", i, cycle, mapped[i]);
      fs[i].deq;
    endrule
  end

  rule tick;
    cycle <= cycle + 1;
    if (cycle == 32) $finish(0);
  endrule

endmodule
