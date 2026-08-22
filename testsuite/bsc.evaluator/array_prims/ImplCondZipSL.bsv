import Vector::*;
import FIFOF::*;
import SimpleList::*;

// Test per-element implicit condition propagation through SimpleList zipWith.
// Reference implementation: must produce identical output to ImplCondZipVec.

(* synthesize *)
module sysImplCondZipSL(Empty);

  Vector#(4, FIFOF#(Bit#(8))) fa <- replicateM(mkFIFOF);
  Vector#(4, FIFOF#(Bit#(8))) fb <- replicateM(mkFIFOF);
  Reg#(Bit#(8)) cycle <- mkReg(0);

  for (Integer i = 0; i < 4; i = i + 1) begin
    rule fill_a (cycle[i] == 1);
      fa[i].enq(fromInteger(i) + zeroExtend(cycle[3:0]));
    endrule
    rule fill_b (cycle[(i+2)%4] == 1);
      fb[i].enq(fromInteger(i) * 16 + zeroExtend(cycle[3:0]));
    endrule
  end

  for (Integer i = 0; i < 4; i = i + 1) begin
    rule observe;
      Vector#(4, Bit#(8)) avals = newVector;
      Vector#(4, Bit#(8)) bvals = newVector;
      for (Integer j = 0; j < 4; j = j + 1) begin
        avals[j] = fa[j].first;
        bvals[j] = fb[j].first;
      end

      SimpleList#(Bit#(8)) zipped = slZipWith(\+ , vectorToSL(avals), vectorToSL(bvals));
      $display("Z%0d c=%0d v=%0h", i, cycle, zipped[i]);
      fa[i].deq;
      fb[i].deq;
    endrule
  end

  rule tick;
    cycle <= cycle + 1;
    if (cycle == 32) $finish(0);
  endrule

endmodule
