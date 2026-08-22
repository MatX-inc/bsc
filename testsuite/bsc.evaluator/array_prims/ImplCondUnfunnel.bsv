import Vector::*;
import SimpleList::*;

// Reproduce the PAClib mkUnfunnel pattern:
// - Vector of Reg of Vector, read via readVReg
// - concat of the result
// - method with explicit guard
// - Dynamic index comparison

interface TestIfc;
  method Vector#(16, Bit#(8)) getData();
endinterface

(* synthesize *)
module sysImplCondUnfunnel(TestIfc);

  // 4 groups of 4 elements each, stored in registers
  Vector#(4, Reg#(Vector#(4, Bit#(8)))) values <- replicateM(mkRegU);

  // Index register with CReg-like behavior simulated with two regs
  Reg#(UInt#(3)) index_wr <- mkReg(0);
  Reg#(UInt#(3)) index_rd <- mkReg(0);

  UInt#(3) k = 4;

  rule rl_receive (index_wr != k);
    // Write to values[index_wr]
    values[index_wr] <= replicate(zeroExtend(pack(index_wr)));
    index_wr <= index_wr + 1;
  endrule

  // Method with guard — the guard creates implicit condition on result
  method Vector#(16, Bit#(8)) getData() if (index_rd == k);
    Vector#(4, Vector#(4, Bit#(8))) ys = readVReg(values);
    Vector#(16, Bit#(8)) result = concat(ys);
    return result;
  endmethod

endmodule

// Test that exercises the method
(* synthesize *)
module sysImplCondUnfunnelTest(Empty);

  TestIfc dut <- sysImplCondUnfunnel;

  Reg#(Bit#(8)) out <- mkRegU;

  rule rl_use;
    Vector#(16, Bit#(8)) d = dut.getData();
    out <= d[0] ^ d[15];
  endrule

endmodule
