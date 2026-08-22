import Vector::*;
import SimpleList::*;

// Test with sizes matching PAClib/dft64 patterns
// The error was: PrimIntegerLT 63 (PrimWhenPred Integer _ 64)
// suggesting a 64-element context

(* synthesize *)
module sysImplCondLarge(Empty);

  // 4 groups of 16 elements = 64 total (like 64-point DFT unfolded as 4x16)
  Vector#(4, Vector#(16, Reg#(Bit#(8)))) regs2d <- replicateM(replicateM(mkRegU));

  Reg#(UInt#(2)) rowIdx <- mkReg(0);

  Reg#(Bit#(8)) out_zip <- mkRegU;
  Reg#(Bool) out_any <- mkRegU;
  Reg#(Bool) out_all <- mkRegU;
  Reg#(Bool) out_elem <- mkRegU;

  rule rl_test;
    // Dynamic row select (creates container predicate)
    Vector#(16, Bit#(8)) row = readVReg(regs2d[rowIdx]);

    // zip with static index vector
    Vector#(16, Bit#(8)) idxs = genWith(fromInteger);
    Vector#(16, Tuple2#(Bit#(8), Bit#(8))) zipped = zip(row, idxs);
    out_zip <= tpl_1(zipped[0]) + tpl_2(zipped[15]);

    // any
    function Bool isNonZero(Bit#(8) x) = (x != 0);
    out_any <= any(isNonZero, row);

    // all
    function Bool isEven(Bit#(8) x) = (x[0] == 0);
    out_all <= all(isEven, row);

    // elem
    out_elem <= elem(8'h42, row);
  endrule

  rule rl_tick;
    rowIdx <= rowIdx + 1;
  endrule

endmodule
