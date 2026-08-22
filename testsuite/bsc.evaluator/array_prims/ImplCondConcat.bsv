import Vector::*;
import SimpleList::*;

// Test the PAClib/dft64 pattern:
// Dynamic 2D select -> zip -> concat -> operations on result
// This is the pattern that caused PrimIntegerLT 63 (PrimWhenPred _ 64)

(* synthesize *)
module sysImplCondConcat(Empty);

  // 2D structure: 4 groups of 4 registers
  Vector#(4, Vector#(4, Reg#(Bit#(8)))) regs2d <- replicateM(replicateM(mkRegU));

  Reg#(UInt#(2)) rowIdx <- mkReg(0);

  Reg#(Bit#(8)) out_vec <- mkRegU;
  Reg#(Bit#(8)) out_tl  <- mkRegU;

  rule rl_test;
    // Dynamically select a row (creates implicit condition on container)
    Vector#(4, Bit#(8)) row_vec = readVReg(regs2d[rowIdx]);
    SimpleList#(Bit#(8))     row_tl  = vectorToSL(row_vec);

    // Generate an index vector (static, no predicates)
    Vector#(4, Bit#(8)) idxs_vec = genWith(fromInteger);
    SimpleList#(Bit#(8))     idxs_tl  = slGenWith(4, fromInteger);

    // Zip the dynamically-selected row with static indices
    // This is the pattern from PAClib's attach_indexes_from_base
    Vector#(4, Tuple2#(Bit#(8), Bit#(8))) zipped_vec = zip(row_vec, idxs_vec);
    SimpleList#(Tuple2#(Bit#(8), Bit#(8)))     zipped_tl  = slZip(row_tl, idxs_tl);

    // Access the zipped result (exercises arrayLength on zip output)
    out_vec <= tpl_1(zipped_vec[0]) + tpl_2(zipped_vec[3]);
    out_tl  <= tpl_1(zipped_tl[0])  + tpl_2(zipped_tl[3]);
  endrule

  rule rl_tick;
    rowIdx <= rowIdx + 1;
  endrule

endmodule
