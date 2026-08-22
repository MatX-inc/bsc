import Vector::*;
import SimpleList::*;

// Test implicit condition propagation through array primitives
// by comparing Vector (uses array primitives) with SimpleList (pure recursion).
//
// Pattern: 2D structure with dynamic row select, then apply operations
// to the selected row. Both representations should compile and produce
// equivalent hardware.

(* synthesize *)
module sysImplCondTest(Empty);

  // 2D vector of registers: 4 rows of 4 elements
  Vector#(4, Vector#(4, Reg#(Bit#(8)))) regs2d <- replicateM(replicateM(mkRegU));

  // Dynamic row index
  Reg#(UInt#(2)) rowIdx <- mkReg(0);

  // Second set for zip tests
  Vector#(4, Reg#(Bit#(8))) regs1d <- replicateM(mkRegU);

  // Output registers to force hardware generation
  Reg#(Bit#(8)) out_map_vec <- mkRegU;
  Reg#(Bit#(8)) out_map_tl  <- mkRegU;

  Reg#(Bit#(8)) out_fold_vec <- mkRegU;
  Reg#(Bit#(8)) out_fold_tl  <- mkRegU;

  Reg#(Bit#(8)) out_zip_vec <- mkRegU;
  Reg#(Bit#(8)) out_zip_tl  <- mkRegU;

  Reg#(Bool) out_any_vec <- mkRegU;
  Reg#(Bool) out_any_tl  <- mkRegU;

  Reg#(Bool) out_all_vec <- mkRegU;
  Reg#(Bool) out_all_tl  <- mkRegU;

  Reg#(Bool) out_elem_vec <- mkRegU;
  Reg#(Bool) out_elem_tl  <- mkRegU;

  // ---- Test: map on dynamically selected row ----
  rule rl_test_map;
    // Dynamic select creates container predicate on the row
    Vector#(4, Bit#(8)) row_vec = readVReg(regs2d[rowIdx]);
    SimpleList#(Bit#(8))     row_tl  = vectorToSL(row_vec);

    // Map: invert each element
    Vector#(4, Bit#(8)) mapped_vec = map(invert, row_vec);
    SimpleList#(Bit#(8))     mapped_tl  = slMap(invert, row_tl);

    // Extract element 0 from each
    out_map_vec <= mapped_vec[0];
    out_map_tl  <= mapped_tl[0];
  endrule

  // ---- Test: fold on dynamically selected row ----
  rule rl_test_fold;
    Vector#(4, Bit#(8)) row_vec = readVReg(regs2d[rowIdx]);
    SimpleList#(Bit#(8))     row_tl  = vectorToSL(row_vec);

    // Fold: XOR all elements
    Bit#(8) folded_vec = fold(\^ , row_vec);
    Bit#(8) folded_tl  = slFoldr(\^ , 0, row_tl);

    out_fold_vec <= folded_vec;
    out_fold_tl  <= folded_tl;
  endrule

  // ---- Test: zip on dynamically selected row ----
  rule rl_test_zip;
    Vector#(4, Bit#(8)) row_vec = readVReg(regs2d[rowIdx]);
    SimpleList#(Bit#(8))     row_tl  = vectorToSL(row_vec);

    Vector#(4, Bit#(8)) other_vec = readVReg(regs1d);
    SimpleList#(Bit#(8))     other_tl  = vectorToSL(other_vec);

    // ZipWith: add corresponding elements
    Vector#(4, Bit#(8)) zipped_vec = zipWith(\+ , row_vec, other_vec);
    SimpleList#(Bit#(8))     zipped_tl  = slZipWith(\+ , row_tl, other_tl);

    out_zip_vec <= zipped_vec[0];
    out_zip_tl  <= zipped_tl[0];
  endrule

  // ---- Test: any on dynamically selected row ----
  rule rl_test_any;
    Vector#(4, Bit#(8)) row_vec = readVReg(regs2d[rowIdx]);
    SimpleList#(Bit#(8))     row_tl  = vectorToSL(row_vec);

    function Bool isNonZero(Bit#(8) x) = (x != 0);

    Bool any_vec = any(isNonZero, row_vec);
    Bool any_tl  = slAny(isNonZero, row_tl);

    out_any_vec <= any_vec;
    out_any_tl  <= any_tl;
  endrule

  // ---- Test: all on dynamically selected row ----
  rule rl_test_all;
    Vector#(4, Bit#(8)) row_vec = readVReg(regs2d[rowIdx]);
    SimpleList#(Bit#(8))     row_tl  = vectorToSL(row_vec);

    function Bool isEven(Bit#(8) x) = (x[0] == 0);

    Bool all_vec = all(isEven, row_vec);
    Bool all_tl  = slAll(isEven, row_tl);

    out_all_vec <= all_vec;
    out_all_tl  <= all_tl;
  endrule

  // ---- Test: elem on dynamically selected row ----
  rule rl_test_elem;
    Vector#(4, Bit#(8)) row_vec = readVReg(regs2d[rowIdx]);
    SimpleList#(Bit#(8))     row_tl  = vectorToSL(row_vec);

    Bool elem_vec = elem(8'h42, row_vec);
    Bool elem_tl  = slElem(8'h42, row_tl);

    out_elem_vec <= elem_vec;
    out_elem_tl  <= elem_tl;
  endrule

  // ---- Test: dynamic select and update (imperative desugaring) ----
  Reg#(Bit#(8)) out_dynsel_vec <- mkRegU;
  Reg#(Bit#(8)) out_dynsel_tl  <- mkRegU;
  Reg#(Bit#(8)) out_dynupd_vec <- mkRegU;
  Reg#(Bit#(8)) out_dynupd_tl  <- mkRegU;

  Reg#(UInt#(2)) elemIdx <- mkReg(0);

  rule rl_test_dynsel;
    Vector#(4, Bit#(8)) row_vec = readVReg(regs2d[rowIdx]);
    SimpleList#(Bit#(8))     row_tl  = vectorToSL(row_vec);

    // Dynamic element select within dynamically selected row (2D dynamic)
    out_dynsel_vec <= row_vec[elemIdx];
    out_dynsel_tl  <= row_tl[elemIdx];

    // Dynamic update then select
    Vector#(4, Bit#(8)) upd_vec = update(row_vec, elemIdx, 8'hFF);
    SimpleList#(Bit#(8))     upd_tl  = slUpdate(row_tl, elemIdx, 8'hFF);

    out_dynupd_vec <= upd_vec[0];
    out_dynupd_tl  <= upd_tl[0];
  endrule

  // Bump indices to make things interesting
  rule rl_tick;
    rowIdx <= rowIdx + 1;
    elemIdx <= elemIdx + 1;
  endrule

endmodule
