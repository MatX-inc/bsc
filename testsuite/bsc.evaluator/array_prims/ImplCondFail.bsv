import Vector::*;
import FIFOF::*;

// Compile-fail: implicit conditions must flow through every primitive.
// Each rule uses (* no_implicit_conditions *) and reads FIFOF values
// through a different primitive. If any primitive drops the condition,
// that rule would wrongly pass the assertion — reducing the error count.
//
// Expected: 6 G0005 errors (one per rule).

(* synthesize *)
module sysImplCondFail(Empty);

  Vector#(4, FIFOF#(Bit#(8))) fs <- replicateM(mkFIFOF);
  Vector#(4, FIFOF#(Bit#(8))) gs <- replicateM(mkFIFOF);
  Reg#(UInt#(2)) idx <- mkReg(0);
  Reg#(Bit#(8)) out <- mkRegU;
  Reg#(Bool) outb <- mkRegU;

  // Through map
  (* no_implicit_conditions *)
  rule r_map;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;
    out <= map(invert, vals)[0];
  endrule

  // Through foldr
  (* no_implicit_conditions *)
  rule r_fold;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;
    out <= foldr(\^ , 0, vals);
  endrule

  // Through zipWith
  (* no_implicit_conditions *)
  rule r_zip;
    Vector#(4, Bit#(8)) avals = newVector;
    Vector#(4, Bit#(8)) bvals = newVector;
    for (Integer j = 0; j < 4; j = j + 1) begin
      avals[j] = fs[j].first;
      bvals[j] = gs[j].first;
    end
    out <= zipWith(\+ , avals, bvals)[0];
  endrule

  // Through any
  (* no_implicit_conditions *)
  rule r_any;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;
    function Bool isNonZero(Bit#(8) x) = (x != 0);
    outb <= any(isNonZero, vals);
  endrule

  // Through dynamic select
  (* no_implicit_conditions *)
  rule r_dynsel;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;
    out <= vals[idx];
  endrule

  // Through dynamic update
  (* no_implicit_conditions *)
  rule r_update;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;
    out <= update(vals, idx, 8'hFF)[1];
  endrule

  // Container predicate tests: the array itself carries a condition via _when_.
  // These test addPredG in withNormalizedArray, which all array primitives use.

  // Container through map
  (* no_implicit_conditions *)
  rule r_container_map;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fromInteger(j) * 16;
    out <= map(invert, when(fs[0].notEmpty, vals))[0];
  endrule

  // Container through foldr
  (* no_implicit_conditions *)
  rule r_container_foldr;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fromInteger(j) * 16;
    out <= foldr(\^ , 0, when(fs[0].notEmpty, vals));
  endrule

  // Container through foldl
  (* no_implicit_conditions *)
  rule r_container_foldl;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fromInteger(j) * 16;
    out <= foldl(\^ , 0, when(fs[0].notEmpty, vals));
  endrule

  // Container through zipWith (both inputs)
  (* no_implicit_conditions *)
  rule r_container_zip;
    Vector#(4, Bit#(8)) avals = newVector;
    Vector#(4, Bit#(8)) bvals = newVector;
    for (Integer j = 0; j < 4; j = j + 1) begin
      avals[j] = fromInteger(j) * 16;
      bvals[j] = fromInteger(j) * 32;
    end
    out <= zipWith(\+ , when(fs[0].notEmpty, avals), when(fs[1].notEmpty, bvals))[0];
  endrule

endmodule
