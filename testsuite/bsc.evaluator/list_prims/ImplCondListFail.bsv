import FIFOF::*;

// Compile-fail: implicit conditions must flow through list primitives.
//
// Tests three kinds of conditions:
//   1. Element conditions (FIFO first as list element value)
//   2. Spine conditions (when on a Cons cell in the middle of the list)
//   3. Container conditions (when on the whole list)
//
// For select, a spine condition at position k should appear at index >= k
// but NOT at index < k.
//
// Expected: G0005 error count given in each section.

(* synthesize *)
module sysImplCondListFail(Empty);

  FIFOF#(Bit#(8)) f0 <- mkFIFOF;
  FIFOF#(Bit#(8)) f1 <- mkFIFOF;
  FIFOF#(Bit#(8)) f2 <- mkFIFOF;
  FIFOF#(Bit#(8)) f3 <- mkFIFOF;
  Reg#(Bit#(8)) out <- mkRegU;

  // ===== Element conditions through primitives (4 errors) =====

  // Through primListMap (element conditions)
  (* no_implicit_conditions *)
  rule r_elem_map;
    List#(Bit#(8)) lv = Cons(f0.first, Cons(f1.first, Cons(f2.first, Cons(f3.first, Nil))));
    List#(Bit#(8)) mapped = primListMap(invert, lv);
    out <= primListSelect(mapped, 0);
  endrule

  // Through primListFoldL (element conditions)
  (* no_implicit_conditions *)
  rule r_elem_foldl;
    List#(Bit#(8)) lv = Cons(f0.first, Cons(f1.first, Cons(f2.first, Cons(f3.first, Nil))));
    out <= primListFoldL(\^ , 0, lv);
  endrule

  // Through primListFoldR (element conditions)
  (* no_implicit_conditions *)
  rule r_elem_foldr;
    List#(Bit#(8)) lv = Cons(f0.first, Cons(f1.first, Cons(f2.first, Cons(f3.first, Nil))));
    out <= primListFoldR(\^ , 0, lv);
  endrule

  // Through primListSelect (element conditions)
  (* no_implicit_conditions *)
  rule r_elem_select;
    List#(Bit#(8)) lv = Cons(f0.first, Cons(f1.first, Cons(f2.first, Cons(f3.first, Nil))));
    out <= primListSelect(lv, 2);
  endrule

  // Through primListZipWith (element conditions from both sources)
  (* no_implicit_conditions *)
  rule r_elem_zipwith;
    List#(Bit#(8)) la = Cons(f0.first, Cons(f1.first, Nil));
    List#(Bit#(8)) lb = Cons(f2.first, Cons(f3.first, Nil));
    List#(Bit#(8)) zipped = primListZipWith(\+ , la, lb);
    out <= primListSelect(zipped, 0);
  endrule

  // ===== Spine condition at position 2 (3 errors) =====
  // List: Cons(10, Cons(20, when(f0.notEmpty, Cons(30, Cons(40, Nil)))))
  // Select at 0 or 1 should NOT have the condition.
  // Select at 2 or 3 SHOULD have the condition.

  // Select before spine condition — should PASS (no error)
  (* no_implicit_conditions *)
  rule r_spine_select_before;
    List#(Bit#(8)) lv = Cons(10, Cons(20, when(f0.notEmpty, Cons(30, Cons(40, Nil)))));
    out <= primListSelect(lv, 1);
  endrule

  // Select at spine condition — should FAIL
  (* no_implicit_conditions *)
  rule r_spine_select_at;
    List#(Bit#(8)) lv = Cons(10, Cons(20, when(f0.notEmpty, Cons(30, Cons(40, Nil)))));
    out <= primListSelect(lv, 2);
  endrule

  // Select after spine condition — should FAIL
  (* no_implicit_conditions *)
  rule r_spine_select_after;
    List#(Bit#(8)) lv = Cons(10, Cons(20, when(f0.notEmpty, Cons(30, Cons(40, Nil)))));
    out <= primListSelect(lv, 3);
  endrule

  // Length with spine condition — should FAIL
  (* no_implicit_conditions *)
  rule r_spine_length;
    List#(Bit#(8)) lv = Cons(10, Cons(20, when(f0.notEmpty, Cons(30, Cons(40, Nil)))));
    Integer len = primListLength(lv);
    out <= fromInteger(len);
  endrule

  // ZipWith with spine condition in one source — should FAIL at that position
  (* no_implicit_conditions *)
  rule r_spine_zipwith;
    List#(Bit#(8)) la = Cons(10, Cons(20, when(f0.notEmpty, Cons(30, Nil))));
    List#(Bit#(8)) lb = Cons(1,  Cons(2,  Cons(3,  Nil)));
    List#(Bit#(8)) zipped = primListZipWith(\+ , la, lb);
    out <= primListSelect(zipped, 2);
  endrule

  // ===== Map with spine condition (1 error) =====
  // Selecting element 0 of mapped list should NOT have spine condition at 2.
  // Selecting element 2 of mapped list SHOULD have it.

  // Map + select before spine condition — should PASS (no error)
  (* no_implicit_conditions *)
  rule r_spine_map_before;
    List#(Bit#(8)) lv = Cons(10, Cons(20, when(f0.notEmpty, Cons(30, Cons(40, Nil)))));
    List#(Bit#(8)) mapped = primListMap(invert, lv);
    out <= primListSelect(mapped, 0);
  endrule

  // Map + select at spine condition — should FAIL
  (* no_implicit_conditions *)
  rule r_spine_map_at;
    List#(Bit#(8)) lv = Cons(10, Cons(20, when(f0.notEmpty, Cons(30, Cons(40, Nil)))));
    List#(Bit#(8)) mapped = primListMap(invert, lv);
    out <= primListSelect(mapped, 2);
  endrule

  // ===== Container condition (2 errors) =====
  // The whole list is wrapped in when — every operation should see it.

  // FoldL with container condition — should FAIL
  (* no_implicit_conditions *)
  rule r_container_foldl;
    List#(Bit#(8)) lv = when(f0.notEmpty, Cons(10, Cons(20, Cons(30, Cons(40, Nil)))));
    out <= primListFoldL(\^ , 0, lv);
  endrule

  // Select with container condition — should FAIL (even at index 0)
  (* no_implicit_conditions *)
  rule r_container_select;
    List#(Bit#(8)) lv = when(f0.notEmpty, Cons(10, Cons(20, Cons(30, Cons(40, Nil)))));
    out <= primListSelect(lv, 0);
  endrule

endmodule
