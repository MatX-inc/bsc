import Vector::*;
import FIFOF::*;

// Compile-fail: implicit conditions must survive list<->array conversion.
//
// Tests:
//   A. list→array: element, spine, and container conditions
//   B. array→list: element and container conditions
//   C. Round-trips: list→array→list, array→list→array
//   D. Cross-primitive: list→array→arrayMap→arrayToList and vice versa
//
// Rules marked "PASS" should compile without error.
// Rules marked "FAIL" should produce G0005.
//
// Expected: 15 G0005 errors.

(* synthesize *)
module sysImplCondConvertFail(Empty);

  FIFOF#(Bit#(8)) f0 <- mkFIFOF;
  FIFOF#(Bit#(8)) f1 <- mkFIFOF;
  Vector#(4, FIFOF#(Bit#(8))) fs <- replicateM(mkFIFOF);
  Reg#(Bit#(8)) out <- mkRegU;

  // ===== A. list→array: element conditions (1 error) =====

  // FAIL: element condition flows into array, accessed via foldl
  (* no_implicit_conditions *)
  rule r_l2a_elem;
    List#(Bit#(8)) lv = Cons(f0.first, Cons(f1.first, Nil));
    out <= primArrayFoldL(\^ , 0, primListToArray(lv));
  endrule

  // ===== A. list→array: spine condition (2 errors) =====

  // FAIL: spine condition at pos 1 — foldl touches all, sees it
  (* no_implicit_conditions *)
  rule r_l2a_spine_fold;
    List#(Bit#(8)) lv = Cons(10, when(f0.notEmpty, Cons(20, Nil)));
    out <= primArrayFoldL(\^ , 0, primListToArray(lv));
  endrule

  // FAIL: spine condition becomes array container condition, visible via foldl
  (* no_implicit_conditions *)
  rule r_l2a_spine_select;
    List#(Bit#(8)) lv = Cons(10, when(f0.notEmpty, Cons(20, Nil)));
    out <= primArrayFoldL(\^ , 0, primListToArray(lv));
  endrule

  // ===== A. list→array: container condition (1 error) =====

  // FAIL: container condition on list becomes array container condition
  (* no_implicit_conditions *)
  rule r_l2a_container;
    List#(Bit#(8)) lv = when(f0.notEmpty, Cons(10, Cons(20, Nil)));
    out <= primArrayFoldL(\^ , 0, primListToArray(lv));
  endrule

  // ===== B. array→list: element conditions (2 errors) =====

  // FAIL: element condition in array cell survives to list, accessed via select
  (* no_implicit_conditions *)
  rule r_a2l_elem_select;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;
    List#(Bit#(8)) lv = primArrayToList(vectorToArray(vals));
    out <= primListSelect(lv, 0);
  endrule

  // FAIL: element condition in array cell survives to list, accessed via foldl
  (* no_implicit_conditions *)
  rule r_a2l_elem_fold;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;
    List#(Bit#(8)) lv = primArrayToList(vectorToArray(vals));
    out <= primListFoldL(\^ , 0, lv);
  endrule

  // ===== B. array→list: container condition (1 error) =====

  // FAIL: container condition on array survives to list
  (* no_implicit_conditions *)
  rule r_a2l_container;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fromInteger(j);
    List#(Bit#(8)) lv = primArrayToList(when(f0.notEmpty, vectorToArray(vals)));
    out <= primListSelect(lv, 0);
  endrule

  // ===== C. Round-trip: list→array→list (4 errors) =====
  // Note: list→array flattens spine conditions into the array container
  // condition, so after round-trip, a spine condition at position k becomes
  // visible at ALL positions (not just k and later).

  // FAIL: element conditions survive round-trip
  (* no_implicit_conditions *)
  rule r_rt_l2a2l_elem;
    List#(Bit#(8)) lv = Cons(f0.first, Cons(f1.first, Nil));
    List#(Bit#(8)) lv2 = primArrayToList(primListToArray(lv));
    out <= primListSelect(lv2, 0);
  endrule

  // FAIL: spine conditions survive round-trip, visible at spine position
  (* no_implicit_conditions *)
  rule r_rt_l2a2l_spine_at;
    List#(Bit#(8)) lv = Cons(10, when(f0.notEmpty, Cons(20, Nil)));
    List#(Bit#(8)) lv2 = primArrayToList(primListToArray(lv));
    out <= primListSelect(lv2, 1);
  endrule

  // FAIL: spine condition flattened to container by round-trip, visible everywhere
  (* no_implicit_conditions *)
  rule r_rt_l2a2l_spine_before;
    List#(Bit#(8)) lv = Cons(10, when(f0.notEmpty, Cons(20, Nil)));
    List#(Bit#(8)) lv2 = primArrayToList(primListToArray(lv));
    out <= primListSelect(lv2, 0);
  endrule

  // FAIL: container condition survives round-trip
  (* no_implicit_conditions *)
  rule r_rt_l2a2l_container;
    List#(Bit#(8)) lv = when(f0.notEmpty, Cons(10, Cons(20, Nil)));
    List#(Bit#(8)) lv2 = primArrayToList(primListToArray(lv));
    out <= primListSelect(lv2, 0);
  endrule

  // ===== D. Cross-primitive: list prims through array, array prims through list (4 errors) =====

  // FAIL: list→array→arrayMap→arrayToList preserves element conditions
  (* no_implicit_conditions *)
  rule r_cross_l_arraymap;
    List#(Bit#(8)) lv = Cons(f0.first, Cons(f1.first, Nil));
    List#(Bit#(8)) result = primArrayToList(primArrayMap(invert, primListToArray(lv)));
    out <= primListSelect(result, 0);
  endrule

  // FAIL: array→list→listMap→listToArray preserves element conditions
  (* no_implicit_conditions *)
  rule r_cross_a_listmap;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fs[j].first;
    out <= primArrayFoldL(\^ , 0, primListToArray(primListMap(invert, primArrayToList(vectorToArray(vals)))));
  endrule

  // FAIL: list→array→arrayFoldL preserves spine conditions
  (* no_implicit_conditions *)
  rule r_cross_l_arrayfold_spine;
    List#(Bit#(8)) lv = Cons(10, when(f0.notEmpty, Cons(20, Nil)));
    out <= primArrayFoldL(\^ , 0, primListToArray(lv));
  endrule

  // FAIL: array→list→listFoldL preserves container conditions
  (* no_implicit_conditions *)
  rule r_cross_a_listfold_container;
    Vector#(4, Bit#(8)) vals = newVector;
    for (Integer j = 0; j < 4; j = j + 1)
      vals[j] = fromInteger(j);
    out <= primListFoldL(\^ , 0, primArrayToList(when(f0.notEmpty, vectorToArray(vals))));
  endrule

endmodule
