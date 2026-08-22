import List::*;

function UInt#(8) addOne(UInt#(8) x);
  return x + 1;
endfunction

function UInt#(8) addAcc(UInt#(8) acc, UInt#(8) x);
  return acc + x;
endfunction

(* synthesize *)
module sysListArrayConvert(Empty);
  Reg#(UInt#(8)) cycle <- mkReg(0);

  rule tick;
    cycle <= cycle + 1;
    if (cycle == 1) $finish;
  endrule

  // Test round-trip: list -> array -> list
  List#(UInt#(8)) l1 = List::cons(10, List::cons(20, List::cons(30, List::nil)));
  List#(UInt#(8)) l2 = primArrayToList(primListToArray(l1));

  rule show_roundtrip (cycle == 0);
    $display("roundtrip: %0d %0d %0d", l2[0], l2[1], l2[2]);
  endrule

  // Test: list -> array -> map -> list
  List#(UInt#(8)) l3 = primArrayToList(primArrayMap(addOne, primListToArray(l1)));

  rule show_map (cycle == 0);
    $display("map +1: %0d %0d %0d", l3[0], l3[1], l3[2]);
  endrule

  // Test: list -> array -> foldl
  UInt#(8) sum_val = primArrayFoldL(addAcc, 0, primListToArray(l1));

  rule show_fold (cycle == 0);
    $display("foldl sum: %0d", sum_val);
  endrule

endmodule
