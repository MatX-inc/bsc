import List::*;
import FIFOF::*;

// Compile-fail: implicit conditions must survive list<->array conversion.
// Each rule uses (* no_implicit_conditions *) and reads FIFOF values
// through primListToArray / primArrayToList / combined paths.
// If any conversion drops the condition, the error count decreases.
//
// Expected: 5 G0005 errors (one per rule).

(* synthesize *)
module sysImplCondListConvert(Empty);

  FIFOF#(Bit#(8)) f0 <- mkFIFOF;
  FIFOF#(Bit#(8)) f1 <- mkFIFOF;
  FIFOF#(Bit#(8)) f2 <- mkFIFOF;
  FIFOF#(Bit#(8)) f3 <- mkFIFOF;
  FIFOF#(Bit#(8)) g0 <- mkFIFOF;
  FIFOF#(Bit#(8)) g1 <- mkFIFOF;
  FIFOF#(Bit#(8)) g2 <- mkFIFOF;
  FIFOF#(Bit#(8)) g3 <- mkFIFOF;
  Reg#(Bit#(8)) out <- mkRegU;

  // Through primListToArray then foldl (touches all elements)
  (* no_implicit_conditions *)
  rule r_listToArray;
    List#(Bit#(8)) lv = Cons(f0.first, Cons(f1.first, Cons(f2.first, Cons(f3.first, Nil))));
    out <= primArrayFoldL(\^ , 0, primListToArray(lv));
  endrule

  // Through round-trip: list -> array -> list, then select
  (* no_implicit_conditions *)
  rule r_roundtrip;
    List#(Bit#(8)) lv = Cons(f0.first, Cons(f1.first, Cons(f2.first, Cons(f3.first, Nil))));
    List#(Bit#(8)) lv2 = primArrayToList(primListToArray(lv));
    out <= lv2[0];
  endrule

  // Through list -> array -> map -> list
  (* no_implicit_conditions *)
  rule r_listMapRoundtrip;
    List#(Bit#(8)) lv = Cons(f0.first, Cons(f1.first, Cons(f2.first, Cons(f3.first, Nil))));
    List#(Bit#(8)) mapped = primArrayToList(primArrayMap(invert, primListToArray(lv)));
    out <= mapped[0];
  endrule

  // Through list -> array -> foldl
  (* no_implicit_conditions *)
  rule r_listFoldRoundtrip;
    List#(Bit#(8)) lv = Cons(f0.first, Cons(f1.first, Cons(f2.first, Cons(f3.first, Nil))));
    out <= primArrayFoldL(\^ , 0, primListToArray(lv));
  endrule

  // Through list -> array -> zipWith -> list
  (* no_implicit_conditions *)
  rule r_listZipRoundtrip;
    List#(Bit#(8)) la = Cons(f0.first, Cons(f1.first, Cons(f2.first, Cons(f3.first, Nil))));
    List#(Bit#(8)) lb = Cons(g0.first, Cons(g1.first, Cons(g2.first, Cons(g3.first, Nil))));
    List#(Bit#(8)) zipped = primArrayToList(primArrayZipWith(\+ , primListToArray(la), primListToArray(lb)));
    out <= zipped[0];
  endrule

endmodule
