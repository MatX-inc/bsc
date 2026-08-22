import List::*;

// Taking the length of an undetermined list must report the same
// error the Prelude's listLength raised, not yield a silent
// undetermined Integer.

(* synthesize *)
module sysListLengthUndet(Empty);

  List#(Bit#(8)) u = ?;
  Reg#(Bit#(8)) r <- mkRegU;

  rule go;
    r <= fromInteger(length(u));
  endrule

endmodule
