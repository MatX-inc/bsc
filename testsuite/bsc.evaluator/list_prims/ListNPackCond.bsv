import ListN::*;
import FIFOF::*;

// ListN's pack routes through primListMap/primListConcat; the implicit
// condition on an element must survive into the packed value.

(* synthesize *)
module sysListNPackCond(Empty);

  FIFOF#(Bit#(8)) f0 <- mkFIFOF;
  Reg#(Bit#(16)) r <- mkRegU;

  ListN#(2, Bit#(8)) ln = cons(f0.first, cons(8'h1, nil));

  (* no_implicit_conditions *)
  rule go;
    r <= pack(ln);
  endrule

endmodule
