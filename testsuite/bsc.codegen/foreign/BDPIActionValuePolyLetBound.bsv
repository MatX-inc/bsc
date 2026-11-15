import Vector::*;

import "BDPI" function ActionValue#(Bit#(n)) my_poly_count (Bit#(8) step, Bit#(32) n);

function ActionValue#(Bit#(n)) poly_count(Bit#(8) step) =
   my_poly_count(step, fromInteger(valueOf(n)));

(* synthesize *)
module mkBDPIActionValuePolyLetBound ();
   rule r;
      ActionValue#(Bit#(64)) next = poly_count(1);
      Vector#(2, Bit#(64)) v <- replicateM(next);
      $display("%0d %0d %0d %0d", v[0][31:0], v[0][63:32], v[1][31:0], v[1][63:32]);
      $finish(0);
   endrule
endmodule
