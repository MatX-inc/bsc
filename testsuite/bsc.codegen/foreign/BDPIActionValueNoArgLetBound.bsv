import Vector::*;

import "BDPI" function ActionValue#(Bit#(32)) my_next ();

(* synthesize *)
module mkBDPIActionValueNoArgLetBound ();
   rule r;
      ActionValue#(Bit#(32)) next = my_next;
      Vector#(3, Bit#(32)) v <- replicateM(next);
      $display("%0d %0d %0d", v[0], v[1], v[2]);
      $finish(0);
   endrule
endmodule
