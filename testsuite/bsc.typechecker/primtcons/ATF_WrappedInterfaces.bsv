// Type functions can produce an entire boundary interface, a subinterface,
// or an ordinary method result.  All three use the same normalization path.
typeclass View#(type tag, type result)
  dependencies (tag determines result);
  type ViewOf#(type tag) = result;
endtypeclass

instance View#(Bool, WriteOnly#(Bit#(3)));
endinstance

instance View#(Bit#(1), Bit#(5));
endinstance

import "BVI" ForeignATF =
module mkImportedATF (ViewOf#(Bool));
  default_clock clk(CLK);
  default_reset rst(RST_N);
  method _write(IN) enable(EN);
  schedule _write C _write;
endmodule

(* synthesize *)
module mkATFInterface (ViewOf#(Bool));
  let imported <- mkImportedATF;
  return imported;
endmodule

interface WithATF;
  interface ViewOf#(Bool) sub;
  method ViewOf#(Bit#(1)) value();
endinterface

(* synthesize *)
module mkATFFields (WithATF);
  interface WriteOnly sub;
    method Action _write(Bit#(3) x);
      noAction;
    endmethod
  endinterface
  method value = 5'h15;
endmodule
