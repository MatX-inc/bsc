// The condition of an if-expression whose arm is filled is a dynamic bit
// selection; checking whether it implies the other arm's tag must not loop.

interface FillDynSelCond;
   method Bit#(5) out(Bit#(8) x, Bit#(3) i, Bit#(5) y);
endinterface

(* synthesize *)
module mkUndet_FillDynSelCond(FillDynSelCond);
   method Bit#(5) out(Bit#(8) x, Bit#(3) i, Bit#(5) y) =
      (x[i] == 1) ? {3'b001, ?} : y;
endmodule
