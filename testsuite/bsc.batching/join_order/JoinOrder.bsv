package JoinOrder;

// Several submodule instantiations and a numeric proviso in one module:
// the residual predicates of one definition include joinable IsModule
// pairs and Add predicates, so the context join has more than one group to
// choose from.
import Gray::*;

interface Ifc#(numeric type n);
   method Bit#(n) v;
endinterface

module mkJoinOrder#(Gray#(n) init)(Ifc#(n)) provisos (Add#(1, msb, n));
   Reg#(Gray#(n)) r1 <- mkRegA(init);
   Reg#(Gray#(n)) r2 <- mkRegA(init);
   PulseWire      pw <- mkPulseWire;

   rule step (pw);
      r1 <= grayIncr(r1);
      r2 <= grayDecr(r2);
   endrule

   method Bit#(n) v = grayDecode(r1) + grayDecode(r2);
endmodule

endpackage
