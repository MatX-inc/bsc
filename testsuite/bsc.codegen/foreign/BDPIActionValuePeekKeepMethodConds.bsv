import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekKeepMethodConds ();
   Reg#(Bit#(32)) q <- mkReg(0);
   Reg#(Bit#(32)) r <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // q == q folds to True, so the peek is in an arm that is never taken
   // and its result is not used.  With -keep-method-conds, the evaluator
   // keeps each read of q as its own expression (with its position), does
   // not fold the comparison, and keeps the arm, so the use is reported
   // with that flag only.  The check sees the design as the evaluator
   // leaves it: this dependence on the flag is accepted.
   rule go;
      r <= (q == q) ? 5 : peekGet(g);
   endrule
endmodule
