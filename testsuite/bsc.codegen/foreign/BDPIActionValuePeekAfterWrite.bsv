import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekAfterWrite ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) s <- mkReg(0);

   // the call of my_count follows a register write in the action, so it is
   // not made in a peek of the action either: the rule uses its result
   Get#(Bit#(32)) g = toGet(actionvalue
                               s <= 5;
                               let m <- my_count(2);
                               return m;
                            endactionvalue);

   rule go;
      r <= peekGet(g);
   endrule
endmodule
