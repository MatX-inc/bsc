import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekTwice ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) s <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // each read of an ActionValue is an execution of it, so two reads are
   // two calls of my_count, which the design does not make, and the
   // differences are not 0
   rule go;
      r <= (peekGet(g) - peekGet(g)) +
           (((s == 0) ? peekGet(g) : peekGet(g)) - peekGet(g));
   endrule
endmodule
