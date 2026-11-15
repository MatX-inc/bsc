import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekWhenInRule ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the when lifts the condition on the peeked result to the rule's
   // condition, which uses it
   rule go;
      when(peekGet(g) == 0, r._write(1));
   endrule
endmodule
