import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekModCase ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the rules added depend on the peeked result of my_count, which
   // elaboration must know
   case (peekGet(g))
      0: rule a; r <= 1; endrule
      default: rule b; r <= 2; endrule
   endcase
endmodule
