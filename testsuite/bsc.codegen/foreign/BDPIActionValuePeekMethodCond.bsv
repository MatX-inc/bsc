import GetPut::*;
import BDPIActionValueImport::*;

interface Ifc;
   method Bit#(32) first;
endinterface

(* synthesize *)
module mkBDPIActionValuePeekMethodCond (Ifc);
   Reg#(Bit#(32)) r <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the condition of the method is on the peeked result of my_count,
   // which it uses
   method Bit#(32) first if (peekGet(g) != 0);
      return r;
   endmethod
endmodule
