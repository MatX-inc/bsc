import GetPut::*;
import BDPIActionValueImport::*;

interface Ifc;
   method Action put(Bit#(32) x);
endinterface

(* synthesize *)
module mkBDPIActionValuePeekMethodBody (Ifc);
   Reg#(Bit#(32)) r <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the body of the action method uses the peeked result
   method Action put(Bit#(32) x);
      r <= x + peekGet(g);
   endmethod
endmodule
