import GetPut::*;
import BDPIActionValueImport::*;

interface Ifc;
   method Bit#(32) first;
endinterface

(* synthesize *)
module mkBDPIActionValuePeekMethod (Ifc);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // the method that returns the peeked result is reported where it is
   // defined
   method Bit#(32) first;
      return peekGet(g);
   endmethod
endmodule
