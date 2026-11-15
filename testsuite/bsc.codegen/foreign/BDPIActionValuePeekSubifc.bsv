import GetPut::*;
import BDPIActionValueImport::*;

interface Ifc;
   interface Get#(Bit#(32)) g;
endinterface

(* synthesize *)
module mkBDPIActionValuePeekSubifc (Ifc);
   Get#(Bit#(32)) inner = toGet(my_count(1));
   ActionValue#(Bit#(32)) av = actionvalue
                                  return peekGet(inner);
                               endactionvalue;

   // the method g.get returns the peeked result of my_count; toGet, in the
   // library, defines it, so it is reported at the subinterface
   interface g = toGet(av);
endmodule
