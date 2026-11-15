import GetPut::*;
import BDPIActionValueImport::*;

interface ConvertMethod;
   method ActionValue#(Bit#(32)) get_peek();
endinterface

(* synthesize *)
module mkBDPIActionValueConvertMethod (ConvertMethod);
   Reg#(Bool) c <- mkReg(False);

   // the call is made where toActionValue_ converts the action, once for
   // every execution of g.get
   Get#(Bit#(32)) g = toGet(fromActionValue_(toActionValue_(my_count(1))));

   // the method makes the call only when c holds, but returns the result
   // whether or not it does
   method ActionValue#(Bit#(32)) get_peek();
      if (c) begin let x <- g.get; end
      return peekGet(g);
   endmethod
endmodule
