import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekStatic ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // elaboration must know an Integer made from the peeked result of
   // my_count, which a peek does not have: the conversion to Integer uses
   // it, and is reported
   rule go;
      r <= fromInteger(primUIntBitsToInteger(peekGet(g)));
   endrule
endmodule
