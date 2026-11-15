import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValueConvertCondSameRule ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) r2 <- mkReg(0);
   Reg#(Bool) c <- mkReg(False);

   Get#(Bit#(32)) g = toGet(fromActionValue_(toActionValue_(my_count(1))));

   // the rule makes the call only when c holds, and uses the result
   // whether or not it does
   rule go;
      if (c) begin let x <- g.get; r2 <= x; end
      r <= peekGet(g);
   endrule
endmodule
