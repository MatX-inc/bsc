import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValueConvertRunTwice ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) r2 <- mkReg(0);
   Reg#(Bit#(32)) r3 <- mkReg(0);
   Reg#(Bool) c <- mkReg(False);

   Get#(Bit#(32)) g = toGet(fromActionValue_(toActionValue_(my_count(1))));

   // the call is made in each arm of the if, so in every cycle in which
   // the rule fires, but the conditions of the two arms are not compared
   // with the (absent) condition of the use: an error
   rule go;
      if (c) begin let x <- g.get; r2 <= x; end
      else begin let y <- g.get; r3 <= y; end
      r <= peekGet(g);
   endrule
endmodule
