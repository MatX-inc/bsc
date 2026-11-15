import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValueStructCondConvert ();
   Reg#(Bool) c <- mkReg(False);
   Reg#(Bool) d <- mkReg(False);
   Reg#(Bit#(32)) r <- mkReg(0);

   Get#(Bit#(32)) g = toGet(fromActionValue_(toActionValue_(my_count(1))));

   // the call is made under c && d, and the result is used under the same
   // condition, written again (the reads of c and d are separate
   // expressions with -keep-method-conds)
   rule a;
      if (c && d) begin let v <- g.get; end
      if (c && d) r <= peekGet(g);
   endrule
endmodule
