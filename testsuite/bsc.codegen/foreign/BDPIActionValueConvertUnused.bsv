import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValueConvertUnused ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) r2 <- mkReg(0);

   // as BDPIActionValueConvert, but the result of each call is not used,
   // so there is nothing to report
   Get#(Bit#(32)) g = toGet(actionvalue
                               Bit#(32) x <- fromActionValue_(toActionValue_(my_count(1)));
                               fromPrimAction(toPrimAction(action
                                                              let t <- my_count(2);
                                                              r2 <= t;
                                                           endaction));
                               return 42;
                            endactionvalue);

   rule go;
      r <= peekGet(g);
   endrule
endmodule
