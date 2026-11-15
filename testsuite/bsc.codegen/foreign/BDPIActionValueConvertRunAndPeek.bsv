import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValueConvertRunAndPeek ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) s <- mkReg(0);

   // toActionValue_ makes the call once, for every execution: rule run
   // runs its action, but rule go reads its value without running it, so
   // the result is used where the call has not run
   Get#(Bit#(32)) g = toGet(fromActionValue_(toActionValue_(my_count(1))));

   rule run;
      let x <- g.get;
      s <= x;
   endrule

   rule go;
      r <= peekGet(g);
   endrule
endmodule
