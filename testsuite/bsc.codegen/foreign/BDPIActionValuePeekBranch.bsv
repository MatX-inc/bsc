import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekBranch ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) s <- mkReg(0);
   Reg#(Bool) c <- mkReg(False);

   Get#(Bit#(32)) g = toGet(my_count(1));

   // the get makes a call when c holds; the peek in the other arm makes
   // none
   rule go;
      if (c) begin
         let x <- g.get;
         s <= x;
      end
      else
         r <= peekGet(g);
      c <= !c;
   endrule
endmodule
