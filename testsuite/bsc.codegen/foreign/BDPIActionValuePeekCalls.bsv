import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekCalls ();
   Reg#(Bit#(32)) r <- mkReg(42);

   // two calls of one imported function whose results the value does not
   // use: they are not used
   Get#(Bit#(32)) g = toGet(actionvalue
                               let a <- my_count(1);
                               let b <- my_count(2);
                               return r;
                            endactionvalue);

   rule go;
      let p = peekGet(g);
      let x <- g.get;
      $display("peek %0d get %0d", p, x);
      $finish(0);
   endrule
endmodule
