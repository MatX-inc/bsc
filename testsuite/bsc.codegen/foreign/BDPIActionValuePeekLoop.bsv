import GetPut::*;
import Vector::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekLoop ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) s <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   function Bit#(32) rd(Integer k) = peekGet(g) + fromInteger(k);

   // each evaluation of a read is a read, wherever it is in the source:
   // the reads in two iterations of a loop, or in two calls of one
   // function, are two calls of my_count, and the differences are not 0
   rule loop;
      Vector#(2, Bit#(32)) v = replicate(0);
      for (Integer i = 0; i < 2; i = i + 1)
         v[i] = peekGet(g);
      r <= v[0] - v[1];
   endrule

   rule helper;
      s <= rd(0) - rd(0);
   endrule
endmodule
