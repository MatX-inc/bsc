import GetPut::*;
import Vector::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekSame ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) s <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // each value that the rule writes is the same whatever the peeked
   // result is: the same computation on it twice, a selection from
   // elements that are all the result, and an if whose arms are the result
   // (one through a function)
   rule go;
      let p = peekGet(g);
      Vector#(4, Bit#(32)) v = replicate(p);
      Bit#(32) q = (s == 0) ? p : id(p);
      r <= (((p + 1) == (p + 1)) ? 7 : 9) + (v[s[1:0]] - p) + (q - p);
   endrule
endmodule
