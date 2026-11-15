import GetPut::*;
import Vector::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekUnreach ();
   Reg#(Bit#(32)) q <- mkReg(0);
   Reg#(Bit#(32)) r <- mkReg(0);
   Get#(Bit#(32)) g = toGet(my_count(1));

   // a selection by the peeked result from elements that are one value
   // where the index can reach them is that value: the result is not
   // used (the fifth element, which a two-bit index cannot reach, differs)
   rule go;
      Bit#(32) e = q;
      Vector#(5, Bit#(32)) v = cons(e, cons(e, cons(e, cons(e, cons(7, nil)))));
      Bit#(2) i = truncate(peekGet(g));
      r <= v[i];
   endrule
endmodule
