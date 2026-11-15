import FIFO::*;
import GetPut::*;
import Vector::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekDynSel ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(2)) s <- mkReg(0);
   Vector#(4, FIFO#(Bit#(32))) fs <- replicateM(mkFIFO);
   Vector#(4, Reg#(Bit#(32))) rs <- replicateM(mkReg(0));

   // each call follows an action that a dynamic index chooses (a FIFO enq,
   // a FIFO deq, a register write), so it is not made in a peek either:
   // the rule uses each result
   Get#(Bit#(32)) g1 = toGet(actionvalue
                                fs[s].enq(17);
                                let x <- my_count(1);
                                return x;
                             endactionvalue);
   Get#(Bit#(32)) g2 = toGet(actionvalue
                                fs[s].deq;
                                let x <- my_count(2);
                                return x;
                             endactionvalue);
   Get#(Bit#(32)) g3 = toGet(actionvalue
                                rs[s] <= 1;
                                let x <- my_count(3);
                                return x;
                             endactionvalue);

   rule go;
      r <= peekGet(g1) + peekGet(g2) + peekGet(g3);
   endrule
endmodule
