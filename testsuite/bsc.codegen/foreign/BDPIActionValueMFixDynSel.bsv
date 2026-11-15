import FIFO::*;
import Vector::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValueMFixDynSel ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(2)) s <- mkReg(0);
   Vector#(4, FIFO#(Bit#(32))) fs <- replicateM(mkFIFO);
   Vector#(4, Reg#(Bit#(32))) rs <- replicateM(mkReg(0));

   // the call follows actions that a dynamic index chooses, and mfix reads
   // its result without running it
   function ActionValue#(Tuple2#(Bit#(32), Bit#(32))) loop(Tuple2#(Bit#(32), Bit#(32)) x);
      actionvalue
         fs[s].enq(1);
         rs[s] <= 1;
         let t <- my_count(1);
         return tuple2(t, tpl_1(x));
      endactionvalue
   endfunction

   rule go;
      let v <- mfix(loop);
      r <= tpl_2(v);
   endrule
endmodule
