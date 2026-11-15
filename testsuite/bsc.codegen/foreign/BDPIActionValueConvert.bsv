import FIFO::*;
import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValueConvert ();
   Reg#(Bit#(32)) r <- mkReg(0);
   FIFO#(Bool) f <- mkFIFO;

   // toActionValue_ and toPrimAction make an action into a value with no
   // world, in which the call is made: here the action is then dropped,
   // by a peek of the value, and by impCondOf, but the result of the call
   // is used
   Get#(Bit#(32)) g = toGet(fromActionValue_(toActionValue_(my_count(1))));
   Action m = action
                 fromPrimAction(toPrimAction(action
                                                let t <- my_count(2);
                                                if (t == 0) f.enq(True);
                                             endaction));
              endaction;

   rule go;
      r <= peekGet(g);
   endrule

   rule cond;
      $display("cond m = %d", impCondOf(m));
   endrule
endmodule
