import GetPut::*;
import FIFO::*;

(* synthesize *)
module sysPeekTaskUsed ();
   FIFO#(Bit#(32)) f <- mkFIFO;

   // the action returns the time: peekGet reads the value without running
   // the action, so the result of $time does not exist when the rule uses
   // it (an error)
   Get#(Bit#(64)) g = (interface Get;
                          method ActionValue#(Bit#(64)) get;
                             let t <- $time;
                             f.deq;
                             return t;
                          endmethod
                       endinterface);

   rule go;
      let p = peekGet(g);
      let x <- g.get;
      $display("peek %0d get %0d", p, x);
   endrule
endmodule
