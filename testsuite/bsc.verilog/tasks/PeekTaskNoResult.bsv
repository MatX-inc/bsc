import GetPut::*;
import FIFO::*;

(* synthesize *)
module sysPeekTaskNoResult ();
   FIFO#(Bit#(32)) f <- mkFIFO;
   Reg#(Bit#(32)) r <- mkReg(0);

   // the Get logs each transfer, with no foreign result: nothing to report
   Get#(Bit#(32)) g = (interface Get;
                          method ActionValue#(Bit#(32)) get;
                             $display("get %0d", f.first);
                             f.deq;
                             return f.first;
                          endmethod
                       endinterface);

   rule fill;
      f.enq(r);
      r <= r + 1;
   endrule

   rule look (peekGet(g) == 3);
      let x <- g.get;
      $display("got %0d", x);
   endrule
endmodule
