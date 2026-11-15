import GetPut::*;
import FIFO::*;

(* synthesize *)
module sysPeekTaskLog ();
   FIFO#(Bit#(32)) f <- mkFIFO;
   Reg#(Bit#(32)) r <- mkReg(0);

   // a Get that logs each transfer with the time: peekGet reads the value
   // without running the action, so neither the $display nor $time is
   // run, and the result of $time is not used
   Get#(Bit#(32)) g = (interface Get;
                          method ActionValue#(Bit#(32)) get;
                             $display("%t get %0d", $time, f.first);
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
