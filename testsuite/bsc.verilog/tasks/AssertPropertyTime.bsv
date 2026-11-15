import FIFO::*;
import GetPut::*;
import SVA::*;

// The assertion library runs the pass action in two arms of a case, and
// the evaluator removes the arm for a result that never happens; a
// peekGet elsewhere in the module drops no action of the assertion
(* synthesize *)
module sysAssertPropertyTime();
   Reg#(Bool) r <- mkReg(False);
   Reg#(Bit#(8)) y <- mkReg(0);
   FIFO#(Bit#(8)) f <- mkFIFO;

   rule flip;
      r <= !r;
   endrule

   rule peek;
      y <= peekGet(toGet(f));
   endrule

   property p();
      r |-> !r;
   endproperty

   always assert property(p())
      $display("Time: %0t PASS", $time);
   else $display("Time: %0t FAIL", $time);
endmodule
