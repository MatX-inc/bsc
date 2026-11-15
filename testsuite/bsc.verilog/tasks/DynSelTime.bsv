import FIFO::*;
import Vector::*;

(* synthesize *)
module sysDynSelTime ();
   Reg#(Bit#(2)) port <- mkReg(0);
   Reg#(Bit#(4)) n <- mkReg(0);
   Vector#(4, FIFO#(Bit#(32))) outQ <- replicateM(mkFIFO);
   Vector#(4, Reg#(Bit#(64))) sent <- replicateM(mkReg(0));

   rule start (n == 0);
      n <= 1;
   endrule

   // a system task with a value after actions that a dynamic index
   // chooses (a FIFO enq and a register write), as testbenches do
   rule send (n >= 1 && n <= 4);
      outQ[port].enq(17);
      let t <- $time;
      sent[port] <= t;
      $display("sent on port %0d after time 0: %b", port, t > 0);
      port <= port + 1;
      n <= n + 1;
   endrule

   rule done (n == 5);
      $finish(0);
   endrule
endmodule
