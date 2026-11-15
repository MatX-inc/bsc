import FIFO::*;
import Vector::*;

import "BDPI" function ActionValue#(Bit#(32)) my_count (Bit#(8) step);

(* synthesize *)
module mkBDPIActionValueDynSel ();
   Reg#(Bit#(2)) s <- mkReg(0);
   Reg#(Bit#(4)) step <- mkReg(0);
   Vector#(4, FIFO#(Bit#(32))) fs <- replicateM(mkFIFO);
   Vector#(4, Reg#(Bit#(32))) rs <- replicateM(mkReg(0));

   // a call that follows actions chosen by a dynamic index: a FIFO enq ...
   rule enq (step < 4);
      fs[s].enq(zeroExtend(s));
      let x <- my_count(1);
      $display("enq %0d %0d", s, x);
      s <= s + 1;
      step <= step + 1;
   endrule

   // ... a FIFO deq and a register write
   rule deq (step >= 4 && step < 8);
      fs[s].deq;
      rs[s] <= fs[s].first;
      let y <- my_count(10);
      $display("deq %0d %0d %0d", s, fs[s].first, y);
      s <= s + 1;
      step <= step + 1;
   endrule

   rule done (step == 8);
      $finish(0);
   endrule
endmodule
