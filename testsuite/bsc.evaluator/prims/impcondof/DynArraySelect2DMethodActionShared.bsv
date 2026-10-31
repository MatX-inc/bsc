import FIFO::*;
import Vector::*;

// The selection is bound by a let that is used twice: impCondOf reads
// it in one rule and the other rule performs it, so the evaluator
// heaps it (and the impCondOf application) rather than evaluating
// either in place.

(* synthesize *)
module sysDynArraySelect2DMethodActionShared();

   Vector#(2, Vector#(2, FIFO#(Bool))) fs <- replicateM(replicateM(mkFIFO));

   Reg#(Bit#(1)) idx1 <- mkReg(0);
   Reg#(Bit#(1)) idx2 <- mkReg(0);

   let a = fs[idx1][idx2].enq(True);

   rule r1;
      $display("cond = %d", impCondOf(a));
   endrule

   rule r2;
      a;
   endrule

endmodule
