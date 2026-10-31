import FIFO::*;
import Vector::*;

// The condition is packed to a bit and bound by a let that two rules
// share: the heap cell for b holds the primImpCondOf application
// itself (the pack and unpack cancel), and evaluates to the case tree
// of the two-level selection.

(* synthesize *)
module sysDynArraySelect2DMethodActionPackShared();

   Vector#(2, Vector#(2, FIFO#(Bool))) fs <- replicateM(replicateM(mkFIFO));

   Reg#(Bit#(1)) idx1 <- mkReg(0);
   Reg#(Bit#(1)) idx2 <- mkReg(0);

   let a = fs[idx1][idx2].enq(True);

   Bit#(1) b = pack(impCondOf(a));

   rule r1;
      $display("cond1 = %d", b);
   endrule

   rule r2;
      $display("cond2 = %d", b);
   endrule

endmodule
