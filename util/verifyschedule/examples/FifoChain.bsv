import FIFO::*;
import SpecialFIFOs::*;

// One value stream through four FIFO kinds.
//   q2 : mkFIFO         FIFO2 primitive (enq CF deq; guards i_notFull/i_notEmpty)
//   ql : mkLFIFO        FIFOL1 primitive (deq SBR enq; path DEQ -> FULL_N)
//   qb : mkBypassFIFO   library BSV module over a 3-port CReg, inlined here
//   qp : mkPipelineFIFO library BSV module over a 3-port CReg, inlined here
// Expected ordering constraints: s2 before s1 (ql), s2 before s3 (qb),
// s4 before s3 (qp); no rule conflicts.
//
// Checked (bsc 2026.07-6): order s0, s2, s1, s4, s3; the inlined CRegs are
// qb_rv and qp_rv, and their Invalid writes carry a DONTCARE payload.
//
// Compile: bsc -u -verilog -elab -show-schedule FifoChain.bsv
(* synthesize *)
module mkFifoChain(Empty);
   FIFO#(Bit#(8)) q2 <- mkFIFO;
   FIFO#(Bit#(8)) ql <- mkLFIFO;
   FIFO#(Bit#(8)) qb <- mkBypassFIFO;
   FIFO#(Bit#(8)) qp <- mkPipelineFIFO;
   Reg#(Bit#(8))  n   <- mkReg(0);
   Reg#(Bit#(8))  sum <- mkReg(0);

   rule s0;
      q2.enq(n);
      n <= n + 1;
   endrule

   rule s1;
      ql.enq(q2.first + 1);
      q2.deq;
   endrule

   rule s2;
      qb.enq(ql.first + 1);
      ql.deq;
   endrule

   rule s3;
      qp.enq(qb.first + 1);
      qb.deq;
   endrule

   rule s4;
      sum <= sum + qp.first;
      qp.deq;
   endrule
endmodule
