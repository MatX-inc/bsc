package Fifo;
// Primitive FIFO instance:
//  enq fires whenever not full:  f$ENQ == f$FULL_N == WILL_FIRE_RL_enq
//  deq fires whenever not empty: f$DEQ == f$EMPTY_N, r$D_IN == f$D_OUT
//  f$D_IN == c (register read)
import FIFO::*;
(* synthesize *)
module sysFifo(Empty);
   FIFO#(Bit#(8)) f <- mkFIFO;
   Reg#(Bit#(8)) c <- mkReg(0);
   Reg#(Bit#(8)) r <- mkReg(0);
   rule enq;
      f.enq(c);
   endrule
   rule deq;
      r <= f.first;
      f.deq;
   endrule
   rule cnt;
      c <= c + 1;
   endrule
endmodule
endpackage
