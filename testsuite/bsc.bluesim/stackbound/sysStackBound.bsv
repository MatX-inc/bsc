// A busy design for validating the model's static stack-depth bound
// (bk_stack_depth_bound): wide registers with wide arithmetic (adds,
// multiplies, shifts, extracts, concatenations and one wide
// division), a wide FIFO, and periodic $display of narrow values --
// so the measured cycles drive both the deepest generated-code paths
// and the runtime's formatted-output engine.
//
// The host_stackbound harness dlopen()s the model, reads the exposed
// bound, paints a stack region below the simulation thread's stack
// pointer, steps many cycles, and requires the painted high-water
// mark to stay within the bound.

import FIFO::*;

(* synthesize *)
module sysStackBound();
   Reg#(Bit#(1024)) huge <- mkReg(1024'h123456789abcdef0fedcba9876543210);
   Reg#(Bit#(512))  big  <- mkReg(3);
   Reg#(Bit#(256))  mid  <- mkReg(17);
   Reg#(Bit#(64))   cnt  <- mkReg(0);
   FIFO#(Bit#(300)) fif  <- mkFIFO;

   rule churn;
      Bit#(512) prod = zeroExtend(mid) * zeroExtend(big[127:0]);
      Bit#(1024) cat = {big, prod} ^ (huge >> big[4:0]);
      huge <= huge + cat + {prod, prod};
      big <= (big << 1) ^ prod ^ zeroExtend(cnt);
      mid <= mid + truncate(huge >> 5) + zeroExtend(cnt[15:0]);
      cnt <= cnt + 1;
   endrule

   rule divide (cnt[2:0] == 3);
      // wide division exercises the runtime's VLA-based quot/rem path
      Bit#(512) q = big / (zeroExtend(mid[63:0]) | 1);
      fif.enq(q[299:0] ^ zeroExtend(cnt));
   endrule

   rule drain;
      let v = fif.first();
      fif.deq();
      mid <= mid ^ zeroExtend(v[127:0]);
   endrule

   rule report (cnt[6:0] == 100);
      $display("cnt=%0d big=%h mid=%h", cnt, big[63:0], mid[31:0]);
   endrule

   rule done (cnt == 2000);
      $finish(0);
   endrule
endmodule
