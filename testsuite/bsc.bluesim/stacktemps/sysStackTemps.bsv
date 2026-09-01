// A wide-heavy design for demonstrating that steady-state rule
// evaluation makes no allocator calls for wide temporaries: wide
// registers and arithmetic, wide concatenations (including one nested
// inside a method argument), wide extracts and shifts, a wide FIFO,
// and wide-returning methods (value, with a wide argument, and
// ActionValue), evaluated every cycle.
//
// Wide division/remainder are deliberately NOT used here: their
// runtime implementation still allocates internal temporaries.

import FIFO::*;

interface StkIfc;
   method Bit#(512) wideVal();
   method Bit#(256) mix(Bit#(128) x);
   method ActionValue#(Bit#(320)) bump(Bit#(320) d);
endinterface

(* synthesize *)
module mkStackTempsSub(StkIfc);
   Reg#(Bit#(512)) big <- mkReg(512'h123456789abcdef0fedcba9876543210);
   Reg#(Bit#(320)) acc <- mkReg(1);

   rule churn;
      big <= {big[254:0], big[511:255]} + zeroExtend(acc * acc);
   endrule

   method Bit#(512) wideVal();
      Bit#(256) h = (acc[255:0] == 0) ? 256'h5 : zeroExtend(acc[63:0]);
      return big + {h, h};
   endmethod

   method Bit#(256) mix(Bit#(128) x);
      return {x, x ^ big[127:0]} + big[255:0];
   endmethod

   method ActionValue#(Bit#(320)) bump(Bit#(320) d);
      acc <= acc + d;
      return (acc ^ d) + zeroExtend(d[31:0]);
   endmethod
endmodule

(* synthesize *)
module sysStackTemps();
   StkIfc sub <- mkStackTempsSub;
   FIFO#(Bit#(300)) fif <- mkFIFO;
   Reg#(Bit#(256)) r1 <- mkReg(17);
   Reg#(Bit#(256)) r3 <- mkReg(0);
   Reg#(Bit#(512)) r2 <- mkReg(3);
   Reg#(Bit#(32))  n  <- mkReg(0);

   rule step;
      Bit#(512) w = sub.wideVal();
      // nested concatenation as a method argument
      Bit#(256) m = sub.mix({r1[63:0], r1[127:64]});
      Bit#(512) cat = {m, r1} ^ w;
      r2 <= (r2 + cat + {r1, m}) << r1[3:0];
      r1 <= m + truncate(w >> 3);
      n <= n + 1;
   endrule

   rule fill (n[0] == 0);
      fif.enq(r2[299:0] ^ zeroExtend(n));
   endrule

   rule drain (n[0] == 1);
      let v = fif.first();
      fif.deq();
      r3 <= r3 ^ ({v[127:0], v[255:128]} + zeroExtend(n));
   endrule

   rule poke (n[2:0] == 3);
      let v <- sub.bump(zeroExtend(r1[63:0]));
      r2 <= r2 ^ zeroExtend(v);
   endrule

   rule done (n == 2000);
      $display("r1=%h", r1);
      $display("r2=%h", r2);
      $display("r3=%h", r3);
      $display("wideVal=%h", sub.wideVal());
      $finish(0);
   endrule
endmodule
