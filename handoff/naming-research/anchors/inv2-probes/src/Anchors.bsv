package Anchors;

// Naming probe: one design that exercises every naming source.
// The //EDITn markers are where gen-variants.sh inserts unrelated
// state, rules and methods for the stability probes.

import FIFO::*;
import Vector::*;

interface SubIfc;
   method Action put(Bit#(8) v);
   method Bit#(8) peek;
   method ActionValue#(Bit#(8)) take;
endinterface

(* synthesize *)
module mkSub(SubIfc);
   Reg#(Bit#(8)) st <- mkReg(0);
   Reg#(Bit#(8)) cnt <- mkReg(0);
   method Action put(Bit#(8) v);
      st <= v;
   endmethod
   method Bit#(8) peek = st;
   method ActionValue#(Bit#(8)) take;
      cnt <= cnt + 1;
      return cnt;
   endmethod
endmodule

interface Inner;
   method Action bump(Bit#(8) d);
   method Bit#(8) val;
endinterface

// not synthesized: inlined into the parent, so its state, rules and
// method bodies take the instance name as a prefix
module mkInner(Inner);
   Reg#(Bit#(8)) r <- mkReg(0);
   Reg#(Bool) on <- mkReg(False);
   rule tick (on);
      r <= r + 1;
   endrule
   method Action bump(Bit#(8) d);
      let nd = r + d;
      r <= nd;
      on <= True;
   endmethod
   method Bit#(8) val = r;
endmodule

interface Top;
   method Action go(Bit#(8) a, Bit#(8) b);
   method Bit#(8) res;
   method Bit#(8) res2;
   method ActionValue#(Bit#(8)) pop;
   //EDIT2I
endinterface

function Bit#(8) mix(Bit#(8) p, Bit#(8) q);
   Bit#(8) t = p ^ q;
   return t + 1;
endfunction

(* synthesize *)
module sysAnchors(Top);
   //EDIT1
   Reg#(Bit#(8)) ra <- mkReg(0);
   Reg#(Bit#(8)) rb <- mkReg(0);
   Reg#(Bit#(8)) rc <- mkReg(0);
   Reg#(Bit#(8)) rd <- mkReg(0);
   Reg#(Bit#(8)) re <- mkReg(0);
   Reg#(Bit#(8)) rf <- mkReg(0);
   Reg#(Bit#(8)) rg <- mkReg(0);
   Reg#(Bit#(16)) acc <- mkReg(0);
   FIFO#(Bit#(8)) q <- mkFIFO;
   FIFO#(Bit#(8)) q2 <- mkFIFO;
   SubIfc sub <- mkSub;
   Inner in1 <- mkInner;
   Inner in2 <- mkInner;
   Vector#(4, Reg#(Bit#(8))) vr <- replicateM(mkReg(0));

   // user binder (sum), user-function binder (t in mix),
   // library binders (max's x/y), conditional method calls
   rule r_step (ra != 0);
      let sum = ra + rb;
      if (sum[0] == 1)
         q.enq(sum);
      else
         sub.put(mix(sum, ra));
      acc <= acc + zeroExtend(max(ra, rb)) + zeroExtend(ra & rb);
      rc <= sum + 1;
   endrule

   // binder bound to an ActionValue result (v), binder equal to
   // q.first, i.e. q$D_OUT (hd); Vector index select
   rule r_drain;
      let v <- sub.take;
      let hd = q.first;
      q.deq;
      vr[v[1:0]] <= hd + v;
      rd <= hd;
      rc <= hd ^ v;
   endrule

   // binder equal to a register read (z); two binders with the same
   // expression in different rules (s1 here, s2 in r_twin);
   // conditional calls of an inlined submodule's method
   rule r_alias (rb != 0);
      let z = ra;
      let s1 = ra & rb;
      if (z[0] == 1)
         in1.bump(z + s1);
      else
         in2.bump(s1 ^ z);
   endrule

   // s2 is a named twin of s1; s3 is a named twin of the unnamed
   // (ra | rb) used twice in r_unnamed; lo is a bit select
   rule r_twin;
      let s2 = ra & rb;
      let s3 = ra | rb;
      let lo = ra[3:0];
      re <= s2 + s3 + zeroExtend(lo);
   endrule

   rule r_unnamed;
      rf <= (ra | rb) + 1;
      rg <= (ra | rb) ^ (ra[7:4] == rb[7:4] ? 8'h1 : 8'h2);
   endrule

   rule r_fill;
      q2.enq(rc + in1.val + in2.val);
   endrule

   //EDIT3

   // method argument bound to a binder (a2) and passed through a
   // library function (pack/unpack)
   method Action go(Bit#(8) a, Bit#(8) b);
      Bit#(8) a2 = a;
      ra <= a2;
      rb <= pack(Int#(8)'(unpack(b)));
      rc <= a ^ b;
   endmethod
   method Bit#(8) res = vr[0] + vr[1] + sub.peek;
   // a method result whose expression has a user binder (w) that is
   // also the expression of sum in r_step
   method Bit#(8) res2;
      let w = ra + rb;
      return w;
   endmethod
   method ActionValue#(Bit#(8)) pop;
      q2.deq;
      return q2.first;
   endmethod
   //EDIT2M
endmodule
endpackage
