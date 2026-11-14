package AnchorsE2;

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

interface Top;
   method Action go(Bit#(8) a, Bit#(8) b);
   method Bit#(8) res;
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
   Reg#(Bit#(16)) acc <- mkReg(0);
   FIFO#(Bit#(8)) q <- mkFIFO;
   SubIfc sub <- mkSub;
   Vector#(4, Reg#(Bit#(8))) vr <- replicateM(mkReg(0));

   rule r_step (ra != 0);
      let sum = ra + rb;
      if (sum[0] == 1)
         q.enq(sum);
      else
         sub.put(mix(sum, ra));
      acc <= acc + zeroExtend(max(ra, rb)) + zeroExtend(ra & rb);
   endrule

   rule r_drain;
      let v <- sub.take;
      q.deq;
      vr[v[1:0]] <= q.first + v;
   endrule

   Reg#(Bit#(4)) zz <- mkReg(0);
   rule r_zz;
      zz <= zz + 3;
   endrule
   method Action go(Bit#(8) a, Bit#(8) b);
      ra <= a;
      rb <= b;
   endmethod
   method Bit#(8) res = vr[0] + vr[1] + sub.peek;
endmodule
endpackage
