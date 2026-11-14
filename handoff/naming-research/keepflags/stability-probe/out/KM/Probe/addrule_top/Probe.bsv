package Probe;

import FIFO::*;
import FIFOF::*;
import Vector::*;

interface Counter;
   method Action incr(Bit#(8) by);
   method Action clear;
   method Bit#(8) value;
   method Bool isMax;
endinterface

// not synthesized: inlined into mkProbe
module mkCounter(Counter);
   Reg#(Bit#(8)) cnt <- mkReg(0);
   RWire#(Bit#(8)) incw <- mkRWire;
   PulseWire clrw <- mkPulseWire;
   Wire#(Bit#(8)) dbg <- mkDWire(0);

   (* fire_when_enabled, no_implicit_conditions *)
   rule apply;
      Bit#(8) n = cnt;
      if (incw.wget matches tagged Valid .d) n = n + d;
      if (clrw) n = 0;
      cnt <= n;
      dbg <= n;
   endrule

   method Action incr(Bit#(8) by);
      incw.wset(by);
   endmethod
   method Action clear;
      clrw.send;
   endmethod
   method value = cnt;
   method isMax = (cnt == 255);
endmodule

typedef struct { Bit#(4) tag; Bit#(12) data; } Item deriving (Bits, Eq);

interface Probe;
   method Action put(Item x);
   method Item peek;
   method ActionValue#(Bit#(16)) take;
   method Bit#(8) count;
   method Action setMode(Bool m);
endinterface

(* synthesize *)
module mkProbe(Probe);
   FIFO#(Item) inq <- mkFIFO;
   FIFOF#(Item) outq <- mkSizedFIFOF(4);
   Reg#(Bit#(16)) acc <- mkReg(0);
   Reg#(Bit#(16)) last <- mkReg(0);
   Reg#(Bool) mode <- mkReg(False);
   Reg#(Bit#(4)) phase <- mkReg(0);
   Vector#(4, Reg#(Bit#(12))) bank <- replicateM(mkReg(0));
   Counter ctr <- mkCounter;

   rule clear_acc (mode && phase == 7);
      acc <= 0;
   endrule

   rule step_phase;
      phase <= phase + 1;
   endrule

   rule process (!mode);
      let x = inq.first; inq.deq;
      let y = acc + zeroExtend(x.data);
      acc <= y;
      if (y[0] == 1) outq.enq(Item { tag: x.tag, data: y[11:0] });
      ctr.incr(1);
   endrule

   rule process_alt (mode);
      let x = inq.first; inq.deq;
      last <= zeroExtend(x.data);
      bank[x.tag[1:0]] <= x.data;
      if (phase == 3) outq.enq(Item { tag: x.tag ^ 1, data: x.data ^ acc[11:0] });
      else if (x.tag == 0) ctr.clear;
   endrule

   rule toggle (phase == 15 && !ctr.isMax);
      mode <= !mode;
   endrule

   for (Integer i = 0; i < 4; i = i + 1)
      rule decay (bank[i] != 0 && phase[1:0] == fromInteger(i));
         bank[i] <= bank[i] >> 1;
      endrule

   method Action put(Item x);
      inq.enq(x);
   endmethod
   method Item peek = outq.first;
   method ActionValue#(Bit#(16)) take;
      outq.deq;
      return zeroExtend(outq.first.data) + last + zeroExtend(bank[outq.first.tag[1:0]]);
   endmethod
   method Bit#(8) count = ctr.value;
   method Action setMode(Bool m) if (phase != 15);
      mode <= m;
   endmethod
endmodule

endpackage
