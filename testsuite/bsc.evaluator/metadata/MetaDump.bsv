import Vector::*;
import FIFO::*;

// A recursive top-level function: a recursion point of this package's
// definition graph.
function Integer sumTo(Integer n);
   return (n == 0) ? 0 : n + sumTo(n - 1);
endfunction

interface MetaIfc;
   method UInt#(8) total();
   method Action poke(UInt#(8) x);
endinterface

(* synthesize *)
module sysMetaDump(MetaIfc);
   Vector#(4, Reg#(UInt#(8))) regs <- replicateM(mkReg(0));
   Reg#(UInt#(8)) cnt <- mkReg(0);
   Reg#(UInt#(8)) last <- mkReg(0);
   FIFO#(UInt#(8)) q <- mkFIFO;

   // a static loop: iterations of one recursive definition under one rule
   rule step;
      cnt <= cnt + 1;
      for (Integer i = 0; i < 4; i = i + 1)
         regs[i] <= regs[i] + fromInteger(sumTo(i));
   endrule

   // method calls with implicit conditions
   rule drain;
      q.deq;
      last <= q.first;
   endrule

   method UInt#(8) total();
      UInt#(8) s = 0;
      for (Integer i = 0; i < 4; i = i + 1)
         s = s + regs[i];
      return s;
   endmethod

   method Action poke(UInt#(8) x);
      q.enq(x);
   endmethod
endmodule
