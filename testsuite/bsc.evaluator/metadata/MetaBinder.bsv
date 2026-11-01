import FIFO::*;

// A struct whose fields are moved into cells of their own when the
// struct is evaluated.
typedef struct {
   Bit#(8) lo;
   Bit#(8) hi;
} Pair deriving (Bits, Eq);

// A function whose arguments are heaped for its parameters.
function Bit#(8) scale(Bit#(8) x, Bit#(8) y);
   return (x << 1) + y;
endfunction

interface MetaBinderIfc;
   method Bit#(8) out();
   method Action put(Bit#(8) v);
endinterface

(* synthesize *)
module sysMetaBinder(MetaBinderIfc);
   Reg#(Bit#(8)) cnt <- mkReg(0);
   Reg#(Bit#(8)) acc <- mkReg(0);
   Reg#(Bit#(8)) sel <- mkReg(0);
   FIFO#(Bit#(8)) q <- mkFIFO;

   // a static loop whose result the evaluator, not the simplifier, decides
   Integer three = 0;
   for (Integer i = 0; i < 3; i = i + 1)
      three = three + i;

   rule args;
      // arguments bound to the parameters x and y
      Bit#(8) s = scale(cnt + 1, cnt ^ 3);
      // a let-bound value, and one that evaluates to it (an alias)
      Bit#(8) a = cnt * 5;
      Bit#(8) b = (three == 3) ? a : s;
      acc <= s + b;
   endrule

   rule moves;
      // a struct under a dynamic condition on a method with an
      // implicit condition: its fields are moved into cells, and a
      // field selected inline is re-heaped under the condition
      Pair p = (q.first > cnt) ? Pair { lo: cnt + 2, hi: cnt - 2 }
                               : Pair { lo: cnt - 7, hi: cnt + 7 };
      sel <= p.lo + p.hi;
      // a tagged union whose operand is moved into a cell, then extracted
      Maybe#(Bit#(8)) m = tagged Valid (cnt + 9);
      if (m matches tagged Valid .v)
         cnt <= v;
      q.deq;
   endrule

   method Bit#(8) out();
      return acc;
   endmethod

   method Action put(Bit#(8) v);
      q.enq(v);
   endmethod
endmodule
