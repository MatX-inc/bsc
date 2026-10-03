package DefMapOrder;

import FIFO::*;

interface DefMapOrder;
   method Action put(Bit#(8) b);
endinterface

// The other two orders ITransform hands on.
//
// Rule rone's case puts each enq under a condition; with
// -keep-method-conds the evaluator adds one COND_RL_rone_<fifo>_enq_<n>
// def per call, numbered in the order the rule makes the calls, and
// marks them NoCSE, so they come out of ITransform's def_map rather than
// its CSE map.  Before the fix that map was walked in Ord Id, the
// string-intern order of the COND_ names.
//
// The method's two defs, first and second, are left distinct by the
// evaluator (second still has its inner mask) and made equal by
// ITransform, which folds the masks; the two then fall in one CSE class
// and one of their names survives as the def's name.  Both are user
// names of the same quality, so before the fix the survivor was the
// greater in Ord Id: whichever name the process interned later.
(* synthesize *)
module mkDefMapOrder(DefMapOrder);
   FIFO#(Bit#(8)) fa <- mkFIFO;
   FIFO#(Bit#(8)) fb <- mkFIFO;
   FIFO#(Bit#(8)) fc <- mkFIFO;
   Reg#(Bit#(2)) s <- mkReg(0);
   Reg#(Bit#(8)) x <- mkReg(0);
   Reg#(Bit#(8)) r1 <- mkReg(0);
   Reg#(Bit#(8)) r2 <- mkReg(0);

   rule rone;
      case (s)
         0: fa.enq(x);
         1: fb.enq(x);
         default: fc.enq(x);
      endcase
   endrule

   method Action put(Bit#(8) b);
      Bit#(8) first = b & 8'h0f;
      Bit#(8) second = (b & 8'h3f) & 8'h0f;
      r1 <= first;
      r2 <= second;
   endmethod
endmodule

endpackage
