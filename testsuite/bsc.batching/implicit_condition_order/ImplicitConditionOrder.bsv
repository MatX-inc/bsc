import FIFO::*;

// A synthesized submodule whose ActionValue method carries an implicit
// condition; two instances give the rule below two RDY_pop conjuncts,
// the shape of impCondOf's sysActionMethodActionValue (f1$RDY_pop &&
// f2$RDY_pop).
interface Popper;
   method ActionValue#(Bit#(8)) pop();
endinterface

(* synthesize *)
module mkPopper(Popper);
   FIFO#(Bit#(8)) f <- mkFIFO;

   method ActionValue#(Bit#(8)) pop();
      f.deq;
      return f.first;
   endmethod
endmodule

interface Peek;
   method Bit#(8) peek();
endinterface

(* synthesize *)
module sysImplicitConditionOrder(Peek);
   Popper p1 <- mkPopper;
   Popper p2 <- mkPopper;
   FIFO#(Bit#(8)) inQ <- mkFIFO;
   FIFO#(Bit#(8)) outQ <- mkFIFO;
   FIFO#(Bit#(8)) sideQ <- mkFIFO;
   Reg#(Bit#(8)) count <- mkReg(0);
   Reg#(Bool) enable <- mkReg(True);

   // one rule: an explicit condition, then implicit conditions from six
   // submodules met in this order: inQ.first, inQ.deq, p1.pop, p2.pop,
   // outQ.enq, sideQ.enq
   rule transfer (enable && count < 200);
      let x = inQ.first;
      inQ.deq;
      let a <- p1.pop;
      let b <- p2.pop;
      outQ.enq(x + a);
      sideQ.enq(x ^ b);
      count <= count + 1;
   endrule

   // a method whose RDY is two implicit conditions: outQ.first, sideQ.first
   method Bit#(8) peek();
      return outQ.first + sideQ.first;
   endmethod
endmodule
