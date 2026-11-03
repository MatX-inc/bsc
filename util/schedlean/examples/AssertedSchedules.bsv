import FIFO::*;

interface AssertedSchedules;
   method Action setMode(Bit#(2) m);
   method Action setLimit(Bit#(8) x);
   method ActionValue#(Bit#(8)) take;
endinterface

// Schedules the designer asserts and bsc checks only at run time.
//  * mutually_exclusive: up and down both write r, under guards bsc cannot
//    relate (they read different registers).  The attribute lets both feed
//    r's write mux without priority; bsc adds a rule, __me_check_0, that
//    reports R0001 if they ever fire in the same cycle.
//  * conflict_free: emitA and emitB both enqueue outQ (enq C enq), each under
//    its own condition.  The attribute lets the two rules fire in the same
//    cycle; bsc records each enqueue in an RWire and reports R0002 if the two
//    enqueues ever coincide.
// The proof assumes both checks pass (the structure Assertions), and takes
// the RWires' behaviour from their Verilog (the structure Primitives).
//
// Compile: bsc -u -verilog -elab -show-schedule AssertedSchedules.bsv
(* synthesize *)
module mkAssertedSchedules(AssertedSchedules);
   Reg#(Bit#(8))  r     <- mkReg(0);
   Reg#(Bit#(2))  mode  <- mkReg(0);
   Reg#(Bit#(8))  limit <- mkReg(0);
   FIFO#(Bit#(8)) outQ  <- mkFIFO;

   (* mutually_exclusive = "up, down" *)
   rule up (mode == 1);
      r <= r + 1;
   endrule

   rule down (limit == 7);
      r <= r - 1;
   endrule

   (* conflict_free = "emitA, emitB" *)
   rule emitA;
      if (mode == 2) outQ.enq(r);
   endrule

   rule emitB;
      if (limit == 9) outQ.enq(limit);
   endrule

   method Action setMode(Bit#(2) m);
      mode <= m;
   endmethod

   method Action setLimit(Bit#(8) x);
      limit <= x;
   endmethod

   method ActionValue#(Bit#(8)) take;
      outQ.deq;
      return outQ.first;
   endmethod
endmodule
