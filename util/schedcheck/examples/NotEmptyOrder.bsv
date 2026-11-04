import FIFOF::*;

interface NotEmptyOrder;
   method Action push(Bit#(8) x);
endinterface

// Method uses that matter only under conditions.  drain's guard reads
// f.notEmpty, g.notEmpty and b, but reads f.notEmpty only when its value can
// decide the guard; bsc's scheduler works with such use conditions.  And
// f.notEmpty comes from the same Verilog port (EMPTY_N) as f.i_notEmpty, the
// implicit condition of f.first and f.deq, so the two reads are one value.
// The .ba records neither fact: verifyschedule recomputes the use conditions
// and reads the port names from the FIFO's VModInfo.
//
// Compile: bsc -u -verilog -elab -show-schedule NotEmptyOrder.bsv
(* synthesize *)
module mkNotEmptyOrder(NotEmptyOrder);
   FIFOF#(Bit#(8)) f <- mkSizedFIFOF(4);
   FIFOF#(Bit#(8)) g <- mkSizedFIFOF(4);
   Reg#(Bit#(8)) r <- mkReg(0);
   Reg#(Bool) b <- mkReg(False);

   rule drain (!(f.notEmpty && g.notEmpty) || b);
      r <= f.first;
      f.deq;
   endrule

   method Action push(Bit#(8) x);
      f.enq(x);
   endmethod
endmodule
