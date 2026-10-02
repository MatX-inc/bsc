package EnableOrder;

interface EnableOrder;
   method Action w1(UInt#(8) x);
   method Action w2(UInt#(8) x);
endinterface

// Two methods write the same register, so its enable is the OR of the two
// method enables and its input a mux between the two arguments.  The
// order of the terms is the order of the method's uses in the scheduler's
// use map and resource table; elaboration meets w1's use before w2's
// whatever the process compiled before.  The scramblers are this module
// with the other method renamed, so each interns one method's names.
(* synthesize *)
module mkEnableOrder(EnableOrder);
   Reg#(UInt#(8)) r <- mkReg(0);
   method Action w1(UInt#(8) x); r <= x; endmethod
   method Action w2(UInt#(8) x); r <= x; endmethod
endmodule

endpackage
