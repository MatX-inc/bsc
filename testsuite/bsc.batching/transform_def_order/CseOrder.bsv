package CseOrder;

interface CseOrder;
   method Action put(Bit#(8) aa, Bit#(8) ab, Bit#(8) zz, Bit#(8) zy);
endinterface

// Two sums of method arguments, each used twice.  The evaluator keeps one
// heap cell per sum, and its common-subexpression pass (eqPtrs) and
// ITransform's CSE map are both keyed by the expression, so each hands
// its defs on in the Ord of the expressions.  Before the fix that Ord
// compared the argument names at the leaves (put_aa, put_ab against
// put_zz, put_zy) by intern id, so the def lists of the expanded and the
// transformed module came out in the order the process first saw the
// names; by content put_aa + put_ab comes before put_zz + put_zy whatever
// was compiled first.  Method arguments rather than registers, so that the
// names are the leaves of the sums themselves and the order does not pass
// through any other def.
(* synthesize *)
module mkCseOrder(CseOrder);
   Reg#(Bit#(8)) r1 <- mkReg(0);
   Reg#(Bit#(8)) r2 <- mkReg(0);
   Reg#(Bit#(8)) r3 <- mkReg(0);
   Reg#(Bit#(8)) r4 <- mkReg(0);
   method Action put(Bit#(8) aa, Bit#(8) ab, Bit#(8) zz, Bit#(8) zy);
      r1 <= aa + ab;
      r2 <= (aa + ab) ^ 8'h5a;
      r3 <= zz + zy;
      r4 <= (zz + zy) ^ 8'h5a;
   endmethod
endmodule

endpackage
