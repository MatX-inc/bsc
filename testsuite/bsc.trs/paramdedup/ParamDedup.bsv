package ParamDedup;

// One synthesized module instantiated at TWO parameter valuations.
//
// Compiled code is shared between instances of a module type, and a
// boundary method fn used to be emitted once per TYPE -- on the
// reasoning that instances of a type share their slot offsets.  They
// do; but the body also bakes the exemplar's PARAMETERS, which
// instances of a type do not share.  Both instances below then ran a
// body carrying k=3, and the design gave a wrong answer with no
// diagnostic.  Nothing in the corpus instantiated one synthesized
// module at two valuations, so nothing caught it.
//
// `k` has to reach a METHOD body, not just a rule: the rule bodies
// are per-instance already.  The interpreter and Bluesim both say 32.

interface Ifc;
   method Action put(Bit#(16) x);
   method Bit#(16) get();
endinterface

(* synthesize *)
module mkScaled #(parameter Bit#(16) k) (Ifc);
   Reg#(Bit#(16)) acc <- mkReg(0);
   method Action put(Bit#(16) x);
      acc <= (acc ^ (x * k)) + 1;
   endmethod
   method Bit#(16) get() = acc;
endmodule

(* synthesize *)
module sysParamDedup();
   Ifc a <- mkScaled(3);
   Ifc b <- mkScaled(7);
   Reg#(Bit#(8)) n <- mkReg(0);

   rule step;
      n <= n + 1;
      a.put(zeroExtend(n));
      b.put(zeroExtend(n));
      $display("a=%0d b=%0d", a.get(), b.get());
      if (n == 8) $finish(0);
   endrule
endmodule

endpackage
