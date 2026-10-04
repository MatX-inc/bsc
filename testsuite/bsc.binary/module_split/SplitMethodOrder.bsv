package SplitMethodOrder;

interface MethodOrder;
   method Action put(Bit#(8) value);
   method Bit#(8) get;
endinterface

// Disabling relaxed method earliness records method-before-rule relations
// between the interface methods and this module's internal rule.
(* synthesize *)
module mkSplitMethodOrder(MethodOrder);
   Reg#(Bit#(8)) value <- mkReg(0);

   rule tick;
      value <= value + 1;
   endrule

   method Action put(Bit#(8) next);
      value <= next;
   endmethod

   method Bit#(8) get = value;
endmodule

endpackage
