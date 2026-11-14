package SubIfc;
// Shared interface and submodule bodies. mkSub is synthesized, mkSubI is the
// same body inlined into its parent.
export SubIfc(..), mkSub, mkSubI;

interface SubIfc;
   method Action put(Bit#(8) v);
   method Bit#(8) get;                 // RDY constant True, result == r
   method ActionValue#(Bit#(8)) take;  // result == r too, RDY == full
endinterface

module mkSubBody(SubIfc);
   Reg#(Bit#(8)) r    <- mkReg(0);
   Reg#(Bool)    full <- mkReg(False);
   method Action put(Bit#(8) v) if (!full);   // argument == r$D_IN, EN_put == r$EN
      r <= v;
      full <= True;
   endmethod
   method Bit#(8) get = r;
   method ActionValue#(Bit#(8)) take() if (full);
      full <= False;
      return r;
   endmethod
endmodule

(* synthesize *)
module mkSub(SubIfc);
   let _i <- mkSubBody;
   return _i;
endmodule

module mkSubI(SubIfc);
   let _i <- mkSubBody;
   return _i;
endmodule
endpackage
