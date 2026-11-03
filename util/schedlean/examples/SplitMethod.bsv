interface SplitMethod;
   method Action put(Bit#(8) x);
   method Action setC(Bit#(8) x);
   method Bit#(8) peek;
endinterface

// An action method that bsc splits into two rules, one per branch of
// (* split *): put_T writes a, put_F writes b.  The exported schedule names
// the method, not its rules: put CF setC, so the swapped order runs both of
// put's rules after setC; and peek SB put, which the check of the execution
// order verifies for each of put's rules.
//
// Compile: bsc -u -verilog -elab -show-schedule SplitMethod.bsv
(* synthesize *)
module mkSplitMethod(SplitMethod);
   Reg#(Bool)    sel <- mkReg(False);
   Reg#(Bit#(8)) a   <- mkReg(0);
   Reg#(Bit#(8)) b   <- mkReg(0);
   Reg#(Bit#(8)) c   <- mkReg(0);

   rule flip;
      sel <= !sel;
   endrule

   method Action put(Bit#(8) x);
      (* split *)
      if (sel) a <= x;
      else b <= x;
   endmethod

   method Action setC(Bit#(8) x);
      c <= x;
   endmethod

   method Bit#(8) peek = b;
endmodule
