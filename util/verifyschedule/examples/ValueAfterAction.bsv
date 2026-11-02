interface VA;
   method Action  put(Bit#(8) x);
   method Bit#(8) cur;
   method Bit#(8) fwd;
endinterface

// Value methods reading state an action method of the same
// module writes.
//  * cur reads register r: it sees the value from before this cycle; commit
//    (the only writer of r) executes after it
//  * fwd reads the RWire put sets: put SBR fwd, and the output fwd depends
//    combinationally on put_x / EN_put in the same cycle
//  * commit is a rule between put and the register update
//
// Checked (bsc 2026.07-6): put SBR fwd, put CF cur, put C put;
// order put, cur, fwd, commit.
//
// Compile: bsc -u -verilog -elab -show-schedule ValueAfterAction.bsv
(* synthesize *)
module mkValueAfterAction(VA);
   Reg#(Bit#(8))   r  <- mkReg(0);
   RWire#(Bit#(8)) bp <- mkRWire;

   rule commit (bp.wget matches tagged Valid .v);
      r <= v;
   endrule

   method Action put(Bit#(8) x);
      bp.wset(x);
   endmethod

   method Bit#(8) cur = r;
   method Bit#(8) fwd = fromMaybe(r, bp.wget);
endmodule
