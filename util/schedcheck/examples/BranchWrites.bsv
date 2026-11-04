// One rule writes the same register under disjoint conditions.
//  * if/else on sel writes r in both arms.  With the default -lift, ILift
//    merges the two calls into one r.write(sel ? r + 1 : s); with -no-lift,
//    ACleanup merges them instead, adding defs _dfooN (cond1 || cond2) and
//    _dfooM (cond1 ? arg1 : arg2).
//  * two separate ifs on mode == 1 / mode == 2 write s.  They are not
//    if/else arms, so ILift leaves two calls; ACleanup proves the conditions
//    disjoint and merges them into one call.
// Either way the .ba holds a single s.write / r.write call per rule.
//
// Checked (bsc 2026.07-6): default flags give r.write IF_sel_THEN_r_PLUS_1_ELSE_s___d5
// and (if _dfoo1 then s.write _dfoo2); -no-lift gives _dfoo merges for both.
//
// Compile: bsc -u -verilog -elab -show-schedule BranchWrites.bsv
//          bsc -u -verilog -elab -no-lift -show-schedule BranchWrites.bsv
(* synthesize *)
module mkBranchWrites(Empty);
   Reg#(Bool)    sel  <- mkReg(False);
   Reg#(Bit#(2)) mode <- mkReg(0);
   Reg#(Bit#(8)) r    <- mkReg(0);
   Reg#(Bit#(8)) s    <- mkReg(0);

   rule step;
      if (sel)
         r <= r + 1;
      else
         r <= s;
      if (mode == 1) s <= r;
      if (mode == 2) s <= ~r;
      sel  <= !sel;
      mode <= mode + 1;
   endrule
endmodule
