// A schedule that relies on a disjointness fact about method uses, which
// bsc's solver proves but the .ba does not record.
//  * up writes r, and fires only when mode < 3
//  * emit reads r only when mode > 9, and writes z, which up reads, so up
//    executes first: emit reads r after up wrote it, which r's annotations
//    (read SB write) forbid, and bsc allows it because the two uses never
//    happen in one cycle
//  * the comparisons are datapath operators, so only the solver's fact
//    (solver_k in section 5) shows that the value emit reads, arbitrary
//    when up wrote r first, does not matter
//
// Checked (bsc 2026.07-13): order up, emit, tick; all always fire.
//
// Compile: bsc -u -verilog -elab -show-schedule UseFact.bsv
(* synthesize *)
module mkUseFact(Empty);
   Reg#(Bit#(4)) mode <- mkReg(0);
   Reg#(Bit#(8)) r    <- mkReg(0);
   Reg#(Bit#(8)) z    <- mkReg(0);
   Reg#(Bit#(8)) out  <- mkReg(0);

   rule up (mode < 3);
      r <= z + 1;
   endrule

   rule emit;
      if (mode > 9) out <= r;
      z <= z + 2;
   endrule

   rule tick;
      mode <= mode + 1;
   endrule
endmodule
