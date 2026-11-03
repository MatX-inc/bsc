// Register conflicts and a non-trivial schedule.
//
//  * bump and clear conflict in both directions (each reads cnt, which the
//    other writes).  descending_urgency makes bump the more urgent, so clear
//    is blocked only in cycles where bump can fire:
//        WILL_FIRE_RL_clear = CAN_FIRE_RL_clear && !WILL_FIRE_RL_bump
//  * snapshot reads cnt, so it executes before bump and clear; it conflicts
//    with neither and fires in the same cycle as either.
//  * bump and shadow both write lastOp (write SBR write).  Both can fire in
//    one cycle; bsc orders shadow before bump (G0036), so bump's write wins
//    (G0117 action shadowing), which AState builds as a priority mux on
//    lastOp's write argument.
// Checked with bsc 2026.07-6: execution order shadow, snapshot, bump, clear;
// clear's blocking rules: bump.
//
// Compile: bsc -u -verilog -elab -show-schedule UrgencyBlocking.bsv
(* synthesize *)
module mkUrgencyBlocking(Empty);
   Reg#(Bit#(8)) cnt    <- mkReg(0);
   Reg#(Bit#(8)) snap   <- mkReg(0);
   Reg#(Bool)    go     <- mkReg(True);
   Reg#(Bit#(2)) lastOp <- mkReg(0);

   (* descending_urgency = "bump, clear" *)
   rule bump (go);
      cnt    <= cnt + 1;
      lastOp <= 1;
   endrule

   rule clear (cnt > 5);
      cnt <= 0;
      go  <= !go;
   endrule

   rule snapshot;
      snap <= cnt;
   endrule

   rule shadow (snap > 2);
      lastOp <= 2;
   endrule
endmodule
