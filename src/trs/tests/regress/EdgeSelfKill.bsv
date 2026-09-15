// INVARIANT GUARD (review-fleet critical finding, fixed by
// pre-eviction): a self-killing consumer must not read a hoisted
// pre-body value.  Empirically this shape does NOT fail even on the
// pre-fix emitter, because bsc's tsort emits a positioned body
// Stmt::Def for any def the body's own actions can change — the
// body recomputes and never consults the edge cache.  The test
// pins that whole contract (bsc positioning + our pre-eviction):
// if either side regresses, r2 sees 8'h55 instead of 8'hAA.
(* synthesize *)
module sysEdgeSelfKill();
  Reg#(Bit#(16))  cyc  <- mkReg(0);
  Reg#(Bit#(8))   acc1 <- mkReg(0);
  Reg#(Bit#(8))   acc2 <- mkReg(0);
  RWire#(Bit#(8)) w    <- mkUnsafeRWire;

  Bit#(8) d = fromMaybe(8'h55, w.wget());

  (* execution_order = "r1, r2" *)
  rule r1;
    acc1 <= acc1 ^ d;      // consumer 1: pre-wset, must see 8'h55
  endrule
  rule r2;
    w.wset(8'hAA);
    acc2 <= acc2 ^ d;      // consumer 2 after wset: must see 8'hAA
  endrule
  rule tick;
    cyc <= cyc + 1;
    if (cyc == 4) begin
      $display("acc1=%h acc2=%h", acc1, acc2);
      $finish(0);
    end
  endrule
endmodule
