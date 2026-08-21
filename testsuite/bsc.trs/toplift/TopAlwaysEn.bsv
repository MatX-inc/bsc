// always_enabled methods on the TOP interface — refused by classic
// Bluesim (EBSimEnablePragma, G0062), lifted for the -trs flow: batch
// mode auto-fires each always_enabled Action method on every cycle at
// its schedule position, EN constant true.  `tick` (no arguments)
// mutates state the rule displays, so the per-cycle firing AND its
// position (the methods' Exec cut follows the rule: show reads count
// before tick writes it) are observable in the values; `setStep`
// takes an argument bound to a constant (+setStep.v=2).  Auto-fire
// designs run INTERPRETED (jit declines: top always_enabled autofire)
// — the sidecar asserts that engine outcome.  Hand-derived: cycle 0
// shows (0,1) then count+=1, step<-2; thereafter count += 2 each
// cycle shown pre-increment: 0,1,3,5,7,9.
interface TopAE;
   method Action tick;
   method Action setStep(Bit#(8) v);
endinterface

(* synthesize, always_enabled = "tick, setStep" *)
module sysTopAlwaysEn(TopAE);
   Reg#(Bit#(16)) count <- mkReg(0);
   Reg#(Bit#(8)) step <- mkReg(1);
   Reg#(Bit#(8)) n <- mkReg(0);

   rule show;
      $display("[%0d] count=%h step=%h", n, count, step);
      n <= n + 1;
      if (n == 5) $finish(0);
   endrule

   method Action tick;
      count <= count + zeroExtend(step);
   endmethod
   method Action setStep(Bit#(8) v);
      step <= v;
   endmethod
endmodule
