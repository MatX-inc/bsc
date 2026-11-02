// A CAN_FIRE signal: two rules with explicit conditions write register
// a, so the scheduler orders them and the WILL_FIRE of the less urgent
// one differs from its CAN_FIRE; both defs are created from the rule by
// the schedule stage, and the backend inlines the CAN_FIRE into the
// WILL_FIRE, whose record stands for the rule.
(* synthesize *)
module sysMetaFire(Empty);
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(0);
   Reg#(UInt#(8)) c <- mkReg(0);

   rule up (b != 0);
      a <= a + b;
   endrule

   rule down (c > 5);
      a <= a - c;
   endrule

   rule count;
      b <= b + 1;
      c <= c + 2;
   endrule
endmodule
