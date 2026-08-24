// rung-40 EN liveness, the REAL live-EN shape: a child rule that
// conflicts with an exported Action method (both write acc) gets the
// scheduler-generated inhibition CAN_FIRE_bump = ... && !EN_poke, so
// the rule's sched cone reads Port(EN_poke) at runtime — the same
// cross-observation class as Toooba's 13 live enables.  Fast plans
// must keep EN_poke's slot; the battery previously had NO design with
// a live EN read.  Byte parity vs Bluesim on all three tiers.

interface EcSub;
   method Action poke(Bit#(8) v);
   method Bit#(8) look();
endinterface

(* synthesize *)
module mkEnConflictSub(EcSub);
   Reg#(Bit#(8)) acc <- mkReg(0);

   // conflicts with poke (both write acc); poke is more urgent, so
   // CAN_FIRE_bump reads EN_poke
   rule bump;
      acc <= acc + 1;
   endrule

   method Action poke(Bit#(8) v);
      acc <= acc + v;
   endmethod

   method Bit#(8) look();
      return acc;
   endmethod
endmodule

(* synthesize *)
module sysEnConflict(Empty);
   EcSub s <- mkEnConflictSub;
   Reg#(Bit#(8)) cnt <- mkReg(0);

   rule tick;
      cnt <= cnt + 1;
      if (cnt == 20) begin
         $display("acc=%0d", s.look());
         $finish(0);
      end
   endrule

   // poke on odd cycles: bump is inhibited exactly then — acc gains
   // cnt on odd cycles and 1 on even cycles, byte-compared throughout
   rule doPoke (cnt[0] == 1);
      s.poke(cnt);
      $display("%0d: poke %0d saw %0d", cnt, cnt, s.look());
   endrule
endmodule
