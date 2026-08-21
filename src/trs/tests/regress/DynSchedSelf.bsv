// Dynamic scheduling: one rule, both flagged calls (bsc G0096).  The
// rule's put must precede the submodule's internal rule and its get
// must follow it — the rule needs to execute on both sides of s.r
// depending on which call is active.  The call conditions are disjoint
// (cnt[0]), so per cycle at most one side's constraint applies:
// -sched-dynamic records the rule's predicate AND the put condition as
// the guard, and only the inactive call's fused edges drop (the rule
// itself executes either way).

interface SubS;
   method Action put(Bit#(8) v);
   method Bit#(8) get();
endinterface

(* synthesize *)
module mkDynSchedSelfSub(SubS);
   RWire#(Bit#(8)) w1 <- mkRWire;
   Wire#(Bit#(8))  w2 <- mkDWire(0);
   Reg#(Bit#(8))  acc <- mkReg(0);

   rule r (w1.wget matches tagged Valid .v);
      acc <= acc + v;
      w2 <= acc + v;
      $display("r: acc <= %0d", acc + v);
   endrule

   method Action put(Bit#(8) v);
      w1.wset(v);
   endmethod
   method Bit#(8) get();
      return w2;
   endmethod
endmodule

(* synthesize *)
module sysDynSchedSelf(Empty);
   SubS s <- mkDynSchedSelfSub;
   Reg#(Bit#(8)) cnt <- mkReg(0);

   rule tick;
      cnt <= cnt + 1;
      if (cnt == 8) $finish(0);
   endrule

   rule both (cnt < 8);
      if (cnt[0] == 1)
         s.put(cnt);
      else
         $display("%0d: get = %0d", cnt, s.get());
   endrule
endmodule
