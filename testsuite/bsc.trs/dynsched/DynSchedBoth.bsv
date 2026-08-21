// Dynamic scheduling: both directions (bsc G0101).  r1's put must
// precede s1's internal rule whose output r2's get reads, AND r2's put
// must precede s2's internal rule whose output r1's get reads — the
// pair is order-constrained in both directions, with no static order
// satisfying either side.  CAN_FIREs are disjoint (cnt[0]), so each
// cycle at most one direction is active: -sched-dynamic records both
// rules' guards and the runtime picks the interleaving per edge.

interface SubB;
   method Action put(Bit#(8) v);
   method Bit#(8) get();
endinterface

(* synthesize *)
module mkDynSchedBothSub(SubB);
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
module sysDynSchedBoth(Empty);
   SubB s1 <- mkDynSchedBothSub;
   SubB s2 <- mkDynSchedBothSub;
   Reg#(Bit#(8)) cnt <- mkReg(0);

   rule tick;
      cnt <= cnt + 1;
      if (cnt == 8) $finish(0);
   endrule

   rule r1 (cnt < 8 && cnt[0] == 1);
      s1.put(cnt);
      $display("%0d: r1 put %0d saw %0d", cnt, cnt, s2.get());
   endrule

   rule r2 (cnt < 8 && cnt[0] == 0);
      s2.put(cnt);
      $display("%0d: r2 put %0d saw %0d", cnt, cnt, s1.get());
   endrule
endmodule
