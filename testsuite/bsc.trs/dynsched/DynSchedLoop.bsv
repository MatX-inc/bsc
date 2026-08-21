// Dynamic scheduling: a loop through multiple pairs (bsc G0116).  Each
// pair alone is satisfiable (its implied edge has no opposing static
// path), but the two implied edges close a loop with the parent's
// static order (a -> b -> mid1 -> c -> d -> mid2 -> a).  With
// -sched-dynamic the pairs on the loop convert to dynamically
// scheduled pairs; guards conjoin per state combination and the
// runtime picks the interleaving per edge (first match wins).
//
// The per-cycle display order between the two put chains (a/s1.r vs
// c/s2.r) is not constrained; the golden pins the linker's
// deterministic choice, and every line's VALUE is hand-derived
// (s1.acc accumulates cnt: 1,4,9,16; s2.acc accumulates cnt+1:
// 2,6,12,20; gets read the same-cycle wire of an idle rule: 0).

interface SubL;
   method Action put(Bit#(8) v);
   method Bit#(8) get();
endinterface

(* synthesize *)
module mkDynSchedLoopSub(SubL);
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
module sysDynSchedLoop(Empty);
   SubL s1 <- mkDynSchedLoopSub;
   SubL s2 <- mkDynSchedLoopSub;
   Reg#(Bit#(8)) cnt <- mkReg(0);
   // static forcers: b SB mid1 SB c, and d SB mid2 SB a (mid rules
   // fire every cycle, so their ordering wires are never dropped)
   RWire#(Bit#(8)) pw1 <- mkRWire;
   RWire#(Bit#(8)) pw2 <- mkRWire;
   RWire#(Bit#(8)) pw3 <- mkRWire;
   RWire#(Bit#(8)) pw4 <- mkRWire;

   rule tick;
      cnt <= cnt + 1;
      if (cnt == 8) $finish(0);
   endrule

   rule a (cnt < 8 && cnt[0] == 1);
      s1.put(cnt);
      $display("%0d: a put %0d saw %0d", cnt, cnt, fromMaybe(0, pw4.wget()));
   endrule

   rule b (cnt < 8 && cnt[0] == 0);
      $display("%0d: b get = %0d", cnt, s1.get());
      pw1.wset(s1.get());
   endrule

   rule mid1;
      pw2.wset(fromMaybe(0, pw1.wget()));
   endrule

   rule c (cnt < 8 && cnt[0] == 1);
      s2.put(cnt + 1);
      $display("%0d: c put %0d saw %0d", cnt, cnt + 1, fromMaybe(0, pw2.wget()));
   endrule

   rule d (cnt < 8 && cnt[0] == 0);
      $display("%0d: d get = %0d", cnt, s2.get());
      pw3.wset(s2.get());
   endrule

   rule mid2;
      pw4.wset(fromMaybe(0, pw3.wget()));
   endrule
endmodule
