// Dynamic scheduling x rung-40 EN liveness: DynSched's G0100 shape
// (put SB r SB get vs the wire-forced doGet SB mid SB doPut, disjoint
// CAN_FIREs) with a LIVE enable inside the alt-carrying composition:
// kick() conflicts with rule r (both write acc), so the scheduler
// puts a real Port(EN_kick) read into r's CAN_FIRE cone — which the
// alternate rows re-expand, so the liveness walk's alternates union
// must keep its slot in fast plans (census: EN_kick read=1 live=1
// alloc=1, while EN_put/EN_peek route through the RWire prim and are
// legitimately pruned).  peek() additionally pins the Expr::MethValue
// consumer from an alt-reordered rule; its takings accumulate in a
// register printed once at $finish so the golden is independent of
// where the schedule places doPeek relative to r.  Goldens are
// hand-derived: no reference Bluesim exists for this class by design.

import FIFO::*;

interface Sub;
   method Action put(Bit#(8) v);
   method Action kick();
   method ActionValue#(Bit#(8)) peek();
   method Bit#(8) get();
endinterface

(* synthesize *)
module mkDynSchedEnSub(Sub);
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

   // conflicts with rule r (both write acc) without changing state:
   // the scheduler inhibition puts a REAL Port(EN_kick) read into r's
   // CAN_FIRE cone — which the alternate rows re-expand, so fast
   // plans must keep its slot through the alts walk
   method Action kick();
      acc <= acc;
   endmethod

   method ActionValue#(Bit#(8)) peek();
      // result cone reads the wire (RWire prim): 1 + acc when put
      // latched this cycle — consumed via MethValue in doPeek
      return (isValid(w1.wget) ? 8'd1 : 8'd0) + acc;
   endmethod

   method Bit#(8) get();
      return w2;
   endmethod
endmodule

(* synthesize *)
module sysDynSchedEn(Empty);
   Sub s <- mkDynSchedEnSub;
   Reg#(Bit#(8)) cnt <- mkReg(0);
   // the static-order forcer, as in DynSched: doGet SB mid SB doPut
   RWire#(Bit#(8)) pw1 <- mkRWire;
   RWire#(Bit#(8)) pw2 <- mkRWire;

   Reg#(Bit#(8)) seen <- mkReg(0);

   rule tick;
      cnt <= cnt + 1;
      if (cnt == 10) begin
         $display("seen=%0d", seen);
         $finish(0);
      end
   endrule

   // put SB peek (wset SB wget-read) schedules this after doPut;
   // peek's value is consumed through the AvAction + Expr::MethValue
   // pair from a rule the dynamic schedule reorders
   rule doPeek (cnt < 10 && cnt[0] == 1);
      let m <- s.peek();
      seen <= seen + m;
   endrule

   // must execute after s.r (get reads the wire r writes)
   rule doGet (cnt < 10 && cnt[0] == 0);
      $display("%0d: get = %0d", cnt, s.get());
      pw1.wset(s.get());
   endrule

   // even cycles only: r fires on odd cycles (put's data), so the
   // EN_kick inhibition in r's CAN_FIRE never actually blocks it and
   // the goldens are unchanged — but the enable is genuinely called
   // (an uncalled EN folds to a constant and allocates no slot)
   rule doKick (cnt < 10 && cnt[0] == 0);
      s.kick();
   endrule

   rule mid;
      pw2.wset(fromMaybe(0, pw1.wget()));
   endrule

   // must execute before s.r (put writes the wire r reads)
   rule doPut (cnt < 10 && cnt[0] == 1);
      s.put(cnt);
      $display("%0d: put %0d (saw %0d)", cnt, cnt, fromMaybe(0, pw2.wget()));
   endrule
endmodule
