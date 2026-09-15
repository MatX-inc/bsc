// Dynamic scheduling v1 (bsc G0100 class): the parent's rule pair calls
// submodule methods with a rule between them (put SB r SB get), while a
// parent wire forces the opposite static order (doGet SB doPut).  The
// pair's CAN_FIREs are disjoint (cnt[0]), so no cycle activates both
// constraints; -sched-dynamic records the pair and the trs runtime picks
// the interleaving per edge from doPut's inlined CAN_FIRE.
//
// Without -sched-dynamic this design is a bsc error (G0100) for the
// Bluesim backend; with it, only the trs backend can execute the result.

import FIFO::*;

interface Sub;
   method Action put(Bit#(8) v);
   method Bit#(8) get();
endinterface

(* synthesize *)
module mkDynSchedSub(Sub);
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
module sysDynSched(Empty);
   Sub s <- mkDynSchedSub;
   Reg#(Bit#(8)) cnt <- mkReg(0);
   // the static-order forcer: doGet SB mid SB doPut.  A direct wire
   // between doGet and doPut would not order them — bsc drops ordering
   // constraints between disjoint rules — so it routes through mid,
   // which fires every cycle and is disjoint with neither.
   RWire#(Bit#(8)) pw1 <- mkRWire;
   RWire#(Bit#(8)) pw2 <- mkRWire;

   rule tick;
      cnt <= cnt + 1;
      if (cnt == 10) $finish(0);
   endrule

   // must execute after s.r (get reads the wire r writes)
   rule doGet (cnt < 10 && cnt[0] == 0);
      $display("%0d: get = %0d", cnt, s.get());
      pw1.wset(s.get());
   endrule

   rule mid;
      pw2.wset(fromMaybe(0, pw1.wget()));
   endrule

   // must execute before s.r (put writes the wire r reads); reading pw2
   // only feeds $display — feeding it to put would be a real
   // combinational cycle through the child's put->get path
   rule doPut (cnt < 10 && cnt[0] == 1);
      s.put(cnt);
      $display("%0d: put %0d (saw %0d)", cnt, cnt, fromMaybe(0, pw2.wget()));
   endrule
endmodule
