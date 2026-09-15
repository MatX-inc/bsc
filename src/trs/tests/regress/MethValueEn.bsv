// rung-40 EN liveness, the PRUNING direction: an ActionValue method
// whose body and result cones read a sibling-set wire.  Under -sim
// the wire stays an RWire PRIM, so both walkers reach it as prim
// method calls (wget/whas), never as a Port(EN_ping) read — EN_ping
// is referenced only by the keep-fires method-WF defs in the table,
// i.e. it is LEGITIMATELY table-read-only and the fast plan prunes
// its slot (the census pin below asserts read=1 live=0 alloc=0, and
// the de-circularized census must SHOW that row).  What this fixture
// pins at runtime: the Expr::MethValue consumers (compiled
// value_call, interp call_value) across all three tiers, and the
// is_action-keyed memo rule that a value-context visit must never
// suppress a later action visit's body walk.  Byte parity vs Bluesim
// on interp, hybrid jit, and the aot artifact.

interface MvSub;
   method Action ping(Bit#(8) v);
   method ActionValue#(Bit#(8)) grab();
endinterface

(* synthesize *)
module mkMethValueEnSub(MvSub);
   RWire#(Bit#(8)) w <- mkRWire;
   Reg#(Bit#(8)) acc <- mkReg(0);

   method Action ping(Bit#(8) v);
      w.wset(v);
   endmethod

   method ActionValue#(Bit#(8)) grab();
      // body reads the wire through the mux (RWire prim methods)
      acc <= acc + fromMaybe(0, w.wget);
      // result cone reads the wire too — evaluated at every consumer
      // through the MethValue path
      return fromMaybe(8'hAA, w.wget) + acc;
   endmethod
endmodule

(* synthesize *)
module sysMethValueEn(Empty);
   MvSub s <- mkMethValueEnSub;
   Reg#(Bit#(8)) cnt <- mkReg(0);
   Reg#(Bit#(8)) got <- mkReg(0);

   rule tick;
      cnt <= cnt + 1;
      if (cnt == 20) begin
         $display("got=%0d", got);
         $finish(0);
      end
   endrule

   // wset SB wget forces doPing SB doGrab — a static schedule; both
   // EN_ping=1 (odd) and EN_ping=0 (even) arms are byte-compared
   rule doPing (cnt[0] == 1);
      s.ping(cnt);
   endrule

   // no cnt read here: tick reads got (below) so doGrab must follow
   // tick, and a cnt read would also force it before — schedule-dead
   rule doGrab;
      let v <- s.grab();
      got <= got + v;
      $display("grab=%0d", v);
   endrule
endmodule
