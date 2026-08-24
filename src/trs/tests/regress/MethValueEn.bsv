// rung-40 EN liveness: an ActionValue method whose BODY and RESULT
// cones both read a sibling method's enable — inside the synthesized
// child the RWire's whas IS the EN_ping input port, so grab's cones
// carry Port(EN_ping) reads that reach lowering through value_call
// (Expr::MethValue) as well as the action call.  Pins the walker's
// MethValue recursion and the memo rule that a value-context visit
// must never suppress a later action visit's body walk.  Byte parity
// vs Bluesim on all three tiers (interp, hybrid jit, aot artifact).

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
      // body reads EN_ping (the wire's whas) through the mux
      acc <= acc + fromMaybe(0, w.wget);
      // result cone reads EN_ping too — this is the read that must
      // stay live through the MethValue path
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
