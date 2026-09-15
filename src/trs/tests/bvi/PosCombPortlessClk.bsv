// R5: a combinational import on a PORTLESS clock -- the shape a
// vendor combinational IP block takes.  `default_clock clk()' places the
// methods in a clock domain without wiring a port, which is what
// combinational IP does (BSV ref guide, input_clock); the module has no
// clock input and no state to commit, and the declared paths carry the
// arcs.  Distinct from an Action clocked_by(no_clock), which bsc itself
// calls unusable (P0172) and which stays refused.
interface Ifc;
   (* always_ready, always_enabled *) method Action a(Bit#(8) v);
   (* always_ready, always_enabled *) method Action b(Bit#(8) v);
   (* always_ready *) method Bit#(8) s();
endinterface

import "BVI" CombXbar =
module mkCombXbar(Ifc);
   default_clock clk();
   no_reset;
   method a(a) enable((*inhigh*) en_a);
   method b(b) enable((*inhigh*) en_b);
   method s s();
   schedule a C a;
   schedule b C b;
   schedule a CF b;
   schedule (a, b) SBR s;
   schedule s CF s;
   path (a, s);
   path (b, s);
endmodule

(* synthesize *)
module sysPosCombPortlessClk();
   Ifc d <- mkCombXbar;
   Reg#(Bit#(4)) n <- mkReg(0);
   Wire#(Bit#(8)) av <- mkDWire(0);
   Wire#(Bit#(8)) bv <- mkDWire(0);

   // the always_enabled inputs are driven every cycle, as the real
   // wrapper's users do from an `always' block
   rule feed;
      d.a(av);
      d.b(bv);
   endrule

   rule stim;
      av <= zeroExtend(n) + 8'h10;
      bv <= 8'h07;
   endrule

   rule show;
      $display("s=%h", d.s());
      n <= n + 1;
      if (n == 3) $finish(0);
   endrule
endmodule
