// Rule splitting with the (* split *) attribute (no global
// -split-if).  route becomes route_T (x[0] == 1) and route_F (x[0] == 0);
// both halves keep x <= x + 1 and are disjoint with each other.  Each half
// conflicts with a different clear rule; the clear rules are more urgent, so
// route_T is blocked only by clrA and route_F only by clrB.  Without the
// split, route would conflict with both clear rules.
//
// Checked (bsc 2026.07-6): route_T blocked by clrA, route_F by clrB;
// order route_T, clrA, route_F, clrB.
//
// Compile: bsc -u -verilog -elab -show-schedule SplitRule.bsv
// Compare: delete the attribute, or keep it and add -split-if globally.
(* synthesize *)
module mkSplitRule(Empty);
   Reg#(Bit#(8)) x  <- mkReg(0);
   Reg#(Bit#(8)) ya <- mkReg(0);
   Reg#(Bit#(8)) yb <- mkReg(0);

   (* descending_urgency = "clrA, clrB, route" *)
   rule route;
      (* split *)
      if (x[0] == 1)
         ya <= ya + x;
      else
         yb <= yb + x;
      x <= x + 1;
   endrule

   rule clrA (ya > 100);
      ya <= 0;
   endrule

   rule clrB (yb > 100);
      yb <= 0;
   endrule
endmodule
