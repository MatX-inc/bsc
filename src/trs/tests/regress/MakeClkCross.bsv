// Coincident MakeClock-domain edges + a CrossingReg sampled across them
// (the matx mkClkGate2Bool shape): a CrossingReg toggle in domain A
// (crossed-side = domain B), a domain-B register sampling .crossed, and
// the gate compare exported to the default domain via mkNullCrossingWire,
// whose write rule is a clock_crossing_rule in domain B's after-edge
// (combo-sched) pass.  Pins two semantics: a crossed read backdates to
// the pre-edge value for destination-domain LOGIC only — the combo pass
// reads post-edge (MOD_Reg::METH_crossed's !bk_is_combo_sched arm) — and
// data defs consumed by early rules recompute at post-edge state rather
// than serving edge-time slot values.
import Clocks::*;

(* synthesize *)
module sysMakeClkCross(Empty);
   MakeClockIfc#(Bool) aclkIfc <- mkUngatedClock(False);
   Clock aclk = aclkIfc.new_clk;
   Reset arst <- mkAsyncResetFromCR(0, aclk);

   MakeClockIfc#(Bool) bclkIfc <- mkUngatedClock(False);
   Clock bclk = bclkIfc.new_clk;
   Reset brst <- mkAsyncResetFromCR(0, bclk);

   // domain-A toggle in a CrossingReg whose crossed side is domain B
   CrossingReg#(Bool) tog <- mkNullCrossingReg(bclk, False, clocked_by aclk, reset_by arst);
   rule tog_r;
      tog <= !tog;
   endrule

   // domain-B delayed sample of the crossed value (starts True, like
   // mkClkGate2Bool's toggleDly)
   Reg#(Bool) dly <- mkReg(True, clocked_by bclk, reset_by brst);
   rule dly_r;
      dly <= tog.crossed;
   endrule

   // the gate-detector output: a domain-B combinational value
   Bool gate = (tog.crossed != dly);

   Clock cc <- exposeCurrentClock;
   ReadOnly#(Bool) gateV <- mkNullCrossingWire(cc, gate);
   ReadOnly#(Bool) togV  <- mkNullCrossingWire(cc, tog.crossed);
   ReadOnly#(Bool) dlyV  <- mkNullCrossingWire(cc, dly);

   Reg#(UInt#(8)) cyc <- mkReg(0);
   rule drive;
      cyc <= cyc + 1;
      Bool lvl = (pack(cyc)[0] == 1);
      aclkIfc.setClockValue(lvl);
      bclkIfc.setClockValue(lvl);
      $display("cyc %0d lvl=%b tog=%b dly=%b gate=%b", cyc, lvl, togV, dlyV, gateV);
      if (cyc == 12) $finish(0);
   endrule
endmodule
