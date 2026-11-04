// Same-cycle forwarding between rules through every wire kind.
// produce (and tick, for the BypassWire) write; consume_w / consume_rest read
// in the same cycle, so they execute after the writers (wset SBR wget/whas).
//
//   rw : mkRWire       -> RWire  prim (wset / wget / whas)
//   w  : mkWire        -> RWire  prim; reading w adds the implicit condition whas
//   dw : mkDWire(dflt) -> RWire  prim; reads the default when not written
//   pw : mkPulseWire   -> RWire0 prim (wset / whas only)
//   bw : mkBypassWire  -> BypassWire prim; enable is (*inhigh*), so it must be
//                         written every cycle (tick never blocks)
//
// Checked (bsc 2026.07-6): order produce, tick, consume_w, consume_rest;
// consume_w's predicate is w.whas; no warnings.
//
// Compile: bsc -u -verilog -elab -show-schedule WireKinds.bsv
(* synthesize *)
module mkWireKinds(Empty);
   Reg#(Bit#(8)) src   <- mkReg(0);
   Reg#(Bit#(8)) sinkW <- mkReg(0);
   Reg#(Bit#(8)) sink  <- mkReg(0);
   Reg#(Bool)    flag  <- mkReg(False);

   RWire#(Bit#(8)) rw <- mkRWire;
   Wire#(Bit#(8))  w  <- mkWire;
   Wire#(Bit#(8))  dw <- mkDWire(8'hFF);
   PulseWire       pw <- mkPulseWire;
   Wire#(Bit#(8))  bw <- mkBypassWire;

   rule tick;
      src <= src + 1;
      bw  <= src;
   endrule

   rule produce (src[0] == 1);
      rw.wset(src);
      w  <= src + 1;
      dw <= src + 2;
      pw.send;
   endrule

   // fires only in cycles where produce fired (implicit condition w.whas)
   rule consume_w;
      sinkW <= w;
   endrule

   // no implicit conditions: sees Invalid / default / False when produce is idle
   rule consume_rest;
      sink <= fromMaybe(0, rw.wget) + dw + bw;
      flag <= pw;
   endrule
endmodule
