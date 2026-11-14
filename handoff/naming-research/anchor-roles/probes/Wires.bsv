package Wires;
// Inlined wires and CReg.
//  w$wget == r == dw$wget, w$whas == WILL_FIRE_RL_set == p$whas (== 1)
import Vector::*;
(* synthesize *)
module sysWires(Empty);
   RWire#(Bit#(8)) w  <- mkRWire;
   PulseWire       p  <- mkPulseWire;
   Wire#(Bit#(8))  dw <- mkDWire(0);
   Wire#(Bit#(8))  bw <- mkBypassWire;
   Reg#(Bit#(8))   r  <- mkReg(0);
   Reg#(Bit#(8))   q  <- mkReg(0);
   Reg#(Bit#(8))   cr[2] <- mkCReg(2, 0);
   rule set;
      w.wset(r);
      p.send;
      dw <= r;
      bw <= r;
   endrule
   rule use_ (w.wget matches tagged Valid .v);
      q <= v + dw + bw;
   endrule
   rule usep (p);
      r <= r + 1;
   endrule
   rule c0;
      cr[0] <= cr[0] + 1;
   endrule
   rule c1;
      cr[1] <= cr[1] + 2;
   endrule
endmodule
endpackage
