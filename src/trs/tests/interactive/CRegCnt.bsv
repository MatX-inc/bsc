// Oracle state-surface witness for the symbol-less prims: Counter
// and CReg register NO debug symbols in the reference (so `sim ls`
// shows nothing for them), but the oracle state compare reads them
// via state_children — a dual-engine session cross-checks their
// registered values at every stop.
import Counter::*;

(* synthesize *)
module sysCRegCnt();
   Reg#(UInt#(8)) cyc <- mkReg(0);
   Counter#(8) cnt <- mkCounter(0);
   Array#(Reg#(Bit#(8))) cr <- mkCReg(3, 0);
   Reg#(Bit#(8)) sink <- mkReg(0);

   rule tick;
      cyc <= cyc + 1;
      cnt.up;
      if (cyc == 40) $finish(0);
   endrule

   rule w0;
      cr[0] <= cr[0] + 1;
   endrule

   rule w1;
      // port 1 sees port 0's same-cycle write
      cr[1] <= cr[1] + 2;
   endrule

   rule readout;
      sink <= cr[2] ^ pack(cnt.value);
      if (cyc == 20) $display("mid %0d %0d", cr[2], cnt.value);
   endrule
endmodule
