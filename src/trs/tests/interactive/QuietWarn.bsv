// Quiet-engine diagnostic witness: an unguarded enqueue overflows a
// 2-deep FIFO, firing the reference's "Enqueuing to a full fifo"
// warning every cycle after it fills.  Under a dual-engine oracle
// session the QUIET secondary must not duplicate the warning lines —
// stdout stays byte-identical to the single-engine reference.
import FIFOF::*;

(* synthesize *)
module sysQuietWarn();
   Reg#(UInt#(8)) cyc <- mkReg(0);
   FIFOF#(Bit#(8)) f <- mkUGFIFOF;

   rule count;
      cyc <= cyc + 1;
      if (cyc == 8) $finish(0);
   endrule

   rule stuff;
      f.enq(pack(cyc)[7:0]);
   endrule
endmodule
