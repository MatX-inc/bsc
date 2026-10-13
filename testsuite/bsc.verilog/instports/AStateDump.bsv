import FIFOF::*;

// A register written from two rules and a guarded FIFOF whose notFull is
// also read directly: in the -dastate dump the register's mux defines the
// port r$D_IN (the MUX_ value names keep the method spelling r$write_1),
// the FIFO's enable defs are f$ENQ / f$DEQ, and the state outputs list
// FULL_N once although notFull (rule watch) and i_notFull (the enq guard)
// both read it.
(* synthesize *)
module sysAStateDump(Empty);
   FIFOF#(Bit#(8)) f <- mkFIFOF;
   Reg#(Bit#(8)) r <- mkReg(0);
   Reg#(Bool) nf <- mkReg(False);
   Reg#(Bool) ne <- mkReg(False);

   rule rA (r[0] == 0);
      r <= r + 1;
      f.enq(r);
   endrule

   rule rB (r[0] == 1);
      r <= r + 3;
   endrule

   rule watch;
      nf <= f.notFull;
      ne <= f.notEmpty;
   endrule

   rule drain;
      f.deq;
   endrule
endmodule
