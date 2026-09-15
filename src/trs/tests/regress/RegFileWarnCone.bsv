// RegFileWarnCone.bsv — RegFile bounds warnings from a rule GUARD
// (sched-side CF cone), reached through two mux paths.
//
// The warning a partial-range RegFile prints on an out-of-bounds read
// is an EVALUATION side effect: byte parity with Bluesim requires the
// compiled code to evaluate the read exactly as often — and in the
// same position — as the interpreter's eager schedule-position latch.
// Two historical failure modes, both caught here:
//   1. count: lazy_mux discards the ssa memo per arm, so a shared def
//      referenced from both mux arms re-expanded (and re-warned) once
//      per arm (sysMips: 116 warnings vs 66);
//   2. order: cones evaluated on-reference instead of latching the
//      eager list first, so warnings interleaved differently within
//      an instant.
import RegFile::*;

(* synthesize *)
module sysRegFileWarnCone(Empty);
   RegFile#(Bit#(4), Bit#(8)) rf <- mkRegFile(1, 10);
   Reg#(Bit#(4)) a   <- mkReg(0);     // 0 is below lo=1: OOB read
   Reg#(Bit#(8)) acc <- mkReg(0);
   Reg#(Bit#(4)) c   <- mkReg(0);

   rule step ((c[0] == 1 ? rf.sub(a) : 8'h7F)
            + (c[1] == 1 ? rf.sub(a) : 8'h3F) != 0);
      acc <= acc + zeroExtend(c);
      c <= c + 1;
      a <= (a == 10) ? 0 : a + 1;
   endrule

   rule fin (c == 15);
      $display("acc %h", acc);
      $finish(0);
   endrule
endmodule
