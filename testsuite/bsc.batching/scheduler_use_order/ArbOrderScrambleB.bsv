package ArbOrderScrambleB;

import RegFile::*;

// The probe with read1's index expression changed, so this package interns
// the other five index def names and not that one.
(* synthesize *)
module mkArbOrderScrambleB(Empty);
   RegFile#(Bit#(3), Bit#(8)) rf <- mkRegFileFull;
   Reg#(Bit#(3)) idx <- mkReg(0);
   Reg#(Bit#(8)) out1 <- mkRegU;
   Reg#(Bit#(8)) out2 <- mkRegU;
   Reg#(Bit#(8)) out3 <- mkRegU;
   Reg#(Bit#(8)) out4 <- mkRegU;
   Reg#(Bit#(8)) out5 <- mkRegU;
   Reg#(Bit#(8)) out6 <- mkRegU;
   rule read1; out1 <= rf.sub(idx + 7); endrule
   rule read2; out2 <= rf.sub(idx + 2); endrule
   rule read3; out3 <= rf.sub(idx + 3); endrule
   rule read4; out4 <= rf.sub(idx + 4); endrule
   rule read5; out5 <= rf.sub(idx + 5); endrule
   rule read6; out6 <= rf.sub(idx + 6); endrule
endmodule

endpackage
