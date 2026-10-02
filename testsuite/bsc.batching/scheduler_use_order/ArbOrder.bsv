package ArbOrder;

import RegFile::*;

// Six rules read the register file, and it has five read ports, so under
// -resource-simple the scheduler arbitrates: it makes two of the rules
// conflict, and which two it picks was decided by the order of the uses,
// which was Ord on the names of the index expressions' defs.  Elaboration
// meets the uses in rule order whatever the process compiled before.
(* synthesize *)
module mkArbOrder(Empty);
   RegFile#(Bit#(3), Bit#(8)) rf <- mkRegFileFull;
   Reg#(Bit#(3)) idx <- mkReg(0);
   Reg#(Bit#(8)) out1 <- mkRegU;
   Reg#(Bit#(8)) out2 <- mkRegU;
   Reg#(Bit#(8)) out3 <- mkRegU;
   Reg#(Bit#(8)) out4 <- mkRegU;
   Reg#(Bit#(8)) out5 <- mkRegU;
   Reg#(Bit#(8)) out6 <- mkRegU;
   rule read1; out1 <= rf.sub(idx + 1); endrule
   rule read2; out2 <= rf.sub(idx + 2); endrule
   rule read3; out3 <= rf.sub(idx + 3); endrule
   rule read4; out4 <= rf.sub(idx + 4); endrule
   rule read5; out5 <= rf.sub(idx + 5); endrule
   rule read6; out6 <= rf.sub(idx + 6); endrule
endmodule

endpackage
