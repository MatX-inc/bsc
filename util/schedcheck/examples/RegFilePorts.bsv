import RegFile::*;

interface RFIfc;
   method Bit#(8) peek(Bit#(3) a);
   method Action  poke(Bit#(3) a, Bit#(8) d);
endinterface

// A multi-ported child method.  RegFile.sub has 5 read ports
// (vf_mult = 5; RegFile.bs declares sub[5]); upd has one port (upd C upd).
//  * sum3 reads three different addresses in one rule -> three sub ports
//  * peek (value method, address from the method argument) -> a fourth port
//  * fill and poke share the single upd port and conflict; the method is
//    more urgent by default, so fill is blocked in cycles where EN_poke is set
//  * sub SB upd orders sum3 (and peek) before fill and poke
//
// Checked (bsc 2026.07-6): sub ports peek_a->4, i->3, i+1->2, i+2->1;
// SchedInfo peek SB poke, poke C poke, rulesBeforeMethods
// [(poke, [Left RL_sum3])]; order peek, sum3, poke, fill.
//
// Compile: bsc -u -verilog -elab -show-schedule RegFilePorts.bsv
(* synthesize *)
module mkRegFilePorts(RFIfc);
   RegFile#(Bit#(3), Bit#(8)) rf <- mkRegFileFull;
   Reg#(Bit#(3)) i   <- mkReg(0);
   Reg#(Bit#(3)) wa  <- mkReg(0);
   Reg#(Bit#(8)) acc <- mkReg(0);

   rule sum3;
      acc <= rf.sub(i) + rf.sub(i + 1) + rf.sub(i + 2);
      i   <= i + 3;
   endrule

   rule fill;
      rf.upd(wa, zeroExtend(wa) * 3);
      wa <= wa + 1;
   endrule

   method Bit#(8) peek(Bit#(3) a) = rf.sub(a);

   method Action poke(Bit#(3) a, Bit#(8) d);
      rf.upd(a, d);
   endmethod
endmodule
