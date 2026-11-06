package AluSliceBSV;

import ShiftRegBSV::*;

typedef struct {
   Bit#(hi_w) hi;
   Bit#(lo_w) lo;
} Packet#(numeric type hi_w, numeric type lo_w) deriving (Bits, Eq);

interface SplitJoinIfc#(numeric type hi_w, numeric type lo_w);
   method Packet#(hi_w, lo_w) splitWord(Bit#(TAdd#(hi_w, lo_w)) word);
   method Bit#(TMul#(2, hi_w)) dupHi(Packet#(hi_w, lo_w) pkt);
endinterface

(* synthesize *)
module mkSplitJoin(SplitJoinIfc#(hi_w, lo_w))
   provisos (Add#(1, k, lo_w));

   method Packet#(hi_w, lo_w) splitWord(Bit#(TAdd#(hi_w, lo_w)) word);
      Bit#(hi_w) h = truncateLSB(word);
      Bit#(lo_w) l = truncate(word);
      Bit#(k) l_upper = truncateLSB(l);
      Bit#(lo_w) l_flipped = { l_upper, ~l[0] };
      return Packet { hi: h + fromInteger(valueOf(hi_w)), lo: l_flipped };
   endmethod

   method Bit#(TMul#(2, hi_w)) dupHi(Packet#(hi_w, lo_w) pkt);
      return { pkt.hi, pkt.hi };
   endmethod
endmodule

(* synthesize *)
module sysAluSliceBSV(Empty);
   ShiftIfc#(12) s12 <- mkShiftReg(12'h050);
   ShiftIfc#(24) s24 <- mkShiftReg(24'h001000);
   SplitJoinIfc#(4, 8)  sj12 <- mkSplitJoin;
   SplitJoinIfc#(8, 16) sj24 <- mkSplitJoin;

   Reg#(Bit#(8)) cycle <- mkReg(0);

   rule step;
      cycle <= cycle + 1;
      s12.shift(zeroExtend(cycle) + 12'h120);
      s24.shift(zeroExtend(cycle) + 24'h340000);
      let p12 = sj12.splitWord(s12.head());
      let p24 = sj24.splitWord(s24.head());
      $display("cycle=%0d p12=(%0h,%0h) dup12=%0h p24=(%0h,%0h) dup24=%0h",
               cycle,
               p12.hi, p12.lo, sj12.dupHi(p12),
               p24.hi, p24.lo, sj24.dupHi(p24));
      if (cycle == 3) $finish(0);
   endrule
endmodule

endpackage
