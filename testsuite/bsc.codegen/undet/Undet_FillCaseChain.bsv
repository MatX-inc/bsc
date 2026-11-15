// Under -split-undetermined-vals, the fill can leave an if-chain inside a
// concatenation: here the tag of f's input becomes
//   (s == 0) ? 1 : ((s == 1) ? 2 : 5)
// next to the filled bits.  Bluesim converts such a chain to a
// case-expression only at the top of a def, and it decides that from the
// flags that the .ba was compiled with, not the flags of the link.

import FIFO::*;

(* synthesize *)
module sysUndet_FillCaseChain(Empty);
   Reg#(Bit#(6)) i <- mkReg(0);
   Reg#(Bit#(2)) s <- mkReg(0);
   Reg#(Bit#(2)) x <- mkReg(0);
   Reg#(Bit#(1)) b <- mkReg(0);
   FIFO#(Bit#(7)) f <- mkFIFO;

   rule step;
      Bit#(4) da = {?, x};
      Bit#(4) db = {?, b};
      if (s == 0) f.enq({3'd1, da});
      else if (s == 1) f.enq({3'd2, db});
      else f.enq({3'd5, db});
      s <= i[1:0];
      x <= i[3:2];
      b <= i[4];
      i <= i + 1;
   endrule

   rule show;
      $display("%b %b", i, f.first);
      f.deq;
      if (i == 63) $finish(0);
   endrule
endmodule
