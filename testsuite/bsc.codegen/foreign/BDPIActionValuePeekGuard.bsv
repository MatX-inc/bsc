import FIFO::*;
import GetPut::*;
import BDPIActionValueImport::*;

(* synthesize *)
module mkBDPIActionValuePeekGuard ();
   Reg#(Bit#(32)) r1 <- mkReg(0);
   Reg#(Bit#(32)) r2 <- mkReg(0);
   Reg#(Bit#(32)) r3 <- mkReg(0);
   Reg#(Bit#(32)) s <- mkReg(0);
   FIFO#(Bit#(32)) f <- mkFIFO;

   // the value read carries the implicit condition of f.first: a fold that
   // gives back a part of a computation on it (the high bits of a
   // concatenation, the truncation of a zero extension) gives back that
   // value, with its condition, as it does a dynamic value, so the folds
   // after it do not use the result
   Get#(Bit#(32)) g = toGet(actionvalue
                               let t <- my_count(1);
                               return t + f.first;
                            endactionvalue);

   rule extract;
      Bit#(32) x = peekGet(g);
      Bit#(64) z = {x, s};
      Bit#(32) h = z[63:32];
      r1 <= (h == x) ? 1 : 0;
   endrule

   rule sub;
      Bit#(32) x = peekGet(g);
      Bit#(64) z = {x, s};
      Bit#(32) h = z[63:32];
      r2 <= s + (h - x);
   endrule

   rule trunc;
      Bit#(32) x = peekGet(g);
      Bit#(64) z = zeroExtend(x);
      Bit#(32) y = truncate(z);
      r3 <= s + (y - x);
   endrule
endmodule
