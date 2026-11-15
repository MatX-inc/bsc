import FIFO::*;
import GetPut::*;
import BDPIActionValueImport::*;

typedef struct { Bit#(32) a; Bit#(32) b; } Pr deriving (Bits, Eq);

function Bit#(32) sq(Bit#(32) x);
   let t = x + 1;
   return t * t;
endfunction

function Pr mk(Bit#(32) x);
   let t = x + 1;
   return Pr { a: t * t, b: t };
endfunction

(* synthesize *)
module mkBDPIActionValuePeekSameArms ();
   Reg#(Bit#(32)) r <- mkReg(0);
   Reg#(Bit#(32)) q <- mkReg(0);
   Reg#(Bit#(32)) s <- mkReg(0);
   FIFO#(Bit#(32)) f <- mkFIFO;
   Get#(Bit#(32)) g = toGet(my_count(1));

   // an if on the peeked result whose arms are one value (computed by a
   // function, a struct, a sum, or a choice with its own condition) is
   // that value: the result is not used
   rule go;
      Bool c = peekGet(g) == 0;
      Pr m = mk(r);
      Bit#(32) v = sq(r);
      Bit#(32) u = q + 1;
      Bit#(32) n = (s == 0) ? q : f.first;
      Pr p = c ? m : m;
      r <= (c ? v : v) + p.a + (c ? u : u) + (c ? n : n);
   endrule
endmodule
