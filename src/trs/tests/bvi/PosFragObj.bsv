// R5: per-fragment compilation across a BVI import.  A synthesized
// wrapper holding an `import "BVI"' is instantiated twice at two
// different argument values, and both instantiations must now come
// out of ONE object -- the argument reaches the body through a slot
// in the instance's arena, not through the code, so it is no longer
// part of what the object IS.  The gates: exactly one mkWrap.o, that
// object built ALONE in its own tree with its own model cache is
// byte-identical to the design's, the design assembles from it, and
// the assembled design still gets both instantiations right.
interface Counter;
   method Action bump(Bit#(8) amt);
   method Bit#(8) read();
endinterface

import "BVI" BviCounter =
module mkBviCounter(Counter);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method bump(bump_amt) enable(EN_bump) ready(RDY_bump);
   method count read();
   schedule read CF read;
   schedule read SB bump;
   schedule bump C bump;
endmodule

interface Wrap;
   method Action step();
   method Bit#(8) value();
endinterface

(* synthesize *)
module mkWrap#(Bit#(8) k)(Wrap);
   Counter c <- mkBviCounter;
   method Action step();
      c.bump(k);
   endmethod
   method Bit#(8) value() = c.read();
endmodule

(* synthesize *)
module sysPosFragObj();
   Wrap a <- mkWrap(3);
   Wrap b <- mkWrap(7);
   Reg#(Bit#(3)) n <- mkReg(0);

   rule go;
      a.step(); b.step();
      n <= n + 1;
      $display("a=%0d b=%0d", a.value(), b.value());
      if (n == 3) $finish(0);
   endrule
endmodule
