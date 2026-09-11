// R5: per-fragment compilation across a BVI import.  A synthesized
// wrapper holding an `import "BVI"' is instantiated twice at different
// parameters, so it is two SPECIALIZATIONS sharing one Verilated
// model.  The gates: the manifest names the model (as the trs-vlt RUN
// key, which is path-free -- the cache's class key is not), the
// fragment built ALONE in its own tree with its own model cache
// produces a byte-identical object, and the design assembled from that
// object runs correctly.
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
module sysPosSpecialize();
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
