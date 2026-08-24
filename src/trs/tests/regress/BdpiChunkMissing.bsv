// the missing-.so trap under CHUNKED AOT: same eight-rule shape as
// BdpiChunk but the import has no partner C source, and every call
// is gated past the link's 2-cycle window bake so the chunked link
// completes and the RUN must die loudly naming the import (never a
// segfault) — proving the loader-null + trap path survives chunking.

import "BDPI" function Bit#(32) bdpi_mystery (Bit#(32) x);

(* synthesize *)
module sysBdpiChunkMissing(Empty);
   Reg#(Bit#(16)) cyc <- mkReg(0);
   Reg#(Bit#(32)) a0 <- mkReg(1);
   Reg#(Bit#(32)) a1 <- mkReg(2);
   Reg#(Bit#(32)) a2 <- mkReg(3);
   Reg#(Bit#(32)) a3 <- mkReg(4);
   Reg#(Bit#(32)) a4 <- mkReg(5);
   Reg#(Bit#(32)) a5 <- mkReg(6);
   Reg#(Bit#(32)) a6 <- mkReg(7);
   Reg#(Bit#(32)) a7 <- mkReg(8);

   rule step;
      cyc <= cyc + 1;
      if (cyc == 40) $finish(0);
   endrule

   rule r0 (cyc >= 3); a0 <= bdpi_mystery(a0); endrule
   rule r1 (cyc >= 3); a1 <= bdpi_mystery(a1); endrule
   rule r2 (cyc >= 3); a2 <= bdpi_mystery(a2); endrule
   rule r3 (cyc >= 3); a3 <= bdpi_mystery(a3); endrule
   rule r4 (cyc >= 3); a4 <= bdpi_mystery(a4); endrule
   rule r5 (cyc >= 3); a5 <= bdpi_mystery(a5); endrule
   rule r6 (cyc >= 3); a6 <= bdpi_mystery(a6); endrule
   rule r7 (cyc >= 3); a7 <= bdpi_mystery(a7); endrule
endmodule
