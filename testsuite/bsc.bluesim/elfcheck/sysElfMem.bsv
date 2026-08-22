// Freestanding-ELF probe: memory primitives -- a preloaded RegFile
// (the memory-file parser runs at construction) and a BRAM.  See
// elfcheck.exp.

import RegFile::*;
import BRAMCore::*;

(* synthesize *)
module sysElfMem();
  RegFile#(Bit#(3), Bit#(16)) rf <- mkRegFileLoad("sysElfMem.dat", 0, 7);
  BRAM_PORT#(Bit#(3), Bit#(16)) br <- mkBRAMCore1(8, False);
  Reg#(Bit#(5)) cnt <- mkReg(0);

  rule load (cnt < 8);
    br.put(True, truncate(cnt), rf.sub(truncate(cnt)) + 1);
    cnt <= cnt + 1;
  endrule

  rule readback (cnt >= 8 && cnt < 17);
    if (cnt > 8)
      $display("bram[%0d]=%h", cnt - 9, br.read);
    if (cnt < 16)
      br.put(False, truncate(cnt), 0);
    cnt <= cnt + 1;
  endrule

  rule fin (cnt == 17);
    $finish(0);
  endrule
endmodule
