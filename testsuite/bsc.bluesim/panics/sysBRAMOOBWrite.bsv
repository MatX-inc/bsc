// A BRAM write at an out-of-bounds address must call the
// out_of_bounds host operation: a message and a nonzero exit
// (previously: a warning, then the write was ignored).
import BRAMCore::*;

module sysBRAMOOBWrite();

BRAM_PORT#(UInt#(4), UInt#(8)) bram <- mkBRAMCore1(10, False);

Reg#(UInt#(4)) idx <- mkReg(0);
Reg#(Bool) started <- mkReg(False);

rule prep (!started);
  idx <= 12;
  started <= True;
endrule

rule go (started);
  bram.put(True, idx, 42);
  $display("not reached");
  $finish(0);
endrule

endmodule
