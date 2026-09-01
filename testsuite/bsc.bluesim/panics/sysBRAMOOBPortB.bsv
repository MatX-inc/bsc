// An out-of-bounds access on port B of a dual-ported BRAM must call
// the out_of_bounds host operation too.
import BRAMCore::*;

module sysBRAMOOBPortB();

BRAM_DUAL_PORT#(UInt#(4), UInt#(8)) bram <- mkBRAMCore2(10, False);

Reg#(UInt#(4)) idx <- mkReg(0);
Reg#(Bool) started <- mkReg(False);

rule prep (!started);
  idx <= 12;
  started <= True;
endrule

rule go (started);
  bram.b.put(True, idx, 42);
  $display("not reached");
  $finish(0);
endrule

endmodule
