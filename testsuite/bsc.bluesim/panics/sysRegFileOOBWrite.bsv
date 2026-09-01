// A RegFile write at an out-of-bounds address must call the
// out_of_bounds host operation: a message and a nonzero exit
// (previously: a warning, then the write was ignored).
import RegFile::*;

module sysRegFileOOBWrite();

RegFile#(UInt#(4), UInt#(8)) rf <- mkRegFile(0, 9);

Reg#(UInt#(4)) idx <- mkReg(0);
Reg#(Bool) started <- mkReg(False);

rule prep (!started);
  idx <= 12;
  started <= True;
endrule

rule go (started);
  rf.upd(idx, 42);
  $display("not reached");
  $finish(0);
endrule

endmodule
