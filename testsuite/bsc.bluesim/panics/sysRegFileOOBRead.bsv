// A RegFile read at an out-of-bounds address must call the
// out_of_bounds host operation: a message and a nonzero exit
// (previously: a warning, then an undetermined value).
//
// The out-of-bounds address is written by a first rule and used by a
// second so that the offending read happens after reset, with a
// determined address (rule bodies execute their RegFile reads before
// the in-reset check, with registers still at their undetermined
// 1010... pattern, and such a read would panic too).
import RegFile::*;

module sysRegFileOOBRead();

RegFile#(UInt#(4), UInt#(8)) rf <- mkRegFile(0, 9);

Reg#(UInt#(4)) idx <- mkReg(0);
Reg#(Bool) started <- mkReg(False);

rule prep (!started);
  idx <= 12;
  started <= True;
endrule

rule go (started);
  $display("not reached: %0d", rf.sub(idx));
  $finish(0);
endrule

endmodule
