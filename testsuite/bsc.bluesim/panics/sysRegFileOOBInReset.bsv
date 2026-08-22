// An out-of-bounds RegFile access executed while the design is still
// in reset must NOT call the out_of_bounds host operation: the
// simulation proceeds silently (no warning) and later in-bounds
// accesses behave normally.
//
// Rule bodies execute their RegFile reads before the in-reset check,
// so at time 0 the unconditional rule below reads rf at idx's
// undetermined initial 1010... pattern (10, out of bounds for a
// 0..9 RegFile) while the default reset is asserted.  The read is
// clamped to an undetermined value and its effects are discarded by
// the in-reset check; after reset deasserts, idx holds its reset
// value (3) and the design runs to a clean finish.
import RegFile::*;

module sysRegFileOOBInReset();

RegFile#(UInt#(4), UInt#(8)) rf <- mkRegFile(0, 9);

Reg#(UInt#(4)) idx <- mkReg(3);
Reg#(UInt#(8)) cnt <- mkReg(0);
Reg#(UInt#(8)) last <- mkReg(0);

rule go;
  last <= rf.sub(idx);
  cnt <= cnt + 1;
  if (cnt == 0)
    rf.upd(3, 42);
  if (cnt == 3) begin
    $display("last: %0d", last);
    $finish(0);
  end
endrule

endmodule
