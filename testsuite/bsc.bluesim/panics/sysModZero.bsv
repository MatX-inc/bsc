// Narrow unsigned remainder by a dynamic zero must call the
// divide_by_zero host operation: a message and a nonzero exit,
// deterministically, on every architecture.
module sysModZero();

Reg#(UInt#(16)) a <- mkReg(1203);
Reg#(UInt#(16)) b <- mkReg(0);

Reg#(Bool) done <- mkReg(False);

rule test (!done);
  a <= a % b;
  b <= b + 1;
  done <= True;
endrule

rule quit (done);
  $display("not reached: %0d", a);
  $finish(0);
endrule

endmodule
