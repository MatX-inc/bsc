// Wide (>64-bit) division by a dynamic zero must call the
// divide_by_zero host operation (previously: raise(SIGFPE) in
// wide_quot_rem, with garbage results if the signal was handled).
module sysWideDivZero();

Reg#(UInt#(96)) a <- mkReg(1203);
Reg#(UInt#(96)) b <- mkReg(0);

Reg#(Bool) done <- mkReg(False);

rule test (!done);
  a <= a / b;
  b <= b + 1;
  done <= True;
endrule

rule quit (done);
  $display("not reached: %0d", a);
  $finish(0);
endrule

endmodule
