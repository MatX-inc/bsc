// Signed division by a dynamic zero must also call the
// divide_by_zero host operation: the Prelude implements signed
// division by stripping the signs and calling the unsigned
// primitives, so the divisor guard covers it too.
module sysSignedDivZero();

Reg#(Int#(32)) a <- mkReg(-1203);
Reg#(Int#(32)) b <- mkReg(0);

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
