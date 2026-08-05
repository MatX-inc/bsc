// Regression: hoisted shared cones must not contain trapping ops
// (review-fleet major finding).  q = x % den is consumed by two
// rules guarded on den != 0; den stays 0 the whole run, the rules
// correctly never fire — but the buggy emitter hoisted q onto the
// edge spine and evaluated it unconditionally: SIGFPE.
(* synthesize *)
module sysHoistDivTrap();
  Reg#(Bit#(16)) cyc <- mkReg(0);
  Reg#(Bit#(8))  den <- mkReg(0);
  Reg#(Bit#(8))  x   <- mkReg(7);
  Reg#(Bit#(8))  a   <- mkReg(0);
  Reg#(Bit#(8))  b   <- mkReg(0);

  Bit#(8) q = x % den;

  rule ra (den != 0);
    a <= a + q;
  endrule
  rule rb (den != 0);
    b <= b ^ q;
  endrule
  rule tick;
    cyc <= cyc + 1;
    if (cyc == 4) begin
      $display("a=%h b=%h", a, b);
      $finish(0);
    end
  endrule
endmodule
