// task #58 witness: an EXECUTED BDPI import whose partner .c/.so was
// never linked.  The compiled call site must trap loudly naming the
// import (never a NULL-call segfault); the interp raises its own
// loud error on the same shape.  The call sits at cycle 3 so the
// link-time window bake (2 cycles) completes and the failure is a
// RUN-time witness.
import "BDPI" function Bit#(32) bdpi_mystery (Bit#(32) x);

(* synthesize *)
module sysBdpiMissing ();
  Reg#(Bit#(32)) c <- mkReg(0);
  rule tick;
    c <= c + 1;
    if (c == 3) begin
      $display("mystery=%0d", bdpi_mystery(c));
      $finish(0);
    end
  endrule
endmodule
