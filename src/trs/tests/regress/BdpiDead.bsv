// task #58 witness: a DECLARED-but-never-called BDPI import with no
// partner .c/.so is harmless on every engine (the monorepo corpus
// relies on this — dead mufu imports ride along in passing tests).
import "BDPI" function Bit#(32) bdpi_mystery (Bit#(32) x);

(* synthesize *)
module sysBdpiDead ();
  Reg#(Bit#(32)) c <- mkReg(0);
  rule tick;
    c <= c + 1;
    if (c == 3) begin
      $display("dead-ok c=%0d", c);
      $finish(0);
    end
  endrule
endmodule
