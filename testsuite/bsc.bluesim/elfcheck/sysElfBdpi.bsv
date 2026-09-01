// Freestanding-ELF probe: BDPI imports (string and wide arguments).
// BDPI models are the documented exception to full freestanding
// operation: the marshaling fallback keeps malloc/free (and with
// them DT_NEEDED on libc), so the checker runs with --bdpi, which
// admits exactly that exclusion list and nothing more.  See
// elfcheck.exp.

import "BDPI" function Bit#(32)  elfcheck_strlen (String s);
import "BDPI" function Bit#(128) elfcheck_incr (Bit#(128) x);

(* synthesize *)
module sysElfBdpi();
  Reg#(Bit#(8))   cnt <- mkReg(0);
  Reg#(Bit#(128)) w   <- mkReg(128'h10);

  rule step (cnt < 6);
    cnt <= cnt + 1;
    w   <= elfcheck_incr(w);
    $display("len=%0d w=%h", elfcheck_strlen("hello"), w);
  endrule

  rule fin (cnt == 6);
    $finish(0);
  endrule
endmodule
