// A string chosen by a runtime condition is the one string shape that is
// not a per-instance constant, so the compiler cannot resolve it and the
// design runs interpreted.  That is a coverage limit, not a licence to be
// wrong: the answer must still match the reference exactly.
package StrDynSelect;

import "BDPI" function ActionValue#(Bit#(32)) bdpi_slen(String s);

(* synthesize *)
module sysStrDynSelect(Empty);
   Reg#(Bit#(4)) i <- mkReg(0);
   rule go;
      String s = (i[0] == 1) ? "odd-one" : "even";
      let n <- bdpi_slen(s);
      $display("i=%0d s=%s n=%0d", i, s, n);
      if (i == 3) $finish(0);
      i <= i + 1;
   endrule
endmodule

endpackage
