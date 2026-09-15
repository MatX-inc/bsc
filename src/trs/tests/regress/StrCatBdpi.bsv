// String-valued expressions reach a compiled body only as per-instance
// constants (BSV cannot compute a string from data), so they carry no
// runtime value: the call-site table names the def or parameter and the
// interpreter resolves it.  Two instances with different parameters keep
// that honest -- compiled bodies are shared across an equivalence class,
// so a single baked-in string would show up here.
//
// The shapes below are every way a constant string can be built: the
// operand orders, a bare parameter, a bare literal, nesting, and a concat
// consumed by $display rather than by a BDPI import.
package StrCatBdpi;

import "BDPI" function ActionValue#(Bit#(32)) bdpi_slen(String s);

(* synthesize *)
module mkLeaf#(parameter String a, parameter String b)(Empty);
   Reg#(Bit#(4)) i <- mkReg(0);
   rule go;
      let n1 <- bdpi_slen(a + ".dat");         // parameter + literal
      let n2 <- bdpi_slen("pre-" + a);         // literal + parameter
      let n3 <- bdpi_slen(a + b);              // parameter + parameter
      let n4 <- bdpi_slen(a + "-" + b + "x");  // nested
      let n5 <- bdpi_slen(a);                  // bare parameter
      let n6 <- bdpi_slen("literal-only");     // bare literal
      $display("[%s|%s] %0d %0d %0d %0d %0d %0d", a, b + "!", n1, n2, n3, n4, n5, n6);
      if (i == 1) $finish(0);
      i <= i + 1;
   endrule
endmodule

(* synthesize *)
module sysStrCatBdpi(Empty);
   Empty x <- mkLeaf("alpha", "one");
   Empty y <- mkLeaf("bravoo", "two");
endmodule

endpackage
