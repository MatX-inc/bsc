// Freestanding-ELF probe: string values built from module
// parameters.  A String parameter concatenated with literals used to
// be the one construct that kept libstdc++ referenced from a model
// (a runtime std::string concatenation); string values are now tStr
// trees (see bs_str.h) whose concatenation nodes are built once, in
// the constructor.  This probe exercises every consumer of a
// parameter-grounded concatenation -- $display (as value and as
// format string), dynamic selection over concatenations, and a
// RegFile load-file name built from the parameter -- and then
// requires the strict ELF check to certify the object freestanding.
// See elfcheck.exp.

import RegFile::*;

(* synthesize *)
module mkElfStrCatSub#(parameter String nm)();
  RegFile#(Bit#(2), Bit#(8)) rf <- mkRegFileFullLoad(strConcat(nm, ".dat"));
  Reg#(Bit#(8)) cnt <- mkReg(0);

  rule step (cnt < 4);
    String t = strConcat("name: ", nm);
    String u = (cnt[0] == 1) ? strConcat(nm, "_odd") : "even";
    cnt <= cnt + 1;
    $display(t);
    $display("%s -> rf[%0d]=%h", u, cnt, rf.sub(truncate(cnt)));
  endrule

  rule fin (cnt == 4);
    $finish(0);
  endrule
endmodule

(* synthesize *)
module sysElfStrCat();
  Empty s <- mkElfStrCatSub("sysElfStrCat");
endmodule
