// A load file is an input to the simulation, not to the build: the
// reference reads it when the model object is constructed, so neither
// its link nor ours may open one.  run.sh links this with the .mem
// absent and only puts it in place to run.
package RegFileLoadLink;

import RegFile::*;

(* synthesize *)
module sysRegFileLoadLink(Empty);
   RegFile#(Bit#(4), Bit#(32)) rf <- mkRegFileLoad("RegFileLoadLink.mem", 0, 15);
   Reg#(Bit#(5)) i <- mkReg(0);

   rule show;
      $display("%0d: %0h", i, rf.sub(truncate(i)));
      if (i == 15) $finish(0);
      i <= i + 1;
   endrule

endmodule

endpackage
