package SimClockDomainsTop;

import SimClockDomainsScramble::*;
import SimClockDomains::*;

// The scrambler is declared before the probe: its .bmod/.bsched pair is read
// first when the model is linked, so its instance names are interned first.
(* synthesize *)
module sysSimClockDomainsTop();
   Empty s <- mkSimClockDomainsScramble;
   Empty p <- sysSimClockDomains;
endmodule

endpackage
