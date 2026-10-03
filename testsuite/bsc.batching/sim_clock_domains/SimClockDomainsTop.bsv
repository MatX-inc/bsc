package SimClockDomainsTop;

import SimClockDomainsScramble::*;
import SimClockDomains::*;

// The scrambler declared before the probe: its .ba is read first when the
// model is linked, so its instance names are interned before the probe's.
(* synthesize *)
module sysSimClockDomainsTop();
   Empty s <- mkSimClockDomainsScramble;
   Empty p <- sysSimClockDomains;
endmodule

endpackage
