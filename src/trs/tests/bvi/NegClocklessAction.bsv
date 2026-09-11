// R2 negative: an Action method clocked by no_clock -- no clock at all,
// as opposed to a PORTLESS one (see PosCombPortlessClk, which is
// accepted).  bsc itself calls these unusable (P0172) and drops the
// rules that call them; there is no edge to commit the effects, so the
// export refuses.
interface Ifc;
   method Action poke(Bit#(8) x);
endinterface

import "BVI" NoClk =
module mkNoClk(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method poke(IN) enable(EN) clocked_by(no_clock);
endmodule

(* synthesize *)
module sysNegClocklessAction();
   let dut <- mkNoClk;
   rule r;
      dut.poke(1);
      $finish(0);
   endrule
endmodule
