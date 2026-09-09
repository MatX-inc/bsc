// R2 positive: argument-dependent readiness via a declared CROSS-method
// path -- cfg's argument combinationally gates put's RDY, with the
// ordering annotation (cfg SB put) the accepted set requires.  (The
// same-method self-path variant is bsc-unlinkable: G0033 rejects a rule
// whose own firing feeds its CAN_FIRE.)
interface Ifc;
   method Action cfg(Bit#(8) x);
   method Action put(Bit#(8) y);
endinterface

import "BVI" Gate3 =
module mkGate3(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method cfg(CFG_IN) enable(EN_cfg);
   method put(PUT_IN) enable(EN_put) ready(RDY_put);
   path(CFG_IN, RDY_put);
   schedule cfg SB put;
   schedule cfg C cfg;
   schedule put C put;
endmodule

(* synthesize *)
module sysPosArgRdy();
   Reg#(Bit#(4)) n <- mkReg(0);
   let dut <- mkGate3;
   rule rc;
      dut.cfg({4'b0, n});
   endrule
   rule rp;
      dut.put(1);
      n <= n + 1;
      if (n == 7) $finish(0);
   endrule
endmodule
