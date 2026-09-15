// --timing positive: an import with real intra-cycle delays (#3/#12/#13
// NBAs in DelayIP.v).  GO fires at cycles 1 and 5 (far enough apart
// that the event queues never overlap); the pulse and the delayed count
// become visible at edges downstream of their own instants.  Oracle:
// the same BSV under the Verilog flow (iverilog).
interface DelayIP;
   method Action go();
   method Bit#(1) pulse();
   method Bit#(8) cnt();
endinterface

import "BVI" DelayIP =
module mkDelayIP(DelayIP);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   method go() enable(GO);
   method PULSE pulse();
   method CNT cnt();
   schedule (pulse, cnt) CF (pulse, cnt);
   schedule (pulse, cnt) SB go;
   schedule go C go;
endmodule

(* synthesize *)
module sysPosDelay();
   DelayIP ip <- mkDelayIP;
   Reg#(Bit#(8)) n <- mkReg(0);

   rule step;
      $display("c=%0d pulse=%b cnt=%0d", n, ip.pulse(), ip.cnt());
      n <= n + 1;
      if (n == 1 || n == 5) ip.go();
      if (n == 12) $finish(0);
   endrule
endmodule
