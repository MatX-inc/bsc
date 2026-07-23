import Probe::*;

// "shared" feeds both the probe and the method; "watched_only" feeds the
// probe alone.
interface ProbeSimOnly;
   method Bit#(8) result;
endinterface

(* synthesize *)
module mkProbeSimOnly (ProbeSimOnly);
   Reg#(Bit#(8)) count <- mkReg(0);
   Probe#(Bit#(8)) p1 <- mkProbe;
   Probe#(Bit#(8)) p2 <- mkProbe;

   rule go;
      count <= count + 1;
      p1 <= count + 8'd7;
      p2 <= count + 8'd9;
   endrule

   method result = count + 8'd7;
endmodule
