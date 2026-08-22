// A top module with a representative interface for the caller-provided
// port buffer test: narrow and wide method arguments, method enables,
// narrow and wide results, readies, an always-ready combinational
// method with an argument (which appears in no schedule and is
// refreshed by the appended value-method calls), and an ActionValue
// method.  The host_ports harness drives the inputs and observes the
// outputs purely through the port buffers at the published
// introspection offsets.

interface Ifc;
   method Action push(Bit#(8) x, Bit#(100) w, Bit#(48) y);
   method Bit#(8) headv();
   method Bit#(48) suml();
   method Bit#(100) widev(Bit#(4) sel);
   method Bit#(16) echo(Bit#(16) v);
   method ActionValue#(Bit#(16)) grab(Bit#(16) a);
endinterface

(* synthesize *)
module sysHostPorts(Ifc);
   Reg#(Bit#(8))   r    <- mkReg(0);
   Reg#(Bit#(100)) wr   <- mkReg(0);
   Reg#(Bit#(48))  yr   <- mkReg(0);
   Reg#(Bool)      full <- mkReg(False);

   method Action push(Bit#(8) x, Bit#(100) w, Bit#(48) y) if (!full);
      r <= x; wr <= w; yr <= y; full <= True;
   endmethod
   method Bit#(8) headv() if (full);
      return r + 1;
   endmethod
   method Bit#(48) suml() if (full);
      return yr + 5;
   endmethod
   method Bit#(100) widev(Bit#(4) sel);
      return wr + zeroExtend(sel);
   endmethod
   method Bit#(16) echo(Bit#(16) v);
      return v + 1;
   endmethod
   method ActionValue#(Bit#(16)) grab(Bit#(16) a) if (full);
      full <= False;
      return zeroExtend(r) ^ a;
   endmethod
endmodule
