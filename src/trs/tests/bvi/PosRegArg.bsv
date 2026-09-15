// A (*reg*) method argument: the imported module writes the port
// straight to a register, so it has no combinational path to an
// output.  The property is a promise about the module's insides, not a
// change to the port protocol -- the caller drives the argument in the
// cycle it calls, exactly as for a plain input -- so the runtime
// carries it and acts on it nowhere.
//
// put() is called with a changing value while the value methods read
// the two registers behind it, which shows the argument arriving a
// cycle later and again the cycle after that.  The Verilog flow is the
// oracle: it sees the same RTL and has no notion of the property.
interface Ifc;
   method Action put(Bit#(8) x);
   method Bit#(8) peek();
   method Bit#(8) prev();
endinterface

import "BVI" RegArg =
module mkRegArg(Ifc);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   // the shape a real import uses: the argument, its enable and the
   // outputs all marked, since a port that feeds a register and one
   // that comes from one are the same claim seen from either side
   method put((* reg *)IN) enable((* reg *)EN);
   method (* reg *)PEEK peek();
   method (* reg *)PREV prev();
   schedule put C put;
   schedule (peek, prev) CF (peek, prev, put);
endmodule

(* synthesize *)
module sysPosRegArg();
   Reg#(Bit#(8)) cyc <- mkReg(0);
   let dut <- mkRegArg;

   rule drive (cyc < 5);
      dut.put(cyc + 8'd100);
   endrule

   rule watch;
      $display("cyc %0d peek %0d prev %0d", cyc, dut.peek, dut.prev);
      cyc <= cyc + 1;
      if (cyc == 7) $finish(0);
   endrule
endmodule
