// Both wrappers must retain the instance name used to record module-argument
// port types, even though their structural interfaces contain no selectors.
import "BVI" EmptyWrapForeign =
module mkEmptyWrapForeign#(Bit#(8) value)(Empty);
   default_clock clk(CLK);
   default_reset rst(RST_N);
   port VALUE = value;
endmodule

(* synthesize *)
module mkEmptyWrapGenerated#(Bit#(8) value)(Empty);
   rule check;
      if (value != 'h5a)
         $display("FAIL: synthesized empty interface argument");
   endrule
endmodule

(* synthesize *)
module sysEmptyWrapIdentity(Empty);
   Empty imported <- mkEmptyWrapForeign('ha5);
   Empty generated <- mkEmptyWrapGenerated('h5a);
   Reg#(UInt#(3)) cycle <- mkReg(0);

   rule tick;
      cycle <= cycle + 1;
      if (cycle == 3) begin
         $display("empty wrappers retain instance identity");
         $finish(0);
      end
   endrule
endmodule
