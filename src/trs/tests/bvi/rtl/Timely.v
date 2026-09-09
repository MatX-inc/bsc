// $time target: the $time > 5 guard pins out the startup instant,
// where the two flows' timebases differ (Verilog main first edge t=1,
// Bluesim/trs t=0); every later edge agrees exactly.
module Timely(CLK, RST_N, GO);
  input CLK, RST_N, GO;
  always @(posedge CLK) if (GO && $time > 5 && $time < 35) $display("model t=%0t", $time);
endmodule
