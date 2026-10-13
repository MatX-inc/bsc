// Stub for the module imported by SharedPort.bs, so that the generated
// sysSharedPort.v can be compiled by the Verilog simulator.
module VShared(CLK, FULL_N, COUNT);
  input        CLK;
  output       FULL_N;
  output [7:0] COUNT;
  assign FULL_N = 1'b1;
  assign COUNT = 8'd0;
endmodule
