// Stub for the module imported by Coll5.bs, so that the generated
// sysColl5.v can be compiled by the Verilog simulator.
module VColl5(CLK, ADDR_1, ADDR_2, D_OUT_1, D_OUT_2);
  input        CLK;
  input  [3:0] ADDR_1;
  input  [3:0] ADDR_2;
  output [7:0] D_OUT_1;
  output [7:0] D_OUT_2;
  assign D_OUT_1 = {4'd0, ADDR_1};
  assign D_OUT_2 = {4'd0, ADDR_2};
endmodule
