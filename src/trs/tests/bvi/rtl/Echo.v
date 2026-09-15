// Combinational ActionValue: OUT = IN + 1 when enabled.
module Echo(CLK, RST_N, IN, EN, OUT);
  input CLK, RST_N;
  input [7:0] IN;
  input EN;
  output [7:0] OUT;
  assign OUT = IN + 8'd1;
endmodule
