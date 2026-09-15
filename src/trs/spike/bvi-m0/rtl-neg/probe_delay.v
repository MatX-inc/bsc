module ProbeDelay(input CLK, input [7:0] d, output reg [7:0] q);
  always @(posedge CLK) q <= #1 d;
endmodule
