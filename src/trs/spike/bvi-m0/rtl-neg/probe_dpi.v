module ProbeDpi(input CLK, output reg [31:0] q);
  import "DPI-C" function int getpid();
  always @(posedge CLK) q <= getpid();
endmodule
