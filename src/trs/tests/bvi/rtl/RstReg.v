// Reset-behavior target: a register with a distinctive reset value,
// exercising startup reset (t=0 assertion as a real transition) and a
// MID-RUN reset assertion.
module RstReg(CLK, RST_N, IN, EN, OUT);
  input CLK, RST_N;
  input [7:0] IN;
  input EN;
  output [7:0] OUT;

  reg [7:0] r;
  assign OUT = r;

  always @(posedge CLK) begin
    if (!RST_N) r <= 8'haa;
    else if (EN) r <= IN;
  end
endmodule
