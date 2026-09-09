// Two independent reset inputs: each register clears on its own reset.
module TwoRst(CLK, RST_N, RST2_N, IN, EN, OUTA, OUTB);
  input CLK, RST_N, RST2_N;
  input [7:0] IN;
  input EN;
  output [7:0] OUTA;
  output [7:0] OUTB;

  reg [7:0] a, b;
  assign OUTA = a;
  assign OUTB = b;

  always @(posedge CLK) begin
    if (!RST_N) a <= 8'h11;
    else if (EN) a <= IN;
  end
  always @(posedge CLK) begin
    if (!RST2_N) b <= 8'h22;
    else if (EN) b <= IN;
  end
endmodule
