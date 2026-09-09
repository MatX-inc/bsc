// Self-SBR Action target: EN latches IN; PEEK reads the stored value.
module Store(CLK, RST_N, IN, EN, PEEK);
  input CLK, RST_N;
  input [7:0] IN;
  input EN;
  output [7:0] PEEK;

  reg [7:0] r;
  assign PEEK = r;

  always @(posedge CLK) begin
    if (!RST_N) r <= 8'd0;
    else if (EN) r <= IN;
  end
endmodule
