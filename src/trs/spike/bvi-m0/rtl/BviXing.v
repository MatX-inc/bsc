// M0 fixture: two clocks, coincident edges, NBA crossing.
// dreg must capture the OLD sreg when both clocks rise in one instant.
module BviXing(SCLK, DCLK, RST_N, EN_send, s_din, SREG, DREG);
  input        SCLK, DCLK, RST_N;
  input        EN_send;
  input  [7:0] s_din;
  output [7:0] SREG;
  output [7:0] DREG;

  reg [7:0] sreg, dreg;
  assign SREG = sreg;
  assign DREG = dreg;

  always @(posedge SCLK) begin
    if (!RST_N) sreg <= 8'd0;
    else if (EN_send) sreg <= s_din;
  end
  always @(posedge DCLK) begin
    if (!RST_N) dreg <= 8'd0;
    else dreg <= sreg;
  end
endmodule
