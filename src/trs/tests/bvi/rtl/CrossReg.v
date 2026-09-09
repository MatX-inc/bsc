// Two-clock crossing register: sreg latches on SCLK, dreg samples sreg
// on DCLK.  With COINCIDENT posedges the NBA semantics require dreg to
// capture the OLD sreg -- the batched single-eval commit reproduces
// this; sequential per-edge evals shoot through (the M0-pinned bug).
module CrossReg(SCLK, DCLK, RST_N, IN, EN, OUT);
  input SCLK, DCLK, RST_N;
  input [7:0] IN;
  input EN;
  output [7:0] OUT;

  reg [7:0] sreg, dreg;
  assign OUT = dreg;

  always @(posedge SCLK) begin
    if (!RST_N) sreg <= 8'd0;
    else if (EN) sreg <= IN;
  end
  always @(posedge DCLK) begin
    if (!RST_N) dreg <= 8'd0;
    else dreg <= sreg;
  end
endmodule
