// Gated-clock counter: the model receives the RAW oscillator and the
// gate as a LEVEL input (never ANDed by the harness), and does its own
// gating -- posedges only count while the gate is high.
module GateCnt(CLK, CLK_GATE, RST_N, CNT);
  input CLK, CLK_GATE, RST_N;
  output [7:0] CNT;

  reg [7:0] c;
  assign CNT = c;

  always @(posedge CLK) begin
    if (!RST_N) c <= 8'd0;
    else if (CLK_GATE) c <= c + 8'd1;
  end
endmodule
