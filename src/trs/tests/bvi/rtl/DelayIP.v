// Delay-bearing IP (the --timing mode fixture): GO at an edge schedules
// a pulse that rises 3 time-units later and falls at +13, and a counter
// bump that lands at +12 -- all strictly BETWEEN clock edges (period
// 10), so correctness depends on the engine firing delayed NBAs at
// their own instants, not on edge boundaries.
module DelayIP(CLK, RST_N, GO, PULSE, CNT);
  input CLK, RST_N, GO;
  output reg PULSE;
  output reg [7:0] CNT;
  initial begin PULSE = 0; CNT = 0; end
  always @(posedge CLK) begin
    if (!RST_N) begin
      PULSE <= 0;
      CNT <= 0;
    end else if (GO) begin
      PULSE <= #3 1'b1;
      PULSE <= #13 1'b0;
      CNT <= #12 CNT + 1;
    end
  end
endmodule
