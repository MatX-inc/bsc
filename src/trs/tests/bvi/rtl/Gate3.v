// Cross-method argument-dependent readiness: cfg's argument gates
// put's RDY combinationally (path(CFG_IN, RDY_put) in the import).
module Gate3(CLK, RST_N, CFG_IN, EN_cfg, PUT_IN, EN_put, RDY_put);
  input CLK, RST_N;
  input [7:0] CFG_IN;
  input EN_cfg;
  input [7:0] PUT_IN;
  input EN_put;
  output RDY_put;

  assign RDY_put = RST_N & ~CFG_IN[3];

  reg [7:0] last;
  always @(posedge CLK) begin
    if (!RST_N) last <= 8'd0;
    else if (EN_put) last <= PUT_IN;
  end
endmodule
