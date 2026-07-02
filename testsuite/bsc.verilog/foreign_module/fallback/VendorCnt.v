// The "vendor IP" implementation of the imported module, used by the
// default (non-fallback) branch of the generated instantiation.
module VendorCnt(CLK, RST_N, EN_incr, VAL);
  input CLK;
  input RST_N;
  input EN_incr;
  output [7:0] VAL;

  reg [7:0] r;
  assign VAL = r;

  always @(posedge CLK) begin
    if (!RST_N)
      r <= 8'd0;
    else if (EN_incr)
      r <= r + 8'd1;
  end
endmodule
