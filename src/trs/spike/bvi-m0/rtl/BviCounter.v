// M0 fixture: ordinary Action + value dependency.
// BVI shape: Action method bump(amt) enable(EN_bump) ready(RDY_bump);
//            value method read() = count.
module BviCounter(CLK, RST_N, EN_bump, bump_amt, count, RDY_bump);
  input        CLK, RST_N;
  input        EN_bump;
  input  [7:0] bump_amt;
  output [7:0] count;
  output       RDY_bump;

  reg [7:0] c;
  assign count = c;
  assign RDY_bump = RST_N;   // argument-independent ready (fixture 3 varies this)

  always @(posedge CLK) begin
    if (!RST_N) c <= 8'd0;
    else if (EN_bump) c <= c + bump_amt;
  end
endmodule
