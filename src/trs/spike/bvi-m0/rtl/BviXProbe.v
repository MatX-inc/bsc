// M0 fixture: X-sensitive control -- accepted two-state limitation.
// A 4-state simulator reports ready while q is X; two-state 0-fill says
// q==0 from birth, so readiness differs in CONTROL, not just data.
module BviXProbe(CLK, RST_N, RDYX, Q);
  input        CLK, RST_N;
  output       RDYX;
  output [7:0] Q;

  reg [7:0] q;
  assign Q = q;
  assign RDYX = (q === 8'hxx) ? 1'b1 : 1'b0;

  always @(posedge CLK) if (RST_N) q <= q + 8'd1;
endmodule
