// Output-reset fixture: GO arms a 2-cycle stretcher; RST_OUT (active
// low) asserts while the counter drains, so the derived reset both
// asserts and deasserts mid-run, driven by a REGISTERED source (the
// transition lands with the clock edge, not combinationally).
module RstStretch(CLK, RST_N, GO, RST_OUT, STATE);
  input CLK, RST_N, GO;
  output RST_OUT;
  output [1:0] STATE;
  reg [1:0] cnt;
  initial cnt = 0;
  assign RST_OUT = (cnt != 0) ? 1'b0 : 1'b1;
  assign STATE = cnt;
  always @(posedge CLK) begin
    if (!RST_N) cnt <= 0;
    else if (GO) cnt <= 2;
    else if (cnt != 0) cnt <= cnt - 1;
  end
endmodule
