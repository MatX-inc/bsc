// Output-clock fixture: a registered divide-by-2 clock generated
// inside the import; downstream BSV state and rules live in the
// derived domain, so edges must fire between the kernel's scheduled
// slices (discovered at the BVI commit point).
module DivClk(CLK, RST_N, CLK_OUT, CNT);
  input CLK, RST_N;
  output CLK_OUT;
  output [7:0] CNT;
  reg [7:0] cnt;
  reg div;
  initial begin cnt = 0; div = 0; end
  assign CLK_OUT = div;
  assign CNT = cnt;
  always @(posedge CLK) begin
    if (!RST_N) begin cnt <= 0; div <= 0; end
    else begin cnt <= cnt + 1; div <= ~div; end
  end
endmodule
