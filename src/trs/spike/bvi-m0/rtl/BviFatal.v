// M0 fixture: $fatal containment -- an assertion that fires at the first
// enabled edge must surface as an error return, never std::abort() in
// the host process.
module BviFatal(CLK, RST_N, EN_go, OUT);
  input        CLK, RST_N;
  input        EN_go;
  output [7:0] OUT;
  assign OUT = 8'd7;
  always @(posedge CLK) if (RST_N && EN_go) begin
    assert (0) else $fatal(1, "fixture fatal fired");
  end
endmodule
