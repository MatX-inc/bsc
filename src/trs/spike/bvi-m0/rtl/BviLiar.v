// M0 fixture: a LYING module -- undeclared combinational path.
// The contract declares peek's output depends on nothing (no args, no
// declared paths), but the RTL routes put's argument straight into it.
module BviLiar(CLK, RST_N, EN_put, put_x, PEEK, STORED);
  input        CLK, RST_N;
  input        EN_put;
  input  [7:0] put_x;
  output [7:0] PEEK;
  output [7:0] STORED;

  reg [7:0] stored;
  assign STORED = stored;
  assign PEEK   = stored ^ put_x;   // UNDECLARED influence of put_x

  always @(posedge CLK) begin
    if (!RST_N) stored <= 8'd0;
    else if (EN_put) stored <= put_x;
  end
endmodule
