// A LYING module: PEEK combinationally depends on put's argument, but
// the import declares no such path.  TRS_BVI_CHECK=observe must
// produce a DYNAMIC_LIE witness naming PEEK and the undeclared
// influence.  (The functional outputs still diverge silently in a
// normal run -- that is exactly why the witness mode exists.)
module Liar(CLK, RST_N, IN, EN, PEEK);
  input CLK, RST_N;
  input [7:0] IN;
  input EN;
  output [7:0] PEEK;

  reg [7:0] r;
  assign PEEK = r ^ IN;   // undeclared combinational path IN -> PEEK

  always @(posedge CLK) begin
    if (!RST_N) r <= 8'd0;
    else if (EN) r <= IN;
  end
endmodule
