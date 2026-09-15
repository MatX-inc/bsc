// A method argument the module writes straight to a register, which is
// what (*reg*) on an input port declares.  PEEK therefore shows the
// PREVIOUS argument: the port has no combinational path to any output.
// A second register makes the value visible two cycles later as well,
// so a run that mistook the timing would print different numbers
// rather than the same ones one cycle apart.
module RegArg(CLK, RST_N, IN, EN, PEEK, PREV);
  input CLK, RST_N;
  input [7:0] IN;
  input EN;
  output [7:0] PEEK;
  output [7:0] PREV;

  reg [7:0] r;
  reg [7:0] r2;
  assign PEEK = r;
  assign PREV = r2;

  always @(posedge CLK) begin
    if (!RST_N) begin
      r  <= 8'd0;
      r2 <= 8'd0;
    end else begin
      r2 <= r;
      if (EN) r <= IN;
    end
  end
endmodule
