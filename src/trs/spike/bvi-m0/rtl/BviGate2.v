// M0 fixture: argument-dependent RDY (declared path from arg to rdy).
// BVI shape: Action put(x) enable(EN_put) ready(RDY_put);
// RDY_put depends combinationally on the CURRENT argument value: the
// module refuses odd values.  Declared path: put_x -> RDY_put, put SB put
// -- exercises RDY as a frontier read whose cone includes an arg port.
module BviGate2(CLK, RST_N, EN_put, put_x, RDY_put, STORED);
  input        CLK, RST_N;
  input        EN_put;
  input  [7:0] put_x;
  output       RDY_put;
  output [7:0] STORED;

  reg [7:0] stored;
  assign RDY_put = RST_N & ~put_x[0];   // ready iff argument is even
  assign STORED  = stored;

  always @(posedge CLK) begin
    if (!RST_N) stored <= 8'd0;
    else if (EN_put) stored <= put_x;
  end
endmodule
