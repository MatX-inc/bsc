// M0 fixture: protocol violator -- architectural state mutates on a raw
// argument TRANSITION, not at the declared method clock.  The BVI port
// protocol gives inter-edge argument values no meaning; this module
// observes them anyway.
module BviViolator(CLK, RST_N, EN_put, put_x, COUNT);
  input        CLK, RST_N;
  input        EN_put;
  input  [7:0] put_x;
  output [7:0] COUNT;

  reg [7:0] cnt;
  assign COUNT = cnt;

  always @(posedge put_x[0]) begin   // edge-sensitive on an ARG bit
    cnt <= cnt + 8'd1;
  end
endmodule
