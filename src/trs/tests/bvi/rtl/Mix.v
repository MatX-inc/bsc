// Grab-bag accepted-set target: a constant Port argument (BASE, driven
// once at construction), an (*inhigh*) always-enabled Action, and a
// clockless value method (PLUS1 is pure combinational logic).
module Mix(CLK, RST_N, BASE, TICK_IN, PIN, PLUS1, TOT);
  input CLK, RST_N;
  input [7:0] BASE;      // constant port argument
  input [7:0] TICK_IN;   // always-enabled action arg (no EN port)
  input [7:0] PIN;       // clockless value method arg
  output [7:0] PLUS1;    // clockless value method result
  output [15:0] TOT;

  assign PLUS1 = PIN + 8'd1;

  reg [15:0] acc;
  assign TOT = acc + {8'd0, BASE};

  always @(posedge CLK) begin
    if (!RST_N) acc <= 16'd0;
    else acc <= acc + {8'd0, TICK_IN};
  end
endmodule
