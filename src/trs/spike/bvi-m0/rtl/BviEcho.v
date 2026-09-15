// M0 fixture: self-SBR shadow group.
// BVI shape: ActionValue m(x) enable(EN) = x+1 combinationally; the edge
// latches the final selected argument (the netlist priority-mux output).
module BviEcho(CLK, RST_N, EN, IN, OUT, LAST);
  input        CLK, RST_N;
  input        EN;
  input  [7:0] IN;
  output [7:0] OUT;
  output [7:0] LAST;

  reg [7:0] last;
  assign OUT  = IN + 8'd1;   // AV result: combinational from EN/args
  assign LAST = last;

  always @(posedge CLK) begin
    if (!RST_N) last <= 8'd0;
    else if (EN) last <= IN;
  end
endmodule
