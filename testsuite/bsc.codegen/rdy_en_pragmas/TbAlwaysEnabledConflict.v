// mkAEConflict (AlwaysEnabledConflict.bs, G0134 demoted) has no EN ports, so
// a and b fire every cycle: any serial order adds 3 to c, the hardware adds 1.
module sysTbAlwaysEnabledConflict;
  reg CLK = 0, RST_N = 0;
  wire [7:0] cur;
  mkAEConflict dut(.CLK(CLK), .RST_N(RST_N), .cur(cur));
  always #5 CLK = ~CLK;
  integer cyc = 0;
  reg [7:0] prev;
  initial #12 RST_N = 1;
  always @(posedge CLK) if (RST_N) begin
    prev <= cur;
    if (cyc > 0) begin
      $display("cycle %0d: c before = %0d, a;b or b;a gives %0d, hardware gives %0d",
               cyc, prev, prev + 8'd3, cur);
      if (cur != prev + 8'd3)
        $display("UNSOUND: no serial execution of a and b gives %0d", cur);
    end
    cyc = cyc + 1;
    if (cyc == 5) $finish(0);
  end
endmodule
