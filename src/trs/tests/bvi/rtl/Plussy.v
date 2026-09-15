// $test$plusargs / $value$plusargs inside the model: plusargs reach the
// per-instance VerilatedContext through vlt_new's argv.
module Plussy(CLK, RST_N, GO);
  input CLK, RST_N, GO;
  reg [31:0] v;
  always @(posedge CLK) begin
    if (GO) begin
      if ($test$plusargs("doit")) $display("model saw +doit");
      else $display("model saw no +doit");
      if ($value$plusargs("lvl=%d", v)) $display("model lvl=%0d", v);
      $finish(0);
    end
  end
endmodule
