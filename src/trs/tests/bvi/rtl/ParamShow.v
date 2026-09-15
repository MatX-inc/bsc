// Typed-parameter target: signed, wide (96-bit), string, and real
// parameters, displayed once on GO -- semantic (not textual) parameter
// equality is the gate.
module ParamShow(CLK, RST_N, GO);
  parameter SIGNED_P = 0;
  parameter [95:0] WIDE_P = 96'h0;
  parameter STR_P = "none";
  parameter real REAL_P = 0.0;

  input CLK, RST_N, GO;

  always @(posedge CLK) begin
    if (GO) begin
      $display("signed=%0d wide=%h str=%s real=%f",
               SIGNED_P, WIDE_P, STR_P, REAL_P);
      $finish(0);
    end
  end
endmodule
