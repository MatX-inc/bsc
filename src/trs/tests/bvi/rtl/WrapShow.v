// Forwarded-parameter target: a bit-vector parameter arrives through a
// parameterized wrapper module level (the common IP-in-a-wrapper
// pattern); a string parameter is bound at the import site.
module WrapShow(CLK, RST_N, GO);
  parameter [7:0] WIDTH_P = 0;
  parameter NAME_P = "none";
  input CLK, RST_N, GO;

  always @(posedge CLK) begin
    if (GO) begin
      $display("width=%0d name=%s", WIDTH_P, NAME_P);
    end
  end
endmodule
