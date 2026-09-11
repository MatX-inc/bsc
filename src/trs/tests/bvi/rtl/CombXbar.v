// Combinational, no clock port: the shape an imported comb IP block has.
module CombXbar(a, b, s);
  input  [7:0] a, b;
  output [7:0] s;
  assign s = a + b;
endmodule
