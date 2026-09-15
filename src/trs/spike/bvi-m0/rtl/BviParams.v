// M0 fixture: typed parameter semantics through -G.
// One of each hazard class from the review: signed (sign-extension bugs),
// string (quoting/escaping), real (round-trip), wide >64 (limb handling).
module BviParams(CLK, P_SINT, P_WIDE);
  parameter signed [31:0] SINT = 0;
  parameter               STR  = "none";
  parameter real          RVAL = 0.0;
  parameter [95:0]        WIDE = 96'h0;
  input               CLK;
  output signed [31:0] P_SINT;
  output [95:0]       P_WIDE;
  assign P_SINT = SINT;
  assign P_WIDE = WIDE;
  initial $display("STR=%s RVAL=%g", STR, RVAL);
endmodule
