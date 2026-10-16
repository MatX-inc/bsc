import ATF_BVIGivens::*;

// The first declaration's given must not leak to the second, even though both
// use the source variable tag and the same associated type function.
import "BVI" WithGiven =
module mkWithGiven (Outer#(tag)) provisos (View#(tag, SubIfc#(3)));
  default_clock no_clock;
  no_reset;
  interface SubIfc sub;
    method OUT value();
  endinterface
  schedule sub_value CF sub_value;
endmodule

import "BVI" MissingGiven =
module mkMissingGiven (Outer#(tag));
  default_clock no_clock;
  no_reset;
  interface SubIfc sub;
    method OUT value();
  endinterface
  schedule sub_value CF sub_value;
endmodule
