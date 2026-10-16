// Two BVI declarations reuse the variable name tag but resolve their fields
// under different provisos.  Relay also exercises instance reduction which
// depends on a given, rather than just a direct match against that given.
typeclass View#(type tag, type result)
  dependencies (tag determines result);
  type ViewOf#(type tag) = result;
endtypeclass

typeclass Relay#(type tag, type result)
  dependencies (tag determines result);
  type RelayOf#(type tag) = result;
endtypeclass

instance Relay#(tag, result) provisos (View#(tag, result));
endinstance

interface SubIfc#(numeric type n);
  method Bit#(n) value();
endinterface

interface Outer#(type tag);
  interface RelayOf#(tag) sub;
endinterface

instance View#(Bit#(3), SubIfc#(3));
endinstance

instance View#(Bit#(5), SubIfc#(5));
endinstance

import "BVI" GivenThree =
module mkGivenThree (Outer#(tag)) provisos (View#(tag, SubIfc#(3)));
  default_clock no_clock;
  no_reset;
  interface SubIfc sub;
    method OUT value();
  endinterface
  schedule sub_value CF sub_value;
endmodule

import "BVI" GivenFive =
module mkGivenFive (Outer#(tag)) provisos (View#(tag, SubIfc#(5)));
  default_clock no_clock;
  no_reset;
  interface SubIfc sub;
    method OUT value();
  endinterface
  schedule sub_value CF sub_value;
endmodule

(* synthesize *)
module mkATFGivenThree (Outer#(Bit#(3)));
  let imported <- mkGivenThree;
  return imported;
endmodule

(* synthesize *)
module mkATFGivenFive (Outer#(Bit#(5)));
  let imported <- mkGivenFive;
  return imported;
endmodule
