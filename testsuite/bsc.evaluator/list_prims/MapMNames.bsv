import Vector::*;
import List::*;

// Pin the instance names produced by monadic traversals: Vector's
// mapM/replicateM must yield indexed names (xs_0, xs_1, ...) and
// List's must yield collision-suffixed names (ls, ls_1, ...).
// Checked with find_regexp on the generated Verilog.

(* synthesize *)
module sysMapMNames(Empty);

  Vector#(3, Reg#(Bit#(8))) xs <- replicateM(mkReg(0));
  List#(Reg#(Bit#(8))) ls <- List::replicateM(3, mkReg(0));

  rule keep;
    xs[0] <= xs[0] + 1;
    xs[1] <= xs[1] + 1;
    xs[2] <= xs[2] + 1;
    (ls[0]) <= (ls[0]) + 1;
    (ls[1]) <= (ls[1]) + 1;
    (ls[2]) <= (ls[2]) + 1;
  endrule

endmodule
