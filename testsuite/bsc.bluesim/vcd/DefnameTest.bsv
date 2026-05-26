// Exercises Bluesim's emission of `$comment defname <modtype> $end`
// after each `$scope` for synthesized user modules.

package DefnameTest;

interface Sub;
  method Action ping();
endinterface

(* synthesize *)
module mkDefnameTestSub (Sub);
  Reg#(UInt#(8)) c <- mkReg(0);
  method Action ping();
    c <= c + 1;
  endmethod
endmodule

(* synthesize *)
module sysDefnameTest ();
  Sub s1 <- mkDefnameTestSub;
  Sub s2 <- mkDefnameTestSub;
  Reg#(UInt#(8)) t <- mkReg(0);

  rule tick;
    t <= t + 1;
    s1.ping();
    s2.ping();
  endrule

  rule done (t >= 5);
    $finish(0);
  endrule
endmodule

endpackage
