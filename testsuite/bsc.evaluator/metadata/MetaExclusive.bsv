// Scheduling attributes: two mutually exclusive rules, for which the
// scheduler adds a rule (__me_check_<n>, created from the first of the
// pair) whose assumption checks the exclusion at run time, and which
// state generation drops once its CAN_FIRE and WILL_FIRE stand for it;
// two conflict-free rules writing one register under conditions, whose
// condition wires are instances created by aAddCFConditionWires and
// dropped by aInlineWires; and a preempting pair, which adds no id.
(* synthesize *)
module sysMetaExclusive(Empty);
   Reg#(UInt#(8)) a <- mkReg(0);
   Reg#(UInt#(8)) b <- mkReg(0);
   Reg#(UInt#(8)) c <- mkReg(0);
   Reg#(Bool) d <- mkReg(False);

   (* mutually_exclusive = "one, two" *)
   rule one (d);
      a <= a + 1;
   endrule

   rule two (c == 2);
      a <= a - 1;
   endrule

   (* conflict_free = "left, right" *)
   rule left;
      if (b == 0) c <= 1;
   endrule

   rule right;
      if (b == 1) c <= 2;
   endrule

   (* preempts = "flip, step" *)
   rule flip (c == 2);
      d <= !d;
   endrule

   rule step;
      d <= (b > 5);
      b <= b + 1;
   endrule
endmodule
