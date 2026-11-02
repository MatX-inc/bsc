// A starved rule: two rules without conditions write register a and the
// more urgent one always fires, so the other never does; under
// -remove-starved-rules aDumpSchedule drops it after the schedule stage
// made its CAN_FIRE and WILL_FIRE.  Its uses stay in the method-uses
// map, so the write port's mux and enable are built with an arm for it;
// their records name only the rule that stays.
(* synthesize *)
module sysMetaStarved(Empty);
   Reg#(UInt#(8)) a <- mkReg(0);

   (* descending_urgency = "hog, starved" *)
   rule hog;
      a <= a + 2;
   endrule

   rule starved;
      a <= a + 1;
   endrule
endmodule
