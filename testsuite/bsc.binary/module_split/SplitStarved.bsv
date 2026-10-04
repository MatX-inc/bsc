package SplitStarved;

// The explicit priority and unconditional conflicting writes make starvation
// independent of source-order tie breaking. Compile with -remove-starved-rules
// to exercise a recorded rule removal when reconstructing the scheduled body.
(* synthesize *)
module sysSplitStarved(Empty);
   Reg#(Bit#(8)) value <- mkReg(0);
   Reg#(Bit#(8)) cycle <- mkReg(0);

   (* descending_urgency = "winner, starved" *)
   rule winner;
      value <= value + 2;
   endrule

   rule starved;
      value <= value + 1;
   endrule

   rule advance (cycle < 4);
      cycle <= cycle + 1;
   endrule

   rule finish (cycle == 4);
      $display("split-starved: %0d", value);
      $finish(0);
   endrule
endmodule

endpackage
